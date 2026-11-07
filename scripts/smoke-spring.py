#!/usr/bin/env python3
"""Exercise installed JDT LS + STS4 through the proxy in temporary projects."""
import argparse
import json
import os
from pathlib import Path
import queue
import shlex
import subprocess
import tempfile
import threading
import time
import urllib.request


class Rpc:
    def __init__(self, process):
        self.process = process
        self.messages = queue.Queue()
        self.sequence = 0
        threading.Thread(target=self.read, daemon=True).start()

    def read(self):
        try:
            while True:
                headers = {}
                while True:
                    line = self.process.stdout.readline()
                    if not line:
                        return
                    if line == b"\r\n":
                        break
                    key, value = line.decode().split(":", 1)
                    headers[key.lower()] = value.strip()
                body = self.process.stdout.read(int(headers["content-length"]))
                self.messages.put(json.loads(body))
        except Exception as error:
            self.messages.put({"reader_error": str(error)})

    def send(self, method, params, request=False):
        message = {"jsonrpc": "2.0", "method": method, "params": params}
        if request:
            self.sequence += 1
            message["id"] = self.sequence
        body = json.dumps(message).encode()
        self.process.stdin.write(f"Content-Length: {len(body)}\r\n\r\n".encode() + body)
        self.process.stdin.flush()
        return message.get("id")

    def request(self, method, params, timeout=150):
        mid = self.send(method, params, True)
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            message = self.messages.get(timeout=max(0.1, deadline - time.monotonic()))
            if message.get("id") == mid:
                if "error" in message:
                    raise RuntimeError(message["error"])
                return message.get("result")
        raise TimeoutError(method)


def project(root, name):
    root.mkdir()
    # The proxy bounds root detection by a Git workspace (otherwise cwd).
    (root / ".git").mkdir()
    (root / "pom.xml").write_text(f'''<project xmlns="http://maven.apache.org/POM/4.0.0">
<modelVersion>4.0.0</modelVersion>
<parent><groupId>org.springframework.boot</groupId><artifactId>spring-boot-starter-parent</artifactId><version>2.2.13.RELEASE</version></parent>
<groupId>smoke</groupId><artifactId>{name}</artifactId><version>1.0</version>
<properties><java.version>1.8</java.version></properties>
<dependencies>
<dependency><groupId>org.springframework.boot</groupId><artifactId>spring-boot-starter-web</artifactId></dependency>
<dependency><groupId>org.springframework.boot</groupId><artifactId>spring-boot-starter-actuator</artifactId></dependency>
</dependencies>
</project>''')
    source = root / "src/main/java/smoke/App.java"
    source.parent.mkdir(parents=True)
    source.write_text(f'''package smoke;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.web.bind.annotation.*;
import java.util.function.Function;
@SpringBootApplication
@RestController
public class App {{
  public static void main(String[] args) {{ SpringApplication.run(App.class, args); }}
  @Bean public String {name}Bean() {{ return "{name}"; }}
  @Bean public Function<String,String> {name}Function() {{ return s -> s; }}
  @GetMapping("/{name}") public String endpoint() {{ return "{name}"; }}
}}
''')
    return source


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--proxy", required=True)
    parser.add_argument("--jdtls", required=True)
    parser.add_argument("--spring-dir", required=True, help="installed STS4 version directory")
    parser.add_argument("--java-home", required=True, help="JDK 21 home for language servers")
    parser.add_argument("--project-jdk", required=True, help="JDK 8 home for fixture projects")
    parser.add_argument("--keep", action="store_true", help="keep the temporary project and log")
    parser.add_argument("--live", action="store_true", help="compile and run the first app with local JMX; test //")
    args = parser.parse_args()
    temporary = tempfile.TemporaryDirectory(prefix="lsp-proxy-spring-smoke-")
    root = Path(temporary.name).resolve()
    print("Smoke workspace:", root, flush=True)
    if args.keep:
        temporary._finalizer.detach()
    spring = Path(args.spring_dir).resolve()
    info = json.loads((spring / "spring-install.json").read_text())
    jdt_launcher = root / "jdtls-smoke"
    jdt_launcher.write_text("#!/bin/sh\nexec " + shlex.quote(args.jdtls) +
                           ' -data ' + shlex.quote(str(root)) + '/jdt-data-"$(basename "$PWD")" "$@"\n')
    jdt_launcher.chmod(0o755)
    config = root / "languages.toml"
    config.write_text(f'''[[language]]
name = "java"
roots = ["pom.xml"]
language-servers = [
  {{ name = "jdtls", support-workspace = false }},
  {{ name = "spring-boot", only-features = ["workspace-symbols"], support-workspace = false }}
]
[language-server.jdtls]
command = {json.dumps(str(jdt_launcher))}
timeout = 120
[language-server.jdtls.config.settings.java.configuration]
runtimes = [{{ name = "JavaSE-1.8", path = {json.dumps(args.project_jdk)}, default = true }}]
[language-server.spring-boot]
command = {json.dumps(str(spring / "bin/spring-boot-ls"))}
timeout = 120
[language-server.spring-boot.config.boot-java.live-information]
all-local-java-processes = true
''')
    environment = os.environ.copy()
    environment["JAVA_HOME"] = args.java_home
    environment["LSP_PROXY_SPRING_JAVA"] = str(Path(args.java_home) / "bin/java")
    environment["LSP_PROXY_SPRING_BUNDLES"] = os.pathsep.join(str(spring / path) for path in info["bundles"])
    with (root / "stderr.log").open("wb") as stderr:
        process = subprocess.Popen([args.proxy, "--stdio", "-c", str(config), "--log-level", "3", "--log", str(root / "proxy.log")],
                                   stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=stderr, env=environment)
        rpc = Rpc(process)
        app = None
        app_log = None
        try:
            sources = [project(root / name, name) for name in ("one", "two")]
            for source in sources:
                rpc.send("textDocument/didOpen", {"params": {"textDocument": {
                    "uri": source.as_uri(), "languageId": "java", "version": 1, "text": source.read_text()}}})
            deadline = time.monotonic() + 180
            for source, name in zip(sources, ("one", "two")):
                for query in ("@/", "@+", "@>", "@"):
                    while True:
                        try:
                            items = rpc.request("workspace/symbol", {"uri": source.as_uri(), "params": {"query": query}})
                        except RuntimeError as error:
                            if time.monotonic() > deadline:
                                raise
                            items = []
                        spring_items = [item["lsp_item"] for item in items or [] if item["language_server_name"] == "spring-boot"]
                        if spring_items:
                            break
                        if time.monotonic() > deadline:
                            raise AssertionError(f"No Spring results for {name} {query}; inspect {root}/proxy.log")
                        time.sleep(2)
                    assert all(item["location"]["uri"].startswith(source.parent.parent.parent.parent.parent.as_uri()) for item in spring_items)
                    print(name, query, [item["name"] for item in spring_items], flush=True)
            print("PASS: Spring symbols and project isolation through real JDT LS + STS4", flush=True)
            for source in sources:
                lines = source.read_text().splitlines()
                line = next(i for i, text in enumerate(lines) if "App.class" in text)
                definition = rpc.request("textDocument/definition", {"uri": source.as_uri(), "params": {
                    "textDocument": {"uri": source.as_uri()},
                    "position": {"line": line, "character": lines[line].index("App.class") + 1}}})
                locations = [definition] if isinstance(definition, dict) else definition
                expected_line = next(i for i, text in enumerate(lines) if "public class App" in text)
                assert any(location["uri"] == source.as_uri() and location["range"]["start"]["line"] == expected_line
                           for location in locations or []), f"Wrong Java definition: {definition}"
            print("PASS: Java definition with JDT LS + STS4 in both projects", flush=True)
            proxy_log = (root / "proxy.log").read_text()
            definition_registrations = 0
            for fragment in proxy_log.split("LSP Response:\n")[1:]:
                message, end = json.JSONDecoder().raw_decode(fragment)
                trailer = fragment[end:].splitlines()[0]
                if message.get("method") == "client/registerCapability" and "server=jdtls " in trailer:
                    definition_registrations += sum(reg["method"] == "textDocument/definition"
                                                    for reg in message["params"]["registrations"])
            assert definition_registrations >= 2, "JDT definition dynamic registration was not exercised in both projects"
            print("PASS: JDT definition dynamic registration exercised in both projects", flush=True)
            assert '"command": "vscode-spring-boot.ls.start"' in proxy_log, "JDT did not send the STS start command"
            assert "No STS callback registered for vscode-spring-boot.ls." not in proxy_log, "STS lifecycle command was rejected"
            print("PASS: JDT STS lifecycle command acknowledged", flush=True)
            if args.live:
                app_root = sources[0].parents[4]
                app_environment = environment.copy()
                app_environment["JAVA_HOME"] = args.project_jdk
                subprocess.run(["mvn", "-q", "compile", "dependency:build-classpath", "-Dmdep.outputFile=classpath.txt"],
                               cwd=app_root, env=app_environment, check=True, timeout=180)
                classpath = str(app_root / "target/classes") + os.pathsep + (app_root / "classpath.txt").read_text().strip()
                app_log = (root / "app.log").open("wb")
                app = subprocess.Popen([str(Path(args.project_jdk) / "bin/java"),
                    "-Dcom.sun.management.jmxremote", "-Dspring.application.admin.enabled=true",
                    "-Dspring.jmx.enabled=true", "-Dmanagement.endpoints.jmx.exposure.include=*",
                    "-cp", classpath, "smoke.App", "--server.port=0"], cwd=app_root, stdout=app_log, stderr=app_log)
                deadline = time.monotonic() + 120
                connected = False
                while True:
                    if not connected:
                        processes = rpc.request("workspace/executeCommand", {"uri": sources[0].as_uri(), "params": {
                            "command": "sts/livedata/listProcesses", "arguments": []}})
                        target = next((p for p in processes or [] if (p.get("processId") == str(app.pid) or str(app.pid) in p.get("label", ""))
                                       and p.get("action") in ("sts/livedata/connect", "sts/livedata/refresh")), None)
                        if target:
                            rpc.request("workspace/executeCommand", {"uri": sources[0].as_uri(), "params": {
                                "command": target["action"], "arguments": [{"processKey": target["processKey"]}]}})
                            connected = True
                    items = rpc.request("workspace/symbol", {"uri": sources[0].as_uri(), "params": {"query": "//"}})
                    urls = [item["lsp_item"]["location"]["uri"] for item in items or []
                            if item["language_server_name"] == "spring-boot" and item["lsp_item"]["location"]["uri"].endswith("/one")]
                    if urls:
                        assert urllib.request.urlopen(urls[0], timeout=5).read().decode() == "one"
                        print("PASS: // returned a reachable endpoint", urls[0], flush=True)
                        break
                    if app.poll() is not None or time.monotonic() > deadline:
                        raise AssertionError(f"No live endpoint; inspect {root}/app.log and proxy.log")
                    time.sleep(2)
        finally:
            if app:
                app.terminate()
                try:
                    app.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    app.kill()
                    app.wait()
            if app_log:
                app_log.close()
            process.terminate()
            try:
                process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()
    if not args.keep:
        temporary.cleanup()


if __name__ == "__main__":
    main()
