#!/usr/bin/env python3
"""Exercise call/type hierarchies with an installed JDT LS in an isolated project."""
import argparse
import json
import os
from pathlib import Path
from runpy import run_path
import shlex
import signal
import subprocess
import tempfile
import time

Rpc = run_path(str(Path(__file__).with_name("smoke-spring.py")))["Rpc"]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--proxy", required=True)
    parser.add_argument("--jdtls", required=True)
    parser.add_argument("--java-home", required=True)
    parser.add_argument("--keep", action="store_true")
    args = parser.parse_args()
    temporary = tempfile.TemporaryDirectory(prefix="lsp-proxy-hierarchy-smoke-")
    root = Path(temporary.name).resolve()
    if args.keep:
        temporary._finalizer.detach()
    print("Smoke workspace:", root, flush=True)
    project = root / "project"
    sources = project / "src/main/java/smoke"
    sources.mkdir(parents=True)
    (project / ".git").mkdir()
    (project / "pom.xml").write_text('''<project xmlns="http://maven.apache.org/POM/4.0.0">
<modelVersion>4.0.0</modelVersion><groupId>smoke</groupId>
<artifactId>hierarchy</artifactId><version>1.0</version>
<properties><maven.compiler.source>8</maven.compiler.source>
<maven.compiler.target>8</maven.compiler.target></properties></project>
''')
    texts = {
        "Base": """package smoke;
@Deprecated
public class Base {
  public void target() {}
  public void caller() { target(); }
}
""",
        "Child": """package smoke;
public class Child extends Base {
  @Override public void target() { super.target(); }
}
""",
        "Leaf": """package smoke;
public class Leaf extends Child {}
""",
    }
    files = {}
    for name, text in texts.items():
        files[name] = sources / f"{name}.java"
        files[name].write_text(text)
    launcher = root / "jdtls-smoke"
    launcher.write_text("#!/bin/sh\nexec " + shlex.quote(args.jdtls) +
                        " -data " + shlex.quote(str(root / "jdt-data")) + ' "$@"\n')
    launcher.chmod(0o755)
    config = root / "languages.toml"
    config.write_text(f'''[[language]]
name = "java"
roots = ["pom.xml"]
language-servers = [{{ name = "jdtls", support-workspace = false }}]
[language-server.jdtls]
command = {json.dumps(str(launcher))}
timeout = 60
''')
    environment = os.environ.copy()
    environment["JAVA_HOME"] = args.java_home
    results = []
    with (root / "stderr.log").open("wb") as stderr:
        process = subprocess.Popen(
            [args.proxy, "--stdio", "-c", str(config), "--log-level", "3",
             "--log", str(root / "proxy.log")],
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=stderr,
            env=environment, start_new_session=True,
        )
        rpc = Rpc(process)
        try:
            for name, source in files.items():
                rpc.send("textDocument/didOpen", {"params": {"textDocument": {
                    "uri": source.as_uri(), "languageId": "java", "version": 1,
                    "text": texts[name]}}})

            def request(method, params):
                result = rpc.request(method, params, timeout=60)
                results.append({"method": method, "params": params, "result": result})
                return result

            def prepare(kind, name, symbol):
                lines = texts[name].splitlines()
                line = next(i for i, text in enumerate(lines) if symbol in text)
                params = {"uri": files[name].as_uri(), "params": {
                    "textDocument": {"uri": files[name].as_uri()}, "position": {
                        "line": line, "character": lines[line].index(symbol) + 1}}}
                deadline = time.monotonic() + 120
                while True:
                    try:
                        items = request(f"textDocument/prepare{kind}Hierarchy", params)
                        if items:
                            assert items[0]["language_server_name"] == "jdtls"
                            return items[0]
                    except RuntimeError:
                        if time.monotonic() >= deadline:
                            raise
                    if time.monotonic() >= deadline:
                        raise AssertionError(f"No {kind} hierarchy for {name}.{symbol}")
                    time.sleep(1)

            def expand(method, node, source):
                return request(method, {"uri": source.as_uri(), "params": {
                    "item": node["lsp_item"]}, "context": {
                        "language-server-id": node["language_server_id"]}}) or []

            child = prepare("Type", "Child", "Child")
            assert child["lsp_item"]["tags"] == []
            supers = expand("typeHierarchy/supertypes", child, files["Child"])
            subs = expand("typeHierarchy/subtypes", child, files["Child"])
            assert any(n["lsp_item"]["name"] == "Base" for n in supers), supers
            assert any(n["lsp_item"]["name"] == "Leaf" for n in subs), subs
            base = prepare("Type", "Base", "Base")
            assert base["lsp_item"]["tags"] == [1]
            assert any(n["lsp_item"]["name"] == "Child" for n in
                       expand("typeHierarchy/subtypes", base, files["Base"]))
            print("PASS: Type hierarchy tags []/[1], supertypes and subtypes", flush=True)

            target = prepare("Call", "Base", "target")
            incoming = expand("callHierarchy/incomingCalls", target, files["Base"])
            assert any("caller" in n["lsp_item"]["from"]["name"] for n in incoming), incoming
            caller = prepare("Call", "Base", "caller")
            outgoing = expand("callHierarchy/outgoingCalls", caller, files["Base"])
            assert any("target" in n["lsp_item"]["to"]["name"] for n in outgoing), outgoing
            assert all(n["lsp_item"]["fromRanges"] for n in incoming + outgoing)
            assert all(n["language_server_id"] == target["language_server_id"]
                       for n in incoming + outgoing)
            print("PASS: Call hierarchy incoming/outgoing, call ranges and server origin", flush=True)
        finally:
            os.killpg(process.pid, signal.SIGTERM)
            try:
                process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()
            (root / "results.json").write_text(json.dumps(results, indent=2))


if __name__ == "__main__":
    main()
