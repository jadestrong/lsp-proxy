#!/usr/bin/env python3
"""Opt-in smoke test: install bundles, then navigate with an isolated JDTLS."""
import argparse
import json
from pathlib import Path
import queue
import subprocess
import tempfile
import threading
import time
from urllib.parse import unquote, urlparse


class Peer:
    def __init__(self, command, cwd, stderr):
        self.process = subprocess.Popen(command, cwd=cwd, stdin=subprocess.PIPE,
                                        stdout=subprocess.PIPE, stderr=stderr)
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
                length = int(headers["content-length"])
                data = self.process.stdout.read(length)
                self.messages.put(json.loads(data))
        finally:
            self.messages.put(None)

    def send(self, message):
        data = json.dumps({"jsonrpc": "2.0", **message}).encode()
        self.process.stdin.write(f"Content-Length: {len(data)}\r\n\r\n".encode() + data)
        self.process.stdin.flush()

    def receive(self, deadline):
        message = self.messages.get(timeout=max(0.01, deadline - time.monotonic()))
        if message is None:
            raise RuntimeError("Language server exited unexpectedly")
        if "method" in message and "id" in message:
            result = None
            if message["method"] == "workspace/configuration":
                result = [{} for _ in message.get("params", {}).get("items", [])]
            self.send({"id": message["id"], "result": result})
        return message

    def request(self, method, params, timeout=120):
        self.sequence += 1
        request_id = self.sequence
        self.send({"id": request_id, "method": method, "params": params})
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            message = self.receive(deadline)
            if message.get("id") == request_id and "method" not in message:
                if "error" in message:
                    raise RuntimeError(message["error"])
                return message.get("result")
        raise TimeoutError(method)

    def close(self):
        if self.process.poll() is None:
            try:
                self.request("shutdown", {"params": None}, timeout=3)
                self.send({"method": "exit", "params": {}})
                self.process.wait(timeout=3)
            except (RuntimeError, TimeoutError, queue.Empty, subprocess.TimeoutExpired, BrokenPipeError):
                self.process.kill()
                self.process.wait()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--proxy", type=Path, default=Path("target/debug/emacs-lsp-proxy"))
    parser.add_argument("--jdtls", type=Path, required=True)
    parser.add_argument("--java", type=Path, required=True)
    parser.add_argument("--install-dir", type=Path,
                        help="Optional persistent bundle directory; otherwise temporary")
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="lsp-proxy-test-navigation-") as temporary:
        root = Path(temporary)
        install_dir = (args.install_dir or root / "bundles").resolve()
        config = root / "languages.toml"
        config.write_text("language = []\n")
        with (root / "proxy.stderr").open("w") as stderr:
            proxy = Peer([str(args.proxy.resolve()), "--stdio", "--config", str(config)], root, stderr)
            try:
                params = {"params": {"installDir": str(install_dir), "version": "0.47.0"}}
                installed = proxy.request("emacs/installJavaTest", params, timeout=300)
                assert installed["bundles"]
                cached = proxy.request("emacs/installJavaTest", params)
                assert cached["alreadyInstalled"]
                bundles = [str(install_dir / installed["version"] / name)
                           for name in installed["bundles"]]
                print(f"Installed and verified {len(bundles)} Java Test Runner bundles", flush=True)
            finally:
                proxy.close()
        project = root / "project"
        source = project / "src/main/java/example/Widget.java"
        test = project / "src/test/java/example/WidgetTest.java"
        for file, text in [(source, "package example; public class Widget {}"),
                           (test, "package example; public class WidgetTest {}")]:
            file.parent.mkdir(parents=True, exist_ok=True)
            file.write_text(text)
        (project / "pom.xml").write_text("""<project><modelVersion>4.0.0</modelVersion>
<groupId>example</groupId><artifactId>test-navigation</artifactId><version>1</version>
<properties><maven.compiler.release>17</maven.compiler.release></properties></project>""")
        with (root / "jdtls.stderr").open("w") as stderr:
            jdt = Peer([str(args.jdtls.resolve()), "--java-executable", str(args.java.resolve()),
                        "-data", str(root / "jdt-data")], project, stderr)
            try:
                initialized = jdt.request("initialize", {
                    "processId": None, "rootUri": project.as_uri(),
                    "workspaceFolders": [{"uri": project.as_uri(), "name": "test-navigation"}],
                    "capabilities": {},
                    "initializationOptions": {"bundles": bundles,
                        "settings": {"java": {"configuration": {"updateBuildConfiguration": "automatic"}}}},
                })
                commands = initialized["capabilities"]["executeCommandProvider"]["commands"]
                assert "vscode.java.test.navigateToTestOrTarget" in commands
                jdt.send({"method": "initialized", "params": {}})
                deadline = time.monotonic() + 120
                while True:
                    message = jdt.receive(deadline)
                    if message.get("method") == "language/status" and message.get("params", {}).get("type") == "ServiceReady":
                        break
                print("JDTLS imported the isolated Maven project", flush=True)
                for file in [source, test]:
                    jdt.send({"method": "textDocument/didOpen", "params": {"textDocument": {
                        "uri": file.as_uri(), "languageId": "java", "version": 0, "text": file.read_text()}}})
                for file, goto_test, expected in [(source, True, test), (test, False, source)]:
                    result = jdt.request("workspace/executeCommand", {
                        "command": "vscode.java.test.navigateToTestOrTarget",
                        "arguments": [file.as_uri(), goto_test]})
                    # Java's File.toURI emits file:/ and JDT canonicalizes macOS /var.
                    found = [Path(unquote(urlparse(item["uri"]).path)).resolve()
                             for item in result["items"]]
                    assert found == [expected.resolve()], result
                    assert result["items"][0]["outOfBelongingProject"] is False
                print("Go to Test and Go to Test Subject passed against real JDTLS", flush=True)
            finally:
                jdt.close()


if __name__ == "__main__":
    main()
