#!/usr/bin/env python3
"""Opt-in search scope smoke test through the proxy and an isolated JDTLS."""
import argparse
import importlib.util
import json
import re
import subprocess
from pathlib import Path
import tempfile
import time
from urllib.parse import unquote, urlparse

spec = importlib.util.spec_from_file_location(
    "test_navigation", Path(__file__).with_name("test-java-test-navigation.py"))
navigation = importlib.util.module_from_spec(spec)
spec.loader.exec_module(navigation)
Peer = navigation.Peer


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--proxy", type=Path, default=Path("target/release/emacs-lsp-proxy"))
    parser.add_argument("--jdtls", type=Path, required=True)
    parser.add_argument("--java", type=Path, required=True)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="lsp-proxy-search-scope-") as temporary:
        temporary = Path(temporary).resolve()
        project = temporary / "project"
        project.mkdir()
        (project / ".git").mkdir()
        library_source = temporary / "library/ScopeLibrary.java"
        library_source.parent.mkdir()
        library_source.write_text("package example.library; public class ScopeLibrary {}\n")
        classes = temporary / "library/classes"
        library = temporary / "scope-library.jar"
        subprocess.run([str(args.java.resolve().with_name("javac")), "--release", "17",
                        "-d", str(classes), str(library_source)], check=True)
        subprocess.run([str(args.java.resolve().with_name("jar")), "--create",
                        "--file", str(library), "-C", str(classes), "."], check=True)
        source = project / "src/main/java/example/Scope.java"
        caller = source.with_name("Caller.java")
        test = project / "src/test/java/example/ScopeTest.java"
        for file, text in [
            (source, "package example;\npublic class Scope {\n    public static void target() {}\n}\n"),
            (caller, "package example;\npublic class Caller {\n    void call() { Scope.target(); }\n}\n"),
            (test, "package example;\npublic class ScopeTest {\n    void first() { Scope.target(); }\n    void second() { Scope.target(); }\n}\n"),
        ]:
            file.parent.mkdir(parents=True, exist_ok=True)
            file.write_text(text)
        (project / "pom.xml").write_text(f"""<project><modelVersion>4.0.0</modelVersion>
<groupId>example</groupId><artifactId>search-scope</artifactId><version>1</version>
<dependencies><dependency><groupId>example</groupId><artifactId>scope-library</artifactId>
<version>1</version><scope>system</scope><systemPath>{library}</systemPath></dependency></dependencies>
<properties><maven.compiler.release>17</maven.compiler.release></properties></project>""")
        config = temporary / "languages.toml"
        config.write_text(f"""[[language]]
name = "java"
file-types = ["java"]
roots = [".git"]
language-servers = ["jdtls"]
[language-server.jdtls]
command = {json.dumps(str(args.jdtls.resolve()))}
args = {json.dumps(["--java-executable", str(args.java.resolve()), "-data", str(temporary / "jdt-data")])}
[language-server.jdtls.config.extendedClientCapabilities]
classFileContentsSupport = true
[language-server.jdtls.config.settings.java.search]
scope = "main"
[language-server.jdtls.config.settings.java.referencesCodeLens]
enabled = true
""")
        original_config = config.read_text()
        with (temporary / "proxy.stderr").open("w") as stderr:
            proxy = Peer([str(args.proxy.resolve()), "--stdio", "--config", str(config),
                          "--log", str(temporary / "proxy.log")], project, stderr)
            try:
                for file in [source, caller, test]:
                    proxy.send({"method": "textDocument/didOpen", "params": {
                        "uri": file.as_uri(), "params": {"textDocument": {
                            "uri": file.as_uri(), "languageId": "java", "version": 0,
                            "text": file.read_text()}}}})
                deadline = time.monotonic() + 120
                while True:
                    message = proxy.receive(deadline)
                    if (message.get("method") == "emacs/javaImportLog"
                            and "ServiceReady" in message.get("params", {}).get("message", "")):
                        break
                print("Proxy/JDTLS imported the isolated Maven project", flush=True)
                envelope = {"uri": source.as_uri(), "params": {"serverName": "jdtls"}}
                initial = proxy.request("emacs/javaSearchScope", envelope)
                assert initial["scope"] == "main", initial
                server_id = initial["serverId"]
                position = {"line": 2, "character": source.read_text().splitlines()[2].index("target") + 1}
                references = {"uri": source.as_uri(), "params": {
                    "textDocument": {"uri": source.as_uri()}, "position": position,
                    "context": {"includeDeclaration": False}}}

                def check_references(expected):
                    locations = proxy.request("textDocument/references", references)
                    files = [Path(unquote(urlparse(item["uri"]).path)).resolve()
                             for item in locations]
                    assert files.count(caller) == 1, locations
                    assert files.count(test) == expected, locations
                    assert len(files) == expected + 1, locations

                def check_codelens(expected):
                    lenses = proxy.request("textDocument/codeLens", {
                        "uri": source.as_uri(),
                        "params": {"textDocument": {"uri": source.as_uri()}}})
                    titles = []
                    for lens in lenses:
                        if lens["lsp_item"]["range"]["start"]["line"] != position["line"]:
                            continue
                        resolved = proxy.request("codeLens/resolve", {
                            "uri": source.as_uri(), "params": lens["lsp_item"],
                            "context": {"language-server-id": lens["language_server_id"]}})
                        title = resolved["lsp_item"].get("command", {}).get("title", "")
                        if re.fullmatch(r"\d+ references?", title):
                            titles.append(title)
                    assert titles == [f"{expected} reference" + ("s" if expected != 1 else "")], titles
                    return titles[0]

                def check_library_symbols(included):
                    symbols = proxy.request("workspace/symbol", {
                        "uri": source.as_uri(), "params": {"query": "ScopeLibrary"}})
                    names = [item["lsp_item"]["name"] for item in symbols]
                    assert ("ScopeLibrary" in names) == included, symbols

                check_references(0)
                check_codelens(1)
                check_library_symbols(True)
                for scope, tests in [("all", 2), ("projectOnly", 2), ("main", 0), ("all", 2)]:
                    envelope["params"] = {"serverName": "jdtls", "serverId": server_id, "scope": scope}
                    changed = proxy.request("emacs/javaSearchScope", envelope)
                    assert changed["scope"] == scope and changed["serverId"] == server_id, changed
                    check_references(tests)
                    title = check_codelens(tests + 1)
                    check_library_symbols(scope != "projectOnly")
                    current = proxy.request("emacs/javaSearchScope", {
                        "uri": source.as_uri(), "params": {"serverName": "jdtls"}})
                    assert current["scope"] == scope, current
                    print(f"scope={scope}: 1 production reference, {tests} test references; "
                          f"CodeLens={title}; library symbols="
                          f"{'excluded' if scope == 'projectOnly' else 'included'}", flush=True)
                envelope["params"]["serverId"] = server_id + 10000
                envelope["params"]["scope"] = "main"
                try:
                    proxy.request("emacs/javaSearchScope", envelope)
                except RuntimeError as error:
                    assert "no longer available" in str(error), error
                else:
                    raise AssertionError("Stale server selection changed configuration")
                check_references(2)
                assert config.read_text() == original_config
                print("PASS: dynamic scope changes, stale-instance guard, unchanged config file", flush=True)
            except Exception:
                stderr.flush()
                print((temporary / "proxy.stderr").read_text()[-3000:], flush=True)
                raise
            finally:
                proxy.close()


if __name__ == "__main__":
    main()
