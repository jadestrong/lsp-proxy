#!/usr/bin/env python3
"""Opt-in proxy/JDTLS test for duplicate bundles and Java test navigation."""
import argparse
import importlib.util
import json
from pathlib import Path
import tempfile
import time
from urllib.parse import unquote, urlparse

spec = importlib.util.spec_from_file_location(
    "test_navigation", Path(__file__).with_name("test-java-test-navigation.py"))
navigation = importlib.util.module_from_spec(spec)
spec.loader.exec_module(navigation)
Peer = navigation.Peer


def check(args, root):
    project = root / "project"
    project.mkdir()
    (project / ".git").mkdir()
    source = project / "src/main/java/example/Widget.java"
    test = project / "src/test/java/example/WidgetTest.java"
    for file, text in [
        (source, "package example; public class Widget {}"),
        (test, "package example; public class WidgetTest { Widget subject; }"),
    ]:
        file.parent.mkdir(parents=True, exist_ok=True)
        file.write_text(text)
    (project / "pom.xml").write_text("""<project><modelVersion>4.0.0</modelVersion>
<groupId>example</groupId><artifactId>bundle-conflicts</artifactId><version>1</version>
<properties><maven.compiler.release>17</maven.compiler.release></properties></project>""")
    installed = json.loads((args.test_bundles / "test-install.json").read_text())
    bundles = [str(args.test_bundles.resolve() / name) for name in installed["bundles"]]
    # Exercise the same merged initialization options used for managed bundles,
    # including user-contributed duplicates and optional Spring/Debug plugins.
    bundles.append(bundles[0])
    if args.debug_bundle:
        bundles.append(str(args.debug_bundle.resolve()))
    if args.spring_bundles:
        spring = json.loads((args.spring_bundles / "spring-install.json").read_text())
        bundles.extend(str(args.spring_bundles.resolve() / name) for name in spring["bundles"])
    config = root / "languages.toml"
    data = root / "jdt-data"
    config.write_text(f"""[[language]]
name = "java"
file-types = ["java"]
roots = [".git"]
language-servers = ["jdtls"]
[language-server.jdtls]
command = {json.dumps(str(args.jdtls.resolve()))}
args = {json.dumps(["--java-executable", str(args.java.resolve()), "-data", str(data)])}
[language-server.jdtls.config]
bundles = {json.dumps(bundles)}
[language-server.jdtls.config.settings.java.configuration]
updateBuildConfiguration = "automatic"
""")
    log = root / "proxy.log"
    with (root / "proxy.stderr").open("w") as stderr:
        proxy = Peer([str(args.proxy.resolve()), "--stdio", "--config", str(config),
                      "--log", str(log), "--log-level", "3"], project, stderr)
        try:
            for file in [source, test]:
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
            print("Isolated JDTLS reached ServiceReady", flush=True)
            for file, goto_test, expected in [(source, True, test), (test, False, source)]:
                result = proxy.request("emacs/forwardRequest", {
                    "uri": file.as_uri(), "params": {
                        "uri": file.as_uri(), "serverName": "jdtls", "method": "workspace/executeCommand",
                        "params": {"command": "vscode.java.test.navigateToTestOrTarget",
                                   "arguments": [file.as_uri(), goto_test]}}})
                found = [Path(unquote(urlparse(item["uri"]).path)).resolve()
                         for item in result["items"]]
                assert found == [expected], result
            references = proxy.request("textDocument/references", {
                "uri": source.as_uri(), "params": {
                    "textDocument": {"uri": source.as_uri()},
                    "position": {"line": 0, "character": source.read_text().index("Widget")},
                    "context": {"includeDeclaration": True}}})
            assert len(references) == 2, references
            print("Go to Test/Subject and references passed", flush=True)
        finally:
            proxy.close()
    metadata = (data / ".metadata/.log").read_text(errors="replace")
    for error in ["Failed to load extension bundles", "A bundle is already installed",
                  "Cannot refresh bundle"]:
        assert error not in metadata, error
    logged = log.read_text()
    decoder = json.JSONDecoder()
    initializations = []
    for part in logged.split("LSP Request:\n")[1:]:
        value, _ = decoder.raw_decode(part)
        if value.get("method") == "initialize":
            initializations.append(value)
    assert len(initializations) == 1, initializations
    sent = initializations[0]["params"]["initializationOptions"]["bundles"]
    asm = [bundle for bundle in bundles if Path(bundle).name.startswith("org.objectweb.asm")]
    assert asm and not set(asm).intersection(sent), sent
    assert len(sent) == len(set(sent)), sent
    assert any("com.microsoft.java.test.plugin" in bundle for bundle in sent), sent
    (root / "result.json").write_text(json.dumps({
        "contributedBundles": len(bundles), "sentBundles": len(sent),
        "reusedAsmBundles": len(asm), "references": len(references),
        "bundleErrors": [], "testNavigation": "passed"}, indent=2))
    print(f"No bundle errors; reused {len(asm)} registered ASM jars", flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--proxy", type=Path, default=Path("target/debug/emacs-lsp-proxy"))
    parser.add_argument("--jdtls", type=Path, required=True)
    parser.add_argument("--java", type=Path, required=True)
    parser.add_argument("--test-bundles", type=Path, required=True,
                        help="Installed Java Test Runner directory with test-install.json")
    parser.add_argument("--debug-bundle", type=Path)
    parser.add_argument("--spring-bundles", type=Path)
    parser.add_argument("--output-dir", type=Path,
                        help="New directory to retain logs and results instead of temporary cleanup")
    args = parser.parse_args()
    if args.output_dir:
        root = args.output_dir.resolve()
        root.mkdir(parents=True, exist_ok=False)
        check(args, root)
    else:
        with tempfile.TemporaryDirectory(prefix="lsp-proxy-bundle-conflicts-") as temporary:
            check(args, Path(temporary).resolve())


if __name__ == "__main__":
    main()
