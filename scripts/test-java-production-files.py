#!/usr/bin/env python3
"""Opt-in production file smoke test using the proxy and an isolated JDTLS."""
import argparse
import importlib.util
import json
from pathlib import Path
import tempfile
import time

spec = importlib.util.spec_from_file_location(
    "test_navigation", Path(__file__).with_name("test-java-test-navigation.py"))
navigation = importlib.util.module_from_spec(spec)
spec.loader.exec_module(navigation)
Peer = navigation.Peer


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--proxy", type=Path, default=Path("target/debug/emacs-lsp-proxy"))
    parser.add_argument("--jdtls", type=Path, required=True)
    parser.add_argument("--java", type=Path, required=True)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="lsp-proxy-production-files-") as temporary:
        temporary = Path(temporary).resolve()
        project = temporary / "project"
        project.mkdir()
        (project / ".git").mkdir()
        source = project / "module-a/custom-production/example/UserServiceTest.java"
        test = project / "module-a/custom-tests/example/UserService.java"
        external = temporary / "external-source/example/External.java"
        for file, name in [(source, "UserServiceTest"), (test, "UserService"), (external, "External")]:
            file.parent.mkdir(parents=True, exist_ok=True)
            file.write_text(f"package example; public class {name} {{}}")
        (project / "pom.xml").write_text("""<project><modelVersion>4.0.0</modelVersion>
<groupId>example</groupId><artifactId>production-files</artifactId><version>1</version>
<packaging>pom</packaging><modules><module>module-a</module><module>module-b</module></modules></project>""")
        parent = """<project><modelVersion>4.0.0</modelVersion><parent>
<groupId>example</groupId><artifactId>production-files</artifactId><version>1</version></parent>
<artifactId>{name}</artifactId><properties><maven.compiler.release>17</maven.compiler.release></properties>
<build><sourceDirectory>{source}</sourceDirectory>{test}</build></project>"""
        (project / "module-a/pom.xml").write_text(parent.format(
            name="module-a", source="custom-production", test="<testSourceDirectory>custom-tests</testSourceDirectory>"))
        (project / "module-b").mkdir()
        (project / "module-b/pom.xml").write_text(parent.format(
            name="module-b", source=str(external.parents[1]), test=""))
        config = temporary / "languages.toml"
        config.write_text(f"""[[language]]
name = "java"
file-types = ["java"]
roots = [".git"]
language-servers = ["jdtls"]
[language-server.jdtls]
command = {json.dumps(str(args.jdtls.resolve()))}
args = {json.dumps(["--java-executable", str(args.java.resolve()), "-data", str(temporary / "jdt-data")])}
[language-server.jdtls.config.settings.java.configuration]
updateBuildConfiguration = "automatic"
""")
        with (temporary / "proxy.stderr").open("w") as stderr:
            proxy = Peer([str(args.proxy.resolve()), "--stdio", "--config", str(config)], project, stderr)
            try:
                proxy.send({"method": "textDocument/didOpen", "params": {
                    "uri": source.as_uri(), "params": {"textDocument": {
                        "uri": source.as_uri(), "languageId": "java", "version": 0,
                        "text": source.read_text()}}}})
                deadline = time.monotonic() + 120
                while True:
                    message = proxy.receive(deadline)
                    if message.get("method") == "emacs/javaImportLog" and "ServiceReady" in message.get("params", {}).get("message", ""):
                        break
                print("Proxy/JDTLS imported the isolated multi-module Maven project", flush=True)
                query = {"uri": source.as_uri(), "params": {
                    "projectUri": project.as_uri(), "serverName": "jdtls",
                    "extensions": ["java"], "includeGenerated": True}}
                result = proxy.request("emacs/javaProductionFiles", query)
                found = {Path(file).resolve() for file in result["files"]}
                assert found == {source, external}, result
                assert not result["fallbackModules"] and not result["warnings"], result
                print("Custom production/test roots and external module source roots passed", flush=True)
                # Create real candidates after import to exercise resource and generated-root discovery.
                resource = project / "module-a/src/main/resources/application.properties"
                generated = project / "module-a/target/generated-sources/annotations/example/Generated.java"
                generated_test = project / "module-a/target/generated-test-sources/test-annotations/example/GeneratedTest.java"
                for file in [resource, generated, generated_test]:
                    file.parent.mkdir(parents=True, exist_ok=True)
                    file.write_text("")
                query["params"]["extensions"] = None
                result = proxy.request("emacs/javaProductionFiles", query)
                found = {Path(file).resolve() for file in result["files"]}
                assert resource in found and generated in found and test not in found and generated_test not in found, result
                query["params"]["includeGenerated"] = False
                result = proxy.request("emacs/javaProductionFiles", query)
                found = {Path(file).resolve() for file in result["files"]}
                assert resource in found and generated not in found, result
                print("Resources and generated-source inclusion/exclusion passed", flush=True)
            finally:
                proxy.close()


if __name__ == "__main__":
    main()
