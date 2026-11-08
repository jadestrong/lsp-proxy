//! The Java Test Runner bundles used by VS Code's class-level test navigation.
use crate::{java_install as install, lsp_ext::InstallJavaTestResult};
use anyhow::{bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, path::Path};

pub const VERSION: &str = "0.47.0";
const API: &str = "https://open-vsx.org/api/vscjava/vscode-java-test";

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct Manifest {
    version: String,
    bundles: Vec<String>,
}

fn jar_name(path: &str) -> bool {
    path.ends_with(".jar")
        && !path.starts_with('.')
        && path
            .bytes()
            .all(|c| c.is_ascii_alphanumeric() || b"._-".contains(&c))
}

fn payload(manifest: &str, listing: &str, version: &str) -> Result<Manifest> {
    let package: serde_json::Value = serde_json::from_str(manifest)?;
    if package["name"] != "vscode-java-test" || package["version"] != version {
        bail!("The VSIX is not the requested Java Test Runner release");
    }
    let entries: HashSet<_> = listing.lines().collect();
    let extensions = package["contributes"]["javaExtensions"]
        .as_array()
        .context("Java Test Runner has no javaExtensions manifest")?;
    let mut bundles = Vec::new();
    for extension in extensions {
        let name = extension
            .as_str()
            .and_then(|path| path.strip_prefix("./server/"))
            .context("Unexpected Java Test Runner bundle path")?;
        if !jar_name(name)
            || !entries.contains(format!("extension/server/{name}").as_str())
            || bundles.iter().any(|entry| entry == name)
        {
            bail!("Unsafe, duplicate or missing Java Test Runner bundle: {name}");
        }
        bundles.push(name.to_owned());
    }
    if !bundles
        .iter()
        .any(|name| name.starts_with("com.microsoft.java.test.plugin-"))
    {
        bail!("Java Test Runner has no test navigation plugin");
    }
    Ok(Manifest {
        version: version.into(),
        bundles,
    })
}

async fn publish(staged: &Path, destination: &Path, backup: &Path) -> Result<()> {
    let had_previous = destination.exists();
    if had_previous {
        tokio::fs::rename(destination, backup)
            .await
            .context("Could not preserve the previous Java Test Runner installation")?;
    }
    if let Err(error) = tokio::fs::rename(staged, destination).await {
        if had_previous {
            tokio::fs::rename(backup, destination)
                .await
                .context("Could not restore the previous Java Test Runner installation")?;
        }
        return Err(error.into());
    }
    Ok(())
}

pub async fn install(
    directory: &Path,
    version: Option<&str>,
    force: bool,
    progress: install::ProgressFn,
) -> Result<InstallJavaTestResult> {
    let version = version.unwrap_or(VERSION);
    if version.is_empty()
        || !version
            .split('.')
            .all(|part| !part.is_empty() && part.bytes().all(|c| c.is_ascii_digit()))
    {
        bail!("Invalid Java Test Runner version: {version:?}");
    }
    let destination = directory.join(version);
    if !force {
        if let Ok(text) = tokio::fs::read_to_string(destination.join("test-install.json")).await {
            if let Ok(info) = serde_json::from_str::<Manifest>(&text) {
                if info.version == version
                    && info
                        .bundles
                        .iter()
                        .any(|name| name.starts_with("com.microsoft.java.test.plugin-"))
                    && info
                        .bundles
                        .iter()
                        .all(|name| jar_name(name) && destination.join(name).is_file())
                {
                    return Ok(InstallJavaTestResult {
                        version: info.version,
                        bundles: info.bundles,
                        already_installed: true,
                    });
                }
            }
        }
    }
    install::require_tool("Java Test Runner", "curl")?;
    install::require_tool("Java Test Runner", "unzip")?;
    install::require_checksum_tool()?;
    let base = format!("{API}/{version}");
    progress(None, format!("Looking up Java Test Runner {version}"));
    let metadata = install::run(
        &["curl".into(), "-fsSL".into(), base.clone()],
        "Java Test Runner metadata",
    )
    .await?;
    let metadata: serde_json::Value = serde_json::from_str(&metadata)?;
    let url = metadata["files"]["download"]
        .as_str()
        .context("Java Test Runner has no download URL")?;
    let digest_url = metadata["files"]["sha256"]
        .as_str()
        .context("Java Test Runner has no SHA-256 URL")?;
    if !url.starts_with(&format!("{base}/file/"))
        || !digest_url.starts_with(&format!("{base}/file/"))
    {
        bail!("Java Test Runner URLs are outside the requested Open VSX release");
    }
    let digest = install::run(
        &["curl".into(), "-fsSL".into(), digest_url.into()],
        "Java Test Runner checksum",
    )
    .await?;
    let checksum = install::parse_jdtls_sha256(&digest)?;
    tokio::fs::create_dir_all(directory).await?;
    let staging = tempfile::tempdir_in(directory)?;
    let vsix = staging.path().join("java-test.vsix");
    install::download_with_progress("Java Test Runner", url, &vsix, &progress).await?;
    progress(Some(90), "Verifying Java Test Runner download".into());
    install::verify_checksum(&vsix, &checksum).await?;
    let vsix = vsix.to_string_lossy().into_owned();
    let listing = install::run(
        &["unzip".into(), "-Z1".into(), vsix.clone()],
        "Java Test Runner VSIX listing",
    )
    .await?;
    let manifest = install::run(
        &[
            "unzip".into(),
            "-p".into(),
            vsix.clone(),
            "extension/package.json".into(),
        ],
        "Java Test Runner manifest",
    )
    .await?;
    let info = payload(&manifest, &listing, version)?;
    let staged = staging.path().join("payload");
    tokio::fs::create_dir(&staged).await?;
    progress(Some(93), "Extracting Java Test Runner bundles".into());
    let mut command = vec!["unzip".into(), "-jqo".into(), vsix];
    command.extend(
        info.bundles
            .iter()
            .map(|name| format!("extension/server/{name}")),
    );
    command.extend(["-d".into(), staged.to_string_lossy().into_owned()]);
    install::run(&command, "Java Test Runner bundle extraction").await?;
    if !info.bundles.iter().all(|name| staged.join(name).is_file()) {
        bail!("Java Test Runner extraction left missing bundles");
    }
    tokio::fs::write(
        staged.join("test-install.json"),
        serde_json::to_vec_pretty(&info)?,
    )
    .await?;
    progress(Some(99), "Installing Java Test Runner bundles".into());
    publish(&staged, &destination, &staging.path().join("previous")).await?;
    Ok(InstallJavaTestResult {
        version: info.version,
        bundles: info.bundles,
        already_installed: false,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_payload_installs_manifest_dependencies_and_rejects_unsafe_paths() {
        let plugin = "com.microsoft.java.test.plugin-0.43.1.jar";
        let package = |paths| {
            json!({"name":"vscode-java-test", "version":VERSION, "contributes":{"javaExtensions":paths}}).to_string()
        };
        let listing = format!("extension/server/{plugin}\nextension/server/junit-api.jar\nextension/server/jacocoagent.jar\n");
        let info = payload(
            &package(vec![
                format!("./server/{plugin}"),
                "./server/junit-api.jar".into(),
            ]),
            &listing,
            VERSION,
        )
        .unwrap();
        assert_eq!(info.bundles, [plugin, "junit-api.jar"]);
        for path in [
            "./server/../escape.jar",
            "./server/nested/a.jar",
            "./server/missing.jar",
            "./server/*.jar",
            "/tmp/a.jar",
        ] {
            assert!(payload(
                &package(vec![format!("./server/{plugin}"), path.into()]),
                &listing,
                VERSION
            )
            .is_err());
        }
        assert!(payload(
            &package(vec![
                format!("./server/{plugin}"),
                format!("./server/{plugin}")
            ]),
            &listing,
            VERSION
        )
        .is_err());
        assert!(payload(
            &package(vec!["./server/junit-api.jar".into()]),
            &listing,
            VERSION
        )
        .is_err());
    }

    #[tokio::test]
    async fn test_navigation_forwarding_preserves_boolean_arguments_and_result() {
        let uri = "file:///project/Widget.java";
        let (client, _) = crate::test_support::client("jdtls", 100, uri, true).await;
        for direction in [true, false] {
            let forward: crate::lsp_ext::ForwardRequestParams = serde_json::from_value(json!({
                "uri":uri, "serverName":"jdtls", "method":"workspace/executeCommand",
                "params":{"command":"vscode.java.test.navigateToTestOrTarget", "arguments":[uri,direction]}
            })).unwrap();
            let response = crate::handlers::request::handle_forward_request(
                crate::test_support::request("emacs/forwardRequest", None),
                forward,
                vec![client.clone()],
            )
            .await
            .unwrap();
            assert_eq!(
                response.result.unwrap(),
                json!({"server":"jdtls", "arguments":[uri,direction]})
            );
        }
        client.stop_and_wait().await.unwrap();
    }

    #[tokio::test]
    async fn test_publish_failure_restores_previous_installation() {
        let temp = tempfile::tempdir().unwrap();
        let destination = temp.path().join("release");
        std::fs::create_dir(&destination).unwrap();
        std::fs::write(destination.join("keep"), "old").unwrap();
        assert!(publish(
            &temp.path().join("missing"),
            &destination,
            &temp.path().join("backup")
        )
        .await
        .is_err());
        assert_eq!(
            std::fs::read_to_string(destination.join("keep")).unwrap(),
            "old"
        );
    }
}
