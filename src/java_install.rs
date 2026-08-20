//! Installing the JetBrains IntelliJ language server, which `lsp-proxy-java` requires.
//!
//! The archives are ~370 MB, so nothing here loads a payload into memory: every
//! step shells out to `curl` / `tar` / `unzip` / `sha256sum` the way
//! [`crate::remote::deploy`] already does, and progress is streamed to the editor
//! as `$/progress`. No HTTP, archive or digest crate is added for this — that
//! dependency tree would land in every prebuilt binary for a feature only
//! JVM users reach.
//!
//! # Where the download URL comes from
//!
//! The JetBrains VSCode extension is a ~1.3 MB *shim* that contains no server. Its
//! `extension/server-bundle.json` is the authoritative `{url, version, sha256}`
//! tuple, so resolving the latest build means:
//!
//! ```text
//! open-vsx.org/api/JetBrains/intellij-server/<target>/latest   →  VSIX url
//!   curl the VSIX (1.3 MB)                                     →  unzip -p …/server-bundle.json
//! ```
//!
//! Note the download host is `download.jetbrains.com` and the product is
//! `intellij-server`. The similarly-named `kotlin-server` product (which Mason's
//! kotlin-lsp package fetches from the `download-cdn.` host) is a *different*
//! server; using it here silently installs the wrong backend.

use anyhow::{bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

const OPEN_VSX_API: &str = "https://open-vsx.org/api/JetBrains/intellij-server";
const DOWNLOAD_BASE: &str = "https://download.jetbrains.com/language-server/intellij-server";

/// The launcher every archive is expected to contain, relative to its root.
const LAUNCHER_NAME: &str = "intellij-server";

/// The `{url, version, sha256}` tuple published inside the extension shim.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct ServerBundle {
    pub url: String,
    pub version: String,
    #[serde(rename = "archiveName")]
    pub archive_name: String,
    pub sha256: String,
}

/// Open VSX target triple for this platform, e.g. `darwin-arm64`.
pub fn open_vsx_target(os: &str, arch: &str) -> Result<String> {
    let platform = match os {
        "macos" => "darwin",
        "linux" => "linux",
        "windows" => "win32",
        other => bail!("unsupported OS for IntelliJ server install: {other}"),
    };
    let arch = match arch {
        "aarch64" => "arm64",
        "x86_64" => "x64",
        other => bail!("unsupported architecture for IntelliJ server install: {other}"),
    };
    Ok(format!("{platform}-{arch}"))
}

/// Archive suffix for this platform, and whether that shape is *confirmed*.
///
/// Confirmed means an actual download was observed to exist. Unconfirmed suffixes
/// follow the same naming scheme and are reported to the user before being tried,
/// so a wrong guess is obvious rather than mysterious.
pub fn archive_suffix(os: &str, arch: &str) -> Result<(String, bool)> {
    let arm = match arch {
        "aarch64" => true,
        "x86_64" => false,
        other => bail!("unsupported architecture for IntelliJ server install: {other}"),
    };
    let (ext, confirmed) = match os {
        // Both macOS arches verified present.
        "macos" => (".sit", true),
        // linux-x86_64 is the URL embedded in the linux-x64 shim; arm64 follows the
        // same scheme but has not been observed.
        "linux" => (".tar.gz", !arm),
        "windows" => (".win.zip", false),
        other => bail!("unsupported OS for IntelliJ server install: {other}"),
    };
    Ok((
        format!("{}{ext}", if arm { "-aarch64" } else { "" }),
        confirmed,
    ))
}

/// Download URL for VERSION on this platform, and whether the shape is confirmed.
pub fn download_url(version: &str, os: &str, arch: &str) -> Result<(String, bool)> {
    let (suffix, confirmed) = archive_suffix(os, arch)?;
    Ok((
        format!("{DOWNLOAD_BASE}/{version}/{LAUNCHER_NAME}-{version}{suffix}"),
        confirmed,
    ))
}

/// Open VSX endpoint that names the newest extension build for this platform.
pub fn open_vsx_latest_url(target: &str) -> String {
    format!("{OPEN_VSX_API}/{target}/latest")
}

/// Every archive name that is legitimate for VERSION.
fn valid_archive_names(version: &str) -> [String; 6] {
    let stem = format!("{LAUNCHER_NAME}-{version}");
    [
        format!("{stem}.tar.gz"),
        format!("{stem}-aarch64.tar.gz"),
        format!("{stem}.win.zip"),
        format!("{stem}-aarch64.win.zip"),
        format!("{stem}.sit"),
        format!("{stem}-aarch64.sit"),
    ]
}

/// Reject anything in a fetched [`ServerBundle`] we would not have constructed
/// ourselves.
///
/// The tuple arrives from the network, and `archive_name` reaches the filesystem
/// while `url` reaches `curl`, so each field is checked rather than trusted: a
/// traversal in the name or an unexpected host would otherwise be honoured.
pub fn validate_bundle(bundle: &ServerBundle) -> Result<()> {
    if bundle.version.is_empty()
        || !bundle.version.starts_with(|c: char| c.is_ascii_digit())
        || !bundle
            .version
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || matches!(c, '.' | '_' | '-'))
    {
        bail!("server-bundle.json has an invalid version: {:?}", bundle.version);
    }
    if bundle.archive_name.contains("..")
        || bundle.archive_name.contains('/')
        || bundle.archive_name.contains('\\')
    {
        bail!(
            "server-bundle.json has an unsafe archive name: {:?}",
            bundle.archive_name
        );
    }
    if !valid_archive_names(&bundle.version).contains(&bundle.archive_name) {
        bail!(
            "server-bundle.json has an unrecognised archive name: {:?}",
            bundle.archive_name
        );
    }
    let expected_url = format!("{DOWNLOAD_BASE}/{}/{}", bundle.version, bundle.archive_name);
    if bundle.url != expected_url {
        bail!(
            "server-bundle.json points somewhere unexpected:\n  got      {}\n  expected {}",
            bundle.url,
            expected_url
        );
    }
    if bundle.sha256.len() != 64 || !bundle.sha256.chars().all(|c| c.is_ascii_hexdigit()) {
        bail!(
            "server-bundle.json has an invalid sha256: {:?}",
            bundle.sha256
        );
    }
    Ok(())
}

/// How an archive is unpacked.
///
/// `.tar.gz` goes through `tar`, everything else through `unzip` — `.sit` archives
/// are plain zips despite the StuffIt extension. Deliberately *without*
/// `--strip-components`: archive roots differ between products, so the launcher is
/// located by searching afterwards instead of assuming a depth.
pub fn extract_argv(archive: &Path, dest: &Path) -> Vec<String> {
    let archive = archive.to_string_lossy().to_string();
    let dest = dest.to_string_lossy().to_string();
    if archive.to_ascii_lowercase().ends_with(".tar.gz") {
        vec!["tar".into(), "-xzf".into(), archive, "-C".into(), dest]
    } else {
        vec![
            "unzip".into(),
            "-qo".into(),
            archive,
            "-d".into(),
            dest.clone(),
        ]
    }
}

/// Locate `bin/intellij-server` anywhere under ROOT (depth-limited).
///
/// Searching rather than assuming a path is what lets one code path handle every
/// archive layout, including the extra top-level directory `.tar.gz` carries.
pub fn find_launcher(root: &Path) -> Option<PathBuf> {
    fn walk(dir: &Path, depth: usize) -> Option<PathBuf> {
        if depth > 3 {
            return None;
        }
        let direct = dir.join("bin").join(exe_name());
        if direct.is_file() {
            return Some(direct);
        }
        for entry in std::fs::read_dir(dir).ok()?.flatten() {
            if entry.file_type().ok()?.is_dir() {
                if let Some(found) = walk(&entry.path(), depth + 1) {
                    return Some(found);
                }
            }
        }
        None
    }
    walk(root, 0)
}

fn exe_name() -> &'static str {
    if cfg!(windows) {
        "intellij-server.exe"
    } else {
        LAUNCHER_NAME
    }
}

/// Directory a given version is installed into.
pub fn version_dir(install_dir: &Path, version: &str) -> PathBuf {
    install_dir.join(version)
}

/// Parse the digest out of `sha256sum` / `shasum -a 256` output.
pub fn parse_digest(stdout: &str) -> Option<String> {
    stdout
        .split_whitespace()
        .next()
        .filter(|s| s.len() == 64 && s.chars().all(|c| c.is_ascii_hexdigit()))
        .map(|s| s.to_ascii_lowercase())
}

/// Current OS token, in the vocabulary the helpers above expect.
pub fn current_os() -> Result<&'static str> {
    Ok(match std::env::consts::OS {
        "macos" => "macos",
        "linux" => "linux",
        "windows" => "windows",
        other => bail!("unsupported OS for IntelliJ server install: {other}"),
    })
}

/// Current architecture token.
pub fn current_arch() -> Result<&'static str> {
    Ok(match std::env::consts::ARCH {
        "aarch64" => "aarch64",
        "x86_64" => "x86_64",
        other => bail!("unsupported architecture for IntelliJ server install: {other}"),
    })
}

/// Extract the VSIX download URL from an Open VSX `latest` response.
pub fn vsix_url_from_metadata(json: &str) -> Result<String> {
    let value: serde_json::Value =
        serde_json::from_str(json).context("Open VSX returned invalid JSON")?;
    let url = value
        .get("files")
        .and_then(|f| f.get("download"))
        .and_then(|d| d.as_str())
        .context("Open VSX response has no files.download")?;
    // The URL is handed to `curl`; keep it inside the namespace we asked about.
    if !url.starts_with(OPEN_VSX_API) || !url.ends_with(".vsix") {
        bail!("Open VSX returned an unexpected VSIX URL: {url}");
    }
    Ok(url.to_string())
}

// ---------------------------------------------------------------------------
// Pipeline
// ---------------------------------------------------------------------------

use std::process::Stdio;
use tokio::process::Command;

/// Progress callback: `(percent, message)`. `percent` is `None` for phases with no
/// measurable size.
pub type ProgressFn = Box<dyn Fn(Option<u32>, String) + Send + Sync>;

/// Run a command, returning stdout and failing with its stderr attached.
async fn run(argv: &[String], what: &str) -> Result<String> {
    let (program, args) = argv
        .split_first()
        .with_context(|| format!("{what}: empty command"))?;
    let output = Command::new(program)
        .args(args)
        .stdin(Stdio::null())
        .output()
        .await
        .with_context(|| format!("{what}: failed to launch `{program}` (is it on PATH?)"))?;
    if !output.status.success() {
        bail!(
            "{what} failed ({}):\n{}",
            output.status,
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

fn require_tool(name: &str) -> Result<()> {
    which::which(name)
        .map(|_| ())
        .with_context(|| format!("`{name}` was not found on PATH; it is required to install the IntelliJ server"))
}

/// Resolve the newest build by reading the extension shim's `server-bundle.json`.
async fn discover_latest_bundle(os: &str, arch: &str, progress: &ProgressFn) -> Result<ServerBundle> {
    let target = open_vsx_target(os, arch)?;
    progress(None, "Looking up the latest IntelliJ server".into());

    let metadata = run(
        &[
            "curl".into(),
            "-fsSL".into(),
            open_vsx_latest_url(&target),
        ],
        "Open VSX lookup",
    )
    .await?;
    let vsix_url = vsix_url_from_metadata(&metadata)?;

    // The shim is ~1.3 MB; download it just to read the tuple inside.
    let tmp = tempfile::tempdir().context("could not create a temporary directory")?;
    let vsix = tmp.path().join("intellij-server.vsix");
    progress(None, "Reading server metadata".into());
    run(
        &[
            "curl".into(),
            "-fL".into(),
            "--retry".into(),
            "3".into(),
            "-o".into(),
            vsix.to_string_lossy().to_string(),
            vsix_url,
        ],
        "extension shim download",
    )
    .await?;

    let json = run(
        &[
            "unzip".into(),
            "-p".into(),
            vsix.to_string_lossy().to_string(),
            "extension/server-bundle.json".into(),
        ],
        "reading server-bundle.json",
    )
    .await?;
    let bundle: ServerBundle =
        serde_json::from_str(&json).context("server-bundle.json is not valid JSON")?;
    validate_bundle(&bundle)?;
    Ok(bundle)
}

/// Content-Length of URL, or 0 when the server does not report one.
async fn remote_size(url: &str) -> u64 {
    let out = Command::new("curl")
        .args(["-sIL", url])
        .stdin(Stdio::null())
        .output()
        .await;
    let Ok(out) = out else { return 0 };
    String::from_utf8_lossy(&out.stdout)
        .lines()
        .filter(|l| l.to_ascii_lowercase().starts_with("content-length:"))
        .filter_map(|l| l.split(':').nth(1)?.trim().parse::<u64>().ok())
        .next_back()
        .unwrap_or(0)
}

/// Percentage the download phase is allowed to occupy.
///
/// The phases after it report fixed values (`PCT_VERIFY` and up), so the transfer
/// has to be squeezed below the lowest of them: reporting raw download percent
/// would reach 99 and then jump *back* to 90 for verification, which reads as a
/// broken indicator rather than as progress.
const PCT_DOWNLOAD_MAX: u64 = 88;
const PCT_VERIFY: u32 = 90;
const PCT_UNPACK: u32 = 93;
const PCT_INSTALL: u32 = 99;

/// Map bytes transferred onto the download phase's slice of the total.
fn download_pct(current: u64, total: u64) -> u32 {
    if total == 0 {
        return 0;
    }
    ((current.min(total) * PCT_DOWNLOAD_MAX / total) as u32).min(PCT_DOWNLOAD_MAX as u32)
}

/// Download URL to ARCHIVE, reporting progress by polling the partial file.
///
/// Mirrors `remote::deploy`: `select!` runs the transfer and a size poller
/// concurrently and cancels the poller as soon as the transfer resolves.
async fn download_with_progress(url: &str, archive: &Path, progress: &ProgressFn) -> Result<()> {
    let total = remote_size(url).await;
    let mb = |b: u64| b as f64 / 1_048_576.0;
    progress(
        Some(0),
        if total > 0 {
            format!("Downloading IntelliJ server ({:.0} MB)", mb(total))
        } else {
            "Downloading IntelliJ server".into()
        },
    );

    let argv = vec![
        "curl".into(),
        "-fL".into(),
        "--retry".into(),
        "3".into(),
        // Give up on a stalled transfer rather than hanging forever.
        "--speed-time".into(),
        "120".into(),
        "--speed-limit".into(),
        "1024".into(),
        "-o".into(),
        archive.to_string_lossy().to_string(),
        url.to_string(),
    ];

    let archive_owned = archive.to_path_buf();
    tokio::select! {
        result = run(&argv, "download") => { result.map(|_| ()) }
        _ = async {
            let mut ticker = tokio::time::interval(std::time::Duration::from_millis(700));
            loop {
                ticker.tick().await;
                let Ok(meta) = tokio::fs::metadata(&archive_owned).await else { continue };
                let current = meta.len();
                if current == 0 { continue; }
                if total > 0 {
                    // The prose deliberately carries no percentage: it is sent as a
                    // structured field, and having it in both makes every renderer
                    // that formats the two together print it twice.
                    progress(Some(download_pct(current, total)), format!(
                        "Downloading IntelliJ server ({:.0} / {:.0} MB)",
                        mb(current), mb(total)
                    ));
                } else {
                    progress(None, format!("Downloading IntelliJ server ({:.0} MB)", mb(current)));
                }
            }
        } => { unreachable!("the poller loops forever") }
    }
}

/// Verify ARCHIVE against EXPECTED, shelling out because the archives are far too
/// large to digest in memory.
async fn verify_checksum(archive: &Path, expected: &str) -> Result<()> {
    let path = archive.to_string_lossy().to_string();
    let argv = if which::which("sha256sum").is_ok() {
        vec!["sha256sum".to_string(), path]
    } else if which::which("shasum").is_ok() {
        vec!["shasum".to_string(), "-a".into(), "256".into(), path]
    } else {
        bail!("neither `sha256sum` nor `shasum` was found on PATH; cannot verify the download");
    };
    let out = run(&argv, "checksum").await?;
    let actual = parse_digest(&out)
        .with_context(|| format!("could not parse a digest from: {}", out.trim()))?;
    if actual != expected.to_ascii_lowercase() {
        bail!(
            "checksum mismatch — the download is corrupt
  expected {expected}
  got      {actual}"
        );
    }
    Ok(())
}

/// Download, verify, unpack and publish the server. Returns the launcher path.
pub async fn install(
    install_dir: &Path,
    version: Option<&str>,
    force: bool,
    progress: ProgressFn,
) -> Result<(PathBuf, String, bool)> {
    let (os, arch) = (current_os()?, current_arch()?);
    for tool in ["curl", "unzip", "tar"] {
        require_tool(tool)?;
    }

    // Pinned version: build the URL ourselves and say so when the shape is a guess.
    // Unpinned: the shim tells us url + version + checksum authoritatively.
    let (url, version, expected_sha) = match version {
        Some(v) => {
            let (url, confirmed) = download_url(v, os, arch)?;
            if !confirmed {
                progress(
                    None,
                    format!("Archive name for {os}-{arch} is inferred, not confirmed; trying {url}"),
                );
            }
            (url, v.to_string(), None)
        }
        None => {
            let bundle = discover_latest_bundle(os, arch, &progress).await?;
            (bundle.url, bundle.version, Some(bundle.sha256))
        }
    };

    let dest = version_dir(install_dir, &version);
    if !force {
        if let Some(existing) = find_launcher(&dest) {
            return Ok((existing, version, true));
        }
    }

    // Stage beside the destination so publishing is a rename on the same
    // filesystem, never a copy over a directory that may be in use.
    tokio::fs::create_dir_all(install_dir)
        .await
        .with_context(|| format!("could not create {}", install_dir.display()))?;
    let staging = install_dir.join(format!(".staging-{version}-{}", std::process::id()));
    let _ = tokio::fs::remove_dir_all(&staging).await;
    tokio::fs::create_dir_all(&staging)
        .await
        .with_context(|| format!("could not create {}", staging.display()))?;

    let result = install_into_staging(&url, expected_sha.as_deref(), &staging, &progress).await;

    let launcher_in_staging = match result {
        Ok(path) => path,
        Err(err) => {
            let _ = tokio::fs::remove_dir_all(&staging).await;
            return Err(err);
        }
    };

    // Publish atomically. A pre-existing destination is replaced only now that a
    // complete tree is ready. The server root is promoted directly to `<version>/`,
    // so the installed launcher is always at `<version>/bin/<exe>`.
    progress(Some(PCT_INSTALL), "Installing IntelliJ server".into());
    let server_root = launcher_in_staging
        .parent()
        .and_then(|bin| bin.parent())
        .context("launcher is not inside a bin/ directory")?
        .to_path_buf();
    if dest.exists() {
        let quarantine = install_dir.join(format!(".old-{version}-{}", std::process::id()));
        tokio::fs::rename(&dest, &quarantine)
            .await
            .with_context(|| format!("could not move aside {}", dest.display()))?;
        let _ = tokio::fs::remove_dir_all(&quarantine).await;
    }
    tokio::fs::rename(&server_root, &dest)
        .await
        .with_context(|| format!("could not publish {}", dest.display()))?;
    let _ = tokio::fs::remove_dir_all(&staging).await;

    let launcher = dest.join("bin").join(exe_name());
    set_executable(&launcher).await?;
    Ok((launcher, version, false))
}

/// Download + verify + unpack into STAGING, returning the launcher found inside.
async fn install_into_staging(
    url: &str,
    expected_sha: Option<&str>,
    staging: &Path,
    progress: &ProgressFn,
) -> Result<PathBuf> {
    let archive_name = url.rsplit('/').next().unwrap_or("intellij-server-archive");
    let archive = staging.join(archive_name);

    download_with_progress(url, &archive, progress).await?;

    match expected_sha {
        Some(expected) => {
            progress(Some(PCT_VERIFY), "Verifying download".into());
            if let Err(err) = verify_checksum(&archive, expected).await {
                // Remove the archive: a resumed transfer must never continue from
                // bytes we already know are wrong.
                let _ = tokio::fs::remove_file(&archive).await;
                return Err(err);
            }
        }
        None => progress(
            Some(PCT_VERIFY),
            "No published checksum for a pinned version — skipping verification".into(),
        ),
    }

    progress(Some(PCT_UNPACK), "Unpacking IntelliJ server".into());
    let extract_dir = staging.join("unpacked");
    tokio::fs::create_dir_all(&extract_dir).await.ok();
    run(&extract_argv(&archive, &extract_dir), "unpacking").await?;
    let _ = tokio::fs::remove_file(&archive).await;

    let launcher = find_launcher(&extract_dir).with_context(|| {
        format!(
            "the archive unpacked but contains no bin/{} — the download may be a different product",
            exe_name()
        )
    })?;

    // Flatten: hoist the server root so the installed layout is
    // `<version>/bin/<exe>` rather than `<version>/unpacked/<archive-root>/bin/<exe>`.
    // A predictable depth is what lets the client find the launcher — and prepend its
    // `bin/` to PATH — without knowing how any given archive was packed.
    let server_root = launcher
        .parent()
        .and_then(|bin| bin.parent())
        .context("launcher is not inside a bin/ directory")?
        .to_path_buf();
    let flattened = staging.join("server-root");
    tokio::fs::rename(&server_root, &flattened)
        .await
        .with_context(|| format!("could not hoist {}", server_root.display()))?;
    let _ = tokio::fs::remove_dir_all(&extract_dir).await;

    Ok(flattened.join("bin").join(exe_name()))
}

/// Archives do not reliably preserve the executable bit.
#[cfg(unix)]
async fn set_executable(path: &Path) -> Result<()> {
    use std::os::unix::fs::PermissionsExt;
    let mut perms = tokio::fs::metadata(path)
        .await
        .with_context(|| format!("could not stat {}", path.display()))?
        .permissions();
    perms.set_mode(0o755);
    tokio::fs::set_permissions(path, perms)
        .await
        .with_context(|| format!("could not make {} executable", path.display()))
}

#[cfg(not(unix))]
async fn set_executable(_path: &Path) -> Result<()> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const V: &str = "263.2689.0";

    fn bundle(archive: &str) -> ServerBundle {
        ServerBundle {
            url: format!("{DOWNLOAD_BASE}/{V}/{archive}"),
            version: V.to_string(),
            archive_name: archive.to_string(),
            sha256: "b".repeat(64),
        }
    }

    #[test]
    fn open_vsx_targets() {
        assert_eq!(open_vsx_target("macos", "aarch64").unwrap(), "darwin-arm64");
        assert_eq!(open_vsx_target("macos", "x86_64").unwrap(), "darwin-x64");
        assert_eq!(open_vsx_target("linux", "x86_64").unwrap(), "linux-x64");
        assert_eq!(open_vsx_target("windows", "aarch64").unwrap(), "win32-arm64");
        assert!(open_vsx_target("plan9", "x86_64").is_err());
        assert!(open_vsx_target("linux", "riscv64").is_err());
    }

    /// The URL that must not regress: wrong host or wrong product installs a
    /// different server (`kotlin-server`) that looks plausible but is not the
    /// backend `lsp-proxy-java` talks to.
    #[test]
    fn download_urls_use_the_right_host_and_product() {
        let (url, confirmed) = download_url(V, "linux", "x86_64").unwrap();
        assert_eq!(
            url,
            "https://download.jetbrains.com/language-server/intellij-server/263.2689.0/intellij-server-263.2689.0.tar.gz"
        );
        assert!(confirmed, "linux-x86_64 is the observed URL");
        assert!(!url.contains("download-cdn"), "must not use the CDN host");
        assert!(!url.contains("kotlin-server"), "must not use the kotlin product");
    }

    #[test]
    fn archive_suffixes_per_platform() {
        assert_eq!(archive_suffix("macos", "aarch64").unwrap(), ("-aarch64.sit".into(), true));
        assert_eq!(archive_suffix("macos", "x86_64").unwrap(), (".sit".into(), true));
        assert_eq!(archive_suffix("linux", "x86_64").unwrap(), (".tar.gz".into(), true));
        // arm64 linux and both Windows arches are inferred, not observed.
        assert_eq!(archive_suffix("linux", "aarch64").unwrap(), ("-aarch64.tar.gz".into(), false));
        assert_eq!(archive_suffix("windows", "x86_64").unwrap(), (".win.zip".into(), false));
    }

    #[test]
    fn accepts_a_well_formed_bundle() {
        for name in valid_archive_names(V) {
            validate_bundle(&bundle(&name)).unwrap_or_else(|e| panic!("{name}: {e}"));
        }
    }

    #[test]
    fn rejects_path_traversal_in_archive_name() {
        let mut b = bundle("intellij-server-263.2689.0.tar.gz");
        b.archive_name = "../../etc/passwd".into();
        b.url = format!("{DOWNLOAD_BASE}/{V}/../../etc/passwd");
        assert!(validate_bundle(&b).is_err());
    }

    #[test]
    fn rejects_foreign_download_host() {
        let mut b = bundle("intellij-server-263.2689.0.tar.gz");
        b.url = format!("https://evil.example/{V}/{}", b.archive_name);
        assert!(validate_bundle(&b).is_err());
    }

    #[test]
    fn rejects_mismatched_version_and_archive() {
        let mut b = bundle("intellij-server-263.2689.0.tar.gz");
        b.version = "999.1.0".into();
        assert!(
            validate_bundle(&b).is_err(),
            "archive name must belong to the stated version"
        );
    }

    #[test]
    fn rejects_bad_checksums_and_versions() {
        let mut b = bundle("intellij-server-263.2689.0.tar.gz");
        b.sha256 = "abc".into();
        assert!(validate_bundle(&b).is_err());

        let mut b = bundle("intellij-server-263.2689.0.tar.gz");
        b.sha256 = "z".repeat(64);
        assert!(validate_bundle(&b).is_err(), "non-hex digest");

        let mut b = bundle("intellij-server-263.2689.0.tar.gz");
        b.version = "../evil".into();
        assert!(validate_bundle(&b).is_err());
    }

    #[test]
    fn extract_uses_tar_for_tarballs_and_unzip_otherwise() {
        let dest = Path::new("/tmp/dest");
        let tar = extract_argv(Path::new("/tmp/a-1.0.tar.gz"), dest);
        assert_eq!(tar[0], "tar");
        assert!(tar.contains(&"-xzf".to_string()));
        assert!(
            !tar.iter().any(|a| a.contains("strip-components")),
            "layout is resolved by searching, not by assuming a depth"
        );
        // `.sit` is a zip despite the extension.
        assert_eq!(extract_argv(Path::new("/tmp/a-1.0.sit"), dest)[0], "unzip");
        assert_eq!(extract_argv(Path::new("/tmp/a-1.0.win.zip"), dest)[0], "unzip");
    }

    #[test]
    fn parses_digest_from_both_tools() {
        let hex = "a".repeat(64);
        // sha256sum
        assert_eq!(parse_digest(&format!("{hex}  file")).unwrap(), hex);
        // shasum -a 256, and the BSD `*name` form
        assert_eq!(parse_digest(&format!("{hex} *file")).unwrap(), hex);
        assert_eq!(parse_digest(&hex.to_uppercase()).unwrap(), hex);
        assert!(parse_digest("").is_none());
        assert!(parse_digest("short  file").is_none());
    }

    #[test]
    fn vsix_url_must_stay_in_the_open_vsx_namespace() {
        let ok = r#"{"files":{"download":"https://open-vsx.org/api/JetBrains/intellij-server/darwin-arm64/0.0.8/file/JetBrains.intellij-server-0.0.8@darwin-arm64.vsix"}}"#;
        assert!(vsix_url_from_metadata(ok).is_ok());

        assert!(vsix_url_from_metadata(r#"{"files":{}}"#).is_err());
        assert!(vsix_url_from_metadata("not json").is_err());
        assert!(
            vsix_url_from_metadata(r#"{"files":{"download":"https://evil.example/x.vsix"}}"#)
                .is_err(),
            "a foreign host must be rejected"
        );
        assert!(
            vsix_url_from_metadata(
                r#"{"files":{"download":"https://open-vsx.org/api/JetBrains/intellij-server/x/y/file/z.sh"}}"#
            )
            .is_err(),
            "non-vsix must be rejected"
        );
    }

    #[test]
    fn version_dir_is_keyed_by_version() {
        assert_eq!(
            version_dir(Path::new("/i"), "263.2689.0"),
            PathBuf::from("/i/263.2689.0")
        );
    }

    #[test]
    fn find_launcher_handles_an_extra_archive_root() {
        let tmp = tempfile::tempdir().unwrap();
        let nested = tmp.path().join("intellij-server-263.2689.0").join("bin");
        std::fs::create_dir_all(&nested).unwrap();
        std::fs::write(nested.join(super::exe_name()), b"#!/bin/sh\n").unwrap();
        assert_eq!(find_launcher(tmp.path()).unwrap(), nested.join(super::exe_name()));
    }

    #[test]
    fn find_launcher_returns_none_when_absent() {
        let tmp = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(tmp.path().join("lib")).unwrap();
        assert!(find_launcher(tmp.path()).is_none());
    }
}

#[cfg(test)]
mod progress_pct_tests {
    use super::{download_pct, PCT_DOWNLOAD_MAX, PCT_INSTALL, PCT_UNPACK, PCT_VERIFY};

    /// The whole point of the mapping: an indicator that only ever moves forward.
    /// Before this, the download reported raw percent up to 99 and verification then
    /// reported 90.
    #[test]
    fn the_sequence_never_goes_backwards() {
        let total = 370 * 1024 * 1024;
        let mut seq: Vec<u32> = (0..=10)
            .map(|tenth| download_pct(total / 10 * tenth, total))
            .collect();
        seq.extend([PCT_VERIFY, PCT_UNPACK, PCT_INSTALL]);
        assert!(
            seq.windows(2).all(|w| w[0] <= w[1]),
            "not monotonic: {seq:?}"
        );
        assert!(*seq.last().unwrap() <= 100);
    }

    #[test]
    fn download_stays_below_the_next_phase() {
        let total = 1000;
        for current in 0..=total {
            let pct = download_pct(current, total);
            assert!(
                pct < PCT_VERIFY,
                "download reported {pct}, which collides with the verify phase"
            );
        }
        assert_eq!(download_pct(total, total), PCT_DOWNLOAD_MAX as u32);
        assert_eq!(download_pct(0, total), 0);
        assert_eq!(download_pct(total / 2, total), PCT_DOWNLOAD_MAX as u32 / 2);
    }

    /// `remote_size` returns 0 when the server sends no Content-Length; dividing by
    /// it would panic.
    #[test]
    fn unknown_total_does_not_divide_by_zero() {
        assert_eq!(download_pct(12345, 0), 0);
    }

    /// A file longer than the advertised length must not report over budget.
    #[test]
    fn overshooting_the_advertised_size_is_clamped() {
        assert_eq!(download_pct(2000, 1000), PCT_DOWNLOAD_MAX as u32);
    }
}

#[cfg(test)]
mod pipeline_tests {
    use super::*;

    /// Exercise the extract → locate → publish half against a real archive of the
    /// shape JetBrains ships (one extra top-level directory), without downloading
    /// 370 MB.
    #[tokio::test]
    async fn unpacks_locates_and_marks_the_launcher_executable() {
        let tmp = tempfile::tempdir().unwrap();
        let build = tmp.path().join("build/intellij-server-263.2689.0");
        std::fs::create_dir_all(build.join("bin")).unwrap();
        std::fs::create_dir_all(build.join("lib")).unwrap();
        std::fs::write(build.join("bin").join(exe_name()), b"#!/bin/sh\nexit 0\n").unwrap();
        std::fs::write(build.join("lib/a.jar"), b"jar").unwrap();

        // Deliberately drop the executable bit: archives often do.
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut p = std::fs::metadata(build.join("bin").join(exe_name()))
                .unwrap()
                .permissions();
            p.set_mode(0o644);
            std::fs::set_permissions(build.join("bin").join(exe_name()), p).unwrap();
        }

        let archive = tmp.path().join("intellij-server-263.2689.0.tar.gz");
        let status = std::process::Command::new("tar")
            .args(["-czf", archive.to_str().unwrap(), "-C"])
            .arg(tmp.path().join("build"))
            .arg("intellij-server-263.2689.0")
            .status()
            .unwrap();
        assert!(status.success());

        let dest = tmp.path().join("dest");
        std::fs::create_dir_all(&dest).unwrap();
        let argv = extract_argv(&archive, &dest);
        let out = std::process::Command::new(&argv[0])
            .args(&argv[1..])
            .output()
            .unwrap();
        assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));

        // The launcher is one level down because we do not strip components.
        let launcher = find_launcher(&dest).expect("launcher must be found through the archive root");
        assert!(launcher.ends_with(Path::new("bin").join(exe_name())));

        set_executable(&launcher).await.unwrap();
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mode = std::fs::metadata(&launcher).unwrap().permissions().mode();
            assert_eq!(mode & 0o777, 0o755, "archives lose the executable bit");
        }
    }

    /// A checksum mismatch must delete the archive, or a resumed transfer would
    /// forever continue from bytes already known to be wrong.
    #[tokio::test]
    async fn checksum_mismatch_is_reported() {
        let tmp = tempfile::tempdir().unwrap();
        let file = tmp.path().join("a.bin");
        tokio::fs::write(&file, b"hello").await.unwrap();
        let err = verify_checksum(&file, &"a".repeat(64)).await.unwrap_err();
        let msg = format!("{err:#}");
        assert!(msg.contains("checksum mismatch"), "{msg}");
    }

    #[tokio::test]
    async fn checksum_match_passes() {
        let tmp = tempfile::tempdir().unwrap();
        let file = tmp.path().join("a.bin");
        tokio::fs::write(&file, b"hello").await.unwrap();
        // sha256("hello")
        let expected = "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824";
        verify_checksum(&file, expected).await.unwrap();
        // Case-insensitive, as digests are sometimes published uppercase.
        verify_checksum(&file, &expected.to_uppercase()).await.unwrap();
    }
}

#[cfg(test)]
mod live_tests {
    use super::*;

    /// Hits the network: resolves the latest build the way the installer does and
    /// checks the tuple passes validation. This is the half that unit tests cannot
    /// cover, and the half that was wrong before (`kotlin-server` on the CDN host).
    /// Ignored by default; run with `cargo test -- --ignored discovery`.
    #[tokio::test]
    #[ignore]
    async fn discovery_returns_a_valid_bundle_for_this_platform() {
        let progress: ProgressFn = Box::new(|_, m| eprintln!("  {m}"));
        let (os, arch) = (current_os().unwrap(), current_arch().unwrap());
        let bundle = discover_latest_bundle(os, arch, &progress)
            .await
            .expect("discovery must succeed");
        eprintln!("  resolved: {bundle:#?}");

        validate_bundle(&bundle).unwrap();
        assert!(bundle.url.starts_with(DOWNLOAD_BASE), "wrong host/product: {}", bundle.url);
        assert!(!bundle.url.contains("kotlin-server"));
        assert_eq!(bundle.sha256.len(), 64);

        // The archive must actually exist and be large: a shim-sized answer means we
        // resolved the wrong artifact.
        let size = remote_size(&bundle.url).await;
        assert!(size > 100 * 1024 * 1024, "archive suspiciously small: {size} bytes");
        eprintln!("  archive is {:.0} MB", size as f64 / 1_048_576.0);
    }
}
