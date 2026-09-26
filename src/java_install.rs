//! Installing a Java language server backend for `lsp-proxy-java`: either the
//! JetBrains IntelliJ server or Eclipse's jdtls.
//!
//! The archives are large (~370 MB for IntelliJ, ~50 MB for jdtls), so nothing here
//! loads a payload into memory: every step shells out to `curl` / `tar` / `unzip` /
//! `sha256sum` the way [`crate::remote::deploy`] already does, and progress is
//! streamed to the editor. No HTTP, archive or digest crate is added for this — that
//! dependency tree would land in every prebuilt binary for a feature only
//! JVM users reach.
//!
//! Both backends share one pipeline — resolve, download, verify, unpack, publish —
//! and differ only in how the `{url, version, sha256}` tuple is resolved; that is
//! the whole of [`Backend`]. Every install is published as
//! `<install-dir>/<version>/bin/<launcher>`, a fixed depth the Emacs side depends on
//! when it prepends that `bin/` to the proxy's PATH.
//!
//! # Where the IntelliJ download URL comes from
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
//!
//! # Where the jdtls download URL comes from
//!
//! Eclipse publishes the archive name and its digest next to the archive itself, but
//! nothing that points at the newest milestone (`milestones/latest.txt` is a 404, and
//! the directory index renders its versions from JavaScript). So the version number —
//! and *only* the version number — comes from Homebrew's formula API, the same source
//! `eglot-java` uses:
//!
//! ```text
//! formulae.brew.sh/api/formula/jdtls.json  →  versions.stable        ("1.61.0")
//!   download.eclipse.org/jdtls/milestones/<version>/latest.txt   →  archive name
//!   download.eclipse.org/jdtls/milestones/<version>/<name>.sha256 →  digest
//! ```
//!
//! The URL is then *constructed* from [`JDTLS_DOWNLOAD_BASE`], never taken from a
//! response: the network supplies two path fragments beneath a constant host and
//! product path, and both are validated before use. Because the digest is published
//! per milestone rather than only for the newest build, a pinned jdtls version is
//! checksum-verified too — unlike a pinned IntelliJ version, whose URL has to be
//! guessed and therefore cannot be.

use anyhow::{bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// Which Java language server to install.
///
/// The two backends share the entire pipeline and differ only in how a
/// `{url, version, sha256}` tuple is resolved, so everything backend-specific about
/// the *install* is reachable from here. A third backend means another resolver and
/// three more arms, not a second copy of the pipeline.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Backend {
    /// JetBrains IntelliJ headless server. The default because it is what
    /// `emacs/installJavaServer` installed before the request carried a backend at
    /// all, so an older client that omits the field still gets what it asked for.
    #[default]
    Intellij,
    /// Eclipse JDT Language Server.
    Jdtls,
}

impl Backend {
    /// Short name for the editor's mode-line indicator, where the full
    /// `display_name` would not fit.
    pub fn tag(self) -> &'static str {
        match self {
            Backend::Intellij => "IntelliJ",
            Backend::Jdtls => "jdtls",
        }
    }

    /// Human-readable name for progress messages and errors.
    pub fn display_name(self) -> &'static str {
        match self {
            Backend::Intellij => "IntelliJ server",
            Backend::Jdtls => "Eclipse JDT language server",
        }
    }

    /// The file inside `bin/` that a valid archive must contain.
    ///
    /// jdtls ships a Python script with a `.bat` shim beside it rather than a native
    /// binary, so its Windows name is *not* the Unix name plus `.exe`. Getting this
    /// wrong does not fail loudly: `find_launcher` simply reports the archive as the
    /// wrong product.
    pub fn launcher_name(self) -> &'static str {
        match (self, cfg!(windows)) {
            (Backend::Intellij, false) => LAUNCHER_NAME,
            (Backend::Intellij, true) => "intellij-server.exe",
            (Backend::Jdtls, false) => "jdtls",
            (Backend::Jdtls, true) => "jdtls.bat",
        }
    }

    /// External tools this backend's install shells out to.
    ///
    /// jdtls is a single `.tar.gz`, so it never reaches `unzip`; demanding it anyway
    /// would refuse to install on a machine that has no other use for it.
    pub fn required_tools(self) -> &'static [&'static str] {
        match self {
            Backend::Intellij => &["curl", "unzip", "tar"],
            Backend::Jdtls => &["curl", "tar"],
        }
    }
}

const OPEN_VSX_API: &str = "https://open-vsx.org/api/JetBrains/intellij-server";
const JAVA_DEBUG_OPEN_VSX_API: &str = "https://open-vsx.org/api/vscjava/vscode-java-debug";
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

pub fn java_debug_open_vsx_latest_url() -> String {
    format!("{JAVA_DEBUG_OPEN_VSX_API}/latest")
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
        bail!(
            "server-bundle.json has an invalid version: {:?}",
            bundle.version
        );
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

/// Locate `bin/<EXE>` anywhere under ROOT (depth-limited).
///
/// Searching rather than assuming a path is what lets one code path handle every
/// archive layout: the extra top-level directory IntelliJ's `.tar.gz` carries, and
/// jdtls's archive, whose root *is* the server root.
pub fn find_launcher(root: &Path, exe: &str) -> Option<PathBuf> {
    fn walk(dir: &Path, exe: &str, depth: usize) -> Option<PathBuf> {
        if depth > 3 {
            return None;
        }
        let direct = dir.join("bin").join(exe);
        if direct.is_file() {
            return Some(direct);
        }
        for entry in std::fs::read_dir(dir).ok()?.flatten() {
            if entry.file_type().ok()?.is_dir() {
                if let Some(found) = walk(&entry.path(), exe, depth + 1) {
                    return Some(found);
                }
            }
        }
        None
    }
    walk(root, exe, 0)
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

pub fn java_debug_vsix_from_metadata(json: &str) -> Result<(String, String)> {
    let value: serde_json::Value =
        serde_json::from_str(json).context("Open VSX returned invalid Java Debug metadata")?;
    let version = value
        .get("version")
        .and_then(|v| v.as_str())
        .context("Open VSX Java Debug response has no version")?;
    if version.is_empty()
        || !version.starts_with(|c: char| c.is_ascii_digit())
        || !version
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || matches!(c, '.' | '_' | '-'))
    {
        bail!("Open VSX Java Debug response has invalid version: {version:?}");
    }
    let url = value
        .get("files")
        .and_then(|f| f.get("download"))
        .and_then(|d| d.as_str())
        .context("Open VSX Java Debug response has no files.download")?;
    if !url.starts_with(JAVA_DEBUG_OPEN_VSX_API) || !url.ends_with(".vsix") {
        bail!("Open VSX returned an unexpected Java Debug VSIX URL: {url}");
    }
    Ok((url.to_string(), version.to_string()))
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

fn require_tool(what: &str, name: &str) -> Result<()> {
    which::which(name).map(|_| ()).with_context(|| {
        format!("`{name}` was not found on PATH; it is required to install {what}")
    })
}

/// Resolve the newest build by reading the extension shim's `server-bundle.json`.
async fn discover_latest_bundle(
    os: &str,
    arch: &str,
    progress: &ProgressFn,
) -> Result<ServerBundle> {
    let target = open_vsx_target(os, arch)?;
    progress(None, "Looking up the latest IntelliJ server".into());

    let metadata = run(
        &["curl".into(), "-fsSL".into(), open_vsx_latest_url(&target)],
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
async fn download_with_progress(
    name: &str,
    url: &str,
    archive: &Path,
    progress: &ProgressFn,
) -> Result<()> {
    let total = remote_size(url).await;
    let mb = |b: u64| b as f64 / 1_048_576.0;
    progress(
        Some(0),
        if total > 0 {
            format!("Downloading {name} ({:.0} MB)", mb(total))
        } else {
            format!("Downloading {name}")
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
                        "Downloading {name} ({:.0} / {:.0} MB)",
                        mb(current), mb(total)
                    ));
                } else {
                    progress(None, format!("Downloading {name} ({:.0} MB)", mb(current)));
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
    backend: Backend,
    install_dir: &Path,
    version: Option<&str>,
    force: bool,
    progress: ProgressFn,
) -> Result<(PathBuf, String, bool)> {
    for tool in backend.required_tools() {
        require_tool(backend.display_name(), tool)?;
    }

    let (url, version, expected_sha) = match backend {
        Backend::Intellij => resolve_intellij(version, &progress).await?,
        Backend::Jdtls => resolve_jdtls(version, &progress).await?,
    };

    let dest = version_dir(install_dir, &version);
    if !force {
        if let Some(existing) = find_launcher(&dest, backend.launcher_name()) {
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

    let result =
        install_into_staging(backend, &url, expected_sha.as_deref(), &staging, &progress).await;

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
    progress(
        Some(PCT_INSTALL),
        format!("Installing {}", backend.display_name()),
    );
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

    let launcher = dest.join("bin").join(backend.launcher_name());
    set_executable(&launcher).await?;
    Ok((launcher, version, false))
}

/// Resolve IntelliJ's `{url, version, sha256}`.
///
/// Pinned: build the URL ourselves and say so when the shape is a guess, because a
/// constructed URL has no published digest to check it against. Unpinned: the
/// extension shim states url + version + checksum authoritatively.
async fn resolve_intellij(
    version: Option<&str>,
    progress: &ProgressFn,
) -> Result<(String, String, Option<String>)> {
    let (os, arch) = (current_os()?, current_arch()?);
    match version {
        Some(v) => {
            let (url, confirmed) = download_url(v, os, arch)?;
            if !confirmed {
                progress(
                    None,
                    format!(
                        "Archive name for {os}-{arch} is inferred, not confirmed; trying {url}"
                    ),
                );
            }
            Ok((url, v.to_string(), None))
        }
        None => {
            let bundle = discover_latest_bundle(os, arch, progress).await?;
            Ok((bundle.url, bundle.version, Some(bundle.sha256)))
        }
    }
}

// ---------------------------------------------------------------------------
// Backend: Eclipse jdtls
// ---------------------------------------------------------------------------

/// Eclipse's download host and product path.
///
/// A constant, never a value read from a response: everything the network supplies
/// is a path fragment *beneath* this, and is validated before it is used.
pub const JDTLS_DOWNLOAD_BASE: &str = "https://download.eclipse.org/jdtls";

/// Homebrew's formula API, used for the newest milestone *number* and nothing else.
///
/// Eclipse publishes no pointer to its newest milestone — `milestones/latest.txt` is
/// a 404 and the directory index renders its versions from JavaScript — so the one
/// fact that cannot be derived from the constant above is taken from the same source
/// `eglot-java` uses. The digest and archive name still come from Eclipse, so a
/// wrong answer here cannot redirect the download; it can only name a milestone that
/// does not exist, which fails loudly on the next request.
const JDTLS_BREW_FORMULA: &str = "https://formulae.brew.sh/api/formula/jdtls.json";

/// Directory holding VERSION's artifacts.
pub fn jdtls_milestone_url(version: &str) -> String {
    format!("{JDTLS_DOWNLOAD_BASE}/milestones/{version}")
}

/// File naming the archive published for VERSION.
pub fn jdtls_latest_txt_url(version: &str) -> String {
    format!("{}/latest.txt", jdtls_milestone_url(version))
}

/// Download URL for ARCHIVE, assembled from the constants above.
pub fn jdtls_download_url(version: &str, archive: &str) -> String {
    format!("{}/{archive}", jdtls_milestone_url(version))
}

/// Sidecar holding ARCHIVE's published sha256.
pub fn jdtls_sha256_url(version: &str, archive: &str) -> String {
    format!("{}.sha256", jdtls_download_url(version, archive))
}

/// Reject a version that would not be safe as a URL path segment.
///
/// Milestones are strictly `x.y.z`, so digits and dots is not merely sufficient —
/// it also rules out `..` and any separator, which is what keeps a hostile or
/// garbled answer from escaping the product path.
pub fn validate_jdtls_version(version: &str) -> Result<()> {
    if version.is_empty()
        || !version.starts_with(|c: char| c.is_ascii_digit())
        || !version.ends_with(|c: char| c.is_ascii_digit())
        || !version.chars().all(|c| c.is_ascii_digit() || c == '.')
        || version.contains("..")
    {
        bail!("not a valid jdtls version: {version:?}");
    }
    Ok(())
}

/// Reject an archive name we would not have constructed ourselves.
///
/// `latest.txt` arrives over the network and its contents reach both `curl` (as a
/// URL segment) and the filesystem (as the staged archive's name), so the name is
/// checked rather than trusted. Only the build timestamp is genuinely unknown to us:
/// everything else is pinned to `jdt-language-server-<version>-<14 digits>.tar.gz`.
pub fn validate_jdtls_archive_name(version: &str, archive: &str) -> Result<()> {
    if archive.contains("..") || archive.contains('/') || archive.contains('\\') {
        bail!("jdtls latest.txt has an unsafe archive name: {archive:?}");
    }
    let prefix = format!("jdt-language-server-{version}-");
    let stamp = archive
        .strip_prefix(&prefix)
        .and_then(|rest| rest.strip_suffix(".tar.gz"))
        .with_context(|| {
            format!("jdtls latest.txt has an unrecognised archive name: {archive:?} (expected {prefix}<timestamp>.tar.gz)")
        })?;
    // The stamp is `yyyyMMddHHmm`, e.g. 202609031315 — checked against the published
    // `latest.txt` of milestones 1.9.0, 1.30.0, 1.40.0, 1.50.0, 1.60.0 and 1.61.0,
    // all 12 digits. Should upstream ever change its shape, this rejects the archive
    // rather than downloading something unrecognised.
    if stamp.len() != 12 || !stamp.chars().all(|c| c.is_ascii_digit()) {
        bail!("jdtls archive name has a malformed build timestamp: {stamp:?}");
    }
    Ok(())
}

/// Parse the `.sha256` sidecar, which holds the bare digest.
pub fn parse_jdtls_sha256(body: &str) -> Result<String> {
    // Same shape `sha256sum` prints, so the existing parser already handles both the
    // bare digest Eclipse publishes and a `<digest>  <file>` line.
    parse_digest(body).with_context(|| {
        format!(
            "jdtls checksum sidecar is not a sha256 digest: {:?}",
            body.trim()
        )
    })
}

/// Extract the newest stable milestone number from Homebrew's formula JSON.
pub fn jdtls_version_from_brew(json: &str) -> Result<String> {
    let value: serde_json::Value =
        serde_json::from_str(json).context("Homebrew returned invalid JSON")?;
    let version = value
        .get("versions")
        .and_then(|v| v.get("stable"))
        .and_then(|v| v.as_str())
        .context("Homebrew formula has no versions.stable")?;
    validate_jdtls_version(version)?;
    Ok(version.to_string())
}

/// Resolve jdtls's `{url, version, sha256}`.
///
/// Unlike IntelliJ, a *pinned* version is checksum-verified too: Eclipse publishes a
/// digest per milestone rather than only for the newest build, so nothing here has
/// to fall back to downloading unverified bytes.
async fn resolve_jdtls(
    version: Option<&str>,
    progress: &ProgressFn,
) -> Result<(String, String, Option<String>)> {
    let version = match version {
        Some(v) => {
            validate_jdtls_version(v)?;
            v.to_string()
        }
        None => {
            progress(None, "Looking up the latest jdtls milestone".into());
            let json = run(
                &[
                    "curl".into(),
                    "-fsSL".into(),
                    JDTLS_BREW_FORMULA.to_string(),
                ],
                "Homebrew formula lookup",
            )
            .await?;
            jdtls_version_from_brew(&json)?
        }
    };

    progress(None, format!("Resolving jdtls {version}"));
    let listing = run(
        &[
            "curl".into(),
            "-fsSL".into(),
            jdtls_latest_txt_url(&version),
        ],
        "jdtls archive lookup",
    )
    .await
    .with_context(|| format!("no jdtls milestone {version} on {JDTLS_DOWNLOAD_BASE}"))?;
    let archive = listing.lines().next().unwrap_or_default().trim();
    validate_jdtls_archive_name(&version, archive)?;

    let sidecar = run(
        &[
            "curl".into(),
            "-fsSL".into(),
            jdtls_sha256_url(&version, archive),
        ],
        "jdtls checksum lookup",
    )
    .await?;
    let sha256 = parse_jdtls_sha256(&sidecar)?;

    Ok((jdtls_download_url(&version, archive), version, Some(sha256)))
}

// ---------------------------------------------------------------------------
// Java agent: Lombok
// ---------------------------------------------------------------------------

/// Maven Central's coordinates for Lombok. A constant; the network supplies no part
/// of this URL, not even a filename.
const LOMBOK_MAVEN_BASE: &str = "https://repo1.maven.org/maven2/org/projectlombok/lombok";

/// The Lombok build this proxy installs.
///
/// Pinned rather than resolved, because Maven Central publishes no sha256 for it —
/// only sha1 and md5. Pinning lets the digest below be a constant *we* carry instead
/// of one the network hands us, which is strictly stronger than how the language
/// servers are verified. The cost is that a Lombok upgrade is a code change.
const LOMBOK_VERSION: &str = "1.18.48";

/// sha256 of `lombok-1.18.48.jar` (2,055,065 bytes).
///
/// Established by downloading the artifact and checking its sha1 against the value
/// Maven Central publishes beside it (`6858f13541bab505384f07053c5a7b539bbfd3e3`,
/// matched exactly), then digesting the same bytes with sha256.
const LOMBOK_SHA256: &str = "85477a4655ebb2c074a9099cfb749be454449fee564d4282610df1b85f7c508b";

const LOMBOK_LABEL: &str = "Lombok";

/// File name the jar is published under.
pub fn lombok_jar_name() -> String {
    format!("lombok-{LOMBOK_VERSION}.jar")
}

/// Download URL, assembled entirely from the constants above.
pub fn lombok_download_url() -> String {
    format!("{LOMBOK_MAVEN_BASE}/{LOMBOK_VERSION}/{}", lombok_jar_name())
}

/// Download and verify the Lombok agent jar into INSTALL_DIR.
///
/// Deliberately *not* routed through `install_into_staging`: that function exists to
/// unpack an archive and locate a launcher inside it, and a jar has neither. What is
/// shared is everything that matters — the transfer, the digest check, and publishing
/// by rename from a staging directory so a half-written jar is never visible.
pub async fn install_lombok(
    install_dir: &Path,
    force: bool,
    progress: ProgressFn,
) -> Result<(PathBuf, String, bool)> {
    require_tool(LOMBOK_LABEL, "curl")?;

    let jar = install_dir.join(lombok_jar_name());
    if !force && jar.is_file() {
        return Ok((jar, LOMBOK_VERSION.to_string(), true));
    }

    tokio::fs::create_dir_all(install_dir)
        .await
        .with_context(|| format!("could not create {}", install_dir.display()))?;
    let staging = install_dir.join(format!(
        ".staging-lombok-{LOMBOK_VERSION}-{}",
        std::process::id()
    ));
    let _ = tokio::fs::remove_dir_all(&staging).await;
    tokio::fs::create_dir_all(&staging)
        .await
        .with_context(|| format!("could not create {}", staging.display()))?;

    let staged = staging.join(lombok_jar_name());
    let outcome = async {
        download_with_progress(LOMBOK_LABEL, &lombok_download_url(), &staged, &progress).await?;
        progress(Some(PCT_VERIFY), "Verifying Lombok download".into());
        if let Err(err) = verify_checksum(&staged, LOMBOK_SHA256).await {
            // Same rule as the servers: remove the file rather than leave bytes we
            // already know are wrong for a resumed transfer to continue from.
            let _ = tokio::fs::remove_file(&staged).await;
            return Err(err);
        }
        Ok(())
    }
    .await;

    if let Err(err) = outcome {
        let _ = tokio::fs::remove_dir_all(&staging).await;
        return Err(err);
    }

    progress(Some(PCT_INSTALL), "Installing Lombok".into());
    // Replace rather than merge: the name carries the version, so an existing file
    // here is either identical or a forced reinstall of the same build.
    let _ = tokio::fs::remove_file(&jar).await;
    tokio::fs::rename(&staged, &jar)
        .await
        .with_context(|| format!("could not publish {}", jar.display()))?;
    let _ = tokio::fs::remove_dir_all(&staging).await;

    Ok((jar, LOMBOK_VERSION.to_string(), false))
}

const JAVA_DEBUG_LABEL: &str = "Java Debug";
const JAVA_DEBUG_JAR_PREFIX: &str = "com.microsoft.java.debug.plugin-";

fn java_debug_jar_in(dir: &Path) -> Option<PathBuf> {
    std::fs::read_dir(dir).ok()?.flatten().find_map(|entry| {
        let path = entry.path();
        let name = path.file_name()?.to_str()?;
        if path.is_file() && name.starts_with(JAVA_DEBUG_JAR_PREFIX) && name.ends_with(".jar") {
            Some(path)
        } else {
            None
        }
    })
}

pub fn java_debug_jar_name_from_vsix_listing(listing: &str) -> Option<String> {
    listing
        .lines()
        .map(str::trim)
        .find(|entry| {
            entry.starts_with("extension/server/")
                && entry
                    .rsplit('/')
                    .next()
                    .is_some_and(|name| name.starts_with(JAVA_DEBUG_JAR_PREFIX))
                && entry.ends_with(".jar")
                && !entry.contains("..")
                && !entry.contains('\\')
        })
        .map(str::to_string)
}

pub async fn install_java_debug(
    install_dir: &Path,
    force: bool,
    progress: ProgressFn,
) -> Result<(PathBuf, String, bool)> {
    require_tool(JAVA_DEBUG_LABEL, "curl")?;
    require_tool(JAVA_DEBUG_LABEL, "unzip")?;

    progress(None, "Looking up the latest Java Debug extension".into());
    let metadata = run(
        &[
            "curl".into(),
            "-fsSL".into(),
            java_debug_open_vsx_latest_url(),
        ],
        "Open VSX Java Debug lookup",
    )
    .await?;
    let (vsix_url, version) = java_debug_vsix_from_metadata(&metadata)?;

    let dest = version_dir(install_dir, &version);
    if !force {
        if let Some(jar) = java_debug_jar_in(&dest) {
            return Ok((jar, version, true));
        }
    }

    tokio::fs::create_dir_all(install_dir)
        .await
        .with_context(|| format!("could not create {}", install_dir.display()))?;
    let staging = install_dir.join(format!(
        ".staging-java-debug-{version}-{}",
        std::process::id()
    ));
    let _ = tokio::fs::remove_dir_all(&staging).await;
    tokio::fs::create_dir_all(&staging)
        .await
        .with_context(|| format!("could not create {}", staging.display()))?;

    let vsix = staging.join("java-debug.vsix");
    let outcome = async {
        download_with_progress(JAVA_DEBUG_LABEL, &vsix_url, &vsix, &progress).await?;
        progress(Some(PCT_UNPACK), "Extracting Java Debug bundle".into());
        let listing = run(
            &[
                "unzip".into(),
                "-Z1".into(),
                vsix.to_string_lossy().to_string(),
            ],
            "listing Java Debug VSIX",
        )
        .await?;
        let jar_entry = java_debug_jar_name_from_vsix_listing(&listing).context(
            "Java Debug VSIX has no extension/server/com.microsoft.java.debug.plugin-*.jar",
        )?;
        run(
            &[
                "unzip".into(),
                "-jqo".into(),
                vsix.to_string_lossy().to_string(),
                jar_entry,
                "-d".into(),
                staging.to_string_lossy().to_string(),
            ],
            "extracting Java Debug bundle",
        )
        .await?;
        java_debug_jar_in(&staging).context("Java Debug bundle extraction produced no jar")
    }
    .await;

    let staged_jar = match outcome {
        Ok(path) => path,
        Err(err) => {
            let _ = tokio::fs::remove_dir_all(&staging).await;
            return Err(err);
        }
    };

    progress(Some(PCT_INSTALL), "Installing Java Debug bundle".into());
    let _ = tokio::fs::remove_dir_all(&dest).await;
    tokio::fs::create_dir_all(&dest)
        .await
        .with_context(|| format!("could not create {}", dest.display()))?;
    let jar = dest.join(
        staged_jar
            .file_name()
            .context("staged Java Debug jar has no file name")?,
    );
    tokio::fs::rename(&staged_jar, &jar)
        .await
        .with_context(|| format!("could not publish {}", jar.display()))?;
    let _ = tokio::fs::remove_dir_all(&staging).await;

    Ok((jar, version, false))
}

/// Render the launch argument that loads JAR as a java agent.
///
/// TEMPLATE comes from `languages.toml` rather than being built here, so nothing
/// dispatches on a server's name: a server declares how it wants an agent passed
/// (jdtls wants `--jvm-arg=-javaagent:{jar}`) and anything with different syntax
/// only has to change that string.
pub fn render_agent_arg(template: &str, jar: &str) -> String {
    template.replace("{jar}", jar)
}

/// The arguments a server is actually launched with.
///
/// Returns ARGS unchanged unless the server declared a TEMPLATE *and* a managed JAR
/// exists *and* the user has not already loaded a Lombok agent themselves. Kept
/// separate from the launch path so all three of those conditions are testable
/// without starting a process.
pub fn with_lombok_agent(
    args: &[String],
    template: Option<&str>,
    jar: Option<&str>,
) -> Vec<String> {
    let mut out = args.to_vec();
    let (Some(template), Some(jar)) = (template, jar) else {
        return out;
    };
    if args_already_load_lombok(args) {
        return out;
    }
    out.push(render_agent_arg(template, jar));
    out
}

/// Whether ARGS already loads a Lombok agent.
///
/// A user who put `-javaagent:` in `args` themselves has picked a jar on purpose;
/// adding a second agent for the same library is at best redundant. Mirrors the
/// duplicate check the VS Code Java extension does before appending its own.
pub fn args_already_load_lombok(args: &[String]) -> bool {
    args.iter().any(|arg| {
        let lower = arg.to_ascii_lowercase();
        lower.contains("-javaagent:") && lower.contains("lombok")
    })
}

/// Download + verify + unpack into STAGING, returning the launcher found inside.
async fn install_into_staging(
    backend: Backend,
    url: &str,
    expected_sha: Option<&str>,
    staging: &Path,
    progress: &ProgressFn,
) -> Result<PathBuf> {
    let archive_name = url.rsplit('/').next().unwrap_or("server-archive");
    let archive = staging.join(archive_name);

    download_with_progress(backend.display_name(), url, &archive, progress).await?;

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

    progress(
        Some(PCT_UNPACK),
        format!("Unpacking {}", backend.display_name()),
    );
    let extract_dir = staging.join("unpacked");
    tokio::fs::create_dir_all(&extract_dir).await.ok();
    run(&extract_argv(&archive, &extract_dir), "unpacking").await?;
    let _ = tokio::fs::remove_file(&archive).await;

    let launcher = find_launcher(&extract_dir, backend.launcher_name()).with_context(|| {
        format!(
            "the archive unpacked but contains no bin/{} — the download may be a different product",
            backend.launcher_name()
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

    Ok(flattened.join("bin").join(backend.launcher_name()))
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
        assert_eq!(
            open_vsx_target("windows", "aarch64").unwrap(),
            "win32-arm64"
        );
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
        assert!(
            !url.contains("kotlin-server"),
            "must not use the kotlin product"
        );
    }

    #[test]
    fn archive_suffixes_per_platform() {
        assert_eq!(
            archive_suffix("macos", "aarch64").unwrap(),
            ("-aarch64.sit".into(), true)
        );
        assert_eq!(
            archive_suffix("macos", "x86_64").unwrap(),
            (".sit".into(), true)
        );
        assert_eq!(
            archive_suffix("linux", "x86_64").unwrap(),
            (".tar.gz".into(), true)
        );
        // arm64 linux and both Windows arches are inferred, not observed.
        assert_eq!(
            archive_suffix("linux", "aarch64").unwrap(),
            ("-aarch64.tar.gz".into(), false)
        );
        assert_eq!(
            archive_suffix("windows", "x86_64").unwrap(),
            (".win.zip".into(), false)
        );
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
        assert_eq!(
            extract_argv(Path::new("/tmp/a-1.0.win.zip"), dest)[0],
            "unzip"
        );
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
        let exe = Backend::Intellij.launcher_name();
        let tmp = tempfile::tempdir().unwrap();
        let nested = tmp.path().join("intellij-server-263.2689.0").join("bin");
        std::fs::create_dir_all(&nested).unwrap();
        std::fs::write(nested.join(exe), b"#!/bin/sh\n").unwrap();
        assert_eq!(find_launcher(tmp.path(), exe).unwrap(), nested.join(exe));
    }

    /// jdtls's archive has no wrapper directory: its root *is* the server root, so
    /// the launcher sits at depth 0 rather than 1.
    #[test]
    fn find_launcher_handles_an_archive_rooted_at_the_server() {
        let exe = Backend::Jdtls.launcher_name();
        let tmp = tempfile::tempdir().unwrap();
        let bin = tmp.path().join("bin");
        std::fs::create_dir_all(&bin).unwrap();
        std::fs::write(bin.join(exe), b"#!/usr/bin/env python3\n").unwrap();
        assert_eq!(find_launcher(tmp.path(), exe).unwrap(), bin.join(exe));
    }

    /// Each backend must not be satisfied by the other's launcher, or a mixed-up
    /// download would install silently and fail only at startup.
    #[test]
    fn find_launcher_does_not_accept_the_other_backends_launcher() {
        let tmp = tempfile::tempdir().unwrap();
        let bin = tmp.path().join("bin");
        std::fs::create_dir_all(&bin).unwrap();
        std::fs::write(bin.join(Backend::Jdtls.launcher_name()), b"x").unwrap();
        assert!(find_launcher(tmp.path(), Backend::Intellij.launcher_name()).is_none());
    }

    #[test]
    fn find_launcher_returns_none_when_absent() {
        let tmp = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(tmp.path().join("lib")).unwrap();
        assert!(find_launcher(tmp.path(), Backend::Intellij.launcher_name()).is_none());
    }
}

#[cfg(test)]
mod jdtls_tests {
    use super::*;

    /// The milestone actually published at the time this was written; every literal
    /// below is copied from Eclipse's own endpoints rather than invented.
    const V: &str = "1.61.0";
    const ARCHIVE: &str = "jdt-language-server-1.61.0-202609031315.tar.gz";
    const SHA: &str = "338e7e73d61836651ba2453919a0d34fa763eb4e7c03342092309bffb8934c64";

    /// The URL that must not regress: it is assembled from constants, so a response
    /// can never redirect the download to another host or product.
    #[test]
    fn download_urls_use_the_constant_host_and_product_path() {
        assert_eq!(
            jdtls_download_url(V, ARCHIVE),
            "https://download.eclipse.org/jdtls/milestones/1.61.0/jdt-language-server-1.61.0-202609031315.tar.gz"
        );
        assert_eq!(
            jdtls_latest_txt_url(V),
            "https://download.eclipse.org/jdtls/milestones/1.61.0/latest.txt"
        );
        assert_eq!(
            jdtls_sha256_url(V, ARCHIVE),
            "https://download.eclipse.org/jdtls/milestones/1.61.0/jdt-language-server-1.61.0-202609031315.tar.gz.sha256"
        );
        for url in [
            jdtls_download_url(V, ARCHIVE),
            jdtls_latest_txt_url(V),
            jdtls_sha256_url(V, ARCHIVE),
        ] {
            assert!(
                url.starts_with(JDTLS_DOWNLOAD_BASE),
                "escaped the product path: {url}"
            );
        }
    }

    #[test]
    fn accepts_the_published_archive_name() {
        validate_jdtls_archive_name(V, ARCHIVE).unwrap();
    }

    #[test]
    fn rejects_path_traversal_in_archive_name() {
        for bad in [
            "../../../etc/passwd",
            "jdt-language-server-1.61.0-../../evil.tar.gz",
            "sub/jdt-language-server-1.61.0-202609031315.tar.gz",
            r"sub\jdt-language-server-1.61.0-202609031315.tar.gz",
        ] {
            assert!(
                validate_jdtls_archive_name(V, bad).is_err(),
                "must reject {bad:?}"
            );
        }
    }

    /// A name for another milestone would download a version we did not resolve a
    /// checksum for, so the version has to appear in the name itself.
    #[test]
    fn rejects_an_archive_name_for_a_different_version() {
        assert!(
            validate_jdtls_archive_name(V, "jdt-language-server-1.60.0-202609031315.tar.gz")
                .is_err()
        );
    }

    #[test]
    fn rejects_a_malformed_build_timestamp() {
        for bad in [
            "jdt-language-server-1.61.0-20260903131.tar.gz", // too short
            "jdt-language-server-1.61.0-2026090313155.tar.gz", // too long
            "jdt-language-server-1.61.0-20260903131a.tar.gz", // not all digits
            "jdt-language-server-1.61.0-202609031315.zip",   // wrong extension
            "jdt-language-server-1.61.0-202609031315.tar.gz.sha256",
        ] {
            assert!(
                validate_jdtls_archive_name(V, bad).is_err(),
                "must reject {bad:?}"
            );
        }
    }

    #[test]
    fn rejects_unsafe_versions() {
        validate_jdtls_version(V).unwrap();
        validate_jdtls_version("1.9.0").unwrap();
        for bad in [
            "",
            "..",
            "1.61.0/../..",
            "../1.61.0",
            "v1.61.0",
            "1.61.0-SNAPSHOT",
            "latest",
            "1..61",
            "1.61.",
        ] {
            assert!(validate_jdtls_version(bad).is_err(), "must reject {bad:?}");
        }
    }

    /// Eclipse publishes the bare digest; `sha256sum` prints `<digest>  <file>`.
    /// Both shapes reach this parser, so both are covered.
    #[test]
    fn parses_the_sha256_sidecar_in_both_shapes() {
        assert_eq!(parse_jdtls_sha256(SHA).unwrap(), SHA);
        assert_eq!(parse_jdtls_sha256(&format!("{SHA}\n")).unwrap(), SHA);
        assert_eq!(
            parse_jdtls_sha256(&format!("{SHA}  {ARCHIVE}\n")).unwrap(),
            SHA
        );
        assert_eq!(parse_jdtls_sha256(&SHA.to_uppercase()).unwrap(), SHA);
    }

    #[test]
    fn rejects_a_bad_sha256_sidecar() {
        for bad in [
            "",
            "not-a-digest",
            &"a".repeat(63),
            &"a".repeat(65),
            &"z".repeat(64),
            "<html>404</html>",
        ] {
            assert!(parse_jdtls_sha256(bad).is_err(), "must reject {bad:?}");
        }
    }

    #[test]
    fn reads_the_stable_version_from_brew_metadata() {
        let json = r#"{"versions":{"stable":"1.61.0","head":null},"urls":{"stable":{"url":"https://example.invalid/x"}}}"#;
        assert_eq!(jdtls_version_from_brew(json).unwrap(), "1.61.0");
    }

    /// The version becomes a URL path segment, so a hostile or garbled answer must
    /// not survive into `jdtls_milestone_url`.
    #[test]
    fn rejects_junk_brew_metadata() {
        for bad in [
            r#"{"versions":{"stable":"../../../evil"}}"#,
            r#"{"versions":{"stable":""}}"#,
            r#"{"versions":{"stable":123}}"#,
            r#"{"versions":{}}"#,
            r#"{}"#,
            "not json at all",
        ] {
            assert!(jdtls_version_from_brew(bad).is_err(), "must reject {bad:?}");
        }
    }
}

#[cfg(test)]
mod lombok_tests {
    use super::*;

    fn s(v: &[&str]) -> Vec<String> {
        v.iter().map(|x| x.to_string()).collect()
    }

    /// The URL that must not regress. Unlike the servers, nothing here is assembled
    /// from a network response at all — not even the filename — so a compromised or
    /// confused Maven Central cannot redirect this download.
    #[test]
    fn download_url_is_built_from_constants_only() {
        assert_eq!(
            lombok_download_url(),
            "https://repo1.maven.org/maven2/org/projectlombok/lombok/1.18.48/lombok-1.18.48.jar"
        );
        assert!(lombok_download_url().starts_with(LOMBOK_MAVEN_BASE));
        assert_eq!(lombok_jar_name(), "lombok-1.18.48.jar");
    }

    /// The digest is a constant we carry rather than one the network hands us, so a
    /// typo in it would be caught only here.
    #[test]
    fn the_pinned_digest_is_a_wellformed_sha256() {
        let (version, sha) = (LOMBOK_VERSION, LOMBOK_SHA256);
        assert_eq!(sha.len(), 64, "not a sha256: {sha}");
        assert!(sha
            .chars()
            .all(|c| c.is_ascii_hexdigit() && !c.is_uppercase()));
        assert!(version.starts_with(|c: char| c.is_ascii_digit()));
        // The jar name embeds the version, so the two cannot drift apart silently.
        assert!(lombok_jar_name().contains(version));
    }

    #[test]
    fn agent_arg_template_substitutes_the_jar_path() {
        assert_eq!(
            render_agent_arg("--jvm-arg=-javaagent:{jar}", "/x/lombok-1.18.48.jar"),
            "--jvm-arg=-javaagent:/x/lombok-1.18.48.jar"
        );
        // A template with no placeholder is passed through rather than mangled.
        assert_eq!(render_agent_arg("--plain", "/x/l.jar"), "--plain");
    }

    #[test]
    fn the_agent_is_appended_when_a_template_and_a_jar_are_both_present() {
        let out = with_lombok_agent(
            &s(&["--stdio"]),
            Some("--jvm-arg=-javaagent:{jar}"),
            Some("/x/lombok-1.18.48.jar"),
        );
        assert_eq!(
            out,
            s(&["--stdio", "--jvm-arg=-javaagent:/x/lombok-1.18.48.jar"])
        );
    }

    /// Every server that declares no template — which is every server but jdtls —
    /// must be launched with byte-identical arguments, since they all now go through
    /// this one call.
    #[test]
    fn no_template_means_the_arguments_are_untouched() {
        let args = s(&["--stdio", "--eula=34d850193ee04897"]);
        assert_eq!(with_lombok_agent(&args, None, Some("/x/l.jar")), args);
    }

    #[test]
    fn a_template_without_an_installed_jar_means_no_injection() {
        let args = s(&["--stdio"]);
        assert_eq!(
            with_lombok_agent(&args, Some("--jvm-arg=-javaagent:{jar}"), None),
            args
        );
    }

    /// A user who wrote their own `-javaagent:` picked that jar deliberately; adding
    /// a second agent for the same library behind their back is at best redundant.
    #[test]
    fn an_existing_lombok_agent_in_args_suppresses_injection() {
        for existing in [
            "--jvm-arg=-javaagent:/opt/lombok.jar",
            "--jvm-arg=-javaagent:/opt/LOMBOK-1.18.4.jar",
            "-javaagent:/home/u/.m2/.../lombok-1.18.30.jar",
        ] {
            let args = s(&["--stdio", existing]);
            assert_eq!(
                with_lombok_agent(&args, Some("--jvm-arg=-javaagent:{jar}"), Some("/x/l.jar")),
                args,
                "must not add a second agent alongside {existing:?}"
            );
        }
    }

    /// An unrelated agent is not Lombok, so it must not suppress the injection.
    #[test]
    fn an_unrelated_javaagent_does_not_suppress_injection() {
        let args = s(&["--jvm-arg=-javaagent:/opt/jacoco.jar"]);
        let out = with_lombok_agent(
            &args,
            Some("--jvm-arg=-javaagent:{jar}"),
            Some("/x/lombok-1.18.48.jar"),
        );
        assert_eq!(out.len(), 2, "the Lombok agent should still be appended");
        assert!(out[1].contains("lombok-1.18.48.jar"));
    }
}

#[cfg(test)]
mod java_debug_tests {
    use super::*;

    #[test]
    fn open_vsx_metadata_names_java_debug_vsix_and_version() {
        let json = r#"{
          "version": "0.58.2",
          "files": {
            "download": "https://open-vsx.org/api/vscjava/vscode-java-debug/0.58.2/file/vscjava.vscode-java-debug-0.58.2.vsix"
          }
        }"#;
        let (url, version) = java_debug_vsix_from_metadata(json).unwrap();
        assert_eq!(version, "0.58.2");
        assert!(url.ends_with(".vsix"));

        let foreign = r#"{
          "version": "0.58.2",
          "files": { "download": "https://evil.example/debug.vsix" }
        }"#;
        assert!(java_debug_vsix_from_metadata(foreign).is_err());
    }

    #[test]
    fn finds_debug_bundle_inside_vsix_listing() {
        let listing = "\
extension/package.json
extension/server/com.microsoft.java.debug.plugin-0.58.2.jar
extension/README.md
";
        assert_eq!(
            java_debug_jar_name_from_vsix_listing(listing).as_deref(),
            Some("extension/server/com.microsoft.java.debug.plugin-0.58.2.jar")
        );

        assert!(java_debug_jar_name_from_vsix_listing(
            "extension/server/../com.microsoft.java.debug.plugin-0.58.2.jar"
        )
        .is_none());
    }
}

#[cfg(test)]
mod backend_tests {
    use super::*;

    /// jdtls ships a Python script plus a `.bat` shim, so its Windows name is not
    /// the Unix name with `.exe` appended — the one place the two backends do not
    /// follow the same rule.
    #[test]
    fn launcher_names_differ_per_backend_and_platform() {
        assert_ne!(
            Backend::Intellij.launcher_name(),
            Backend::Jdtls.launcher_name()
        );
        if cfg!(windows) {
            assert_eq!(Backend::Intellij.launcher_name(), "intellij-server.exe");
            assert_eq!(Backend::Jdtls.launcher_name(), "jdtls.bat");
        } else {
            assert_eq!(Backend::Intellij.launcher_name(), "intellij-server");
            assert_eq!(Backend::Jdtls.launcher_name(), "jdtls");
        }
    }

    /// jdtls is a single `.tar.gz`; demanding `unzip` anyway would refuse to install
    /// on a machine that has no other use for it.
    #[test]
    fn required_tools_do_not_over_demand() {
        assert!(Backend::Jdtls.required_tools().contains(&"tar"));
        assert!(Backend::Jdtls.required_tools().contains(&"curl"));
        assert!(!Backend::Jdtls.required_tools().contains(&"unzip"));
        assert!(Backend::Intellij.required_tools().contains(&"unzip"));
    }

    /// The wire default has to stay IntelliJ: a client built before the field
    /// existed omits it and must still get what it asked for.
    #[test]
    fn the_wire_default_is_intellij() {
        assert_eq!(Backend::default(), Backend::Intellij);
        assert_eq!(
            serde_json::from_str::<Backend>(r#""jdtls""#).unwrap(),
            Backend::Jdtls
        );
        assert_eq!(
            serde_json::to_string(&Backend::Jdtls).unwrap(),
            r#""jdtls""#
        );
        assert_eq!(
            serde_json::to_string(&Backend::Intellij).unwrap(),
            r#""intellij""#
        );
        assert!(serde_json::from_str::<Backend>(r#""eclipse""#).is_err());
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
        let exe = Backend::Intellij.launcher_name();
        let tmp = tempfile::tempdir().unwrap();
        let build = tmp.path().join("build/intellij-server-263.2689.0");
        std::fs::create_dir_all(build.join("bin")).unwrap();
        std::fs::create_dir_all(build.join("lib")).unwrap();
        std::fs::write(build.join("bin").join(exe), b"#!/bin/sh\nexit 0\n").unwrap();
        std::fs::write(build.join("lib/a.jar"), b"jar").unwrap();

        // Deliberately drop the executable bit: archives often do.
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut p = std::fs::metadata(build.join("bin").join(exe))
                .unwrap()
                .permissions();
            p.set_mode(0o644);
            std::fs::set_permissions(build.join("bin").join(exe), p).unwrap();
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
        assert!(
            out.status.success(),
            "{}",
            String::from_utf8_lossy(&out.stderr)
        );

        // The launcher is one level down because we do not strip components.
        let launcher =
            find_launcher(&dest, exe).expect("launcher must be found through the archive root");
        assert!(launcher.ends_with(Path::new("bin").join(exe)));

        set_executable(&launcher).await.unwrap();
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mode = std::fs::metadata(&launcher).unwrap().permissions().mode();
            assert_eq!(mode & 0o777, 0o755, "archives lose the executable bit");
        }
    }

    /// jdtls's archive has no wrapper directory — its root *is* the server root —
    /// so the flatten step renames the extraction directory onto itself. That is the
    /// one structural difference between the two backends, and the only place a
    /// wrapper-less archive could go wrong.
    ///
    /// Runs the whole staging half (download, verify, unpack, flatten) over a
    /// `file://` URL, so it exercises the real pipeline without fetching 49 MB.
    #[tokio::test]
    async fn staging_flattens_an_archive_whose_root_is_the_server_root() {
        let exe = Backend::Jdtls.launcher_name();
        let tmp = tempfile::tempdir().unwrap();

        // Shaped like Eclipse's: bin/ and plugins/ sit at the archive root.
        let build = tmp.path().join("build");
        std::fs::create_dir_all(build.join("bin")).unwrap();
        std::fs::create_dir_all(build.join("plugins")).unwrap();
        std::fs::write(build.join("bin").join(exe), b"#!/usr/bin/env python3\n").unwrap();
        std::fs::write(build.join("plugins/a.jar"), b"jar").unwrap();

        let archive = tmp
            .path()
            .join("jdt-language-server-9.9.9-202600000000.tar.gz");
        let status = std::process::Command::new("tar")
            .args([
                "-czf",
                archive.to_str().unwrap(),
                "-C",
                build.to_str().unwrap(),
                ".",
            ])
            .status()
            .unwrap();
        assert!(status.success());

        // The digest the installer will be asked to match.
        let digest_out = std::process::Command::new("shasum")
            .args(["-a", "256", archive.to_str().unwrap()])
            .output()
            .unwrap();
        let sha = parse_digest(&String::from_utf8_lossy(&digest_out.stdout)).unwrap();

        let staging = tmp.path().join("staging");
        std::fs::create_dir_all(&staging).unwrap();
        let progress: ProgressFn = Box::new(|_, _| {});
        let launcher = install_into_staging(
            Backend::Jdtls,
            &format!("file://{}", archive.display()),
            Some(&sha),
            &staging,
            &progress,
        )
        .await
        .expect("staging must succeed for a wrapper-less archive");

        assert_eq!(launcher, staging.join("server-root").join("bin").join(exe));
        assert!(launcher.is_file(), "launcher must survive the flatten");
        assert!(
            staging.join("server-root/plugins/a.jar").is_file(),
            "the rest of the tree must be flattened along with the launcher"
        );
        assert!(
            !staging.join("unpacked").exists(),
            "the extraction directory must not be left behind"
        );
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
        verify_checksum(&file, &expected.to_uppercase())
            .await
            .unwrap();
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
        assert!(
            bundle.url.starts_with(DOWNLOAD_BASE),
            "wrong host/product: {}",
            bundle.url
        );
        assert!(!bundle.url.contains("kotlin-server"));
        assert_eq!(bundle.sha256.len(), 64);

        // The archive must actually exist and be large: a shim-sized answer means we
        // resolved the wrong artifact.
        let size = remote_size(&bundle.url).await;
        assert!(
            size > 100 * 1024 * 1024,
            "archive suspiciously small: {size} bytes"
        );
        eprintln!("  archive is {:.0} MB", size as f64 / 1_048_576.0);
    }

    /// Hits the network: resolves jdtls the way the installer does, across all three
    /// endpoints, and checks the tuple survives validation.
    ///
    /// This covers the half unit tests cannot: that Homebrew still exposes
    /// `versions.stable`, that Eclipse still publishes `latest.txt` and a `.sha256`
    /// sidecar for that milestone, and that the URL we construct actually resolves.
    ///
    /// RUN THIS after upgrading the pinned jdtls version, or whenever a jdtls install
    /// starts failing at the lookup stage — those are the only times the assumptions
    /// above can have changed. Ignored by default because it is slow and networked:
    /// `cargo test -- --ignored jdtls_resolution`.
    #[tokio::test]
    #[ignore]
    async fn jdtls_resolution_reaches_a_real_archive() {
        let progress: ProgressFn = Box::new(|_, m| eprintln!("  {m}"));
        let (url, version, sha) = resolve_jdtls(None, &progress)
            .await
            .expect("jdtls resolution must succeed");
        eprintln!("  resolved: {version} -> {url}");

        assert!(
            url.starts_with(JDTLS_DOWNLOAD_BASE),
            "wrong host/product: {url}"
        );
        validate_jdtls_version(&version).unwrap();
        let sha = sha.expect("a milestone always publishes a digest");
        assert_eq!(sha.len(), 64, "digest is not a sha256: {sha}");

        // A milestone archive is tens of MB; an HTML error page or a sidecar-sized
        // answer means we resolved the wrong artifact.
        let size = remote_size(&url).await;
        assert!(
            size > 10 * 1024 * 1024,
            "archive suspiciously small: {size} bytes"
        );
        eprintln!("  archive is {:.0} MB", size as f64 / 1_048_576.0);

        // Pinning the version we just resolved must take the same path and agree.
        let (pinned_url, _, pinned_sha) = resolve_jdtls(Some(&version), &progress).await.unwrap();
        assert_eq!(pinned_url, url);
        assert_eq!(
            pinned_sha.as_deref(),
            Some(sha.as_str()),
            "a pinned jdtls version must still be checksum-verified"
        );
    }
    /// Hits the network: fetches the pinned Lombok jar and checks it still digests to
    /// the constant compiled in above.
    ///
    /// This is the half a unit test cannot cover. Pinning means the digest is only
    /// ever as good as the moment it was taken — if Maven Central ever served
    /// different bytes under the same coordinates, or the artifact were withdrawn,
    /// installs would start failing with a checksum mismatch and nothing else would
    /// explain why.
    ///
    /// RUN THIS when bumping `LOMBOK_VERSION`, to establish the new digest and prove
    /// the URL resolves. Ignored by default because it is networked:
    /// `cargo test -- --ignored lombok_jar`.
    #[tokio::test]
    #[ignore]
    async fn lombok_jar_still_matches_the_pinned_digest() {
        let (version, sha) = (LOMBOK_VERSION, LOMBOK_SHA256);
        let url = lombok_download_url();
        eprintln!("  fetching {url}");

        let size = remote_size(&url).await;
        assert!(size > 500 * 1024, "jar suspiciously small: {size} bytes");
        eprintln!("  {:.1} MB", size as f64 / 1_048_576.0);

        let tmp = tempfile::tempdir().unwrap();
        let jar = tmp.path().join(lombok_jar_name());
        let progress: ProgressFn = Box::new(|_, m| eprintln!("  {m}"));
        download_with_progress(LOMBOK_LABEL, &url, &jar, &progress)
            .await
            .expect("the pinned Lombok version must still be downloadable");

        verify_checksum(&jar, sha).await.unwrap_or_else(|err| {
            panic!("Lombok {version} no longer digests to the pinned value: {err:#}")
        });
        eprintln!("  digest matches the pinned constant");
    }
}
