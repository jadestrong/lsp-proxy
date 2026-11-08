//! Installing Eclipse JDT LS for `lsp-proxy-java`.
//!
//! The archives are large (~50 MB for jdtls), so nothing here
//! loads a payload into memory: every step shells out to `curl` / `tar` / `unzip` /
//! `sha256sum` the way [`crate::remote::deploy`] already does, and progress is
//! streamed to the editor. No HTTP, archive or digest crate is added for this — that
//! dependency tree would land in every prebuilt binary for a feature only
//! JVM users reach.
//!
//! The pipeline resolves, downloads, verifies, unpacks and publishes each server.
//! Every install is published as
//! `<install-dir>/<version>/bin/<launcher>`, a fixed depth the Emacs side depends on
//! when it prepends that `bin/` to the proxy's PATH.
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
//! checksum-verified too.

use anyhow::{bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

static ACTIVE_SERVER_INSTALLS: std::sync::Mutex<Option<std::collections::HashSet<PathBuf>>> =
    std::sync::Mutex::new(None);

/// Hold an installation destination until its async task completes or is dropped.
pub(crate) struct ServerInstallGuard(PathBuf);

impl ServerInstallGuard {
    pub(crate) fn acquire(install_dir: &Path) -> Result<Self> {
        let directory = if install_dir.is_absolute() {
            install_dir.to_path_buf()
        } else {
            std::env::current_dir()?.join(install_dir)
        };
        // Canonicalize the existing prefix even before the destination exists.
        // Otherwise creating a directory below a symlink could change its lock key.
        let directory = directory
            .ancestors()
            .find_map(|ancestor| {
                ancestor
                    .canonicalize()
                    .ok()
                    .map(|resolved| resolved.join(directory.strip_prefix(ancestor).unwrap()))
            })
            .context("could not resolve the Java server installation directory")?;
        let mut active = ACTIVE_SERVER_INSTALLS.lock().unwrap();
        if !active
            .get_or_insert_with(Default::default)
            .insert(directory.clone())
        {
            bail!(
                "a Java server installation is already in progress for {}",
                directory.display()
            );
        }
        Ok(Self(directory))
    }
}

impl Drop for ServerInstallGuard {
    fn drop(&mut self) {
        if let Some(active) = ACTIVE_SERVER_INSTALLS.lock().unwrap().as_mut() {
            active.remove(&self.0);
        }
    }
}

/// Which Java language server to install.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Backend {
    /// Eclipse JDT Language Server.
    #[default]
    Jdtls,
}

impl Backend {
    /// Short name for the editor's mode-line indicator, where the full
    /// `display_name` would not fit.
    pub fn tag(self) -> &'static str {
        match self {
            Backend::Jdtls => "jdtls",
        }
    }

    /// Human-readable name for progress messages and errors.
    pub fn display_name(self) -> &'static str {
        match self {
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
            Backend::Jdtls => &["curl", "tar"],
        }
    }
}

const JAVA_DEBUG_OPEN_VSX_API: &str = "https://open-vsx.org/api/vscjava/vscode-java-debug";
pub fn java_debug_open_vsx_latest_url() -> String {
    format!("{JAVA_DEBUG_OPEN_VSX_API}/latest")
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
/// archive layout, including wrapper directories and archives rooted at the server.
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
pub(crate) async fn run(argv: &[String], what: &str) -> Result<String> {
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

pub(crate) fn require_tool(what: &str, name: &str) -> Result<()> {
    which::which(name).map(|_| ()).with_context(|| {
        format!("`{name}` was not found on PATH; it is required to install {what}")
    })
}

/// Content-Length of URL, or 0 when the server does not report one.
async fn remote_size(url: &str) -> u64 {
    let out = Command::new("curl")
        .args(["-sIL", "--max-time", "20", url])
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
pub(crate) async fn download_with_progress(
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
pub(crate) async fn verify_checksum(archive: &Path, expected: &str) -> Result<()> {
    let argv = checksum_argv(archive, cfg!(windows), |name| which::which(name).is_ok())?;
    let out = run(&argv, "checksum").await?;
    let actual = parse_checksum_output(&out)
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

fn checksum_argv(
    archive: &Path,
    windows: bool,
    available: impl Fn(&str) -> bool,
) -> Result<Vec<String>> {
    let path = archive.to_string_lossy().to_string();
    if available("sha256sum") {
        Ok(vec!["sha256sum".into(), path])
    } else if available("shasum") {
        Ok(vec!["shasum".into(), "-a".into(), "256".into(), path])
    } else if windows && available("certutil") {
        Ok(vec![
            "certutil".into(),
            "-hashfile".into(),
            path,
            "SHA256".into(),
        ])
    } else {
        bail!("no SHA-256 tool was found on PATH (sha256sum, shasum or Windows certutil); cannot verify the download")
    }
}

pub(crate) fn require_checksum_tool() -> Result<()> {
    checksum_argv(Path::new(""), cfg!(windows), |name| {
        which::which(name).is_ok()
    })
    .map(|_| ())
}

fn parse_checksum_output(stdout: &str) -> Option<String> {
    // certutil prints a localized heading before its bare digest line.
    stdout.lines().find_map(parse_digest)
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
        Backend::Jdtls => resolve_jdtls(version, &progress).await?,
    };

    let dest = version_dir(install_dir, &version);
    if !force {
        if let Some(existing) = find_launcher(&dest, backend.launcher_name()) {
            return Ok((existing, version, true));
        }
    }
    require_checksum_tool()?;

    // Stage beside the destination so publishing is a rename on the same
    // filesystem, never a copy over a directory that may be in use.
    tokio::fs::create_dir_all(install_dir)
        .await
        .with_context(|| format!("could not create {}", install_dir.display()))?;
    let staging_dir = tempfile::Builder::new()
        .prefix(&format!(".staging-{version}-"))
        .tempdir_in(install_dir)?;
    let staging = staging_dir.path();

    let result =
        install_into_staging(backend, &url, expected_sha.as_deref(), staging, &progress).await;

    let launcher_in_staging = match result {
        Ok(path) => path,
        Err(err) => {
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
        let quarantine = install_dir.join(format!(
            ".old-{}",
            staging.file_name().unwrap().to_string_lossy()
        ));
        tokio::fs::rename(&dest, &quarantine)
            .await
            .with_context(|| format!("could not move aside {}", dest.display()))?;
        let _ = tokio::fs::remove_dir_all(&quarantine).await;
    }
    tokio::fs::rename(&server_root, &dest)
        .await
        .with_context(|| format!("could not publish {}", dest.display()))?;

    let launcher = dest.join("bin").join(backend.launcher_name());
    set_executable(&launcher).await?;
    Ok((launcher, version, false))
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
/// A *pinned* version is checksum-verified too: Eclipse publishes a
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
    require_checksum_tool()?;

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

/// Stable release whose VSIX layout and stdio launcher have been verified.
pub const SPRING_VERSION: &str = "1.62.0";

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SpringInstall {
    pub version: String,
    pub server_jar: String,
    pub bundles: Vec<String>,
    pub download_url: String,
    pub sha256: String,
}

fn spring_relative_path(path: &str) -> bool {
    !path.is_empty()
        && path
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || "/._-".contains(c))
        && Path::new(path)
            .components()
            .all(|c| matches!(c, std::path::Component::Normal(_)))
}

fn spring_payload(
    manifest: &str,
    listing: &str,
    version: &str,
    url: &str,
    sha256: &str,
) -> Result<SpringInstall> {
    let manifest: serde_json::Value = serde_json::from_str(manifest)?;
    if manifest["name"] != "vscode-spring-boot" || manifest["version"] != version {
        bail!("The VSIX is not the requested Spring Boot extension version");
    }
    let entries: std::collections::HashSet<_> = listing.lines().collect();
    let server_jar = listing
        .lines()
        .find(|entry| {
            spring_relative_path(entry)
                && entry.ends_with(".jar")
                && (entry.starts_with("extension/language-server/spring-boot-language-server-")
                    || entry.starts_with("extension/jars/spring-boot-language-server-"))
        })
        .context("Spring VSIX has no executable language server jar")?
        .to_string();
    let extensions = manifest["contributes"]["javaExtensions"]
        .as_array()
        .context("Spring VSIX has no contributes.javaExtensions")?;
    let mut bundles = Vec::new();
    for extension in extensions {
        let relative = extension
            .as_str()
            .context("Invalid Spring Java extension path")?
            .strip_prefix("./")
            .context("Spring Java extension must be relative to extension/")?;
        let path = format!("extension/{relative}");
        if !spring_relative_path(&path)
            || !path.starts_with("extension/jars/")
            || !entries.contains(path.as_str())
        {
            bail!("Invalid or missing Spring JDT bundle: {path}");
        }
        if !bundles.contains(&path) {
            bundles.push(path);
        }
    }
    if !bundles
        .iter()
        .any(|path| path.ends_with("/jdt-ls-extension.jar"))
    {
        bail!("Spring VSIX does not contain the required JDT LS extension");
    }
    Ok(SpringInstall {
        version: version.into(),
        server_jar,
        bundles,
        download_url: url.into(),
        sha256: sha256.into(),
    })
}

fn spring_launcher(server_jar: &str) -> String {
    if cfg!(windows) {
        format!("@echo off\r\nset \"JAVA_CMD=java\"\r\nif defined JAVA_HOME set \"JAVA_CMD=%JAVA_HOME%\\bin\\java.exe\"\r\nif defined LSP_PROXY_SPRING_JAVA set \"JAVA_CMD=%LSP_PROXY_SPRING_JAVA%\"\r\n\"%JAVA_CMD%\" -Dsts.lsp.client=vscode -Dspring.main.banner-mode=off -jar \"%~dp0..\\{}\" %*\r\n", server_jar.replace('/', "\\"))
    } else {
        format!("#!/bin/sh\nroot=\"$(CDPATH= cd \"$(dirname \"$0\")/..\" && pwd)\"\nif [ -n \"${{LSP_PROXY_SPRING_JAVA:-}}\" ]; then java_cmd=\"$LSP_PROXY_SPRING_JAVA\"; elif [ -n \"${{JAVA_HOME:-}}\" ]; then java_cmd=\"$JAVA_HOME/bin/java\"; else java_cmd=java; fi\nexec \"$java_cmd\" -Dsts.lsp.client=vscode -Dspring.main.banner-mode=off -jar \"$root/{server_jar}\" \"$@\"\n")
    }
}

pub async fn install_spring(
    install_dir: &Path,
    version: Option<&str>,
    force: bool,
    progress: ProgressFn,
) -> Result<(PathBuf, SpringInstall, bool)> {
    let version = version.unwrap_or(SPRING_VERSION);
    validate_jdtls_version(version)?;
    let dest = version_dir(install_dir, version);
    let launcher_name = if cfg!(windows) {
        "spring-boot-ls.bat"
    } else {
        "spring-boot-ls"
    };
    let launcher = dest.join("bin").join(launcher_name);
    if !force {
        if let Ok(text) = tokio::fs::read_to_string(dest.join("spring-install.json")).await {
            if let Ok(info) = serde_json::from_str::<SpringInstall>(&text) {
                if info.version == version
                    && launcher.is_file()
                    && spring_relative_path(&info.server_jar)
                    && dest.join(&info.server_jar).is_file()
                    && !info.bundles.is_empty()
                    && info
                        .bundles
                        .iter()
                        .all(|path| spring_relative_path(path) && dest.join(path).is_file())
                {
                    return Ok((launcher, info, true));
                }
            }
        }
    }
    require_tool("Spring Boot LS", "curl")?;
    require_tool("Spring Boot LS", "unzip")?;
    require_checksum_tool()?;
    progress(None, format!("Looking up Spring Boot Tools {version}"));
    let base = format!("https://open-vsx.org/api/VMware/vscode-spring-boot/{version}");
    let metadata = run(
        &["curl".into(), "-fsSL".into(), base.clone()],
        "Spring extension lookup",
    )
    .await?;
    let metadata: serde_json::Value = serde_json::from_str(&metadata)?;
    let url = metadata["files"]["download"]
        .as_str()
        .context("Spring metadata has no download URL")?;
    let digest_url = metadata["files"]["sha256"]
        .as_str()
        .context("Spring metadata has no SHA-256 URL")?;
    if !url.starts_with(&format!("{base}/file/"))
        || !digest_url.starts_with(&format!("{base}/file/"))
    {
        bail!("Spring extension URLs are outside the selected Open VSX release");
    }
    let digest = run(
        &["curl".into(), "-fsSL".into(), digest_url.into()],
        "Spring checksum lookup",
    )
    .await?;
    let sha256 = parse_jdtls_sha256(&digest)?;
    tokio::fs::create_dir_all(install_dir).await?;
    let temp = tempfile::tempdir_in(install_dir)?;
    let vsix = temp.path().join("spring.vsix");
    download_with_progress("Spring Boot LS", url, &vsix, &progress).await?;
    verify_checksum(&vsix, &sha256).await?;
    let path = vsix.to_string_lossy().to_string();
    let listing = run(
        &["unzip".into(), "-Z1".into(), path.clone()],
        "Spring VSIX listing",
    )
    .await?;
    // Check paths before extraction, including files that unzip's globs match.
    if listing
        .lines()
        .filter(|s| s.starts_with("extension/language-server/") || s.starts_with("extension/jars/"))
        .any(|s| !spring_relative_path(s.trim_end_matches('/')))
    {
        bail!("Spring VSIX contains an unsafe payload path");
    }
    let manifest = run(
        &[
            "unzip".into(),
            "-p".into(),
            path.clone(),
            "extension/package.json".into(),
        ],
        "Spring extension manifest",
    )
    .await?;
    let info = spring_payload(&manifest, &listing, version, url, &sha256)?;
    let payload = temp.path().join("payload");
    tokio::fs::create_dir_all(payload.join("bin")).await?;
    progress(
        Some(PCT_UNPACK),
        "Extracting Spring language server and JDT bundles".into(),
    );
    let mut extract = vec!["unzip".into(), "-q".into(), path, "extension/jars/*".into()];
    if listing
        .lines()
        .any(|s| s.starts_with("extension/language-server/"))
    {
        extract.push("extension/language-server/*".into());
    }
    extract.extend(["-d".into(), payload.to_string_lossy().to_string()]);
    run(&extract, "Spring VSIX extraction").await?;
    let staged_launcher = payload.join("bin").join(launcher_name);
    tokio::fs::write(&staged_launcher, spring_launcher(&info.server_jar)).await?;
    set_executable(&staged_launcher).await?;
    tokio::fs::write(
        payload.join("spring-install.json"),
        serde_json::to_vec_pretty(&info)?,
    )
    .await?;
    // Keep a forced reinstall recoverable until the new payload is published.
    let backup = temp.path().join("previous");
    let had_previous = dest.exists();
    if had_previous {
        tokio::fs::rename(&dest, &backup).await?;
    }
    if let Err(err) = tokio::fs::rename(&payload, &dest).await {
        if had_previous {
            let _ = tokio::fs::rename(&backup, &dest).await;
        }
        return Err(err.into());
    }
    progress(Some(100), format!("Spring Boot Tools {version} installed"));
    Ok((launcher, info, false))
}

#[cfg(test)]
mod spring_tests {
    use super::*;

    #[tokio::test]
    #[ignore = "downloads the fixed STS4 release; optional LSP_PROXY_SPRING_TEST_DIR keeps it for smoke tests"]
    async fn test_spring_release_installs() {
        let temporary = tempfile::tempdir().unwrap();
        let dir = std::env::var_os("LSP_PROXY_SPRING_TEST_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|| temporary.path().to_path_buf());
        let (launcher, info, _) = install_spring(
            &dir,
            None,
            false,
            Box::new(|_, message| eprintln!("{message}")),
        )
        .await
        .unwrap();
        assert!(launcher.is_file());
        assert_eq!(info.bundles.len(), 5);
        assert!(info
            .bundles
            .iter()
            .all(|path| dir.join(SPRING_VERSION).join(path).is_file()));
        eprintln!("Spring test launcher: {}", launcher.display());
    }

    #[test]
    fn test_spring_manifest_selects_only_jdt_bundles_and_executable() {
        let manifest = r#"{"name":"vscode-spring-boot","version":"1.62.0",
            "contributes":{"javaExtensions":["./jars/jdt-ls-extension.jar","./jars/reactor.jar"]}}"#;
        let listing = "extension/language-server/spring-boot-language-server-1.62.0-exec.jar\nextension/language-server/lib/runtime.jar\nextension/jars/jdt-ls-extension.jar\nextension/jars/reactor.jar\n";
        let info = spring_payload(manifest, listing, "1.62.0", "url", "hash").unwrap();
        assert_eq!(
            info.bundles,
            vec![
                "extension/jars/jdt-ls-extension.jar",
                "extension/jars/reactor.jar"
            ]
        );
        assert!(!info
            .bundles
            .contains(&"extension/language-server/lib/runtime.jar".into()));
        assert!(spring_payload(manifest, listing, "2.0.0", "url", "hash").is_err());
        assert!(spring_payload(
            manifest,
            &listing.replace("extension/jars/reactor.jar\n", ""),
            "1.62.0",
            "url",
            "hash"
        )
        .is_err());
        assert!(spring_payload(
            &manifest.replace("./jars/reactor.jar", "./jars/../outside.jar"),
            listing,
            "1.62.0",
            "url",
            "hash"
        )
        .is_err());
        for path in [
            "../a.jar",
            "/a.jar",
            "extension/../../a.jar",
            "extension/a;$x.jar",
            "",
        ] {
            assert!(!spring_relative_path(path));
        }
    }

    #[tokio::test]
    async fn test_spring_install_reuses_only_a_complete_selected_release() {
        let dir = tempfile::tempdir().unwrap();
        let dest = dir.path().join(SPRING_VERSION);
        let jar = "extension/language-server/spring-boot-language-server-exec.jar";
        tokio::fs::create_dir_all(dest.join("bin")).await.unwrap();
        tokio::fs::create_dir_all(dest.join("extension/language-server"))
            .await
            .unwrap();
        tokio::fs::create_dir_all(dest.join("extension/jars"))
            .await
            .unwrap();
        tokio::fs::write(dest.join(jar), b"jar").await.unwrap();
        tokio::fs::write(dest.join("extension/jars/jdt-ls-extension.jar"), b"bundle")
            .await
            .unwrap();
        let name = if cfg!(windows) {
            "spring-boot-ls.bat"
        } else {
            "spring-boot-ls"
        };
        tokio::fs::write(dest.join("bin").join(name), spring_launcher(jar))
            .await
            .unwrap();
        let info = SpringInstall {
            version: SPRING_VERSION.into(),
            server_jar: jar.into(),
            bundles: vec!["extension/jars/jdt-ls-extension.jar".into()],
            download_url: "verified-release".into(),
            sha256: "digest".into(),
        };
        tokio::fs::write(
            dest.join("spring-install.json"),
            serde_json::to_vec(&info).unwrap(),
        )
        .await
        .unwrap();
        let (_, _, reused) = install_spring(dir.path(), None, false, Box::new(|_, _| {}))
            .await
            .unwrap();
        assert!(reused);
    }
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
    fn checksum_supports_windows_certutil_without_unix_tools() {
        let archive = Path::new("archive with spaces.zip");
        assert_eq!(
            checksum_argv(archive, true, |name| name == "certutil").unwrap(),
            vec!["certutil", "-hashfile", "archive with spaces.zip", "SHA256"]
        );
        let digest = "a".repeat(64);
        let output = format!(
            "SHA256 hash of archive:\r\n{}\r\nCertUtil: command completed successfully.\r\n",
            digest.to_uppercase()
        );
        assert_eq!(parse_checksum_output(&output), Some(digest));
        assert!(parse_checksum_output("CertUtil: command failed.").is_none());
        assert!(checksum_argv(archive, false, |name| name == "certutil").is_err());
        assert!(checksum_argv(archive, true, |_| false).is_err());
    }

    #[test]
    fn checksum_prefers_available_unix_tools() {
        let archive = Path::new("archive.zip");
        assert_eq!(
            checksum_argv(archive, true, |_| true).unwrap()[0],
            "sha256sum"
        );
        assert_eq!(
            checksum_argv(archive, true, |name| name != "sha256sum").unwrap(),
            vec!["shasum", "-a", "256", "archive.zip"]
        );
    }

    #[test]
    fn server_install_guard_rejects_duplicates_and_releases_on_drop() {
        let first_dir = tempfile::tempdir().unwrap();
        let second_dir = tempfile::tempdir().unwrap();
        let first = ServerInstallGuard::acquire(first_dir.path()).unwrap();
        assert!(ServerInstallGuard::acquire(first_dir.path()).is_err());
        assert!(ServerInstallGuard::acquire(&first_dir.path().join(".")).is_err());
        let second = ServerInstallGuard::acquire(second_dir.path()).unwrap();
        drop(first);
        let next = ServerInstallGuard::acquire(first_dir.path()).unwrap();
        drop((next, second));
    }

    #[test]
    fn server_install_guard_keeps_the_same_key_when_the_destination_is_created() {
        let root = tempfile::tempdir().unwrap();
        let destination = root.path().join("new-install");
        let guard = ServerInstallGuard::acquire(&destination).unwrap();
        std::fs::create_dir_all(&destination).unwrap();
        assert!(ServerInstallGuard::acquire(&destination).is_err());
        assert!(ServerInstallGuard::acquire(&destination.canonicalize().unwrap()).is_err());
        drop(guard);
        assert!(ServerInstallGuard::acquire(&destination).is_ok());
    }

    #[test]
    fn version_dir_is_keyed_by_version() {
        assert_eq!(
            version_dir(Path::new("/i"), "1.61.0"),
            PathBuf::from("/i/1.61.0")
        );
    }

    #[test]
    fn find_launcher_handles_an_extra_archive_root() {
        let exe = Backend::Jdtls.launcher_name();
        let tmp = tempfile::tempdir().unwrap();
        let nested = tmp.path().join("jdtls-1.61.0").join("bin");
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
        std::fs::write(bin.join("other-server"), b"x").unwrap();
        assert!(find_launcher(tmp.path(), Backend::Jdtls.launcher_name()).is_none());
    }

    #[test]
    fn find_launcher_returns_none_when_absent() {
        let tmp = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(tmp.path().join("lib")).unwrap();
        assert!(find_launcher(tmp.path(), Backend::Jdtls.launcher_name()).is_none());
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

    #[test]
    fn launcher_name_matches_the_platform() {
        assert_eq!(
            Backend::Jdtls.launcher_name(),
            if cfg!(windows) { "jdtls.bat" } else { "jdtls" }
        );
    }

    /// jdtls is a single `.tar.gz`; demanding `unzip` anyway would refuse to install
    /// on a machine that has no other use for it.
    #[test]
    fn required_tools_do_not_over_demand() {
        assert!(Backend::Jdtls.required_tools().contains(&"tar"));
        assert!(Backend::Jdtls.required_tools().contains(&"curl"));
        assert!(!Backend::Jdtls.required_tools().contains(&"unzip"));
    }

    /// The only supported backend is also the default on the wire.
    #[test]
    fn the_wire_default_is_jdtls() {
        assert_eq!(Backend::default(), Backend::Jdtls);
        assert_eq!(
            serde_json::from_str::<Backend>(r#""jdtls""#).unwrap(),
            Backend::Jdtls
        );
        assert_eq!(
            serde_json::to_string(&Backend::Jdtls).unwrap(),
            r#""jdtls""#
        );
        assert!(serde_json::from_str::<Backend>(r#""intellij""#).is_err());
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
    /// shape with one extra top-level directory, without downloading a server.
    #[tokio::test]
    async fn unpacks_locates_and_marks_the_launcher_executable() {
        let exe = Backend::Jdtls.launcher_name();
        let tmp = tempfile::tempdir().unwrap();
        let build = tmp.path().join("build/jdtls-1.61.0");
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

        let archive = tmp.path().join("jdtls-1.61.0.tar.gz");
        let status = std::process::Command::new("tar")
            .args(["-czf", archive.to_str().unwrap(), "-C"])
            .arg(tmp.path().join("build"))
            .arg("jdtls-1.61.0")
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
