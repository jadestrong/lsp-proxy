//! Remote binary management: check whether a compatible `emacs-lsp-proxy`
//! exists on the remote host, and deploy it on demand.
//!
//! The check runs `<remote_path> --version` over the SSH ControlMaster.
//! Deployment downloads the correct release archive from GitHub based on the
//! remote platform — the local binary is never uploaded directly, avoiding
//! cross-architecture issues.  Deployment is always user-initiated; callers
//! that need it should invoke `deploy_with_progress` explicitly.

use anyhow::{anyhow, Context, Result};
use log::{debug, info, warn};

use super::ssh::SshConnection;

/// Default location we install the remote binary. Lives under the XDG-style
/// cache directory so it doesn't pollute the user's PATH — callers invoke it
/// by absolute path, not by name.
pub const DEFAULT_REMOTE_BINARY_PATH: &str = "~/.cache/emacs/lsp-proxy/emacs-lsp-proxy";

/// Version expected on the remote side — baked in at compile time so that
/// upgrading the local crate naturally triggers a redeploy.
pub const EXPECTED_VERSION: &str = env!("CARGO_PKG_VERSION");

/// Outcome of probing the remote binary.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RemoteBinaryStatus {
    /// Binary isn't present (or isn't executable).
    Missing,
    /// Present and its reported version matches [`EXPECTED_VERSION`].
    VersionMatch,
    /// Present but reports a different version; redeploy recommended.
    VersionMismatch { remote: String },
}

/// Probe the remote host for the binary at `remote_path`.
pub async fn check_remote_binary(
    conn: &SshConnection,
    remote_path: &str,
) -> Result<RemoteBinaryStatus> {
    // Wrap in a shell so `~` expands on the remote side.
    let command = format!(
        "sh -c '{} --version 2>/dev/null'",
        shell_escape(remote_path)
    );
    let output = conn
        .run_command_with_status(&command)
        .await
        .context("probing remote binary version")?;

    if !output.is_success() {
        debug!(
            "remote `{}` probe exited non-zero (code={:?}, stderr={})",
            remote_path,
            output.exit_code,
            output.stderr.trim()
        );
        return Ok(RemoteBinaryStatus::Missing);
    }

    match parse_version_output(&output.stdout) {
        Some(v) if v == EXPECTED_VERSION => Ok(RemoteBinaryStatus::VersionMatch),
        Some(v) => Ok(RemoteBinaryStatus::VersionMismatch {
            remote: v.to_string(),
        }),
        None => {
            warn!(
                "remote `{}` produced unparsable version output: {:?}",
                remote_path, output.stdout
            );
            Ok(RemoteBinaryStatus::Missing)
        }
    }
}

/// Detect the remote host's OS and CPU architecture.
///
/// Returns `(os, arch)` where `os` is lowercase `"linux"` or `"darwin"` and
/// `arch` is `"x86_64"` or `"aarch64"`.
async fn detect_remote_platform(conn: &SshConnection) -> Result<(String, String)> {
    let output = conn
        .run_command("uname -sm")
        .await
        .context("failed to detect remote platform")?;
    let parts: Vec<&str> = output.trim().splitn(2, ' ').collect();
    if parts.len() != 2 {
        anyhow::bail!("unexpected `uname -sm` output: {:?}", output.trim());
    }
    let os = parts[0].to_lowercase();
    let arch = parts[1].to_lowercase();
    Ok((os, arch))
}

/// Map `(os, arch)` to the GitHub release archive filename.
///
/// Archive naming from CI:
///   lsp-proxy-linux-x86_64.tar.gz
///   lsp-proxy-linux-arm64.tar.gz
///   lsp-proxy-macos-x86_64.tar.gz
///   lsp-proxy-macos-arm64.tar.gz
fn release_archive_name(os: &str, arch: &str) -> Result<String> {
    let os_label = match os {
        "linux" => "linux",
        "darwin" => "macos",
        other => anyhow::bail!("unsupported remote OS: {other}"),
    };
    let arch_label = match arch {
        "x86_64" => "x86_64",
        "aarch64" | "arm64" => "arm64",
        other => anyhow::bail!("unsupported remote architecture: {other}"),
    };
    Ok(format!("lsp-proxy-{os_label}-{arch_label}.tar.gz"))
}

/// Download the correct GitHub release binary onto the remote host.
///
/// Uses `curl` when available, falls back to `wget`.  The archive is
/// downloaded to a temporary directory, the binary extracted, moved to
/// `remote_path`, and the temp directory cleaned up.
async fn download_release_on_remote(
    conn: &SshConnection,
    remote_path: &str,
    archive_name: &str,
    on_progress: &(impl Fn(String) + Send + Sync),
) -> Result<()> {
    // All commands that need POSIX syntax are wrapped in `sh -c '...'` so
    // they run under a POSIX shell regardless of the remote user's login
    // shell (fish, nushell, etc. do not support POSIX variable syntax).
    let url = format!(
        "https://github.com/jadestrong/lsp-proxy/releases/download/v{EXPECTED_VERSION}/{archive_name}"
    );
    let parent = remote_parent_dir(remote_path);

    // 1. Create a temp directory and capture its path.
    on_progress("Preparing temporary directory...".to_string());
    let tmpdir = conn
        .run_command("sh -c 'mktemp -d'")
        .await
        .context("failed to create remote temp directory")?
        .trim()
        .to_string();
    if tmpdir.is_empty() {
        anyhow::bail!("mktemp -d returned an empty path");
    }
    let archive  = format!("{tmpdir}/archive.tar.gz");
    let extracted = format!("{tmpdir}/emacs-lsp-proxy");
    let cleanup  = format!("rm -rf {}", shell_escape(&tmpdir));

    // 2. Ensure the destination directory exists.
    on_progress(format!("Creating remote directory {parent}..."));
    if let Err(e) = conn
        .run_command(&format!("mkdir -p {}", shell_escape(&parent)))
        .await
    {
        conn.run_command(&cleanup).await.ok();
        return Err(e).context("failed to create remote install directory");
    }

    // 3. Get total file size for percentage display (best-effort, not fatal).
    let total_bytes: u64 = {
        // `-IL` follows redirects (GitHub releases → CDN 302) so the final
        // response contains the actual Content-Length.
        let head_script = format!(
            "command -v curl >/dev/null 2>&1 && \
             curl -sIL {url} | grep -i '^content-length:' | tail -1 | \
             tr -d '\\r' | awk '{{print $2}}' || echo 0",
            url = shell_escape(&url),
        );
        conn.run_command(&format!("sh -c {}", shell_escape(&head_script)))
            .await
            .unwrap_or_default()
            .trim()
            .parse()
            .unwrap_or(0)
    };

    // 4. Download the archive with concurrent progress polling.
    on_progress(format!(
        "Downloading {url}{}...",
        if total_bytes > 0 {
            format!(" ({:.1} MB)", total_bytes as f64 / 1_048_576.0)
        } else {
            String::new()
        }
    ));

    let dl_script = format!(
        "command -v curl >/dev/null 2>&1 && \
         curl -fSL {url} -o {archive} || \
         wget -q -O {archive} {url}",
        url     = shell_escape(&url),
        archive = shell_escape(&archive),
    );
    let dl_cmd = format!("sh -c {}", shell_escape(&dl_script));

    // `stat -f%z` is macOS; `stat -c%s` is Linux.  Try both.
    let size_cmd = format!(
        "sh -c {}",
        shell_escape(&format!(
            "stat -f%z {a} 2>/dev/null || stat -c%s {a} 2>/dev/null || echo 0",
            a = shell_escape(&archive),
        )),
    );

    // Run download and size-polling concurrently.  `select!` cancels the
    // polling branch as soon as the download future resolves.
    let output = tokio::select! {
        result = conn.run_command_with_status(&dl_cmd) => {
            result.context("download command failed to launch")?
        }
        _ = async {
            let mut interval =
                tokio::time::interval(std::time::Duration::from_millis(500));
            loop {
                interval.tick().await;
                if let Ok(s) = conn.run_command(&size_cmd).await {
                    let current: u64 = s.trim().parse().unwrap_or(0);
                    if current == 0 { continue; }
                    if total_bytes > 0 {
                        let pct = (current * 100 / total_bytes).min(99);
                        on_progress(format!(
                            "Downloading... {pct}% ({:.1} / {:.1} MB)",
                            current as f64 / 1_048_576.0,
                            total_bytes as f64 / 1_048_576.0,
                        ));
                    } else {
                        on_progress(format!(
                            "Downloading... {:.1} MB",
                            current as f64 / 1_048_576.0,
                        ));
                    }
                }
            }
        } => { unreachable!() }
    };

    if !output.is_success() {
        conn.run_command(&cleanup).await.ok();
        anyhow::bail!(
            "download failed (exit {:?}):\n{}",
            output.exit_code,
            output.stderr.trim()
        );
    }
    on_progress("Download complete.".to_string());

    // 4. Extract the binary from the archive.
    on_progress("Extracting binary from archive...".to_string());
    let output = conn
        .run_command_with_status(&format!(
            "tar -xzf {} -C {} emacs-lsp-proxy",
            shell_escape(&archive),
            shell_escape(&tmpdir),
        ))
        .await
        .context("extraction command failed to launch")?;
    if !output.is_success() {
        conn.run_command(&cleanup).await.ok();
        anyhow::bail!(
            "extraction failed (exit {:?}):\n{}",
            output.exit_code,
            output.stderr.trim()
        );
    }
    on_progress("Extraction complete.".to_string());

    // 5. Move the binary into place and make it executable.
    on_progress(format!("Installing to {remote_path}..."));
    let install_script = format!(
        "mv {src} {dest} && chmod +x {dest}",
        src  = shell_escape(&extracted),
        dest = shell_escape(remote_path),
    );
    let output = conn
        .run_command_with_status(&format!("sh -c {}", shell_escape(&install_script)))
        .await
        .context("install command failed to launch")?;
    conn.run_command(&cleanup).await.ok();
    if !output.is_success() {
        anyhow::bail!(
            "install failed (exit {:?}):\n{}",
            output.exit_code,
            output.stderr.trim()
        );
    }

    on_progress(format!("Installed {remote_path} v{EXPECTED_VERSION}."));
    info!("installed {remote_path} v{EXPECTED_VERSION} from {archive_name}");
    Ok(())
}

/// Check whether a compatible binary is already available on the remote host.
///
/// Priority: global `emacs-lsp-proxy` in PATH first, then `remote_path`.
/// Returns the command/path to use, or `Err` if neither location has a
/// matching binary — the caller should then ask the user to run a deploy.
pub async fn check_binary_available(
    conn: &SshConnection,
    remote_path: &str,
) -> Result<String> {
    match check_remote_binary(conn, "emacs-lsp-proxy").await? {
        RemoteBinaryStatus::VersionMatch => {
            info!("using global emacs-lsp-proxy on remote (v{EXPECTED_VERSION})");
            return Ok("emacs-lsp-proxy".to_string());
        }
        RemoteBinaryStatus::Missing => {
            debug!("emacs-lsp-proxy not found in remote PATH, checking {remote_path}");
        }
        RemoteBinaryStatus::VersionMismatch { remote } => {
            info!(
                "global emacs-lsp-proxy on remote is v{remote} (need v{EXPECTED_VERSION}), \
                 checking {remote_path}"
            );
        }
    }

    match check_remote_binary(conn, remote_path).await? {
        RemoteBinaryStatus::VersionMatch => {
            debug!("remote binary at {remote_path} is up to date");
            Ok(remote_path.to_string())
        }
        RemoteBinaryStatus::Missing => Err(anyhow!(
            "emacs-lsp-proxy not found (checked global PATH and {remote_path})"
        )),
        RemoteBinaryStatus::VersionMismatch { remote } => Err(anyhow!(
            "emacs-lsp-proxy version mismatch: remote has v{remote}, need v{EXPECTED_VERSION} \
             (checked global PATH and {remote_path})"
        )),
    }
}

/// Deploy the correct release binary to `remote_path`, reporting progress via
/// `on_progress`.
///
/// Checks the global command and `remote_path` first; if either already has a
/// matching version no download is performed.  When a download is needed the
/// remote platform is detected automatically and the appropriate release
/// archive is fetched from GitHub.  Returns the command/path to use.
pub async fn deploy_with_progress(
    conn: &SshConnection,
    remote_path: &str,
    on_progress: &(impl Fn(String) + Send + Sync),
) -> Result<String> {
    on_progress(format!(
        "Checking global emacs-lsp-proxy on remote (need v{EXPECTED_VERSION})..."
    ));
    match check_remote_binary(conn, "emacs-lsp-proxy").await? {
        RemoteBinaryStatus::VersionMatch => {
            on_progress(format!(
                "Global emacs-lsp-proxy v{EXPECTED_VERSION} found — no download needed."
            ));
            return Ok("emacs-lsp-proxy".to_string());
        }
        RemoteBinaryStatus::Missing => {
            on_progress("Global emacs-lsp-proxy not found in remote PATH.".to_string());
        }
        RemoteBinaryStatus::VersionMismatch { remote } => {
            on_progress(format!(
                "Global emacs-lsp-proxy is v{remote}, need v{EXPECTED_VERSION}."
            ));
        }
    }

    on_progress(format!("Checking {remote_path}..."));
    match check_remote_binary(conn, remote_path).await? {
        RemoteBinaryStatus::VersionMatch => {
            on_progress(format!(
                "{remote_path} is already v{EXPECTED_VERSION} — no download needed."
            ));
            return Ok(remote_path.to_string());
        }
        RemoteBinaryStatus::Missing => {
            on_progress(format!("{remote_path} not found, downloading..."));
        }
        RemoteBinaryStatus::VersionMismatch { remote } => {
            on_progress(format!(
                "{remote_path} is v{remote}, downloading v{EXPECTED_VERSION}..."
            ));
        }
    }

    on_progress("Detecting remote platform...".to_string());
    let (os, arch) = detect_remote_platform(conn).await?;
    on_progress(format!("Remote platform: {os} / {arch}"));

    let archive = release_archive_name(&os, &arch)?;
    on_progress(format!("Release archive: {archive}"));

    download_release_on_remote(conn, remote_path, &archive, on_progress).await?;

    on_progress("Verifying installed binary...".to_string());
    match check_remote_binary(conn, remote_path).await? {
        RemoteBinaryStatus::VersionMatch => {
            on_progress(format!("✓ Deploy successful: {remote_path} v{EXPECTED_VERSION}."));
            Ok(remote_path.to_string())
        }
        other => Err(anyhow!(
            "remote binary still not healthy after deploy: {other:?}"
        )),
    }
}

/// Parent directory component of a remote path string. We do string-level
/// splitting rather than `Path` because the remote may not use the same path
/// conventions (e.g. the caller passes `~/.local/bin/...`).
fn remote_parent_dir(remote_path: &str) -> String {
    match remote_path.rfind('/') {
        Some(0) => "/".to_string(),
        Some(idx) => remote_path[..idx].to_string(),
        None => ".".to_string(),
    }
}

/// Parse the output of `emacs-lsp-proxy --version`, which is of the form
/// `emacs-lsp-proxy 0.7.2`. Returns just the version token.
pub fn parse_version_output(output: &str) -> Option<&str> {
    output
        .lines()
        .next()?
        .trim()
        .rsplit_once(' ')
        .map(|(_, v)| v)
}

/// Very small shell escape for use inside single-quoted `sh -c '…'` wrappers
/// and path-only args. This is intentionally conservative: we only accept
/// characters that are safe without any quoting, so anything exotic just
/// surfaces a clearer error than silent expansion.
fn shell_escape(s: &str) -> String {
    if s.chars().all(is_shell_safe) {
        s.to_string()
    } else {
        // Wrap in single quotes and escape any internal quotes.
        let escaped = s.replace('\'', r#"'\''"#);
        format!("'{escaped}'")
    }
}

fn is_shell_safe(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '/' | '.' | '_' | '-' | '~')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_version_ok() {
        assert_eq!(
            parse_version_output("emacs-lsp-proxy 0.7.2\n"),
            Some("0.7.2")
        );
        assert_eq!(parse_version_output("emacs-lsp-proxy 0.8.0"), Some("0.8.0"));
        // Subsequent lines are ignored.
        assert_eq!(
            parse_version_output("emacs-lsp-proxy 1.0.0\nextra info\n"),
            Some("1.0.0")
        );
    }

    #[test]
    fn parse_version_handles_garbage() {
        // Single token with no space can't be parsed as "<name> <version>".
        assert_eq!(parse_version_output("nonsense"), None);
        assert_eq!(parse_version_output(""), None);
    }

    #[test]
    fn remote_parent_dir_cases() {
        assert_eq!(remote_parent_dir("~/.local/bin/foo"), "~/.local/bin");
        assert_eq!(remote_parent_dir("/usr/local/bin/foo"), "/usr/local/bin");
        assert_eq!(remote_parent_dir("/foo"), "/");
        assert_eq!(remote_parent_dir("plain"), ".");
    }

    #[test]
    fn shell_escape_safe_strings_untouched() {
        assert_eq!(
            shell_escape("~/.local/bin/emacs-lsp-proxy"),
            "~/.local/bin/emacs-lsp-proxy"
        );
        assert_eq!(
            shell_escape("/usr/local/bin/foo.sh"),
            "/usr/local/bin/foo.sh"
        );
    }

    #[test]
    fn shell_escape_quotes_dangerous_strings() {
        // Space must force quoting.
        assert_eq!(shell_escape("hello world"), "'hello world'");
        // Embedded single quote must be escaped.
        assert_eq!(shell_escape("it's"), r#"'it'\''s'"#);
        // Shell metacharacters must force quoting.
        assert_eq!(shell_escape("foo;rm -rf"), "'foo;rm -rf'");
    }

    #[test]
    fn status_enum_equality() {
        // Sanity-check the derived PartialEq so later tests can use assert_eq!.
        assert_eq!(RemoteBinaryStatus::Missing, RemoteBinaryStatus::Missing);
        assert_ne!(
            RemoteBinaryStatus::VersionMatch,
            RemoteBinaryStatus::VersionMismatch { remote: "x".into() }
        );
    }
}
