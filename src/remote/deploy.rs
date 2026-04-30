//! Auto-deploy: make sure a compatible `emacs-lsp-proxy` binary lives at the
//! expected path on the remote host before we try to start it.
//!
//! The check runs `<remote_path> --version` over the SSH ControlMaster. If the
//! binary is missing or reports a different version than the local one, we
//! upload the currently-running executable via `scp` (reusing the same socket,
//! so it's a single round trip).

use anyhow::{anyhow, Context, Result};
use log::{debug, info, warn};
use std::path::{Path, PathBuf};

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
    let command = format!("sh -c '{} --version 2>/dev/null'", shell_escape(remote_path));
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

/// Upload the local running binary to `remote_path`. Creates the parent
/// directory and chmods +x afterwards.
pub async fn deploy_binary(
    conn: &SshConnection,
    local_path: &Path,
    remote_path: &str,
) -> Result<()> {
    let parent = remote_parent_dir(remote_path);
    let mkdir = format!("mkdir -p {}", shell_escape(&parent));
    debug!("preparing remote dir: {}", mkdir);
    conn.run_command(&mkdir)
        .await
        .context("failed to create remote install directory")?;

    info!(
        "uploading {} -> {} (v{})",
        local_path.display(),
        remote_path,
        EXPECTED_VERSION
    );
    conn.scp_upload(local_path, remote_path)
        .await
        .context("scp upload failed")?;

    let chmod = format!("chmod +x {}", shell_escape(remote_path));
    conn.run_command(&chmod)
        .await
        .context("failed to chmod remote binary")?;

    Ok(())
}

/// Make sure the remote binary is present at the right version, deploying
/// the local binary if needed. Returns the final path the caller should use
/// when launching the remote server.
pub async fn ensure_remote_binary(conn: &SshConnection, remote_path: &str) -> Result<()> {
    let status = check_remote_binary(conn, remote_path).await?;
    match &status {
        RemoteBinaryStatus::VersionMatch => {
            debug!("remote binary at {} is up to date", remote_path);
            return Ok(());
        }
        RemoteBinaryStatus::Missing => {
            info!("remote binary {} missing, deploying", remote_path);
        }
        RemoteBinaryStatus::VersionMismatch { remote } => {
            info!(
                "remote binary {} version mismatch ({} vs local {}), redeploying",
                remote_path, remote, EXPECTED_VERSION
            );
        }
    }

    let local_path = current_binary_path().context("locating local lsp-proxy binary")?;
    deploy_binary(conn, &local_path, remote_path).await?;

    // Post-deploy sanity: the new binary should now report the expected version.
    match check_remote_binary(conn, remote_path).await? {
        RemoteBinaryStatus::VersionMatch => Ok(()),
        other => Err(anyhow!(
            "after deploy, remote binary still not healthy: {:?}",
            other
        )),
    }
}

/// Best-guess path to the currently-running executable, for `scp`-ing.
fn current_binary_path() -> Result<PathBuf> {
    std::env::current_exe().context("std::env::current_exe() failed")
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
        format!("'{}'", escaped)
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
        assert_eq!(
            parse_version_output("emacs-lsp-proxy 0.8.0"),
            Some("0.8.0")
        );
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
        assert_eq!(shell_escape("~/.local/bin/emacs-lsp-proxy"), "~/.local/bin/emacs-lsp-proxy");
        assert_eq!(shell_escape("/usr/local/bin/foo.sh"), "/usr/local/bin/foo.sh");
    }

    #[test]
    fn shell_escape_quotes_dangerous_strings() {
        // Space must force quoting.
        assert_eq!(shell_escape("hello world"), "'hello world'");
        // Embedded single quote must be escaped.
        assert_eq!(
            shell_escape("it's"),
            r#"'it'\''s'"#
        );
        // Shell metacharacters must force quoting.
        assert_eq!(shell_escape("foo;rm -rf"), "'foo;rm -rf'");
    }

    #[test]
    fn status_enum_equality() {
        // Sanity-check the derived PartialEq so later tests can use assert_eq!.
        assert_eq!(RemoteBinaryStatus::Missing, RemoteBinaryStatus::Missing);
        assert_ne!(
            RemoteBinaryStatus::VersionMatch,
            RemoteBinaryStatus::VersionMismatch {
                remote: "x".into()
            }
        );
    }
}
