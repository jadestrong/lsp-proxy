use anyhow::{Result, anyhow, Context};
use log::{debug, error, info, warn};
use std::collections::HashMap;
use std::net::IpAddr;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::process::{Child, Command};
use tokio::io::{AsyncRead, AsyncWrite};
use tokio::sync::Mutex;
use tempfile::TempDir;

use super::RemoteHost;

/// Result of a remote command, preserving the exit code so callers can tell
/// "binary not found" (non-zero exit) from "transport failure" (error on the
/// Result).
#[derive(Debug, Clone)]
pub struct CommandOutput {
    pub exit_code: Option<i32>,
    pub stdout: String,
    pub stderr: String,
}

impl CommandOutput {
    pub fn is_success(&self) -> bool {
        self.exit_code == Some(0)
    }
}

/// SSH连接主机类型
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SshConnectionHost {
    IpAddr(IpAddr),
    Hostname(String),
}

impl SshConnectionHost {
    pub fn to_string(&self) -> String {
        match self {
            Self::IpAddr(ip) => ip.to_string(),
            Self::Hostname(hostname) => hostname.clone(),
        }
    }
}

impl From<&str> for SshConnectionHost {
    fn from(value: &str) -> Self {
        if let Ok(address) = value.parse() {
            Self::IpAddr(address)
        } else {
            Self::Hostname(value.to_string())
        }
    }
}

impl From<String> for SshConnectionHost {
    fn from(value: String) -> Self {
        if let Ok(address) = value.parse() {
            Self::IpAddr(address)
        } else {
            Self::Hostname(value)
        }
    }
}

/// SSH连接选项
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SshConnectionOptions {
    pub host: SshConnectionHost,
    pub username: Option<String>,
    pub port: Option<u16>,
    pub args: Option<Vec<String>>,
    pub connection_timeout: Option<u16>,
}

impl SshConnectionOptions {
    pub fn new(host: String, username: String) -> Self {
        // Empty username means "let ssh resolve via ~/.ssh/config", which is
        // the common case with SSH aliases like `Host home\n  User me`.
        let username = if username.is_empty() { None } else { Some(username) };
        Self {
            host: host.into(),
            username,
            port: None,
            args: None,
            connection_timeout: Some(30),
        }
    }

    pub fn with_port(mut self, port: u16) -> Self {
        self.port = Some(port);
        self
    }

    pub fn destination(&self) -> String {
        let host_str = self.host.to_string();
        match &self.username {
            Some(username) => format!("{}@{}", username, host_str),
            None => host_str,
        }
    }

    pub fn connection_key(&self) -> String {
        match self.port {
            Some(port) => format!("{}:{}", self.destination(), port),
            None => self.destination(),
        }
    }
}

/// SSH Socket封装
struct SshSocket {
    connection_options: SshConnectionOptions,
    #[cfg(not(target_os = "windows"))]
    socket_path: PathBuf,
    envs: HashMap<String, String>,
}

/// Maximum time we wait for the SSH master to become ready (socket present
/// and `ssh -O check` succeeds).
const MASTER_READY_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(30);
/// How often we poll for readiness. Keeping this short makes the happy path
/// (local or cached-key SSH targets) feel instant.
const MASTER_POLL_INTERVAL: std::time::Duration = std::time::Duration::from_millis(50);

/// SSH 主连接进程。持有启动的 ssh master 子进程,以及为就绪探测所需的元数据。
struct MasterProcess {
    process: Child,
    #[cfg(not(target_os = "windows"))]
    socket_path: std::path::PathBuf,
    #[cfg(not(target_os = "windows"))]
    destination: String,
}

impl MasterProcess {
    #[cfg(not(target_os = "windows"))]
    pub async fn new(
        socket_path: &std::path::Path,
        destination: &str,
        additional_args: Vec<String>,
    ) -> Result<Self> {
        let args = [
            "-N",
            "-o",
            "ControlPersist=no",
            "-o",
            "ControlMaster=yes",
            "-o",
            "StrictHostKeyChecking=no",
            "-o",
            "UserKnownHostsFile=/dev/null",
        ];

        let mut master_process = Command::new("ssh");
        master_process
            .kill_on_drop(true)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .args(&args)
            .args(&additional_args)
            .arg("-o")
            .arg(format!("ControlPath={}", socket_path.display()))
            .arg(destination);

        debug!("Starting SSH master process: ssh {}", destination);
        let process = master_process
            .spawn()
            .context("Failed to spawn SSH master process")?;

        Ok(MasterProcess {
            process,
            socket_path: socket_path.to_path_buf(),
            destination: destination.to_string(),
        })
    }

    #[cfg(target_os = "windows")]
    pub async fn new(destination: &str, additional_args: Vec<String>) -> Result<Self> {
        // Windows 不走 ControlMaster,仍保留一个 echo 标记后 sleep 的长连接占位。
        let args = [
            "-o",
            "StrictHostKeyChecking=no",
            "-o",
            "UserKnownHostsFile=NUL",
        ];

        let mut master_process = Command::new("ssh");
        master_process
            .kill_on_drop(true)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .args(&args)
            .args(&additional_args)
            .arg(destination)
            .arg("echo SSH_CONNECTION_ESTABLISHED; sleep 3600");

        debug!("Starting SSH master process: ssh {}", destination);
        let process = master_process
            .spawn()
            .context("Failed to spawn SSH master process")?;

        Ok(MasterProcess { process })
    }

    /// Wait until the SSH master is actually usable, not just started.
    ///
    /// Unix: poll for the control socket file, then verify the master is alive
    /// with `ssh -O check`. Bail immediately if the master process exits early
    /// (surfacing its stderr to make auth/host errors debuggable).
    ///
    /// Windows: wait for the marker string on stdout, since there's no control
    /// socket to probe.
    #[cfg(not(target_os = "windows"))]
    pub async fn wait_connected(&mut self) -> Result<()> {
        let deadline = std::time::Instant::now() + MASTER_READY_TIMEOUT;

        loop {
            if let Some(status) = self.process.try_wait()? {
                let stderr_msg = self.drain_stderr().await;
                return Err(anyhow!(
                    "SSH master exited early with status {}: {}",
                    status,
                    stderr_msg.trim()
                ));
            }

            if self.socket_path.exists()
                && ssh_check_master(&self.socket_path, &self.destination).await?
            {
                info!(
                    "SSH master ready (socket {})",
                    self.socket_path.display()
                );
                return Ok(());
            }

            if std::time::Instant::now() >= deadline {
                return Err(anyhow!(
                    "SSH master not ready within {:?} (socket {} never responded)",
                    MASTER_READY_TIMEOUT,
                    self.socket_path.display()
                ));
            }

            tokio::time::sleep(MASTER_POLL_INTERVAL).await;
        }
    }

    #[cfg(target_os = "windows")]
    pub async fn wait_connected(&mut self) -> Result<()> {
        use tokio::io::{AsyncBufReadExt, BufReader};

        let deadline = std::time::Instant::now() + MASTER_READY_TIMEOUT;
        let Some(stdout) = self.process.stdout.take() else {
            return Err(anyhow!("SSH master has no captured stdout to monitor"));
        };
        let mut lines = BufReader::new(stdout).lines();

        loop {
            if let Some(status) = self.process.try_wait()? {
                return Err(anyhow!(
                    "SSH master exited early with status {}",
                    status
                ));
            }

            let remaining = deadline.saturating_duration_since(std::time::Instant::now());
            if remaining.is_zero() {
                return Err(anyhow!(
                    "SSH master did not emit readiness marker within {:?}",
                    MASTER_READY_TIMEOUT
                ));
            }

            match tokio::time::timeout(remaining, lines.next_line()).await {
                Ok(Ok(Some(line))) if line.contains("SSH_CONNECTION_ESTABLISHED") => {
                    info!("SSH master ready (saw readiness marker)");
                    return Ok(());
                }
                Ok(Ok(Some(_))) => continue,
                Ok(Ok(None)) => {
                    return Err(anyhow!("SSH master stdout closed before readiness marker"));
                }
                Ok(Err(e)) => return Err(anyhow!("error reading SSH master stdout: {}", e)),
                Err(_) => {} // tick around the outer loop so we can recheck try_wait
            }
        }
    }

    /// Non-blocking best-effort drain of the master's stderr. Called only
    /// after the process has already exited, so reading to EOF is safe.
    async fn drain_stderr(&mut self) -> String {
        let Some(mut stderr) = self.process.stderr.take() else {
            return String::new();
        };
        let mut buf = Vec::new();
        let _ = tokio::time::timeout(
            std::time::Duration::from_millis(200),
            tokio::io::AsyncReadExt::read_to_end(&mut stderr, &mut buf),
        )
        .await;
        String::from_utf8_lossy(&buf).into_owned()
    }
}

/// Verify the ssh master at `socket_path` is still responsive.
#[cfg(not(target_os = "windows"))]
async fn ssh_check_master(
    socket_path: &std::path::Path,
    destination: &str,
) -> Result<bool> {
    let output = Command::new("ssh")
        .arg("-o")
        .arg(format!("ControlPath={}", socket_path.display()))
        .arg("-O")
        .arg("check")
        .arg(destination)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .output()
        .await
        .context("Failed to run `ssh -O check`")?;
    Ok(output.status.success())
}

impl SshSocket {
    pub fn new(connection_options: SshConnectionOptions) -> Result<Self> {
        #[cfg(not(target_os = "windows"))]
        let socket_path = {
            let temp_dir = std::env::temp_dir();
            temp_dir.join(format!("lsp-proxy-ssh-{}", uuid::Uuid::new_v4()))
        };

        let envs = HashMap::new();

        Ok(Self {
            connection_options,
            #[cfg(not(target_os = "windows"))]
            socket_path,
            envs,
        })
    }

    pub async fn ssh_command(&self, command: &str) -> Result<Vec<u8>> {
        let mut cmd = Command::new("ssh");

        #[cfg(not(target_os = "windows"))]
        cmd.arg("-o")
            .arg(format!("ControlPath={}", self.socket_path.display()));

        cmd.arg("-o")
            .arg("ControlMaster=no")
            .arg(self.connection_options.destination())
            .arg(command);

        debug!("Executing SSH command: {}", command);
        let output = cmd.output().await?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!("SSH command failed: {}", stderr));
        }

        Ok(output.stdout)
    }
}

/// SSH连接管理器
pub struct SshConnection {
    socket: SshSocket,
    master_process: Arc<Mutex<Option<MasterProcess>>>,
    _temp_dir: TempDir,
}

impl SshConnection {
    pub async fn connect(options: SshConnectionOptions) -> Result<Self> {
        let socket = SshSocket::new(options.clone())?;
        let temp_dir = tempfile::tempdir().context("Failed to create temp directory")?;

        let connection = Self {
            socket,
            master_process: Arc::new(Mutex::new(None)),
            _temp_dir: temp_dir,
        };

        // 建立SSH主连接
        connection.ensure_master_connection().await?;

        Ok(connection)
    }

    async fn ensure_master_connection(&self) -> Result<()> {
        let mut master_guard = self.master_process.lock().await;

        if master_guard.is_none() {
            let destination = self.socket.connection_options.destination();
            let additional_args = self.socket.connection_options.args
                .as_ref()
                .cloned()
                .unwrap_or_default();

            #[cfg(not(target_os = "windows"))]
            let master = MasterProcess::new(&self.socket.socket_path, &destination, additional_args).await?;

            #[cfg(target_os = "windows")]
            let master = MasterProcess::new(&destination, additional_args).await?;

            *master_guard = Some(master);
        }

        if let Some(master) = master_guard.as_mut() {
            master.wait_connected().await?;
        }

        Ok(())
    }

    pub async fn run_command(&self, command: &str) -> Result<String> {
        self.ensure_master_connection().await?;

        let output = self.socket.ssh_command(command).await?;
        let result = String::from_utf8(output).context("Invalid UTF-8 in command output")?;

        debug!("SSH command result: {}", result.trim());
        Ok(result)
    }

    /// Run a command on the remote host and return the full exit details.
    ///
    /// Unlike [`run_command`], this does NOT treat a non-zero exit code as an
    /// error — callers who care about e.g. "binary not found" (exit 127)
    /// versus "ssh transport failed" need that distinction.
    pub async fn run_command_with_status(&self, command: &str) -> Result<CommandOutput> {
        self.ensure_master_connection().await?;

        let mut cmd = Command::new("ssh");
        #[cfg(not(target_os = "windows"))]
        cmd.arg("-o")
            .arg(format!("ControlPath={}", self.socket.socket_path.display()));
        cmd.arg("-o")
            .arg("ControlMaster=no")
            .arg(self.socket.connection_options.destination())
            .arg(command);

        debug!("Executing SSH command (with status): {}", command);
        let output = cmd.output().await.context("ssh command failed to launch")?;
        Ok(CommandOutput {
            exit_code: output.status.code(),
            stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
            stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        })
    }

    /// Copy a local file to a remote path via `scp`, reusing the SSH
    /// ControlMaster socket for zero-handshake transfer. Caller should ensure
    /// the parent directory exists (use [`run_command`] with `mkdir -p`).
    pub async fn scp_upload(
        &self,
        local_path: &std::path::Path,
        remote_path: &str,
    ) -> Result<()> {
        self.ensure_master_connection().await?;

        let mut cmd = Command::new("scp");
        #[cfg(not(target_os = "windows"))]
        cmd.arg("-o")
            .arg(format!("ControlPath={}", self.socket.socket_path.display()));
        cmd.arg("-o")
            .arg("ControlMaster=no")
            .arg(local_path)
            .arg(format!(
                "{}:{}",
                self.socket.connection_options.destination(),
                remote_path
            ));

        debug!(
            "scp {} -> {}:{}",
            local_path.display(),
            self.socket.connection_options.destination(),
            remote_path
        );
        let output = cmd.output().await.context("scp failed to launch")?;
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!(
                "scp upload failed (exit {}): {}",
                output.status,
                stderr.trim()
            ));
        }
        Ok(())
    }

    pub async fn start_remote_lsp_proxy(&self, remote_path: &str) -> Result<SshLspProcess> {
        self.ensure_master_connection().await?;

        // 构造启动远程LSP代理的命令。远程端必须使用 --remote-server 模式,
        // 这样 stdio 上走的是 Protobuf Envelope 协议,与本地 RpcClient 对齐。
        let command = format!("{} --remote-server", remote_path);

        let mut cmd = Command::new("ssh");

        #[cfg(not(target_os = "windows"))]
        cmd.arg("-o")
            .arg(format!("ControlPath={}", self.socket.socket_path.display()));

        cmd.arg("-o")
            .arg("ControlMaster=no")
            .arg(self.socket.connection_options.destination())
            .arg(&command)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped());

        info!("Starting remote LSP proxy: {}", command);
        let mut process = cmd.spawn().context("Failed to spawn remote LSP proxy")?;

        let stdin = process.stdin.take().ok_or_else(|| anyhow!("Failed to get stdin"))?;
        let stdout = process.stdout.take().ok_or_else(|| anyhow!("Failed to get stdout"))?;

        Ok(SshLspProcess {
            process,
            stdin,
            stdout,
        })
    }

    pub fn connection_key(&self) -> String {
        self.socket.connection_options.connection_key()
    }
}

/// SSH LSP进程包装
pub struct SshLspProcess {
    process: Child,
    pub stdin: tokio::process::ChildStdin,
    pub stdout: tokio::process::ChildStdout,
}

impl SshLspProcess {
    pub async fn wait(&mut self) -> Result<std::process::ExitStatus> {
        self.process.wait().await.context("SSH LSP process wait failed")
    }

    pub fn kill(&mut self) -> Result<()> {
        self.process.start_kill().context("Failed to kill SSH LSP process")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_connection_options() {
        let options = SshConnectionOptions::new("example.com".to_string(), "user".to_string())
            .with_port(2222);

        assert_eq!(options.destination(), "user@example.com");
        assert_eq!(options.connection_key(), "user@example.com:2222");
    }

    #[test]
    fn test_ssh_host_parsing() {
        let host: SshConnectionHost = "192.168.1.1".into();
        match host {
            SshConnectionHost::IpAddr(_) => {}
            _ => panic!("Expected IP address"),
        }

        let host: SshConnectionHost = "example.com".into();
        match host {
            SshConnectionHost::Hostname(name) => assert_eq!(name, "example.com"),
            _ => panic!("Expected hostname"),
        }
    }

    #[cfg(not(target_os = "windows"))]
    #[tokio::test]
    async fn ssh_check_master_returns_false_for_missing_socket() {
        // Pointing at a socket that doesn't exist must surface as "not alive"
        // rather than panicking or hanging.
        let tmp = std::env::temp_dir().join(format!(
            "lsp-proxy-ssh-test-{}",
            uuid::Uuid::new_v4()
        ));
        assert!(!tmp.exists());
        let alive = ssh_check_master(&tmp, "nobody@nowhere.invalid")
            .await
            .expect("ssh binary present");
        assert!(!alive, "missing socket must not be reported as alive");
    }

    #[cfg(not(target_os = "windows"))]
    #[tokio::test]
    async fn wait_connected_reports_early_exit_fast() {
        // Stand in for the ssh master with a program that exits immediately so
        // we can exercise the early-exit branch without a real SSH target.
        let child = Command::new("sh")
            .arg("-c")
            .arg("echo 'boom' 1>&2; exit 7")
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .kill_on_drop(true)
            .spawn()
            .expect("spawn test master");

        let socket_path = std::env::temp_dir()
            .join(format!("lsp-proxy-ssh-test-{}", uuid::Uuid::new_v4()));
        let mut master = MasterProcess {
            process: child,
            socket_path,
            destination: "nobody@nowhere.invalid".to_string(),
        };

        let start = std::time::Instant::now();
        let err = master
            .wait_connected()
            .await
            .expect_err("master exits non-zero, wait should fail");
        let elapsed = start.elapsed();

        assert!(
            elapsed < std::time::Duration::from_secs(2),
            "early-exit detection took {:?}, expected well under MASTER_READY_TIMEOUT",
            elapsed
        );
        let msg = format!("{:#}", err);
        assert!(msg.contains("exited early"), "unexpected message: {msg}");
        assert!(
            msg.contains("boom"),
            "stderr ('boom') should be surfaced in the error, got: {msg}"
        );
    }
}