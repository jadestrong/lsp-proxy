use anyhow::{anyhow, Context, Result};
use log::{debug, info, warn};
use std::collections::HashMap;
use std::net::IpAddr;
use std::path::PathBuf;
use std::sync::Arc;
use tempfile::TempDir;
use tokio::process::{Child, Command};
use tokio::sync::Mutex;

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

impl std::fmt::Display for SshConnectionHost {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IpAddr(ip) => write!(f, "{}", ip),
            Self::Hostname(hostname) => write!(f, "{}", hostname),
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
        let username = if username.is_empty() {
            None
        } else {
            Some(username)
        };
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
            Some(username) => format!("{username}@{host_str}"),
            None => host_str,
        }
    }

    #[allow(dead_code)]
    pub fn connection_key(&self) -> String {
        match self.port {
            Some(port) => format!("{}:{}", self.destination(), port),
            None => self.destination(),
        }
    }

    /// Returns the `-p <port>` argument pair when a non-default port is set,
    /// or an empty vec otherwise. Use with `cmd.args(options.port_args())`.
    pub fn port_args(&self) -> Vec<String> {
        match self.port {
            Some(port) => vec!["-p".to_string(), port.to_string()],
            None => vec![],
        }
    }
}

/// SSH Socket封装
struct SshSocket {
    connection_options: SshConnectionOptions,
    #[cfg(not(target_os = "windows"))]
    socket_path: PathBuf,
    #[allow(dead_code)]
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

impl Drop for MasterProcess {
    fn drop(&mut self) {
        // Dropping a tokio Child does NOT kill the process; we must do it
        // explicitly so that all muxed sessions (including remote
        // `emacs-lsp-proxy --remote-server` processes) receive SIGHUP and
        // exit when lsp-proxy restarts or a connection is evicted.
        //
        // With ControlPersist=600 the foreground ssh daemonises immediately,
        // so `self.process` is already dead by the time we reach here.
        // We must use `ssh -O exit` to signal the background ControlMaster
        // daemon; only then will muxed sessions receive a proper SSH
        // disconnect.  Fall back to SIGKILL as a belt-and-suspenders measure.
        //
        // `start_kill()` is synchronous (SIGKILL, no await needed) and safe
        // to call from any context, including outside the tokio runtime.
        // We intentionally avoid `tokio::spawn` here — Drop can be invoked
        // after the runtime has shut down, and spawn would panic in that case.
        #[cfg(not(target_os = "windows"))]
        {
            let _ = std::process::Command::new("ssh")
                .args([
                    "-S",
                    &self.socket_path.to_string_lossy(),
                    "-O",
                    "exit",
                    "-o",
                    "LogLevel=ERROR",
                    &self.destination,
                ])
                .output();
            let _ = std::fs::remove_file(&self.socket_path);
        }

        let _ = self.process.start_kill();
    }
}

impl MasterProcess {
    /// Sentinel printed by the remote command to signal the SSH connection is
    /// ready. Shared between `new()` (which embeds it in the remote command)
    /// and `wait_connected()` (which scans stdout for it).
    #[cfg(target_os = "windows")]
    const CONNECTION_ESTABLISHED_MAGIC: &'static str = "LSP_PROXY_SSH_CONNECTION_ESTABLISHED";

    #[cfg(not(target_os = "windows"))]
    pub async fn new(
        socket_path: &std::path::Path,
        destination: &str,
        additional_args: Vec<String>,
    ) -> Result<Self> {
        let args = [
            "-N",
            // ControlPersist=600 keeps the master alive for up to 600 seconds
            // (10 minutes) after the last mux client disconnects.  This serves
            // as an automatic cleanup fallback: when lsp-proxy is killed without
            // running its exit hook (e.g. SIGKILL from the OS), the ControlMaster
            // will eventually idle out, close its TCP connection with a proper
            // FIN, and the remote sshd will cleanly EOF the remote process's
            // stdin — causing emacs-lsp-proxy --remote-server to exit on its own.
            // "yes" would keep it alive indefinitely, which leaks the remote
            // process forever in that scenario.
            "-o",
            "ControlPersist=600",
            "-o",
            "ControlMaster=yes",
            // Keepalive: send an SSH-level probe every 15s; if 3 go
            // unanswered (~45s of silence) ssh tears the tunnel down
            // instead of letting it sit half-dead until the OS TCP
            // keepalive eventually notices (which can be hours on macOS).
            // This is what turns a silently-broken link into a fast
            // EOF that our `is_dead()` path can react to.
            "-o",
            "ServerAliveInterval=15",
            "-o",
            "ServerAliveCountMax=3",
            // Don't prompt for host-key confirmation when running
            // non-interactively; we treat the SSH config's trust of the host
            // as sufficient. Callers wanting stricter policy can override via
            // connection args.
            "-o",
            "StrictHostKeyChecking=accept-new",
            // Also silence the "Permanently added … to known hosts" warning
            // that litters stderr and made earlier debugging noisy.
            "-o",
            "LogLevel=ERROR",
        ];

        let mut master_process = Command::new("ssh");
        master_process
            .kill_on_drop(true)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .args(args)
            .args(&additional_args)
            .arg("-o")
            .arg(format!("ControlPath={}", socket_path.display()))
            .arg(destination);

        debug!("Starting SSH master process: ssh {destination}");
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
        // On Windows, `ControlMaster` and `ControlPath` are not supported:
        // https://github.com/PowerShell/Win32-OpenSSH/issues/405
        // https://github.com/PowerShell/Win32-OpenSSH/wiki/Project-Scope
        //
        // Using an ugly workaround to detect connection establishment
        // -N doesn't work with JumpHosts as windows openssh never closes stdin in that case
        let args = [
            "-t",
            &format!("echo '{}'; exec $0", Self::CONNECTION_ESTABLISHED_MAGIC),
        ];

        let mut master_process = Command::new("ssh");
        master_process
            .kill_on_drop(true)
            .stdin(std::process::Stdio::null())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .args(&additional_args)
            .arg(destination)
            .args(&args);

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
        // With ControlPersist=<seconds>, ssh forks a background daemon and the
        // foreground process exits 0 almost immediately. That's expected —
        // the real master lives on via the control socket. Treat exit 0 as
        // "daemonised successfully" and keep polling; only non-zero exits
        // are actual failures.
        let mut foreground_reaped = false;

        loop {
            if !foreground_reaped {
                if let Some(status) = self.process.try_wait()? {
                    if status.success() {
                        foreground_reaped = true;
                    } else {
                        let stderr_msg = self.drain_stderr().await;
                        return Err(anyhow!(
                            "SSH master exited early with status {}: {}",
                            status,
                            stderr_msg.trim()
                        ));
                    }
                }
            }

            if self.socket_path.exists()
                && ssh_check_master(&self.socket_path, &self.destination).await?
            {
                info!("SSH master ready (socket {})", self.socket_path.display());
                return Ok(());
            }

            if std::time::Instant::now() >= deadline {
                let detail = if foreground_reaped {
                    format!(
                        "ssh daemonised but socket {} never responded",
                        self.socket_path.display()
                    )
                } else {
                    format!("socket {} never appeared", self.socket_path.display())
                };
                return Err(anyhow!(
                    "SSH master not ready within {MASTER_READY_TIMEOUT:?} ({detail})"
                ));
            }

            tokio::time::sleep(MASTER_POLL_INTERVAL).await;
        }
    }

    #[cfg(target_os = "windows")]
    pub async fn wait_connected(&mut self) -> Result<()> {
        use tokio::io::{AsyncBufReadExt, BufReader};

        let Some(stdout) = self.process.stdout.take() else {
            return Err(anyhow!("SSH master has no captured stdout to monitor"));
        };
        let mut reader = BufReader::new(stdout);
        let mut line = String::new();

        loop {
            let n = reader.read_line(&mut line).await?;
            if n == 0 {
                return Err(anyhow!("SSH master stdout closed before readiness marker"));
            }
            if line.contains(Self::CONNECTION_ESTABLISHED_MAGIC) {
                info!("SSH master ready (saw readiness marker)");
                return Ok(());
            }
            line.clear();
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
async fn ssh_check_master(socket_path: &std::path::Path, destination: &str) -> Result<bool> {
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
            // sockaddr_un.sun_path is capped at 104 bytes on macOS / BSD.
            // Combined with macOS's default $TMPDIR (`/var/folders/…/T/`,
            // ~50 chars) and the ~17-char internal suffix OpenSSH appends
            // during atomic rename, a full UUID was busting the limit:
            //   /var/folders/d_/…/T/lsp-proxy-ssh-<uuid>.<xxxxxxxxx>  → >104
            // Anchor at /tmp and truncate the random id so the total path
            // stays comfortably short.
            let mut buf = std::path::PathBuf::from("/tmp");
            let uuid = uuid::Uuid::new_v4();
            // 16 hex chars of the UUID is 64 bits of entropy — more than
            // enough for per-session uniqueness.
            let short = format!("{:016x}", (uuid.as_u128() >> 64) as u64);
            buf.push(format!("lspp-{short}"));
            buf
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
            .arg("-T")
            .arg("-o")
            .arg("LogLevel=ERROR")
            .arg("-o")
            .arg("RemoteCommand=none")
            .args(self.connection_options.port_args())
            .arg(self.connection_options.destination())
            .arg(command);

        debug!("Executing SSH command: {command}");
        let output = cmd.output().await?;

        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            return Err(anyhow!("SSH command failed: {stderr}"));
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
    /// Kill the SSH ControlMaster process synchronously without async/await.
    ///
    /// Called from `kill_all_sync` (a synchronous shutdown path) just before
    /// `process::exit()`.  Killing the master causes all muxed sessions to
    /// receive SIGHUP, which terminates the remote `emacs-lsp-proxy --remote-server`
    /// processes.  Uses `try_lock` — if the lock is held the kill is skipped,
    /// but the `MasterProcess::drop` impl will still fire later when the Arc
    /// is released.
    pub fn kill_master_sync(&self) {
        if let Ok(mut guard) = self.master_process.try_lock() {
            if let Some(ref mut mp) = *guard {
                #[cfg(not(target_os = "windows"))]
                {
                    let socket = mp.socket_path.to_string_lossy();
                    let ssh_args_base = [
                        "-S",
                        &socket,
                        "-o",
                        "ControlMaster=no",
                        "-T",
                        "-o",
                        "LogLevel=ERROR",
                        "-o",
                        "BatchMode=yes",
                        &mp.destination,
                    ];

                    // Step 1: Directly kill the remote emacs-lsp-proxy process
                    // via the existing ControlMaster connection.
                    //
                    // On macOS, the remote host does not have a PTY (we use -T),
                    // so sshd never sends SIGHUP to the remote process group when
                    // the connection drops.  TCP RST also does not reliably close
                    // the remote process's stdin on macOS — it can survive for
                    // hours waiting for TCP keepalive to expire.  The only
                    // guaranteed fix is to kill it directly over SSH.
                    let kill_result = std::process::Command::new("ssh")
                        .args(ssh_args_base)
                        .arg("pkill -TERM -f 'emacs-lsp-proxy.*--remote-server'; true")
                        .output();
                    match kill_result {
                        Ok(_) => info!(
                            "sent SIGTERM to remote emacs-lsp-proxy on {}",
                            mp.destination
                        ),
                        Err(e) => warn!(
                            "could not send SIGTERM to remote on {}: {e}",
                            mp.destination
                        ),
                    }

                    // Step 2: Gracefully close the ControlMaster so it sends
                    // proper SSH disconnect messages.
                    let exit_result = std::process::Command::new("ssh")
                        .args([
                            "-S",
                            &socket,
                            "-O",
                            "exit",
                            "-o",
                            "LogLevel=ERROR",
                            &mp.destination,
                        ])
                        .output();
                    match exit_result {
                        Ok(out) if out.status.success() => {
                            info!("ssh -O exit succeeded for {}", mp.destination);
                        }
                        Ok(out) => warn!(
                            "ssh -O exit failed for {} (exit={:?}): {}",
                            mp.destination,
                            out.status.code(),
                            String::from_utf8_lossy(&out.stderr).trim()
                        ),
                        Err(e) => warn!("ssh -O exit could not run for {}: {e}", mp.destination),
                    }
                }

                // Fallback / belt-and-suspenders: SIGKILL the master process.
                // If `ssh -O exit` already caused it to exit this is a no-op.
                let _ = mp.process.start_kill();

                #[cfg(not(target_os = "windows"))]
                let _ = std::fs::remove_file(&mp.socket_path);
            }
            *guard = None;
        }
    }

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
            let port_args = self.socket.connection_options.port_args();
            let user_args = self
                .socket
                .connection_options
                .args
                .as_ref()
                .cloned()
                .unwrap_or_default();
            // Prepend port args so they appear before any user-supplied args.
            let mut additional_args = port_args;
            additional_args.extend(user_args);

            #[cfg(not(target_os = "windows"))]
            let master =
                MasterProcess::new(&self.socket.socket_path, &destination, additional_args).await?;

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
            .arg("-T")
            .arg("-o")
            .arg("LogLevel=ERROR")
            .arg("-o")
            .arg("RemoteCommand=none")
            .args(self.socket.connection_options.port_args())
            .arg(self.socket.connection_options.destination())
            .arg(command);

        debug!("Executing SSH command (with status): {command}");
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
    pub async fn scp_upload(&self, local_path: &std::path::Path, remote_path: &str) -> Result<()> {
        self.ensure_master_connection().await?;

        let mut cmd = Command::new("scp");
        #[cfg(not(target_os = "windows"))]
        cmd.arg("-o")
            .arg(format!("ControlPath={}", self.socket.socket_path.display()));
        cmd.arg("-o")
            .arg("ControlMaster=no")
            // scp auto-infers no-TTY, but LogLevel + RemoteCommand overrides
            // still help suppress spurious host config behaviour.
            .arg("-o")
            .arg("LogLevel=ERROR")
            .arg("-o")
            .arg("RemoteCommand=none")
            .args(self.socket.connection_options.port_args())
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
        // 把本地的 log-level 透传过去,远端才会写出有内容的 log
        // (默认落在远端 binary 所在目录下的 `remote-server.log`)。
        let log_level = crate::config::log_level();
        // Forward completion cap too. Remote defaults to 0 (the zero-valued
        // `usize` from `Args::default()`); if it ever runs without this flag
        // handle_completion slices every response to empty.
        let max_items = crate::config::MAX_COMPLETION_ITEMS
            .get()
            .copied()
            .unwrap_or(20);
        let mut inner = format!(
            "{remote_path} --remote-server --max-item {max_items}"
        );
        if log_level > 0 {
            inner.push_str(&format!(" --log-level {log_level}"));
        }

        // Execute the remote binary directly.  The binary itself calls
        // `load_login_shell_env()` at startup to capture and apply the login
        // shell's environment (PATH, nvm, Homebrew, etc.), so we do not need
        // any shell wrapper here.  Avoiding a wrapper removes all shell syntax
        // compatibility issues (bash vs fish vs zsh) and stdout-pollution risk.
        let command = inner;

        let mut cmd = Command::new("ssh");

        #[cfg(not(target_os = "windows"))]
        cmd.arg("-o")
            .arg(format!("ControlPath={}", self.socket.socket_path.display()));

        cmd.arg("-o")
            .arg("ControlMaster=no")
            // -T disables pseudo-terminal allocation. If the user's SSH
            // config has `RequestTTY yes` for this host, any PTY in the pipe
            // would CR/LF-mangle the raw Protobuf frames we're about to send.
            .arg("-T")
            // Silence "Permanently added … known hosts" noise on stderr.
            .arg("-o")
            .arg("LogLevel=ERROR")
            // Don't let a host-wide RemoteCommand (like `RemoteCommand /bin/zsh`)
            // override the binary we're trying to exec.
            .arg("-o")
            .arg("RemoteCommand=none")
            // Same keepalive as the master — applies to this session's ssh
            // channel (the one our Protobuf frames actually travel through).
            // Without this, a silently-dead link would just hang RPC requests
            // until our 30s per-request timeout fires, repeatedly.
            .arg("-o")
            .arg("ServerAliveInterval=15")
            .arg("-o")
            .arg("ServerAliveCountMax=3")
            .args(self.socket.connection_options.port_args())
            .arg(self.socket.connection_options.destination())
            .arg(&command)
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            // When the Child is dropped (together with SshRpcStream / RpcClient),
            // automatically send SIGKILL to the local SSH slave so the remote
            // process loses its connection and exits before the ControlMaster
            // is torn down.
            .kill_on_drop(true);

        info!("Starting remote LSP proxy: {command}");
        let mut process = cmd.spawn().context("Failed to spawn remote LSP proxy")?;

        let stdin = process
            .stdin
            .take()
            .ok_or_else(|| anyhow!("Failed to get stdin"))?;
        let stdout = process
            .stdout
            .take()
            .ok_or_else(|| anyhow!("Failed to get stdout"))?;
        // Surface the remote process's stderr into our log so that when ssh
        // or the remote binary dies (bad arg, not-executable, gatekeeper,
        // missing LD libs) we can see WHY instead of just "RPC channel closed".
        if let Some(stderr) = process.stderr.take() {
            tokio::spawn(async move {
                use tokio::io::{AsyncBufReadExt, BufReader};
                let mut lines = BufReader::new(stderr).lines();
                loop {
                    match lines.next_line().await {
                        Ok(Some(line)) => warn!("[remote stderr] {line}"),
                        Ok(None) => {
                            debug!("[remote stderr] EOF");
                            break;
                        }
                        Err(e) => {
                            warn!("[remote stderr] read error: {e}");
                            break;
                        }
                    }
                }
            });
        }

        Ok(SshLspProcess {
            process,
            stdin,
            stdout,
        })
    }

    #[allow(dead_code)]
    pub fn connection_key(&self) -> String {
        self.socket.connection_options.connection_key()
    }
}

/// SSH LSP进程包装
#[allow(dead_code)]
pub struct SshLspProcess {
    pub process: Child,
    pub stdin: tokio::process::ChildStdin,
    pub stdout: tokio::process::ChildStdout,
}

impl SshLspProcess {
    #[allow(dead_code)]
    pub async fn wait(&mut self) -> Result<std::process::ExitStatus> {
        self.process
            .wait()
            .await
            .context("SSH LSP process wait failed")
    }

    #[allow(dead_code)]
    pub fn kill(&mut self) -> Result<()> {
        self.process
            .start_kill()
            .context("Failed to kill SSH LSP process")
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
        let tmp = std::env::temp_dir().join(format!("lsp-proxy-ssh-test-{}", uuid::Uuid::new_v4()));
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

        let socket_path =
            std::env::temp_dir().join(format!("lsp-proxy-ssh-test-{}", uuid::Uuid::new_v4()));
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
