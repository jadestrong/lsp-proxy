//! Remote LSP client integration for local lsp-proxy
//!
//! This module provides functionality for the local lsp-proxy to detect remote files
//! opened via TRAMP and forward LSP requests to remote lsp-proxy-server instances.

use anyhow::{anyhow, Result};
use log::{debug, error, info, warn};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::TcpStream as TokioTcpStream;
use tokio::sync::{Mutex, RwLock};
use url::Url;
use uuid::Uuid;

use crate::remote::{RemoteAuth, RemoteMode, RemoteServerConfig};

/// SSH tunnel configuration
#[derive(Debug, Clone, Deserialize)]
pub struct SshTunnelConfig {
    pub host: String,
    pub port: u16,
    pub user: String,
    pub local_port: u16,
    pub identity_file: Option<PathBuf>,
}

/// Load remote LSP configuration from file
pub fn load_remote_lsp_config() -> Result<RemoteLspConfig> {
    let config_path = std::env::var("LSP_PROXY_REMOTE_CONFIG").unwrap_or_else(|_| {
        let home = std::env::var("HOME").unwrap_or(".".to_string());
        format!("{}/.config/lsp-proxy/remote-lsp.toml", home)
    });

    if !std::path::Path::new(&config_path).exists() {
        return Err(anyhow!("Remote LSP config file not found: {}", config_path));
    }

    let content = std::fs::read_to_string(&config_path)?;
    let config: RemoteLspConfig = toml::from_str(&content)
        .map_err(|e| anyhow!("Failed to parse remote LSP config: {}", e))?;

    Ok(config)
}

/// Remote LSP client configuration
#[derive(Debug, Clone, Deserialize, Default)]
pub struct RemoteLspConfig {
    /// Remote servers configuration (using existing RemoteServerConfig)
    pub servers: HashMap<String, RemoteServerConfig>,
    /// Host name mapping for TRAMP paths
    pub host_mapping: Option<HashMap<String, String>>,
}

impl RemoteLspConfig {
    /// Convert from legacy format to new format
    pub fn from_legacy(
        hosts: HashMap<String, LegacyRemoteHostConfig>,
        host_mapping: Option<HashMap<String, String>>,
    ) -> Self {
        let mut servers = HashMap::new();

        for (name, host_config) in hosts {
            let server_config = RemoteServerConfig {
                name: name.clone(),
                host: host_config.host.unwrap_or(name.clone()),
                port: host_config.port,
                user: host_config
                    .user
                    .unwrap_or_else(|| std::env::var("USER").unwrap_or("root".to_string())),
                auth: match host_config.connection_type {
                    LegacyConnectionType::SshPipe => {
                        if let Some(ssh_config) = host_config.ssh_pipe {
                            RemoteAuth::SshConfig {
                                host: Some(ssh_config.host),
                            }
                        } else {
                            RemoteAuth::SshConfig { host: None }
                        }
                    }
                    _ => RemoteAuth::SshConfig { host: None },
                },
                workspace_root: None,
                connection_timeout: None,
                mode: RemoteMode::Server {
                    auto_deploy: host_config.server_install_script.is_some(),
                    server_path: host_config.server_binary_path.map(PathBuf::from),
                },
            };
            servers.insert(name, server_config);
        }

        Self {
            servers,
            host_mapping,
        }
    }
}

// Legacy configuration types for backward compatibility
#[derive(Debug, Clone, Deserialize)]
struct LegacyRemoteHostConfig {
    pub host: Option<String>,
    pub port: Option<u16>,
    pub user: Option<String>,
    pub connection_type: LegacyConnectionType,
    pub ssh_pipe: Option<LegacySshPipeConfig>,
    pub server_install_script: Option<String>,
    pub server_binary_path: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
enum LegacyConnectionType {
    Tcp,
    SshTunnel,
    SshPipe,
}

#[derive(Debug, Clone, Deserialize)]
struct LegacySshPipeConfig {
    pub host: String,
    pub port: Option<u16>,
    pub user: String,
    pub identity_file: Option<PathBuf>,
    pub server_install_script: Option<String>,
    pub server_binary_path: Option<String>,
    pub config_file: Option<String>,
}

/// Protocol message types (same as server-side)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum ProxyMessage {
    StartLspServer {
        id: String,
        language: String,
        workspace_root: PathBuf,
        client_capabilities: Option<Value>,
    },
    StopLspServer {
        id: String,
    },
    LspRequest {
        server_id: String,
        method: String,
        params: Value,
        request_id: Option<u64>,
    },
    LspNotification {
        server_id: String,
        method: String,
        params: Value,
    },
    Response {
        id: String,
        result: Result<Value, String>,
    },
    LspResponse {
        server_id: String,
        response: Value,
    },
    Ping,
    Pong,
    ListServers,
    ServersResponse {
        servers: Vec<LspServerInfo>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LspServerInfo {
    pub id: String,
    pub language: String,
    pub workspace_root: PathBuf,
    pub status: String,
    pub pid: Option<u32>,
}

/// SSH tunnel process management
pub struct SshTunnel {
    process: tokio::process::Child,
    local_port: u16,
    host: String,
}

impl SshTunnel {
    pub async fn establish(config: &SshTunnelConfig) -> Result<Self> {
        info!(
            "Establishing SSH tunnel to {}:{} via local port {}",
            config.host, config.port, config.local_port
        );

        let mut cmd = tokio::process::Command::new("ssh");
        cmd.args(&[
            "-L",
            &format!("{}:127.0.0.1:9527", config.local_port),
            "-N", // 不执行远程命令
            "-o",
            "ExitOnForwardFailure=yes", // 转发失败时退出
            "-o",
            "StrictHostKeyChecking=no", // 跳过主机密钥检查 (仅用于开发)
            "-o",
            "UserKnownHostsFile=/dev/null", // 不保存主机密钥
            &format!("{}@{}", config.user, config.host),
        ]);

        if let Some(identity_file) = &config.identity_file {
            cmd.args(&["-i", &identity_file.to_string_lossy()]);
        }

        let process = cmd
            .spawn()
            .map_err(|e| anyhow!("Failed to spawn SSH process: {}", e))?;

        // 等待隧道建立
        tokio::time::sleep(tokio::time::Duration::from_secs(3)).await;

        // 测试隧道是否可用
        match TokioTcpStream::connect(format!("127.0.0.1:{}", config.local_port)).await {
            Ok(_) => {
                info!(
                    "SSH tunnel established successfully on port {}",
                    config.local_port
                );
            }
            Err(e) => {
                error!("SSH tunnel test failed: {}", e);
                return Err(anyhow!("SSH tunnel failed to establish: {}", e));
            }
        }

        Ok(Self {
            process,
            local_port: config.local_port,
            host: config.host.clone(),
        })
    }

    pub fn local_port(&self) -> u16 {
        self.local_port
    }
}

impl Drop for SshTunnel {
    fn drop(&mut self) {
        info!("Cleaning up SSH tunnel to {}", self.host);
        let _ = self.process.kill();
    }
}

/// SSH pipe connection using persistent SSH process
pub struct SshPipeConnection {
    ssh_process: Arc<Mutex<tokio::process::Child>>,
    stdin_writer: Arc<Mutex<tokio::process::ChildStdin>>,
    stdout_reader: Arc<Mutex<tokio::io::BufReader<tokio::process::ChildStdout>>>,
    host: String,
    server_binary: String,
}

impl SshPipeConnection {
    pub async fn new(config: &RemoteServerConfig) -> Result<Self> {
        info!(
            "Establishing SSH pipe connection to {}@{}",
            config.user, config.host
        );

        // First, ensure server is installed using existing SSH infrastructure
        // let ssh_connection = Arc::new(SSHConnection::new(config.clone()).await?);
        // ssh_connection.connect().await?;

        // if let RemoteMode::Server { auto_deploy: true, server_path } = &config.mode {
        //     if let Err(e) = Self::ensure_server_installed(&ssh_connection, server_path).await {
        //         warn!("Server installation check failed: {}", e);
        //     }
        // }

        let server_binary = match &config.mode {
            RemoteMode::Server {
                server_path: Some(path),
                ..
            } => path.to_string_lossy().to_string(),
            _ => "lsp-proxy-server".to_string(),
        };

        // Build SSH command that includes server installation check if needed
        let ssh_target = format!("{}@{}", config.user, config.host);

        // Create the remote command with optional installation
        // format!("{} --stdio", server_binary)
        let remote_cmd = if let RemoteMode::Server {
            auto_deploy: true, ..
        } = &config.mode
        {
            format!(
                "if ! command -v {} >/dev/null 2>&1; then \
                    echo 'Installing lsp-proxy-server...' >&2 && \
                    curl -fsSL https://raw.githubusercontent.com/jadestrong/lsp-proxy/main/scripts/server-install.sh | bash >&2; \
                 fi && {} --stdio",
                server_binary, server_binary
            )
        } else {
            format!("{} --stdio", server_binary)
        };

        info!(
            "Starting persistent SSH pipe: ssh {} {}",
            ssh_target, remote_cmd
        );

        // Create persistent SSH process
        let mut ssh_cmd = tokio::process::Command::new("ssh");
        ssh_cmd.args(&[
            "-o",
            "BatchMode=yes", // 非交互模式
            "-o",
            "StrictHostKeyChecking=no", // 跳过主机密钥检查 (仅用于开发)
            &ssh_target,
            &remote_cmd,
        ]);

        // Configure stdio
        ssh_cmd.stdin(std::process::Stdio::piped());
        ssh_cmd.stdout(std::process::Stdio::piped());
        ssh_cmd.stderr(std::process::Stdio::piped());

        // Spawn the SSH process
        let mut ssh_process = ssh_cmd
            .spawn()
            .map_err(|e| anyhow!("Failed to spawn SSH process: {}", e))?;

        // Extract stdin and stdout
        let stdin = ssh_process
            .stdin
            .take()
            .ok_or_else(|| anyhow!("Failed to get stdin handle"))?;
        let stdout = ssh_process
            .stdout
            .take()
            .ok_or_else(|| anyhow!("Failed to get stdout handle"))?;

        let stdout_reader = tokio::io::BufReader::new(stdout);

        info!(
            "Successfully established SSH pipe connection to {}",
            config.host
        );

        Ok(Self {
            ssh_process: Arc::new(Mutex::new(ssh_process)),
            stdin_writer: Arc::new(Mutex::new(stdin)),
            stdout_reader: Arc::new(Mutex::new(stdout_reader)),
            host: config.host.clone(),
            server_binary,
        })
    }

    pub async fn send_message(&self, message: ProxyMessage) -> Result<Option<ProxyMessage>> {
        let msg_json = serde_json::to_string(&message)?;
        debug!("Sending via SSH pipe to {}: {}", self.host, msg_json);

        // Write message to remote process stdin
        {
            let mut stdin_guard = self.stdin_writer.lock().await;
            let message_with_newline = format!("{}\n", msg_json);
            stdin_guard
                .write_all(message_with_newline.as_bytes())
                .await
                .map_err(|e| anyhow!("Failed to write to SSH pipe stdin: {}", e))?;
            stdin_guard
                .flush()
                .await
                .map_err(|e| anyhow!("Failed to flush SSH pipe stdin: {}", e))?;
        }

        // Read response from remote process stdout
        {
            let mut stdout_guard = self.stdout_reader.lock().await;
            let mut response_line = String::new();
            match stdout_guard.read_line(&mut response_line).await {
                Ok(0) => {
                    warn!("SSH pipe connection closed by remote");
                    return Ok(None);
                }
                Ok(_) => {
                    let response_content = response_line.trim();
                    if response_content.is_empty() {
                        return Ok(None);
                    }

                    debug!(
                        "Received via SSH pipe from {}: {}",
                        self.host, response_content
                    );
                    match serde_json::from_str::<ProxyMessage>(response_content) {
                        Ok(response) => Ok(Some(response)),
                        Err(e) => {
                            warn!(
                                "Failed to parse SSH pipe response from {}: {} - {}",
                                self.host, e, response_content
                            );
                            Ok(None)
                        }
                    }
                }
                Err(e) => Err(anyhow!("Failed to read from SSH pipe stdout: {}", e)),
            }
        }
    }

    // async fn ensure_server_installed(
    //     ssh_connection: &SSHConnection,
    //     server_path: &Option<PathBuf>,
    // ) -> Result<()> {
    //     // Check if server binary exists
    //     let binary_path = server_path
    //         .as_ref()
    //         .map(|p| p.to_string_lossy().to_string())
    //         .unwrap_or_else(|| "lsp-proxy-server".to_string());

    //     let check_result = ssh_connection
    //         .execute_command("which", &[&binary_path])
    //         .await;

    //     match check_result {
    //         Ok(result) if result.exit_code == 0 => {
    //             info!("Remote lsp-proxy-server found at: {}", result.stdout.trim());
    //             return Ok(());
    //         }
    //         _ => {
    //             info!("lsp-proxy-server not found, running installation script...");
    //         }
    //     }

    //     // Run installation script
    //     let install_script = r#"
    //         curl -fsSL https://raw.githubusercontent.com/jadestrong/lsp-proxy/main/scripts/server-install.sh | bash
    //     "#;

    //     let install_result = ssh_connection
    //         .execute_command("bash", &["-c", install_script])
    //         .await?;

    //     if install_result.exit_code == 0 {
    //         info!("Server installation completed successfully");
    //         Ok(())
    //     } else {
    //         Err(anyhow!(
    //             "Server installation failed: {}",
    //             install_result.stderr
    //         ))
    //     }
    // }
}

impl Drop for SshPipeConnection {
    fn drop(&mut self) {
        info!("Cleaning up SSH pipe connection to {}", self.host);
        // Kill the SSH process
        if let Ok(mut process_guard) = self.ssh_process.try_lock() {
            let _ = process_guard.kill();
        }
    }
}

/// Unified remote connection wrapper
pub struct RemoteConnection {
    connection_type: ConnectionType,
    tcp_stream: Option<Arc<Mutex<TokioTcpStream>>>,
    ssh_pipe: Option<Arc<Mutex<SshPipeConnection>>>,
    host: String,
    remote_lsp_servers: Arc<RwLock<HashMap<String, String>>>, // workspace_path -> server_id
    _ssh_tunnel: Option<Arc<SshTunnel>>,                      // Keep for backward compatibility
}

#[derive(Debug, Clone)]
enum ConnectionType {
    Tcp,
    SshTunnel,
    SshPipe,
}

impl RemoteConnection {
    pub async fn new(host: &str, config: &RemoteServerConfig) -> Result<Self> {
        let connection_type = match config.mode {
            RemoteMode::Server { .. } => ConnectionType::SshPipe,
            RemoteMode::SSHTunnel { .. } => ConnectionType::SshTunnel,
            RemoteMode::Direct => ConnectionType::Tcp,
            RemoteMode::Auto => ConnectionType::SshPipe, // Default to SSH pipe
        };

        match connection_type {
            ConnectionType::Tcp => {
                info!(
                    "Establishing direct TCP connection to {}:{}",
                    config.host,
                    config.port.unwrap_or(9527)
                );
                let addr = format!("{}:{}", config.host, config.port.unwrap_or(9527));
                let stream = TokioTcpStream::connect(&addr)
                    .await
                    .map_err(|e| anyhow!("Failed to connect to {}: {}", addr, e))?;

                Ok(Self {
                    connection_type,
                    tcp_stream: Some(Arc::new(Mutex::new(stream))),
                    ssh_pipe: None,
                    host: host.to_string(),
                    remote_lsp_servers: Arc::new(RwLock::new(HashMap::new())),
                    _ssh_tunnel: None,
                })
            }

            ConnectionType::SshTunnel => {
                // TODO: Implement SSH tunnel using existing infrastructure
                // For now, fallback to SSH pipe
                warn!("SSH tunnel mode not fully implemented, using SSH pipe");
                let ssh_pipe = Arc::new(Mutex::new(SshPipeConnection::new(config).await?));

                Ok(Self {
                    connection_type: ConnectionType::SshPipe,
                    tcp_stream: None,
                    ssh_pipe: Some(ssh_pipe),
                    host: host.to_string(),
                    remote_lsp_servers: Arc::new(RwLock::new(HashMap::new())),
                    _ssh_tunnel: None,
                })
            }

            ConnectionType::SshPipe => {
                // Use the new integrated SSH pipe connection
                let ssh_pipe = Arc::new(Mutex::new(SshPipeConnection::new(config).await?));

                Ok(Self {
                    connection_type,
                    tcp_stream: None,
                    ssh_pipe: Some(ssh_pipe),
                    host: host.to_string(),
                    remote_lsp_servers: Arc::new(RwLock::new(HashMap::new())),
                    _ssh_tunnel: None,
                })
            }
        }
    }

    pub async fn send_message(&self, message: ProxyMessage) -> Result<Option<ProxyMessage>> {
        match self.connection_type {
            ConnectionType::Tcp | ConnectionType::SshTunnel => self.send_tcp_message(message).await,
            ConnectionType::SshPipe => self.send_pipe_message(message).await,
        }
    }

    async fn send_tcp_message(&self, message: ProxyMessage) -> Result<Option<ProxyMessage>> {
        let msg_json = serde_json::to_string(&message)?;
        debug!("Sending TCP to {}: {}", self.host, msg_json);

        let stream = self
            .tcp_stream
            .as_ref()
            .ok_or_else(|| anyhow!("TCP stream not available"))?;

        let mut stream_guard = stream.lock().await;
        stream_guard.write_all(msg_json.as_bytes()).await?;
        stream_guard.write_all(b"\n").await?;
        stream_guard.flush().await?;

        // 读取响应 (如果期望有响应)
        if matches!(
            message,
            ProxyMessage::Ping
                | ProxyMessage::StartLspServer { .. }
                | ProxyMessage::StopLspServer { .. }
                | ProxyMessage::LspRequest { .. }
                | ProxyMessage::ListServers
        ) {
            let mut response_line = String::new();
            let (reader, _) = stream_guard.split();
            let mut buf_reader = BufReader::new(reader);

            match buf_reader.read_line(&mut response_line).await {
                Ok(_) => {
                    debug!("Received TCP from {}: {}", self.host, response_line.trim());
                    match serde_json::from_str::<ProxyMessage>(response_line.trim()) {
                        Ok(response) => Ok(Some(response)),
                        Err(e) => {
                            warn!(
                                "Failed to parse TCP response from {}: {} - {}",
                                self.host, e, response_line
                            );
                            Ok(None)
                        }
                    }
                }
                Err(e) => {
                    error!("Failed to read TCP response from {}: {}", self.host, e);
                    Err(e.into())
                }
            }
        } else {
            Ok(None)
        }
    }

    async fn send_pipe_message(&self, message: ProxyMessage) -> Result<Option<ProxyMessage>> {
        let ssh_pipe = self
            .ssh_pipe
            .as_ref()
            .ok_or_else(|| anyhow!("SSH pipe not available"))?;

        let pipe_guard = ssh_pipe.lock().await;
        pipe_guard.send_message(message).await
    }

    pub async fn start_lsp_server(&self, workspace_path: &Path, language: &str) -> Result<String> {
        let server_id = format!("remote-{}-{}", language, Uuid::new_v4());
        let workspace_root = workspace_path.to_path_buf();

        let message = ProxyMessage::StartLspServer {
            id: server_id.clone(),
            language: language.to_string(),
            workspace_root,
            client_capabilities: None, // TODO: Get from lsp-mode
        };

        match self.send_message(message).await? {
            Some(ProxyMessage::Response { id: _, result }) => match result {
                Ok(_) => {
                    let mut servers = self.remote_lsp_servers.write().await;
                    servers.insert(
                        workspace_path.to_string_lossy().to_string(),
                        server_id.clone(),
                    );
                    info!(
                        "Started remote LSP server {} for {} on {}",
                        server_id, language, self.host
                    );
                    Ok(server_id)
                }
                Err(e) => Err(anyhow!("Remote server error: {}", e)),
            },
            _ => Err(anyhow!("Unexpected response from remote server")),
        }
    }

    pub async fn forward_lsp_request(
        &self,
        server_id: &str,
        method: &str,
        params: Value,
    ) -> Result<Value> {
        let message = ProxyMessage::LspRequest {
            server_id: server_id.to_string(),
            method: method.to_string(),
            params,
            request_id: None,
        };

        match self.send_message(message).await? {
            Some(ProxyMessage::Response { id: _, result }) => match result {
                Ok(value) => Ok(value),
                Err(e) => Err(anyhow!("LSP request error: {}", e)),
            },
            _ => Err(anyhow!("Unexpected response to LSP request")),
        }
    }

    pub async fn forward_lsp_notification(
        &self,
        server_id: &str,
        method: &str,
        params: Value,
    ) -> Result<()> {
        let message = ProxyMessage::LspNotification {
            server_id: server_id.to_string(),
            method: method.to_string(),
            params,
        };

        self.send_message(message).await?;
        Ok(())
    }
}

/// Remote LSP manager for the local lsp-proxy
pub struct RemoteLspManager {
    config: RemoteLspConfig,
    connections: Arc<RwLock<HashMap<String, Arc<RemoteConnection>>>>,
}

impl RemoteLspManager {
    pub fn new(config: RemoteLspConfig) -> Self {
        Self {
            config,
            connections: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Check if a URI represents a remote file
    pub fn is_remote_file(uri: &str) -> bool {
        // Check for TRAMP-style URIs
        if uri.starts_with("file:///ssh:") || uri.starts_with("file:///tramp:") {
            return true;
        }

        // Check for SSH-style paths
        if let Ok(url) = Url::parse(uri) {
            if url.scheme() == "file" {
                let path = url.path();
                // TRAMP paths like /ssh:host:/path or /method:host:/path
                if path.starts_with("/ssh:")
                    || path.starts_with("/scp:")
                    || path.starts_with("/tramp:")
                    || (path.contains(":") && path.contains("@"))
                {
                    return true;
                }
            }
        }

        false
    }

    /// Extract remote host information from URI
    pub fn extract_remote_info(uri: &str) -> Option<RemoteFileInfo> {
        if let Ok(url) = Url::parse(uri) {
            if url.scheme() == "file" {
                let path = url.path();

                // Handle TRAMP-style paths: /ssh:user@host:/remote/path
                if let Some(tramp_match) = Self::parse_tramp_path(path) {
                    return Some(tramp_match);
                }
            }
        }

        None
    }

    fn parse_tramp_path(path: &str) -> Option<RemoteFileInfo> {
        // Pattern: /method:user@host:/remote/path
        // or: /method:host:/remote/path
        if let Some(colon_pos) = path.find(':') {
            let method_part = &path[1..colon_pos]; // Skip leading /
            let rest = &path[colon_pos + 1..];

            if let Some(second_colon) = rest.find(':') {
                let host_part = &rest[..second_colon];
                let remote_path = &rest[second_colon + 1..];

                let (user, host) = if let Some(at_pos) = host_part.find('@') {
                    let user = &host_part[..at_pos];
                    let host = &host_part[at_pos + 1..];
                    (Some(user.to_string()), host.to_string())
                } else {
                    (None, host_part.to_string())
                };

                return Some(RemoteFileInfo {
                    method: method_part.to_string(),
                    user,
                    host,
                    path: remote_path.to_string(),
                });
            }
        }

        None
    }

    /// Get or create connection to remote host
    pub async fn get_connection(&self, host: &str) -> Result<Arc<RemoteConnection>> {
        let connections = self.connections.read().await;
        if let Some(conn) = connections.get(host) {
            return Ok(Arc::clone(conn));
        }
        drop(connections);

        // Check if we have a mapping for this host
        let config_host = if let Some(host_mapping) = &self.config.host_mapping {
            host_mapping.get(host).unwrap_or(&host.to_string()).clone()
        } else {
            host.to_string()
        };

        // Create new connection
        let host_config = self.config.servers.get(&config_host).ok_or_else(|| {
            anyhow!(
                "No configuration for host: {} (mapped from: {})",
                config_host,
                host
            )
        })?;

        let connection = Arc::new(RemoteConnection::new(&config_host, host_config).await?);

        let mut connections = self.connections.write().await;
        connections.insert(host.to_string(), Arc::clone(&connection));

        info!(
            "Established connection to remote host: {} (config: {})",
            host, config_host
        );
        Ok(connection)
    }

    /// Start remote LSP server for a file
    pub async fn start_remote_lsp(
        &self,
        file_info: &RemoteFileInfo,
        language: &str,
    ) -> Result<String> {
        let connection = self.get_connection(&file_info.host).await?;
        let workspace_path = Path::new(&file_info.path)
            .parent()
            .unwrap_or(Path::new("/"))
            .to_path_buf();

        connection.start_lsp_server(&workspace_path, language).await
    }

    /// Forward LSP request to remote server
    pub async fn forward_request(
        &self,
        host: &str,
        server_id: &str,
        method: &str,
        params: Value,
    ) -> Result<Value> {
        let connection = self.get_connection(host).await?;
        connection
            .forward_lsp_request(server_id, method, params)
            .await
    }

    /// Forward LSP notification to remote server
    pub async fn forward_notification(
        &self,
        host: &str,
        server_id: &str,
        method: &str,
        params: Value,
    ) -> Result<()> {
        let connection = self.get_connection(host).await?;
        connection
            .forward_lsp_notification(server_id, method, params)
            .await
    }
}

#[derive(Debug, Clone)]
pub struct RemoteFileInfo {
    pub method: String, // ssh, scp, tramp, etc.
    pub user: Option<String>,
    pub host: String,
    pub path: String, // Remote file path
}
