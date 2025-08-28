//! SSH Tunnel-based Server Mode Connection
//!
//! This implementation uses SSH port forwarding to create a secure tunnel
//! to the remote lsp-proxy-server, combining the security of SSH with the
//! performance benefits of direct TCP communication.

use async_trait::async_trait;
use anyhow::{anyhow, Result};
use log::{debug, error, info, warn};
use std::net::SocketAddr;
use std::sync::Arc;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::{Mutex, RwLock};
use tokio::time::{timeout, Duration};

use crate::remote::protocol::{ClientMessage, ServerMessage, ProtocolError, ClientCapabilities, PROTOCOL_VERSION};
use crate::remote::{RemoteServerConfig, RemoteMode};
use super::{Connection, CommandResult, FileTransfer};
use super::ssh::SSHConnection;

/// SSH Tunnel-based connection to lsp-proxy-server
pub struct SSHTunnelConnection {
    config: RemoteServerConfig,
    ssh_connection: Arc<SSHConnection>,
    local_port: Arc<RwLock<Option<u16>>>,
    tunnel_active: Arc<RwLock<bool>>,
    server_stream: Arc<Mutex<Option<TcpStream>>>,
}

impl SSHTunnelConnection {
    /// Create a new SSH tunnel connection
    pub async fn new(config: RemoteServerConfig) -> Result<Self> {
        let ssh_connection = Arc::new(SSHConnection::new(config.clone()).await?);
        
        Ok(Self {
            config,
            ssh_connection,
            local_port: Arc::new(RwLock::new(None)),
            tunnel_active: Arc::new(RwLock::new(false)),
            server_stream: Arc::new(Mutex::new(None)),
        })
    }

    /// Setup SSH tunnel to the remote server
    async fn setup_tunnel(&self) -> Result<u16> {
        let remote_port = if let RemoteMode::Server { server_path: _, auto_deploy: _ } = &self.config.mode {
            self.config.port.unwrap_or(7878)
        } else {
            return Err(anyhow!("SSH tunnel mode requires server configuration"));
        };

        // Find an available local port
        let local_port = self.find_available_port().await?;
        
        info!("Setting up SSH tunnel: localhost:{} -> {}:{}", 
              local_port, self.config.host, remote_port);

        // Create SSH tunnel using port forwarding
        self.ssh_connection.create_tunnel(local_port, remote_port).await?;
        
        // Update local port and tunnel status
        *self.local_port.write().await = Some(local_port);
        *self.tunnel_active.write().await = true;
        
        info!("SSH tunnel established successfully on local port {}", local_port);
        Ok(local_port)
    }

    /// Find an available local port for tunneling
    async fn find_available_port(&self) -> Result<u16> {
        for port in 8000..9000 {
            match TcpListener::bind(format!("127.0.0.1:{}", port)).await {
                Ok(_) => return Ok(port),
                Err(_) => continue,
            }
        }
        Err(anyhow!("No available ports found for SSH tunnel"))
    }

    /// Connect to the lsp-proxy-server through SSH tunnel
    async fn connect_to_server(&self) -> Result<TcpStream> {
        let local_port = self.local_port.read().await
            .ok_or_else(|| anyhow!("SSH tunnel not established"))?;

        let server_address = format!("127.0.0.1:{}", local_port);
        info!("Connecting to lsp-proxy-server through SSH tunnel at {}", server_address);

        let stream = timeout(Duration::from_secs(10), 
            TcpStream::connect(&server_address)
        ).await??;

        // For simplicity, we'll just return the stream
        // The actual handshake can be done by the caller
        info!("Connected to lsp-proxy-server through SSH tunnel");
        Ok(stream)
    }

    /// Perform protocol handshake with the server
    async fn perform_handshake(&self, stream: TcpStream) -> Result<TcpStream> {
        let (mut reader, mut writer) = stream.into_split();
        
        // Send handshake message
        let handshake = ClientMessage::Handshake {
            version: PROTOCOL_VERSION.to_string(),
            capabilities: ClientCapabilities {
                file_operations: vec!["read".to_string(), "write".to_string(), "list".to_string()],
                lsp_features: vec!["completion".to_string(), "hover".to_string(), "definition".to_string()],
                compression: false,
                streaming: true,
            },
        };

        let handshake_json = serde_json::to_string(&handshake)?;
        debug!("Sending handshake: {}", handshake_json);
        
        writer.write_all(handshake_json.as_bytes()).await?;
        writer.write_all(b"\n").await?;
        writer.flush().await?;

        // Read handshake response
        let mut buf_reader = BufReader::new(reader);
        let mut response_line = String::new();
        buf_reader.read_line(&mut response_line).await?;

        let response: ServerMessage = serde_json::from_str(response_line.trim())?;
        match response {
            ServerMessage::HandshakeResponse { version, capabilities: _, server_info } => {
                if version != PROTOCOL_VERSION {
                    warn!("Protocol version mismatch: client={}, server={}", 
                          PROTOCOL_VERSION, version);
                }
                info!("Connected to lsp-proxy-server on {} ({})", 
                      server_info.hostname, server_info.platform);
                
                // Recreate the stream from the split parts - this is tricky
                // For simplicity, let's create a new connection since handshake is done
                let local_port = self.local_port.read().await
                    .ok_or_else(|| anyhow!("SSH tunnel not established"))?;
                let server_address = format!("127.0.0.1:{}", local_port);
                let new_stream = TcpStream::connect(&server_address).await?;
                Ok(new_stream)
            },
            ServerMessage::Error { error, .. } => {
                Err(anyhow!("Handshake failed: {} ({})", error.message, error.code))
            },
            _ => Err(anyhow!("Unexpected handshake response: {:?}", response))
        }
    }

    /// Deploy the server if auto-deploy is enabled
    async fn deploy_server_if_needed(&self) -> Result<()> {
        if let RemoteMode::SSHTunnel { auto_deploy: true, .. } = &self.config.mode {
            info!("Auto-deploy enabled, deploying lsp-proxy-server");
            
            // Check if server is already running
            if self.check_server_running().await.is_ok() {
                info!("lsp-proxy-server is already running");
                return Ok(());
            }

            // Deploy the server binary
            self.deploy_server_binary().await?;
            
            // Start the server
            self.start_remote_server().await?;
            
            // Wait for server to be ready
            self.wait_for_server_ready().await?;
        }
        Ok(())
    }

    /// Check if the remote server is running
    async fn check_server_running(&self) -> Result<()> {
        let remote_port = self.config.port.unwrap_or(7878);
        let result = self.ssh_connection.execute_command(
            "netstat",
            &["-ln", &format!("| grep :{}", remote_port)]
        ).await?;
        
        if result.exit_code == 0 && !result.stdout.trim().is_empty() {
            Ok(())
        } else {
            Err(anyhow!("Server not running on port {}", remote_port))
        }
    }

    /// Deploy the server binary to remote host
    async fn deploy_server_binary(&self) -> Result<()> {
        info!("Building and deploying lsp-proxy-server binary");
        
        // Build the server binary locally
        let local_binary = self.build_server_binary().await?;
        
        // Upload to remote server
        let remote_path = "/tmp/lsp-proxy-server";
        let file_transfer = self.ssh_connection.file_transfer().await?;
        file_transfer.upload(&local_binary, remote_path).await?;
        
        // Make executable
        self.ssh_connection.execute_command("chmod", &["+x", remote_path]).await?;
        
        info!("Server binary deployed to {}", remote_path);
        Ok(())
    }

    /// Build the server binary locally
    async fn build_server_binary(&self) -> Result<String> {
        use std::process::Command;
        
        info!("Building lsp-proxy-server binary");
        
        let output = Command::new("cargo")
            .args(&["build", "--release", "--bin", "lsp-proxy-server"])
            .output()?;
            
        if !output.status.success() {
            return Err(anyhow!("Failed to build server binary: {}", 
                              String::from_utf8_lossy(&output.stderr)));
        }
        
        Ok("./target/release/lsp-proxy-server".to_string())
    }

    /// Start the remote server
    async fn start_remote_server(&self) -> Result<()> {
        let remote_port = self.config.port.unwrap_or(7878);
        let workspace_root = self.config.workspace_root
            .as_ref()
            .and_then(|p| p.to_str())
            .unwrap_or(".");
            
        info!("Starting lsp-proxy-server on remote host");
        
        // Start server in background
        let _result = self.ssh_connection.execute_command(
            "nohup",
            &[
                "/tmp/lsp-proxy-server",
                "--host", "127.0.0.1",
                "--port", &remote_port.to_string(),
                "--workspace", workspace_root,
                "--log-level", "2",
                ">", "/tmp/lsp-proxy-server.log",
                "2>&1", "&"
            ]
        ).await?;
        
        Ok(())
    }

    /// Wait for the server to be ready
    async fn wait_for_server_ready(&self) -> Result<()> {
        info!("Waiting for lsp-proxy-server to be ready");
        
        for _ in 0..30 { // Wait up to 30 seconds
            if self.check_server_running().await.is_ok() {
                info!("lsp-proxy-server is ready");
                return Ok(());
            }
            tokio::time::sleep(Duration::from_secs(1)).await;
        }
        
        Err(anyhow!("Timeout waiting for server to start"))
    }

    /// Get the active tunnel port
    pub async fn get_tunnel_port(&self) -> Option<u16> {
        *self.local_port.read().await
    }

    /// Check if tunnel is active
    pub async fn is_tunnel_active(&self) -> bool {
        *self.tunnel_active.read().await
    }
}

#[async_trait]
impl Connection for SSHTunnelConnection {
    async fn connect(&self) -> Result<()> {
        info!("Establishing SSH tunnel connection to {}", self.config.host);
        
        // First establish SSH connection
        self.ssh_connection.connect().await?;
        
        // Deploy server if needed
        self.deploy_server_if_needed().await?;
        
        // Setup SSH tunnel
        self.setup_tunnel().await?;
        
        // Connect to server through tunnel
        let stream = self.connect_to_server().await?;
        *self.server_stream.lock().await = Some(stream);
        
        info!("SSH tunnel connection established successfully");
        Ok(())
    }

    async fn disconnect(&self) -> Result<()> {
        info!("Disconnecting SSH tunnel connection");
        
        // Close server stream
        if let Some(stream) = self.server_stream.lock().await.take() {
            drop(stream);
        }
        
        // Mark tunnel as inactive
        *self.tunnel_active.write().await = false;
        *self.local_port.write().await = None;
        
        // Disconnect SSH connection
        self.ssh_connection.disconnect().await?;
        
        info!("SSH tunnel connection disconnected");
        Ok(())
    }

    fn is_connected(&self) -> bool {
        self.ssh_connection.is_connected()
    }

    async fn execute_command(&self, command: &str, args: &[&str]) -> Result<CommandResult> {
        self.ssh_connection.execute_command(command, args).await
    }

    async fn create_tunnel(&self, local_port: u16, remote_port: u16) -> Result<()> {
        self.ssh_connection.create_tunnel(local_port, remote_port).await
    }

    async fn file_transfer(&self) -> Result<Box<dyn FileTransfer>> {
        self.ssh_connection.file_transfer().await
    }
}

/// Architecture comparison between different modes
#[allow(dead_code)]
mod architecture_comparison {
    //! # LSP-Proxy Remote Development Architecture Comparison
    //!
    //! ## 1. Direct SSH Mode (当前的 Direct Mode)
    //! ```
    //! [Emacs] ↔ [lsp-proxy客户端] ↔ SSH连接 ↔ [远程LSP服务器]
    //! ```
    //! - 优点: 零部署，直接使用SSH
    //! - 缺点: 每次LSP请求都需要SSH命令执行，性能较低
    //!
    //! ## 2. Direct TCP Server Mode (当前实现的方案)
    //! ```
    //! [Emacs] ↔ [lsp-proxy客户端] ↔ 直接TCP ↔ [lsp-proxy-server] ↔ [远程LSP服务器]
    //! ```
    //! - 优点: 高性能，持久连接
    //! - 缺点: 需要开放端口，安全性依赖网络配置
    //!
    //! ## 3. SSH Tunnel Server Mode (本文件实现的方案)
    //! ```
    //! [Emacs] ↔ [lsp-proxy客户端] ↔ 本地TCP ↔ SSH隧道 ↔ [lsp-proxy-server] ↔ [远程LSP服务器]
    //! ```
    //! - 优点: 高性能 + SSH安全性，无需开放远程端口
    //! - 缺点: 略微复杂的连接建立过程
    //!
    //! ## 4. WebSocket over SSH Mode (可扩展方案)
    //! ```
    //! [Emacs] ↔ [lsp-proxy客户端] ↔ WebSocket ↔ SSH隧道 ↔ [lsp-proxy-server] ↔ [远程LSP服务器]
    //! ```
    //! - 优点: 支持代理服务器，更好的防火墙兼容性
    //! - 缺点: 额外的WebSocket协议开销
}