//! LSP-Proxy Server - Standalone remote server component

use anyhow::Result;
use clap::{Arg, Command};
use log::{debug, error, info, warn};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::{TcpListener, TcpStream};
use tokio::signal;

/// Protocol version
const PROTOCOL_VERSION: &str = "1.0";

/// Client message types
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "data")]
pub enum ClientMessage {
    Handshake { version: String, capabilities: ClientCapabilities },
    Ping { id: u64 },
    Shutdown,
}

/// Server message types
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "data")]
pub enum ServerMessage {
    HandshakeResponse {
        version: String,
        capabilities: ServerCapabilities,
        server_info: ServerInfo,
    },
    Pong { id: u64 },
    Error { id: Option<u64>, error: ProtocolError },
}

/// Client capabilities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClientCapabilities {
    pub file_operations: Vec<String>,
    pub lsp_features: Vec<String>,
    pub compression: bool,
    pub streaming: bool,
}

/// Server capabilities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerCapabilities {
    pub language_servers: Vec<String>,
    pub file_operations: Vec<String>,
    pub max_file_size: u64,
    pub compression: bool,
    pub streaming: bool,
}

/// Server information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerInfo {
    pub hostname: String,
    pub platform: String,
    pub arch: String,
    pub working_directory: PathBuf,
    pub available_memory: u64,
    pub cpu_count: u32,
}

/// Protocol error
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProtocolError {
    pub code: i32,
    pub message: String,
}

impl ProtocolError {
    pub fn server_error(code: i32, message: &str) -> Self {
        Self {
            code,
            message: message.to_string(),
        }
    }

    pub fn invalid_request(message: &str) -> Self {
        Self {
            code: -32600,
            message: message.to_string(),
        }
    }
}

/// Server connection handler
pub struct ServerConnection {
    stream: TcpStream,
    workspace_root: PathBuf,
}

impl ServerConnection {
    pub fn new(stream: TcpStream, workspace_root: PathBuf) -> Self {
        Self {
            stream,
            workspace_root,
        }
    }

    pub async fn handle(self) -> Result<()> {
        let peer_addr = match self.stream.peer_addr() {
            Ok(addr) => addr,
            Err(e) => {
                error!("Failed to get peer address: {}", e);
                return Err(e.into());
            }
        };
        info!("Handling connection from {}", peer_addr);

        let (reader, mut writer) = self.stream.into_split();
        let mut buf_reader = BufReader::new(reader);
        let mut line = String::new();
        let mut authenticated = false;

        loop {
            line.clear();
            
            match buf_reader.read_line(&mut line).await {
                Ok(0) => {
                    info!("Client {} disconnected", peer_addr);
                    break;
                },
                Ok(_) => {
                    let line_content = line.trim();
                    if line_content.is_empty() {
                        continue;
                    }

                    debug!("Received: {}", line_content);

                    match serde_json::from_str::<ClientMessage>(line_content) {
                        Ok(message) => {
                            let response = match message {
                                ClientMessage::Handshake { version: _, capabilities } => {
                                    authenticated = true;
                                    ServerMessage::HandshakeResponse {
                                        version: PROTOCOL_VERSION.to_string(),
                                        capabilities: ServerCapabilities {
                                            language_servers: vec![],
                                            file_operations: capabilities.file_operations.clone(),
                                            max_file_size: 100 * 1024 * 1024,
                                            compression: capabilities.compression,
                                            streaming: capabilities.streaming,
                                        },
                                        server_info: ServerInfo {
                                            hostname: gethostname::gethostname()
                                                .to_string_lossy()
                                                .to_string(),
                                            platform: std::env::consts::OS.to_string(),
                                            arch: std::env::consts::ARCH.to_string(),
                                            working_directory: self.workspace_root.clone(),
                                            available_memory: 8 * 1024 * 1024 * 1024, // 8GB default
                                            cpu_count: num_cpus::get() as u32,
                                        },
                                    }
                                },
                                ClientMessage::Ping { id } => ServerMessage::Pong { id },
                                ClientMessage::Shutdown => {
                                    info!("Shutdown requested");
                                    break;
                                },
                            };

                            let response_json = serde_json::to_string(&response)?;
                            debug!("Sending: {}", response_json);
                            
                            writer.write_all(response_json.as_bytes()).await?;
                            writer.write_all(b"\n").await?;
                            writer.flush().await?;
                        },
                        Err(e) => {
                            error!("Failed to parse message: {}", e);
                            let error_response = ServerMessage::Error {
                                id: None,
                                error: ProtocolError::invalid_request(&format!("Invalid JSON: {}", e)),
                            };
                            let error_json = serde_json::to_string(&error_response)?;
                            writer.write_all(error_json.as_bytes()).await?;
                            writer.write_all(b"\n").await?;
                            writer.flush().await?;
                        }
                    }
                },
                Err(e) => {
                    error!("Error reading from client {}: {}", peer_addr, e);
                    break;
                }
            }
        }

        info!("Connection from {} ended", peer_addr);
        Ok(())
    }
}

/// Main server instance
pub struct LspProxyServer {
    workspace_root: PathBuf,
}

impl LspProxyServer {
    pub async fn new(workspace_root: PathBuf) -> Result<Self> {
        Ok(Self {
            workspace_root,
        })
    }

    pub async fn run(&self, host: &str, port: u16) -> Result<()> {
        let bind_address = format!("{}:{}", host, port);
        let listener = TcpListener::bind(&bind_address).await?;
        
        info!("LSP-Proxy server listening on {}", bind_address);
        info!("Workspace root: {:?}", self.workspace_root);

        // Setup signal handling
        let (shutdown_tx, mut shutdown_rx) = tokio::sync::oneshot::channel::<()>();
        let shutdown_tx = std::sync::Arc::new(tokio::sync::Mutex::new(Some(shutdown_tx)));

        // Spawn signal handler
        let shutdown_tx_clone = shutdown_tx.clone();
        tokio::spawn(async move {
            match signal::ctrl_c().await {
                Ok(()) => {
                    info!("Received Ctrl+C signal, shutting down...");
                    if let Some(tx) = shutdown_tx_clone.lock().await.take() {
                        let _ = tx.send(());
                    }
                },
                Err(err) => {
                    error!("Unable to listen for shutdown signal: {}", err);
                }
            }
        });

        // Accept connections
        loop {
            tokio::select! {
                result = listener.accept() => {
                    match result {
                        Ok((stream, addr)) => {
                            info!("New connection from: {}", addr);
                            
                            let connection = ServerConnection::new(
                                stream,
                                self.workspace_root.clone(),
                            );

                            // Handle connection in background
                            tokio::spawn(async move {
                                if let Err(e) = connection.handle().await {
                                    error!("Connection error: {}", e);
                                }
                            });
                        },
                        Err(e) => {
                            error!("Failed to accept connection: {}", e);
                        }
                    }
                },
                _ = &mut shutdown_rx => {
                    info!("Shutdown signal received, stopping server...");
                    break;
                }
            }
        }

        info!("Shutting down LSP-Proxy server");
        Ok(())
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let matches = Command::new("lsp-proxy-server")
        .version(env!("CARGO_PKG_VERSION"))
        .about("LSP-Proxy remote server component")
        .arg(
            Arg::new("port")
                .short('p')
                .long("port")
                .value_name("PORT")
                .help("TCP port to listen on")
                .default_value("7878"),
        )
        .arg(
            Arg::new("host")
                .long("host") 
                .value_name("HOST")
                .help("Host address to bind to")
                .default_value("127.0.0.1"),
        )
        .arg(
            Arg::new("log-level")
                .short('l')
                .long("log-level")
                .value_name("LEVEL")
                .help("Log level (0-3: Error, Warn, Info, Debug)")
                .default_value("1"),
        )
        .arg(
            Arg::new("workspace")
                .short('w')
                .long("workspace")
                .value_name("PATH")
                .help("Default workspace root directory")
                .default_value("."),
        )
        .get_matches();

    // Initialize logging
    let log_level = matches.get_one::<String>("log-level").unwrap().parse::<u8>()?;
    init_logging(log_level)?;

    info!("Starting lsp-proxy-server v{}", env!("CARGO_PKG_VERSION"));

    // Get configuration from command line arguments
    let host = matches.get_one::<String>("host").unwrap();
    let port: u16 = matches.get_one::<String>("port").unwrap().parse()?;
    let workspace_root: PathBuf = matches.get_one::<String>("workspace").unwrap().into();

    // Create server instance
    let server = LspProxyServer::new(workspace_root).await?;
    
    // Start server
    server.run(host, port).await
}

fn init_logging(level: u8) -> Result<()> {
    let log_level = match level {
        0 => log::LevelFilter::Error,
        1 => log::LevelFilter::Warn,
        2 => log::LevelFilter::Info,
        3 => log::LevelFilter::Debug,
        _ => log::LevelFilter::Trace,
    };

    env_logger::Builder::from_default_env()
        .filter_level(log_level)
        .format_timestamp_millis()
        .init();

    Ok(())
}