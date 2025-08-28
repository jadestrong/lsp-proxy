//! Connection handling for lsp-proxy-server

use crate::remote::protocol::{ClientMessage, ServerMessage, ProtocolError, ServerCapabilities, ServerInfo, PROTOCOL_VERSION};
use anyhow::Result;
use log::{debug, error, info};
use std::path::PathBuf;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::TcpStream;

/// Server connection handler
pub struct ServerConnection {
    stream: TcpStream,
    workspace_root: PathBuf,
}

impl ServerConnection {
    /// Create a new server connection
    pub fn new(stream: TcpStream, workspace_root: PathBuf) -> Self {
        Self {
            stream,
            workspace_root,
        }
    }

    /// Handle the connection
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
                                ClientMessage::Handshake { version, capabilities } => {
                                    authenticated = true;
                                    ServerMessage::HandshakeResponse {
                                        version: PROTOCOL_VERSION.to_string(),
                                        capabilities: ServerCapabilities {
                                            language_servers: vec![],
                                            file_operations: vec!["read".to_string(), "write".to_string()],
                                            max_file_size: 100 * 1024 * 1024,
                                            compression: capabilities.compression,
                                            streaming: capabilities.streaming,
                                        },
                                        server_info: ServerInfo {
                                            hostname: "localhost".to_string(),
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
                                _ => {
                                    if !authenticated {
                                        ServerMessage::Error {
                                            id: None,
                                            error: ProtocolError::server_error(401, "Authentication required"),
                                        }
                                    } else {
                                        // Handle other messages
                                        ServerMessage::Error {
                                            id: None,
                                            error: ProtocolError::server_error(501, "Not implemented"),
                                        }
                                    }
                                }
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