//! Server Mode connection implementation

use crate::remote::{protocol::*, RemoteServerConfig};
use anyhow::Result;
use async_trait::async_trait;
use log::{debug, error, info, warn};
use std::sync::Arc;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::TcpStream;
use tokio::sync::{mpsc, oneshot, Mutex};
use std::collections::HashMap;

use super::{Connection, CommandResult, FileTransfer};

/// Server mode connection that communicates with lsp-proxy-server
pub struct ServerModeConnection {
    config: RemoteServerConfig,
    stream: Arc<Mutex<Option<TcpStream>>>,
    request_sender: Arc<Mutex<Option<mpsc::UnboundedSender<ServerRequest>>>>,
    response_handlers: Arc<Mutex<HashMap<u64, oneshot::Sender<ServerMessage>>>>,
    connected: Arc<Mutex<bool>>,
    request_id_counter: Arc<Mutex<u64>>,
}

struct ServerRequest {
    id: u64,
    message: ClientMessage,
    response_sender: oneshot::Sender<ServerMessage>,
}

impl ServerModeConnection {
    /// Create a new server mode connection
    pub async fn new(config: RemoteServerConfig) -> Result<Self> {
        let connection = Self {
            config,
            stream: Arc::new(Mutex::new(None)),
            request_sender: Arc::new(Mutex::new(None)),
            response_handlers: Arc::new(Mutex::new(HashMap::new())),
            connected: Arc::new(Mutex::new(false)),
            request_id_counter: Arc::new(Mutex::new(0)),
        };

        Ok(connection)
    }

    /// Get next request ID
    async fn next_request_id(&self) -> u64 {
        let mut counter = self.request_id_counter.lock().await;
        *counter += 1;
        *counter
    }

    /// Send a request and wait for response
    async fn send_request(&self, message: ClientMessage) -> Result<ServerMessage> {
        let (response_sender, response_receiver) = oneshot::channel();
        let id = self.next_request_id().await;

        let request = ServerRequest {
            id,
            message,
            response_sender,
        };

        // Send request
        if let Some(sender) = self.request_sender.lock().await.as_ref() {
            sender.send(request).map_err(|_| anyhow::anyhow!("Failed to send request"))?;
        } else {
            return Err(anyhow::anyhow!("Connection not established"));
        }

        // Wait for response
        let response = response_receiver.await
            .map_err(|_| anyhow::anyhow!("Failed to receive response"))?;

        Ok(response)
    }

    /// Start the message handling loop
    async fn start_message_loop(&self, stream: TcpStream) -> Result<()> {
        let (request_sender, mut request_receiver) = mpsc::unbounded_channel::<ServerRequest>();
        *self.request_sender.lock().await = Some(request_sender);

        let (reader, mut writer) = stream.into_split();
        let mut buf_reader = BufReader::new(reader);

        let response_handlers = self.response_handlers.clone();

        // Spawn response handler
        let response_handlers_clone = response_handlers.clone();
        let response_task = tokio::spawn(async move {
            let mut line = String::new();
            
            loop {
                line.clear();
                match buf_reader.read_line(&mut line).await {
                    Ok(0) => {
                        info!("Server disconnected");
                        break;
                    },
                    Ok(_) => {
                        let line = line.trim();
                        if line.is_empty() {
                            continue;
                        }

                        debug!("Received from server: {}", line);

                        match serde_json::from_str::<ServerMessage>(line) {
                            Ok(message) => {
                                match &message {
                                    ServerMessage::FileOperationResponse(resp) => {
                                        let id = Self::extract_response_id_file(resp);
                                        Self::handle_response(response_handlers_clone.clone(), id, message).await;
                                    },
                                    ServerMessage::LspOperationResponse(resp) => {
                                        let id = Self::extract_response_id_lsp(resp);
                                        Self::handle_response(response_handlers_clone.clone(), id, message).await;
                                    },
                                    ServerMessage::WorkspaceOperationResponse(resp) => {
                                        let id = Self::extract_response_id_workspace(resp);
                                        Self::handle_response(response_handlers_clone.clone(), id, message).await;
                                    },
                                    ServerMessage::Pong { id } => {
                                        Self::handle_response(response_handlers_clone.clone(), *id, message).await;
                                    },
                                    ServerMessage::HandshakeResponse { .. } => {
                                        // Handle handshake response
                                        info!("Received handshake response");
                                    },
                                    ServerMessage::Error { id, error } => {
                                        error!("Server error: {:?}", error);
                                        if let Some(id) = id {
                                            Self::handle_response(response_handlers_clone.clone(), *id, message).await;
                                        }
                                    },
                                    ServerMessage::Notification(notification) => {
                                        info!("Server notification: {:?}", notification);
                                    },
                                }
                            },
                            Err(e) => {
                                error!("Failed to parse server message: {}", e);
                            }
                        }
                    },
                    Err(e) => {
                        error!("Error reading from server: {}", e);
                        break;
                    }
                }
            }
        });

        // Spawn request handler
        let request_task = tokio::spawn(async move {
            while let Some(request) = request_receiver.recv().await {
                let message_json = match serde_json::to_string(&request.message) {
                    Ok(json) => json,
                    Err(e) => {
                        error!("Failed to serialize request: {}", e);
                        continue;
                    }
                };

                debug!("Sending to server: {}", message_json);

                if let Err(e) = writer.write_all(message_json.as_bytes()).await {
                    error!("Failed to write to server: {}", e);
                    break;
                }
                
                if let Err(e) = writer.write_all(b"\n").await {
                    error!("Failed to write newline to server: {}", e);
                    break;
                }
                
                if let Err(e) = writer.flush().await {
                    error!("Failed to flush writer: {}", e);
                    break;
                }

                // Store response handler for non-fire-and-forget requests
                match &request.message {
                    ClientMessage::FileOperation(_) | 
                    ClientMessage::LspOperation(_) |
                    ClientMessage::WorkspaceOperation(_) |
                    ClientMessage::Ping { .. } => {
                        response_handlers.lock().await.insert(request.id, request.response_sender);
                    },
                    _ => {
                        // For other messages, send immediate success response
                        let _ = request.response_sender.send(ServerMessage::Pong { id: request.id });
                    }
                }
            }
        });

        // Wait for either task to complete
        tokio::select! {
            _ = response_task => info!("Response task completed"),
            _ = request_task => info!("Request task completed"),
        }

        *self.connected.lock().await = false;
        Ok(())
    }

    async fn handle_response(
        response_handlers: Arc<Mutex<HashMap<u64, oneshot::Sender<ServerMessage>>>>,
        id: u64,
        message: ServerMessage,
    ) {
        if let Some(sender) = response_handlers.lock().await.remove(&id) {
            if sender.send(message).is_err() {
                error!("Failed to send response to handler for request {}", id);
            }
        } else {
            warn!("No handler found for response with id {}", id);
        }
    }

    fn extract_response_id_file(resp: &FileOperationResponse) -> u64 {
        match resp {
            FileOperationResponse::Read { id, .. } => *id,
            FileOperationResponse::Write { id, .. } => *id,
            FileOperationResponse::List { id, .. } => *id,
            FileOperationResponse::Exists { id, .. } => *id,
            FileOperationResponse::Delete { id, .. } => *id,
            FileOperationResponse::Move { id, .. } => *id,
            FileOperationResponse::Copy { id, .. } => *id,
            FileOperationResponse::CreateDir { id, .. } => *id,
            FileOperationResponse::GetMetadata { id, .. } => *id,
            FileOperationResponse::Watch { id, .. } => *id,
            FileOperationResponse::Unwatch { id, .. } => *id,
        }
    }

    fn extract_response_id_lsp(resp: &LspOperationResponse) -> u64 {
        match resp {
            LspOperationResponse::StartServer { id, .. } => *id,
            LspOperationResponse::StopServer { id, .. } => *id,
            LspOperationResponse::SendRequest { id, .. } => *id,
            LspOperationResponse::SendNotification { id, .. } => *id,
            LspOperationResponse::ListServers { id, .. } => *id,
            LspOperationResponse::GetServerStatus { id, .. } => *id,
        }
    }

    fn extract_response_id_workspace(resp: &WorkspaceOperationResponse) -> u64 {
        match resp {
            WorkspaceOperationResponse::GetInfo { id, .. } => *id,
            WorkspaceOperationResponse::Search { id, .. } => *id,
            WorkspaceOperationResponse::GetEnvironment { id, .. } => *id,
            WorkspaceOperationResponse::ExecuteCommand { id, .. } => *id,
        }
    }
}

#[async_trait]
impl Connection for ServerModeConnection {
    async fn connect(&self) -> Result<()> {
        info!("Connecting to server mode at {}:{}", 
              self.config.host, 
              self.config.port.unwrap_or(7878));

        // Connect to the remote server
        let address = format!("{}:{}", self.config.host, self.config.port.unwrap_or(7878));
        let stream = TcpStream::connect(&address).await
            .map_err(|e| anyhow::anyhow!("Failed to connect to server: {}", e))?;

        info!("Connected to server at {}", address);

        // Store stream reference
        let stream_copy = TcpStream::connect(&address).await
            .map_err(|e| anyhow::anyhow!("Failed to create stream copy: {}", e))?;
        *self.connected.lock().await = true;

        // Start message loop
        let connection_clone = Self {
            config: self.config.clone(),
            stream: self.stream.clone(),
            request_sender: self.request_sender.clone(),
            response_handlers: self.response_handlers.clone(),
            connected: self.connected.clone(),
            request_id_counter: self.request_id_counter.clone(),
        };

        tokio::spawn(async move {
            if let Err(e) = connection_clone.start_message_loop(stream).await {
                error!("Message loop error: {}", e);
            }
        });

        // Perform handshake
        let capabilities = ClientCapabilities {
            file_operations: vec![
                "read".to_string(), "write".to_string(), "list".to_string(),
                "exists".to_string(), "delete".to_string(), "move".to_string(),
                "copy".to_string(), "create_dir".to_string(), "get_metadata".to_string(),
                "watch".to_string(),
            ],
            lsp_features: vec![
                "completion".to_string(), "hover".to_string(), "definition".to_string(),
                "references".to_string(), "formatting".to_string(),
            ],
            compression: true,
            streaming: true,
        };

        let handshake = ClientMessage::Handshake {
            version: PROTOCOL_VERSION.to_string(),
            capabilities,
        };

        match self.send_request(handshake).await? {
            ServerMessage::HandshakeResponse { version, capabilities: server_caps, server_info } => {
                info!("Handshake successful with server version: {}", version);
                info!("Server info: {:?}", server_info);
                info!("Server capabilities: {} language servers, {} file operations",
                      server_caps.language_servers.len(),
                      server_caps.file_operations.len());
            },
            ServerMessage::Error { error, .. } => {
                return Err(anyhow::anyhow!("Handshake failed: {:?}", error));
            },
            _ => {
                return Err(anyhow::anyhow!("Unexpected handshake response"));
            }
        }

        Ok(())
    }

    async fn disconnect(&self) -> Result<()> {
        info!("Disconnecting from server mode");

        // Send shutdown message
        if self.is_connected() {
            let shutdown = ClientMessage::Shutdown;
            let _ = self.send_request(shutdown).await; // Ignore errors during shutdown
        }

        *self.connected.lock().await = false;
        *self.stream.lock().await = None;
        *self.request_sender.lock().await = None;

        Ok(())
    }

    fn is_connected(&self) -> bool {
        // Use try_lock to avoid blocking
        self.connected.try_lock()
            .map(|guard| *guard)
            .unwrap_or(false)
    }

    async fn execute_command(&self, command: &str, args: &[&str]) -> Result<CommandResult> {
        let id = self.next_request_id().await;
        
        let request = ClientMessage::WorkspaceOperation(
            WorkspaceOperationRequest::ExecuteCommand {
                id,
                command: command.to_string(),
                args: args.iter().map(|s| s.to_string()).collect(),
                working_dir: None,
                env: std::env::vars().collect(),
            }
        );

        match self.send_request(request).await? {
            ServerMessage::WorkspaceOperationResponse(
                WorkspaceOperationResponse::ExecuteCommand { exit_code, stdout, stderr, .. }
            ) => {
                Ok(CommandResult {
                    exit_code,
                    stdout,
                    stderr,
                })
            },
            ServerMessage::Error { error, .. } => {
                Err(anyhow::anyhow!("Command execution failed: {:?}", error))
            },
            _ => {
                Err(anyhow::anyhow!("Unexpected response for command execution"))
            }
        }
    }

    async fn create_tunnel(&self, local_port: u16, remote_port: u16) -> Result<()> {
        // Server mode doesn't need tunneling as it uses direct TCP connection
        warn!("Tunneling not needed in server mode (local: {}, remote: {})", local_port, remote_port);
        Ok(())
    }

    async fn file_transfer(&self) -> Result<Box<dyn FileTransfer>> {
        Ok(Box::new(ServerModeFileTransfer {
            connection: self.clone(),
        }))
    }
}

// Clone implementation for ServerModeConnection
impl Clone for ServerModeConnection {
    fn clone(&self) -> Self {
        Self {
            config: self.config.clone(),
            stream: self.stream.clone(),
            request_sender: self.request_sender.clone(),
            response_handlers: self.response_handlers.clone(),
            connected: self.connected.clone(),
            request_id_counter: self.request_id_counter.clone(),
        }
    }
}

/// File transfer implementation for server mode
pub struct ServerModeFileTransfer {
    connection: ServerModeConnection,
}

#[async_trait]
impl FileTransfer for ServerModeFileTransfer {
    async fn upload(&self, _local_path: &str, _remote_path: &str) -> Result<()> {
        // In server mode, files are written directly
        Err(anyhow::anyhow!("Use write_file instead of upload in server mode"))
    }

    async fn download(&self, _remote_path: &str, _local_path: &str) -> Result<()> {
        // In server mode, files are read directly
        Err(anyhow::anyhow!("Use read_file instead of download in server mode"))
    }

    async fn read_file(&self, remote_path: &str) -> Result<Vec<u8>> {
        let id = self.connection.next_request_id().await;
        
        let request = ClientMessage::FileOperation(
            FileOperationRequest::Read {
                id,
                path: remote_path.into(),
                offset: None,
                length: None,
            }
        );

        match self.connection.send_request(request).await? {
            ServerMessage::FileOperationResponse(
                FileOperationResponse::Read { content, .. }
            ) => Ok(content),
            ServerMessage::Error { error, .. } => {
                Err(anyhow::anyhow!("File read failed: {:?}", error))
            },
            _ => Err(anyhow::anyhow!("Unexpected response for file read")),
        }
    }

    async fn write_file(&self, remote_path: &str, content: &[u8]) -> Result<()> {
        let id = self.connection.next_request_id().await;
        
        let request = ClientMessage::FileOperation(
            FileOperationRequest::Write {
                id,
                path: remote_path.into(),
                content: content.to_vec(),
                create_dirs: true,
            }
        );

        match self.connection.send_request(request).await? {
            ServerMessage::FileOperationResponse(
                FileOperationResponse::Write { bytes_written, .. }
            ) => {
                if bytes_written as usize != content.len() {
                    warn!("Partial write: {} of {} bytes", bytes_written, content.len());
                }
                Ok(())
            },
            ServerMessage::Error { error, .. } => {
                Err(anyhow::anyhow!("File write failed: {:?}", error))
            },
            _ => Err(anyhow::anyhow!("Unexpected response for file write")),
        }
    }

    async fn exists(&self, remote_path: &str) -> Result<bool> {
        let id = self.connection.next_request_id().await;
        
        let request = ClientMessage::FileOperation(
            FileOperationRequest::Exists {
                id,
                path: remote_path.into(),
            }
        );

        match self.connection.send_request(request).await? {
            ServerMessage::FileOperationResponse(
                FileOperationResponse::Exists { exists, .. }
            ) => Ok(exists),
            ServerMessage::Error { error, .. } => {
                Err(anyhow::anyhow!("File exists check failed: {:?}", error))
            },
            _ => Err(anyhow::anyhow!("Unexpected response for file exists check")),
        }
    }

    async fn list_dir(&self, remote_path: &str) -> Result<Vec<String>> {
        let id = self.connection.next_request_id().await;
        
        let request = ClientMessage::FileOperation(
            FileOperationRequest::List {
                id,
                path: remote_path.into(),
                recursive: false,
            }
        );

        match self.connection.send_request(request).await? {
            ServerMessage::FileOperationResponse(
                FileOperationResponse::List { entries, .. }
            ) => {
                Ok(entries.into_iter().map(|entry| entry.name).collect())
            },
            ServerMessage::Error { error, .. } => {
                Err(anyhow::anyhow!("Directory listing failed: {:?}", error))
            },
            _ => Err(anyhow::anyhow!("Unexpected response for directory listing")),
        }
    }
}