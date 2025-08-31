//! LSP-Proxy Server - Remote LSP service daemon
//! 
//! This is a standalone server that runs on remote machines and manages
//! local LSP servers, accepting LSP requests from remote lsp-proxy clients.

use anyhow::{anyhow, Result};
use clap::{Arg, Command};
use log::{debug, error, info, warn};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::net::{TcpListener, TcpStream, UnixListener, UnixStream};
use tokio::sync::{Mutex, RwLock};
use tokio::process::{Child, Command as TokioCommand};
use uuid::Uuid;

/// LSP-Proxy Server configuration
#[derive(Debug, Clone, Deserialize)]
pub struct ServerConfig {
    /// Listening address (TCP or Unix socket)
    pub listen_address: String,
    /// Working directory
    pub work_dir: PathBuf,
    /// Maximum number of concurrent LSP servers
    pub max_lsp_servers: usize,
    /// LSP server configurations
    pub lsp_servers: HashMap<String, LspServerConfig>,
    /// Authentication settings
    pub auth: Option<AuthConfig>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct LspServerConfig {
    pub command: String,
    pub args: Vec<String>,
    pub env: HashMap<String, String>,
    pub initialization_options: Option<Value>,
}

#[derive(Debug, Clone, Deserialize)]  
pub struct AuthConfig {
    pub enabled: bool,
    pub token: Option<String>,
    pub allowed_hosts: Vec<String>,
}

impl Default for ServerConfig {
    fn default() -> Self {
        let mut lsp_servers = HashMap::new();
        
        // Default LSP server configurations
        lsp_servers.insert("rust".to_string(), LspServerConfig {
            command: "rust-analyzer".to_string(),
            args: vec![],
            env: HashMap::new(),
            initialization_options: None,
        });
        
        lsp_servers.insert("python".to_string(), LspServerConfig {
            command: "pylsp".to_string(),
            args: vec![],
            env: HashMap::new(),
            initialization_options: None,
        });
        
        lsp_servers.insert("typescript".to_string(), LspServerConfig {
            command: "typescript-language-server".to_string(),
            args: vec!["--stdio".to_string()],
            env: HashMap::new(),
            initialization_options: None,
        });
        
        lsp_servers.insert("go".to_string(), LspServerConfig {
            command: "gopls".to_string(),
            args: vec![],
            env: HashMap::new(),
            initialization_options: None,
        });
        
        lsp_servers.insert("c".to_string(), LspServerConfig {
            command: "clangd".to_string(),
            args: vec![],
            env: HashMap::new(),
            initialization_options: None,
        });
        
        lsp_servers.insert("cpp".to_string(), LspServerConfig {
            command: "clangd".to_string(),
            args: vec![],
            env: HashMap::new(),
            initialization_options: None,
        });
        
        Self {
            listen_address: "127.0.0.1:9527".to_string(),
            work_dir: PathBuf::from("."),
            max_lsp_servers: 10,
            lsp_servers,
            auth: Some(AuthConfig {
                enabled: false,
                token: None,
                allowed_hosts: vec!["127.0.0.1".to_string()],
            }),
        }
    }
}

/// Request/Response protocol between local lsp-proxy and remote lsp-proxy-server
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum ProxyMessage {
    /// Start an LSP server for a workspace
    StartLspServer {
        id: String,
        language: String,
        workspace_root: PathBuf,
        client_capabilities: Option<Value>,
    },
    
    /// Stop an LSP server
    StopLspServer {
        id: String,
    },
    
    /// Forward LSP request to server
    LspRequest {
        server_id: String,
        method: String,
        params: Value,
        request_id: Option<u64>,
    },
    
    /// Forward LSP notification to server
    LspNotification {
        server_id: String, 
        method: String,
        params: Value,
    },
    
    /// Response to request
    Response {
        id: String,
        result: Result<Value, String>,
    },
    
    /// LSP server notification/response forwarding
    LspResponse {
        server_id: String,
        response: Value,
    },
    
    /// Server status and health check
    Ping,
    Pong,
    
    /// List available LSP servers
    ListServers,
    ServersResponse {
        servers: Vec<LspServerInfo>,
    },
}

/// Managed LSP server instance
#[derive(Debug)]
pub struct ManagedLspServer {
    pub id: String,
    pub language: String,
    pub workspace_root: PathBuf,
    pub process: Child,
    pub stdin: Arc<Mutex<tokio::process::ChildStdin>>,
    pub request_id_counter: Arc<Mutex<u64>>,
    pub pending_requests: Arc<Mutex<HashMap<u64, tokio::sync::oneshot::Sender<Value>>>>,
    pub initialized: Arc<Mutex<bool>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LspServerInfo {
    pub id: String,
    pub language: String,
    pub workspace_root: PathBuf,
    pub status: String,  // "starting", "running", "stopped", "error"
    pub pid: Option<u32>,
}

/// Main server state
#[derive(Debug)]
pub struct ProxyServer {
    config: ServerConfig,
    lsp_servers: Arc<RwLock<HashMap<String, Arc<ManagedLspServer>>>>,
    client_connections: Arc<RwLock<HashMap<String, ClientConnection>>>,
}

#[derive(Debug)]
pub struct ClientConnection {
    pub id: String,
    pub remote_addr: String,
    pub authenticated: bool,
}

impl ProxyServer {
    pub fn new(config: ServerConfig) -> Self {
        Self {
            config,
            lsp_servers: Arc::new(RwLock::new(HashMap::new())),
            client_connections: Arc::new(RwLock::new(HashMap::new())),
        }
    }
    
    /// Start the proxy server
    pub async fn start(&self) -> Result<()> {
        info!("Starting lsp-proxy-server on {}", self.config.listen_address);
        
        if self.config.listen_address.starts_with('/') || self.config.listen_address.starts_with('.') {
            // Unix socket
            self.start_unix_server().await
        } else {
            // TCP socket
            self.start_tcp_server().await
        }
    }
    
    /// Run in stdio mode for SSH pipe communication
    pub async fn run_stdio_mode(&self) -> Result<()> {
        info!("Running in stdio mode, communicating via stdin/stdout");
        
        let stdin = tokio::io::stdin();
        let stdout = tokio::io::stdout();
        
        let mut reader = BufReader::new(stdin);
        let mut writer = stdout;
        let mut line = String::new();
        
        // Create a dummy client ID for stdio mode
        let client_id = "stdio-client";
        
        // Register the stdio client
        {
            let mut clients = self.client_connections.write().await;
            clients.insert(client_id.to_string(), ClientConnection {
                id: client_id.to_string(),
                remote_addr: "stdio".to_string(),
                authenticated: true, // stdio connections are trusted
            });
        }
        
        info!("Ready to accept JSON-RPC messages via stdin/stdout");
        
        loop {
            line.clear();
            match reader.read_line(&mut line).await {
                Ok(0) => {
                    info!("EOF received, shutting down");
                    break;
                }
                Ok(_) => {
                    let line_content = line.trim();
                    if line_content.is_empty() {
                        continue;
                    }
                    
                    debug!("Received via stdin: {}", line_content);
                    
                    match serde_json::from_str::<ProxyMessage>(line_content) {
                        Ok(message) => {
                            let response = self.process_message(client_id, message).await;
                            
                            if let Some(resp) = response {
                                let resp_json = serde_json::to_string(&resp)?;
                                writer.write_all(resp_json.as_bytes()).await?;
                                writer.write_all(b"\n").await?;
                                writer.flush().await?;
                                debug!("Sent via stdout: {}", resp_json);
                            }
                        }
                        Err(e) => {
                            error!("Failed to parse JSON-RPC message: {}", e);
                            // Send error response
                            let error_response = ProxyMessage::Response {
                                id: "parse_error".to_string(),
                                result: Err(format!("JSON parse error: {}", e)),
                            };
                            let error_json = serde_json::to_string(&error_response)?;
                            writer.write_all(error_json.as_bytes()).await?;
                            writer.write_all(b"\n").await?;
                            writer.flush().await?;
                        }
                    }
                }
                Err(e) => {
                    error!("Error reading from stdin: {}", e);
                    break;
                }
            }
        }
        
        // Cleanup
        self.shutdown_all_lsp_servers().await;
        info!("Stdio mode server shutdown complete");
        Ok(())
    }
    
    async fn start_tcp_server(&self) -> Result<()> {
        let listener = TcpListener::bind(&self.config.listen_address).await?;
        info!("TCP server listening on {}", self.config.listen_address);
        
        // Setup signal handling
        let (shutdown_tx, mut shutdown_rx) = tokio::sync::oneshot::channel::<()>();
        let shutdown_tx = Arc::new(Mutex::new(Some(shutdown_tx)));
        
        // Spawn signal handler
        let shutdown_tx_clone = shutdown_tx.clone();
        tokio::spawn(async move {
            match tokio::signal::ctrl_c().await {
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
        
        loop {
            tokio::select! {
                result = listener.accept() => {
                    match result {
                        Ok((stream, addr)) => {
                            info!("New client connection from {}", addr);
                            let server = Arc::new(self.clone());
                            tokio::spawn(async move {
                                if let Err(e) = server.handle_tcp_client(stream, addr).await {
                                    error!("Client handler error: {}", e);
                                }
                            });
                        }
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
        
        // Cleanup
        self.shutdown_all_lsp_servers().await;
        info!("LSP-Proxy server shutdown complete");
        Ok(())
    }
    
    async fn start_unix_server(&self) -> Result<()> {
        // Remove existing socket file
        if let Err(e) = std::fs::remove_file(&self.config.listen_address) {
            debug!("Could not remove socket file: {}", e);
        }
        
        let listener = UnixListener::bind(&self.config.listen_address)?;
        info!("Unix server listening on {}", self.config.listen_address);
        
        loop {
            match listener.accept().await {
                Ok((stream, _)) => {
                    info!("New client connection via Unix socket");
                    let server = Arc::new(self.clone());
                    tokio::spawn(async move {
                        if let Err(e) = server.handle_unix_client(stream).await {
                            error!("Client handler error: {}", e);
                        }
                    });
                }
                Err(e) => {
                    error!("Failed to accept connection: {}", e);
                }
            }
        }
    }
    
    async fn handle_tcp_client(&self, stream: TcpStream, addr: SocketAddr) -> Result<()> {
        let client_id = format!("tcp-{}", addr);
        
        // Register client
        {
            let mut clients = self.client_connections.write().await;
            clients.insert(client_id.clone(), ClientConnection {
                id: client_id.clone(),
                remote_addr: addr.to_string(),
                authenticated: !self.config.auth.as_ref().map_or(false, |a| a.enabled),
            });
        }
        
        self.handle_client_stream(stream, &client_id).await?;
        
        // Cleanup client
        {
            let mut clients = self.client_connections.write().await;
            clients.remove(&client_id);
        }
        
        info!("Client {} disconnected", client_id);
        Ok(())
    }
    
    async fn handle_unix_client(&self, stream: UnixStream) -> Result<()> {
        let client_id = format!("unix-{}", Uuid::new_v4());
        
        // Register client
        {
            let mut clients = self.client_connections.write().await;
            clients.insert(client_id.clone(), ClientConnection {
                id: client_id.clone(),
                remote_addr: "unix".to_string(),
                authenticated: true, // Unix socket connections are trusted
            });
        }
        
        self.handle_client_stream(stream, &client_id).await?;
        
        // Cleanup client
        {
            let mut clients = self.client_connections.write().await;
            clients.remove(&client_id);
        }
        
        info!("Client {} disconnected", client_id);
        Ok(())
    }
    
    async fn handle_client_stream<T>(&self, stream: T, client_id: &str) -> Result<()>
    where
        T: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin,
    {
        let (reader, mut writer) = tokio::io::split(stream);
        let mut buf_reader = BufReader::new(reader);
        let mut line = String::new();
        
        loop {
            line.clear();
            match buf_reader.read_line(&mut line).await {
                Ok(0) => break, // EOF
                Ok(_) => {
                    let line_content = line.trim();
                    if line_content.is_empty() {
                        continue;
                    }
                    
                    debug!("Received from {}: {}", client_id, line_content);
                    
                    match serde_json::from_str::<ProxyMessage>(line_content) {
                        Ok(message) => {
                            let response = self.process_message(client_id, message).await;
                            
                            if let Some(resp) = response {
                                let resp_json = serde_json::to_string(&resp)?;
                                writer.write_all(resp_json.as_bytes()).await?;
                                writer.write_all(b"\n").await?;
                                writer.flush().await?;
                            }
                        }
                        Err(e) => {
                            error!("Failed to parse message from {}: {}", client_id, e);
                        }
                    }
                }
                Err(e) => {
                    error!("Connection error with {}: {}", client_id, e);
                    break;
                }
            }
        }
        
        Ok(())
    }
    
    async fn process_message(&self, client_id: &str, message: ProxyMessage) -> Option<ProxyMessage> {
        // Check authentication
        {
            let clients = self.client_connections.read().await;
            if let Some(client) = clients.get(client_id) {
                if !client.authenticated {
                    return Some(ProxyMessage::Response {
                        id: "auth_error".to_string(),
                        result: Err("Authentication required".to_string()),
                    });
                }
            }
        }
        
        match message {
            ProxyMessage::Ping => {
                Some(ProxyMessage::Pong)
            }
            
            ProxyMessage::StartLspServer { id, language, workspace_root, client_capabilities } => {
                match self.start_lsp_server(&id, &language, &workspace_root, client_capabilities).await {
                    Ok(_) => Some(ProxyMessage::Response {
                        id: id.clone(),
                        result: Ok(json!({"server_id": id, "status": "started"})),
                    }),
                    Err(e) => Some(ProxyMessage::Response {
                        id,
                        result: Err(format!("Failed to start LSP server: {}", e)),
                    })
                }
            }
            
            ProxyMessage::StopLspServer { id } => {
                match self.stop_lsp_server(&id).await {
                    Ok(_) => Some(ProxyMessage::Response {
                        id: id.clone(),
                        result: Ok(json!({"server_id": id, "status": "stopped"})),
                    }),
                    Err(e) => Some(ProxyMessage::Response {
                        id,
                        result: Err(format!("Failed to stop LSP server: {}", e)),
                    })
                }
            }
            
            ProxyMessage::LspRequest { server_id, method, params, request_id } => {
                match self.forward_lsp_request(&server_id, &method, params, request_id).await {
                    Ok(result) => Some(ProxyMessage::Response {
                        id: server_id,
                        result: Ok(result),
                    }),
                    Err(e) => Some(ProxyMessage::Response {
                        id: server_id,
                        result: Err(format!("LSP request failed: {}", e)),
                    })
                }
            }
            
            ProxyMessage::LspNotification { server_id, method, params } => {
                if let Err(e) = self.forward_lsp_notification(&server_id, &method, params).await {
                    warn!("Failed to forward LSP notification: {}", e);
                }
                None // Notifications don't expect responses
            }
            
            ProxyMessage::ListServers => {
                let servers = self.list_lsp_servers().await;
                Some(ProxyMessage::ServersResponse { servers })
            }
            
            _ => {
                warn!("Unhandled message type");
                None
            }
        }
    }
    
    async fn start_lsp_server(&self, id: &str, language: &str, workspace_root: &PathBuf, _client_capabilities: Option<Value>) -> Result<()> {
        // Check if server already exists
        {
            let servers = self.lsp_servers.read().await;
            if servers.contains_key(id) {
                return Ok(()); // Server already running
            }
        }
        
        // Get LSP server config
        let lsp_config = self.config.lsp_servers.get(language)
            .ok_or_else(|| anyhow!("No LSP server configured for language: {}", language))?;
        
        info!("Starting {} LSP server with ID: {}", language, id);
        
        // Start the LSP server process
        let mut cmd = TokioCommand::new(&lsp_config.command);
        cmd.args(&lsp_config.args)
           .current_dir(workspace_root)
           .stdin(std::process::Stdio::piped())
           .stdout(std::process::Stdio::piped())
           .stderr(std::process::Stdio::piped())
           .kill_on_drop(true);
        
        // Set environment variables
        for (key, value) in &lsp_config.env {
            cmd.env(key, value);
        }
        
        let mut process = cmd.spawn()?;
        let stdin = process.stdin.take().ok_or_else(|| anyhow!("Failed to get stdin"))?;
        let _stdout = process.stdout.take().ok_or_else(|| anyhow!("Failed to get stdout"))?;
        let _stderr = process.stderr.take().ok_or_else(|| anyhow!("Failed to get stderr"))?;
        
        let managed_server = Arc::new(ManagedLspServer {
            id: id.to_string(),
            language: language.to_string(), 
            workspace_root: workspace_root.clone(),
            process,
            stdin: Arc::new(Mutex::new(stdin)),
            request_id_counter: Arc::new(Mutex::new(1)),
            pending_requests: Arc::new(Mutex::new(HashMap::new())),
            initialized: Arc::new(Mutex::new(false)),
        });
        
        // Store the server
        {
            let mut servers = self.lsp_servers.write().await;
            servers.insert(id.to_string(), managed_server.clone());
        }
        
        // TODO: Send initialize request to LSP server
        // TODO: Start stdout/stderr handling tasks
        
        info!("LSP server {} started successfully", id);
        Ok(())
    }
    
    async fn stop_lsp_server(&self, id: &str) -> Result<()> {
        let server = {
            let mut servers = self.lsp_servers.write().await;
            servers.remove(id)
        };
        
        if let Some(mut server) = server {
            info!("Stopping LSP server {}", id);
            
            // TODO: Send shutdown and exit requests to LSP server
            
            // Kill the process
            if let Err(e) = Arc::get_mut(&mut server).unwrap().process.kill().await {
                warn!("Failed to kill LSP server process: {}", e);
            }
            
            info!("LSP server {} stopped", id);
        }
        
        Ok(())
    }
    
    async fn forward_lsp_request(&self, server_id: &str, method: &str, params: Value, request_id: Option<u64>) -> Result<Value> {
        let servers = self.lsp_servers.read().await;
        let server = servers.get(server_id).ok_or_else(|| anyhow!("LSP server not found: {}", server_id))?;
        
        // Generate request ID
        let req_id = if let Some(id) = request_id {
            id
        } else {
            let mut counter = server.request_id_counter.lock().await;
            let id = *counter;
            *counter += 1;
            id
        };
        
        // Create JSON-RPC request
        let request = json!({
            "jsonrpc": "2.0",
            "id": req_id,
            "method": method,
            "params": params
        });
        
        // Send request to LSP server
        // TODO: Implement actual JSON-RPC communication with LSP server
        debug!("Forwarding LSP request to {}: {} - {}", server_id, method, request);
        
        // For now, return a dummy response based on method
        let dummy_response = match method {
            "initialize" => json!({
                "capabilities": {
                    "textDocumentSync": 1,
                    "completionProvider": true,
                    "hoverProvider": true,
                    "definitionProvider": true
                }
            }),
            "textDocument/completion" => json!({
                "items": [
                    {
                        "label": "example_completion",
                        "kind": 1,
                        "detail": "Example completion item"
                    }
                ]
            }),
            "textDocument/hover" => json!({
                "contents": {
                    "kind": "markdown",
                    "value": "Example hover information"
                }
            }),
            _ => json!(null)
        };
        
        Ok(dummy_response)
    }
    
    async fn forward_lsp_notification(&self, server_id: &str, method: &str, params: Value) -> Result<()> {
        let servers = self.lsp_servers.read().await;
        let _server = servers.get(server_id).ok_or_else(|| anyhow!("LSP server not found: {}", server_id))?;
        
        // Create JSON-RPC notification
        let notification = json!({
            "jsonrpc": "2.0", 
            "method": method,
            "params": params
        });
        
        // Send notification to LSP server
        // TODO: Implement actual JSON-RPC communication
        debug!("Forwarding LSP notification to {}: {} - {}", server_id, method, notification);
        
        Ok(())
    }
    
    async fn list_lsp_servers(&self) -> Vec<LspServerInfo> {
        let servers = self.lsp_servers.read().await;
        let mut server_list = Vec::new();
        
        for server in servers.values() {
            let pid = server.process.id();
            server_list.push(LspServerInfo {
                id: server.id.clone(),
                language: server.language.clone(),
                workspace_root: server.workspace_root.clone(),
                status: "running".to_string(), // TODO: Get actual status
                pid,
            });
        }
        
        server_list
    }
    
    async fn shutdown_all_lsp_servers(&self) {
        let mut servers = self.lsp_servers.write().await;
        for (id, mut server) in servers.drain() {
            info!("Shutting down LSP server {}", id);
            if let Err(e) = Arc::get_mut(&mut server).unwrap().process.kill().await {
                warn!("Failed to kill LSP server {}: {}", id, e);
            }
        }
    }
}

impl Clone for ProxyServer {
    fn clone(&self) -> Self {
        Self {
            config: self.config.clone(),
            lsp_servers: Arc::clone(&self.lsp_servers),
            client_connections: Arc::clone(&self.client_connections),
        }
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    let matches = Command::new("lsp-proxy-server")
        .version("1.0.0")
        .about("LSP-Proxy Server - Remote LSP service daemon")
        .arg(Arg::new("config")
            .short('c')
            .long("config")
            .value_name("FILE")
            .help("Configuration file path")
            .default_value("lsp-proxy-server.toml"))
        .arg(Arg::new("listen")
            .short('l')
            .long("listen")
            .value_name("ADDRESS")  
            .help("Listen address (e.g., 127.0.0.1:9527 or /tmp/lsp-proxy.sock)"))
        .arg(Arg::new("work-dir")
            .short('w')
            .long("work-dir") 
            .value_name("DIR")
            .help("Working directory")
            .default_value("."))
        .arg(Arg::new("stdio")
            .long("stdio")
            .help("Run in stdio mode for SSH pipe communication")
            .action(clap::ArgAction::SetTrue))
        .arg(Arg::new("log-level")
            .long("log-level")
            .value_name("LEVEL")
            .help("Log level (error, warn, info, debug, trace)")
            .default_value("info"))
        .get_matches();
    
    // Initialize logging
    let log_level = matches.get_one::<String>("log-level").unwrap();
    let stdio_mode = matches.get_flag("stdio");
    
    // In stdio mode, log to stderr to avoid interfering with JSON-RPC communication
    let mut log_builder = env_logger::Builder::from_default_env();
    log_builder.filter_level(match log_level.as_str() {
            "error" => log::LevelFilter::Error,
            "warn" => log::LevelFilter::Warn,
            "info" => log::LevelFilter::Info,
            "debug" => log::LevelFilter::Debug,
            "trace" => log::LevelFilter::Trace,
            _ => log::LevelFilter::Info,
        })
        .format_timestamp_millis();
        
    if stdio_mode {
        log_builder.target(env_logger::Target::Stderr);
    }
    
    log_builder.init();
    
    info!("Starting lsp-proxy-server v{}", env!("CARGO_PKG_VERSION"));
    
    let config_path = matches.get_one::<String>("config").unwrap();
    let mut config = if std::path::Path::new(config_path).exists() {
        let content = std::fs::read_to_string(config_path)?;
        toml::from_str(&content)?
    } else {
        info!("Config file not found, using defaults");
        ServerConfig::default()
    };
    
    // Override with CLI arguments
    if let Some(listen) = matches.get_one::<String>("listen") {
        config.listen_address = listen.clone();
    }
    
    if let Some(work_dir) = matches.get_one::<String>("work-dir") {
        config.work_dir = PathBuf::from(work_dir);
    }
    
    let server = ProxyServer::new(config.clone());
    
    if stdio_mode {
        info!("Running in stdio mode for SSH pipe communication");
        server.run_stdio_mode().await?;
    } else {
        info!("Starting lsp-proxy-server with config: {:#?}", config);
        server.start().await?;
    }
    
    Ok(())
}