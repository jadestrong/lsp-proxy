//! LSP-Proxy Server Mode Protocol
//!
//! This module defines the communication protocol between the main LSP-Proxy
//! instance and remote lsp-proxy-server instances.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

/// Protocol version for compatibility checking
pub const PROTOCOL_VERSION: &str = "1.0.0";

/// Messages sent from client (main lsp-proxy) to server (lsp-proxy-server)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "data")]
pub enum ClientMessage {
    /// Handshake to establish connection
    Handshake {
        version: String,
        capabilities: ClientCapabilities,
    },
    /// Request file operation
    FileOperation(FileOperationRequest),
    /// Request LSP operation
    LspOperation(LspOperationRequest),
    /// Request workspace information
    WorkspaceOperation(WorkspaceOperationRequest),
    /// Health check / ping
    Ping { id: u64 },
    /// Request server shutdown
    Shutdown,
}

/// Messages sent from server (lsp-proxy-server) to client (main lsp-proxy)
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "data")]
pub enum ServerMessage {
    /// Handshake response
    HandshakeResponse {
        version: String,
        capabilities: ServerCapabilities,
        server_info: ServerInfo,
    },
    /// File operation response
    FileOperationResponse(FileOperationResponse),
    /// LSP operation response
    LspOperationResponse(LspOperationResponse),
    /// Workspace operation response
    WorkspaceOperationResponse(WorkspaceOperationResponse),
    /// Ping response
    Pong { id: u64 },
    /// Error response
    Error { id: Option<u64>, error: ProtocolError },
    /// Server notification
    Notification(ServerNotification),
}

/// Client capabilities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClientCapabilities {
    /// Supported file operations
    pub file_operations: Vec<String>,
    /// Supported LSP features
    pub lsp_features: Vec<String>,
    /// Compression support
    pub compression: bool,
    /// Streaming support for large files
    pub streaming: bool,
}

/// Server capabilities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerCapabilities {
    /// Available language servers
    pub language_servers: Vec<LanguageServerInfo>,
    /// Supported file operations
    pub file_operations: Vec<String>,
    /// Maximum file size for operations
    pub max_file_size: u64,
    /// Compression support
    pub compression: bool,
    /// Streaming support
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

/// Language server information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LanguageServerInfo {
    pub name: String,
    pub command: String,
    pub args: Vec<String>,
    pub file_types: Vec<String>,
    pub capabilities: Vec<String>,
}

/// File operation requests
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "operation")]
pub enum FileOperationRequest {
    Read { 
        id: u64, 
        path: PathBuf,
        offset: Option<u64>,
        length: Option<u64>,
    },
    Write { 
        id: u64, 
        path: PathBuf, 
        content: Vec<u8>,
        create_dirs: bool,
    },
    List { 
        id: u64, 
        path: PathBuf,
        recursive: bool,
    },
    Exists { 
        id: u64, 
        path: PathBuf 
    },
    Delete { 
        id: u64, 
        path: PathBuf,
        recursive: bool,
    },
    Move { 
        id: u64, 
        from: PathBuf, 
        to: PathBuf 
    },
    Copy { 
        id: u64, 
        from: PathBuf, 
        to: PathBuf,
        recursive: bool,
    },
    CreateDir { 
        id: u64, 
        path: PathBuf,
        recursive: bool,
    },
    GetMetadata { 
        id: u64, 
        path: PathBuf 
    },
    Watch { 
        id: u64, 
        path: PathBuf,
        recursive: bool,
    },
    Unwatch { 
        id: u64, 
        path: PathBuf 
    },
}

/// File operation responses
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "operation")]
pub enum FileOperationResponse {
    Read { 
        id: u64, 
        content: Vec<u8>,
        metadata: FileMetadata,
    },
    Write { 
        id: u64, 
        bytes_written: u64 
    },
    List { 
        id: u64, 
        entries: Vec<FileEntry> 
    },
    Exists { 
        id: u64, 
        exists: bool 
    },
    Delete { 
        id: u64, 
        success: bool 
    },
    Move { 
        id: u64, 
        success: bool 
    },
    Copy { 
        id: u64, 
        success: bool 
    },
    CreateDir { 
        id: u64, 
        success: bool 
    },
    GetMetadata { 
        id: u64, 
        metadata: FileMetadata 
    },
    Watch { 
        id: u64, 
        watcher_id: String 
    },
    Unwatch { 
        id: u64, 
        success: bool 
    },
}

/// File metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileMetadata {
    pub size: u64,
    pub modified: u64,  // Unix timestamp
    pub created: u64,   // Unix timestamp
    pub is_dir: bool,
    pub is_file: bool,
    pub is_symlink: bool,
    pub permissions: u32,
}

/// File entry for directory listing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileEntry {
    pub name: String,
    pub path: PathBuf,
    pub metadata: FileMetadata,
}

/// LSP operation requests
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "operation")]
pub enum LspOperationRequest {
    StartServer { 
        id: u64, 
        server_name: String,
        workspace_root: PathBuf,
    },
    StopServer { 
        id: u64, 
        server_id: String 
    },
    SendRequest { 
        id: u64, 
        server_id: String,
        method: String,
        params: serde_json::Value,
    },
    SendNotification { 
        id: u64, 
        server_id: String,
        method: String,
        params: serde_json::Value,
    },
    ListServers { 
        id: u64 
    },
    GetServerStatus { 
        id: u64, 
        server_id: String 
    },
}

/// LSP operation responses
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "operation")]
pub enum LspOperationResponse {
    StartServer { 
        id: u64, 
        server_id: String,
        capabilities: serde_json::Value,
    },
    StopServer { 
        id: u64, 
        success: bool 
    },
    SendRequest { 
        id: u64, 
        result: serde_json::Value 
    },
    SendNotification { 
        id: u64, 
        success: bool 
    },
    ListServers { 
        id: u64, 
        servers: Vec<LspServerStatus> 
    },
    GetServerStatus { 
        id: u64, 
        status: LspServerStatus 
    },
}

/// LSP server status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LspServerStatus {
    pub server_id: String,
    pub name: String,
    pub status: String,  // "running", "stopped", "error"
    pub workspace_root: PathBuf,
    pub capabilities: serde_json::Value,
    pub pid: Option<u32>,
    pub memory_usage: Option<u64>,
}

/// Workspace operation requests
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "operation")]
pub enum WorkspaceOperationRequest {
    GetInfo { 
        id: u64, 
        path: PathBuf 
    },
    Search { 
        id: u64, 
        pattern: String,
        path: PathBuf,
        include_hidden: bool,
    },
    GetEnvironment { 
        id: u64 
    },
    ExecuteCommand { 
        id: u64, 
        command: String,
        args: Vec<String>,
        working_dir: Option<PathBuf>,
        env: HashMap<String, String>,
    },
}

/// Workspace operation responses
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "operation")]
pub enum WorkspaceOperationResponse {
    GetInfo { 
        id: u64, 
        info: WorkspaceInfo 
    },
    Search { 
        id: u64, 
        results: Vec<SearchResult> 
    },
    GetEnvironment { 
        id: u64, 
        environment: HashMap<String, String> 
    },
    ExecuteCommand { 
        id: u64, 
        exit_code: i32,
        stdout: String,
        stderr: String,
    },
}

/// Workspace information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceInfo {
    pub root: PathBuf,
    pub name: String,
    pub project_type: Option<String>,
    pub language_servers: Vec<String>,
    pub git_repository: Option<GitInfo>,
}

/// Git repository information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GitInfo {
    pub branch: String,
    pub remote: Option<String>,
    pub commit: String,
    pub is_dirty: bool,
}

/// Search result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchResult {
    pub file: PathBuf,
    pub line: u32,
    pub column: u32,
    pub content: String,
}

/// Server notifications
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "notification")]
pub enum ServerNotification {
    FileChanged { 
        path: PathBuf,
        change_type: String,  // "created", "modified", "deleted"
    },
    LspServerCrashed { 
        server_id: String,
        error: String 
    },
    LspNotification { 
        server_id: String,
        method: String,
        params: serde_json::Value,
    },
    MemoryWarning { 
        current_usage: u64,
        limit: u64 
    },
}

/// Protocol errors
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProtocolError {
    pub code: i32,
    pub message: String,
    pub data: Option<serde_json::Value>,
}

impl ProtocolError {
    pub fn invalid_request(message: &str) -> Self {
        Self {
            code: -32600,
            message: message.to_string(),
            data: None,
        }
    }

    pub fn method_not_found(method: &str) -> Self {
        Self {
            code: -32601,
            message: format!("Method not found: {}", method),
            data: None,
        }
    }

    pub fn invalid_params(message: &str) -> Self {
        Self {
            code: -32602,
            message: message.to_string(),
            data: None,
        }
    }

    pub fn internal_error(message: &str) -> Self {
        Self {
            code: -32603,
            message: message.to_string(),
            data: None,
        }
    }

    pub fn server_error(code: i32, message: &str) -> Self {
        Self {
            code,
            message: message.to_string(),
            data: None,
        }
    }
}