//! Connection management for remote development
//! 
//! Provides abstractions for different connection types (SSH, TCP, WebSocket)
//! and manages the lifecycle of remote connections.

use async_trait::async_trait;
use anyhow::Result;
use std::sync::Arc;

use crate::remote::RemoteServerConfig;

/// Trait for remote connections
#[async_trait]
pub trait Connection: Send + Sync {
    /// Connect to the remote server
    async fn connect(&self) -> Result<()>;
    
    /// Disconnect from the remote server
    async fn disconnect(&self) -> Result<()>;
    
    /// Check if connected
    fn is_connected(&self) -> bool;
    
    /// Execute a command on the remote server
    async fn execute_command(&self, command: &str, args: &[&str]) -> Result<CommandResult>;
    
    /// Create a tunnel for forwarding connections
    async fn create_tunnel(&self, local_port: u16, remote_port: u16) -> Result<()>;
    
    /// Get a file transfer interface
    async fn file_transfer(&self) -> Result<Box<dyn FileTransfer>>;
}

/// Result of executing a command
#[derive(Debug)]
pub struct CommandResult {
    pub exit_code: i32,
    pub stdout: String,
    pub stderr: String,
}

/// File transfer interface
#[async_trait]
pub trait FileTransfer: Send + Sync {
    /// Upload a file to the remote server
    async fn upload(&self, local_path: &str, remote_path: &str) -> Result<()>;
    
    /// Download a file from the remote server
    async fn download(&self, remote_path: &str, local_path: &str) -> Result<()>;
    
    /// Read a remote file
    async fn read_file(&self, remote_path: &str) -> Result<Vec<u8>>;
    
    /// Write a file to remote server
    async fn write_file(&self, remote_path: &str, content: &[u8]) -> Result<()>;
    
    /// Check if a remote path exists
    async fn exists(&self, remote_path: &str) -> Result<bool>;
    
    /// List directory contents
    async fn list_dir(&self, remote_path: &str) -> Result<Vec<String>>;
}

/// Create a connection based on configuration
pub async fn create_connection(config: &RemoteServerConfig) -> Result<Arc<dyn Connection>> {
    match &config.mode {
        crate::remote::RemoteMode::Direct | crate::remote::RemoteMode::Auto => {
            let ssh_conn = ssh::SSHConnection::new(config.clone()).await?;
            Ok(Arc::new(ssh_conn))
        },
        crate::remote::RemoteMode::Server { .. } => {
            // First try SSH, then potentially upgrade to server mode
            let ssh_conn = ssh::SSHConnection::new(config.clone()).await?;
            Ok(Arc::new(ssh_conn))
        }
    }
}

pub mod ssh;