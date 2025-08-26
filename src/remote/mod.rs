//! Remote development module for LSP-Proxy
//! 
//! This module provides remote development capabilities including:
//! - SSH-based connections to remote servers
//! - Remote file system access with local caching
//! - LSP proxy functionality for remote language servers
//! - Server mode for enhanced performance

pub mod connection;
pub mod filesystem;
pub mod lsp;
pub mod auth;
pub mod config;

use std::path::PathBuf;
use serde::{Deserialize, Serialize};
use anyhow::Result;
use std::sync::Arc;

/// Remote server configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RemoteServerConfig {
    pub name: String,
    pub host: String,
    pub port: Option<u16>,
    pub user: String,
    pub auth: RemoteAuth,
    pub workspace_root: Option<PathBuf>,
    pub connection_timeout: Option<u64>,
    pub mode: RemoteMode,
}

/// Authentication methods for remote connections
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RemoteAuth {
    /// SSH key authentication
    Key { path: PathBuf, passphrase: Option<String> },
    /// Password authentication
    Password(String),
    /// SSH agent authentication
    Agent,
}

/// Remote development modes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RemoteMode {
    /// Direct SSH mode (zero deployment)
    Direct,
    /// Server mode with optional auto-deployment
    Server { auto_deploy: bool, server_path: Option<PathBuf> },
    /// Auto-detect best mode
    Auto,
}

/// Remote development session
#[derive(Clone)]
pub struct RemoteSession {
    pub config: RemoteServerConfig,
    pub connection: Arc<dyn connection::Connection>,
    pub filesystem: Arc<dyn filesystem::RemoteFileSystem>,
    pub lsp_proxy: Arc<dyn lsp::RemoteLspProxy>,
}

impl RemoteSession {
    /// Create a new remote session
    pub async fn new(config: RemoteServerConfig) -> Result<Self> {
        let connection = Arc::new(connection::ssh::SSHConnection::new(config.clone()).await?);
        let filesystem = filesystem::create_filesystem(connection.clone()).await?;
        let lsp_proxy = lsp::create_lsp_proxy(connection.clone()).await?;

        Ok(RemoteSession {
            config,
            connection,
            filesystem: Arc::from(filesystem),
            lsp_proxy: Arc::from(lsp_proxy),
        })
    }

    /// Connect to the remote server
    pub async fn connect(&mut self) -> Result<()> {
        self.connection.connect().await
    }

    /// Disconnect from the remote server
    pub async fn disconnect(&mut self) -> Result<()> {
        self.connection.disconnect().await
    }

    /// Check if connected to remote server
    pub fn is_connected(&self) -> bool {
        self.connection.is_connected()
    }
}