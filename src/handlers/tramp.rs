//! Enhanced remote handlers for TRAMP integration
//! 
//! This module extends the remote development capabilities to support
//! TRAMP-style file operations and remote LSP server management.

use super::super::{RemoteServerConfig, RemoteMode, RemoteAuth};
use crate::application::Application;
use crate::msg::{RequestId, ResponseError};
use anyhow::Result;
use log::{debug, info, warn};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Enhanced remote file operations for TRAMP integration
pub async fn handle_remote_file_exists(
    app: &mut Application,
    params: Value,
) -> Result<Value, ResponseError> {
    let server_name = params["server_name"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing server_name"))?;
    
    let file_path = params["file_path"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing file_path"))?;

    if let Some(session) = app.remote_sessions.get(server_name) {
        match session.filesystem.exists(file_path).await {
            Ok(exists) => Ok(json!({
                "success": true,
                "exists": exists
            })),
            Err(e) => Ok(json!({
                "success": false,
                "message": format!("Failed to check file existence: {}", e)
            }))
        }
    } else {
        Ok(json!({
            "success": false,
            "message": format!("Server '{}' not found or not connected", server_name)
        }))
    }
}

pub async fn handle_remote_file_info(
    app: &mut Application,
    params: Value,
) -> Result<Value, ResponseError> {
    let server_name = params["server_name"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing server_name"))?;
    
    let file_path = params["file_path"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing file_path"))?;

    if let Some(session) = app.remote_sessions.get(server_name) {
        match session.filesystem.get_file_info(file_path).await {
            Ok(info) => Ok(json!({
                "success": true,
                "type": info.file_type,
                "size": info.size,
                "modified": info.modified_time,
                "permissions": info.permissions
            })),
            Err(e) => Ok(json!({
                "success": false,
                "message": format!("Failed to get file info: {}", e)
            }))
        }
    } else {
        Ok(json!({
            "success": false,
            "message": format!("Server '{}' not found or not connected", server_name)
        }))
    }
}

pub async fn handle_remote_file_attributes(
    app: &mut Application,
    params: Value,
) -> Result<Value, ResponseError> {
    let server_name = params["server_name"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing server_name"))?;
    
    let file_path = params["file_path"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing file_path"))?;

    if let Some(session) = app.remote_sessions.get(server_name) {
        match session.filesystem.get_file_attributes(file_path).await {
            Ok(attrs) => Ok(json!({
                "success": true,
                "is_directory": attrs.is_directory,
                "links": attrs.links,
                "uid": attrs.uid,
                "gid": attrs.gid,
                "atime": attrs.access_time,
                "mtime": attrs.modified_time,
                "ctime": attrs.change_time,
                "size": attrs.size,
                "mode": attrs.mode,
                "inode": attrs.inode,
                "device": attrs.device
            })),
            Err(e) => Ok(json!({
                "success": false,
                "message": format!("Failed to get file attributes: {}", e)
            }))
        }
    } else {
        Ok(json!({
            "success": false,
            "message": format!("Server '{}' not found or not connected", server_name)
        }))
    }
}

pub async fn handle_remote_directory_list(
    app: &mut Application,
    params: Value,
) -> Result<Value, ResponseError> {
    let server_name = params["server_name"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing server_name"))?;
    
    let directory_path = params["directory_path"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing directory_path"))?;
    
    let full_path = params["full_path"].as_bool().unwrap_or(false);
    let pattern = params["pattern"].as_str();

    if let Some(session) = app.remote_sessions.get(server_name) {
        match session.filesystem.list_directory(directory_path, full_path, pattern).await {
            Ok(files) => Ok(json!({
                "success": true,
                "files": files
            })),
            Err(e) => Ok(json!({
                "success": false,
                "message": format!("Failed to list directory: {}", e)
            }))
        }
    } else {
        Ok(json!({
            "success": false,
            "message": format!("Server '{}' not found or not connected", server_name)
        }))
    }
}

pub async fn handle_remote_make_directory(
    app: &mut Application,
    params: Value,
) -> Result<Value, ResponseError> {
    let server_name = params["server_name"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing server_name"))?;
    
    let directory_path = params["directory_path"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing directory_path"))?;
    
    let parents = params["parents"].as_bool().unwrap_or(false);

    if let Some(session) = app.remote_sessions.get(server_name) {
        match session.filesystem.create_directory(directory_path, parents).await {
            Ok(_) => Ok(json!({
                "success": true
            })),
            Err(e) => Ok(json!({
                "success": false,
                "message": format!("Failed to create directory: {}", e)
            }))
        }
    } else {
        Ok(json!({
            "success": false,
            "message": format!("Server '{}' not found or not connected", server_name)
        }))
    }
}

pub async fn handle_remote_delete_file(
    app: &mut Application,
    params: Value,
) -> Result<Value, ResponseError> {
    let server_name = params["server_name"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing server_name"))?;
    
    let file_path = params["file_path"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing file_path"))?;
    
    let _trash = params["trash"].as_bool().unwrap_or(false);

    if let Some(session) = app.remote_sessions.get(server_name) {
        match session.filesystem.delete_file(file_path).await {
            Ok(_) => Ok(json!({
                "success": true
            })),
            Err(e) => Ok(json!({
                "success": false,
                "message": format!("Failed to delete file: {}", e)
            }))
        }
    } else {
        Ok(json!({
            "success": false,
            "message": format!("Server '{}' not found or not connected", server_name)
        }))
    }
}

/// LSP server management for remote development
pub async fn handle_remote_start_lsp_server(
    app: &mut Application,
    params: Value,
) -> Result<Value, ResponseError> {
    let server_name = params["server_name"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing server_name"))?;
    
    let language_id = params["language_id"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing language_id"))?;
    
    let workspace_root = params["workspace_root"]
        .as_str()
        .unwrap_or("/");

    if let Some(session) = app.remote_sessions.get(server_name) {
        match session.lsp_proxy.start_lsp_server(language_id, workspace_root).await {
            Ok(lsp_server_id) => {
                info!("Started {} LSP server on {}: {}", language_id, server_name, lsp_server_id);
                Ok(json!({
                    "success": true,
                    "lsp_server_id": lsp_server_id,
                    "language_id": language_id,
                    "workspace_root": workspace_root
                }))
            },
            Err(e) => Ok(json!({
                "success": false,
                "message": format!("Failed to start LSP server: {}", e)
            }))
        }
    } else {
        Ok(json!({
            "success": false,
            "message": format!("Server '{}' not found or not connected", server_name)
        }))
    }
}

pub async fn handle_remote_lsp_request(
    app: &mut Application,
    params: Value,
) -> Result<Value, ResponseError> {
    let server_name = params["server_name"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing server_name"))?;
    
    let lsp_server_id = params["lsp_server_id"]
        .as_u64()
        .ok_or_else(|| ResponseError::invalid_params("Missing lsp_server_id"))? as u32;
    
    let method = params["method"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing method"))?;
    
    let lsp_params = params["params"].clone();

    if let Some(session) = app.remote_sessions.get(server_name) {
        match session.lsp_proxy.send_request(lsp_server_id, method, lsp_params).await {
            Ok(result) => Ok(json!({
                "success": true,
                "result": result
            })),
            Err(e) => Ok(json!({
                "success": false,
                "message": format!("LSP request failed: {}", e)
            }))
        }
    } else {
        Ok(json!({
            "success": false,
            "message": format!("Server '{}' not found or not connected", server_name)
        }))
    }
}

pub async fn handle_remote_lsp_notification(
    app: &mut Application,
    params: Value,
) -> Result<Value, ResponseError> {
    let server_name = params["server_name"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing server_name"))?;
    
    let lsp_server_id = params["lsp_server_id"]
        .as_u64()
        .ok_or_else(|| ResponseError::invalid_params("Missing lsp_server_id"))? as u32;
    
    let method = params["method"]
        .as_str()
        .ok_or_else(|| ResponseError::invalid_params("Missing method"))?;
    
    let lsp_params = params["params"].clone();

    if let Some(session) = app.remote_sessions.get(server_name) {
        match session.lsp_proxy.send_notification(lsp_server_id, method, lsp_params).await {
            Ok(_) => Ok(json!({
                "success": true
            })),
            Err(e) => Ok(json!({
                "success": false,
                "message": format!("LSP notification failed: {}", e)
            }))
        }
    } else {
        Ok(json!({
            "success": false,
            "message": format!("Server '{}' not found or not connected", server_name)
        }))
    }
}

/// File system information for TRAMP integration
#[derive(Debug, Clone)]
pub struct RemoteFileInfo {
    pub file_type: String,  // "file", "directory", "symlink"
    pub size: u64,
    pub modified_time: u64,
    pub permissions: String,
}

#[derive(Debug, Clone)]
pub struct RemoteFileAttributes {
    pub is_directory: bool,
    pub links: u32,
    pub uid: u32,
    pub gid: u32,
    pub access_time: u64,
    pub modified_time: u64,
    pub change_time: u64,
    pub size: u64,
    pub mode: String,
    pub inode: u64,
    pub device: u64,
}

// Extend the RemoteFileSystem trait with new methods for TRAMP
use crate::remote::filesystem::RemoteFileSystem;
use async_trait::async_trait;

#[async_trait]
pub trait TrampFileSystem: RemoteFileSystem {
    async fn get_file_info(&self, path: &str) -> Result<RemoteFileInfo>;
    async fn get_file_attributes(&self, path: &str) -> Result<RemoteFileAttributes>;
    async fn list_directory(&self, path: &str, full_path: bool, pattern: Option<&str>) -> Result<Vec<String>>;
    async fn create_directory(&self, path: &str, parents: bool) -> Result<()>;
    async fn delete_file(&self, path: &str) -> Result<()>;
}

// Extend the RemoteLspProxy trait with server management
use crate::remote::lsp::RemoteLspProxy;

#[async_trait]
pub trait TrampLspProxy: RemoteLspProxy {
    async fn start_lsp_server(&self, language_id: &str, workspace_root: &str) -> Result<u32>;
    async fn send_request(&self, server_id: u32, method: &str, params: Value) -> Result<Value>;
    async fn send_notification(&self, server_id: u32, method: &str, params: Value) -> Result<()>;
    async fn stop_lsp_server(&self, server_id: u32) -> Result<()>;
    async fn list_lsp_servers(&self) -> Result<Vec<LspServerInfo>>;
}

#[derive(Debug, Clone)]
pub struct LspServerInfo {
    pub server_id: u32,
    pub language_id: String,
    pub workspace_root: String,
    pub status: String,  // "starting", "running", "stopped", "error"
}