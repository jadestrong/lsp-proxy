//! Remote development request handlers
//!
//! This module contains handlers for remote development related LSP requests
//! that are specific to lsp-proxy.

use crate::{
    application::Application,
    lsp::jsonrpc,
    lsp_ext,
    msg::Response,
    remote::{config::RemoteConfigManager, RemoteServerConfig, RemoteSession},
    utils::show_message,
};
use anyhow::Result;
use log::{debug, error, info, warn};
use lsp_types::MessageType;
use serde_json::{json, Value};
use std::{path::PathBuf, sync::Arc};

/// Create a JSON-RPC error
fn create_jsonrpc_error(code: i64, message: String) -> jsonrpc::Error {
    let error_code = match code {
        -32700 => jsonrpc::ErrorCode::ParseError,
        -32600 => jsonrpc::ErrorCode::InvalidRequest,
        -32601 => jsonrpc::ErrorCode::MethodNotFound,
        -32602 => jsonrpc::ErrorCode::InvalidParams,
        -32603 => jsonrpc::ErrorCode::InternalError,
        -32800 => jsonrpc::ErrorCode::RequestCanceled,
        _ => jsonrpc::ErrorCode::ServerError(code),
    };

    jsonrpc::Error {
        code: error_code,
        message,
        data: None,
    }
}

/// Handle remote server list request
pub fn handle_remote_list(
    app: &mut Application,
    req: &crate::msg::Request,
) -> Result<serde_json::Value> {
    let sessions = app.list_remote_sessions();
    let config_guard = app.remote_config.lock().unwrap();
    let available_servers = config_guard.list_servers();

    let server_info: Vec<Value> = available_servers
        .iter()
        .map(|server_name| {
            let connected = sessions.contains(&server_name.to_string());
            let config = config_guard.get_server(server_name);

            json!({
                "name": server_name,
                "connected": connected,
                "host": config.map(|c| &c.host).unwrap_or(&"unknown".to_string()),
                "user": config.map(|c| &c.user).unwrap_or(&"unknown".to_string()),
                "mode": config.map(|c| format!("{:?}", c.mode)).unwrap_or("unknown".to_string())
            })
        })
        .collect();

    Ok(json!({
        "servers": server_info,
        "connected_count": sessions.len(),
        "total_count": available_servers.len()
    }))
}

/// Handle remote server status request
pub fn handle_remote_status(
    app: &mut Application,
    req: &crate::msg::Request,
) -> Result<serde_json::Value> {
    let params: lsp_ext::RemoteStatusParams = if req.params.params.is_null() {
        return Err(anyhow::anyhow!("Missing status parameters"));
    } else {
        serde_json::from_value(req.params.params.clone())?
    };

    let server_name = params.server_name;
    let connected = app.is_remote_session_connected(&server_name);

    let config_guard = app.remote_config.lock().unwrap();
    let server_config = config_guard.get_server(&server_name);

    if let Some(config) = server_config {
        Ok(json!({
            "name": server_name,
            "connected": connected,
            "host": config.host,
            "user": config.user,
            "port": config.port,
            "mode": format!("{:?}", config.mode),
            "workspace_root": config.workspace_root
        }))
    } else {
        Err(anyhow::anyhow!(
            "Server '{}' not found in configuration",
            server_name
        ))
    }
}

/// Handle remote file read request
pub fn handle_remote_file_read(
    app: &mut Application,
    req: &crate::msg::Request,
) -> Result<serde_json::Value> {
    let params: lsp_ext::RemoteFileReadParams = if req.params.params.is_null() {
        return Err(anyhow::anyhow!("Missing file read parameters"));
    } else {
        serde_json::from_value(req.params.params.clone())?
    };

    let server_name = params.server_name;
    let file_path = PathBuf::from(params.file_path);

    // Get remote session
    let session = match app.get_remote_session(&server_name) {
        Some(session) => session,
        None => {
            return Err(anyhow::anyhow!(
                "Remote server '{}' not connected",
                server_name
            ));
        }
    };

    let sender = app.sender.clone();
    let response_id = req.id.clone();

    tokio::spawn(async move {
        match session.filesystem.read_file(&file_path).await {
            Ok(content) => {
                let content_str = String::from_utf8_lossy(&content);

                let success_response = Response {
                    id: response_id,
                    result: Some(json!({
                        "success": true,
                        "content": content_str,
                        "file_path": file_path.to_string_lossy(),
                        "size": content.len()
                    })),
                    error: None,
                };

                let _ = sender.send(success_response.into());
            }
            Err(err) => {
                let error_response = Response {
                    id: response_id,
                    result: None,
                    error: Some(create_jsonrpc_error(
                        -32603,
                        format!("Failed to read file: {}", err),
                    )),
                };

                let _ = sender.send(error_response.into());
            }
        }
    });

    // Return immediately for async operation
    Ok(json!({"async": true}))
}

/// Handle remote file write request
pub fn handle_remote_file_write(
    app: &mut Application,
    req: &crate::msg::Request,
) -> Result<serde_json::Value> {
    let params: lsp_ext::RemoteFileWriteParams = if req.params.params.is_null() {
        return Err(anyhow::anyhow!("Missing file write parameters"));
    } else {
        serde_json::from_value(req.params.params.clone())?
    };

    let server_name = params.server_name;
    let file_path = PathBuf::from(params.file_path);
    let content = params.content;

    // Get remote session
    let session = match app.get_remote_session(&server_name) {
        Some(session) => session,
        None => {
            return Err(anyhow::anyhow!(
                "Remote server '{}' not connected",
                server_name
            ));
        }
    };

    let sender = app.sender.clone();
    let response_id = req.id.clone();

    tokio::spawn(async move {
        match session
            .filesystem
            .write_file(&file_path, content.as_bytes())
            .await
        {
            Ok(_) => {
                let success_response = Response {
                    id: response_id,
                    result: Some(json!({
                        "success": true,
                        "file_path": file_path.to_string_lossy(),
                        "size": content.len()
                    })),
                    error: None,
                };

                let _ = sender.send(success_response.into());
            }
            Err(err) => {
                let error_response = Response {
                    id: response_id,
                    result: None,
                    error: Some(create_jsonrpc_error(
                        -32603,
                        format!("Failed to write file: {}", err),
                    )),
                };

                let _ = sender.send(error_response.into());
            }
        }
    });

    // Return immediately for async operation
    Ok(json!({"async": true}))
}

/// Handle remote LSP request forwarding
pub fn handle_remote_lsp_request(
    app: &mut Application,
    req: &crate::msg::Request,
) -> Result<serde_json::Value> {
    let params: lsp_ext::RemoteLspRequestParams = if req.params.params.is_null() {
        return Err(anyhow::anyhow!("Missing LSP request parameters"));
    } else {
        serde_json::from_value(req.params.params.clone())?
    };

    let server_name = params.server_name;
    let lsp_server_id = params.lsp_server_id;
    let method = params.method;
    let lsp_params = params.params;

    // Get remote session
    let session = match app.get_remote_session(&server_name) {
        Some(session) => session,
        None => {
            return Err(anyhow::anyhow!(
                "Remote server '{}' not connected",
                server_name
            ));
        }
    };

    let sender = app.sender.clone();
    let response_id = req.id.clone();

    tokio::spawn(async move {
        match session
            .lsp_proxy
            .send_request(lsp_server_id, &method, lsp_params)
            .await
        {
            Ok(result) => {
                let success_response = Response {
                    id: response_id,
                    result: Some(result),
                    error: None,
                };

                let _ = sender.send(success_response.into());
            }
            Err(err) => {
                let error_response = Response {
                    id: response_id,
                    result: None,
                    error: Some(create_jsonrpc_error(
                        -32603,
                        format!("LSP request failed: {}", err),
                    )),
                };

                let _ = sender.send(error_response.into());
            }
        }
    });

    // Return immediately for async operation
    Ok(json!({"async": true}))
}

/// Create a remote session from configuration
async fn create_remote_session(config: RemoteServerConfig) -> Result<RemoteSession> {
    debug!("Creating remote session for server: {}", config.name);
    RemoteSession::new(config).await
}

/// Handle remote server connection request
pub async fn handle_remote_connect(
    remote_config: Arc<std::sync::Mutex<RemoteConfigManager>>,
    remote_sessions: Arc<std::sync::Mutex<std::collections::HashMap<String, RemoteSession>>>,
    req: &crate::msg::Request,
) -> Result<Response> {
    let params: lsp_ext::RemoteConnectParams = if req.params.params.is_null() {
        return Err(anyhow::anyhow!("Missing connect parameters"));
    } else {
        serde_json::from_value(req.params.params.clone())?
    };

    let server_name = params.server_name.clone();

    // Check if already connected - 限制锁的作用域
    {
        let sessions_guard = remote_sessions.lock().unwrap();
        if sessions_guard.contains_key(&server_name) {
            return Ok(Response {
                id: req.id.clone(),
                result: Some(json!({
                    "success": true,
                    "message": format!("Already connected to server '{}'", server_name),
                    "server_name": server_name
                })),
                error: None,
            });
        }
    } // 锁在这里释放

    // Get server configuration - 限制锁的作用域，并克隆配置
    let config = {
        let config_guard = remote_config.lock().unwrap();
        let server_config = config_guard.get_server(&server_name);

        if server_config.is_none() {
            return Err(anyhow::anyhow!(
                "Server '{}' not found in configuration",
                server_name
            ));
        }

        let mut config = server_config.unwrap().clone();

        // Override config with provided parameters if any
        if let Some(host) = params.host {
            config.host = host;
        }
        if let Some(user) = params.user {
            config.user = user;
        }
        if let Some(port) = params.port {
            config.port = Some(port);
        }

        config
    }; // 锁在这里释放

    info!("Attempting to connect to remote server: {}", server_name);

    let server_name_clone = server_name.clone();

    match RemoteSession::new(config).await {
        Ok(session) => {
            info!(
                "Successfully connected to remote server: {}",
                server_name_clone
            );

            // Add session to application's remote sessions - 限制锁的作用域
            {
                if let Ok(mut sessions) = remote_sessions.lock() {
                    sessions.insert(server_name_clone.clone(), session);
                    info!(
                        "Remote session '{}' added to application",
                        server_name_clone
                    );
                }
            } // 锁在这里释放
            
            Ok(Response {
                id: req.id.clone(),
                result: Some(json!({
                    "success": true,
                    "message": format!("Successfully connected to server '{}'", server_name_clone),
                    "server_name": server_name_clone
                })),
                error: None,
            })
        }
        Err(err) => {
            error!(
                "Failed to connect to remote server '{}': {}",
                server_name_clone, err
            );
            
            Ok(Response {
                id: req.id.clone(),
                result: Some(json!({
                    "success": false,
                    "message": format!("Failed to connect to server '{}': {}", server_name_clone, err),
                    "server_name": server_name_clone
                })),
                error: None,
            })
        }
    }
}

/// Handle remote server disconnection request
pub fn handle_remote_disconnect(
    app: &mut Application,
    req: &crate::msg::Request,
) -> Result<serde_json::Value> {
    let params: lsp_ext::RemoteDisconnectParams = if req.params.params.is_null() {
        return Err(anyhow::anyhow!("Missing disconnect parameters"));
    } else {
        serde_json::from_value(req.params.params.clone())?
    };

    let server_name = params.server_name.clone();

    // Check if connected
    if !app.is_remote_session_connected(&server_name) {
        return Ok(json!({
            "success": true,
            "message": format!("Server '{}' is not connected", server_name),
            "server_name": server_name
        }));
    }

    // Remove the session
    app.remove_remote_session(&server_name);

    Ok(json!({
        "success": true,
        "message": format!("Successfully disconnected from server '{}'", server_name),
        "server_name": server_name
    }))
}
/// Handle remote workspace management
pub fn handle_remote_workspace(
    app: &mut Application,
    req: &crate::msg::Request,
) -> Result<serde_json::Value> {
    // List all connected sessions and their workspace info
    let sessions = app.list_remote_sessions();
    let config_guard = app.remote_config.lock().unwrap();

    let workspace_info: Vec<Value> = sessions
        .iter()
        .filter_map(|server_name| {
            let config = config_guard.get_server(server_name)?;
            Some(json!({
                "server_name": server_name,
                "workspace_root": config.workspace_root,
                "host": config.host,
                "connected": true
            }))
        })
        .collect();

    Ok(json!({
        "workspaces": workspace_info,
        "total_count": workspace_info.len()
    }))
}

/// Check if a method result can be cached
fn is_cacheable_method(method: &str) -> bool {
    matches!(
        method,
        "textDocument/hover"
            | "textDocument/definition"
            | "textDocument/references"
            | "textDocument/documentSymbol"
            | "workspace/symbol"
    )
}
