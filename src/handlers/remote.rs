//! Remote development request handlers
//! 
//! This module contains handlers for remote development related LSP requests
//! that are specific to lsp-proxy.

use crate::{
    application::Application,
    lsp::jsonrpc,
    lsp_ext,
    msg::Response,
    remote::{RemoteSession, RemoteServerConfig},
    utils::show_message,
};
use anyhow::Result;
use log::{debug, error, info, warn};
use lsp_types::MessageType;
use serde_json::{json, Value};
use std::path::PathBuf;

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
    mut response: Response,
) -> Result<()> {
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

    response.result = Some(json!({
        "servers": server_info,
        "connected_count": sessions.len(),
        "total_count": available_servers.len()
    }));

    app.respond(response);
    Ok(())
}

/// Handle remote server status request
pub fn handle_remote_status(
    app: &mut Application,
    mut response: Response,
) -> Result<()> {
    let params: lsp_ext::RemoteStatusParams = match response.result.take() {
        Some(params) => serde_json::from_value(params)?,
        None => {
            response.error = Some(create_jsonrpc_error(-32602, "Missing status parameters".to_string()));
            app.respond(response);
            return Ok(());
        }
    };

    let server_name = params.server_name;
    let connected = app.is_remote_session_connected(&server_name);
    
    let config_guard = app.remote_config.lock().unwrap();
    let server_config = config_guard.get_server(&server_name);

    if let Some(config) = server_config {
        response.result = Some(json!({
            "name": server_name,
            "connected": connected,
            "host": config.host,
            "user": config.user,
            "port": config.port,
            "mode": format!("{:?}", config.mode),
            "workspace_root": config.workspace_root
        }));
    } else {
        response.error = Some(create_jsonrpc_error(
            -32602,
            format!("Server '{}' not found in configuration", server_name)
        ));
    }

    app.respond(response);
    Ok(())
}

/// Handle remote file read request
pub fn handle_remote_file_read(
    app: &mut Application,
    mut response: Response,
) -> Result<()> {
    let params: lsp_ext::RemoteFileReadParams = match response.result.take() {
        Some(params) => serde_json::from_value(params)?,
        None => {
            response.error = Some(create_jsonrpc_error(-32602, "Missing file read parameters".to_string()));
            app.respond(response);
            return Ok(());
        }
    };

    let server_name = params.server_name;
    let file_path = PathBuf::from(params.file_path);

    // Get remote session
    let session = match app.get_remote_session(&server_name) {
        Some(session) => session,
        None => {
            response.error = Some(create_jsonrpc_error(
                -32602,
                format!("Remote server '{}' not connected", server_name)
            ));
            app.respond(response);
            return Ok(());
        }
    };

    let sender = app.sender.clone();
    let response_id = response.id.clone();

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
                        format!("Failed to read file: {}", err)
                    )),
                };
                
                let _ = sender.send(error_response.into());
            }
        }
    });

    Ok(())
}

/// Handle remote file write request
pub fn handle_remote_file_write(
    app: &mut Application,
    mut response: Response,
) -> Result<()> {
    let params: lsp_ext::RemoteFileWriteParams = match response.result.take() {
        Some(params) => serde_json::from_value(params)?,
        None => {
            response.error = Some(create_jsonrpc_error(-32602, "Missing file write parameters".to_string()));
            app.respond(response);
            return Ok(());
        }
    };

    let server_name = params.server_name;
    let file_path = PathBuf::from(params.file_path);
    let content = params.content;

    // Get remote session
    let session = match app.get_remote_session(&server_name) {
        Some(session) => session,
        None => {
            response.error = Some(create_jsonrpc_error(
                -32602,
                format!("Remote server '{}' not connected", server_name)
            ));
            app.respond(response);
            return Ok(());
        }
    };

    let sender = app.sender.clone();
    let response_id = response.id.clone();

    tokio::spawn(async move {
        match session.filesystem.write_file(&file_path, content.as_bytes()).await {
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
                        format!("Failed to write file: {}", err)
                    )),
                };
                
                let _ = sender.send(error_response.into());
            }
        }
    });

    Ok(())
}

/// Handle remote LSP request forwarding
pub fn handle_remote_lsp_request(
    app: &mut Application,
    mut response: Response,
) -> Result<()> {
    let params: lsp_ext::RemoteLspRequestParams = match response.result.take() {
        Some(params) => serde_json::from_value(params)?,
        None => {
            response.error = Some(create_jsonrpc_error(-32602, "Missing LSP request parameters".to_string()));
            app.respond(response);
            return Ok(());
        }
    };

    let server_name = params.server_name;
    let lsp_server_id = params.lsp_server_id;
    let method = params.method;
    let lsp_params = params.params;

    // Get remote session
    let session = match app.get_remote_session(&server_name) {
        Some(session) => session,
        None => {
            response.error = Some(create_jsonrpc_error(
                -32602,
                format!("Remote server '{}' not connected", server_name)
            ));
            app.respond(response);
            return Ok(());
        }
    };

    let sender = app.sender.clone();
    let response_id = response.id.clone();

    tokio::spawn(async move {
        match session.lsp_proxy.send_request(lsp_server_id, &method, lsp_params).await {
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
                        format!("LSP request failed: {}", err)
                    )),
                };
                
                let _ = sender.send(error_response.into());
            }
        }
    });

    Ok(())
}

/// Create a remote session from configuration
async fn create_remote_session(config: RemoteServerConfig) -> Result<RemoteSession> {
    debug!("Creating remote session for server: {}", config.name);
    RemoteSession::new(config).await
}

/// Handle remote server connection request
pub fn handle_remote_connect(
    app: &mut Application,
    mut response: Response,
) -> Result<()> {
    let params: lsp_ext::RemoteConnectParams = match response.result.take() {
        Some(params) => serde_json::from_value(params)?,
        None => {
            response.error = Some(create_jsonrpc_error(-32602, "Missing connect parameters".to_string()));
            app.respond(response);
            return Ok(());
        }
    };

    let server_name = params.server_name.clone();
    
    // Check if already connected
    if app.is_remote_session_connected(&server_name) {
        response.result = Some(json!({
            "success": true,
            "message": format!("Already connected to server '{}'", server_name),
            "server_name": server_name
        }));
        app.respond(response);
        return Ok(());
    }

    // Get server configuration
    let config_guard = app.remote_config.lock().unwrap();
    let server_config = config_guard.get_server(&server_name);
    
    if server_config.is_none() {
        response.error = Some(create_jsonrpc_error(
            -32602,
            format!("Server '{}' not found in configuration", server_name)
        ));
        app.respond(response);
        return Ok(());
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
    
    drop(config_guard); // Release the lock
    
    let sender = app.sender.clone();
    let response_id = response.id.clone();
    let remote_sessions = app.remote_sessions.clone();

    tokio::spawn(async move {
        match create_remote_session(config.clone()).await {
            Ok(session) => {
                // Add session to the application state
                {
                    let mut sessions = remote_sessions.lock().unwrap();
                    sessions.insert(server_name.clone(), session);
                }
                
                let success_response = Response {
                    id: response_id,
                    result: Some(json!({
                        "success": true,
                        "message": format!("Successfully connected to server '{}'", server_name),
                        "server_name": server_name,
                        "host": config.host,
                        "user": config.user,
                        "port": config.port
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
                        format!("Failed to connect to server '{}': {}", server_name, err)
                    )),
                };
                
                let _ = sender.send(error_response.into());
            }
        }
    });

    Ok(())
}

/// Handle remote server disconnection request
pub fn handle_remote_disconnect(
    app: &mut Application,
    mut response: Response,
) -> Result<()> {
    let params: lsp_ext::RemoteDisconnectParams = match response.result.take() {
        Some(params) => serde_json::from_value(params)?,
        None => {
            response.error = Some(create_jsonrpc_error(-32602, "Missing disconnect parameters".to_string()));
            app.respond(response);
            return Ok(());
        }
    };

    let server_name = params.server_name.clone();
    
    // Check if connected
    if !app.is_remote_session_connected(&server_name) {
        response.result = Some(json!({
            "success": true,
            "message": format!("Server '{}' is not connected", server_name),
            "server_name": server_name
        }));
        app.respond(response);
        return Ok(());
    }

    // Remove session from application state
    let removed_session = app.remove_remote_session(&server_name);
    
    if let Some(_session) = removed_session {
        response.result = Some(json!({
            "success": true,
            "message": format!("Successfully disconnected from server '{}'", server_name),
            "server_name": server_name
        }));
    } else {
        response.error = Some(create_jsonrpc_error(
            -32603,
            format!("Failed to disconnect from server '{}'", server_name)
        ));
    }

    app.respond(response);
    Ok(())
}

/// Handle remote workspace management
pub fn handle_remote_workspace(
    app: &mut Application,
    mut response: Response,
) -> Result<()> {
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

    response.result = Some(json!({
        "workspaces": workspace_info,
        "total_count": workspace_info.len()
    }));

    app.respond(response);
    Ok(())
}

/// Check if a method result can be cached
fn is_cacheable_method(method: &str) -> bool {
    matches!(method, 
        "textDocument/hover" |
        "textDocument/definition" |
        "textDocument/references" |
        "textDocument/documentSymbol" |
        "workspace/symbol"
    )
}