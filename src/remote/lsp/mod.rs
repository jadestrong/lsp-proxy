//! Remote LSP proxy functionality
//! 
//! Handles LSP communication with remote language servers, including
//! request optimization, caching, and batching.

use async_trait::async_trait;
use anyhow::Result;
use std::sync::Arc;
use std::collections::HashMap;
use serde_json::Value;
use tokio::sync::{RwLock, Mutex};
use std::time::{SystemTime, Duration};

use crate::remote::connection::Connection;
use crate::msg::RequestId;

/// Remote LSP proxy interface
#[async_trait]
pub trait RemoteLspProxy: Send + Sync {
    /// Start a remote language server
    async fn start_language_server(&self, server_name: &str, cmd: &str, args: &[String]) -> Result<u32>;
    
    /// Stop a remote language server
    async fn stop_language_server(&self, server_id: u32) -> Result<()>;
    
    /// Send LSP request to remote server
    async fn send_request(&self, server_id: u32, method: &str, params: Value) -> Result<Value>;
    
    /// Send LSP notification to remote server
    async fn send_notification(&self, server_id: u32, method: &str, params: Value) -> Result<()>;
    
    /// Subscribe to server notifications
    async fn subscribe_notifications(&self, callback: Box<dyn Fn(u32, &str, Value) + Send + Sync>) -> Result<()>;
}

/// Cache entry for LSP responses
#[derive(Debug, Clone)]
struct CacheEntry {
    response: Value,
    timestamp: SystemTime,
    ttl: Duration,
}

/// Request batching entry
#[derive(Debug)]
struct BatchEntry {
    request_id: RequestId,
    method: String,
    params: Value,
    timestamp: SystemTime,
}

/// Remote LSP proxy implementation
pub struct RemoteLspProxyImpl {
    connection: Arc<dyn Connection>,
    servers: Arc<RwLock<HashMap<u32, RemoteServer>>>,
    next_server_id: Arc<Mutex<u32>>,
    response_cache: Arc<RwLock<HashMap<String, CacheEntry>>>,
    request_batch: Arc<Mutex<Vec<BatchEntry>>>,
    batch_timeout: Duration,
}

/// Remote server instance
#[derive(Debug)]
struct RemoteServer {
    id: u32,
    name: String,
    pid: u32,
    command: String,
    args: Vec<String>,
    active: bool,
}

impl RemoteLspProxyImpl {
    /// Create a new remote LSP proxy
    pub fn new(connection: Arc<dyn Connection>) -> Self {
        Self {
            connection,
            servers: Arc::new(RwLock::new(HashMap::new())),
            next_server_id: Arc::new(Mutex::new(1)),
            response_cache: Arc::new(RwLock::new(HashMap::new())),
            request_batch: Arc::new(Mutex::new(Vec::new())),
            batch_timeout: Duration::from_millis(50), // 50ms batch window
        }
    }
    
    /// Generate cache key for request
    fn cache_key(&self, server_id: u32, method: &str, params: &Value) -> String {
        format!("{}:{}:{}", server_id, method, serde_json::to_string(params).unwrap_or_default())
    }
    
    /// Check if method should be cached
    fn should_cache(&self, method: &str) -> bool {
        matches!(method,
            "textDocument/hover" |
            "textDocument/signatureHelp" |
            "textDocument/documentHighlight" |
            "textDocument/documentSymbol"
        )
    }
    
    /// Get TTL for method
    fn get_ttl(&self, method: &str) -> Duration {
        match method {
            "textDocument/hover" => Duration::from_secs(30),
            "textDocument/signatureHelp" => Duration::from_secs(10),
            "textDocument/documentHighlight" => Duration::from_secs(5),
            "textDocument/documentSymbol" => Duration::from_secs(60),
            _ => Duration::from_secs(30),
        }
    }
    
    /// Execute batched requests
    async fn flush_batch(&self) -> Result<()> {
        let mut batch = self.request_batch.lock().await;
        if batch.is_empty() {
            return Ok(());
        }
        
        let requests = std::mem::take(&mut *batch);
        drop(batch);
        
        // Group requests by server
        let mut server_requests: HashMap<u32, Vec<BatchEntry>> = HashMap::new();
        for request in requests {
            // Extract server_id from request (would need proper parsing)
            let server_id = 1u32; // Simplified for now
            server_requests.entry(server_id).or_default().push(request);
        }
        
        // Send batched requests to each server
        for (server_id, requests) in server_requests {
            self.send_batch_to_server(server_id, requests).await?;
        }
        
        Ok(())
    }
    
    /// Send batch of requests to specific server
    async fn send_batch_to_server(&self, server_id: u32, requests: Vec<BatchEntry>) -> Result<()> {
        if requests.is_empty() {
            return Ok(());
        }
        
        // For single request, send directly
        if requests.len() == 1 {
            let req = &requests[0];
            self.send_direct_request(server_id, &req.method, &req.params).await?;
            return Ok(());
        }
        
        // For multiple requests, create batch request
        let batch_requests: Vec<Value> = requests
            .iter()
            .enumerate()
            .map(|(i, req)| {
                serde_json::json!({
                    "jsonrpc": "2.0",
                    "id": i,
                    "method": req.method,
                    "params": req.params
                })
            })
            .collect();
        
        // Send batch via SSH
        let batch_json = serde_json::to_string(&batch_requests)?;
        let script = format!(
            r#"
            echo '{}' | lsp-proxy-batch-handler {}
            "#,
            batch_json, server_id
        );
        
        self.connection.execute_command("bash", &["-c", &script]).await?;
        
        Ok(())
    }
    
    /// Send direct request to server
    async fn send_direct_request(&self, server_id: u32, method: &str, params: &Value) -> Result<Value> {
        let request_json = serde_json::json!({
            "jsonrpc": "2.0",
            "id": 1,
            "method": method,
            "params": params
        });
        
        let script = format!(
            r#"
            echo '{}' | lsp-proxy-request-handler {}
            "#,
            serde_json::to_string(&request_json)?, server_id
        );
        
        let result = self.connection.execute_command("bash", &["-c", &script]).await?;
        
        if result.exit_code != 0 {
            return Err(anyhow::anyhow!("LSP request failed: {}", result.stderr));
        }
        
        let response: Value = serde_json::from_str(&result.stdout)?;
        Ok(response)
    }
}

#[async_trait]
impl RemoteLspProxy for RemoteLspProxyImpl {
    async fn start_language_server(&self, server_name: &str, cmd: &str, args: &[String]) -> Result<u32> {
        let server_id = {
            let mut next_id = self.next_server_id.lock().await;
            let id = *next_id;
            *next_id += 1;
            id
        };
        
        // Start language server on remote machine
        let args_str = args.join(" ");
        let start_script = format!(
            r#"
            # Start language server and capture PID
            nohup {} {} > /tmp/lsp-{}.log 2>&1 &
            PID=$!
            echo "Server started with PID: $PID"
            echo $PID > /tmp/lsp-{}.pid
            "#,
            cmd, args_str, server_id, server_id
        );
        
        let result = self.connection.execute_command("bash", &["-c", &start_script]).await?;
        
        if result.exit_code != 0 {
            return Err(anyhow::anyhow!("Failed to start language server: {}", result.stderr));
        }
        
        // Parse PID from output
        let pid = result.stdout
            .lines()
            .find(|line| line.starts_with("Server started with PID:"))
            .and_then(|line| line.split_whitespace().last())
            .and_then(|pid_str| pid_str.parse::<u32>().ok())
            .unwrap_or(0);
        
        let server = RemoteServer {
            id: server_id,
            name: server_name.to_string(),
            pid,
            command: cmd.to_string(),
            args: args.to_vec(),
            active: true,
        };
        
        {
            let mut servers = self.servers.write().await;
            servers.insert(server_id, server);
        }
        
        log::info!("Started remote language server '{}' with ID {} (PID: {})", server_name, server_id, pid);
        Ok(server_id)
    }
    
    async fn stop_language_server(&self, server_id: u32) -> Result<()> {
        let server = {
            let mut servers = self.servers.write().await;
            servers.remove(&server_id)
        };
        
        if let Some(server) = server {
            // Kill the remote process
            let kill_script = format!("kill {} && rm -f /tmp/lsp-{}.pid", server.pid, server_id);
            let _result = self.connection.execute_command("bash", &["-c", &kill_script]).await?;
            
            log::info!("Stopped remote language server '{}' (ID: {})", server.name, server_id);
        }
        
        Ok(())
    }
    
    async fn send_request(&self, server_id: u32, method: &str, params: Value) -> Result<Value> {
        // Check cache first
        if self.should_cache(method) {
            let cache_key = self.cache_key(server_id, method, &params);
            let cache = self.response_cache.read().await;
            
            if let Some(entry) = cache.get(&cache_key) {
                if entry.timestamp.elapsed().unwrap_or(Duration::MAX) < entry.ttl {
                    return Ok(entry.response.clone());
                }
            }
        }
        
        // Send request directly or add to batch
        let response = if method == "textDocument/completion" {
            // Completion requests should be sent immediately
            self.send_direct_request(server_id, method, &params).await?
        } else {
            // Add to batch for other requests
            let batch_len = {
                let batch = self.request_batch.lock().await;
                batch.len()
            };
            
            let mut batch = self.request_batch.lock().await;
            batch.push(BatchEntry {
                request_id: RequestId::from(format!("batch_{}", batch_len)),
                method: method.to_string(),
                params: params.clone(),
                timestamp: SystemTime::now(),
            });
            drop(batch);
            
            // Send direct for now (batching would need more complex response handling)
            self.send_direct_request(server_id, method, &params).await?
        };
        
        // Cache response if applicable
        if self.should_cache(method) {
            let cache_key = self.cache_key(server_id, method, &params);
            let entry = CacheEntry {
                response: response.clone(),
                timestamp: SystemTime::now(),
                ttl: self.get_ttl(method),
            };
            
            let mut cache = self.response_cache.write().await;
            cache.insert(cache_key, entry);
        }
        
        Ok(response)
    }
    
    async fn send_notification(&self, server_id: u32, method: &str, params: Value) -> Result<()> {
        let notification_json = serde_json::json!({
            "jsonrpc": "2.0",
            "method": method,
            "params": params
        });
        
        let script = format!(
            r#"
            echo '{}' | lsp-proxy-notify-handler {}
            "#,
            serde_json::to_string(&notification_json)?, server_id
        );
        
        let result = self.connection.execute_command("bash", &["-c", &script]).await?;
        
        if result.exit_code != 0 {
            log::warn!("LSP notification failed: {}", result.stderr);
        }
        
        Ok(())
    }
    
    async fn subscribe_notifications(&self, _callback: Box<dyn Fn(u32, &str, Value) + Send + Sync>) -> Result<()> {
        // TODO: Implement notification subscription
        // This would involve setting up a background task to monitor
        // server notifications and call the provided callback
        Ok(())
    }
}

/// Create a remote LSP proxy
pub async fn create_lsp_proxy(connection: Arc<dyn Connection>) -> Result<Arc<dyn RemoteLspProxy>> {
    Ok(Arc::new(RemoteLspProxyImpl::new(connection)))
}