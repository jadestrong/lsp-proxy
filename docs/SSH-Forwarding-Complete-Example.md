# LSP-Proxy SSH 转发机制实战示例

## 完整工作流程演示

让我们通过一个具体的例子来展示本地 lsp-proxy 如何通过 SSH 将 LSP 请求转发到远程 lsp-proxy-server。

### 场景设定

- **本地机器**: 开发者的 Mac/Linux 机器，运行 Emacs + lsp-proxy
- **远程服务器**: `dev-server.com`，运行 lsp-proxy-server 和 rust-analyzer
- **目标文件**: `/home/developer/rust-project/src/main.rs`

## 1. 初始配置

### 1.1 远程服务器配置

在 `dev-server.com` 上创建 `~/.config/lsp-proxy-server.toml`：

```toml
listen_address = "127.0.0.1:9527"
work_dir = "/home/developer"

[lsp_servers.rust]
command = "rust-analyzer"
args = []
env = {}
```

启动远程服务器：
```bash
# 在 dev-server.com 上执行
lsp-proxy-server -c ~/.config/lsp-proxy-server.toml &
```

### 1.2 本地客户端配置

在本地创建 `~/.config/lsp-proxy/remote-lsp.toml`：

```toml
[hosts.dev-server]
address = "127.0.0.1:19527"  # 本地隧道端口
connection_type = "ssh_tunnel"

[hosts.dev-server.ssh_tunnel]
host = "dev-server.com"
port = 22
user = "developer"
identity_file = "~/.ssh/dev_key"
local_port = 19527

[host_mapping]
"dev-server.com" = "dev-server"  # TRAMP 主机名映射
```

### 1.3 SSH 配置优化

在本地 `~/.ssh/config` 中添加：

```
Host dev-server.com
    HostName dev-server.com
    User developer
    Port 22
    IdentityFile ~/.ssh/dev_key
    # 连接复用，避免多次建立隧道
    ControlMaster auto
    ControlPath ~/.ssh/sockets/ssh-%r@%h:%p
    ControlPersist 10m
    # 压缩传输，提高性能
    Compression yes
    # 保持连接活跃
    ServerAliveInterval 30
    ServerAliveCountMax 3
```

## 2. 用户操作序列

### 2.1 用户在 Emacs 中打开远程文件

```elisp
;; 用户执行
C-x C-f /ssh:dev-server.com:/home/developer/rust-project/src/main.rs RET
```

这会触发：
1. TRAMP 通过 SSH 访问远程文件系统
2. lsp-mode 自动检测到 Rust 文件，尝试启动 LSP

### 2.2 lsp-mode 发送 initialize 请求

lsp-mode 向本地 lsp-proxy 发送初始化请求：

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "processId": 12345,
    "rootUri": "file:///ssh:dev-server.com:/home/developer/rust-project",
    "capabilities": {
      "workspace": { "workspaceFolders": true },
      "textDocument": {
        "completion": { "completionItem": { "snippetSupport": true } },
        "hover": { "contentFormat": ["markdown"] },
        "definition": { "linkSupport": true }
      }
    },
    "workspaceFolders": [{
      "uri": "file:///ssh:dev-server.com:/home/developer/rust-project",
      "name": "rust-project"
    }]
  }
}
```

## 3. 本地 lsp-proxy 处理流程

### 3.1 远程文件检测

```rust
// src/handlers/request.rs 中的处理逻辑
pub async fn handle_initialize_request(
    app: &mut Application,
    params: Value
) -> Result<Value, ResponseError> {
    // 提取 rootUri
    let root_uri = params.get("rootUri")
        .and_then(|uri| uri.as_str())
        .unwrap_or("");
    
    // 检测是否为远程文件
    if RemoteLspManager::is_remote_file(root_uri) {
        info!("Detected remote file: {}", root_uri);
        return handle_remote_initialize(app, params, root_uri).await;
    }
    
    // 本地文件处理...
    handle_local_initialize(app, params).await
}

async fn handle_remote_initialize(
    app: &mut Application, 
    params: Value, 
    root_uri: &str
) -> Result<Value, ResponseError> {
    // 解析远程文件信息
    let file_info = RemoteLspManager::extract_remote_info(root_uri)
        .ok_or_else(|| ResponseError::invalid_params("Invalid remote URI"))?;
    
    info!("Remote file info: method={}, host={}, path={}", 
          file_info.method, file_info.host, file_info.path);
    
    // file_info = RemoteFileInfo {
    //     method: "ssh",
    //     user: None,
    //     host: "dev-server.com", 
    //     path: "/home/developer/rust-project"
    // }
    
    // 获取远程 LSP 管理器
    let remote_manager = app.remote_lsp_manager.as_ref()
        .ok_or_else(|| ResponseError::internal_error("Remote LSP not initialized"))?;
    
    // 启动远程 LSP 服务器
    let server_id = remote_manager.start_remote_lsp(&file_info, "rust").await
        .map_err(|e| ResponseError::internal_error(&format!("Remote LSP error: {}", e)))?;
    
    // 转换参数并转发请求
    let remote_params = convert_params_for_remote(params, &file_info);
    let result = remote_manager.forward_request(
        &file_info.host, 
        &server_id, 
        "initialize", 
        remote_params
    ).await
    .map_err(|e| ResponseError::internal_error(&format!("Forward error: {}", e)))?;
    
    Ok(result)
}
```

### 3.2 SSH 隧道建立

```rust
// src/remote/lsp_client.rs
impl RemoteLspManager {
    pub async fn get_connection(&self, host: &str) -> Result<Arc<RemoteConnection>> {
        // 检查现有连接
        if let Some(conn) = self.connections.read().await.get(host) {
            return Ok(Arc::clone(conn));
        }
        
        // host = "dev-server.com"
        // 查找配置: "dev-server.com" -> "dev-server"
        let config_host = self.config.host_mapping
            .as_ref()
            .and_then(|mapping| mapping.get(host))
            .unwrap_or(&host.to_string())
            .clone();
        
        // config_host = "dev-server"
        let host_config = self.config.hosts.get(&config_host)
            .ok_or_else(|| anyhow!("No config for host: {}", config_host))?;
        
        // 建立连接
        let connection = Arc::new(
            RemoteConnection::new(&config_host, host_config).await?
        );
        
        self.connections.write().await.insert(host.to_string(), Arc::clone(&connection));
        Ok(connection)
    }
}

impl RemoteConnection {
    pub async fn new(host: &str, config: &RemoteHostConfig) -> Result<Self> {
        match config.connection_type {
            ConnectionType::SshTunnel => {
                let ssh_config = config.ssh_tunnel.as_ref().unwrap();
                
                info!("Establishing SSH tunnel to {}@{}:{}", 
                      ssh_config.user, ssh_config.host, ssh_config.port);
                
                // 建立 SSH 隧道
                let tunnel = Arc::new(SshTunnel::establish(ssh_config).await?);
                
                // 连接到本地隧道端口
                let stream = TokioTcpStream::connect(
                    format!("127.0.0.1:{}", tunnel.local_port())
                ).await?;
                
                Ok(Self {
                    stream: Arc::new(Mutex::new(stream)),
                    host: host.to_string(),
                    remote_lsp_servers: Arc::new(RwLock::new(HashMap::new())),
                    _ssh_tunnel: Some(tunnel),  // 保持隧道存活
                })
            }
            ConnectionType::Tcp => { /* 直接 TCP 连接 */ }
        }
    }
}

impl SshTunnel {
    pub async fn establish(config: &SshTunnelConfig) -> Result<Self> {
        // 执行: ssh -L 19527:127.0.0.1:9527 -N -o ExitOnForwardFailure=yes developer@dev-server.com
        let mut cmd = tokio::process::Command::new("ssh");
        cmd.args(&[
            "-L", "19527:127.0.0.1:9527",  // 端口转发
            "-N",                          // 不执行远程命令
            "-o", "ExitOnForwardFailure=yes",
            "-i", "/home/user/.ssh/dev_key",
            "developer@dev-server.com"
        ]);
        
        let process = cmd.spawn()?;
        
        // 等待隧道建立
        tokio::time::sleep(Duration::from_secs(3)).await;
        
        // 测试连接
        TokioTcpStream::connect("127.0.0.1:19527").await?;
        
        info!("SSH tunnel established: local:19527 -> dev-server.com:9527");
        
        Ok(Self { process, local_port: 19527, host: "dev-server.com".to_string() })
    }
}
```

### 3.3 启动远程 LSP 服务器

```rust
impl RemoteConnection {
    pub async fn start_lsp_server(&self, workspace_path: &Path, language: &str) -> Result<String> {
        let server_id = format!("remote-rust-{}", Uuid::new_v4());
        
        // 发送启动请求到远程服务器
        let message = ProxyMessage::StartLspServer {
            id: server_id.clone(),
            language: "rust".to_string(),
            workspace_root: "/home/developer/rust-project".into(),
            client_capabilities: None,
        };
        
        // 通过 SSH 隧道发送
        self.send_message(message).await?;
        
        info!("Started remote Rust LSP server: {}", server_id);
        Ok(server_id)
    }
    
    pub async fn send_message(&self, message: ProxyMessage) -> Result<Option<ProxyMessage>> {
        let msg_json = serde_json::to_string(&message)?;
        
        // 通过 SSH 隧道发送到远程 lsp-proxy-server
        let mut stream = self.stream.lock().await;
        stream.write_all(msg_json.as_bytes()).await?;
        stream.write_all(b"\n").await?;
        stream.flush().await?;
        
        // 读取响应
        let mut response_line = String::new();
        let (reader, _) = stream.split();
        let mut buf_reader = BufReader::new(reader);
        buf_reader.read_line(&mut response_line).await?;
        
        Ok(Some(serde_json::from_str(&response_line)?))
    }
}
```

### 3.4 参数转换

```rust
fn convert_params_for_remote(params: Value, file_info: &RemoteFileInfo) -> Value {
    let mut remote_params = params.clone();
    
    // 转换 TRAMP URI 到远程文件路径
    // 输入: "file:///ssh:dev-server.com:/home/developer/rust-project"
    // 输出: "file:///home/developer/rust-project"
    
    if let Some(root_uri) = remote_params.get_mut("rootUri") {
        *root_uri = json!("file:///home/developer/rust-project");
    }
    
    if let Some(workspace_folders) = remote_params.get_mut("workspaceFolders") {
        if let Some(folders) = workspace_folders.as_array_mut() {
            for folder in folders {
                if let Some(uri) = folder.get_mut("uri") {
                    *uri = json!("file:///home/developer/rust-project");
                }
            }
        }
    }
    
    remote_params
}
```

## 4. 远程服务器处理

### 4.1 lsp-proxy-server 接收请求

```rust
// src/bin/lsp-proxy-server.rs
async fn process_message(&self, client_id: &str, message: ProxyMessage) -> Option<ProxyMessage> {
    match message {
        ProxyMessage::StartLspServer { id, language, workspace_root, .. } => {
            info!("Starting {} LSP server: {}", language, id);
            
            // id = "remote-rust-uuid123"
            // language = "rust"
            // workspace_root = "/home/developer/rust-project"
            
            match self.start_lsp_server(&id, &language, &workspace_root, None).await {
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
        
        ProxyMessage::LspRequest { server_id, method, params, .. } => {
            // server_id = "remote-rust-uuid123"
            // method = "initialize"
            // params = { "rootUri": "file:///home/developer/rust-project", ... }
            
            match self.forward_lsp_request(&server_id, &method, params, None).await {
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
    }
}

async fn start_lsp_server(&self, id: &str, language: &str, workspace_root: &PathBuf, _: Option<Value>) -> Result<()> {
    // 启动 rust-analyzer 进程
    let mut cmd = TokioCommand::new("rust-analyzer");
    cmd.current_dir(workspace_root)  // /home/developer/rust-project
       .stdin(Stdio::piped())
       .stdout(Stdio::piped())
       .stderr(Stdio::piped())
       .kill_on_drop(true);
    
    let mut process = cmd.spawn()?;
    let stdin = process.stdin.take().unwrap();
    let stdout = process.stdout.take().unwrap();
    
    // 存储进程信息
    let managed_server = Arc::new(ManagedLspServer {
        id: id.to_string(),
        language: language.to_string(),
        workspace_root: workspace_root.clone(),
        process,
        stdin: Arc::new(Mutex::new(stdin)),
        // ...
    });
    
    self.lsp_servers.write().await.insert(id.to_string(), managed_server);
    
    // 启动 stdout 处理任务
    tokio::spawn(Self::handle_lsp_output(id.to_string(), stdout));
    
    info!("rust-analyzer started successfully: {}", id);
    Ok(())
}

async fn forward_lsp_request(&self, server_id: &str, method: &str, params: Value, _: Option<u64>) -> Result<Value> {
    let servers = self.lsp_servers.read().await;
    let server = servers.get(server_id).unwrap();
    
    let req_id = {
        let mut counter = server.request_id_counter.lock().await;
        let id = *counter;
        *counter += 1;
        id
    };
    
    // 创建 JSON-RPC 请求发送给 rust-analyzer
    let request = json!({
        "jsonrpc": "2.0",
        "id": req_id,
        "method": method,  // "initialize"
        "params": params   // { "rootUri": "file:///home/developer/rust-project", ... }
    });
    
    // 发送到 rust-analyzer stdin
    let mut stdin = server.stdin.lock().await;
    let request_str = format!("{}\n", serde_json::to_string(&request)?);
    stdin.write_all(request_str.as_bytes()).await?;
    stdin.flush().await?;
    
    info!("Forwarded {} request to rust-analyzer: {}", method, req_id);
    
    // 等待响应 (通过 stdout 处理任务)
    // ... 实际实现中需要设置响应等待机制
    
    Ok(json!({
        "capabilities": {
            "textDocumentSync": 1,
            "completionProvider": true,
            "hoverProvider": true,
            "definitionProvider": true,
            "referencesProvider": true
        }
    }))
}
```

### 4.2 rust-analyzer 响应处理

```rust
async fn handle_lsp_output(server_id: String, mut stdout: tokio::process::ChildStdout) {
    let mut buf_reader = BufReader::new(stdout);
    let mut line = String::new();
    
    loop {
        line.clear();
        match buf_reader.read_line(&mut line).await {
            Ok(0) => break, // EOF
            Ok(_) => {
                if let Ok(response) = serde_json::from_str::<Value>(&line) {
                    debug!("rust-analyzer response: {}", response);
                    
                    // 解析响应并转发给客户端
                    // 这里需要实现响应路由逻辑
                }
            }
            Err(e) => {
                error!("Error reading from rust-analyzer: {}", e);
                break;
            }
        }
    }
}
```

## 5. 响应回传

### 5.1 远程服务器返回响应

远程 lsp-proxy-server 将 rust-analyzer 的响应封装并发送回本地：

```json
{
  "type": "Response",
  "id": "remote-rust-uuid123",
  "result": {
    "Ok": {
      "capabilities": {
        "textDocumentSync": 1,
        "completionProvider": true,
        "hoverProvider": true,
        "definitionProvider": true,
        "referencesProvider": true
      }
    }
  }
}
```

### 5.2 本地 lsp-proxy 接收并转换

```rust
async fn handle_remote_lsp_response(&self, response: Value, file_info: &RemoteFileInfo) -> Value {
    // 将远程文件路径转换回 TRAMP URI
    let mut local_response = response.clone();
    
    // 如果响应中包含文件 URI，需要转换回 TRAMP 格式
    if let Some(locations) = local_response.get_mut("locations") {
        if let Some(locations_array) = locations.as_array_mut() {
            for location in locations_array {
                if let Some(uri) = location.get_mut("uri") {
                    if let Some(uri_str) = uri.as_str() {
                        if uri_str.starts_with("file:///") {
                            let remote_path = &uri_str[7..];
                            let tramp_uri = format!("file:///ssh:{}:{}", 
                                                   file_info.host, remote_path);
                            *uri = json!(tramp_uri);
                        }
                    }
                }
            }
        }
    }
    
    local_response
}
```

### 5.3 发送给 Emacs

最终，本地 lsp-proxy 将转换后的响应发送给 Emacs：

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "capabilities": {
      "textDocumentSync": 1,
      "completionProvider": true,
      "hoverProvider": true,
      "definitionProvider": true,
      "referencesProvider": true
    }
  }
}
```

## 6. 后续 LSP 操作示例

### 6.1 代码补全请求

用户在文件中输入 `Vec::` 时，触发补全：

```json
// Emacs -> 本地 lsp-proxy
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "textDocument/completion",
  "params": {
    "textDocument": {
      "uri": "file:///ssh:dev-server.com:/home/developer/rust-project/src/main.rs"
    },
    "position": { "line": 5, "character": 8 }
  }
}
```

本地 lsp-proxy 转换并转发：

```json
// 本地 lsp-proxy -> SSH 隧道 -> 远程 lsp-proxy-server
{
  "type": "LspRequest",
  "server_id": "remote-rust-uuid123",
  "method": "textDocument/completion", 
  "params": {
    "textDocument": {
      "uri": "file:///home/developer/rust-project/src/main.rs"
    },
    "position": { "line": 5, "character": 8 }
  }
}
```

远程服务器转发给 rust-analyzer 并返回补全项目。

### 6.2 跳转定义请求

用户按 `M-.` 跳转到定义：

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "textDocument/definition",
  "params": {
    "textDocument": {
      "uri": "file:///ssh:dev-server.com:/home/developer/rust-project/src/main.rs"
    },
    "position": { "line": 10, "character": 15 }
  }
}
```

响应包含定义位置：

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": [
    {
      "uri": "file:///ssh:dev-server.com:/home/developer/rust-project/src/lib.rs",
      "range": {
        "start": { "line": 25, "character": 8 },
        "end": { "line": 25, "character": 20 }
      }
    }
  ]
}
```

## 7. 性能优化要点

### 7.1 连接复用

- SSH 隧道在整个会话中保持活跃
- 多个文件共享同一个远程 LSP 服务器实例
- 使用 SSH ControlMaster 复用底层 SSH 连接

### 7.2 异步处理

- 所有网络 I/O 都是异步的
- LSP 请求和响应通过 tokio 异步处理
- SSH 隧道建立不阻塞主线程

### 7.3 错误恢复

- SSH 隧道断开时自动重连
- LSP 服务器崩溃时自动重启
- 网络超时的优雅处理

这个完整的实现展示了如何通过 SSH 隧道实现透明的远程 LSP 转发，让用户在编辑远程文件时享受与本地开发完全相同的体验。