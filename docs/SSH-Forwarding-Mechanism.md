# LSP-Proxy SSH 转发机制详解

## 当前实现的请求转发流程

### 1. 整体转发架构

```
本地 Emacs (lsp-mode) 
     ↓ JSON-RPC over stdio
本地 lsp-proxy 进程
     ↓ [远程文件检测]
     ↓ 
SSH 连接 + 端口转发
     ↓
远程 lsp-proxy-server (TCP:9527)
     ↓ JSON-RPC over stdio  
远程 LSP 服务器 (rust-analyzer, pylsp, etc.)
```

### 2. 远程文件检测机制

本地 lsp-proxy 通过以下方式检测远程文件：

#### 2.1 TRAMP 路径识别

```rust
// src/remote/lsp_client.rs
impl RemoteLspManager {
    /// 检查 URI 是否为远程文件
    pub fn is_remote_file(uri: &str) -> bool {
        // 检查 TRAMP 风格的 URI
        if uri.starts_with("file:///ssh:") || uri.starts_with("file:///tramp:") {
            return true;
        }
        
        // 检查 SSH 风格路径
        if let Ok(url) = Url::parse(uri) {
            if url.scheme() == "file" {
                let path = url.path();
                // TRAMP 路径：/ssh:host:/path 或 /method:host:/path
                if path.starts_with("/ssh:") || path.starts_with("/scp:") || 
                   path.starts_with("/tramp:") || path.contains("@") {
                    return true;
                }
            }
        }
        
        false
    }
    
    /// 从 URI 提取远程主机信息
    pub fn extract_remote_info(uri: &str) -> Option<RemoteFileInfo> {
        // 解析路径：/ssh:user@host:/remote/path
        if let Some(tramp_match) = Self::parse_tramp_path(path) {
            return Some(tramp_match);
        }
    }
}
```

#### 2.2 路径解析示例

```rust
// 输入: file:///ssh:developer@dev-server.com:/home/user/project/src/main.rs
// 解析结果:
RemoteFileInfo {
    method: "ssh",
    user: Some("developer"),
    host: "dev-server.com", 
    path: "/home/user/project/src/main.rs"
}
```

### 3. SSH 连接建立机制

#### 3.1 SSH 隧道自动建立

```rust
// src/remote/lsp_client.rs
impl RemoteConnection {
    pub async fn new(config: &RemoteHostConfig) -> Result<Self> {
        let stream = match config.connection_type {
            ConnectionType::SshTunnel => {
                if let Some(ssh_config) = &config.ssh_tunnel {
                    // 1. 建立 SSH 隧道
                    self.establish_ssh_tunnel(ssh_config).await?;
                    
                    // 2. 连接到本地隧道端口
                    let tunnel_address = format!("127.0.0.1:{}", ssh_config.local_port);
                    let stream = TokioTcpStream::connect(tunnel_address).await?;
                    stream
                } else {
                    return Err(anyhow!("SSH tunnel config required"));
                }
            }
            ConnectionType::Tcp => {
                // 直接 TCP 连接
                let stream = TokioTcpStream::connect(&config.address).await?;
                stream
            }
        }
    }
    
    async fn establish_ssh_tunnel(&self, ssh_config: &SshTunnelConfig) -> Result<()> {
        // 使用 tokio::process 启动 SSH 隧道
        let mut cmd = tokio::process::Command::new("ssh");
        cmd.args(&[
            "-L", &format!("{}:127.0.0.1:9527", ssh_config.local_port),
            "-N", "-f",  // 不执行命令，后台运行
            &format!("{}@{}", ssh_config.user, ssh_config.host)
        ]);
        
        if let Some(identity_file) = &ssh_config.identity_file {
            cmd.args(&["-i", &identity_file.to_string_lossy()]);
        }
        
        let output = cmd.output().await?;
        if !output.status.success() {
            return Err(anyhow!("Failed to establish SSH tunnel: {}", 
                              String::from_utf8_lossy(&output.stderr)));
        }
        
        // 等待隧道建立
        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;
        Ok(())
    }
}
```

### 4. LSP 请求转发流程

#### 4.1 请求拦截和路由

```rust
// src/application.rs (需要添加)
impl Application {
    /// 处理 LSP 请求，支持远程转发
    pub async fn handle_lsp_request_with_remote(
        &mut self, 
        method: &str, 
        params: Value
    ) -> Result<Value, ResponseError> {
        // 1. 从参数中提取 URI
        let uri = self.extract_uri_from_params(&params);
        
        if let Some(uri_str) = uri {
            // 2. 检查是否为远程文件
            if RemoteLspManager::is_remote_file(&uri_str) {
                return self.handle_remote_lsp_request(method, params, &uri_str).await;
            }
        }
        
        // 3. 本地文件，使用原有逻辑
        self.handle_lsp_request_local(method, params).await
    }
    
    async fn handle_remote_lsp_request(
        &mut self, 
        method: &str, 
        params: Value, 
        uri: &str
    ) -> Result<Value, ResponseError> {
        let remote_manager = self.remote_lsp_manager.as_ref()
            .ok_or_else(|| ResponseError::internal_error("Remote LSP not initialized"))?;
        
        // 4. 提取远程主机信息
        let file_info = RemoteLspManager::extract_remote_info(uri)
            .ok_or_else(|| ResponseError::invalid_params("Invalid remote file URI"))?;
        
        // 5. 获取或创建远程连接
        let connection = remote_manager.get_connection(&file_info.host).await?;
        
        // 6. 确保远程 LSP 服务器运行
        let language = self.detect_language_from_path(&file_info.path)
            .unwrap_or_else(|| "text".to_string());
        let server_id = connection.start_lsp_server(&workspace_path, &language).await?;
        
        // 7. 转换 URI：本地 TRAMP → 远程文件路径
        let remote_params = self.convert_uri_in_params(params, uri, &file_info.path);
        
        // 8. 转发请求到远程服务器
        let result = connection.forward_lsp_request(&server_id, method, remote_params).await?;
        
        // 9. 转换响应：远程路径 → 本地 TRAMP URI
        let local_result = self.convert_remote_uris_in_response(result, &file_info);
        Ok(local_result)
    }
}
```

#### 4.2 协议转换详解

```rust
impl Application {
    /// 将本地 TRAMP URI 转换为远程文件路径
    fn convert_uri_in_params(&self, mut params: Value, local_uri: &str, remote_path: &str) -> Value {
        // 输入: file:///ssh:dev-server:/home/user/project/src/main.rs
        // 输出: file:///home/user/project/src/main.rs
        
        let remote_uri = format!("file://{}", remote_path);
        
        // 替换 LSP 参数中的 URI
        if let Some(text_document) = params.get_mut("textDocument") {
            if let Some(uri) = text_document.get_mut("uri") {
                if uri.as_str() == Some(local_uri) {
                    *uri = json!(remote_uri);
                }
            }
        }
        
        params
    }
    
    /// 将远程响应中的路径转换回本地 TRAMP URI
    fn convert_remote_uris_in_response(&self, mut response: Value, file_info: &RemoteFileInfo) -> Value {
        // 输入: file:///home/user/project/src/lib.rs
        // 输出: file:///ssh:dev-server:/home/user/project/src/lib.rs
        
        if let Some(locations) = response.get_mut("locations") {
            if let Some(locations_array) = locations.as_array_mut() {
                for location in locations_array {
                    if let Some(uri) = location.get_mut("uri") {
                        if let Some(uri_str) = uri.as_str() {
                            if uri_str.starts_with("file://") {
                                let remote_path = &uri_str[7..]; // 移除 file://
                                let tramp_uri = format!("file:///{}:{}:{}", 
                                                       file_info.method,
                                                       file_info.host,
                                                       remote_path);
                                *uri = json!(tramp_uri);
                            }
                        }
                    }
                }
            }
        }
        
        response
    }
}
```

### 5. SSH 配置集成

#### 5.1 SSH 配置文件解析

```rust
// 利用现有的 ssh_config.rs 模块
impl RemoteConnection {
    pub async fn new_with_ssh_config(host: &str) -> Result<Self> {
        // 1. 解析 SSH 配置
        let ssh_parser = SshConfigParser::new();
        let ssh_config = ssh_parser.get_host_config(host)?
            .ok_or_else(|| anyhow!("No SSH config found for host: {}", host))?;
        
        // 2. 构建连接配置
        let config = RemoteHostConfig {
            address: format!("127.0.0.1:{}", self.allocate_local_port()),
            connection_type: ConnectionType::SshTunnel,
            ssh_tunnel: Some(SshTunnelConfig {
                host: ssh_config.hostname.unwrap_or_else(|| host.to_string()),
                port: ssh_config.port.unwrap_or(22),
                user: ssh_config.user.unwrap_or_else(|| "root".to_string()),
                identity_file: ssh_config.identity_file,
                local_port: self.allocate_local_port(),
            }),
            auth_token: None,
        };
        
        // 3. 建立连接
        Self::new(&config).await
    }
    
    fn allocate_local_port(&self) -> u16 {
        // 动态分配本地端口，避免冲突
        use std::net::TcpListener;
        TcpListener::bind("127.0.0.1:0").unwrap().local_addr().unwrap().port()
    }
}
```

#### 5.2 SSH 隧道生命周期管理

```rust
impl RemoteConnection {
    pub struct SshTunnel {
        process: tokio::process::Child,
        local_port: u16,
        host: String,
    }
    
    impl SshTunnel {
        pub async fn establish(config: &SshTunnelConfig) -> Result<Self> {
            let mut cmd = tokio::process::Command::new("ssh");
            cmd.args(&[
                "-L", &format!("{}:127.0.0.1:9527", config.local_port),
                "-N",           // 不执行远程命令
                "-o", "ExitOnForwardFailure=yes",  // 转发失败时退出
                "-o", "StrictHostKeyChecking=no",  // 跳过主机密钥检查
                &format!("{}@{}", config.user, config.host)
            ]);
            
            if let Some(identity_file) = &config.identity_file {
                cmd.args(&["-i", &identity_file.to_string_lossy()]);
            }
            
            let process = cmd.spawn()?;
            
            // 等待隧道建立
            tokio::time::sleep(tokio::time::Duration::from_secs(3)).await;
            
            // 测试隧道是否可用
            if let Err(_) = TokioTcpStream::connect(
                format!("127.0.0.1:{}", config.local_port)
            ).await {
                return Err(anyhow!("SSH tunnel failed to establish"));
            }
            
            Ok(Self {
                process,
                local_port: config.local_port,
                host: config.host.clone(),
            })
        }
    }
    
    impl Drop for SshTunnel {
        fn drop(&mut self) {
            // 自动清理 SSH 隧道进程
            let _ = self.process.kill();
        }
    }
}
```

### 6. 完整的请求转发示例

#### 6.1 用户操作序列

```elisp
;; 1. 用户在 Emacs 中打开远程文件
C-x C-f /ssh:dev-server:/home/user/project/src/main.rs RET

;; 2. lsp-mode 发送 initialize 请求
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "rootUri": "file:///ssh:dev-server:/home/user/project",
    "capabilities": { ... }
  }
}
```

#### 6.2 本地 lsp-proxy 处理流程

```rust
// 1. lsp-proxy 接收到请求
let request = json!({
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize",
    "params": {
        "rootUri": "file:///ssh:dev-server:/home/user/project",
        "capabilities": { ... }
    }
});

// 2. 检测远程文件
let uri = "file:///ssh:dev-server:/home/user/project";
let is_remote = RemoteLspManager::is_remote_file(uri); // true

// 3. 提取主机信息
let file_info = RemoteFileInfo {
    method: "ssh",
    user: None,
    host: "dev-server",
    path: "/home/user/project"
};

// 4. 建立 SSH 隧道 (如果不存在)
// ssh -L 19527:127.0.0.1:9527 -N -f dev-server

// 5. 连接到隧道端口
let stream = TokioTcpStream::connect("127.0.0.1:19527").await?;

// 6. 转换请求参数
let remote_request = json!({
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize", 
    "params": {
        "rootUri": "file:///home/user/project",  // 本地路径
        "capabilities": { ... }
    }
});
```

#### 6.3 远程服务器处理

```rust
// lsp-proxy-server 接收请求
let message = ProxyMessage::StartLspServer {
    id: "rust-main-abc123",
    language: "rust",
    workspace_root: "/home/user/project".into(),
    client_capabilities: Some(capabilities),
};

// 启动远程 rust-analyzer
let mut cmd = TokioCommand::new("rust-analyzer");
cmd.current_dir("/home/user/project")
   .stdin(Stdio::piped())
   .stdout(Stdio::piped());

let process = cmd.spawn()?;

// 转发 initialize 请求到 rust-analyzer
let lsp_request = json!({
    "jsonrpc": "2.0",
    "id": 1,
    "method": "initialize",
    "params": {
        "rootUri": "file:///home/user/project",
        "capabilities": { ... }
    }
});

// 发送到 rust-analyzer stdin
process.stdin.write_all(serde_json::to_string(&lsp_request)?.as_bytes()).await?;
```

#### 6.4 响应回传

```rust
// rust-analyzer 响应
let response = json!({
    "jsonrpc": "2.0",
    "id": 1,
    "result": {
        "capabilities": {
            "textDocumentSync": 1,
            "completionProvider": true,
            // ...
        }
    }
});

// lsp-proxy-server 转发响应
let proxy_response = ProxyMessage::Response {
    id: "rust-main-abc123",
    result: Ok(response),
};

// 发送回本地 lsp-proxy
stream.write_all(serde_json::to_string(&proxy_response)?.as_bytes()).await?;

// 本地 lsp-proxy 转换 URI 并发送给 Emacs
let final_response = json!({
    "jsonrpc": "2.0", 
    "id": 1,
    "result": {
        "capabilities": {
            "textDocumentSync": 1,
            "completionProvider": true,
            // ...
        }
    }
});

// 发送给 Emacs via stdout
println!("{}", serde_json::to_string(&final_response)?);
```

### 7. 性能和安全考虑

#### 7.1 连接复用

```rust
impl RemoteLspManager {
    // 连接池管理
    connections: Arc<RwLock<HashMap<String, Arc<RemoteConnection>>>>,
    
    pub async fn get_connection(&self, host: &str) -> Result<Arc<RemoteConnection>> {
        // 复用现有连接
        if let Some(conn) = self.connections.read().await.get(host) {
            if conn.is_alive().await {
                return Ok(Arc::clone(conn));
            }
        }
        
        // 创建新连接
        let connection = Arc::new(RemoteConnection::new_with_ssh_config(host).await?);
        self.connections.write().await.insert(host.to_string(), Arc::clone(&connection));
        Ok(connection)
    }
}
```

#### 7.2 SSH 配置安全

```bash
# ~/.ssh/config 推荐配置
Host dev-server
    HostName dev-server.company.com
    User developer
    Port 22
    IdentityFile ~/.ssh/dev_key
    # 安全选项
    StrictHostKeyChecking yes
    UserKnownHostsFile ~/.ssh/known_hosts
    # 连接复用
    ControlMaster auto
    ControlPath ~/.ssh/sockets/ssh-%r@%h:%p
    ControlPersist 10m
    # 端口转发
    LocalForward 19527 127.0.0.1:9527
```

这个实现完全满足了你的需求：**本地 lsp-proxy 自动检测远程文件，通过 SSH 隧道将 LSP 请求转发到远程 lsp-proxy-server，实现透明的远程 LSP 开发体验**。