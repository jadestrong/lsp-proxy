# LSP服务器日志优化方案

## 问题分析

当前LSP-Proxy的日志系统存在以下问题：

1. **多服务器日志混合**: 多个LSP服务器的日志信息混在一起，难以区分
2. **JSON消息不可读**: LSP协议消息以压缩的JSON字符串形式输出，难以阅读和调试
3. **缺乏结构化**: 使用简单的字符串日志，缺乏结构化字段支持
4. **无法按服务器过滤**: 无法针对特定的LSP服务器进行日志过滤

## 优化目标

1. **服务器隔离**: 为每个LSP服务器提供独立的日志标识
2. **JSON美化**: 自动格式化JSON消息，提高可读性
3. **结构化日志**: 使用结构化日志记录，支持字段查询
4. **可配置性**: 允许用户配置日志级别和格式

## 技术方案

### 1. 引入结构化日志库

使用 `tracing` 替代简单的 `log` 库：

```toml
# Cargo.toml
[dependencies]
tracing = "0.1"
tracing-subscriber = { version = "0.3", features = ["json", "env-filter"] }
serde_json = "1.0"
```

### 2. 创建专用日志模块

新建 `src/logging.rs` 模块：

```rust
use tracing::{debug, error, info, warn};
use serde_json::Value;

pub struct LspLogger {
    server_name: String,
    server_id: Option<u32>,
}

impl LspLogger {
    pub fn new(server_name: String, server_id: Option<u32>) -> Self {
        Self { server_name, server_id }
    }

    pub fn log_request(&self, method: &str, params: &str) {
        match self.pretty_print_json(params) {
            Ok(formatted) => {
                info!(
                    server = %self.server_name,
                    server_id = self.server_id,
                    direction = "outgoing",
                    message_type = "request",
                    method = method,
                    "LSP Request:\n{}",
                    formatted
                );
            },
            Err(_) => {
                info!(
                    server = %self.server_name,
                    server_id = self.server_id,
                    direction = "outgoing", 
                    message_type = "request",
                    method = method,
                    raw_content = params,
                    "LSP Request (invalid JSON)"
                );
            }
        }
    }

    pub fn log_response(&self, content: &str) {
        match self.pretty_print_json(content) {
            Ok(formatted) => {
                info!(
                    server = %self.server_name,
                    server_id = self.server_id,
                    direction = "incoming",
                    message_type = "response",
                    "LSP Response:\n{}",
                    formatted
                );
            },
            Err(_) => {
                info!(
                    server = %self.server_name,
                    server_id = self.server_id,
                    direction = "incoming",
                    message_type = "response", 
                    raw_content = content,
                    "LSP Response (invalid JSON)"
                );
            }
        }
    }

    pub fn log_notification(&self, method: &str, params: &str) {
        match self.pretty_print_json(params) {
            Ok(formatted) => {
                debug!(
                    server = %self.server_name,
                    server_id = self.server_id,
                    direction = "incoming",
                    message_type = "notification",
                    method = method,
                    "LSP Notification:\n{}",
                    formatted
                );
            },
            Err(_) => {
                debug!(
                    server = %self.server_name,
                    server_id = self.server_id,
                    direction = "incoming",
                    message_type = "notification",
                    method = method,
                    raw_content = params,
                    "LSP Notification (invalid JSON)"
                );
            }
        }
    }

    pub fn log_error(&self, error: &str) {
        error!(
            server = %self.server_name,
            server_id = self.server_id,
            message_type = "error",
            "LSP Server Error: {}",
            error
        );
    }

    fn pretty_print_json(&self, json_str: &str) -> Result<String, serde_json::Error> {
        let value: Value = serde_json::from_str(json_str)?;
        serde_json::to_string_pretty(&value)
    }
}
```

### 3. 修改传输层

更新 `src/lsp/transport.rs`：

```rust
use crate::logging::LspLogger;

impl Transport {
    pub fn new(
        server_stdout: ChildStdout,
        server_stdin: ChildStdin,
        server_stderr: ChildStderr,
        id: usize,
        name: String,
    ) -> (Receiver<Call>, Self) {
        let logger = LspLogger::new(name.clone(), Some(id as u32));
        // ... 其他代码
    }

    // 替换现有的日志调用
    async fn recv_server_msg(
        inp: &mut (impl AsyncBufRead + Unpin + Send),
        buffer: &mut String,
        logger: &LspLogger,
    ) -> Result<ServerMessage> {
        // ... 解析消息的代码 ...
        
        // 替换: debug!("{_language_server_name} <- {msg}");
        logger.log_response(msg);
        
        // ... 其他代码
    }

    async fn send_to_server(
        server_stdin: &mut (impl AsyncWrite + Unpin + Send),
        request: String,
        logger: &LspLogger,
    ) -> Result<()> {
        // 替换: debug!("{language_server_name} -> {request}");
        // 需要解析请求以获取method
        if let Ok(req_value) = serde_json::from_str::<serde_json::Value>(&request) {
            if let Some(method) = req_value.get("method").and_then(|m| m.as_str()) {
                logger.log_request(method, &request);
            }
        }
        
        // ... 发送逻辑
    }
}
```

### 4. 配置日志系统

更新 `src/main.rs`:

```rust
use tracing_subscriber::{EnvFilter, fmt, prelude::*};

fn setup_logging(log_level: u64) -> Result<()> {
    let filter = match log_level {
        0 => "warn",
        1 => "info", 
        2 => "debug",
        _ => "trace",
    };

    let env_filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::new(filter));

    tracing_subscriber::registry()
        .with(
            fmt::layer()
                .with_target(true)
                .with_thread_ids(true)
                .with_thread_names(true)
                .with_file(true)
                .with_line_number(true)
        )
        .with(env_filter)
        .init();

    Ok(())
}
```

### 5. 日志过滤和查询

为用户提供过滤选项：

```bash
# 只显示特定服务器的日志
RUST_LOG="lsp_proxy[server=rust-analyzer]" ./lsp-proxy

# 只显示请求
RUST_LOG="lsp_proxy[message_type=request]" ./lsp-proxy  

# 只显示错误
RUST_LOG="lsp_proxy[message_type=error]" ./lsp-proxy

# 显示特定方法
RUST_LOG="lsp_proxy[method=textDocument/completion]" ./lsp-proxy
```

## 实施步骤

1. **阶段1**: 添加依赖和基础日志模块
2. **阶段2**: 更新传输层使用新的日志系统  
3. **阶段3**: 更新主循环和控制器的日志
4. **阶段4**: 添加配置选项和文档
5. **阶段5**: 测试和优化性能

## 期望效果

实施后的日志输出示例：

```
2024-01-15T10:30:15.123456Z  INFO lsp_proxy::transport [server=rust-analyzer server_id=1 direction=outgoing message_type=request method=textDocument/completion]: LSP Request:
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "textDocument/completion",
  "params": {
    "textDocument": {
      "uri": "file:///path/to/file.rs"
    },
    "position": {
      "line": 42,
      "character": 10
    }
  }
}

2024-01-15T10:30:15.456789Z  INFO lsp_proxy::transport [server=rust-analyzer server_id=1 direction=incoming message_type=response]: LSP Response:
{
  "jsonrpc": "2.0", 
  "id": 1,
  "result": {
    "isIncomplete": false,
    "items": [
      {
        "label": "println!",
        "kind": 3,
        "detail": "macro"
      }
    ]
  }
}
```

## 性能考虑

- JSON格式化仅在相应日志级别启用时执行
- 使用惰性求值避免不必要的字符串分配
- 提供选项禁用JSON美化以提高性能

## 向后兼容

- 保持现有的日志级别参数 `--log-level`
- 添加新的环境变量配置，不破坏现有使用方式
- 提供传统日志格式的回退选项