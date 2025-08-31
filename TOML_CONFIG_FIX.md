# TOML Configuration Fix for LSP-Proxy Remote

## 问题分析

报错信息：
```
TOML parse error at line 4, column 1
  |
4 | [[servers]]
  | ^^^^^^^^^^^
invalid type: sequence, expected a map
```

## 问题根因

原始配置使用了 `[[servers]]` 数组表格式，但后端代码期望的是 `HashMap<String, RemoteServerConfig>` 结构，即以服务器名为键的映射表。

### 错误的格式（之前）:
```toml
[[servers]]  # 这创建了数组
name = "dev-server"
...

[servers.auth]  # 这与上面的数组冲突
...
```

### 正确的格式（修复后）:
```toml
[servers.dev-server]  # 直接以服务器名作为键
name = "dev-server"
...
auth = { Key = { path = "~/.ssh/id_rsa" } }  # 内联格式
```

## 修复要点

1. **服务器配置结构**:
   - 从 `[[servers]]` 改为 `[servers.server-name]`
   - 每个服务器都有自己的配置节

2. **认证配置**:
   - 从嵌套表格改为内联枚举
   - `auth = { Key = { path = "path" } }` 或 `auth = "Agent"`

3. **模式配置**:
   - 支持的模式：`"Direct"`, `"Auto"`, `{ Server = {...} }`, `{ SSHTunnel = {...} }`

4. **字段名匹配**:
   - `keepalive_interval` 而不是 `keep_alive`
   - 确保所有字段名与 Rust 结构体匹配

## 完整示例

```toml
[servers.dev-server]
name = "dev-server"
host = "dev.example.com" 
user = "developer"
port = 22
workspace_root = "/home/developer/projects"
mode = { Server = { auto_deploy = true, server_path = "/usr/local/bin/lsp-proxy-server" } }
auth = { Key = { path = "~/.ssh/id_rsa" } }

[servers.direct-server]
name = "direct-server"
host = "direct.example.com"
user = "developer"
port = 22
workspace_root = "/home/developer/work"
mode = "Direct"
auth = "Agent"

default_server = "dev-server"

[cache_settings]
enabled = true
max_size_mb = 100
ttl_seconds = 3600

[connection_settings]
timeout_seconds = 30
retry_attempts = 3
keepalive_interval = 60
compression = true
```

## 验证

修复后的配置文件应该能够：
- ✅ 正确解析 TOML 格式
- ✅ 映射到后端的 `RemoteConfig` 结构
- ✅ 支持所有远程开发模式
- ✅ 提供正确的类型信息