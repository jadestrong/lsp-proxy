# Server Mode 使用指南

LSP-Proxy 现在完全支持 Server Mode，提供更高性能的远程开发体验。

## Server Mode vs Direct Mode

| 特性 | Direct Mode | Server Mode |
|------|-------------|-------------|
| 部署要求 | 仅需语言服务器 | 需部署 lsp-proxy-server |
| 性能 | 中等 | 高 |
| 文件操作 | SSH/SFTP | 自定义协议 |
| LSP 通信 | 通过 SSH 管道 | 直接 TCP 连接 |
| 缓存支持 | 无 | 是 |
| 自动部署 | N/A | 支持 |

## 快速开始

### 1. 构建项目

```bash
cargo build --release
```

这将生成两个二进制文件：
- `emacs-lsp-proxy`: 主程序
- `lsp-proxy-server`: 服务器端程序

### 2. 配置远程服务器

创建 `~/.config/lsp-proxy/remote.toml`:

```toml
[[servers]]
name = "dev-server"
host = "dev.example.com"
user = "developer"
port = 22
workspace_root = "/home/developer/projects"

# 启用 Server Mode 并自动部署
mode = { Server = { auto_deploy = true, server_path = "/usr/local/bin/lsp-proxy-server" } }

[servers.auth]
method = "key"
key_file = "~/.ssh/id_rsa"
```

### 3. 在 Emacs 中使用

```elisp
;; 加载远程开发模块
(require 'lsp-proxy-remote)

;; 启用远程模式
(lsp-proxy-remote-mode 1)

;; 连接到远程服务器
M-x lsp-proxy-remote-connect RET dev-server RET
```

## 配置选项详解

### Server Mode 配置

```toml
[[servers]]
name = "server-name"
host = "hostname.com"
user = "username"
port = 22
workspace_root = "/path/to/workspace"

# Server Mode 配置
mode = { Server = { auto_deploy = true, server_path = "/usr/local/bin/lsp-proxy-server" } }

[servers.auth]
method = "key"  # 或 "password"
key_file = "~/.ssh/id_rsa"
```

### 模式选项

1. **Direct Mode**: `mode = "Direct"`
   - 传统 SSH 模式，零部署

2. **Server Mode**: `mode = { Server = { auto_deploy = true, server_path = "/path" } }`
   - `auto_deploy`: 是否自动部署服务器
   - `server_path`: 远程服务器二进制路径

3. **Auto Mode**: `mode = "Auto"`
   - 自动选择最佳模式

## 自动部署

当启用 `auto_deploy = true` 时，系统会：

1. **检查版本**: 比较本地和远程版本
2. **构建二进制**: 在本地构建 lsp-proxy-server
3. **上传文件**: 将二进制上传到远程服务器
4. **配置服务**: 设置 systemd 服务 (可选)
5. **验证部署**: 测试服务器功能

### 手动部署

如果需要手动部署：

```bash
# 1. 构建服务器二进制
cargo build --release --bin lsp-proxy-server

# 2. 上传到远程服务器
scp ./target/release/lsp-proxy-server user@host:/usr/local/bin/

# 3. 在远程服务器上
ssh user@host
chmod +x /usr/local/bin/lsp-proxy-server

# 4. 创建配置文件
mkdir -p ~/.config/lsp-proxy
lsp-proxy-server --create-config ~/.config/lsp-proxy/server.toml

# 5. 启动服务器
lsp-proxy-server --port 7878 --config ~/.config/lsp-proxy/server.toml
```

## 服务器端配置

### 创建服务器配置

```bash
# 创建默认配置
lsp-proxy-server --create-config server.toml
```

### 配置文件示例

```toml
[server]
max_connections = 10
connection_timeout = 30
keepalive_interval = 60
protocol_version = "1.0.0"
name = "lsp-proxy-server"

[filesystem]
max_file_size = 104857600  # 100MB
enable_watching = true
max_watched_files = 10000
enable_compression = true
chunk_size = 8192

[security]
allowed_ips = []  # 空 = 允许所有IP
require_auth = false
shared_secret = ""
rate_limit = 1000
enable_tls = false

[performance]
buffer_size = 8192
enable_cache = true
cache_size = 256  # 256MB
cache_ttl = 3600  # 1小时
enable_metrics = false

# 语言服务器配置
[language_servers.rust-analyzer]
command = "rust-analyzer"
args = []
file_types = ["rs"]
auto_restart = true
restart_delay = 5
max_memory = 1024  # MB
```

### 启动选项

```bash
lsp-proxy-server \
    --port 7878 \
    --host 0.0.0.0 \
    --config ~/.config/lsp-proxy/server.toml \
    --workspace /home/user/projects \
    --log-level 2 \
    --daemon
```

## 使用 systemd 管理服务

### 创建服务文件

```ini
# ~/.config/systemd/user/lsp-proxy-server.service
[Unit]
Description=LSP-Proxy Server
After=network.target

[Service]
Type=simple
ExecStart=/usr/local/bin/lsp-proxy-server --port 7878 --config %h/.config/lsp-proxy/server.toml
Restart=always
RestartSec=5
User=%i
WorkingDirectory=%h

[Install]
WantedBy=default.target
```

### 管理服务

```bash
# 重新加载服务配置
systemctl --user daemon-reload

# 启用服务
systemctl --user enable lsp-proxy-server

# 启动服务
systemctl --user start lsp-proxy-server

# 查看状态
systemctl --user status lsp-proxy-server

# 查看日志
journalctl --user -u lsp-proxy-server -f
```

## Emacs 集成

### 基础配置

```elisp
(use-package lsp-proxy-remote
  :after lsp-proxy
  :config
  (setq lsp-proxy-remote-auto-connect t
        lsp-proxy-remote-default-server "dev-server"
        lsp-proxy-remote-connection-timeout 60)
  
  ;; 自动启用远程模式
  (lsp-proxy-remote-enable-with-main-mode)
  
  ;; 键绑定
  (global-set-key (kbd "C-c R c") #'lsp-proxy-remote-connect)
  (global-set-key (kbd "C-c R d") #'lsp-proxy-remote-disconnect)
  (global-set-key (kbd "C-c R o") #'lsp-proxy-remote-open-file)
  (global-set-key (kbd "C-c R l") #'lsp-proxy-remote-list-servers))
```

### 高级配置

```elisp
;; 钩子配置
(add-hook 'lsp-proxy-remote-connected-hook
          (lambda ()
            (message "🌐 Connected to remote server")
            (setq mode-line-misc-info 
                  (append mode-line-misc-info '(" [Remote]")))))

(add-hook 'lsp-proxy-remote-disconnected-hook
          (lambda ()
            (message "📴 Disconnected from remote server")
            (setq mode-line-misc-info 
                  (remove " [Remote]" mode-line-misc-info))))

;; 自定义状态栏显示
(defun my/remote-connection-status ()
  (if (and (bound-and-true-p lsp-proxy-remote-mode)
           lsp-proxy-remote--connected-servers)
      (format " [R:%d]" (length lsp-proxy-remote--connected-servers))
    ""))

(setq-default mode-line-format
              (append mode-line-format '((:eval (my/remote-connection-status)))))
```

## 故障排除

### 常见问题

1. **连接失败**
   ```
   Error: Failed to connect to server mode
   ```
   
   解决方案：
   - 检查服务器是否运行：`systemctl --user status lsp-proxy-server`
   - 检查端口是否开放：`ss -tlnp | grep 7878`
   - 检查防火墙设置

2. **自动部署失败**
   ```
   Error: Failed to build server binary
   ```
   
   解决方案：
   - 确保本地有完整的 Rust 工具链
   - 检查 SSH 连接和权限
   - 手动构建测试：`cargo build --release --bin lsp-proxy-server`

3. **语言服务器无法启动**
   ```
   Error: Language server 'rust-analyzer' not found
   ```
   
   解决方案：
   - 在远程服务器上安装语言服务器：`rustup component add rust-analyzer`
   - 检查 PATH 设置
   - 更新服务器配置文件

### 调试模式

```bash
# 启动服务器时启用详细日志
lsp-proxy-server --log-level 3 --port 7878

# 在 Emacs 中启用调试
(setq lsp-proxy-remote-debug t)
```

### 性能优化

1. **启用缓存**：
   ```toml
   [performance]
   enable_cache = true
   cache_size = 512  # 增大缓存
   cache_ttl = 7200  # 增长 TTL
   ```

2. **调整网络设置**：
   ```toml
   [server]
   max_connections = 20
   connection_timeout = 60
   keepalive_interval = 30
   ```

3. **文件系统优化**：
   ```toml
   [filesystem]
   enable_compression = true
   chunk_size = 16384  # 增大块大小
   ```

## 监控和维护

### 查看服务器状态

```bash
# 通过 Emacs
M-x lsp-proxy-remote-list-servers

# 直接连接测试
echo '{"type":"Ping","data":{"id":1}}' | nc localhost 7878
```

### 日志查看

```bash
# systemd 日志
journalctl --user -u lsp-proxy-server -f

# 应用日志 (如果配置了文件日志)
tail -f ~/.local/share/lsp-proxy/logs/server.log
```

### 维护任务

1. **定期更新**：
   - 更新 lsp-proxy: `git pull && cargo build --release`
   - 重新部署服务器：连接时会自动检查版本

2. **清理缓存**：
   ```bash
   # 清理服务器缓存
   systemctl --user stop lsp-proxy-server
   rm -rf ~/.cache/lsp-proxy/
   systemctl --user start lsp-proxy-server
   ```

3. **备份配置**：
   ```bash
   cp ~/.config/lsp-proxy/server.toml ~/.config/lsp-proxy/server.toml.backup
   ```

## 总结

Server Mode 为 LSP-Proxy 提供了企业级的远程开发能力：

- ✅ **高性能**: 专用协议和本地缓存
- ✅ **自动部署**: 一键部署和版本管理  
- ✅ **可靠性**: 连接重试和故障恢复
- ✅ **可扩展性**: 支持多服务器和负载均衡
- ✅ **易用性**: 与现有工作流程无缝集成

选择适合您需求的模式：
- **Direct Mode**: 简单项目，临时使用
- **Server Mode**: 长期开发，高频使用
- **Auto Mode**: 让系统自动选择最佳模式