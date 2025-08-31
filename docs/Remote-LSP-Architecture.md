# LSP-Proxy Remote Server Architecture Guide

## 概述

这个方案实现了一个**远程 LSP 代理架构**，其中：
- 在远程服务器上运行独立的 `lsp-proxy-server`
- 本地 `lsp-proxy` 检测到 SSH 远程文件时，自动将 LSP 请求转发给远程服务器
- 远程服务器管理本地 LSP 服务器实例，处理所有语言分析

## 架构图

```
本地 Emacs + lsp-mode
     ↓ (标准 LSP 请求)
本地 lsp-proxy (检测 TRAMP 远程文件)
     ↓ (JSON-RPC 协议 via TCP/SSH)
远程服务器上的 lsp-proxy-server
     ↓ (管理和调用远程 LSP)
远程 LSP 服务器 (rust-analyzer, pylsp, etc.)
```

## 1. 远程服务器设置

### 安装 lsp-proxy-server

```bash
# 在远程服务器上编译安装
git clone <lsp-proxy-repo>
cd lsp-proxy
cargo build --release --bin lsp-proxy-server

# 复制到系统路径
sudo cp target/release/lsp-proxy-server /usr/local/bin/
```

### 创建配置文件

创建 `/home/user/.config/lsp-proxy-server.toml`：

```toml
# LSP-Proxy Server 配置文件

# 监听地址
listen_address = "127.0.0.1:9527"  # TCP 监听
# listen_address = "/tmp/lsp-proxy.sock"  # Unix socket 监听

# 工作目录
work_dir = "/home/developer"

# 最大 LSP 服务器数量
max_lsp_servers = 10

# 各语言的 LSP 服务器配置
[lsp_servers.rust]
command = "rust-analyzer"
args = []
env = {}

[lsp_servers.python] 
command = "pylsp"
args = []
env = { "PYTHONPATH" = "/home/developer/venv/lib/python3.9/site-packages" }

[lsp_servers.typescript]
command = "typescript-language-server"
args = ["--stdio"]
env = {}

[lsp_servers.go]
command = "gopls"
args = []
env = {}

[lsp_servers.c]
command = "clangd"
args = []
env = {}

[lsp_servers.cpp]
command = "clangd" 
args = []
env = {}

# 认证设置 (可选)
[auth]
enabled = false
token = "your-secret-token"
allowed_hosts = ["127.0.0.1", "10.0.0.0/8"]
```

### 启动远程服务器

```bash
# 前台运行
lsp-proxy-server -c ~/.config/lsp-proxy-server.toml

# 后台运行
nohup lsp-proxy-server -c ~/.config/lsp-proxy-server.toml > /tmp/lsp-proxy-server.log 2>&1 &

# 使用 systemd 服务
sudo tee /etc/systemd/system/lsp-proxy-server.service > /dev/null <<EOF
[Unit]
Description=LSP-Proxy Server
After=network.target

[Service]
Type=simple
User=developer
WorkingDirectory=/home/developer
ExecStart=/usr/local/bin/lsp-proxy-server -c /home/developer/.config/lsp-proxy-server.toml
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
EOF

sudo systemctl daemon-reload
sudo systemctl enable lsp-proxy-server
sudo systemctl start lsp-proxy-server
```

## 2. 本地客户端配置

### 配置本地 lsp-proxy

在本地创建 `~/.config/lsp-proxy/remote-lsp.toml`：

```toml
# 远程 LSP 客户端配置

# 远程主机配置
[hosts.dev-server]
address = "dev.example.com:9527"
connection_type = "tcp"
auth_token = "your-secret-token"

[hosts.dev-server.ssh_tunnel]
host = "dev.example.com"
port = 22
user = "developer"
identity_file = "~/.ssh/id_rsa"
local_port = 19527  # 本地端口转发

[hosts.production]
address = "127.0.0.1:19528"
connection_type = "ssh_tunnel"

[hosts.production.ssh_tunnel]
host = "prod.example.com"
port = 22
user = "admin"
identity_file = "~/.ssh/prod_key"
local_port = 19528
```

### SSH 配置 (推荐)

配置 `~/.ssh/config`：

```
# 开发服务器 - 直接连接
Host dev.example.com
    HostName 192.168.1.100
    User developer
    Port 22
    IdentityFile ~/.ssh/dev_key

# 生产服务器 - 通过跳板机
Host prod.example.com
    HostName 10.0.1.50
    User admin
    Port 22
    IdentityFile ~/.ssh/prod_key
    ProxyJump bastion.example.com
    LocalForward 19528 127.0.0.1:9527
```

## 3. 使用方法

### 基本文件编辑流程

1. **打开远程文件** (通过 TRAMP)：
   ```elisp
   C-x C-f /ssh:dev.example.com:/home/developer/project/src/main.rs RET
   ```

2. **自动检测和转发**：
   - 本地 lsp-proxy 检测到这是远程文件
   - 自动连接到 `dev.example.com:9527`
   - 在远程服务器启动 rust-analyzer
   - 所有 LSP 请求透明转发

3. **正常编辑**：
   - 代码补全、跳转、诊断等功能正常工作
   - 所有处理在远程服务器进行
   - 结果返回到本地 Emacs

### 支持的 TRAMP 格式

系统支持以下远程文件格式：

```elisp
;; SSH 协议
/ssh:user@host:/path/to/file
/ssh:host:/path/to/file

;; SCP 协议  
/scp:user@host:/path/to/file

;; 通用 TRAMP
/tramp:method:user@host:/path/to/file
```

### 工作空间管理

每个远程目录会自动创建独立的 LSP 服务器实例：

```
远程服务器上的 LSP 实例：
├── /home/developer/project1/ → rust-analyzer (ID: remote-rust-abc123)
├── /home/developer/project2/ → pylsp (ID: remote-python-def456)
└── /opt/work/typescript/   → ts-language-server (ID: remote-typescript-ghi789)
```

## 4. 监控和调试

### 查看远程服务器状态

```bash
# 检查服务器进程
ps aux | grep lsp-proxy-server

# 查看日志
tail -f /tmp/lsp-proxy-server.log

# 检查端口监听
netstat -tlnp | grep 9527

# 测试连接
telnet dev.example.com 9527
```

### 本地调试

```elisp
;; 在 Emacs 中查看 lsp-proxy 日志
M-x view-buffer RET *lsp-proxy-log* RET

;; 查看远程 LSP 状态  
M-x lsp-workspace-show-log

;; 重启 lsp-mode (如果有问题)
M-x lsp-workspace-restart
```

### 协议调试

在远程服务器启用详细日志：

```bash
lsp-proxy-server --log-level debug -c ~/.config/lsp-proxy-server.toml
```

本地启用 lsp-proxy 调试：

```bash
lsp-proxy --log-level 3
```

## 5. 性能优化

### 网络优化

```bash
# SSH 连接复用
echo "
Host *
    ControlMaster auto
    ControlPath ~/.ssh/sockets/ssh-%r@%h:%p
    ControlPersist 10m
" >> ~/.ssh/config

mkdir -p ~/.ssh/sockets
```

### 服务器资源管理

```toml
# 在 lsp-proxy-server.toml 中限制资源
max_lsp_servers = 5  # 减少同时运行的 LSP 服务器数量

# 各 LSP 服务器环境变量优化
[lsp_servers.rust.env]
RUST_ANALYZER_NO_SYSROOT = "true"  # 减少内存使用

[lsp_servers.typescript.env] 
TSS_DEBUG_BRK = ""  # 禁用调试模式
```

### 连接池优化

```toml
# 远程客户端配置优化
[hosts.dev-server]
address = "dev.example.com:9527"
connection_type = "tcp"
# 启用连接复用
keepalive_interval = 30
connection_timeout = 10
```

## 6. 安全考虑

### 网络安全

```toml
# 启用认证
[auth]
enabled = true
token = "your-very-long-and-secure-token"
allowed_hosts = ["127.0.0.1", "10.0.0.0/8"]
```

### SSH 隧道 (推荐)

对于生产环境，建议使用 SSH 隧道：

```bash
# 建立 SSH 隧道
ssh -L 19527:127.0.0.1:9527 -N -f user@remote-host

# 本地连接隧道端口
# 配置中使用 address = "127.0.0.1:19527"
```

### 防火墙配置

```bash
# 只允许特定 IP 访问
sudo ufw allow from 10.0.0.0/8 to any port 9527
sudo ufw deny 9527
```

## 7. 故障排除

### 常见问题

**1. 连接失败**
```bash
# 检查网络连通性
telnet remote-host 9527

# 检查防火墙
sudo ufw status
```

**2. LSP 服务器启动失败**
```bash
# 检查命令路径
which rust-analyzer
which pylsp

# 手动测试 LSP 服务器
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}' | rust-analyzer
```

**3. 性能问题**
```bash
# 监控资源使用
top -p $(pgrep lsp-proxy-server)
htop

# 检查网络延迟
ping remote-host
```

### 日志级别调整

```bash
# 远程服务器详细日志
lsp-proxy-server --log-level trace

# 本地客户端详细日志
lsp-proxy --log-level 3
```

## 8. 扩展和自定义

### 添加新语言支持

在 `lsp-proxy-server.toml` 中添加：

```toml
[lsp_servers.ruby]
command = "solargraph"
args = ["stdio"]
env = { "GEM_PATH" = "/home/user/.gem" }

[lsp_servers.php]
command = "phpactor"
args = ["language-server"]
env = {}
```

### 自定义 LSP 服务器

```toml
[lsp_servers.custom]
command = "/path/to/custom-lsp-server"
args = ["--stdio", "--log-level", "debug"]
env = { 
    "CUSTOM_CONFIG" = "/path/to/config",
    "DEBUG" = "1"
}
initialization_options = { "feature_flags" = ["experimental"] }
```

这个架构提供了一个高效、安全、易于扩展的远程 LSP 开发环境。通过将 LSP 处理完全移到远程服务器，实现了真正的"远程计算，本地显示"的开发模式。