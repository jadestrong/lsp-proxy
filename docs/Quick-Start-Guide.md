# 🚀 LSP-Proxy 远程开发快速启动指南

## 10 分钟设置远程 LSP 开发环境

### 新推荐方式：SSH 管道模式（无需端口暴露）

#### 步骤 1: 本地配置

在**本地机器**上创建配置文件：

```bash
mkdir -p ~/.config/lsp-proxy
cat > ~/.config/lsp-proxy/remote-lsp.toml << EOF
[hosts.my-dev-server]
connection_type = "ssh_pipe"

[hosts.my-dev-server.ssh_pipe]
host = "your-remote-server.com"
user = "your-username"
identity_file = "~/.ssh/your_key"
server_install_script = '''
curl -fsSL https://raw.githubusercontent.com/jadestrong/lsp-proxy/main/scripts/server-install.sh | bash
'''

[host_mapping]
"your-remote-server.com" = "my-dev-server"
EOF
```

#### 步骤 2: 测试连接

```bash
# 确保可以 SSH 到远程服务器
ssh your-username@your-remote-server.com "echo 'SSH connection OK'"
```

#### 步骤 3: 在 Emacs 中使用

```elisp
;; 直接打开远程文件，系统会自动：
;; 1. 检测到这是远程文件
;; 2. 建立 SSH 管道连接
;; 3. 运行安装脚本（首次）
;; 4. 启动远程 lsp-proxy-server
;; 5. 提供完整 LSP 功能
C-x C-f /ssh:your-remote-server.com:/path/to/your/project/main.rs
```

**就是这么简单！** 无需配置端口、防火墙或手动安装任何东西。

---

### 传统方式：TCP/SSH 隧道模式（需要端口配置）

### 步骤 1: 编译安装 lsp-proxy-server

在**远程服务器**上执行：

```bash
# 克隆项目
git clone https://github.com/jadestrong/lsp-proxy.git
cd lsp-proxy

# 编译 lsp-proxy-server
cargo build --release --bin lsp-proxy-server

# 安装到系统路径
sudo cp target/release/lsp-proxy-server /usr/local/bin/
chmod +x /usr/local/bin/lsp-proxy-server
```

### 步骤 2: 配置远程服务器

创建配置文件：

```bash
mkdir -p ~/.config
cp lsp-proxy-server.toml.example ~/.config/lsp-proxy-server.toml
```

编辑配置文件，确保你有需要的 LSP 服务器：

```bash
# 安装常用 LSP 服务器
# Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup component add rust-analyzer

# Python
pip install 'python-lsp-server[all]'

# TypeScript/JavaScript
npm install -g typescript-language-server typescript

# Go
go install golang.org/x/tools/gopls@latest
```

### 步骤 3: 启动远程服务器

```bash
# 前台启动 (测试用)
lsp-proxy-server -c ~/.config/lsp-proxy-server.toml

# 后台启动
nohup lsp-proxy-server -c ~/.config/lsp-proxy-server.toml > /tmp/lsp-proxy-server.log 2>&1 &
```

### 步骤 4: 配置本地客户端

在**本地机器**上：

```bash
# 创建本地配置
mkdir -p ~/.config/lsp-proxy
cd /path/to/lsp-proxy
cp remote-lsp.toml.example ~/.config/lsp-proxy/remote-lsp.toml
```

编辑配置文件，替换为你的远程服务器信息：

```toml
[hosts.my-dev-server]
address = "your-remote-server.com:9527"
connection_type = "tcp"
```

### 步骤 5: 测试连接

```bash
# 测试远程服务器是否可达
telnet your-remote-server.com 9527

# 如果需要 SSH 隧道
ssh -L 9527:127.0.0.1:9527 -N -f user@your-remote-server.com
```

### 步骤 6: 在 Emacs 中使用

现在你可以直接打开远程文件：

```elisp
;; 打开远程 Rust 项目
C-x C-f /ssh:your-remote-server.com:/home/user/rust-project/src/main.rs

;; LSP 功能会自动启用！
;; - 代码补全: M-TAB
;; - 跳转定义: M-.
;; - 查看文档: C-c C-d
;; - 查找引用: M-?
```

## 🎯 验证设置

### 检查远程服务器状态

```bash
# 检查进程
ps aux | grep lsp-proxy-server

# 检查端口
netstat -tlnp | grep 9527

# 查看日志
tail -f /tmp/lsp-proxy-server.log
```

### 检查 LSP 功能

在 Emacs 中打开远程文件后：

1. **代码补全测试**：
   - 在 Rust 文件中输入 `Vec::`，应该看到补全提示

2. **跳转定义测试**：
   - 将光标放在函数名上，按 `M-.`，应该跳转到定义

3. **错误诊断测试**：
   - 故意写错语法，应该看到红色波浪线

4. **悬停信息测试**：
   - 将鼠标悬停在符号上，应该显示类型信息

## 🐛 常见问题解决

### 问题 1: 连接失败
```bash
# 检查防火墙
sudo ufw allow 9527

# 检查服务器是否运行
ps aux | grep lsp-proxy-server

# 测试网络连通性
ping your-remote-server.com
```

### 问题 2: LSP 服务器启动失败
```bash
# 检查 LSP 服务器是否安装
which rust-analyzer
which pylsp

# 手动测试 LSP 服务器
echo '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}' | rust-analyzer
```

### 问题 3: 权限问题
```bash
# 给 lsp-proxy-server 执行权限
chmod +x /usr/local/bin/lsp-proxy-server

# 检查配置文件权限
ls -la ~/.config/lsp-proxy-server.toml
```

## 🔧 高级配置

### SSH 隧道自动化

在 `~/.ssh/config` 中添加：

```
Host dev-server
    HostName your-remote-server.com
    User your-username
    Port 22
    LocalForward 9527 127.0.0.1:9527
    ControlMaster auto
    ControlPath ~/.ssh/sockets/ssh-%r@%h:%p
    ControlPersist 10m
```

然后只需要：

```bash
ssh dev-server -N -f  # 建立隧道
```

### 多项目配置

```toml
# 为不同项目配置不同的远程主机
[host_mapping]
"rust-server.com" = "rust-dev"
"python-server.com" = "python-dev" 
"js-server.com" = "js-dev"
```

### 性能优化

```toml
# 在 lsp-proxy-server.toml 中
max_lsp_servers = 3  # 限制同时运行的服务器数量

[lsp_servers.rust.env]
RUST_ANALYZER_NO_SYSROOT = "true"  # 减少内存使用
```

## 📊 监控和维护

### 创建 systemd 服务

```bash
sudo tee /etc/systemd/system/lsp-proxy-server.service > /dev/null <<EOF
[Unit]
Description=LSP-Proxy Server
After=network.target

[Service]
Type=simple
User=$(whoami)
WorkingDirectory=$HOME
ExecStart=/usr/local/bin/lsp-proxy-server -c $HOME/.config/lsp-proxy-server.toml
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
EOF

sudo systemctl daemon-reload
sudo systemctl enable lsp-proxy-server
sudo systemctl start lsp-proxy-server
```

### 日志轮转

```bash
# 配置 logrotate
sudo tee /etc/logrotate.d/lsp-proxy-server > /dev/null <<EOF
/tmp/lsp-proxy-server.log {
    daily
    missingok
    rotate 7
    compress
    notifempty
    create 644 $(whoami) $(whoami)
}
EOF
```

恭喜！🎉 你现在拥有了一个功能完整的远程 LSP 开发环境。你可以像编辑本地文件一样编辑远程文件，同时享受完整的 LSP 功能支持。