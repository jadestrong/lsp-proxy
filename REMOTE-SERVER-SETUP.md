# 远程服务器配置指南

本指南详细说明如何在远程服务器上配置环境以支持 lsp-proxy 远程开发功能。

## 服务器端架构

```
本地 Emacs + lsp-proxy
        ↓ SSH
远程服务器
    ├── lsp-proxy binary (可选)
    ├── Language Servers (必须)
    ├── SSH 配置
    ├── 开发工具链
    └── 项目文件
```

## 1. 基础环境准备

### 1.1 操作系统要求

支持的操作系统：
- Linux (Ubuntu 20.04+, CentOS 7+, Debian 10+)
- macOS (10.15+)
- Windows (WSL2)

### 1.2 必需的软件包

```bash
# Ubuntu/Debian
sudo apt update
sudo apt install -y curl git openssh-server build-essential

# CentOS/RHEL
sudo yum update -y
sudo yum install -y curl git openssh-server gcc gcc-c++ make

# macOS (使用 Homebrew)
brew install curl git openssh
```

## 2. SSH 配置

### 2.1 SSH 服务器配置

编辑 `/etc/ssh/sshd_config`:

```bash
# 基础安全配置
Port 22                          # 或自定义端口
PasswordAuthentication no        # 禁用密码登录 (推荐)
PubkeyAuthentication yes         # 启用密钥认证
PermitRootLogin no              # 禁用 root 登录
MaxStartups 10:30:100           # 限制并发连接

# 性能优化
ClientAliveInterval 60          # 保持连接活跃
ClientAliveCountMax 3           # 最大重试次数
TCPKeepAlive yes               # 启用 TCP keep-alive
Compression yes                # 启用压缩

# 连接复用支持
MaxSessions 10                 # 允许多个会话
```

重启 SSH 服务：
```bash
sudo systemctl reload sshd
```

### 2.2 用户账户配置

创建专用的开发用户：

```bash
# 创建用户
sudo useradd -m -s /bin/bash developer
sudo usermod -aG sudo developer  # 可选：添加 sudo 权限

# 设置 SSH 密钥
sudo -u developer mkdir -p /home/developer/.ssh
sudo -u developer chmod 700 /home/developer/.ssh

# 复制公钥到服务器
# 本地执行：ssh-copy-id developer@remote-server
# 或手动添加到 /home/developer/.ssh/authorized_keys
```

### 2.3 SSH 密钥优化

在用户 home 目录下创建 `~/.ssh/config`:

```bash
# 连接复用配置
Host *
    ServerAliveInterval 60
    ServerAliveCountMax 3
    ControlMaster auto
    ControlPath ~/.ssh/sockets/%r@%h-%p
    ControlPersist 600
    Compression yes

# 创建 socket 目录
mkdir -p ~/.ssh/sockets
chmod 700 ~/.ssh/sockets
```

## 3. Language Servers 安装

这是远程开发的核心要求 - 必须在远程服务器上安装所需的语言服务器。

### 3.1 通用安装脚本

创建 `~/install-language-servers.sh`:

```bash
#!/bin/bash

# Language Servers Installation Script
# 根据你的开发需求选择安装

set -e

echo "🚀 Installing Language Servers..."

# 创建工具目录
mkdir -p ~/bin ~/tools
export PATH="$HOME/bin:$PATH"

# =============================================================================
# Rust - rust-analyzer
# =============================================================================
install_rust_analyzer() {
    echo "📦 Installing Rust and rust-analyzer..."
    
    # 安装 Rust
    if ! command -v rustc &> /dev/null; then
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
        source ~/.cargo/env
    fi
    
    # 安装 rust-analyzer
    mkdir -p ~/.local/bin
    curl -L https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-x86_64-unknown-linux-gnu.gz | gunzip -c > ~/.local/bin/rust-analyzer
    chmod +x ~/.local/bin/rust-analyzer
    
    echo "✅ Rust analyzer installed"
}

# =============================================================================
# Python - pylsp/pyright
# =============================================================================
install_python_servers() {
    echo "📦 Installing Python language servers..."
    
    # 安装 Python 和 pip (如果没有)
    if ! command -v python3 &> /dev/null; then
        sudo apt install -y python3 python3-pip python3-venv
    fi
    
    # 安装 pylsp
    python3 -m pip install --user python-lsp-server[all]
    
    # 或者安装 pyright (二选一)
    if command -v npm &> /dev/null; then
        npm install -g pyright
    fi
    
    echo "✅ Python language servers installed"
}

# =============================================================================
# Node.js/TypeScript - typescript-language-server
# =============================================================================
install_typescript_server() {
    echo "📦 Installing TypeScript language server..."
    
    # 安装 Node.js (如果没有)
    if ! command -v node &> /dev/null; then
        # 使用 NodeSource repository
        curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
        sudo apt-get install -y nodejs
    fi
    
    # 安装 TypeScript language server
    npm install -g typescript typescript-language-server
    
    echo "✅ TypeScript language server installed"
}

# =============================================================================
# Go - gopls
# =============================================================================
install_go_server() {
    echo "📦 Installing Go language server..."
    
    # 安装 Go (如果没有)
    if ! command -v go &> /dev/null; then
        GO_VERSION="1.21.0"
        wget https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz
        sudo tar -C /usr/local -xzf go${GO_VERSION}.linux-amd64.tar.gz
        echo 'export PATH="/usr/local/go/bin:$PATH"' >> ~/.bashrc
        export PATH="/usr/local/go/bin:$PATH"
        rm go${GO_VERSION}.linux-amd64.tar.gz
    fi
    
    # 安装 gopls
    go install golang.org/x/tools/gopls@latest
    
    echo "✅ Go language server installed"
}

# =============================================================================
# C/C++ - clangd
# =============================================================================
install_clangd() {
    echo "📦 Installing clangd..."
    
    # Ubuntu/Debian
    if command -v apt &> /dev/null; then
        sudo apt install -y clangd-12
        sudo update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-12 100
    # CentOS/RHEL
    elif command -v yum &> /dev/null; then
        sudo yum install -y clang-tools-extra
    fi
    
    echo "✅ clangd installed"
}

# =============================================================================
# Java - eclipse.jdt.ls
# =============================================================================
install_java_server() {
    echo "📦 Installing Java language server..."
    
    # 安装 Java JDK
    sudo apt install -y openjdk-17-jdk
    
    # 下载 eclipse.jdt.ls
    JDTLS_VERSION="1.26.0"
    mkdir -p ~/tools/jdt-language-server
    cd ~/tools/jdt-language-server
    wget https://download.eclipse.org/jdtls/snapshots/jdt-language-server-${JDTLS_VERSION}-202307271613.tar.gz
    tar xf jdt-language-server-${JDTLS_VERSION}-*.tar.gz
    rm jdt-language-server-${JDTLS_VERSION}-*.tar.gz
    
    # 创建启动脚本
    cat > ~/bin/jdtls << 'EOF'
#!/bin/bash
java \
    -Declipse.application=org.eclipse.jdt.ls.core.id1 \
    -Dosgi.bundles.defaultStartLevel=4 \
    -Declipse.product=org.eclipse.jdt.ls.core.product \
    -Dlog.protocol=true \
    -Dlog.level=ALL \
    -jar ~/tools/jdt-language-server/plugins/org.eclipse.jdt.ls.core_*.jar \
    -configuration ~/tools/jdt-language-server/config_linux \
    -data "${1:-$HOME/workspace}" \
    --add-modules=ALL-SYSTEM \
    --add-opens java.base/java.util=ALL-UNNAMED \
    --add-opens java.base/java.lang=ALL-UNNAMED
EOF
    chmod +x ~/bin/jdtls
    
    echo "✅ Java language server installed"
}

# =============================================================================
# 主安装流程
# =============================================================================

echo "请选择要安装的语言服务器 (多选用空格分隔):"
echo "1) rust-analyzer"
echo "2) Python (pylsp)"
echo "3) TypeScript"
echo "4) Go (gopls)"
echo "5) C/C++ (clangd)"
echo "6) Java (jdtls)"
echo "7) 全部安装"

read -p "输入选择 (1-7): " choices

for choice in $choices; do
    case $choice in
        1) install_rust_analyzer ;;
        2) install_python_servers ;;
        3) install_typescript_server ;;
        4) install_go_server ;;
        5) install_clangd ;;
        6) install_java_server ;;
        7) 
            install_rust_analyzer
            install_python_servers
            install_typescript_server
            install_go_server
            install_clangd
            install_java_server
            ;;
        *) echo "未知选择: $choice" ;;
    esac
done

# 更新 PATH
echo 'export PATH="$HOME/.local/bin:$HOME/bin:$PATH"' >> ~/.bashrc
echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> ~/.bashrc

echo ""
echo "🎉 语言服务器安装完成!"
echo "请运行 'source ~/.bashrc' 或重新登录以更新 PATH"
echo ""
echo "已安装的语言服务器:"
command -v rust-analyzer && echo "✅ rust-analyzer: $(rust-analyzer --version)"
command -v pylsp && echo "✅ pylsp: $(pylsp --version)"
command -v typescript-language-server && echo "✅ typescript-language-server: $(typescript-language-server --version)"
command -v gopls && echo "✅ gopls: $(gopls version)"
command -v clangd && echo "✅ clangd: $(clangd --version)"
command -v java && echo "✅ Java: $(java -version 2>&1 | head -n1)"
```

使脚本可执行并运行：
```bash
chmod +x ~/install-language-servers.sh
./install-language-servers.sh
```

### 3.2 验证语言服务器安装

创建验证脚本 `~/verify-servers.sh`:

```bash
#!/bin/bash

echo "🔍 验证语言服务器安装..."

check_server() {
    local name=$1
    local cmd=$2
    local args=${3:-"--version"}
    
    if command -v "$cmd" &> /dev/null; then
        echo "✅ $name: 已安装"
        if [[ "$args" != "skip" ]]; then
            echo "   版本: $($cmd $args 2>&1 | head -n1)"
        fi
        return 0
    else
        echo "❌ $name: 未安装"
        return 1
    fi
}

echo ""
check_server "rust-analyzer" "rust-analyzer" "--version"
check_server "pylsp" "pylsp" "--version"
check_server "typescript-language-server" "typescript-language-server" "--version"
check_server "gopls" "gopls" "version"
check_server "clangd" "clangd" "--version"
check_server "java" "java" "-version"

echo ""
echo "🏠 环境变量:"
echo "PATH=$PATH"
echo ""

# 测试语言服务器是否能正常启动
test_server() {
    local name=$1
    local cmd=$2
    
    echo "🧪 测试 $name..."
    if timeout 5 $cmd --help &> /dev/null; then
        echo "✅ $name 可以正常启动"
    else
        echo "⚠️  $name 可能存在问题"
    fi
}

echo "🧪 功能测试:"
command -v rust-analyzer &> /dev/null && test_server "rust-analyzer" "rust-analyzer"
command -v pylsp &> /dev/null && test_server "pylsp" "pylsp"
command -v typescript-language-server &> /dev/null && test_server "typescript-language-server" "typescript-language-server"
command -v gopls &> /dev/null && test_server "gopls" "gopls"
command -v clangd &> /dev/null && test_server "clangd" "clangd"
```

## 4. 开发环境配置

### 4.1 创建工作空间结构

```bash
# 创建标准的开发目录结构
mkdir -p ~/projects/{personal,work,experiments}
mkdir -p ~/tools/scripts
mkdir -p ~/.config/lsp-proxy

# 设置权限
chmod 755 ~/projects
chmod 700 ~/.config
```

### 4.2 配置 shell 环境

在 `~/.bashrc` 或 `~/.zshrc` 中添加：

```bash
# 开发环境配置
export EDITOR=vim                    # 或你喜欢的编辑器
export TERM=xterm-256color          # 支持颜色
export LC_ALL=en_US.UTF-8           # 字符编码

# 语言服务器路径
export PATH="$HOME/.local/bin:$HOME/bin:$HOME/.cargo/bin:$PATH"

# Go 配置 (如果使用 Go)
export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:/usr/local/go/bin:$PATH"

# Node.js 配置 (如果使用 Node.js)
export NODE_PATH="/usr/local/lib/node_modules:$NODE_PATH"

# Python 配置
export PYTHONPATH="$HOME/.local/lib/python3.*/site-packages:$PYTHONPATH"

# 别名
alias ll='ls -la'
alias la='ls -A'
alias l='ls -CF'
alias ..='cd ..'
alias ...='cd ../..'

# 开发相关别名
alias gs='git status'
alias gd='git diff'
alias gl='git log --oneline'

# LSP 相关函数
lsp_status() {
    echo "🔍 检查语言服务器状态..."
    pgrep -f "rust-analyzer|pylsp|typescript-language-server|gopls|clangd" | wc -l
}

lsp_restart() {
    echo "🔄 重启语言服务器..."
    pkill -f "rust-analyzer|pylsp|typescript-language-server|gopls|clangd"
}
```

### 4.3 Git 配置

```bash
# 配置 Git (如果还没配置)
git config --global user.name "Your Name"
git config --global user.email "your.email@example.com"
git config --global init.defaultBranch main
git config --global pull.rebase false
```

## 5. 性能优化

### 5.1 系统优化

```bash
# 增加文件监控限制 (对于大型项目)
echo 'fs.inotify.max_user_watches=524288' | sudo tee -a /etc/sysctl.conf
sudo sysctl -p

# 增加文件描述符限制
echo '* soft nofile 65536' | sudo tee -a /etc/security/limits.conf
echo '* hard nofile 65536' | sudo tee -a /etc/security/limits.conf
```

### 5.2 SSH 连接池优化

在本地 `~/.ssh/config` 中添加：

```bash
Host remote-server
    HostName your-server.com
    User developer
    Port 22
    IdentityFile ~/.ssh/id_rsa
    
    # 连接复用
    ControlMaster auto
    ControlPath ~/.ssh/sockets/%r@%h-%p
    ControlPersist 600
    
    # 性能优化
    Compression yes
    ServerAliveInterval 60
    ServerAliveCountMax 3
    TCPKeepAlive yes
    
    # 安全设置
    ForwardAgent no
    ForwardX11 no
```

## 6. 安全配置

### 6.1 防火墙配置

```bash
# Ubuntu/Debian (ufw)
sudo ufw allow 22/tcp
sudo ufw enable

# CentOS/RHEL (firewalld)
sudo firewall-cmd --permanent --add-service=ssh
sudo firewall-cmd --reload
```

### 6.2 SSH 安全加固

```bash
# 定期检查登录日志
sudo tail -f /var/log/auth.log    # Ubuntu/Debian
sudo tail -f /var/log/secure      # CentOS/RHEL

# 安装 fail2ban (可选)
sudo apt install fail2ban         # Ubuntu/Debian
sudo systemctl enable fail2ban
sudo systemctl start fail2ban
```

### 6.3 备份重要配置

```bash
# 备份关键配置文件
mkdir -p ~/backups/config
cp ~/.bashrc ~/backups/config/
cp ~/.ssh/authorized_keys ~/backups/config/
cp -r ~/.config ~/backups/
```

## 7. 测试配置

### 7.1 创建测试项目

```bash
# 创建测试项目
mkdir -p ~/projects/test
cd ~/projects/test

# Rust 测试项目
cargo init --name test-rust
echo 'fn main() { println!("Hello from remote Rust!"); }' > src/main.rs

# Python 测试项目
mkdir python-test && cd python-test
echo 'print("Hello from remote Python!")' > main.py
cd ..

# 测试 TypeScript 项目 (如果安装了)
if command -v npm &> /dev/null; then
    mkdir ts-test && cd ts-test
    npm init -y
    echo 'console.log("Hello from remote TypeScript!");' > index.ts
    cd ..
fi
```

### 7.2 连接测试

从本地测试远程连接：

```bash
# 测试 SSH 连接
ssh remote-server "echo 'SSH connection successful'"

# 测试语言服务器
ssh remote-server "rust-analyzer --version"
ssh remote-server "pylsp --version"
ssh remote-server "which typescript-language-server"

# 测试文件操作
ssh remote-server "ls -la ~/projects/test"
```

## 8. 故障排除

### 8.1 常见问题

1. **SSH 连接失败**
   ```bash
   # 检查 SSH 服务状态
   sudo systemctl status ssh
   
   # 检查端口监听
   sudo ss -tlnp | grep :22
   
   # 检查防火墙
   sudo ufw status
   ```

2. **语言服务器无法启动**
   ```bash
   # 检查路径
   echo $PATH
   which rust-analyzer
   
   # 检查权限
   ls -la ~/.local/bin/rust-analyzer
   
   # 手动测试
   rust-analyzer --version
   ```

3. **性能问题**
   ```bash
   # 检查系统资源
   htop
   df -h
   free -h
   
   # 检查网络延迟
   ping your-local-machine
   ```

### 8.2 日志检查

```bash
# SSH 日志
sudo tail -f /var/log/auth.log

# 系统日志
journalctl -f

# 创建调试脚本
cat > ~/debug-lsp.sh << 'EOF'
#!/bin/bash
echo "=== LSP Debug Information ==="
echo "Date: $(date)"
echo "Hostname: $(hostname)"
echo "User: $(whoami)"
echo "Working directory: $(pwd)"
echo ""
echo "=== Environment ==="
echo "PATH=$PATH"
echo ""
echo "=== Language Servers ==="
echo "rust-analyzer: $(command -v rust-analyzer || echo 'Not found')"
echo "pylsp: $(command -v pylsp || echo 'Not found')"
echo "typescript-language-server: $(command -v typescript-language-server || echo 'Not found')"
echo ""
echo "=== System Resources ==="
echo "Memory: $(free -h | grep Mem)"
echo "Disk: $(df -h / | tail -1)"
echo "Load: $(uptime)"
echo ""
echo "=== Network ==="
echo "Listening ports: $(ss -tlnp | grep :22)"
EOF

chmod +x ~/debug-lsp.sh
```

## 9. 维护和更新

### 9.1 自动更新脚本

```bash
cat > ~/update-language-servers.sh << 'EOF'
#!/bin/bash
echo "🔄 更新语言服务器..."

# 更新 Rust 工具链
if command -v rustup &> /dev/null; then
    rustup update
    rustup component add rust-analyzer
fi

# 更新 Python 包
if command -v pip3 &> /dev/null; then
    pip3 install --user --upgrade python-lsp-server
fi

# 更新 Node.js 包
if command -v npm &> /dev/null; then
    npm update -g typescript typescript-language-server
fi

# 更新 Go 工具
if command -v go &> /dev/null; then
    go install golang.org/x/tools/gopls@latest
fi

echo "✅ 更新完成"
EOF

chmod +x ~/update-language-servers.sh

# 设置定期更新 (可选)
echo "0 2 * * 1 ~/update-language-servers.sh" | crontab -
```

### 9.2 监控脚本

```bash
cat > ~/monitor-lsp.sh << 'EOF'
#!/bin/bash
# 简单的 LSP 服务监控

LOG_FILE=~/logs/lsp-monitor.log
mkdir -p ~/logs

log_message() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" >> "$LOG_FILE"
}

# 检查语言服务器进程
check_servers() {
    local servers=("rust-analyzer" "pylsp" "typescript-language-server" "gopls" "clangd")
    for server in "${servers[@]}"; do
        if pgrep -f "$server" > /dev/null; then
            log_message "✅ $server is running"
        else
            log_message "❌ $server is not running"
        fi
    done
}

log_message "Starting LSP monitoring check"
check_servers
log_message "LSP monitoring check completed"
EOF

chmod +x ~/monitor-lsp.sh
```

完成这些配置后，你的远程服务器就准备好支持 lsp-proxy 远程开发了！

记住测试所有功能并根据你的具体需求调整配置。