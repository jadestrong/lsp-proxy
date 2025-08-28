#!/bin/bash

# LSP-Proxy 远程服务器自动配置脚本
# 这个脚本会在远程服务器上配置完整的开发环境

set -e

# 配置变量
SCRIPT_VERSION="1.0.0"
CONFIG_DIR="$HOME/.config/lsp-proxy"
TOOLS_DIR="$HOME/tools"
BIN_DIR="$HOME/.local/bin"

# 颜色输出
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
NC='\033[0m' # No Color

# 日志函数
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_step() {
    echo -e "${PURPLE}[STEP]${NC} $1"
}

# 显示横幅
show_banner() {
    echo -e "${BLUE}"
    cat << 'EOF'
╔══════════════════════════════════════════════════════════════╗
║                 LSP-Proxy Remote Server Setup               ║
║                                                              ║
║  This script will configure your remote server for          ║
║  LSP-Proxy remote development                                ║
╚══════════════════════════════════════════════════════════════╝
EOF
    echo -e "${NC}"
    echo "Version: $SCRIPT_VERSION"
    echo ""
}

# 检测系统信息
detect_system() {
    log_step "Detecting system information..."
    
    # 检测操作系统
    if [[ -f /etc/os-release ]]; then
        . /etc/os-release
        OS=$NAME
        VERSION=$VERSION_ID
    else
        OS=$(uname -s)
        VERSION="Unknown"
    fi
    
    # 检测架构
    ARCH=$(uname -m)
    
    log_info "Operating System: $OS $VERSION"
    log_info "Architecture: $ARCH"
    log_info "Kernel: $(uname -r)"
    log_info "Shell: $SHELL"
    echo ""
}

# 检查先决条件
check_prerequisites() {
    log_step "Checking prerequisites..."
    
    local missing_tools=()
    local required_tools=("curl" "git" "wget")
    
    for tool in "${required_tools[@]}"; do
        if ! command -v "$tool" &> /dev/null; then
            missing_tools+=("$tool")
        fi
    done
    
    if [[ ${#missing_tools[@]} -gt 0 ]]; then
        log_warning "Missing required tools: ${missing_tools[*]}"
        install_prerequisites "${missing_tools[@]}"
    else
        log_success "All prerequisites satisfied"
    fi
    echo ""
}

# 安装先决条件
install_prerequisites() {
    log_step "Installing prerequisites..."
    
    if command -v apt &> /dev/null; then
        # Debian/Ubuntu
        sudo apt update
        sudo apt install -y curl git wget build-essential "$@"
    elif command -v yum &> /dev/null; then
        # CentOS/RHEL
        sudo yum update -y
        sudo yum install -y curl git wget gcc gcc-c++ make "$@"
    elif command -v dnf &> /dev/null; then
        # Fedora
        sudo dnf install -y curl git wget gcc gcc-c++ make "$@"
    elif command -v brew &> /dev/null; then
        # macOS
        brew install "$@"
    else
        log_error "Unsupported package manager. Please install manually: $*"
        exit 1
    fi
    
    log_success "Prerequisites installed"
}

# 创建目录结构
setup_directories() {
    log_step "Setting up directory structure..."
    
    local directories=(
        "$CONFIG_DIR"
        "$TOOLS_DIR"
        "$BIN_DIR"
        "$HOME/projects/personal"
        "$HOME/projects/work"
        "$HOME/projects/experiments"
        "$HOME/logs"
        "$HOME/.ssh/sockets"
    )
    
    for dir in "${directories[@]}"; do
        mkdir -p "$dir"
        log_info "Created: $dir"
    done
    
    # 设置权限
    chmod 700 "$HOME/.ssh/sockets"
    chmod 755 "$HOME/projects"
    chmod 700 "$CONFIG_DIR"
    
    log_success "Directory structure created"
    echo ""
}

# 配置 SSH
configure_ssh() {
    log_step "Configuring SSH..."
    
    # 检查 SSH 密钥
    if [[ ! -f "$HOME/.ssh/authorized_keys" ]]; then
        log_warning "No SSH keys found. Please add your public key to ~/.ssh/authorized_keys"
        mkdir -p "$HOME/.ssh"
        chmod 700 "$HOME/.ssh"
        touch "$HOME/.ssh/authorized_keys"
        chmod 600 "$HOME/.ssh/authorized_keys"
        
        echo ""
        log_info "To add your public key, run on your local machine:"
        echo "    ssh-copy-id $(whoami)@$(hostname -I | awk '{print $1}')"
        echo ""
        read -p "Press Enter after adding your SSH key..."
    fi
    
    # 配置 SSH 客户端
    if [[ ! -f "$HOME/.ssh/config" ]]; then
        cat > "$HOME/.ssh/config" << 'EOF'
# SSH client configuration for remote development

Host *
    ServerAliveInterval 60
    ServerAliveCountMax 3
    ControlMaster auto
    ControlPath ~/.ssh/sockets/%r@%h-%p
    ControlPersist 600
    Compression yes
    ForwardAgent no
    ForwardX11 no
EOF
        chmod 600 "$HOME/.ssh/config"
        log_info "Created SSH client configuration"
    fi
    
    log_success "SSH configuration completed"
    echo ""
}

# 安装 Rust 和 rust-analyzer
install_rust() {
    log_step "Installing Rust and rust-analyzer..."
    
    if command -v rustc &> /dev/null; then
        log_info "Rust already installed: $(rustc --version)"
    else
        log_info "Installing Rust..."
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
        source "$HOME/.cargo/env"
    fi
    
    # 安装 rust-analyzer
    log_info "Installing rust-analyzer..."
    if [[ "$ARCH" == "x86_64" ]]; then
        if [[ "$OS" == *"Darwin"* ]]; then
            RUST_ANALYZER_URL="https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-x86_64-apple-darwin.gz"
        else
            RUST_ANALYZER_URL="https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-x86_64-unknown-linux-gnu.gz"
        fi
    elif [[ "$ARCH" == "aarch64" ]] || [[ "$ARCH" == "arm64" ]]; then
        if [[ "$OS" == *"Darwin"* ]]; then
            RUST_ANALYZER_URL="https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-aarch64-apple-darwin.gz"
        else
            RUST_ANALYZER_URL="https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-aarch64-unknown-linux-gnu.gz"
        fi
    else
        log_warning "Unsupported architecture for rust-analyzer: $ARCH"
        return 1
    fi
    
    curl -L "$RUST_ANALYZER_URL" | gunzip -c > "$BIN_DIR/rust-analyzer"
    chmod +x "$BIN_DIR/rust-analyzer"
    
    log_success "Rust and rust-analyzer installed"
    echo ""
}

# 安装 Python 语言服务器
install_python_lsp() {
    log_step "Installing Python language server..."
    
    # 确保 Python3 和 pip 已安装
    if ! command -v python3 &> /dev/null; then
        log_info "Installing Python3..."
        if command -v apt &> /dev/null; then
            sudo apt install -y python3 python3-pip python3-venv
        elif command -v yum &> /dev/null; then
            sudo yum install -y python3 python3-pip
        elif command -v dnf &> /dev/null; then
            sudo dnf install -y python3 python3-pip
        fi
    fi
    
    # 安装 python-lsp-server
    log_info "Installing python-lsp-server..."
    python3 -m pip install --user --upgrade pip
    python3 -m pip install --user python-lsp-server[all]
    
    log_success "Python language server installed"
    echo ""
}

# 安装 Node.js 和 TypeScript 语言服务器
install_nodejs_lsp() {
    log_step "Installing Node.js and TypeScript language server..."
    
    # 安装 Node.js
    if ! command -v node &> /dev/null; then
        log_info "Installing Node.js..."
        
        if command -v apt &> /dev/null; then
            # Ubuntu/Debian
            curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
            sudo apt-get install -y nodejs
        elif command -v yum &> /dev/null; then
            # CentOS/RHEL
            curl -fsSL https://rpm.nodesource.com/setup_18.x | sudo bash -
            sudo yum install -y nodejs npm
        elif command -v brew &> /dev/null; then
            # macOS
            brew install node
        else
            log_warning "Please install Node.js manually"
            return 1
        fi
    fi
    
    # 安装 TypeScript 语言服务器
    log_info "Installing TypeScript language server..."
    npm install -g typescript typescript-language-server
    
    log_success "Node.js and TypeScript language server installed"
    echo ""
}

# 安装 Go 语言服务器
install_go_lsp() {
    log_step "Installing Go and gopls..."
    
    if ! command -v go &> /dev/null; then
        log_info "Installing Go..."
        
        GO_VERSION="1.21.5"
        if [[ "$ARCH" == "x86_64" ]]; then
            GO_ARCH="amd64"
        elif [[ "$ARCH" == "aarch64" ]] || [[ "$ARCH" == "arm64" ]]; then
            GO_ARCH="arm64"
        else
            log_warning "Unsupported architecture for Go: $ARCH"
            return 1
        fi
        
        if [[ "$OS" == *"Darwin"* ]]; then
            GO_OS="darwin"
        else
            GO_OS="linux"
        fi
        
        GO_TARBALL="go${GO_VERSION}.${GO_OS}-${GO_ARCH}.tar.gz"
        wget "https://go.dev/dl/${GO_TARBALL}"
        sudo tar -C /usr/local -xzf "${GO_TARBALL}"
        rm "${GO_TARBALL}"
        
        # 添加到 PATH
        echo 'export PATH="/usr/local/go/bin:$PATH"' >> "$HOME/.bashrc"
        export PATH="/usr/local/go/bin:$PATH"
    fi
    
    # 安装 gopls
    log_info "Installing gopls..."
    go install golang.org/x/tools/gopls@latest
    
    log_success "Go and gopls installed"
    echo ""
}

# 安装 C/C++ 语言服务器
install_clangd() {
    log_step "Installing clangd..."
    
    if command -v apt &> /dev/null; then
        # Ubuntu/Debian
        sudo apt install -y clangd
    elif command -v yum &> /dev/null; then
        # CentOS/RHEL
        sudo yum install -y clang-tools-extra
    elif command -v dnf &> /dev/null; then
        # Fedora
        sudo dnf install -y clang-tools-extra
    elif command -v brew &> /dev/null; then
        # macOS
        brew install llvm
    else
        log_warning "Please install clangd manually"
        return 1
    fi
    
    log_success "clangd installed"
    echo ""
}

# 配置环境变量
configure_environment() {
    log_step "Configuring environment variables..."
    
    # 备份现有的 bashrc
    if [[ -f "$HOME/.bashrc" ]]; then
        cp "$HOME/.bashrc" "$HOME/.bashrc.backup.$(date +%Y%m%d_%H%M%S)"
    fi
    
    # 添加环境配置
    cat >> "$HOME/.bashrc" << 'EOF'

# === LSP-Proxy Remote Development Configuration ===
# Added by lsp-proxy setup script

# Development tools paths
export PATH="$HOME/.local/bin:$HOME/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/usr/local/go/bin:$HOME/go/bin:$PATH"

# Development environment
export EDITOR=vim
export TERM=xterm-256color
export LC_ALL=en_US.UTF-8

# Language-specific configurations
export GOPATH="$HOME/go"
export PYTHONPATH="$HOME/.local/lib/python3.*/site-packages:$PYTHONPATH"

# Aliases for development
alias ll='ls -la'
alias la='ls -A'
alias l='ls -CF'
alias ..='cd ..'
alias ...='cd ../..'
alias gs='git status'
alias gd='git diff'
alias gl='git log --oneline'

# LSP utilities
lsp_status() {
    echo "🔍 LSP Server Status:"
    echo "Rust analyzer: $(pgrep -c rust-analyzer || echo 0) processes"
    echo "Python LSP: $(pgrep -c pylsp || echo 0) processes" 
    echo "TypeScript LSP: $(pgrep -c typescript-language-server || echo 0) processes"
    echo "Gopls: $(pgrep -c gopls || echo 0) processes"
    echo "Clangd: $(pgrep -c clangd || echo 0) processes"
}

lsp_restart() {
    echo "🔄 Restarting LSP servers..."
    pkill -f "rust-analyzer|pylsp|typescript-language-server|gopls|clangd" 2>/dev/null || true
    sleep 2
    echo "✅ LSP servers restarted"
}

lsp_update() {
    echo "🔄 Updating language servers..."
    
    # Update Rust toolchain
    if command -v rustup &> /dev/null; then
        rustup update
    fi
    
    # Update Python packages
    if command -v pip3 &> /dev/null; then
        pip3 install --user --upgrade python-lsp-server
    fi
    
    # Update Node.js packages
    if command -v npm &> /dev/null; then
        npm update -g typescript typescript-language-server
    fi
    
    # Update Go tools
    if command -v go &> /dev/null; then
        go install golang.org/x/tools/gopls@latest
    fi
    
    echo "✅ Language servers updated"
}

EOF
    
    log_success "Environment configuration added"
    echo ""
}

# 创建测试项目
create_test_projects() {
    log_step "Creating test projects..."
    
    local test_dir="$HOME/projects/lsp-proxy-tests"
    mkdir -p "$test_dir"
    cd "$test_dir"
    
    # Rust 测试项目
    if command -v cargo &> /dev/null || [[ -f "$HOME/.cargo/bin/cargo" ]]; then
        log_info "Creating Rust test project..."
        source "$HOME/.cargo/env" 2>/dev/null || true
        cargo init --name rust-test --bin rust-test 2>/dev/null || mkdir -p rust-test/src
        cat > rust-test/src/main.rs << 'EOF'
fn main() {
    println!("🦀 Hello from remote Rust!");
    
    // 测试一些语言服务器功能
    let numbers = vec![1, 2, 3, 4, 5];
    let sum: i32 = numbers.iter().sum();
    println!("Sum: {}", sum);
    
    test_function();
}

fn test_function() {
    println!("🚀 LSP should provide completion and hover info here!");
}
EOF
    fi
    
    # Python 测试项目
    log_info "Creating Python test project..."
    mkdir -p python-test
    cat > python-test/main.py << 'EOF'
#!/usr/bin/env python3
"""
🐍 Python test project for LSP-Proxy remote development
"""

import os
import sys
from typing import List, Dict, Any


def main() -> None:
    """Main function to test Python LSP features."""
    print("🐍 Hello from remote Python!")
    
    # 测试类型提示和自动完成
    numbers: List[int] = [1, 2, 3, 4, 5]
    result = calculate_sum(numbers)
    print(f"Sum: {result}")
    
    # 测试字典类型
    config: Dict[str, Any] = {
        "name": "LSP-Proxy",
        "version": "1.0.0",
        "features": ["remote-development", "lsp-support"]
    }
    
    print_config(config)


def calculate_sum(numbers: List[int]) -> int:
    """Calculate sum of numbers - LSP should provide hover info."""
    return sum(numbers)


def print_config(config: Dict[str, Any]) -> None:
    """Print configuration - test docstring and type hints."""
    for key, value in config.items():
        print(f"{key}: {value}")


if __name__ == "__main__":
    main()
EOF
    
    # TypeScript 测试项目
    if command -v npm &> /dev/null; then
        log_info "Creating TypeScript test project..."
        mkdir -p typescript-test
        cd typescript-test
        cat > package.json << 'EOF'
{
  "name": "lsp-proxy-typescript-test",
  "version": "1.0.0",
  "description": "TypeScript test project for LSP-Proxy",
  "main": "index.js",
  "scripts": {
    "build": "tsc",
    "start": "node index.js"
  },
  "dependencies": {},
  "devDependencies": {
    "typescript": "^5.0.0"
  }
}
EOF
        
        cat > tsconfig.json << 'EOF'
{
  "compilerOptions": {
    "target": "ES2020",
    "module": "commonjs",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "outDir": "./dist",
    "rootDir": "./src"
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist"]
}
EOF
        
        mkdir -p src
        cat > src/index.ts << 'EOF'
/**
 * 🟦 TypeScript test project for LSP-Proxy remote development
 */

interface Config {
    name: string;
    version: string;
    features: string[];
}

class RemoteDevTester {
    private config: Config;
    
    constructor(config: Config) {
        this.config = config;
    }
    
    public run(): void {
        console.log("🟦 Hello from remote TypeScript!");
        console.log(`Project: ${this.config.name} v${this.config.version}`);
        
        const numbers = [1, 2, 3, 4, 5];
        const sum = this.calculateSum(numbers);
        console.log(`Sum: ${sum}`);
        
        this.printFeatures();
    }
    
    private calculateSum(numbers: number[]): number {
        return numbers.reduce((acc, num) => acc + num, 0);
    }
    
    private printFeatures(): void {
        console.log("Features:");
        this.config.features.forEach((feature, index) => {
            console.log(`  ${index + 1}. ${feature}`);
        });
    }
}

// Main execution
const config: Config = {
    name: "LSP-Proxy",
    version: "1.0.0",
    features: ["remote-development", "lsp-support", "type-checking"]
};

const tester = new RemoteDevTester(config);
tester.run();
EOF
        cd ..
    fi
    
    # Go 测试项目
    if command -v go &> /dev/null || [[ -d "/usr/local/go" ]]; then
        log_info "Creating Go test project..."
        export PATH="/usr/local/go/bin:$PATH"
        mkdir -p go-test
        cd go-test
        cat > main.go << 'EOF'
package main

import (
	"fmt"
	"log"
)

// Config represents the application configuration
type Config struct {
	Name     string   `json:"name"`
	Version  string   `json:"version"`
	Features []string `json:"features"`
}

func main() {
	fmt.Println("🐹 Hello from remote Go!")
	
	config := Config{
		Name:     "LSP-Proxy",
		Version:  "1.0.0",
		Features: []string{"remote-development", "lsp-support", "go-modules"},
	}
	
	printConfig(config)
	
	numbers := []int{1, 2, 3, 4, 5}
	sum := calculateSum(numbers)
	fmt.Printf("Sum: %d\n", sum)
}

// calculateSum calculates the sum of integers
func calculateSum(numbers []int) int {
	sum := 0
	for _, num := range numbers {
		sum += num
	}
	return sum
}

// printConfig prints the configuration
func printConfig(config Config) {
	fmt.Printf("Project: %s v%s\n", config.Name, config.Version)
	fmt.Println("Features:")
	for i, feature := range config.Features {
		fmt.Printf("  %d. %s\n", i+1, feature)
	}
}
EOF
        go mod init lsp-proxy-go-test 2>/dev/null || true
        cd ..
    fi
    
    cd "$HOME"
    log_success "Test projects created in $test_dir"
    echo ""
}

# 验证安装
verify_installation() {
    log_step "Verifying installation..."
    
    local servers_found=0
    local total_servers=5
    
    echo "🔍 Language Server Status:"
    
    # 检查 rust-analyzer
    if command -v rust-analyzer &> /dev/null; then
        echo "  ✅ rust-analyzer: $(rust-analyzer --version 2>/dev/null | head -n1 || echo 'installed')"
        ((servers_found++))
    else
        echo "  ❌ rust-analyzer: not found"
    fi
    
    # 检查 pylsp
    if command -v pylsp &> /dev/null; then
        echo "  ✅ pylsp: $(pylsp --version 2>/dev/null | head -n1 || echo 'installed')"
        ((servers_found++))
    else
        echo "  ❌ pylsp: not found"
    fi
    
    # 检查 typescript-language-server
    if command -v typescript-language-server &> /dev/null; then
        echo "  ✅ typescript-language-server: $(typescript-language-server --version 2>/dev/null || echo 'installed')"
        ((servers_found++))
    else
        echo "  ❌ typescript-language-server: not found"
    fi
    
    # 检查 gopls
    if command -v gopls &> /dev/null; then
        echo "  ✅ gopls: $(gopls version 2>/dev/null | head -n1 || echo 'installed')"
        ((servers_found++))
    else
        echo "  ❌ gopls: not found"
    fi
    
    # 检查 clangd
    if command -v clangd &> /dev/null; then
        echo "  ✅ clangd: $(clangd --version 2>/dev/null | head -n1 || echo 'installed')"
        ((servers_found++))
    else
        echo "  ❌ clangd: not found"
    fi
    
    echo ""
    echo "📊 Summary: $servers_found/$total_servers language servers installed"
    
    if [[ $servers_found -gt 0 ]]; then
        log_success "Installation verification completed"
    else
        log_warning "No language servers found. Please check the installation."
    fi
    echo ""
}

# 显示完成信息
show_completion_info() {
    log_step "Setup completed!"
    
    echo -e "${GREEN}"
    cat << 'EOF'
╔══════════════════════════════════════════════════════════════╗
║                    🎉 Setup Complete! 🎉                     ║
╚══════════════════════════════════════════════════════════════╝
EOF
    echo -e "${NC}"
    
    echo "Your remote server is now configured for LSP-Proxy remote development!"
    echo ""
    echo "📋 Next Steps:"
    echo "1. Reload your shell environment:"
    echo "   source ~/.bashrc"
    echo ""
    echo "2. Test the installation:"
    echo "   lsp_status    # Check language server status"
    echo "   lsp_update    # Update language servers"
    echo ""
    echo "3. On your local machine, configure the remote server in:"
    echo "   ~/.config/lsp-proxy/remote.toml"
    echo ""
    echo "4. Test projects are available in:"
    echo "   ~/projects/lsp-proxy-tests/"
    echo ""
    echo "🔧 Useful commands:"
    echo "   lsp_status     - Check LSP server processes"
    echo "   lsp_restart    - Restart all LSP servers"
    echo "   lsp_update     - Update language servers"
    echo ""
    echo "📁 Important directories:"
    echo "   ~/.config/lsp-proxy/   - Configuration"
    echo "   ~/.local/bin/          - User binaries"
    echo "   ~/projects/            - Development projects"
    echo "   ~/logs/                - Log files"
    echo ""
    echo "🔗 Connection info:"
    echo "   Hostname: $(hostname)"
    echo "   IP Address: $(hostname -I | awk '{print $1}' || echo 'unknown')"
    echo "   SSH Port: 22 (default)"
    echo "   Username: $(whoami)"
    echo ""
    echo "Happy remote coding! 🚀"
}

# 主函数
main() {
    show_banner
    detect_system
    check_prerequisites
    setup_directories
    configure_ssh
    
    echo "📦 Select language servers to install:"
    echo "1) Rust (rust-analyzer)"
    echo "2) Python (pylsp)"
    echo "3) TypeScript/JavaScript (typescript-language-server)"
    echo "4) Go (gopls)" 
    echo "5) C/C++ (clangd)"
    echo "6) All of the above"
    echo ""
    
    read -p "Enter your choice (1-6, or multiple numbers separated by space): " choices
    
    for choice in $choices; do
        case $choice in
            1) install_rust ;;
            2) install_python_lsp ;;
            3) install_nodejs_lsp ;;
            4) install_go_lsp ;;
            5) install_clangd ;;
            6) 
                install_rust
                install_python_lsp
                install_nodejs_lsp
                install_go_lsp
                install_clangd
                ;;
            *) log_warning "Unknown choice: $choice" ;;
        esac
    done
    
    configure_environment
    create_test_projects
    verify_installation
    show_completion_info
    
    echo ""
    log_info "Setup script completed. Please run 'source ~/.bashrc' to reload your environment."
}

# 脚本入口
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi