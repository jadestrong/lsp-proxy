#!/bin/bash
# LSP-Proxy Server Installation Script
# This script installs or updates lsp-proxy-server on the remote machine

set -e

# Configuration
INSTALL_DIR="${INSTALL_DIR:-$HOME/.local/bin}"
CONFIG_DIR="${CONFIG_DIR:-$HOME/.config}"
BINARY_NAME="lsp-proxy-server"
DOWNLOAD_URL="${DOWNLOAD_URL:-https://github.com/jadestrong/lsp-proxy/releases/latest/download/lsp-proxy-server}"

echo "🚀 Installing LSP-Proxy Server..."

# Create directories
mkdir -p "$INSTALL_DIR"
mkdir -p "$CONFIG_DIR"

# Function to detect architecture
detect_arch() {
    case $(uname -m) in
        x86_64) echo "x86_64" ;;
        aarch64|arm64) echo "aarch64" ;;
        *) echo "unsupported" ;;
    esac
}

# Function to detect OS
detect_os() {
    case $(uname -s) in
        Linux) echo "linux" ;;
        Darwin) echo "macos" ;;
        *) echo "unsupported" ;;
    esac
}

ARCH=$(detect_arch)
OS=$(detect_os)

if [ "$ARCH" = "unsupported" ] || [ "$OS" = "unsupported" ]; then
    echo "❌ Unsupported platform: $(uname -s) $(uname -m)"
    echo "🔧 Attempting to build from source..."
    
    # Check if we have Rust installed
    if command -v cargo >/dev/null 2>&1; then
        echo "📦 Building lsp-proxy-server from source..."
        
        # Clone and build
        if [ ! -d "/tmp/lsp-proxy" ]; then
            git clone https://github.com/jadestrong/lsp-proxy.git /tmp/lsp-proxy
        fi
        
        cd /tmp/lsp-proxy
        git pull origin main
        cargo build --release --bin lsp-proxy-server
        
        # Install binary
        cp target/release/lsp-proxy-server "$INSTALL_DIR/"
        chmod +x "$INSTALL_DIR/lsp-proxy-server"
        
        echo "✅ Built and installed lsp-proxy-server from source"
    else
        echo "❌ Rust not found. Please install Rust or download a compatible binary"
        exit 1
    fi
else
    # Download precompiled binary
    BINARY_URL="${DOWNLOAD_URL}-${OS}-${ARCH}"
    echo "📥 Downloading $BINARY_URL..."
    
    if command -v curl >/dev/null 2>&1; then
        curl -L -o "$INSTALL_DIR/$BINARY_NAME" "$BINARY_URL"
    elif command -v wget >/dev/null 2>&1; then
        wget -O "$INSTALL_DIR/$BINARY_NAME" "$BINARY_URL"
    else
        echo "❌ Neither curl nor wget found. Cannot download binary"
        exit 1
    fi
    
    chmod +x "$INSTALL_DIR/$BINARY_NAME"
    echo "✅ Downloaded and installed lsp-proxy-server binary"
fi

# Create default configuration if it doesn't exist
CONFIG_FILE="$CONFIG_DIR/lsp-proxy-server.toml"
if [ ! -f "$CONFIG_FILE" ]; then
    echo "📝 Creating default configuration..."
    cat > "$CONFIG_FILE" << 'EOF'
listen_address = "127.0.0.1:9527"
work_dir = "."
max_lsp_servers = 5

[lsp_servers.rust]
command = "rust-analyzer"
args = []
env = {}

[lsp_servers.python]
command = "pylsp"
args = []
env = {}

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

[lsp_servers.java]
command = "jdtls"
args = []
env = {}

[auth]
enabled = false
EOF
    echo "✅ Created default configuration at $CONFIG_FILE"
fi

# Install common LSP servers
echo "🔧 Installing common LSP servers..."

# Check and suggest LSP server installations
check_lsp_server() {
    local name=$1
    local command=$2
    local install_cmd=$3
    
    if command -v "$command" >/dev/null 2>&1; then
        echo "✅ $name is already installed"
    else
        echo "❌ $name not found"
        if [ -n "$install_cmd" ]; then
            echo "💡 To install: $install_cmd"
        fi
    fi
}

check_lsp_server "rust-analyzer" "rust-analyzer" "rustup component add rust-analyzer"
check_lsp_server "Python LSP Server" "pylsp" "pip install 'python-lsp-server[all]'"
check_lsp_server "TypeScript Language Server" "typescript-language-server" "npm install -g typescript-language-server typescript"
check_lsp_server "Go LSP Server" "gopls" "go install golang.org/x/tools/gopls@latest"
check_lsp_server "Clangd" "clangd" "# Install via package manager (apt, brew, etc.)"

# Add to PATH if needed
if [[ ":$PATH:" != *":$INSTALL_DIR:"* ]]; then
    echo "📌 Add the following to your shell profile to use lsp-proxy-server:"
    echo "   export PATH=\"$INSTALL_DIR:\$PATH\""
fi

# Test the installation
if "$INSTALL_DIR/$BINARY_NAME" --help >/dev/null 2>&1; then
    echo "🎉 Installation successful!"
    echo "📍 Binary location: $INSTALL_DIR/$BINARY_NAME"
    echo "📍 Config location: $CONFIG_FILE"
    echo ""
    echo "🔥 Usage:"
    echo "   # Run in TCP mode:"
    echo "   $INSTALL_DIR/$BINARY_NAME --config $CONFIG_FILE"
    echo ""
    echo "   # Run in stdio mode (for SSH pipe):"
    echo "   $INSTALL_DIR/$BINARY_NAME --stdio --config $CONFIG_FILE"
else
    echo "❌ Installation test failed"
    exit 1
fi