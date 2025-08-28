#!/bin/bash

# LSP-Proxy Remote Development Setup Script
# This script helps users set up remote development with lsp-proxy

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration directories
CONFIG_DIR="$HOME/.config/lsp-proxy"
CACHE_DIR="$HOME/.cache/lsp-proxy"
LOG_DIR="$HOME/.local/share/lsp-proxy/logs"

print_header() {
    echo -e "${BLUE}================================================${NC}"
    echo -e "${BLUE}  LSP-Proxy Remote Development Setup${NC}"
    echo -e "${BLUE}================================================${NC}"
    echo
}

print_step() {
    echo -e "${GREEN}[STEP]${NC} $1"
}

print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

create_directories() {
    print_step "Creating configuration directories..."
    
    mkdir -p "$CONFIG_DIR"
    mkdir -p "$CACHE_DIR"
    mkdir -p "$LOG_DIR"
    
    print_info "Created directories:"
    print_info "  Config: $CONFIG_DIR"
    print_info "  Cache:  $CACHE_DIR"
    print_info "  Logs:   $LOG_DIR"
}

setup_config() {
    print_step "Setting up configuration..."
    
    local config_file="$CONFIG_DIR/remote.toml"
    
    if [[ -f "$config_file" ]]; then
        print_warning "Configuration file already exists: $config_file"
        read -p "Do you want to backup and replace it? [y/N]: " -r
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            cp "$config_file" "$config_file.backup.$(date +%Y%m%d_%H%M%S)"
            print_info "Backed up existing config"
        else
            print_info "Keeping existing configuration"
            return
        fi
    fi
    
    # Copy the example configuration
    if [[ -f "remote-config-example.toml" ]]; then
        cp "remote-config-example.toml" "$config_file"
        print_info "Created configuration file: $config_file"
        print_info "Please edit this file to configure your remote servers"
    else
        print_error "Example configuration file not found!"
        print_info "Creating minimal configuration..."
        
        cat > "$config_file" << 'EOF'
# LSP-Proxy Remote Configuration
[settings]
default_server = ""
connection_timeout = 30
max_connections = 5

[settings.cache]
enabled = true
max_size = "100MB"
ttl = 3600

[settings.connection]
keep_alive = true
retry_attempts = 3
retry_delay = 5

# Add your remote servers here
# Example:
# [[servers]]
# name = "my-server"
# host = "example.com"
# user = "username"
# port = 22
# mode = "SSH"
# workspace_root = "/home/username/projects"
# 
# [servers.auth]
# method = "key"
# key_file = "~/.ssh/id_rsa"
EOF
        print_info "Created minimal configuration file: $config_file"
    fi
}

check_dependencies() {
    print_step "Checking dependencies..."
    
    local missing_deps=()
    
    # Check for Emacs
    if ! command -v emacs &> /dev/null; then
        missing_deps+=("emacs")
    fi
    
    # Check for SSH
    if ! command -v ssh &> /dev/null; then
        missing_deps+=("ssh")
    fi
    
    # Check for common language servers (optional)
    local optional_deps=("rust-analyzer" "typescript-language-server" "pylsp")
    local missing_optional=()
    
    for dep in "${optional_deps[@]}"; do
        if ! command -v "$dep" &> /dev/null; then
            missing_optional+=("$dep")
        fi
    done
    
    if [[ ${#missing_deps[@]} -gt 0 ]]; then
        print_error "Missing required dependencies: ${missing_deps[*]}"
        print_info "Please install the missing dependencies and run this script again"
        exit 1
    fi
    
    if [[ ${#missing_optional[@]} -gt 0 ]]; then
        print_warning "Optional language servers not found: ${missing_optional[*]}"
        print_info "Install them for better remote development experience"
    fi
    
    print_info "All required dependencies are available"
}

setup_emacs_integration() {
    print_step "Setting up Emacs integration..."
    
    local emacs_dir="$HOME/.emacs.d"
    local doom_dir="$HOME/.doom.d"
    
    # Check for different Emacs configurations
    if [[ -d "$doom_dir" ]]; then
        print_info "Detected Doom Emacs configuration"
        local config_file="$doom_dir/config.el"
        
        if ! grep -q "lsp-proxy-remote" "$config_file" 2>/dev/null; then
            echo "" >> "$config_file"
            echo ";; LSP-Proxy Remote Development" >> "$config_file"
            echo "(add-to-list 'load-path \"$(pwd)\")" >> "$config_file"
            echo "(require 'lsp-proxy-remote)" >> "$config_file"
            echo "(lsp-proxy-remote-setup)" >> "$config_file"
            print_info "Added lsp-proxy-remote configuration to $config_file"
        else
            print_info "LSP-Proxy remote integration already configured"
        fi
    elif [[ -d "$emacs_dir" ]]; then
        print_info "Detected standard Emacs configuration"
        local init_file="$emacs_dir/init.el"
        
        if [[ ! -f "$init_file" ]]; then
            touch "$init_file"
        fi
        
        if ! grep -q "lsp-proxy-remote" "$init_file" 2>/dev/null; then
            echo "" >> "$init_file"
            echo ";; LSP-Proxy Remote Development" >> "$init_file"
            echo "(add-to-list 'load-path \"$(pwd)\")" >> "$init_file"
            echo "(require 'lsp-proxy-remote)" >> "$init_file"
            echo "(lsp-proxy-remote-setup)" >> "$init_file"
            print_info "Added lsp-proxy-remote configuration to $init_file"
        else
            print_info "LSP-Proxy remote integration already configured"
        fi
    else
        print_warning "No Emacs configuration directory found"
        print_info "Please manually add the following to your Emacs configuration:"
        echo ""
        echo "  (add-to-list 'load-path \"$(pwd)\")"
        echo "  (require 'lsp-proxy-remote)"
        echo "  (lsp-proxy-remote-setup)"
        echo ""
    fi
}

setup_ssh_config() {
    print_step "SSH configuration recommendations..."
    
    local ssh_config="$HOME/.ssh/config"
    
    print_info "For better remote development experience, consider adding to $ssh_config:"
    echo ""
    echo "  Host your-remote-server"
    echo "    HostName example.com"
    echo "    User your-username"
    echo "    Port 22"
    echo "    IdentityFile ~/.ssh/your-key"
    echo "    ServerAliveInterval 60"
    echo "    ServerAliveCountMax 3"
    echo "    ControlMaster auto"
    echo "    ControlPath ~/.ssh/sockets/%r@%h-%p"
    echo "    ControlPersist 600"
    echo ""
}

print_usage_instructions() {
    print_step "Usage Instructions"
    
    echo ""
    print_info "1. Configure your remote servers in: $CONFIG_DIR/remote.toml"
    print_info "2. Start lsp-proxy: cargo run --release"
    print_info "3. In Emacs, use these key bindings:"
    echo ""
    echo "   C-c r c - Connect to remote server"
    echo "   C-c r d - Disconnect from remote server"  
    echo "   C-c r l - List remote servers"
    echo "   C-c r s - Show server status"
    echo "   C-c r o - Open remote file"
    echo "   C-c r w - List remote workspaces"
    echo "   C-c r S - Save remote file"
    echo ""
    print_info "4. Or use M-x commands:"
    echo "   - lsp-proxy-remote-connect"
    echo "   - lsp-proxy-remote-list-servers"
    echo "   - lsp-proxy-remote-open-file"
    echo ""
}

print_troubleshooting() {
    print_step "Troubleshooting"
    
    echo ""
    print_info "If you encounter issues:"
    print_info "1. Check logs in: $LOG_DIR"
    print_info "2. Verify SSH connectivity: ssh your-server-name"
    print_info "3. Test lsp-proxy connection: lsp-proxy --log-level 3"
    print_info "4. Check Emacs *Messages* buffer for errors"
    echo ""
}

main() {
    print_header
    
    check_dependencies
    create_directories
    setup_config
    setup_emacs_integration
    setup_ssh_config
    print_usage_instructions
    print_troubleshooting
    
    echo ""
    print_step "Setup complete!"
    print_info "Please restart Emacs to load the remote development integration."
    echo ""
}

# Run main function
main "$@"