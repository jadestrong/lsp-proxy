# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

LSP-Proxy is a Language Server Protocol (LSP) client for Emacs, implemented in Rust. It acts as an intermediary proxy between Emacs and various language servers, handling communication, processing responses, and providing LSP features like completion, diagnostics, hover, code actions, and more.

## Development Commands

### Build Commands
```bash
# Build the project
cargo build

# Build release version  
cargo build --release

# Copy binary to root directory after release build
cp ./target/release/lsp-proxy ./
```

### Running and Testing
```bash
# Run the binary directly
./lsp-proxy

# Run with different log levels for different purposes:
# Level 0: Only errors and warnings
./lsp-proxy --log-level 0

# Level 1: Only LSP communication logs (requests/responses/notifications)
./lsp-proxy --log-level 1

# Level 2: Service debug logs + LSP communication logs  
./lsp-proxy --log-level 2

# Level 3: Everything (most verbose)
./lsp-proxy --log-level 3

# No formal test suite - testing is primarily done through integration with Emacs
```

### Development Tools
```bash
# Check code with clippy
cargo clippy

# Format code  
cargo fmt

# Check compilation without building
cargo check

# Version management (uses Node.js scripts)
npm run update-version    # Update version in all relevant files
npm run extract-version   # Extract version from Cargo.toml
npm run test-update      # Test version update process
npm run test-npm-package # Test npm package functionality
```

## Architecture Overview

### Core Components

**Main Application Flow:**
- `main.rs` - Entry point, argument parsing, logging setup
- `main_loop.rs` - Core event loop handling messages between Emacs and LSP servers
- `application.rs` - Central application state and request/response handling
- `connection.rs` - stdio communication with Emacs using JSON-RPC

**LSP Management:**
- `editor.rs` - Manages documents and language server registry
- `registry.rs` - Tracks and manages multiple language server instances
- `client.rs` - Individual language server client wrapper
- `document.rs` - Document state and diagnostics management

**Message Processing:**
- `handlers/` - Request and notification handlers for LSP protocol
  - `handlers/request.rs` - LSP request handlers (completion, hover, goto, etc.)
  - `handlers/notification.rs` - LSP notification handlers (document changes, diagnostics)
- `dispatch.rs` - Message routing and dispatching
- `msg.rs` - Message types and serialization
- `req_queue.rs` - Request queue management for async operations

**Language Support:**
- `syntax.rs` - Language detection and syntax loading
- `config.rs` - Language server configuration loading
- `languages.toml` - Built-in language server configurations

**Features:**
- `completion_cache.rs` - Caching and fuzzy matching for completions  
- `code_action.rs` - Code action support
- `fuzzy/` - Fuzzy string matching for completions
- `large_file_manager.rs` - Large file handling and optimization for performance
- `lsp_ext.rs` - LSP protocol extensions and custom methods

### Communication Architecture

1. **Emacs ↔ LSP-Proxy**: JSON-RPC over stdio using `jsonrpc.el`
2. **LSP-Proxy ↔ Language Servers**: Standard LSP protocol over stdio/TCP
3. **Multi-threaded**: Separate threads for application logic, controller, and I/O

### Configuration System

- `languages.toml` - Built-in language server configurations (supports 15+ languages including JS/TS, Python, Rust, Java, Dart, C/C++, Ruby, CSS, JSON, TOML, Bash)
- User can override with custom config file via `lsp-proxy-open-config-file` command
- Configuration follows Helix editor format with fields: `name`, `roots`, `language-id`, `file-types`, `language-servers`
- Supports language server features like auto-imports, inlay hints, code actions, formatting

## Key Features Implemented

- **Navigation**: Find definitions, references, implementations, type definitions, declarations
- **Completion**: Auto-completion with snippets, auto-imports, fuzzy matching, and caching
- **Diagnostics**: Real-time error/warning reporting (flycheck/flymake integration)
- **Code Actions**: Quick fixes and refactoring suggestions  
- **Formatting**: Document formatting
- **Hover**: Documentation on hover
- **Rename**: Symbol renaming
- **Inlay Hints**: Type hints and parameter names
- **Document Symbols**: For imenu integration
- **Workspace Commands**: Language server specific commands
- **Smart xref**: Multiple optimization strategies for large files
  - `optimized` (default): Fast with content preview using line-based calculation
  - `lazy`: Fastest, no preview (file:line:column only)  
  - `eager`: Original method (full content preview, may be slow)
  - Configurable via `lsp-proxy-xref-optimization-strategy` and `lsp-proxy-lazy-xref-threshold`

## Emacs Integration

The project includes Emacs Lisp integration files:
- `lsp-proxy.el` - Main Emacs interface and client implementation
- `lsp-proxy-large-file.el` - Large file handling extensions for Emacs integration

## Build and Packaging

- **Binary name**: `emacs-lsp-proxy` (defined in Cargo.toml)
- **Release process**: Automated via GitHub Actions with version management
- **Version management**: Node.js scripts handle version updates across Cargo.toml and related files

## Adding Language Support

To add a new language server, modify the user config file (accessible via `lsp-proxy-open-config-file`) following this pattern:

```toml
[language-server.server-name]
command = "language-server-binary"
args = ["--stdio"]

[[language]]  
name = "language-name"
roots = ["package.json", "Cargo.toml"] 
language-id = "language-identifier"
file-types = ["ext1", "ext2"]
language-servers = ["server-name"]
```

## Remote LSP Development

LSP-Proxy supports remote development through multiple connection modes:

### Connection Modes

1. **SSH Pipe Mode** (Recommended - No Port Exposure)
   ```bash
   # Remote server runs in stdio mode
   ssh user@remote-server "lsp-proxy-server --stdio"
   ```

2. **SSH Tunnel Mode** (Port forwarding)
   ```bash
   # Local port forwarding
   ssh -L 19527:127.0.0.1:9527 user@remote-server
   ```

3. **Direct TCP Mode** (Requires open ports)
   ```bash
   # Direct connection to remote server
   lsp-proxy-server --listen 0.0.0.0:9527
   ```

### Configuration

**Remote LSP Config** (`~/.config/lsp-proxy/remote-lsp.toml`):
```toml
# SSH Pipe Mode (No ports needed)
[hosts.dev-server]
connection_type = "ssh_pipe"

[hosts.dev-server.ssh_pipe]
host = "dev-server.com"
user = "developer"
identity_file = "~/.ssh/dev_key"
server_install_script = '''
curl -fsSL https://raw.githubusercontent.com/jadestrong/lsp-proxy/main/scripts/server-install.sh | bash
'''
```

### Remote Server Installation

The installation script automatically:
- Downloads/builds lsp-proxy-server binary
- Installs common LSP servers (rust-analyzer, pylsp, etc.)
- Creates default configuration
- Sets up proper permissions

### Usage with Emacs

Open remote files normally with TRAMP:
```elisp
;; SSH Pipe mode will automatically activate
C-x C-f /ssh:dev-server:/home/user/project/src/main.rs

;; LSP features work seamlessly:
;; - Code completion: M-TAB
;; - Go to definition: M-.
;; - Find references: M-?
;; - Hover documentation: C-c C-d
```

### Architecture

```
Emacs (TRAMP) ↔ Local lsp-proxy ↔ SSH Pipe ↔ Remote lsp-proxy-server ↔ Remote LSP Servers
```

The SSH pipe mode eliminates the need for:
- Port exposure on remote servers
- Firewall configuration
- Network security concerns

## Debugging

- Set log level: `lsp-proxy --log-level 3` (0=Warn, 1=Info, 2=Debug, 3=Trace)
- Check `*lsp-proxy-events*` buffer in Emacs for server crashes
- Check `*lsp-proxy-log*` buffer for LSP server messages
- Use `lsp-proxy-open-log-file` to view detailed server logs