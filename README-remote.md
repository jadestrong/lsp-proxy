# LSP-Proxy Remote Development

This module provides comprehensive remote development integration for LSP-Proxy, allowing you to seamlessly work with code on remote servers while maintaining full Language Server Protocol (LSP) support in Emacs.

## Features

- **Remote Server Management**: Connect/disconnect from multiple remote servers
- **File Operations**: Read and write files on remote servers
- **LSP Integration**: Forward LSP requests to remote language servers
- **Workspace Management**: Manage multiple remote workspaces
- **Caching**: Intelligent caching for improved performance
- **Emacs Integration**: Rich Emacs Lisp commands and key bindings

## Quick Start

1. **Setup**: Run the setup script to configure your environment:
   ```bash
   ./setup-remote-dev.sh
   ```

2. **Configure**: Edit `~/.config/lsp-proxy/remote.toml` to add your remote servers:
   ```toml
   [[servers]]
   name = "my-server"
   host = "example.com"
   user = "developer" 
   port = 22
   mode = "SSH"
   workspace_root = "/home/developer/projects"
   
   [servers.auth]
   method = "key"
   key_file = "~/.ssh/id_rsa"
   ```

3. **Start LSP-Proxy**: 
   ```bash
   cargo run --release
   ```

4. **Use in Emacs**:
   - `C-c r c` - Connect to a remote server
   - `C-c r o` - Open a remote file
   - `C-c r l` - List all servers

## Configuration

### Server Configuration

Each server in your `remote.toml` file supports these options:

```toml
[[servers]]
name = "server-name"           # Unique identifier
host = "hostname.com"          # Server hostname/IP
user = "username"              # SSH username  
port = 22                      # SSH port (optional, default: 22)
mode = "SSH"                   # Connection mode (currently only SSH)
workspace_root = "/path/to/workspace"  # Remote workspace directory

[servers.auth]
method = "key"                 # Auth method: "key" or "password"
key_file = "~/.ssh/id_rsa"    # SSH key file (for key auth)
# password = "secret"          # Password (for password auth)
```

### Global Settings

```toml
[settings]
default_server = "my-server"   # Auto-connect server
connection_timeout = 30        # Connection timeout (seconds)
max_connections = 5            # Max concurrent connections

[settings.cache]
enabled = true                 # Enable caching
max_size = "100MB"            # Maximum cache size
ttl = 3600                    # Cache TTL (seconds)

[settings.connection]
keep_alive = true             # Keep SSH connections alive
retry_attempts = 3            # Connection retry attempts
retry_delay = 5               # Delay between retries (seconds)
```

## Emacs Integration

### Key Bindings

When `lsp-proxy-remote-mode` is enabled:

| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c r c` | `lsp-proxy-remote-connect` | Connect to remote server |
| `C-c r d` | `lsp-proxy-remote-disconnect` | Disconnect from server |
| `C-c r l` | `lsp-proxy-remote-list-servers` | List all servers |
| `C-c r s` | `lsp-proxy-remote-status` | Show server status |
| `C-c r o` | `lsp-proxy-remote-open-file` | Open remote file |
| `C-c r S` | `lsp-proxy-remote-save-file` | Save remote file |
| `C-c r w` | `lsp-proxy-remote-list-workspaces` | List remote workspaces |
| `C-c r r` | `lsp-proxy-remote-refresh-server-list` | Refresh server list |

### Interactive Commands

All commands are also available via `M-x`:

- `lsp-proxy-remote-connect` - Connect to a remote server
- `lsp-proxy-remote-disconnect` - Disconnect from a server  
- `lsp-proxy-remote-list-servers` - Display server list in buffer
- `lsp-proxy-remote-status` - Show detailed server status
- `lsp-proxy-remote-open-file` - Open file from remote server
- `lsp-proxy-remote-save-file` - Save current buffer to remote
- `lsp-proxy-remote-list-workspaces` - Display remote workspaces
- `lsp-proxy-remote-lsp-request` - Send raw LSP request to remote
- `lsp-proxy-remote-setup` - Initialize remote development mode

### Customization

```elisp
;; Enable auto-connect on startup
(setq lsp-proxy-remote-auto-connect t)

;; Set default server
(setq lsp-proxy-remote-default-server "my-server")

;; Set connection timeout
(setq lsp-proxy-remote-connection-timeout 60)

;; Enable remote mode globally
(global-lsp-proxy-remote-mode 1)
```

## Usage Examples

### Connecting to a Server

```elisp
;; Connect with default configuration
M-x lsp-proxy-remote-connect RET my-server RET

;; Connect with custom parameters
M-x lsp-proxy-remote-connect RET my-server RET
; Override host: custom-host.com
; Override user: different-user  
; Override port: 2222
```

### Working with Remote Files

```elisp
;; Open a remote file
M-x lsp-proxy-remote-open-file RET my-server RET /path/to/file.rs RET

;; Edit the file normally in Emacs
;; Save back to remote server
C-c r S
```

### Managing Workspaces

```elisp
;; List all remote workspaces
M-x lsp-proxy-remote-list-workspaces

;; Check server status  
M-x lsp-proxy-remote-status RET my-server RET
```

## Architecture

The remote development system consists of several components:

1. **Rust Backend**: Handles SSH connections, file operations, and LSP forwarding
2. **Configuration Manager**: Loads and manages server configurations  
3. **Session Manager**: Tracks active remote sessions
4. **LSP Proxy**: Forwards LSP requests to remote language servers
5. **File System**: Provides remote file operations with local caching
6. **Emacs Frontend**: Rich integration with interactive commands

### Request Flow

```
Emacs → JSON-RPC → LSP-Proxy → SSH → Remote Server → Language Server
                     ↓
                 Local Cache ← Response ← SSH ← Remote Response
```

## Language Server Support

The system supports any language server available on the remote machine:

- **Rust**: rust-analyzer
- **TypeScript/JavaScript**: typescript-language-server
- **Python**: pylsp, pyright  
- **Go**: gopls
- **C/C++**: clangd
- **Java**: eclipse.jdt.ls
- And many more...

Language servers are automatically detected and managed by the remote LSP-Proxy instance.

## Troubleshooting

### Common Issues

1. **Connection Failed**
   ```
   Error: Failed to connect to server 'my-server': SSH connection failed
   ```
   - Check SSH connectivity: `ssh my-server`
   - Verify server configuration in `remote.toml`
   - Check SSH key permissions: `chmod 600 ~/.ssh/id_rsa`

2. **File Not Found**
   ```
   Error: Failed to open remote file: /path/not/found
   ```
   - Verify file path exists on remote server
   - Check workspace_root configuration
   - Ensure proper permissions

3. **LSP Request Failed**
   ```
   Error: LSP request failed: Language server not found
   ```
   - Verify language server is installed on remote machine
   - Check language server is in PATH on remote system
   - Review remote LSP-Proxy logs

### Debug Mode

Enable verbose logging:

```bash
# Start LSP-Proxy with debug logging
cargo run --release -- --log-level 3

# Check logs
tail -f ~/.local/share/lsp-proxy/logs/remote.log
```

In Emacs:
```elisp
;; Enable debug messages
(setq lsp-proxy-remote-debug t)

;; Check *Messages* buffer for debug output
```

### Log Files

- **LSP-Proxy Logs**: `~/.local/share/lsp-proxy/logs/`
- **SSH Debug**: Use `ssh -v` for connection debugging
- **Emacs Messages**: Check `*Messages*` buffer for errors

## Performance Tips

1. **Use SSH Connection Multiplexing**:
   ```
   Host *
       ControlMaster auto
       ControlPath ~/.ssh/sockets/%r@%h-%p  
       ControlPersist 600
   ```

2. **Enable Caching**:
   ```toml
   [settings.cache]
   enabled = true
   max_size = "500MB"
   ttl = 7200
   ```

3. **Optimize SSH Settings**:
   ```
   Host my-server
       Compression yes
       ServerAliveInterval 60
       TCPKeepAlive yes
   ```

## Security Considerations

- Use SSH key authentication instead of passwords
- Keep SSH keys secure with proper permissions (600)
- Use SSH agent for key management  
- Consider SSH certificate authentication for better security
- Regularly rotate SSH keys
- Use strong passphrases for SSH keys

## Contributing

Contributions are welcome! Please see the main LSP-Proxy repository for contribution guidelines.

## License

This project is licensed under the same terms as LSP-Proxy. See the main LICENSE file for details.