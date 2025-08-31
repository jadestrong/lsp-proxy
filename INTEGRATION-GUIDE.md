# LSP-Proxy 和 LSP-Proxy-Remote 集成指南

本指南详细说明如何将 `lsp-proxy-remote.el` 与现有的 `lsp-proxy.el` 集成。

## 集成架构

### 依赖关系

```
lsp-proxy-remote.el
       ↓
   (require 'lsp-proxy)
       ↓
   lsp-proxy.el (主模块)
       ↓
   lsp-proxy binary (Rust 后端)
```

### 集成要点

1. **连接共享**: 使用相同的 `lsp-proxy--connection` JSON-RPC 连接
2. **状态集成**: 在 `lsp-proxy-status` 中显示远程状态
3. **模式协调**: 远程模式与主模式的生命周期管理
4. **钩子集成**: 自动启用和清理机制

## 安装和配置

### 1. 基础安装

```elisp
;; 在你的 init.el 或 config.el 中添加
(add-to-list 'load-path "/path/to/lsp-proxy")

;; 加载主模块
(require 'lsp-proxy)

;; 加载远程开发模块
(require 'lsp-proxy-remote)
```

### 2. 自动集成设置

```elisp
;; 方案 A: 手动启用远程模式
(lsp-proxy-remote-setup)

;; 方案 B: 自动随主模式启用
(lsp-proxy-remote-enable-with-main-mode)

;; 方案 C: 全局启用
(global-lsp-proxy-mode 1)
(lsp-proxy-remote-mode 1)
```

### 3. 配置选项

```elisp
;; 远程开发配置
(setq lsp-proxy-remote-auto-connect t)              ; 自动连接
(setq lsp-proxy-remote-default-server "dev-server") ; 默认服务器
(setq lsp-proxy-remote-connection-timeout 60)       ; 连接超时

;; 主模块配置 (保持现有配置)
(setq lsp-proxy-user-languages-config "~/.config/lsp-proxy/languages.toml")
(setq lsp-proxy-log-max 1000)
```

## 使用方式

### 启动流程

1. **启动 LSP-Proxy 后端**:
   ```bash
   cargo run --release
   ```

2. **在 Emacs 中启用模式**:
   ```elisp
   M-x lsp-proxy-mode          ; 启用主模式
   M-x lsp-proxy-remote-mode   ; 启用远程模式 (如果未自动启用)
   ```

3. **连接远程服务器**:
   ```elisp
   M-x lsp-proxy-remote-connect RET dev-server RET
   ```

### 状态检查

```elisp
;; 检查整体状态 (包含远程信息)
M-x lsp-proxy-status

;; 检查远程连接
M-x lsp-proxy-remote-list-servers
```

## 集成特性

### 1. 连接管理集成

- **共享连接**: 远程请求复用主模块的 `lsp-proxy--connection`
- **连接检查**: 自动检查连接状态，确保操作有效
- **错误处理**: 统一的错误处理和用户反馈

```elisp
;; 内部实现 - 确保连接可用
(defun lsp-proxy-remote--ensure-connection ()
  (unless (and (boundp 'lsp-proxy--connection) 
               lsp-proxy--connection
               (lsp-proxy--connection-alivep))
    (user-error "LSP-Proxy not connected. Please start lsp-proxy-mode first")))
```

### 2. 状态显示集成

远程状态会自动添加到 `lsp-proxy-status` 缓冲区中:

```
=== LSP Proxy Configuration ===
lsp-proxy-mode: Enabled
lsp-proxy--connection: Alive
...

=== Remote Development Status ===
Remote mode enabled: Yes
Connected servers: 2
Active connections:
  - dev-server
  - staging
```

### 3. 模式生命周期集成

- **启动时**: 远程模式自动检测主模式状态
- **关闭时**: 主模式关闭会触发远程连接清理
- **钩子集成**: 使用 `lsp-proxy-mode-hook` 进行协调

### 4. 键位绑定

远程模式添加了 `C-c r` 前缀键位，不与主模式冲突:

```elisp
;; 主模式键位 (现有)
C-c l ...    ; LSP 操作

;; 远程模式键位 (新增)
C-c r c      ; 连接远程服务器
C-c r d      ; 断开连接
C-c r o      ; 打开远程文件
C-c r S      ; 保存远程文件
...
```

## 配置文件集成

### 1. 远程服务器配置

创建 `~/.config/lsp-proxy/remote.toml`:

```toml
[settings]
default_server = "dev-server"

[[servers]]
name = "dev-server"
host = "dev.example.com"
user = "developer"
port = 22
mode = "SSH"
workspace_root = "/home/developer/projects"

[servers.auth]
method = "key"
key_file = "~/.ssh/id_rsa"
```

### 2. Emacs 配置示例

```elisp
;; === 基础配置 ===
(use-package lsp-proxy
  :load-path "/path/to/lsp-proxy"
  :config
  (setq lsp-proxy-log-max 1000)
  (global-lsp-proxy-mode 1))

;; === 远程开发集成 ===
(use-package lsp-proxy-remote
  :after lsp-proxy
  :load-path "/path/to/lsp-proxy"
  :config
  (setq lsp-proxy-remote-auto-connect t
        lsp-proxy-remote-default-server "dev-server"
        lsp-proxy-remote-connection-timeout 60)
  
  ;; 自动启用
  (lsp-proxy-remote-enable-with-main-mode)
  
  ;; 自定义钩子
  (add-hook 'lsp-proxy-remote-connected-hook
            (lambda () (message "🌐 Remote development ready!")))
            
  (add-hook 'lsp-proxy-remote-disconnected-hook
            (lambda () (message "📴 Remote session ended"))))
```

## 故障排除

### 常见问题

1. **远程模式无法启用**:
   ```elisp
   ;; 检查主模块是否已加载
   (featurep 'lsp-proxy)  ; 应该返回 t
   
   ;; 手动加载
   (require 'lsp-proxy)
   ```

2. **连接失败**:
   ```elisp
   ;; 检查主连接状态
   (lsp-proxy--connection-alivep)  ; 应该返回 t
   
   ;; 重启主模式
   M-x lsp-proxy-mode  ; 禁用
   M-x lsp-proxy-mode  ; 重新启用
   ```

3. **远程请求失败**:
   - 确保 lsp-proxy binary 支持远程功能
   - 检查远程服务器配置
   - 查看 `*Messages*` 缓冲区的错误信息

### 调试方法

```elisp
;; 启用详细日志
(setq lsp-proxy-log-max 5000)

;; 检查连接状态
M-x lsp-proxy-status

;; 查看远程状态
M-x lsp-proxy-remote-list-servers

;; 手动测试连接
(lsp-proxy-remote--ensure-connection)  ; 应该不报错
```

## 扩展开发

如果你需要扩展远程功能，可以:

### 1. 添加新的远程操作

```elisp
(defun my-custom-remote-operation (server-name)
  "自定义远程操作."
  (interactive 
   (list (completing-read "Server: " lsp-proxy-remote--connected-servers)))
  (lsp-proxy-remote--send-async-request
   "emacs/myCustomOperation" 
   `((server_name . ,server-name))
   (lambda (result)
     (message "Custom operation result: %s" result))))
```

### 2. 添加到状态显示

```elisp
(defun my-remote-status-extension ()
  "扩展远程状态显示."
  (when (get-buffer "*lsp-proxy-status*")
    (with-current-buffer "*lsp-proxy-status*"
      (goto-char (point-max))
      (insert "Custom remote info: ...\n"))))

;; 添加到现有的状态函数
(advice-add 'lsp-proxy-remote--add-to-status-buffer :after
            #'my-remote-status-extension)
```

## 总结

通过这种集成方式，`lsp-proxy-remote.el` 与 `lsp-proxy.el` 形成了一个统一的开发环境:

- ✅ 共享连接和状态管理
- ✅ 协调的模式生命周期
- ✅ 一致的用户体验
- ✅ 扩展性和可维护性

用户只需要按照配置指南设置，就可以享受本地和远程开发的无缝切换体验。