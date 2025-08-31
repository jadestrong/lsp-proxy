# LSP-Proxy TRAMP Integration Guide

## 概述

LSP-Proxy TRAMP 集成提供了现代化的远程开发体验，结合了 TRAMP 的文件系统抽象和 LSP-Proxy 的远程 LSP 支持。

## 核心特性

### 1. 文件访问层 (TRAMP 集成)
- **透明文件访问**: 使用 TRAMP 协议直接访问远程文件
- **文件格式**: `/lsp-proxy:server-name:/path/to/file`
- **支持所有 TRAMP 操作**: find-file, dired, save, delete 等

### 2. 自动 LSP 集成
- **自动语言检测**: 根据文件扩展名自动启动对应的 LSP 服务器
- **LSP 请求代理**: 所有 LSP 请求通过 lsp-proxy 转发到远程服务器
- **实时同步**: 文件修改实时通知远程 LSP 服务器

### 3. 智能缓存和优化
- **按需加载**: 只在需要时加载文件内容
- **增量同步**: 仅传输文件变更部分
- **连接复用**: 复用现有的 lsp-proxy 连接

## 安装和配置

### 1. 基本设置

```elisp
;; init.el 或 .doom.d/config.el

;; 确保 lsp-proxy 已配置
(use-package lsp-proxy
  :config
  (lsp-proxy-mode 1))

;; 加载 TRAMP 集成
(require 'lsp-proxy-tramp)

;; 设置 lsp-proxy 远程服务器
(use-package lsp-proxy-remote
  :config
  (lsp-proxy-remote-mode 1)
  ;; 自动连接默认服务器
  (setq lsp-proxy-remote-auto-connect t
        lsp-proxy-remote-default-server "dev-server"))

;; 启用 TRAMP 集成
(lsp-proxy-tramp-setup)
```

### 2. 远程服务器配置

在 `~/.config/lsp-proxy/remote.toml` 中配置远程服务器：

```toml
# 现代化的远程开发服务器配置
[servers.rust-dev]
name = "rust-dev"
host = "dev.example.com"
user = "developer"
port = 22
workspace_root = "/home/developer/projects"
mode = { SSHTunnel = { auto_deploy = true } }
auth = { SshConfig = { host = "dev.example.com" } }

[servers.python-server]
name = "python-server"
host = "python.internal"
user = "dev"
workspace_root = "/opt/projects"
mode = "Auto"
auth = { Key = { path = "~/.ssh/python_key" } }

# 全局设置
default_server = "rust-dev"

[connection_settings]
timeout_seconds = 30
keepalive_interval = 60
compression = true
```

### 3. SSH 配置 (推荐)

在 `~/.ssh/config` 中配置主机：

```
# 开发服务器
Host dev.example.com
    HostName 192.168.1.100
    User developer
    Port 22
    IdentityFile ~/.ssh/dev_key
    Compression yes
    KeepAlive yes

# Python 服务器
Host python.internal
    HostName python.example.com
    User dev
    Port 2222
    IdentityFile ~/.ssh/python_key
    ProxyJump bastion.example.com
```

## 使用方法

### 1. 文件访问

#### 打开远程文件
```elisp
;; 方法 1: 直接使用 find-file
M-x find-file RET /lsp-proxy:rust-dev:/home/developer/project/src/main.rs RET

;; 方法 2: 使用便捷命令
M-x lsp-proxy-tramp-find-file RET
```

#### 浏览远程目录
```elisp
;; 使用 dired 浏览远程目录
M-x dired RET /lsp-proxy:rust-dev:/home/developer/projects/ RET

;; 或使用便捷命令
M-x lsp-proxy-tramp-dired RET
```

### 2. LSP 集成使用

#### 自动 LSP 启动
当打开远程文件时，系统会：
1. 检测文件类型（基于扩展名）
2. 在远程服务器上自动启动对应的 LSP 服务器
3. 建立 LSP 代理连接
4. 启用 lsp-mode

#### 支持的语言
- **Rust**: `.rs` 文件 → rust-analyzer
- **Python**: `.py` 文件 → pylsp/pyright  
- **TypeScript**: `.ts`, `.tsx` 文件 → typescript-language-server
- **Go**: `.go` 文件 → gopls
- **C/C++**: `.c`, `.cpp` 文件 → clangd
- **Java**: `.java` 文件 → eclipse.jdt.ls

#### LSP 功能
所有标准 LSP 功能都通过代理工作：
- **代码补全**: `company-mode` 或 `corfu`
- **跳转定义**: `lsp-find-definition`
- **查找引用**: `lsp-find-references`
- **重命名符号**: `lsp-rename`
- **格式化代码**: `lsp-format-buffer`
- **代码诊断**: flycheck/flymake 集成

### 3. 高级功能

#### 工作空间管理
```elisp
;; 查看远程工作空间
M-x lsp-proxy-remote-list-workspaces

;; 切换工作空间根目录
M-x lsp-workspace-folders-add RET /lsp-proxy:server:/new/root RET
```

#### 多项目支持
```elisp
;; 同时打开多个远程项目
(find-file "/lsp-proxy:rust-dev:/home/dev/project1/src/main.rs")
(find-file "/lsp-proxy:python-server:/opt/project2/app.py")
```

## 性能优化

### 1. 连接优化
```elisp
;; 增加连接缓存时间
(setq lsp-proxy-tramp-cache-timeout 600) ; 10 分钟

;; 启用压缩传输
(setq lsp-proxy-remote-connection-settings
      '((compression . t)
        (keepalive_interval . 30)))
```

### 2. LSP 优化
```elisp
;; 减少 LSP 请求频率
(setq lsp-idle-delay 0.5
      lsp-completion-provider :capf
      lsp-enable-file-watchers nil) ; 禁用文件监控以减少网络流量
```

### 3. 大文件处理
```elisp
;; 大文件模式阈值
(setq large-file-warning-threshold (* 10 1024 1024)) ; 10MB

;; 对大文件禁用某些功能
(add-hook 'lsp-proxy-tramp-large-file-hook
          (lambda ()
            (setq-local lsp-enable-semantic-highlighting nil
                        lsp-enable-imenu nil)))
```

## 故障排除

### 1. 连接问题
```elisp
;; 检查连接状态
M-x lsp-proxy-remote-status

;; 刷新服务器列表
M-x lsp-proxy-remote-refresh-server-list

;; 重新连接服务器
M-x lsp-proxy-remote-disconnect RET server-name RET
M-x lsp-proxy-remote-connect RET server-name RET
```

### 2. LSP 问题
```elisp
;; 查看 LSP 日志
M-x lsp-workspace-show-log

;; 重启 LSP 服务器
M-x lsp-workspace-restart

;; 检查远程 LSP 服务器状态
M-x lsp-proxy-remote-lsp-status
```

### 3. 文件访问问题
```elisp
;; 清理 TRAMP 缓存
M-x tramp-cleanup-all-connections

;; 启用详细日志
(setq tramp-verbose 6
      lsp-proxy-log-level 3)
```

## 键盘快捷键

默认快捷键绑定（在 `lsp-proxy-remote-mode` 下）：

```
C-c r f  lsp-proxy-tramp-find-file      ; 打开远程文件
C-c r d  lsp-proxy-tramp-dired          ; 远程目录浏览  
C-c r c  lsp-proxy-remote-connect       ; 连接服务器
C-c r D  lsp-proxy-remote-disconnect    ; 断开连接
C-c r l  lsp-proxy-remote-list-servers  ; 列出服务器
C-c r s  lsp-proxy-remote-status        ; 查看状态
C-c r w  lsp-proxy-remote-list-workspaces ; 列出工作空间
C-c r r  lsp-proxy-remote-refresh-server-list ; 刷新服务器列表
```

## 最佳实践

### 1. 项目组织
```
# 远程项目结构建议
/home/developer/
├── projects/
│   ├── rust-project/
│   │   ├── Cargo.toml
│   │   └── src/
│   ├── python-project/
│   │   ├── pyproject.toml  
│   │   └── src/
│   └── shared-libs/
└── .config/
    └── lsp-servers/  # LSP 服务器配置
```

### 2. 工作流建议
1. **项目开始**: 连接到远程服务器
2. **文件浏览**: 使用 `dired` 浏览项目结构
3. **代码编辑**: 直接打开文件，自动启用 LSP
4. **调试和测试**: 使用远程终端或集成工具
5. **版本控制**: 在远程服务器上操作 Git

### 3. 团队协作
- 使用共享的远程服务器配置
- 统一 LSP 服务器版本和配置
- 建立代码审查和同步机制

## 扩展和自定义

### 1. 自定义语言支持
```elisp
;; 添加新语言的 LSP 支持
(add-to-list 'lsp-proxy-tramp-language-map
             '("dart" . "dart_language_server"))

;; 自定义文件类型检测
(add-to-list 'lsp-proxy-tramp-file-extensions
             '("kt" . "kotlin"))
```

### 2. 钩子函数
```elisp
;; 远程文件打开时的钩子
(add-hook 'lsp-proxy-tramp-file-opened-hook
          (lambda ()
            (message "Opened remote file: %s" buffer-file-name)))

;; LSP 服务器启动成功钩子  
(add-hook 'lsp-proxy-tramp-lsp-started-hook
          (lambda (server-id language)
            (message "LSP server %s started for %s" server-id language)))
```

这个现代化方案将 TRAMP 的文件系统抽象与 LSP-Proxy 的远程能力完美结合，提供了类似本地开发的远程开发体验。