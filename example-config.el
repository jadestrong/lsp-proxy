;;; example-config.el --- LSP-Proxy 集成配置示例 -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;; 这个文件展示了如何在 Emacs 配置中集成 lsp-proxy.el 和 lsp-proxy-remote.el
;; 适用于不同的配置管理方式 (use-package, straight, 手动配置等)

;;; Code:

;; =============================================================================
;; 方案 1: use-package 配置 (推荐)
;; =============================================================================

(use-package lsp-proxy
  :load-path "~/.doom.d/extensions/lsp-proxy"  ; 或你的实际路径
  :custom
  (lsp-proxy-log-max 1000)
  (lsp-proxy-idle-delay 0.5)
  :config
  ;; 启用全局模式
  (global-lsp-proxy-mode 1)
  
  ;; 可选：自定义键绑定
  (define-key lsp-proxy-mode-map (kbd "C-c l s") #'lsp-proxy-status)
  (define-key lsp-proxy-mode-map (kbd "C-c l r") #'lsp-proxy-restart))

(use-package lsp-proxy-remote
  :after lsp-proxy
  :load-path "~/.doom.d/extensions/lsp-proxy"
  :custom
  (lsp-proxy-remote-auto-connect t)
  (lsp-proxy-remote-default-server "dev-server")
  (lsp-proxy-remote-connection-timeout 60)
  :config
  ;; 自动集成：当 lsp-proxy-mode 启用时自动启用远程模式
  (lsp-proxy-remote-enable-with-main-mode)
  
  ;; 添加钩子
  (add-hook 'lsp-proxy-remote-connected-hook
            (lambda () 
              (message "🌐 Remote development session started")
              (setq mode-line-misc-info 
                    (append mode-line-misc-info '(" [Remote]")))))
  
  (add-hook 'lsp-proxy-remote-disconnected-hook
            (lambda () 
              (message "📴 Remote development session ended")
              (setq mode-line-misc-info 
                    (remove " [Remote]" mode-line-misc-info))))
  
  ;; 可选：全局键绑定 
  (global-set-key (kbd "C-c R c") #'lsp-proxy-remote-connect)
  (global-set-key (kbd "C-c R l") #'lsp-proxy-remote-list-servers)
  (global-set-key (kbd "C-c R o") #'lsp-proxy-remote-open-file))

;; =============================================================================
;; 方案 2: 手动配置
;; =============================================================================

;; 如果不使用 use-package，可以这样配置：

;; ;; 添加路径
;; (add-to-list 'load-path "~/.doom.d/extensions/lsp-proxy")

;; ;; 加载模块
;; (require 'lsp-proxy)
;; (require 'lsp-proxy-remote)

;; ;; 基础配置
;; (setq lsp-proxy-log-max 1000
;;       lsp-proxy-remote-auto-connect t
;;       lsp-proxy-remote-default-server "dev-server")

;; ;; 启用模式
;; (global-lsp-proxy-mode 1)
;; (lsp-proxy-remote-mode 1)

;; ;; 集成设置
;; (lsp-proxy-remote-enable-with-main-mode)

;; =============================================================================
;; 方案 3: Doom Emacs 配置
;; =============================================================================

;; 在 ~/.doom.d/config.el 中添加：

;; (after! lsp-proxy
;;   ;; 基础配置
;;   (setq lsp-proxy-log-max 1000)
;;   (global-lsp-proxy-mode 1)
   
;;   ;; 加载远程模块
;;   (load! "extensions/lsp-proxy/lsp-proxy-remote")
   
;;   ;; 远程配置
;;   (setq lsp-proxy-remote-auto-connect t
;;         lsp-proxy-remote-default-server "dev-server")
   
;;   ;; 自动集成
;;   (lsp-proxy-remote-enable-with-main-mode)
   
;;   ;; Doom 风格的键绑定
;;   (map! :leader
;;         :prefix "c"
;;         :desc "Remote connect" "r c" #'lsp-proxy-remote-connect
;;         :desc "Remote list" "r l" #'lsp-proxy-remote-list-servers
;;         :desc "Remote open" "r o" #'lsp-proxy-remote-open-file))

;; =============================================================================
;; 高级配置示例
;; =============================================================================

;; 自定义远程文件保存函数
(defun my/lsp-proxy-remote-save-and-format ()
  "保存远程文件并格式化代码."
  (interactive)
  (when (and (boundp 'lsp-proxy-remote--server-name)
             lsp-proxy-remote--server-name)
    ;; 先格式化
    (lsp-format-buffer)
    ;; 等待格式化完成后保存
    (run-with-timer 0.5 nil #'lsp-proxy-remote-save-file)))

;; 自定义远程项目切换
(defun my/lsp-proxy-remote-switch-project ()
  "在远程服务器间切换工作空间."
  (interactive)
  (let ((server (completing-read "Switch to server: " 
                                lsp-proxy-remote--connected-servers)))
    (lsp-proxy-remote-list-workspaces)
    (message "Switched to remote server: %s" server)))

;; 状态栏显示远程连接状态
(defun my/lsp-proxy-remote-mode-line ()
  "在状态栏显示远程连接信息."
  (if (and (bound-and-true-p lsp-proxy-remote-mode)
           lsp-proxy-remote--connected-servers)
      (format " [R:%d]" (length lsp-proxy-remote--connected-servers))
    ""))

;; 添加到模式栏
(setq-default mode-line-format
              (append mode-line-format '((:eval (my/lsp-proxy-remote-mode-line)))))

;; =============================================================================
;; 项目相关集成
;; =============================================================================

;; 与 projectile 集成
(with-eval-after-load 'projectile
  (defun my/projectile-remote-project-root ()
    "获取远程项目根目录."
    (when (and (bound-and-true-p lsp-proxy-remote--server-name)
               (bound-and-true-p lsp-proxy-remote--file-path))
      ;; 这里可以实现远程项目根目录检测逻辑
      "/remote/project/root"))
  
  ;; 可以扩展 projectile 以支持远程项目
  )

;; 与 magit 集成 (如果需要远程 Git 操作)
(with-eval-after-load 'magit
  ;; 可以添加远程 Git 操作的支持
  )

;; =============================================================================
;; 调试和开发辅助
;; =============================================================================

;; 开发模式：启用详细日志
(defun my/lsp-proxy-debug-mode ()
  "启用 LSP-Proxy 调试模式."
  (interactive)
  (setq lsp-proxy-log-max 5000)
  (setq lsp-proxy-log-buffer-max t)
  (message "LSP-Proxy debug mode enabled"))

;; 快速重连函数
(defun my/lsp-proxy-remote-reconnect (server-name)
  "快速重连远程服务器."
  (interactive 
   (list (completing-read "Reconnect to: " 
                         (append lsp-proxy-remote--connected-servers
                                '("dev-server" "staging" "production")))))
  (when (member server-name lsp-proxy-remote--connected-servers)
    (lsp-proxy-remote-disconnect server-name))
  (lsp-proxy-remote-connect server-name))

;; 健康检查函数
(defun my/lsp-proxy-health-check ()
  "检查 LSP-Proxy 和远程连接的健康状态."
  (interactive)
  (let ((main-status (if (lsp-proxy--connection-alivep) "✓ 正常" "✗ 异常"))
        (remote-status (if lsp-proxy-remote--connected-servers 
                          (format "✓ %d 个连接" (length lsp-proxy-remote--connected-servers))
                        "- 无连接")))
    (message "LSP-Proxy状态: %s | 远程连接: %s" main-status remote-status)
    (when (called-interactively-p 'interactive)
      (lsp-proxy-status))))

;; =============================================================================
;; 自动化设置
;; =============================================================================

;; 自动保存远程文件
(add-hook 'lsp-proxy-remote-connected-hook
          (lambda ()
            ;; 启用自动保存
            (when (bound-and-true-p lsp-proxy-remote--server-name)
              (auto-save-mode 1))))

;; 文件改变时的自动同步 (可选)
(defvar my/lsp-proxy-remote-auto-sync nil
  "是否启用远程文件自动同步.")

(when my/lsp-proxy-remote-auto-sync
  (add-hook 'after-save-hook
            (lambda ()
              (when (and (bound-and-true-p lsp-proxy-remote--server-name)
                         (bound-and-true-p lsp-proxy-remote--file-path))
                (lsp-proxy-remote-save-file)))))

;; 提供这个配置模块
(provide 'example-config)

;;; example-config.el ends here