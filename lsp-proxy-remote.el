;;; lsp-proxy-remote.el --- Remote development integration for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2024 LSP-Proxy

;; Author: LSP-Proxy Team
;; Keywords: remote, development, lsp, proxy
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (lsp-proxy "0.4.0"))

;;; Commentary:

;; This package provides Emacs integration for lsp-proxy remote development
;; features, allowing seamless remote coding with full LSP support.
;;
;; This module extends lsp-proxy.el with remote development capabilities.
;; It integrates with the existing lsp-proxy--connection and mode system.

;;; Code:

(require 'lsp-proxy)  ; 依赖主模块
(require 'jsonrpc)
(require 'project)
(require 'cl-lib)

(defgroup lsp-proxy-remote nil
  "Remote development integration for lsp-proxy."
  :group 'lsp-proxy
  :prefix "lsp-proxy-remote-")

(defcustom lsp-proxy-remote-auto-connect nil
  "Whether to automatically connect to default remote server on startup."
  :type 'boolean
  :group 'lsp-proxy-remote)

(defcustom lsp-proxy-remote-default-server nil
  "Name of the default remote server to connect to."
  :type '(choice (const nil) string)
  :group 'lsp-proxy-remote)

(defcustom lsp-proxy-remote-connection-timeout 30
  "Timeout in seconds for remote connections."
  :type 'integer
  :group 'lsp-proxy-remote)

(defvar lsp-proxy-remote--connected-servers '()
  "List of currently connected remote servers.")

(defvar lsp-proxy-remote--server-cache (make-hash-table :test 'equal)
  "Cache for remote server configurations.")

;;; Integration with main lsp-proxy

(defun lsp-proxy-remote--ensure-connection ()
  "Ensure lsp-proxy connection is active before remote operations."
  (unless (and (boundp 'lsp-proxy--connection) 
               lsp-proxy--connection
               (lsp-proxy--connection-alivep))
    (user-error "LSP-Proxy not connected. Please start lsp-proxy-mode first")))

(defun lsp-proxy-remote--send-request (method params &optional callback)
  "Send a remote request to lsp-proxy with METHOD and PARAMS.
If CALLBACK is provided, call it with the result."
  (lsp-proxy-remote--ensure-connection)
  (if callback
      (jsonrpc-async-request lsp-proxy--connection method params
                           :success-fn callback
                           :error-fn (lambda (error)
                                       (message "Remote request failed: %s" error)))
    (jsonrpc-request lsp-proxy--connection method params)))

(defun lsp-proxy-remote--send-async-request (method params callback)
  "Send async remote request to lsp-proxy with METHOD and PARAMS.
Call CALLBACK with the result when complete."
  (lsp-proxy-remote--send-request method params callback))

;;; Integration with existing lsp-proxy status

(defun lsp-proxy-remote--add-to-status-buffer ()
  "Add remote status information to lsp-proxy status buffer."
  (when (get-buffer "*lsp-proxy-status*")
    (with-current-buffer "*lsp-proxy-status*"
      (goto-char (point-max))
      (insert "\n=== Remote Development Status ===\n")
      (insert (format "Remote mode enabled: %s\n" 
                      (if (bound-and-true-p lsp-proxy-remote-mode) "Yes" "No")))
      (insert (format "Connected servers: %d\n" (length lsp-proxy-remote--connected-servers)))
      (when lsp-proxy-remote--connected-servers
        (insert "Active connections:\n")
        (dolist (server lsp-proxy-remote--connected-servers)
          (insert (format "  - %s\n" server)))))))

(defun lsp-proxy-remote--status-advice (orig-fun &rest args)
  "Advice to add remote status to lsp-proxy-status command."
  (let ((result (apply orig-fun args)))
    (lsp-proxy-remote--add-to-status-buffer)
    result))

;;; Mode integration

(defun lsp-proxy-remote--mode-enter-hook ()
  "Hook function called when lsp-proxy-mode is enabled."
  (when (and lsp-proxy-remote-mode lsp-proxy-remote-auto-connect)
    (lsp-proxy-remote--auto-connect)))

(defun lsp-proxy-remote--mode-exit-hook ()
  "Hook function called when lsp-proxy-mode is disabled."
  (when lsp-proxy-remote-mode
    ;; Disconnect all remote servers when main mode exits
    (dolist (server lsp-proxy-remote--connected-servers)
      (lsp-proxy-remote-disconnect server))))

;;; Enhanced connection status tracking

(defun lsp-proxy-remote--update-connected-servers (server-name action)
  "Update connected servers list based on ACTION (connect/disconnect) for SERVER-NAME."
  (pcase action
    ('connect
     (add-to-list 'lsp-proxy-remote--connected-servers server-name)
     (message "Remote server '%s' connected. Total: %d" 
              server-name (length lsp-proxy-remote--connected-servers)))
    ('disconnect
     (setq lsp-proxy-remote--connected-servers
           (remove server-name lsp-proxy-remote--connected-servers))
     (message "Remote server '%s' disconnected. Remaining: %d" 
              server-name (length lsp-proxy-remote--connected-servers)))))

;;; Server Management Commands

;;;###autoload
(defun lsp-proxy-remote-list-servers ()
  "List all available remote servers."
  (interactive)
  (lsp-proxy-remote--send-request
   "emacs/remoteList" (list)
   (lambda (result)
     (let ((servers (alist-get 'servers result))
           (connected-count (alist-get 'connected_count result))
           (total-count (alist-get 'total_count result)))
       (with-current-buffer (get-buffer-create "*LSP-Proxy Remote Servers*")
         (erase-buffer)
         (insert (format "Remote Servers (%d/%d connected)\n"
                        connected-count total-count))
         (insert (make-string 50 ?=) "\n\n")
         (dolist (server servers)
           (let ((name (alist-get 'name server))
                 (connected (alist-get 'connected server))
                 (host (alist-get 'host server))
                 (user (alist-get 'user server))
                 (mode (alist-get 'mode server)))
             (insert (format "• %s %s\n"
                           name
                           (if connected "(connected)" "(disconnected)")))
             (insert (format "  Host: %s@%s\n" user host))
             (insert (format "  Mode: %s\n\n" mode))))
         (goto-char (point-min))
         (display-buffer (current-buffer)))))))

;;;###autoload
(defun lsp-proxy-remote-connect (server-name &optional host user port)
  "Connect to remote SERVER-NAME.
Optionally override HOST, USER, and PORT."
  (interactive 
   (list (completing-read "Server name: " 
                         (lsp-proxy-remote--get-available-servers))))
  (let ((params `((server_name . ,server-name))))
    (when host (push `(host . ,host) params))
    (when user (push `(user . ,user) params))
    (when port (push `(port . ,port) params))
    
    (message "Connecting to %s..." server-name)
    (lsp-proxy-remote--send-async-request
     "emacs/remoteConnect" params
     (lambda (result)
       (if (alist-get 'success result)
           (progn
             (lsp-proxy-remote--update-connected-servers server-name 'connect)
             (run-hooks 'lsp-proxy-remote-connected-hook))
         (message "Failed to connect to %s: %s" 
                 server-name 
                 (alist-get 'message result)))))))

;;;###autoload
(defun lsp-proxy-remote-disconnect (server-name)
  "Disconnect from remote SERVER-NAME."
  (interactive 
   (list (completing-read "Server name: " 
                         lsp-proxy-remote--connected-servers)))
  (message "Disconnecting from %s..." server-name)
  (lsp-proxy-remote--send-async-request
   "emacs/remoteDisconnect" `((server_name . ,server-name))
   (lambda (result)
     (if (alist-get 'success result)
         (progn
           (lsp-proxy-remote--update-connected-servers server-name 'disconnect)
           (run-hooks 'lsp-proxy-remote-disconnected-hook))
       (message "Failed to disconnect from %s: %s" 
               server-name 
               (alist-get 'message result))))))

;;;###autoload
(defun lsp-proxy-remote-status (server-name)
  "Get status of remote SERVER-NAME."
  (interactive 
   (list (completing-read "Server name: " 
                         (lsp-proxy-remote--get-available-servers))))
  (lsp-proxy-remote--send-request
   "emacs/remoteStatus" `((server_name . ,server-name))
   (lambda (result)
     (let ((name (alist-get 'name result))
           (connected (alist-get 'connected result))
           (host (alist-get 'host result))
           (user (alist-get 'user result))
           (port (alist-get 'port result))
           (mode (alist-get 'mode result))
           (workspace-root (alist-get 'workspace_root result)))
       (with-current-buffer (get-buffer-create "*LSP-Proxy Server Status*")
         (erase-buffer)
         (insert (format "Server Status: %s\n" name))
         (insert (make-string 30 ?=) "\n\n")
         (insert (format "Status: %s\n" (if connected "Connected" "Disconnected")))
         (insert (format "Host: %s@%s:%s\n" user host port))
         (insert (format "Mode: %s\n" mode))
         (insert (format "Workspace: %s\n" workspace-root))
         (goto-char (point-min))
         (display-buffer (current-buffer)))))))

;;; File Operations

;;;###autoload
(defun lsp-proxy-remote-open-file (server-name file-path)
  "Open FILE-PATH from remote SERVER-NAME."
  (interactive 
   (let ((server (completing-read "Server: " lsp-proxy-remote--connected-servers)))
     (list server 
           (read-file-name "Remote file: " "/"))))
  (lsp-proxy-remote--send-async-request
   "emacs/remoteFileRead" `((server_name . ,server-name)
                           (file_path . ,file-path))
   (lambda (result)
     (if (alist-get 'success result)
         (let ((content (alist-get 'content result))
               (size (alist-get 'size result)))
           (with-current-buffer (get-buffer-create 
                               (format "*Remote: %s:%s*" server-name file-path))
             (erase-buffer)
             (insert content)
             (set-buffer-modified-p nil)
             (setq buffer-file-name file-path)
             (setq-local lsp-proxy-remote--server-name server-name)
             (setq-local lsp-proxy-remote--file-path file-path)
             (normal-mode)
             (message "Opened remote file %s (%d bytes)" file-path size)
             (display-buffer (current-buffer))))
       (message "Failed to open remote file: %s" (alist-get 'message result))))))

;;;###autoload
(defun lsp-proxy-remote-save-file ()
  "Save current buffer to remote server."
  (interactive)
  (if (and (boundp 'lsp-proxy-remote--server-name)
           (boundp 'lsp-proxy-remote--file-path)
           lsp-proxy-remote--server-name
           lsp-proxy-remote--file-path)
      (let ((content (buffer-string))
            (server-name lsp-proxy-remote--server-name)
            (file-path lsp-proxy-remote--file-path))
        (lsp-proxy-remote--send-async-request
         "emacs/remoteFileWrite" `((server_name . ,server-name)
                                  (file_path . ,file-path)
                                  (content . ,content))
         (lambda (result)
           (if (alist-get 'success result)
               (progn
                 (set-buffer-modified-p nil)
                 (message "Saved to remote: %s" file-path))
             (message "Failed to save remote file: %s" 
                     (alist-get 'message result))))))
    (user-error "This buffer is not associated with a remote file")))

;;; Workspace Management

;;;###autoload
(defun lsp-proxy-remote-list-workspaces ()
  "List all remote workspaces."
  (interactive)
  (lsp-proxy-remote--send-request
   "emacs/remoteWorkspace" (list)
   (lambda (result)
     (let ((workspaces (alist-get 'workspaces result))
           (total-count (alist-get 'total_count result)))
       (with-current-buffer (get-buffer-create "*LSP-Proxy Remote Workspaces*")
         (erase-buffer)
         (insert (format "Remote Workspaces (%d total)\n" total-count))
         (insert (make-string 40 ?=) "\n\n")
         (dolist (workspace workspaces)
           (let ((server-name (alist-get 'server_name workspace))
                 (workspace-root (alist-get 'workspace_root workspace))
                 (host (alist-get 'host workspace))
                 (connected (alist-get 'connected workspace)))
             (insert (format "• %s %s\n"
                           server-name
                           (if connected "(connected)" "(disconnected)")))
             (insert (format "  Host: %s\n" host))
             (insert (format "  Workspace: %s\n\n" workspace-root))))
         (goto-char (point-min))
         (display-buffer (current-buffer)))))))

;;; LSP Integration

;;;###autoload
(defun lsp-proxy-remote-lsp-request (server-name lsp-server-id method params)
  "Send LSP request to remote SERVER-NAME.
LSP-SERVER-ID identifies the language server, METHOD is the LSP method,
PARAMS are the LSP parameters."
  (interactive 
   (let ((server (completing-read "Server: " lsp-proxy-remote--connected-servers))
         (lsp-id (read-number "LSP Server ID: " 0))
         (method (read-string "LSP Method: "))
         (params (read-string "LSP Params (JSON): " "{}")))
     (list server lsp-id method (json-parse-string params))))
  (lsp-proxy-remote--send-async-request
   "emacs/remoteLspRequest" `((server_name . ,server-name)
                             (lsp_server_id . ,lsp-server-id)
                             (method . ,method)
                             (params . ,params))
   (lambda (result)
     (if (alist-get 'success result)
         (let ((lsp-result (alist-get 'result result)))
           (message "LSP request succeeded")
           (with-current-buffer (get-buffer-create "*LSP-Proxy Remote Result*")
             (erase-buffer)
             (insert (format "LSP Request Result: %s\n" method))
             (insert (make-string 40 ?=) "\n\n")
             (insert (json-encode lsp-result))
             (json-pretty-print-buffer)
             (goto-char (point-min))
             (display-buffer (current-buffer))))
       (message "LSP request failed: %s" (alist-get 'message result))))))

;;; Utility Functions

(defun lsp-proxy-remote--get-available-servers ()
  "Get list of available server names from cache or server."
  ;; This could be enhanced to cache server list
  '("dev-server" "staging" "production"))

(defun lsp-proxy-remote-refresh-server-list ()
  "Refresh the list of available servers."
  (interactive)
  (lsp-proxy-remote--send-request
   "emacs/remoteList" (list)
   (lambda (result)
     (let ((servers (alist-get 'servers result)))
       (clrhash lsp-proxy-remote--server-cache)
       (dolist (server servers)
         (let ((name (alist-get 'name server)))
           (puthash name server lsp-proxy-remote--server-cache)))
       (message "Refreshed server list: %d servers" (length servers))))))

;;; Mode and Minor Mode

(defvar lsp-proxy-remote-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r c") #'lsp-proxy-remote-connect)
    (define-key map (kbd "C-c r d") #'lsp-proxy-remote-disconnect)
    (define-key map (kbd "C-c r l") #'lsp-proxy-remote-list-servers)
    (define-key map (kbd "C-c r s") #'lsp-proxy-remote-status)
    (define-key map (kbd "C-c r o") #'lsp-proxy-remote-open-file)
    (define-key map (kbd "C-c r w") #'lsp-proxy-remote-list-workspaces)
    (define-key map (kbd "C-c r r") #'lsp-proxy-remote-refresh-server-list)
    (define-key map (kbd "C-c r S") #'lsp-proxy-remote-save-file)
    map)
  "Keymap for `lsp-proxy-remote-mode'.")

;;;###autoload
(define-minor-mode lsp-proxy-remote-mode
  "Minor mode for lsp-proxy remote development integration."
  :lighter " LSP-Remote"
  :keymap lsp-proxy-remote-mode-map
  :group 'lsp-proxy-remote
  (if lsp-proxy-remote-mode
      (progn
        ;; 集成到主模式的钩子中
        (add-hook 'lsp-proxy-mode-hook #'lsp-proxy-remote--mode-enter-hook)
        (add-hook 'lsp-proxy-mode-exit-hook #'lsp-proxy-remote--mode-exit-hook)
        
        ;; 添加状态显示的 advice
        (when (fboundp 'lsp-proxy-status)
          (advice-add 'lsp-proxy-status :around #'lsp-proxy-remote--status-advice))
        
        ;; 自动连接
        (when (and lsp-proxy-remote-auto-connect 
                   (bound-and-true-p lsp-proxy-mode)
                   (lsp-proxy--connection-alivep))
          (lsp-proxy-remote--auto-connect))
        
        (message "LSP-Proxy Remote mode enabled"))
    (progn
      ;; 清理钩子
      (remove-hook 'lsp-proxy-mode-hook #'lsp-proxy-remote--mode-enter-hook)
      (remove-hook 'lsp-proxy-mode-exit-hook #'lsp-proxy-remote--mode-exit-hook)
      
      ;; 移除 advice
      (when (fboundp 'lsp-proxy-status)
        (advice-remove 'lsp-proxy-status #'lsp-proxy-remote--status-advice))
      
      ;; 断开所有连接
      (dolist (server lsp-proxy-remote--connected-servers)
        (lsp-proxy-remote-disconnect server))
      
      (message "LSP-Proxy Remote mode disabled"))))

(defun lsp-proxy-remote--auto-connect ()
  "Auto-connect to default remote server if configured."
  (when (and lsp-proxy-remote-default-server
             (lsp-proxy--connection-alivep))
    (lsp-proxy-remote-connect lsp-proxy-remote-default-server)))

;;; Hooks

(defvar lsp-proxy-remote-connected-hook nil
  "Hook run after successfully connecting to a remote server.")

(defvar lsp-proxy-remote-disconnected-hook nil
  "Hook run after disconnecting from a remote server.")

;;; Integration with existing lsp-proxy

;;;###autoload
(defun lsp-proxy-remote-setup ()
  "Setup lsp-proxy remote integration."
  (interactive)
  ;; 确保主模式可用
  (unless (featurep 'lsp-proxy)
    (require 'lsp-proxy))
  
  ;; 启用远程模式
  (lsp-proxy-remote-mode 1)
  
  ;; 如果主模式已启用且连接活跃，则尝试自动连接
  (when (and (bound-and-true-p lsp-proxy-mode)
             (lsp-proxy--connection-alivep)
             lsp-proxy-remote-auto-connect)
    (lsp-proxy-remote--auto-connect))
  
  (when (called-interactively-p 'interactive)
    (message "LSP-Proxy Remote integration setup complete")))

;;;###autoload
(defun lsp-proxy-remote-enable-with-main-mode ()
  "Enable remote mode automatically when lsp-proxy-mode is enabled.
Add this to your init file for automatic integration."
  (add-hook 'lsp-proxy-mode-hook 
            (lambda () (when (not lsp-proxy-remote-mode)
                         (lsp-proxy-remote-mode 1)))))

(provide 'lsp-proxy-remote)

;;; lsp-proxy-remote.el ends here