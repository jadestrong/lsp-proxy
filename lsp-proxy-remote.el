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

(require 'cl-lib)
(require 'json)

;; Forward declarations for lsp-proxy functions
(declare-function lsp-proxy--connection-alivep "lsp-proxy")
(declare-function jsonrpc-request "jsonrpc")
(declare-function jsonrpc-async-request "jsonrpc")
(defvar lsp-proxy--connection)
(defvar lsp-proxy-mode)
(defvar lsp-proxy-mode-hook)
(defvar lsp-proxy-mode-exit-hook)

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

;; Safe function definitions for when lsp-proxy is not loaded
(defun lsp-proxy-remote--connection-alivep-safe ()
  "Safely check if lsp-proxy connection is alive."
  (and (boundp 'lsp-proxy--connection)
       lsp-proxy--connection
       (if (fboundp 'lsp-proxy--connection-alivep)
           (lsp-proxy--connection-alivep)
         t)))  ; fallback

;;; Integration with main lsp-proxy

(defun lsp-proxy-remote--ensure-connection ()
  "Ensure lsp-proxy connection is active before remote operations."
  (unless (lsp-proxy-remote--connection-alivep-safe)
    (user-error "LSP-Proxy not connected. Please start lsp-proxy-mode first")))

(defun lsp-proxy-remote--send-request (method params &optional callback)
  "Send a remote request to lsp-proxy with METHOD and PARAMS.
If CALLBACK is provided, call it with the result.
Manually constructs the parameter format expected by lsp-proxy backend."
  (lsp-proxy-remote--ensure-connection)
  ;; Construct the wrapped params format that lsp-proxy backend expects
  ;; For remote requests, we typically don't need the current file URI
  (let ((wrapped-params (list :uri nil  ; Remote requests don't need current file URI
                              :params params)))
    (if callback
        (jsonrpc-async-request lsp-proxy--connection method wrapped-params
                               :success-fn callback
                               :error-fn (lambda (error)
                                           (message "Remote request failed: %s" error)))
      (jsonrpc-request lsp-proxy--connection method wrapped-params))))

(defun lsp-proxy-remote--send-async-request (method params callback)
  "Send async remote request to lsp-proxy with METHOD and PARAMS.
Call CALLBACK with the result when complete.
Manually constructs the parameter format expected by lsp-proxy backend."
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
  (when lsp-proxy-remote-mode
    ;; Refresh server list when entering remote mode
    (lsp-proxy-remote-refresh-server-list)
    ;; Auto-connect if configured
    (when lsp-proxy-remote-auto-connect
      (lsp-proxy-remote--auto-connect))))

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
   "emacs/remoteList" nil
   (lambda (result)
     (let ((servers (plist-get result :servers))
           (connected-count (plist-get result :connected_count))
           (total-count (plist-get result :total_count)))
       (with-current-buffer (get-buffer-create "*LSP-Proxy Remote Servers*")
         (erase-buffer)
         (insert (format "Remote Servers (%d/%d connected)\n"
                         connected-count total-count))
         (insert (make-string 50 ?=) "\n\n")
         (dolist (server servers)
           (let ((name (plist-get server :name))
                 (connected (plist-get server :connected))
                 (host (plist-get server :host))
                 (user (plist-get server :user))
                 (mode (plist-get server :mode)))
             (insert (format "• %s %s\n"
                             name
                             (if connected "(connected)" "(disconnected)")))
             (insert (format "  Host: %s@%s\n" user host))
             (insert (format "  Mode: %s\n\n" mode))))
         (goto-char (point-min))
         (display-buffer (current-buffer)))))))

;;;###autoload
(defun lsp-proxy-remote-connect (&optional server-name host user port)
  "Connect to remote SERVER-NAME.
If SERVER-NAME is not provided, prompt user to select from available servers.
Optionally override HOST, USER, and PORT."
  (interactive)
  ;; First, get available servers from the server
  (let* ((available-servers (lsp-proxy-remote--get-available-servers t)) ; force refresh
         (selected-server (or server-name
                              (if available-servers
                                  (completing-read "Server name: " available-servers)
                                (user-error "No remote servers available. Please check your lsp-proxy configuration")))))

    ;; Get additional parameters if called interactively
    (when (called-interactively-p 'interactive)
      (let ((server-info (gethash selected-server lsp-proxy-remote--server-cache)))
        (when (and server-info (y-or-n-p "Override server configuration? "))
          (setq host (read-string (format "Host (default: %s): " (plist-get server-info :host)) nil nil (plist-get server-info :host)))
          (setq user (read-string (format "User (default: %s): " (plist-get server-info :user)) nil nil (plist-get server-info :user)))
          (when (y-or-n-p "Specify custom port? ")
            (setq port (read-number "Port: "))))))
    
    ;; Build connection parameters
    (let ((params `((server_name . ,selected-server))))
      (when host (push `(host . ,host) params))
      (when user (push `(user . ,user) params))
      (when port (push `(port . ,port) params))
      
      (message "Connecting to %s..." selected-server)
      (lsp-proxy-remote--send-async-request
       "emacs/remoteConnect" params
       (lambda (result)
         (if (plist-get result :success)
             (progn
               (lsp-proxy-remote--update-connected-servers selected-server 'connect)
               (run-hooks 'lsp-proxy-remote-connected-hook)
               (message "Successfully connected to %s" selected-server))
           (message "Failed to connect to %s: %s" 
                    selected-server
                    (plist-get result :message))))))))

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
     (if (plist-get result :success)
         (progn
           (lsp-proxy-remote--update-connected-servers server-name 'disconnect)
           (run-hooks 'lsp-proxy-remote-disconnected-hook))
       (message "Failed to disconnect from %s: %s" 
                server-name
                (plist-get result :message))))))

;;;###autoload
(defun lsp-proxy-remote-status (&optional server-name)
  "Get status of remote SERVER-NAME.
If SERVER-NAME is not provided, prompt user to select from available servers."
  (interactive)
  (let* ((available-servers (lsp-proxy-remote--get-available-servers))
         (selected-server (or server-name
                              (if available-servers
                                  (completing-read "Server name: " available-servers)
                                (user-error "No remote servers available")))))
    (lsp-proxy-remote--send-request
     "emacs/remoteStatus" `((server_name . ,selected-server))
     (lambda (result)
       (let ((name (plist-get result :name))
             (connected (plist-get result :connected))
             (host (plist-get result :host))
             (user (plist-get result :user))
             (port (plist-get result :port))
             (mode (plist-get result :mode))
             (workspace-root (plist-get 'workspace_root result)))
         (with-current-buffer (get-buffer-create "*LSP-Proxy Server Status*")
           (erase-buffer)
           (insert (format "Server Status: %s\n" name))
           (insert (make-string 30 ?=) "\n\n")
           (insert (format "Status: %s\n" (if connected "Connected" "Disconnected")))
           (insert (format "Host: %s@%s:%s\n" user host port))
           (insert (format "Mode: %s\n" mode))
           (insert (format "Workspace: %s\n" workspace-root))
           (goto-char (point-min))
           (display-buffer (current-buffer))))))))

;;; File Operations

;;;###autoload
(defun lsp-proxy-remote-open-file (server-name file-path)
  "Open FILE-PATH from remote SERVER-NAME."
  (interactive 
   (let ((server (completing-read "Server: " lsp-proxy-remote--connected-servers)))
     (list server 
           (read-file-name "Remote file: " "/"))))
  ;; Validate file path
  (when (or (string-empty-p file-path)
            (not (string-match "^/" file-path)))
    (user-error "Invalid file path: %s (must be absolute path)" file-path))
  
  (message "Opening remote file: %s from %s..." file-path server-name)
  (lsp-proxy-remote--send-async-request
   "emacs/remoteFileRead" `((server_name . ,server-name)
                            (file_path . ,file-path))
   (lambda (result)
     (if (plist-get result :success)
         (let ((content (plist-get result :content))
               (size (plist-get result :size))
               ;; Safe buffer name without problematic characters
               (buffer-name (format "*Remote-%s-%s*" 
                                   (replace-regexp-in-string "[^a-zA-Z0-9_-]" "-" server-name)
                                   (replace-regexp-in-string "^/\\|[^a-zA-Z0-9_.-]" "-" 
                                                            (file-name-nondirectory file-path)))))
           (with-current-buffer (get-buffer-create buffer-name)
             (erase-buffer)
             (insert content)
             (set-buffer-modified-p nil)
             (setq buffer-file-name file-path)
             (setq-local lsp-proxy-remote--server-name server-name)
             (setq-local lsp-proxy-remote--file-path file-path)
             (normal-mode)
             (message "Opened remote file %s (%d bytes)" file-path size)
             (display-buffer (current-buffer))))
       (message "Failed to open remote file: %s" (plist-get result :message))))))

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
           (if (plist-get :success result)
               (progn
                 (set-buffer-modified-p nil)
                 (message "Saved to remote: %s" file-path))
             (message "Failed to save remote file: %s" 
                      (plist-get :message result))))))
    (user-error "This buffer is not associated with a remote file")))

;;; Workspace Management

;;;###autoload
(defun lsp-proxy-remote-list-workspaces ()
  "List all remote workspaces."
  (interactive)
  (lsp-proxy-remote--send-request
   "emacs/remoteWorkspace" nil
   (lambda (result)
     (let ((workspaces (plist-get :workspaces result))
           (total-count (plist-get :total_count result)))
       (with-current-buffer (get-buffer-create "*LSP-Proxy Remote Workspaces*")
         (erase-buffer)
         (insert (format "Remote Workspaces (%d total)\n" total-count))
         (insert (make-string 40 ?=) "\n\n")
         (dolist (workspace workspaces)
           (let ((server-name (plist-get workspace :server_name))
                 (workspace-root (plist-get workspace :workspace_root))
                 (host (plist-get workspace :host))
                 (connected (plist-get workspace :connected)))
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
     (if (plist-get result :success)
         (let ((lsp-result (plist-get result :result)))
           (message "LSP request succeeded")
           (with-current-buffer (get-buffer-create "*LSP-Proxy Remote Result*")
             (erase-buffer)
             (insert (format "LSP Request Result: %s\n" method))
             (insert (make-string 40 ?=) "\n\n")
             (insert (json-encode lsp-result))
             (json-pretty-print-buffer)
             (goto-char (point-min))
             (display-buffer (current-buffer))))
       (message "LSP request failed: %s" (plist-get result :message))))))

;;; Utility Functions

(defun lsp-proxy-remote--get-available-servers (&optional force-refresh)
  "Get list of available server names from server.
If FORCE-REFRESH is non-nil, always fetch from server.
Otherwise, use cache if available."
  (if (and (not force-refresh) (> (hash-table-count lsp-proxy-remote--server-cache) 0))
      ;; Use cached server names
      (hash-table-keys lsp-proxy-remote--server-cache)
    ;; Fetch from server synchronously
    (let ((result (lsp-proxy-remote--send-request "emacs/remoteList" nil)))
      (if result
          (let ((servers (plist-get result :servers)))
            (clrhash lsp-proxy-remote--server-cache)
            (mapcar (lambda (server)
                      (let ((name (plist-get server :name)))
                        (puthash name server lsp-proxy-remote--server-cache)
                        name))
                    servers))
        ;; Fallback if server request fails
        (progn
          (message "Warning: Could not fetch server list from lsp-proxy")
          '())))))

(defun lsp-proxy-remote-refresh-server-list ()
  "Refresh the list of available servers from lsp-proxy."
  (interactive)
  (message "Refreshing server list...")
  (lsp-proxy-remote--send-request
   "emacs/remoteList" nil
   (lambda (result)
     (let ((servers (plist-get result :servers)))
       (clrhash lsp-proxy-remote--server-cache)
       (dolist (server servers)
         (let ((name (plist-get server :name)))
           (puthash name server lsp-proxy-remote--server-cache)))
       (message "Refreshed server list: %d servers available" (length servers))
       ;; Also update connected servers list based on actual status
       (setq lsp-proxy-remote--connected-servers
             (mapcar (lambda (server) (plist-get server :name))
                     (seq-filter (lambda (server) (plist-get server :connected))
                                 servers)))))))

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
             (lsp-proxy-remote--connection-alivep-safe))
    ;; Check if the default server is in available servers
    (let ((available-servers (lsp-proxy-remote--get-available-servers)))
      (if (member lsp-proxy-remote-default-server available-servers)
          (progn
            (message "Auto-connecting to default server: %s" lsp-proxy-remote-default-server)
            (lsp-proxy-remote-connect lsp-proxy-remote-default-server))
        (message "Warning: Default server '%s' not found in available servers: %s" 
                 lsp-proxy-remote-default-server
                 (string-join available-servers ", "))))))

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
  
  ;; 如果主模式已启用且连接活跃，则刷新服务器列表并尝试自动连接
  (when (and (bound-and-true-p lsp-proxy-mode)
             (lsp-proxy-remote--connection-alivep-safe))
    ;; First refresh server list
    (lsp-proxy-remote-refresh-server-list)
    ;; Then auto-connect if configured
    (when lsp-proxy-remote-auto-connect
      (lsp-proxy-remote--auto-connect)))
  
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
