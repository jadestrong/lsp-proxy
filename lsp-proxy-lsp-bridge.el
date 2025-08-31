;;; lsp-proxy-lsp-bridge.el --- LSP bridge for remote development -*- lexical-binding: t; -*-

;; Copyright (C) 2024 LSP-Proxy
;; Author: LSP-Proxy Team  
;; Keywords: remote, development, lsp, proxy, bridge
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (lsp-mode "8.0") (jsonrpc "1.0"))

;;; Commentary:

;; This module provides the LSP bridge functionality that allows lsp-mode
;; to communicate with remote LSP servers through lsp-proxy connections.
;;
;; Key features:
;; - Custom LSP client that proxies requests through lsp-proxy
;; - Automatic language server detection and startup
;; - Seamless integration with existing lsp-mode workflows
;; - Support for all standard LSP features

;;; Code:

(require 'lsp-mode)
(require 'jsonrpc)
(require 'lsp-proxy-tramp)
(require 'cl-lib)

(defgroup lsp-proxy-bridge nil
  "LSP bridge for remote development through lsp-proxy."
  :group 'lsp-proxy-tramp
  :prefix "lsp-proxy-bridge-")

(defcustom lsp-proxy-bridge-server-startup-timeout 30
  "Timeout in seconds for LSP server startup."
  :type 'integer
  :group 'lsp-proxy-bridge)

(defcustom lsp-proxy-bridge-request-timeout 10  
  "Timeout in seconds for LSP requests."
  :type 'integer
  :group 'lsp-proxy-bridge)

;;; Language server mappings

(defvar lsp-proxy-bridge-language-servers
  '(("rust" . "rust-analyzer")
    ("python" . "pylsp")
    ("typescript" . "typescript-language-server") 
    ("javascript" . "typescript-language-server")
    ("go" . "gopls")
    ("c" . "clangd")
    ("cpp" . "clangd")
    ("java" . "jdtls")
    ("ruby" . "solargraph")
    ("php" . "phpactor")
    ("dart" . "dart_language_server")
    ("kotlin" . "kotlin_language_server"))
  "Mapping from language IDs to LSP server names.")

(defvar lsp-proxy-bridge-file-extensions
  '(("rs" . "rust")
    ("py" . "python") 
    ("pyi" . "python")
    ("ts" . "typescript")
    ("tsx" . "typescript") 
    ("js" . "javascript")
    ("jsx" . "javascript")
    ("go" . "go")
    ("c" . "c")
    ("h" . "c")
    ("cpp" . "cpp")
    ("cc" . "cpp")
    ("cxx" . "cpp") 
    ("hpp" . "cpp")
    ("java" . "java")
    ("rb" . "ruby")
    ("php" . "php")
    ("dart" . "dart")
    ("kt" . "kotlin"))
  "Mapping from file extensions to language IDs.")

;;; LSP Connection Management

(defvar-local lsp-proxy-bridge--server-name nil
  "Remote server name for current buffer.")

(defvar-local lsp-proxy-bridge--lsp-server-id nil
  "Remote LSP server ID for current buffer.")

(defvar-local lsp-proxy-bridge--language-id nil
  "Language ID for current buffer.")

(defvar-local lsp-proxy-bridge--workspace-root nil
  "Workspace root for current buffer.")

(defvar lsp-proxy-bridge--active-connections (make-hash-table :test 'equal)
  "Hash table tracking active LSP bridge connections.")

;;; LSP Client Implementation

(defclass lsp-proxy-bridge-client (lsp--client)
  ((server-name :initarg :server-name :type string)
   (lsp-server-id :initarg :lsp-server-id :type integer)
   (language-id :initarg :language-id :type string))
  :documentation "LSP client that proxies through lsp-proxy.")

(defun lsp-proxy-bridge-create-client (server-name lsp-server-id language-id workspace-root)
  "Create LSP client for remote server."
  (let ((client-id (intern (format "lsp-proxy-%s-%s-%d" 
                                   server-name language-id lsp-server-id))))
    (make-lsp-proxy-bridge-client
     :server-id client-id
     :server-name server-name
     :lsp-server-id lsp-server-id
     :language-id language-id
     :new-connection (lsp-proxy-bridge-create-connection server-name lsp-server-id)
     :activation-fn (lambda (file-name _mode)
                     (lsp-proxy-bridge-should-activate-p file-name server-name workspace-root))
     :priority 100  ; Higher priority than default clients
     :notification-handlers (ht)
     :request-handlers (ht)
     :response-handlers (ht)
     :prefix-function nil
     :uri-handlers (ht)
     :action-handlers (ht)
     :major-modes '()  ; We handle activation manually
     :capabilities '()))) ; Will be populated from remote server

(defun lsp-proxy-bridge-should-activate-p (file-name server-name workspace-root)
  "Check if LSP client should activate for FILE-NAME."
  (and (string-prefix-p (format "/lsp-proxy:%s:" server-name) file-name)
       (or (not workspace-root)
           (string-prefix-p workspace-root 
                           (lsp-proxy-tramp-extract-remote-path file-name)))))

(defun lsp-proxy-bridge-extract-remote-path (tramp-file-name)
  "Extract remote path from TRAMP file name."
  (when (string-match "^/lsp-proxy:[^:]+:\\(.+\\)$" tramp-file-name)
    (match-string 1 tramp-file-name)))

;;; Connection Implementation

(defun lsp-proxy-bridge-create-connection (server-name lsp-server-id)
  "Create LSP connection that proxies through lsp-proxy."
  (lambda ()
    (lsp-proxy-bridge-stdio-connection server-name lsp-server-id)))

(defun lsp-proxy-bridge-stdio-connection (server-name lsp-server-id)
  "Create stdio-style connection for LSP bridge."
  (let* ((connection-name (format "lsp-proxy-bridge-%s-%d" server-name lsp-server-id))
         (process (make-process
                   :name connection-name
                   :command '("cat")  ; Dummy command, we handle I/O manually
                   :coding 'utf-8-unix
                   :noquery t
                   :filter #'lsp-proxy-bridge-process-filter
                   :sentinel #'lsp-proxy-bridge-process-sentinel))
         (connection (make-instance 'jsonrpc-process-connection
                                   :process process
                                   :name connection-name)))
    
    ;; Store connection metadata
    (process-put process :lsp-proxy-server-name server-name)
    (process-put process :lsp-proxy-lsp-server-id lsp-server-id)
    (process-put process :lsp-proxy-connection connection)
    
    ;; Register connection
    (puthash connection-name connection lsp-proxy-bridge--active-connections)
    
    connection))

(defun lsp-proxy-bridge-process-filter (process output)
  "Process filter for LSP bridge connections."
  (let ((connection (process-get process :lsp-proxy-connection)))
    (when connection
      ;; Forward output to jsonrpc connection
      (jsonrpc--process-filter connection process output))))

(defun lsp-proxy-bridge-process-sentinel (process event)
  "Process sentinel for LSP bridge connections."  
  (let ((connection (process-get process :lsp-proxy-connection)))
    (when connection
      (jsonrpc--process-sentinel connection process event))))

;;; Request/Response Handling

(cl-defmethod jsonrpc-request ((connection jsonrpc-process-connection) method params &key id)
  "Override jsonrpc-request to proxy through lsp-proxy."
  (let* ((process (jsonrpc--process connection))
         (server-name (process-get process :lsp-proxy-server-name))
         (lsp-server-id (process-get process :lsp-proxy-lsp-server-id)))
    
    (if (and server-name lsp-server-id)
        ;; Proxy request through lsp-proxy
        (lsp-proxy-bridge-send-lsp-request server-name lsp-server-id method params id)
      ;; Fallback to default behavior
      (cl-call-next-method))))

(cl-defmethod jsonrpc-notify ((connection jsonrpc-process-connection) method params)
  "Override jsonrpc-notify to proxy through lsp-proxy."
  (let* ((process (jsonrpc--process connection))
         (server-name (process-get process :lsp-proxy-server-name))
         (lsp-server-id (process-get process :lsp-proxy-lsp-server-id)))
    
    (if (and server-name lsp-server-id)
        ;; Proxy notification through lsp-proxy
        (lsp-proxy-bridge-send-lsp-notification server-name lsp-server-id method params)
      ;; Fallback to default behavior  
      (cl-call-next-method))))

(defun lsp-proxy-bridge-send-lsp-request (server-name lsp-server-id method params request-id)
  "Send LSP request through lsp-proxy connection."
  (lsp-proxy-remote--send-async-request
   "emacs/remoteLspRequest"
   `((server_name . ,server-name)
     (lsp_server_id . ,lsp-server-id)
     (method . ,method)
     (params . ,params)
     (request_id . ,request-id))
   (lambda (result)
     (if (plist-get result :success)
         (let ((lsp-result (plist-get result :result)))
           ;; Handle successful response
           (lsp-proxy-bridge-handle-lsp-response request-id lsp-result))
       ;; Handle error response
       (lsp-proxy-bridge-handle-lsp-error request-id (plist-get result :message))))))

(defun lsp-proxy-bridge-send-lsp-notification (server-name lsp-server-id method params)
  "Send LSP notification through lsp-proxy connection."
  (lsp-proxy-remote--send-async-request
   "emacs/remoteLspNotification"
   `((server_name . ,server-name)
     (lsp_server_id . ,lsp-server-id)
     (method . ,method)
     (params . ,params))
   (lambda (result)
     (unless (plist-get result :success)
       (message "LSP notification failed: %s" (plist-get result :message))))))

(defun lsp-proxy-bridge-handle-lsp-response (request-id result)
  "Handle successful LSP response."
  ;; Find the connection and forward the response
  (maphash (lambda (_name connection)
             (when (jsonrpc--request-get connection request-id)
               (jsonrpc--handle-reply connection request-id result nil)))
           lsp-proxy-bridge--active-connections))

(defun lsp-proxy-bridge-handle-lsp-error (request-id error-message)
  "Handle LSP error response."
  ;; Find the connection and forward the error
  (maphash (lambda (_name connection)
             (when (jsonrpc--request-get connection request-id)
               (jsonrpc--handle-reply connection request-id nil 
                                     (list :code -1 :message error-message))))
           lsp-proxy-bridge--active-connections))

;;; Auto-activation for TRAMP files

(defun lsp-proxy-bridge-auto-setup ()
  "Automatically setup LSP bridge for lsp-proxy TRAMP files."
  (when (lsp-proxy-bridge-is-tramp-file-p buffer-file-name)
    (lsp-proxy-bridge-setup-for-buffer)))

(defun lsp-proxy-bridge-is-tramp-file-p (file-name)
  "Check if FILE-NAME is a lsp-proxy TRAMP file."
  (and file-name
       (string-match "^/lsp-proxy:" file-name)))

(defun lsp-proxy-bridge-setup-for-buffer ()
  "Setup LSP bridge for current buffer."
  (when-let* ((file-name buffer-file-name)
              (parsed-name (lsp-proxy-bridge-parse-tramp-file-name file-name))
              (server-name (plist-get parsed-name :server-name))
              (remote-path (plist-get parsed-name :remote-path))
              (file-ext (file-name-extension remote-path))
              (language-id (cdr (assoc file-ext lsp-proxy-bridge-file-extensions))))
    
    ;; Store buffer-local variables
    (setq-local lsp-proxy-bridge--server-name server-name)
    (setq-local lsp-proxy-bridge--language-id language-id)
    (setq-local lsp-proxy-bridge--workspace-root (file-name-directory remote-path))
    
    ;; Start remote LSP server
    (lsp-proxy-bridge-start-lsp-server server-name language-id remote-path)))

(defun lsp-proxy-bridge-parse-tramp-file-name (file-name)
  "Parse lsp-proxy TRAMP file name into components."
  (when (string-match "^/lsp-proxy:\\([^:]+\\):\\(.+\\)$" file-name)
    (list :server-name (match-string 1 file-name)
          :remote-path (match-string 2 file-name))))

(defun lsp-proxy-bridge-start-lsp-server (server-name language-id remote-path)
  "Start LSP server for language on remote server."
  (let ((workspace-root (file-name-directory remote-path)))
    (lsp-proxy-remote--send-async-request
     "emacs/remoteStartLspServer"
     `((server_name . ,server-name)
       (language_id . ,language-id)
       (workspace_root . ,workspace-root))
     (lambda (result)
       (if (plist-get result :success)
           (let ((lsp-server-id (plist-get result :lsp_server_id)))
             ;; Store LSP server ID
             (setq-local lsp-proxy-bridge--lsp-server-id lsp-server-id)
             
             ;; Create and register LSP client
             (let ((client (lsp-proxy-bridge-create-client 
                           server-name lsp-server-id language-id workspace-root)))
               (lsp-register-client client))
             
             ;; Start lsp-mode
             (lsp-mode 1)
             (message "Started %s LSP server (ID: %d) for %s" 
                      language-id lsp-server-id server-name))
         (message "Failed to start LSP server: %s" (plist-get result :message)))))))

;;; Integration hooks

(defun lsp-proxy-bridge-enable ()
  "Enable LSP bridge integration."
  (add-hook 'find-file-hook #'lsp-proxy-bridge-auto-setup)
  (add-hook 'lsp-proxy-tramp-file-opened-hook #'lsp-proxy-bridge-auto-setup))

(defun lsp-proxy-bridge-disable ()
  "Disable LSP bridge integration."
  (remove-hook 'find-file-hook #'lsp-proxy-bridge-auto-setup)
  (remove-hook 'lsp-proxy-tramp-file-opened-hook #'lsp-proxy-bridge-auto-setup))

;;; Interactive commands

;;;###autoload
(defun lsp-proxy-bridge-restart ()
  "Restart LSP bridge for current buffer."
  (interactive)
  (when (and lsp-proxy-bridge--server-name 
             lsp-proxy-bridge--lsp-server-id)
    ;; Stop current LSP session
    (lsp-workspace-shutdown (lsp-find-workspace 'lsp-proxy))
    
    ;; Clear buffer-local variables
    (setq-local lsp-proxy-bridge--lsp-server-id nil)
    
    ;; Restart
    (lsp-proxy-bridge-setup-for-buffer)))

;;;###autoload
(defun lsp-proxy-bridge-status ()
  "Show LSP bridge status for current buffer."
  (interactive)
  (if lsp-proxy-bridge--server-name
      (message "LSP Bridge: Server=%s, LSP-ID=%s, Language=%s" 
               lsp-proxy-bridge--server-name
               (or lsp-proxy-bridge--lsp-server-id "none")
               (or lsp-proxy-bridge--language-id "unknown"))
    (message "LSP Bridge: Not active for current buffer")))

;;; Cleanup

(defun lsp-proxy-bridge-cleanup ()
  "Cleanup LSP bridge connections."
  (maphash (lambda (name connection)
             (ignore-errors
               (jsonrpc-shutdown connection))
             (remhash name lsp-proxy-bridge--active-connections))
           lsp-proxy-bridge--active-connections))

;; Auto-enable when loading
(lsp-proxy-bridge-enable)

;; Cleanup on exit
(add-hook 'kill-emacs-hook #'lsp-proxy-bridge-cleanup)

(provide 'lsp-proxy-lsp-bridge)

;;; lsp-proxy-lsp-bridge.el ends here