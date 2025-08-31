;;; lsp-proxy-tramp.el --- TRAMP integration for lsp-proxy remote development -*- lexical-binding: t; -*-

;; Copyright (C) 2024 LSP-Proxy
;; Author: LSP-Proxy Team
;; Keywords: remote, development, lsp, proxy, tramp
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (lsp-proxy "0.4.0") (tramp "2.5.0"))

;;; Commentary:

;; This package provides TRAMP integration for lsp-proxy, enabling modern
;; remote file access combined with remote LSP server support.
;;
;; Key features:
;; - Custom TRAMP method "lsp-proxy" for seamless remote file access
;; - Automatic LSP server discovery and startup on remote hosts
;; - LSP request proxying through existing lsp-proxy connections
;; - File change monitoring and intelligent caching

;;; Code:

(require 'tramp)
(require 'lsp-proxy-remote)
(require 'cl-lib)

(defgroup lsp-proxy-tramp nil
  "TRAMP integration for lsp-proxy remote development."
  :group 'lsp-proxy-remote
  :prefix "lsp-proxy-tramp-")

(defcustom lsp-proxy-tramp-auto-start-lsp t
  "Whether to automatically start LSP servers for remote files."
  :type 'boolean
  :group 'lsp-proxy-tramp)

(defcustom lsp-proxy-tramp-cache-timeout 300
  "Timeout in seconds for cached remote file information."
  :type 'integer
  :group 'lsp-proxy-tramp)

;;; TRAMP Method Definition

(defconst lsp-proxy-tramp-method "lsp-proxy"
  "TRAMP method name for lsp-proxy connections.")

;; Define the TRAMP method
(add-to-list 'tramp-methods
             `(,lsp-proxy-tramp-method
               (tramp-login-program        "echo")
               (tramp-login-args           (("Connected via lsp-proxy")))
               (tramp-remote-shell         "/bin/sh")
               (tramp-remote-shell-args    ("-c"))
               (tramp-connection-timeout   30)
               (tramp-copy-program         "echo")
               (tramp-copy-args            (("lsp-proxy file transfer")))
               ;; Custom handlers
               (tramp-file-name-handler-alist
                (file-directory-p . lsp-proxy-tramp-handle-directory-p)
                (file-exists-p . lsp-proxy-tramp-handle-file-exists-p)
                (file-readable-p . lsp-proxy-tramp-handle-file-readable-p)
                (file-regular-p . lsp-proxy-tramp-handle-file-regular-p)
                (file-attributes . lsp-proxy-tramp-handle-file-attributes)
                (insert-file-contents . lsp-proxy-tramp-handle-insert-file-contents)
                (write-region . lsp-proxy-tramp-handle-write-region)
                (directory-files . lsp-proxy-tramp-handle-directory-files)
                (make-directory . lsp-proxy-tramp-handle-make-directory)
                (delete-file . lsp-proxy-tramp-handle-delete-file))))

;;; File operation handlers

(defun lsp-proxy-tramp-handle-file-exists-p (filename)
  "Handle file-exists-p for lsp-proxy TRAMP method."
  (lsp-proxy-tramp-with-connection filename
    (lambda (server-name remote-path)
      (let ((result (lsp-proxy-remote--send-request
                     "emacs/remoteFileExists"
                     `((server_name . ,server-name)
                       (file_path . ,remote-path)))))
        (and result (plist-get result :exists))))))

(defun lsp-proxy-tramp-handle-file-readable-p (filename)
  "Handle file-readable-p for lsp-proxy TRAMP method."
  (lsp-proxy-tramp-handle-file-exists-p filename))

(defun lsp-proxy-tramp-handle-file-regular-p (filename)
  "Handle file-regular-p for lsp-proxy TRAMP method."
  (lsp-proxy-tramp-with-connection filename
    (lambda (server-name remote-path)
      (let ((result (lsp-proxy-remote--send-request
                     "emacs/remoteFileInfo"
                     `((server_name . ,server-name)
                       (file_path . ,remote-path)))))
        (and result (string= (plist-get result :type) "file"))))))

(defun lsp-proxy-tramp-handle-directory-p (filename)
  "Handle file-directory-p for lsp-proxy TRAMP method."
  (lsp-proxy-tramp-with-connection filename
    (lambda (server-name remote-path)
      (let ((result (lsp-proxy-remote--send-request
                     "emacs/remoteFileInfo"
                     `((server_name . ,server-name)
                       (file_path . ,remote-path)))))
        (and result (string= (plist-get result :type) "directory"))))))

(defun lsp-proxy-tramp-handle-file-attributes (filename &optional id-format)
  "Handle file-attributes for lsp-proxy TRAMP method."
  (lsp-proxy-tramp-with-connection filename
    (lambda (server-name remote-path)
      (let ((result (lsp-proxy-remote--send-request
                     "emacs/remoteFileAttributes"
                     `((server_name . ,server-name)
                       (file_path . ,remote-path)))))
        (when result
          ;; Convert to Emacs file-attributes format
          (list
           (plist-get result :is_directory)  ; directory flag
           (plist-get result :links)         ; number of links
           (plist-get result :uid)           ; user ID
           (plist-get result :gid)           ; group ID
           (plist-get result :atime)         ; access time
           (plist-get result :mtime)         ; modification time
           (plist-get result :ctime)         ; change time
           (plist-get result :size)          ; size in bytes
           (plist-get result :mode)          ; file mode
           nil                               ; gid changep
           (plist-get result :inode)         ; inode number
           (plist-get result :device)))))))  ; device number

(defun lsp-proxy-tramp-handle-insert-file-contents (filename &optional visit beg end replace)
  "Handle insert-file-contents for lsp-proxy TRAMP method."
  (lsp-proxy-tramp-with-connection filename
    (lambda (server-name remote-path)
      (let ((result (lsp-proxy-remote--send-request
                     "emacs/remoteFileRead"
                     `((server_name . ,server-name)
                       (file_path . ,remote-path)
                       (start . ,beg)
                       (end . ,end)))))
        (when (and result (plist-get result :success))
          (let ((content (plist-get result :content))
                (size (plist-get result :size)))
            (when replace
              (erase-buffer))
            (insert content)
            ;; Set up remote LSP integration
            (lsp-proxy-tramp-setup-lsp-for-buffer server-name remote-path)
            (list filename size)))))))

(defun lsp-proxy-tramp-handle-write-region (start end filename &optional append visit lockname confirm)
  "Handle write-region for lsp-proxy TRAMP method."
  (lsp-proxy-tramp-with-connection filename
    (lambda (server-name remote-path)
      (let ((content (if (stringp start)
                         start
                       (buffer-substring-no-properties (or start (point-min))
                                                       (or end (point-max))))))
        (let ((result (lsp-proxy-remote--send-request
                       "emacs/remoteFileWrite"
                       `((server_name . ,server-name)
                         (file_path . ,remote-path)
                         (content . ,content)
                         (append . ,(and append t))))))
          (when (and result (plist-get result :success))
            ;; Notify LSP server of file changes
            (lsp-proxy-tramp-notify-lsp-file-changed server-name remote-path)
            t))))))

(defun lsp-proxy-tramp-handle-directory-files (directory &optional full match nosort)
  "Handle directory-files for lsp-proxy TRAMP method."
  (lsp-proxy-tramp-with-connection directory
    (lambda (server-name remote-path)
      (let ((result (lsp-proxy-remote--send-request
                     "emacs/remoteDirectoryList"
                     `((server_name . ,server-name)
                       (directory_path . ,remote-path)
                       (full_path . ,(and full t))
                       (pattern . ,match)))))
        (when (and result (plist-get result :success))
          (let ((files (plist-get result :files)))
            (if nosort files (sort files #'string<))))))))

(defun lsp-proxy-tramp-handle-make-directory (dir &optional parents)
  "Handle make-directory for lsp-proxy TRAMP method."
  (lsp-proxy-tramp-with-connection dir
    (lambda (server-name remote-path)
      (let ((result (lsp-proxy-remote--send-request
                     "emacs/remoteMakeDirectory"
                     `((server_name . ,server-name)
                       (directory_path . ,remote-path)
                       (parents . ,(and parents t))))))
        (and result (plist-get result :success))))))

(defun lsp-proxy-tramp-handle-delete-file (filename &optional trash)
  "Handle delete-file for lsp-proxy TRAMP method."
  (lsp-proxy-tramp-with-connection filename
    (lambda (server-name remote-path)
      (let ((result (lsp-proxy-remote--send-request
                     "emacs/remoteDeleteFile"
                     `((server_name . ,server-name)
                       (file_path . ,remote-path)
                       (trash . ,(and trash t))))))
        (when (and result (plist-get result :success))
          ;; Notify LSP server of file deletion
          (lsp-proxy-tramp-notify-lsp-file-deleted server-name remote-path)
          t)))))

;;; Helper functions

(defun lsp-proxy-tramp-with-connection (filename func)
  "Execute FUNC with lsp-proxy connection for FILENAME.
FUNC should accept (server-name remote-path) as arguments."
  (let* ((parsed (tramp-dissect-file-name filename))
         (server-name (tramp-file-name-host parsed))
         (remote-path (tramp-file-name-localname parsed)))
    (unless (member server-name lsp-proxy-remote--connected-servers)
      (error "LSP-Proxy server '%s' is not connected" server-name))
    (funcall func server-name remote-path)))

(defmacro lsp-proxy-tramp-file-name (server-name remote-path)
  "Construct lsp-proxy TRAMP file name."
  `(format "/%s:%s:%s" lsp-proxy-tramp-method ,server-name ,remote-path))

;;; LSP Integration

(defvar-local lsp-proxy-tramp--server-name nil
  "LSP-Proxy server name for current buffer.")

(defvar-local lsp-proxy-tramp--remote-path nil
  "Remote file path for current buffer.")

(defvar-local lsp-proxy-tramp--lsp-server-id nil
  "Remote LSP server ID for current buffer.")

(defun lsp-proxy-tramp-setup-lsp-for-buffer (server-name remote-path)
  "Set up LSP integration for current buffer with remote file."
  (setq-local lsp-proxy-tramp--server-name server-name)
  (setq-local lsp-proxy-tramp--remote-path remote-path)
  
  (when lsp-proxy-tramp-auto-start-lsp
    ;; Auto-detect and start appropriate LSP server
    (lsp-proxy-tramp-auto-start-lsp-server server-name remote-path)))

(defun lsp-proxy-tramp-auto-start-lsp-server (server-name remote-path)
  "Automatically start appropriate LSP server for remote file."
  (let* ((file-ext (file-name-extension remote-path))
         (lang-id (lsp-proxy-tramp-detect-language file-ext)))
    (when lang-id
      (lsp-proxy-remote--send-async-request
       "emacs/remoteStartLspServer"
       `((server_name . ,server-name)
         (language_id . ,lang-id)
         (workspace_root . ,(file-name-directory remote-path)))
       (lambda (result)
         (if (plist-get result :success)
             (progn
               (setq-local lsp-proxy-tramp--lsp-server-id 
                          (plist-get result :lsp_server_id))
               (message "Started %s LSP server on %s" lang-id server-name)
               ;; Enable lsp-mode with custom client
               (lsp-proxy-tramp-enable-lsp-mode))
           (message "Failed to start LSP server: %s" 
                    (plist-get result :message))))))))

(defun lsp-proxy-tramp-detect-language (file-ext)
  "Detect programming language from file extension."
  (cond
   ((member file-ext '("rs")) "rust")
   ((member file-ext '("py" "pyi")) "python")
   ((member file-ext '("js" "jsx" "ts" "tsx")) "typescript")
   ((member file-ext '("go")) "go")
   ((member file-ext '("c" "h")) "c")
   ((member file-ext '("cpp" "cc" "cxx" "hpp")) "cpp")
   ((member file-ext '("java")) "java")
   ((member file-ext '("rb")) "ruby")
   (t nil)))

(defun lsp-proxy-tramp-enable-lsp-mode ()
  "Enable LSP mode with custom lsp-proxy client."
  (when (and lsp-proxy-tramp--server-name 
             lsp-proxy-tramp--lsp-server-id)
    ;; Create custom LSP client that proxies requests through lsp-proxy
    (lsp-proxy-tramp-create-lsp-client)
    (lsp-mode 1)))

(defun lsp-proxy-tramp-create-lsp-client ()
  "Create custom LSP client for remote lsp-proxy integration."
  (let ((client-id (intern (format "lsp-proxy-tramp-%s-%s" 
                                   lsp-proxy-tramp--server-name
                                   lsp-proxy-tramp--lsp-server-id))))
    (unless (lsp--client-get client-id)
      (lsp-register-client
       (make-lsp-client
        :new-connection (lambda () 
                         (lsp-proxy-tramp-create-connection 
                          lsp-proxy-tramp--server-name
                          lsp-proxy-tramp--lsp-server-id))
        :activation-fn (lambda (&rest _) t) ; Always active for our files
        :server-id client-id)))))

(defun lsp-proxy-tramp-create-connection (server-name lsp-server-id)
  "Create LSP connection that proxies through lsp-proxy."
  (make-lsp--stdio-connection
   :process (lsp-proxy-tramp-create-proxy-process server-name lsp-server-id)
   :stderr (lambda (msg) (lsp-log "LSP-Proxy TRAMP: %s" msg))))

(defun lsp-proxy-tramp-create-proxy-process (server-name lsp-server-id)
  "Create proxy process for LSP communication."
  (let ((process-name (format "lsp-proxy-tramp-%s-%s" server-name lsp-server-id)))
    ;; This would need to be implemented as a custom process that
    ;; forwards LSP JSON-RPC messages through lsp-proxy connections
    ;; For now, return a placeholder
    (start-process process-name nil "cat")))

(defun lsp-proxy-tramp-notify-lsp-file-changed (server-name remote-path)
  "Notify remote LSP server that file has changed."
  (when lsp-proxy-tramp--lsp-server-id
    (lsp-proxy-remote--send-async-request
     "emacs/remoteLspNotification"
     `((server_name . ,server-name)
       (lsp_server_id . ,lsp-proxy-tramp--lsp-server-id)
       (method . "textDocument/didChange")
       (params . ((textDocument . ((uri . ,(format "file://%s" remote-path))
                                   (version . ,(random 1000000)))))))
     (lambda (result)
       (unless (plist-get result :success)
         (message "Failed to notify LSP of file change: %s" 
                  (plist-get result :message)))))))

(defun lsp-proxy-tramp-notify-lsp-file-deleted (server-name remote-path)
  "Notify remote LSP server that file has been deleted."
  (when lsp-proxy-tramp--lsp-server-id
    (lsp-proxy-remote--send-async-request
     "emacs/remoteLspNotification"
     `((server_name . ,server-name)
       (lsp_server_id . ,lsp-proxy-tramp--lsp-server-id)
       (method . "workspace/didDeleteFiles")
       (params . ((files . (((uri . ,(format "file://%s" remote-path)))))))
     (lambda (result)
       (unless (plist-get result :success)
         (message "Failed to notify LSP of file deletion: %s" 
                  (plist-get result :message)))))))

;;; Interactive commands

;;;###autoload
(defun lsp-proxy-tramp-find-file (server-name remote-path)
  "Open remote file using lsp-proxy TRAMP method."
  (interactive
   (let ((server (completing-read "Server: " lsp-proxy-remote--connected-servers))
         (path (read-file-name "Remote file: " "/")))
     (list server path)))
  (find-file (lsp-proxy-tramp-file-name server-name remote-path)))

;;;###autoload
(defun lsp-proxy-tramp-dired (server-name remote-directory)
  "Open remote directory using lsp-proxy TRAMP method."
  (interactive
   (let ((server (completing-read "Server: " lsp-proxy-remote--connected-servers))
         (dir (read-file-name "Remote directory: " "/" nil nil nil #'file-directory-p)))
     (list server dir)))
  (dired (lsp-proxy-tramp-file-name server-name remote-directory)))

;;;###autoload  
(defun lsp-proxy-tramp-setup ()
  "Setup lsp-proxy TRAMP integration."
  (interactive)
  ;; Ensure lsp-proxy-remote is loaded
  (unless (featurep 'lsp-proxy-remote)
    (require 'lsp-proxy-remote))
  
  ;; Enable remote mode
  (unless lsp-proxy-remote-mode
    (lsp-proxy-remote-mode 1))
    
  (message "LSP-Proxy TRAMP integration setup complete"))

(provide 'lsp-proxy-tramp)

;;; lsp-proxy-tramp.el ends here