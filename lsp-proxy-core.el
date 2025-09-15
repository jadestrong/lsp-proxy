;;; lsp-proxy-core.el --- Core connection and communication for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core connection management and LSP communication for lsp-proxy.
;; Supports both direct jsonrpc connection and Eglot-based connection modes.

;;; Code:

(require 'cl-lib)
(require 'jsonrpc)
(require 'dash)
(require 'lsp-proxy-utils)

(defcustom lsp-proxy-log-file-directory temporary-file-directory
  "The directory for `lsp-proxy' server to generate log file."
  :type 'string
  :group 'lsp-proxy)

(defcustom lsp-proxy-user-languages-config (expand-file-name (concat user-emacs-directory (file-name-as-directory "lsp-proxy") "languages.toml"))
  "The user config file to store custom language config."
  :type 'string
  :group 'lsp-proxy)

(defcustom lsp-proxy-log-max 0
  "Max size of events buffer. 0 disables, nil means infinite.
Enabling event logging may slightly affect performance."
  :group 'lsp-proxy
  :type 'integer)

(defcustom lsp-proxy-log-level 0
  "Log level for lsp-proxy.
0: No logging
1: Basic logging
2: Verbose logging"
  :group 'lsp-proxy
  :type 'integer)

(defcustom lsp-proxy--send-changes-idle-time 0
  "Don't tell server of changes before Emacs's been idle for this many seconds."
  :group 'lsp-proxy
  :type 'number)

(defvar-local lsp-proxy--support-inlay-hints nil
  "Is there any server associated with this buffer
 that support `textDocument/inlayHint' request.")

(defvar-local lsp-proxy--support-document-highlight nil
  "Is there any server associated with this buffer
that support `textDocument/documentHighlight' request.")

(defvar-local lsp-proxy--support-document-symbols nil
  "Is there any server associated with this buffer
that support `textDocument/documentSymbols' request.")

(defvar-local lsp-proxy--support-signature-help nil
  "Is there any server associated with this buffer
that support `textDocument/signatureHelp' request.")

(defvar-local lsp-proxy--support-pull-diagnostic nil
  "Is there any server associated with this buffer
that support `textDocument/diagnostic' request.")


;;; Connection management variables

(defvar lsp-proxy--connection nil
  "The jsonrpc connection to lsp-proxy server.")

(defvar lsp-proxy--exec-file nil
  "Path to the lsp-proxy executable.")

(defvar lsp-proxy--log-file nil
  "Path to the current log file.")

(defvar lsp-proxy--opened-buffers nil
  "List of buffers opened with lsp-proxy.")

(defvar lsp-proxy--base-dir (if load-file-name
                                (file-name-directory load-file-name)
                              default-directory)
  "Base directory for lsp-proxy.")

;;; External variables (to be defined by main module)
(defvar lsp-proxy-max-completion-item)
(defvar lsp-proxy-mode)

;;; External functions from eglot (for backward compatibility)
(declare-function eglot--TextDocumentIdentifier "ext:eglot")
(declare-function eglot--VersionedTextDocumentIdentifier "ext:eglot")
(declare-function eglot--widening "ext:eglot")
(declare-function eglot--apply-workspace-edit "ext:eglot")

;;; External variables from eglot
(defvar eglot--versioned-identifier)

;;; External functions (to be defined by other modules)
(declare-function lsp-proxy-diagnostics--request-pull-diagnostics "lsp-proxy-diagnostics")
(declare-function lsp-proxy-diagnostics--handle-publish-diagnostics "lsp-proxy-diagnostics")
(declare-function lsp-proxy-activate-inlay-hints-mode "lsp-proxy-inlay-hints")
(declare-function lsp-proxy-inline-completion-mode "lsp-proxy")
(declare-function lsp-proxy--set-work-done-token "lsp-proxy")
(declare-function lsp-proxy--rem-work-done-token "lsp-proxy")
(declare-function lsp-proxy--cleanup "lsp-proxy")
(declare-function lsp-proxy--progress-status "lsp-proxy")
(declare-function lsp-proxy--async-load-large-file "lsp-proxy-large-file")

;;; Connection utilities

(defun lsp-proxy--connection-alivep ()
  "Check if connection is alive."
  (and lsp-proxy--connection
       (jsonrpc-running-p lsp-proxy--connection)))

(defun lsp-proxy--ensure-connection ()
  "Ensure connection is established."
  (unless (lsp-proxy--connection-alivep)
    (lsp-proxy--start-server)))

(defun lsp-proxy-server-executable ()
  "Find emacs-lsp-proxy executable with priority order:
1. System PATH (emacs-lsp-proxy)
2. Current directory (./emacs-lsp-proxy)
3. target/release directory (./target/release/emacs-lsp-proxy)"
  (let* ((base-dir lsp-proxy--base-dir)
         (exe-name (if (eq system-type 'windows-nt)
                       "emacs-lsp-proxy.exe"
                     "emacs-lsp-proxy"))
         (candidates (list (executable-find exe-name)
                           (expand-file-name exe-name base-dir)
                           (expand-file-name (concat "target/release/" exe-name) base-dir))))
    (or (seq-find #'file-exists-p (delq nil candidates))
        (error "No emacs-lsp-proxy executable found in any location"))))

;;; Communication macros

(defconst lsp-proxy--show-error
  (lambda (err)
    (lsp-proxy--error "%s" (or (and err (plist-get err :message)) err)))
  "Default handler for error message.")

(defconst lsp-proxy--show-timeout
  (lambda (method)
    (lsp-proxy--error "%s(%s)" "Request timeout" method))
  "Default handler for timeout.")

(defconst lsp-proxy--ignore-response
  (lambda (_))
  "Simply ignore the response.")

(cl-defmacro lsp-proxy--notify (method &rest params)
  "Send a notification (METHOD PARAMS) to the lsp proxy agent with ARGS."
  `(progn
     (lsp-proxy--ensure-connection)
     (if (or (eq ,method 'textDocument/didOpen)
             (eq ,method 'textDocument/willSave)
             (eq ,method 'textDocument/didSave)
             (-contains-p lsp-proxy--opened-buffers (current-buffer)))
         (let ((new-params (append (eglot--TextDocumentIdentifier) (list :params ,@params))))
           (jsonrpc-notify lsp-proxy--connection ,method new-params))
       (lsp-proxy--on-doc-open))))

(cl-defmacro lsp-proxy--async-request (method params &rest args &key (success-fn #'lsp-proxy--ignore-response) (error-fn #'lsp-proxy--show-error) (timeout-fn #'lsp-proxy--show-timeout) &allow-other-keys)
  "Send an asynchronous request (METHOD PARAMS ARGS) to the lsp proxy agent."
  `(progn
     (lsp-proxy--ensure-connection)
     (if (not (eq ,method 'textDocument/diagnostic))
         (lsp-proxy--send-did-change))
     (unless (-contains-p lsp-proxy--opened-buffers (current-buffer))
       (lsp-proxy--on-doc-open))
     ;; jsonrpc will use temp buffer for callbacks, so we need to save the current buffer
     (let ((buf (current-buffer)))
       (jsonrpc-async-request lsp-proxy--connection
                              ,method ,params
                              :success-fn (lambda (result)
                                            (with-current-buffer buf
                                              (funcall ,success-fn result)))
                              :error-fn (lambda (err)
                                          (funcall ,error-fn err))
                              :timeout-fn (lambda ()
                                            (with-current-buffer buf
                                              (funcall ,timeout-fn ,method)))
                              ,@args))))


(cl-defmacro lsp-proxy--request (&rest args)
  "Send a request to the lsp proxy agent with ARGS."
  `(progn
     (when lsp-proxy-mode
       (lsp-proxy--ensure-connection)
       (lsp-proxy--send-did-change)
       (unless (-contains-p lsp-proxy--opened-buffers (current-buffer))
         (lsp-proxy--on-doc-open))
       (jsonrpc-request lsp-proxy--connection ,@args))))

;;; Connection

(defun lsp-proxy--make-connection ()
  "Establish proxy jsonrpc connection."
  (let ((make-fn (apply-partially
                  #'make-instance
                  'jsonrpc-process-connection
                  :name "lsp proxy"
                  :notification-dispatcher #'lsp-proxy--handle-notification
                  :request-dispatcher #'lsp-proxy--handle-request
                  :process (make-process :name "lsp proxy agent"
                                         :command (list lsp-proxy--exec-file "--stdio" "--config" lsp-proxy-user-languages-config "--log-level" (number-to-string lsp-proxy-log-level) "--log" lsp-proxy--log-file "--max-item" (number-to-string lsp-proxy-max-completion-item))
                                         :coding 'utf-8-emacs-unix
                                         :connection-type 'pipe
                                         :stderr (get-buffer-create "*lsp proxy stderr*")
                                         :noquery t))))
    (condition-case nil
        (funcall make-fn :events-buffer-config `(:size ,lsp-proxy-log-max))
      (invalid-slot-name
       ;; handle older jsonrpc versions
       (funcall make-fn :events-buffer-scrollback-size lsp-proxy-log-max)))))

(defun lsp-proxy--start-server ()
  "Start the lsp proxy agent process in local."
  (let* ((timestamp (format-time-string "%Y%m%d%H%M%S"))
         (random-num (random 100000))
         (filename (format "lsp-proxy-%s-%05d.log" timestamp random-num)))
    (setq lsp-proxy--log-file (concat lsp-proxy-log-file-directory filename))
    (setq lsp-proxy--exec-file (lsp-proxy-server-executable))
    (if lsp-proxy--exec-file
        (progn
          (setq lsp-proxy--connection (lsp-proxy--make-connection))
          (message "Lsp proxy server started using: %s" lsp-proxy--exec-file))
      (lsp-proxy--error "No emacs-lsp-proxy executable found. Please ensure it's installed in PATH, current directory, or target/release directory"))))

;;; Message handling

(defun lsp-proxy--handle-notification (_ method msg)
  "Handle MSG of type METHOD."
  (when (eql method 'textDocument/publishDiagnostics)
    (lsp-proxy-diagnostics--handle-publish-diagnostics msg))
  (when  (eql method 'window/logMessage)
    (lsp-proxy--dbind (:type type :message message) msg
      (lsp-proxy-log "%s" (lsp-proxy--propertize message type))))
  (when  (eql method 'window/showMessage)
    (lsp-proxy--dbind (:type type :message message) msg
      (lsp-proxy--info "%s" (lsp-proxy--propertize message type))))
  (when (eql method 'emacs/serverCapabilities)
    (lsp-proxy--dbind (:uri uri
                       :triggerCharacters trigger-characters
                       :supportInlayHints support-inlay-hints
                       :supportDocumentHighlight support-document-highlight
                       :supportDocumentSymbols support-document-symbols
                       :supportSignatureHelp support-signature-help
                       :supportPullDiagnostic support-pull-diagnostic
                       :supportInlineCompletion support-inline-completion)
        msg
      (let* ((filepath (lsp-proxy--uri-to-path uri)))
        (when (file-exists-p filepath)
          (with-current-buffer (find-file-noselect filepath)
            (setq-local lsp-proxy--completion-trigger-characters trigger-characters)
            (setq-local lsp-proxy--support-inlay-hints (not (eq support-inlay-hints :json-false)))
            (setq-local lsp-proxy--support-document-highlight (not (eq support-document-highlight :json-false)))
            (setq-local lsp-proxy--support-document-symbols (not (eq support-document-symbols :json-false)))
            (setq-local lsp-proxy--support-signature-help (not (eq support-signature-help :json-false)))
            (setq-local lsp-proxy--support-pull-diagnostic (not (eq support-pull-diagnostic :json-false)))
            (lsp-proxy-activate-inlay-hints-mode)
            (lsp-proxy-diagnostics--request-pull-diagnostics)
            (if (not (eq support-inline-completion :json-false))
                (lsp-proxy-inline-completion-mode)))))))
  (when (eql method '$/progress)
    (add-to-list 'global-mode-string '(t (:eval (lsp-proxy--progress-status))))
    (lsp-proxy--dbind (:rootPath root-path :params params) msg
      (let* ((token (plist-get params :token))
             (value (plist-get params :value))
             (kind (plist-get value :kind)))
        (pcase kind
          ("begin" (lsp-proxy--set-work-done-token (lsp-proxy--normalize-path root-path) token value))
          ("report" (lsp-proxy--set-work-done-token (lsp-proxy--normalize-path root-path) token value))
          ("end" (lsp-proxy--rem-work-done-token (lsp-proxy--normalize-path root-path) token)))))))

(defun lsp-proxy--handle-request (_ method msg)
  "Handle MSG of type METHOD."
  (when (eql method 'workspace/applyEdit)
    (lsp-proxy--dbind (:edit edit) msg
      (eglot--apply-workspace-edit edit last-command)))
  (when (eql method 'eslint/openDoc)
    (lsp-proxy--dbind (:url url) msg
      (browse-url url))))

;;; Change tracking

(defvar-local lsp-proxy--recent-changes nil)
(defvar-local lsp-proxy--change-idle-timer nil)

(defun lsp-proxy--before-change (beg end)
  "Hook onto `before-change-functions' with BEG and END."
  (when (listp lsp-proxy--recent-changes)
    (push `(,(eglot--pos-to-lsp-position beg)
            ,(eglot--pos-to-lsp-position end)
            (,beg . ,(copy-marker beg nil))
            (,end . ,(copy-marker end t)))
          lsp-proxy--recent-changes)))

(defun lsp-proxy--after-change (beg end pre-change-length)
  "Hook onto `after-change-functions'.
Records BEG, END and PRE-CHANGE-LENGTH locally."
  (cl-incf eglot--versioned-identifier)
  (pcase (and (listp lsp-proxy--recent-changes)
              (car lsp-proxy--recent-changes))
    (`(,lsp-beg ,lsp-end
       (,b-beg . ,b-beg-marker)
       (,b-end . ,b-end-marker))
     (if (and (= b-end b-end-marker) (= b-beg b-beg-marker)
              (or (/= beg b-beg) (/= end b-end)))
         (setcar lsp-proxy--recent-changes
                 `(,lsp-beg ,lsp-end ,(- b-end-marker b-beg-marker)
                   ,(buffer-substring-no-properties b-beg-marker b-end-marker)))
       (setcar lsp-proxy--recent-changes
               `(,lsp-beg ,lsp-end ,pre-change-length
                 ,(buffer-substring-no-properties beg end)))))
    (_ (setf lsp-proxy--recent-changes :emacs-messup)))
  (when lsp-proxy--change-idle-timer (cancel-timer lsp-proxy--change-idle-timer))
  (let ((buf (current-buffer)))
    (setq lsp-proxy--change-idle-timer
          (run-with-idle-timer
           lsp-proxy--send-changes-idle-time
           nil (lambda () (eglot--when-live-buffer buf
                            (when lsp-proxy-mode
                              (lsp-proxy--send-did-change)
                              (setq lsp-proxy--change-idle-timer nil))))))))

;;; Document lifecycle

(defun lsp-proxy--on-doc-focus (window)
  "Notify that the document WINDOW has been focussed or opened."
  ;; When switching windows, this function is called twice, once for the
  ;; window losing and once for the window gaining focus. We only want to
  ;; send a notification for the window gaining focus and only if the buffer has
  ;; lsp-proxy-mode enabled.
  (when (and lsp-proxy-mode (eq window (selected-window)))
    (if (-contains-p lsp-proxy--opened-buffers (current-buffer))
        (lsp-proxy--notify ':textDocument/didFocus
                           (list :textDocument (eglot--TextDocumentIdentifier)))
      (lsp-proxy--on-doc-open))))

(defun lsp-proxy--get-initial-content ()
  "Get initial content to send for large files."
  (let ((file-name (file-name-nondirectory buffer-file-name))
        (file-size (nth 7 (file-attributes buffer-file-name))))
    (format ";; Large file: %s\n;; Size: %s\n;; Loading content asynchronously...\n\n"
            file-name
            (lsp-proxy--format-file-size file-size))))

(defun lsp-proxy--on-doc-open ()
  "On doc open."
  (setq lsp-proxy--recent-changes nil
        eglot--versioned-identifier 0)
  (when buffer-file-name
    (when (not (file-exists-p buffer-file-name))
      (save-buffer))
    (let* ((initial-content (if (and (boundp 'lsp-proxy--is-large-file) lsp-proxy--is-large-file)
                                (lsp-proxy--get-initial-content)
                              (eglot--widening
                               (buffer-substring-no-properties (point-min) (point-max))))))
      (add-to-list 'lsp-proxy--opened-buffers (current-buffer))
      (setq-local lsp-proxy--support-document-highlight (< (line-number-at-pos (point-max)) 10000))
      (lsp-proxy--notify 'textDocument/didOpen
                         (list :textDocument (append (eglot--TextDocumentIdentifier)
                                                     (list
                                                      :text initial-content
                                                      :languageId ""
                                                      :version (if (and (boundp 'lsp-proxy--is-large-file) lsp-proxy--is-large-file) -1 eglot--versioned-identifier)
                                                      :isLargeFile (and (boundp 'lsp-proxy--is-large-file) lsp-proxy--is-large-file)))))
      ;; send large file content
      (when (and (boundp 'lsp-proxy--is-large-file) lsp-proxy--is-large-file)
        (require 'lsp-proxy-large-file)
        (lsp-proxy--async-load-large-file (current-buffer))))))

(defun lsp-proxy--on-doc-close (&rest _args)
  "Notify that the document has been closed."
  (when (-contains-p lsp-proxy--opened-buffers (current-buffer))
    (lsp-proxy--notify 'textDocument/didClose
                       (list :textDocument (eglot--TextDocumentIdentifier)))
    (setq lsp-proxy--opened-buffers (delete (current-buffer) lsp-proxy--opened-buffers))))

(defun lsp-proxy--will-save ()
  "Send textDocument/willSave notification."
  (lsp-proxy--notify 'textDocument/willSave
                     ;; 1 Manual, 2 AfterDelay, 3 FocusOut
                     (list :textDocument (eglot--TextDocumentIdentifier) :reason 1)))

(defun lsp-proxy--did-save ()
  "Send textDocument/didSave notification."
  (lsp-proxy--notify 'textDocument/didSave
                     (list :textDocument (eglot--TextDocumentIdentifier))))

(defun lsp-proxy--send-did-change ()
  "Send textDocument/didChange to server."
  (when lsp-proxy--recent-changes
    (let ((full-sync-p (eq :emacs-messup lsp-proxy--recent-changes)))
      (lsp-proxy--notify 'textDocument/didChange
                         (list :textDocument
                               (eglot--VersionedTextDocumentIdentifier)
                               :contentChanges
                               (if full-sync-p
                                   (vector (list :text (eglot--widening
                                                        (buffer-substring-no-properties (point-min)
                                                                                        (point-max)))))
                                 (cl-loop for (beg end len text) in (reverse lsp-proxy--recent-changes)
                                          when (numberp len)
                                          vconcat `[,(list :range `(:start ,beg :end ,end)
                                                           :rangeLength len :text text)]))))
      (lsp-proxy-diagnostics--request-pull-diagnostics)
      (setq lsp-proxy--recent-changes nil))))

;;; Commands
(defun lsp-proxy-restart ()
  "Restart."
  (interactive)
  (unwind-protect
      (progn
        (lsp-proxy--request 'shutdown (lsp-proxy--request-or-notify-params nil) :timeout 1.5)
        (jsonrpc-notify lsp-proxy--connection 'exit (lsp-proxy--request-or-notify-params nil)))
    (jsonrpc-shutdown lsp-proxy--connection)
    (setq lsp-proxy--connection nil))
  (lsp-proxy--cleanup)
  (lsp-proxy--on-doc-focus (selected-window))
  (message "[LSP-PROXY] Process restarted."))

(defun lsp-proxy-open-log-file ()
  "Open the log file. If it does not exist, create it first."
  (interactive)
  (unless (file-exists-p lsp-proxy--log-file)
    (with-temp-buffer lsp-proxy--log-file))
  (find-file lsp-proxy--log-file))

(defun lsp-proxy-open-config-file ()
  "Open the configuration file. If it does not exist, create it first."
  (interactive)
  (unless (file-exists-p lsp-proxy-user-languages-config)
    (with-temp-buffer lsp-proxy-user-languages-config))
  (find-file lsp-proxy-user-languages-config))

(defun lsp-proxy-toggle-trace-io ()
  "Toggle jsonrpc logging."
  (interactive)
  (setq lsp-proxy-log-max (if lsp-proxy-log-max nil 0))
  (lsp-proxy-restart)
  (lsp-proxy--info "JSON-RPC logging %s." (if lsp-proxy-log-max "disabled" "enabled")))

(provide 'lsp-proxy-core)
;;; lsp-proxy-core.el ends here
