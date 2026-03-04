;;; lsp-proxy-copilot.el --- Copilot integration for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file provides integration with GitHub Copilot language server
;; through lsp-proxy's generic request forwarding mechanism.

;;; Commentary:

;; This module provides functions to sign in and sign out of GitHub Copilot
;; through the lsp-proxy server, as well as support for Next Edit Suggestions
;; (inline edits that may include deletions or modifications to existing text).

;;; Code:

(require 'lsp-proxy-core)
(require 'cl-lib)
(require 'eglot)

(defgroup lsp-proxy-copilot nil
  "Copilot integration for lsp-proxy."
  :group 'lsp-proxy
  :prefix "lsp-proxy-copilot-")

(defcustom lsp-proxy-copilot-server-name "copilot"
  "The copilot server name configed at custom config file."
  :type 'string
  :group 'lsp-proxy-copilot)

(defcustom lsp-proxy-copilot-next-edit-auto-discard-threshold 3
  "Number of commands before automatically discarding next edit suggestions.
After this many non-copilot commands, the suggestion will be automatically dismissed."
  :type 'integer
  :group 'lsp-proxy-copilot)

;;; Faces

(defface lsp-proxy-next-edit-face
  '((t :inherit shadow :slant italic))
  "Face for displaying next edit suggestions."
  :group 'lsp-proxy-copilot)

;;; Forward request helper with async support

(defun lsp-proxy-copilot--forward-request (method params &rest args)
  "Forward a request with METHOD and PARAMS to the Copilot language server.
Supports both synchronous and asynchronous requests via ARGS.
For async requests, pass :success-fn, :error-fn, and :timeout-fn.
Returns response for sync requests, nil for async."
  (unless (and (boundp 'lsp-proxy-mode) lsp-proxy-mode)
    (user-error "lsp-proxy-mode is not enabled in this buffer"))
  (unless buffer-file-name
    (user-error "Buffer must be visiting a file to communicate with Copilot"))

  (let* ((uri-plist (eglot--TextDocumentIdentifier))
         (uri (plist-get uri-plist :uri))
         (request-params (lsp-proxy--build-params
                         (list :uri uri
                               :serverName lsp-proxy-copilot-server-name
                               :method method
                               :params (or params (make-hash-table))))))
    (if (plist-member args :success-fn)
        ;; Async request
        (lsp-proxy--async-request
         'emacs/forwardRequest
         request-params
         :success-fn (plist-get args :success-fn)
         :error-fn (or (plist-get args :error-fn) #'lsp-proxy--show-error)
         :timeout-fn (or (plist-get args :timeout-fn) #'lsp-proxy--show-timeout))
      ;; Sync request
      (lsp-proxy--request
       'emacs/forwardRequest
       request-params))))

;;;###autoload
(defun lsp-proxy-copilot-sign-in ()
  "Sign in to GitHub Copilot.
This will display a user code and open a browser for authentication."
  (interactive)
  (condition-case err
      (let ((response (lsp-proxy-copilot--forward-request "signIn" nil)))
        (when response
          (let ((user-code (plist-get response :userCode))
                (command (plist-get response :command))
                (verification-uri (plist-get response :verificationUri)))
            (cond ((yes-or-no-p "Copilot requires you to log into your GitHub account. Proceed now?")
                   (if (display-graphic-p)
                       (progn
                         (gui-set-selection 'CLIPBOARD user-code)
                         (read-from-minibuffer (format "Your one-time code %s is copied. Press \
to open Github in your browser. If your browser does not open \
tically, browse to %s." user-code verification-uri))
                         (browse-url verification-uri)
                         (read-from-minibuffer "Press ENTER if you finish authorizing."))
                     ;; Console:
                     (read-from-minibuffer (format "First copy your one-time code: %s. Press ENTER to continue." user-code))
                     (read-from-minibuffer (format "Please open %s in your browses. Press ENTER if you finish authorizing." verification-uri)))
                   (lsp-proxy--info "Verifying...")
                   (let ((cmd-name (plist-get command :command)))
                     (when (string= cmd-name "github.copilot.finishDeviceFlow")
                       (lsp-proxy-copilot--forward-request
                        "workspace/executeCommand"
                        (list :command "github.copilot.finishDeviceFlow"
                              :arguments [])))))
                  (t
                   (message "Aborting Copilot login."))))))
    (error
     (message "Copilot sign-in failed: %s" (error-message-string err)))))

;;;###autoload
(defun lsp-proxy-copilot-sign-out ()
  "Sign out from GitHub Copilot."
  (interactive)
  (condition-case err
      (progn
        (lsp-proxy-copilot--forward-request "signOut" nil)
        (message "Copilot: Signed out successfully"))
    (error
     (message "Copilot sign-out failed: %s" (error-message-string err)))))

;;;###autoload
(defun lsp-proxy-copilot-status ()
  "Get the current Copilot status.
This queries the Copilot server for authentication and connection status."
  (interactive)
  (condition-case err
      (let ((response (lsp-proxy-copilot--forward-request "checkStatus" nil)))
        (if response
            (message "Copilot status: %S" response)
          (message "Copilot: No status information available")))
    (error
     (message "Failed to get Copilot status: %s" (error-message-string err)))))

;;; Next Edit Suggestions Support

;; Session management structure
(cl-defstruct lsp-proxy-next-edit-session
  "Structure to manage a next edit suggestion session."
  overlays     ; List of overlays displaying the suggestions
  edits        ; List of edit plists with :start-point :end-point :new-text
  command      ; LSP command to execute on accept
  mouse-count  ; Counter for tracking user movements/commands
  buffer)      ; Buffer where the session is active

(defvar-local lsp-proxy--next-edit-session nil
  "Current next edit suggestion session, if any.")

;;; Helper functions

(defun lsp-proxy--clear-next-edit-session ()
  "Clear the current next edit suggestion session and all overlays."
  (when-let ((session lsp-proxy--next-edit-session))
    (dolist (ov (lsp-proxy-next-edit-session-overlays session))
      (when (overlayp ov)
        (delete-overlay ov)))
    (setq lsp-proxy--next-edit-session nil)
    (lsp-proxy-next-edit-mode -1)
    (remove-hook 'post-command-hook #'lsp-proxy--next-edit-post-command t)))

(defun lsp-proxy--convert-lsp-range-to-edit (edit)
  "Convert LSP EDIT with range to plist with buffer positions.
Returns plist with :start-point, :end-point, and :new-text."
  (let* ((text (plist-get edit :text))
         (range (plist-get edit :range))
         (start-pos (plist-get range :start))
         (end-pos (plist-get range :end))
         (start-point (eglot--lsp-position-to-point start-pos))
         (end-point (eglot--lsp-position-to-point end-pos)))
    (list :start-point start-point
          :end-point end-point
          :new-text text)))

(defun lsp-proxy--render-next-edits (edits command)
  "Render next edit suggestions as ghost overlays.
EDITS is a list of edit plists with :start-point, :end-point, and :new-text.
COMMAND is the LSP command to execute when accepting."
  (lsp-proxy--clear-next-edit-session)

  (let ((ovs '()))
    (dolist (edit edits)
      (let* ((start (plist-get edit :start-point))
             (end (plist-get edit :end-point))
             (text (plist-get edit :new-text))
             (ov (make-overlay start end nil nil t)))

        ;; Render based on whether it's an insert or replace
        (if (= start end)
            ;; Insert: use after-string
            (overlay-put ov 'after-string
                         (propertize text 'face 'lsp-proxy-next-edit-face))
          ;; Replace: use display property
          (overlay-put ov 'display
                       (propertize text 'face 'lsp-proxy-next-edit-face)))

        ;; Set high priority and modification hooks
        (overlay-put ov 'priority 999)
        (overlay-put ov 'modification-hooks
                     (list (lambda (&rest _)
                             (lsp-proxy-next-edit-discard))))

        (push ov ovs)))

    ;; Create session
    (setq lsp-proxy--next-edit-session
          (make-lsp-proxy-next-edit-session
           :overlays (nreverse ovs)
           :edits edits
           :command command
           :mouse-count 0
           :buffer (current-buffer)))

    ;; Activate minor mode and hooks
    (lsp-proxy-next-edit-mode 1)
    (add-hook 'post-command-hook #'lsp-proxy--next-edit-post-command nil t)))

;;; Interactive commands

;;;###autoload
(defun lsp-proxy-copilot-next-edit ()
  "Request next edit suggestions from Copilot at point."
  (interactive)
  (unless (and (boundp 'lsp-proxy-mode) lsp-proxy-mode)
    (user-error "lsp-proxy-mode is not enabled in this buffer"))
  (unless buffer-file-name
    (user-error "Buffer must be visiting a file"))

  (condition-case err
      (let* ((uri-plist (eglot--TextDocumentIdentifier))
             (uri (plist-get uri-plist :uri))
             (pos-params (eglot--TextDocumentPositionParams))
             (position (plist-get pos-params :position))
             (version (lsp-proxy--doc-version))
             (params (list :textDocument (list :uri uri :version version)
                          :position position)))
        (lsp-proxy-copilot--forward-request
         "textDocument/copilotInlineEdit"
         params
         :success-fn #'lsp-proxy--handle-next-edit-response
         :error-fn (lambda (err)
                     (message "Failed to get next edit suggestions: %s"
                              (or (plist-get err :message) err)))
         :timeout-fn (lambda ()
                       (message "Next edit request timed out"))))
    (error
     (message "Failed to request next edit: %s" (error-message-string err)))))

(defun lsp-proxy--handle-next-edit-response (response)
  "Handle the response from textDocument/copilotInlineEdit request.
RESPONSE should contain an :edits array."
  (when response
    (let ((edits-array (plist-get response :edits)))
      (if (and edits-array (> (length edits-array) 0))
          (let* ((edits (mapcar
                        (lambda (edit)
                          (lsp-proxy--convert-lsp-range-to-edit edit))
                        (append edits-array nil)))
                 ;; Sort edits by start position (descending) for reverse application
                 (sorted-edits (sort edits
                                    (lambda (a b)
                                      (> (plist-get a :start-point)
                                         (plist-get b :start-point)))))
                 ;; Extract command from first edit (if present)
                 (first-edit (aref edits-array 0))
                 (command (plist-get first-edit :command)))
            (lsp-proxy--render-next-edits sorted-edits command))
        (message "No next edit suggestions available")))))

;;;###autoload
(defun lsp-proxy-next-edit-accept ()
  "Accept the current next edit suggestion."
  (interactive)
  (unless lsp-proxy--next-edit-session
    (user-error "No next edit suggestion to accept"))

  (let* ((session lsp-proxy--next-edit-session)
         (edits (lsp-proxy-next-edit-session-edits session))
         (cmd (lsp-proxy-next-edit-session-command session)))

    ;; Clear overlays first
    (lsp-proxy--clear-next-edit-session)

    ;; Apply edits in reverse order to avoid offset issues
    ;; (edits are already sorted in descending order)
    (dolist (edit edits)
      (let ((start (plist-get edit :start-point))
            (end (plist-get edit :end-point))
            (text (plist-get edit :new-text)))
        (goto-char start)
        (delete-region start end)
        (insert text)))

    ;; Execute LSP command if present
    (when cmd
      (lsp-proxy-copilot-execute-command cmd))))

;;;###autoload
(defun lsp-proxy-next-edit-discard ()
  "Discard the current next edit suggestion."
  (interactive)
  (when lsp-proxy--next-edit-session
    (lsp-proxy--clear-next-edit-session)
    (message "Next edit suggestion discarded")))

(defun lsp-proxy-copilot-execute-command (command)
  "Execute a Copilot LSP COMMAND.
COMMAND should be a plist with :command and :arguments."
  (when command
    (let* ((cmd-name (plist-get command :command))
           (args (plist-get command :arguments)))
      (condition-case err
          (lsp-proxy-copilot--forward-request
           "workspace/executeCommand"
           (list :command cmd-name
                 :arguments (or args [])))
        (error
         (message "Failed to execute command %s: %s"
                  cmd-name (error-message-string err)))))))

;;; Auto-discard mechanism

(defun lsp-proxy--next-edit-post-command ()
  "Handle post-command for auto-discarding suggestions.
Increments movement counter and discards after threshold."
  (when-let ((session lsp-proxy--next-edit-session))
    (unless (memq this-command
                  '(lsp-proxy-next-edit-accept
                    lsp-proxy-next-edit-discard
                    lsp-proxy-copilot-next-edit))
      (cl-incf (lsp-proxy-next-edit-session-mouse-count session))
      (when (> (lsp-proxy-next-edit-session-mouse-count session)
               lsp-proxy-copilot-next-edit-auto-discard-threshold)
        (lsp-proxy-next-edit-discard)))))

;;; Minor mode for next edit suggestions

(defvar lsp-proxy-next-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'lsp-proxy-next-edit-accept)
    (define-key map (kbd "<tab>") #'lsp-proxy-next-edit-accept)
    (define-key map (kbd "C-g") #'lsp-proxy-next-edit-discard)
    map)
  "Keymap active when next edit suggestion is displayed.")

(define-minor-mode lsp-proxy-next-edit-mode
  "Minor mode active when a next edit suggestion is displayed.
\\{lsp-proxy-next-edit-mode-map}"
  :lighter " NextEdit"
  :keymap lsp-proxy-next-edit-mode-map
  (unless lsp-proxy-next-edit-mode
    (lsp-proxy--clear-next-edit-session)))

(provide 'lsp-proxy-copilot)
;;; lsp-proxy-copilot.el ends here
