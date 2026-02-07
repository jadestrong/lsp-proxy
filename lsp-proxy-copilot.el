;;; lsp-proxy-copilot.el --- Copilot integration for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file provides integration with GitHub Copilot language server
;; through lsp-proxy's generic request forwarding mechanism.

;;; Commentary:

;; This module provides functions to sign in and sign out of GitHub Copilot
;; through the lsp-proxy server.

;;; Code:

(require 'lsp-proxy-core)

(defcustom lsp-proxy-copilot-server-name "copilot"
  "The copilot server name configed at custom config file."
  :type 'string
  :group 'lsp-proxy)

(defun lsp-proxy-copilot--forward-request (method params)
  "Forward a request with METHOD and PARAMS to the Copilot language server.
Returns a promise that resolves with the response."
  (unless (and (boundp 'lsp-proxy-mode) lsp-proxy-mode)
    (user-error "lsp-proxy-mode is not enabled in this buffer"))
  (unless buffer-file-name
    (user-error "Buffer must be visiting a file to communicate with Copilot"))

  (let* ((uri-plist (eglot--TextDocumentIdentifier))
         (uri (plist-get uri-plist :uri)))
    (lsp-proxy--request
     'emacs/forwardRequest
     (lsp-proxy--build-params
      (list :uri uri
            :serverName lsp-proxy-copilot-server-name
            :method method
            :params (or params (make-hash-table)))))))

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

(provide 'lsp-proxy-copilot)
;;; lsp-proxy-copilot.el ends here
