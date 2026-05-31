;;; lsp-proxy-remote.el --- Remote development support for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Remote development configuration for lsp-proxy.
;; Provides customization options, notification handlers and an interactive
;; deploy command for working with remote hosts via TRAMP/SSH.

;;; Code:

(require 'lsp-proxy-utils)

(declare-function jsonrpc-async-request "jsonrpc" (connection method params &rest args))
(declare-function lsp-proxy--ensure-connection "lsp-proxy-core" ())
(defvar lsp-proxy--connection nil
  "The active jsonrpc connection to the lsp-proxy server.
Defined in `lsp-proxy-core'; declared here to avoid a circular require.")

;;; Low-level request helper
;;
;; `lsp-proxy--async-request' gates every send behind `lsp-proxy--has-any-servers',
;; which is only set after a file is opened with lsp-proxy-mode active.
;; Remote-management requests (check, deploy) are not document-scoped and must
;; reach the server regardless, so we bypass that guard and call
;; `jsonrpc-async-request' directly.
(cl-defun lsp-proxy-remote--async-request (method params &rest args
                                           &key
                                           (success-fn #'ignore)
                                           (error-fn   (lambda (err)
                                                         (message "[lsp-proxy] %s" err)))
                                           &allow-other-keys)
  "Send a server-management async request without the has-any-servers guard."
  (apply #'jsonrpc-async-request
         lsp-proxy--connection
         method params
         :success-fn success-fn
         :error-fn   error-fn
         (cl-loop for (k v) on args by #'cddr
                  unless (memq k '(:success-fn :error-fn))
                  append (list k v))))

(defgroup lsp-proxy-remote nil
  "Remote development settings for lsp-proxy."
  :prefix "lsp-proxy-remote-"
  :group 'lsp-proxy)

(defcustom lsp-proxy-remote-binary-path "~/.cache/emacs/lsp-proxy/emacs-lsp-proxy"
  "Path on the remote host where the emacs-lsp-proxy binary is installed.
The path is interpreted by the remote shell, so `~' is expanded on the
remote side.  Change this if your remote home directory is not writable
or you prefer a different installation location."
  :type 'string
  :group 'lsp-proxy-remote)

(defcustom lsp-proxy-remote-deploy-mode 'manual
  "How lsp-proxy handles a missing or outdated remote binary.

`manual' (default) — when the binary check fails, lsp-proxy prints a
  message and waits.  Run `M-x lsp-proxy-remote-deploy' to open the
  deploy buffer, review check results, confirm, and start the upload.

`auto' — lsp-proxy automatically starts the deploy as soon as the check
  fails, streaming progress to the `*lsp-proxy-deploy*' buffer so you
  can follow along.  No confirmation prompt is shown."
  :type '(choice (const :tag "Manual (prompt before deploying)" manual)
                 (const :tag "Auto (deploy immediately, show progress)" auto))
  :group 'lsp-proxy-remote)

;;; Internal state

(defvar lsp-proxy-remote--deploy-buffer-name "*lsp-proxy-deploy*"
  "Name of the buffer used to display remote deploy progress.")

(defvar lsp-proxy-remote--pending-connection-key nil
  "Connection key of the most recent host that requested a deploy.")

;;; Deploy buffer helpers

(defun lsp-proxy-remote--deploy-buffer ()
  "Return the deploy progress buffer, creating it if necessary."
  (get-buffer-create lsp-proxy-remote--deploy-buffer-name))

(defun lsp-proxy-remote--log (msg)
  "Append MSG with a timestamp to the deploy progress buffer."
  (with-current-buffer (lsp-proxy-remote--deploy-buffer)
    (goto-char (point-max))
    (insert (format-time-string "[%H:%M:%S] ") msg "\n")))

(defun lsp-proxy-remote--show-deploy-buffer ()
  "Pop up the deploy progress buffer without switching focus."
  (display-buffer (lsp-proxy-remote--deploy-buffer)
                  '((display-buffer-at-bottom)
                    (window-height . 12))))

(defun lsp-proxy-remote--reset-deploy-buffer (connection-key mode-label)
  "Clear the deploy buffer and write an opening header for CONNECTION-KEY.
MODE-LABEL is a short string describing the current deploy mode (e.g.
\"auto\" or \"manual\") shown in the header."
  (with-current-buffer (lsp-proxy-remote--deploy-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "Deploy log — %s  [%s]  (started %s)\n%s\n\n"
                      connection-key
                      mode-label
                      (format-time-string "%Y-%m-%d %H:%M:%S")
                      (make-string 60 ?-)))))
  (lsp-proxy-remote--show-deploy-buffer))

;;; Internal: run the actual upload

(defun lsp-proxy-remote--do-deploy (connection-key)
  "Send the deploy request for CONNECTION-KEY and stream progress to the buffer."
  (lsp-proxy-remote--log "")
  (lsp-proxy-remote--log "Starting upload...")
  (lsp-proxy-remote--async-request
   'emacs/deployRemoteBinary
   (list :params (list :connectionKey connection-key))
   :success-fn
   (lambda (result)
     (let ((ok   (eq (plist-get result :success) t))
           (msg  (plist-get result :message))
           (path (plist-get result :binaryPath)))
       (lsp-proxy-remote--log "")
       (lsp-proxy-remote--log
        (if ok
            (format "✓ Deploy succeeded. Binary: %s" (or path ""))
          (format "✗ Deploy failed: %s" msg)))
       (lsp-proxy-remote--show-deploy-buffer)
       (if ok
           (message "[lsp-proxy] Deploy succeeded. Re-open the remote file to connect.")
         (message "[lsp-proxy] Deploy failed — see %s for details."
                  lsp-proxy-remote--deploy-buffer-name))))
   :error-fn
   (lambda (err)
     (lsp-proxy-remote--log (format "✗ Error: %s" err))
     (lsp-proxy-remote--show-deploy-buffer)
     (message "[lsp-proxy] Deploy error — see %s for details."
              lsp-proxy-remote--deploy-buffer-name))))

;;; Notification handlers (called from lsp-proxy-core notification dispatcher)

(defun lsp-proxy-remote--handle-deploy-needed (params)
  "Handle `emacs/remoteDeployNeeded' notification with PARAMS.
Behaviour depends on `lsp-proxy-remote-deploy-mode':
- `manual': show a minibuffer hint pointing to `lsp-proxy-remote-deploy'.
- `auto':   start the deploy immediately, streaming progress to the buffer."
  (let* ((key        (plist-get params :connectionKey))
         (reason     (plist-get params :reason))
         (local-ver  (plist-get params :localVersion))
         (deploy-path (plist-get params :deployPath)))
    (setq lsp-proxy-remote--pending-connection-key key)
    (pcase lsp-proxy-remote-deploy-mode
      ('manual
       (message
        (concat "[lsp-proxy] Remote binary unavailable on %s (%s). "
                "Run M-x lsp-proxy-remote-deploy to deploy v%s to %s.")
        key reason local-ver deploy-path))
      ('auto
       (lsp-proxy-remote--reset-deploy-buffer key "auto")
       (lsp-proxy-remote--log
        (format "Binary unavailable (%s). Starting automatic deploy of v%s..."
                reason local-ver))
       (lsp-proxy-remote--log (format "Target path: %s" deploy-path))
       (lsp-proxy-remote--do-deploy key)))))

(defun lsp-proxy-remote--handle-deploy-progress (params)
  "Handle `emacs/remoteDeployProgress' notification with PARAMS."
  (lsp-proxy-remote--log (plist-get params :message)))

;;; Remote info query

;;;###autoload
(defun lsp-proxy-remote-get-info (success-fn)
  "Query the server for remote connection status and call SUCCESS-FN with the result.
Bypasses the `lsp-proxy--has-any-servers' guard so it works even when no
buffer has `lsp-proxy-mode' active."
  (lsp-proxy-remote--async-request
   'emacs/getRemoteInfo
   (list :params nil)
   :success-fn success-fn))

;;; TRAMP host candidates

(defun lsp-proxy-remote--tramp-ssh-keys ()
  "Return a deduplicated list of SSH host candidates for deploy completion.
Candidates are collected by calling every completion function registered
for the `ssh' method via `tramp-get-completion-function', which covers:
 - historical connections (`tramp-parse-connection-properties')
 - ~/.ssh/config  (`tramp-parse-sconfig')
 - ~/.ssh/known_hosts (`tramp-parse-shosts')
 - /etc/hosts.equiv, auth-sources, etc.
Each entry has the form `[user@]host' matching lsp-proxy's connection key."
  (when (fboundp 'tramp-get-completion-function)
    (let (keys)
      (dolist (spec (tramp-get-completion-function "ssh"))
        (let* ((fn  (car spec))
               (arg (cadr spec))
               (pairs (ignore-errors
                        (if arg (funcall fn arg) (funcall fn)))))
          (dolist (pair pairs)
            (let ((user (car pair))
                  (host (cadr pair)))
              (when (and host (not (string-empty-p host)))
                (cl-pushnew
                 (if (and user (not (string-empty-p user)))
                     (concat user "@" host)
                   host)
                 keys :test #'equal))))))
      (nreverse keys))))

;;; Remote config file

(defun lsp-proxy-remote--config-path-for-key (connection-key)
  "Return the TRAMP path to languages.toml for CONNECTION-KEY.
The config file lives in the same directory as the remote binary, which is
determined by `lsp-proxy-remote-binary-path'."
  (let* (;; Strip trailing /emacs-lsp-proxy (or any basename) to get the dir.
         (remote-dir (file-name-directory lsp-proxy-remote-binary-path))
         ;; Determine TRAMP method: prefer /ssh: but accept /rpc: for RPC hosts.
         (tramp-prefix (concat "/ssh:" connection-key ":")))
    (concat tramp-prefix remote-dir "languages.toml")))

(defun lsp-proxy-remote--connection-key-from-buffer ()
  "Return the SSH connection key for the current buffer's remote host, or nil.
Recognises TRAMP paths of the form /ssh:user@host:/... and /rpc:user@host:/...
and returns the `[user@]host' portion that lsp-proxy uses as a connection key."
  (when-let ((remote (and buffer-file-name
                          (file-remote-p buffer-file-name))))
    ;; remote looks like "/ssh:user@host:" or "/rpc:user@host:"
    (when (string-match "^/\\(?:ssh\\|rpc\\):\\(.*\\):$" remote)
      (match-string 1 remote))))

;;;###autoload
(defun lsp-proxy-remote-open-config-file ()
  "Open the languages.toml config file on a remote lsp-proxy host.

When the current buffer visits a remote file (via TRAMP), opens the config
directly on that host.  Otherwise prompts the user to choose a host from the
known SSH connections (same completion as `lsp-proxy-remote-deploy')."
  (interactive)
  (let ((key (lsp-proxy-remote--connection-key-from-buffer)))
    (unless key
      ;; Not on a remote buffer — ask the user.
      (setq key (completing-read
                 "user@host: "
                 (lsp-proxy-remote--tramp-ssh-keys)
                 nil nil
                 lsp-proxy-remote--pending-connection-key)))
    (unless (and key (not (string-empty-p key)))
      (user-error "No remote host selected"))
    (let ((config-file (lsp-proxy-remote--config-path-for-key key)))
      (find-file config-file))))

;;; Interactive deploy command

;;;###autoload
(defun lsp-proxy-remote-deploy (connection-key)
  "Check and optionally deploy emacs-lsp-proxy to the remote host CONNECTION-KEY.

Opens `*lsp-proxy-deploy*' showing the check result for both the global
command and the deploy path.  If a matching binary is already available
the command stops there.  Otherwise the user is asked to confirm before
the upload begins.

When called interactively, defaults to the host that last requested a deploy."
  (interactive
   (list (completing-read
          "user@host: "
          (lsp-proxy-remote--tramp-ssh-keys)
          nil nil
          lsp-proxy-remote--pending-connection-key)))
  (unless (and connection-key (not (string-empty-p connection-key)))
    (user-error "No connection key specified"))
  ;; Ensure the local lsp-proxy server is running — the deploy requests are
  ;; sent through it regardless of whether lsp-proxy-mode is active in any
  ;; buffer.
  (lsp-proxy--ensure-connection)
  (lsp-proxy-remote--reset-deploy-buffer connection-key "manual")
  (lsp-proxy-remote--log
   (concat "Checking remote binary on " connection-key
           "  (may take a moment while establishing SSH connection)..."))
  (lsp-proxy-remote--async-request
   'emacs/checkRemoteBinary
   (list :params (list :connectionKey connection-key))
   :success-fn
   (lambda (result)
     (let* ((local-ver   (plist-get result :localVersion))
            (deploy-path (plist-get result :deployPath))
            (available   (plist-get result :availableBinary))
            (global      (plist-get result :global))
            (path-info   (plist-get result :path))
            (g-status    (plist-get global    :status))
            (g-version   (plist-get global    :version))
            (p-status    (plist-get path-info :status))
            (p-version   (plist-get path-info :version)))
       (lsp-proxy-remote--log "")
       (lsp-proxy-remote--log (format "Local version : %s" local-ver))
       (lsp-proxy-remote--log (format "Deploy path   : %s" deploy-path))
       (lsp-proxy-remote--log "")
       (lsp-proxy-remote--log
        (format "Global command (emacs-lsp-proxy): %s"
                (pcase g-status
                  ("match"            (format "✓ v%s — up to date" g-version))
                  ("version_mismatch" (format "✗ v%s — version mismatch" g-version))
                  (_                  "✗ not found in PATH"))))
       (lsp-proxy-remote--log
        (format "Deploy path   (%s): %s"
                deploy-path
                (pcase p-status
                  ("match"            (format "✓ v%s — up to date" p-version))
                  ("version_mismatch" (format "✗ v%s — version mismatch" p-version))
                  (_                  "✗ not found"))))
       (lsp-proxy-remote--log "")
       (if available
           (progn
             (lsp-proxy-remote--log
              (format "✓ Binary available at %s — no deploy needed." available))
             (lsp-proxy-remote--show-deploy-buffer)
             (message "[lsp-proxy] Remote binary is up to date on %s." connection-key))
         (lsp-proxy-remote--log "Binary not available or outdated. Deploy required.")
         (lsp-proxy-remote--show-deploy-buffer)
         (when (yes-or-no-p
                (format "[lsp-proxy] Deploy v%s to %s? " local-ver connection-key))
           (lsp-proxy-remote--do-deploy connection-key)))))
   :error-fn
   (lambda (err)
     (lsp-proxy-remote--log (format "✗ Check failed: %s" err))
     (lsp-proxy-remote--show-deploy-buffer)
     (message "[lsp-proxy] Binary check failed — see %s."
              lsp-proxy-remote--deploy-buffer-name))))

(provide 'lsp-proxy-remote)
;;; lsp-proxy-remote.el ends here
