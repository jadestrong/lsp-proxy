;;; lsp-proxy-utils.el --- Utility functions for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utility functions and helpers for lsp-proxy

;;; Code:

(require 'cl-lib)
(require 'url-util)
(require 'project)
(require 'eglot)
(require 'yasnippet nil t)

(defvar lsp-proxy-mode)

(defcustom lsp-proxy-log-buffer-max message-log-max
  "Maximum number of lines to keep in the log buffer.
If nil, disable message logging.  If t, log messages but don't truncate
the buffer when it becomes large."
  :group 'lsp-proxy
  :type '(choice (const :tag "Disable" nil)
          (integer :tag "lines")
          (const :tag "Unlimited" t)))

(defcustom lsp-proxy-idle-delay 0.500
  "Debounce interval for `after-change-functions'."
  :type 'number
  :group 'lsp-proxy)

(defcustom lsp-proxy-on-idle-hook nil
  "Hooks to run after `lsp-proxy-idle-delay'."
  :type 'hook
  :group 'lsp-proxy)

;;; Message and logging functions

(defvar lsp-proxy--show-message t
  "Whether to show lsp-proxy messages.")

(defconst lsp-proxy--message-type-face
  `((1 . ,compilation-error-face)
    (2 . ,compilation-warning-face)
    (3 . ,compilation-message-face)
    (4 . ,compilation-info-face))
  "Faces for different message types.")

(defun lsp-proxy--message (format &rest args)
  "Wrapper for `message'.

We `inhibit-message' the message when the cursor is in the
minibuffer and when emacs version is before emacs 27 due to the
fact that we often use `lsp--info', `lsp--warn' and `lsp--error'
in async context and the call to these function is removing the
minibuffer prompt. The issue with async messages is already fixed
in emacs 27.

See #2049"
  (when lsp-proxy--show-message
    (let ((inhibit-message (or inhibit-message
                               (and (minibufferp)
                                    (version< emacs-version "27.0")))))
      (apply #'message format args))))

(defun lsp-proxy--info (format &rest args)
  "Display lsp info message with FORMAT with ARGS."
  (lsp-proxy--message "%s :: %s" (propertize "LSP-PROXY" 'face 'success) (apply #'format format args)))

(defun lsp-proxy--warn (format &rest args)
  "Display lsp warn message with FORMAT with ARGS."
  (lsp-proxy--message "%s :: %s" (propertize "LSP-PROXY" 'face 'warning) (apply #'format format args)))

(defun lsp-proxy--error (format &rest args)
  "Display lsp error message with FORMAT with ARGS."
  (lsp-proxy--message "%s :: %s" (propertize "LSP-PROXY" 'face 'error) (apply #'format format args)))

(defun lsp-proxy--propertize (str type)
  "Propertize STR as per TYPE."
  (propertize str 'face (alist-get type lsp-proxy--message-type-face)))

;;; Logging functionality

(defvar lsp-proxy--log-lines)

(defun lsp-proxy-log (format &rest args)
  "Log message to the *lsp-proxy-log* buffer.
FORMAT and ARGS is the same as for `message'."
  (when lsp-proxy-log-buffer-max
    (let ((log-buffer (get-buffer "*lsp-proxy-log*"))
          (inhibit-read-only t))
      (unless log-buffer
        (setq log-buffer (get-buffer-create "*lsp-proxy-log*"))
        (with-current-buffer log-buffer
          (buffer-disable-undo)
          (view-mode 1)
          (set (make-local-variable 'lsp-proxy--log-lines) 0)))
      (with-current-buffer log-buffer
        (save-excursion
          (let* ((message (apply 'format format args))
                 ;; Count newlines in message.
                 (newlines (1+ (cl-loop with start = 0
                                        for count from 0
                                        while (string-match "\n" message start)
                                        do (setq start (match-end 0))
                                        finally return count))))
            (goto-char (point-max))

            ;; in case the buffer is not empty insert before last \n to preserve
            ;; the point position(in case it is in the end)
            (if (eq (point) (point-min))
                (progn
                  (insert "\n")
                  (backward-char))
              (backward-char)
              (insert "\n"))
            (insert message)

            (setq lsp-proxy--log-lines (+ lsp-proxy--log-lines newlines))

            (when (and (integerp lsp-proxy-log-buffer-max) (> lsp-proxy--log-lines lsp-proxy-log-buffer-max))
              (let ((to-delete (- lsp-proxy--log-lines lsp-proxy-log-buffer-max)))
                (goto-char (point-min))
                (forward-line to-delete)
                (delete-region (point-min) (point))
                (setq lsp-proxy--log-lines lsp-proxy-log-buffer-max)))))))))

;;; Project management

(defvar-local lsp-proxy--current-project-root nil
  "Cached project root for current buffer.")

(defun lsp-proxy-project-root ()
  "Return the project root of current project."
  (if lsp-proxy--current-project-root
      lsp-proxy--current-project-root
    (let* ((project (project-current))
           (root (and project (project-root project)))
           (root-path (and root (directory-file-name root))))
      (setq lsp-proxy--current-project-root root-path)
      root-path)))

;;; Pattern matching utilities

(eval-and-compile
  (defun lsp-proxy--transform-pattern (pattern)
    "Transform PATTERN to (&plist PATTERN) recursively."
    (cons '&plist
          (mapcar (lambda (p)
                    (if (listp p)
                        (lsp-proxy--transform-pattern p)
                      p))
                  pattern))))

(defmacro lsp-proxy--dbind (pattern source &rest body)
  "Destructure SOURCE against plist PATTERN and eval BODY."
  (declare (indent 2))
  `(-let ((,(lsp-proxy--transform-pattern pattern) ,source))
     ,@body))

;;; Path utilities

(defun lsp-proxy--fix-path-casing (path)
  "On windows, downcases path because the windows file system is
case-insensitive.

On other systems, returns path without change."
  (if (eq system-type 'windows-nt) (downcase path) path))

(defun lsp-proxy--normalize-path (path)
  "On Windows systems, normalize path separators to Unix-style.
If the system is not Windows, return the original path."
  (if (eq system-type 'windows-nt)
      (replace-regexp-in-string "\\\\" "/" path)
    path))

(declare-function w32-long-file-name "w32proc.c" (fn))

(defun lsp-proxy--uri-to-path (uri)
  "Convert URI to file path."
  (when (keywordp uri) (setq uri (substring (symbol-name uri) 1)))
  (let* ((remote-prefix (and lsp-proxy--current-project-root (file-remote-p lsp-proxy--current-project-root)))
         (url (url-generic-parse-url uri)))
    ;; Only parse file:// URIs, leave other URI untouched as
    ;; `file-name-handler-alist' should know how to handle them
    ;; (bug#58790).
    (if (string= "file" (url-type url))
        (let* ((retval (url-unhex-string (url-filename url)))
               ;; Remove the leading "/" for local MS Windows-style paths.
               (normalized (if (and (not remote-prefix)
                                    (eq system-type 'windows-nt)
                                    (cl-plusp (length retval))
                                    (eq (aref retval 0) ?/))
                               (w32-long-file-name (substring retval 1))
                             retval)))
          (concat remote-prefix normalized))
      uri)))

;;; Snippet expansion

(declare-function yas-expand-snippet "ext:yasnippet")

(defun lsp-proxy--expand-snippet (snippet &optional start end expand-env)
  "Wrapper of `yas-expand-snippet' with all of it arguments.
The snippet will be convert to LSP style and indent according to
LSP server result."
  (require 'yasnippet)
  (let* ((inhibit-field-text-motion t)
         (yas-wrap-around-region nil)
         (yas-indent-line 'none)
         (yas-also-auto-indent-first-line nil))
    (yas-expand-snippet snippet start end expand-env)))

;;; Text indentation utilities

(defun lsp-proxy--indent-lines (start end &optional insert-text-mode?)
  "Indent from START to END based on INSERT-TEXT-MODE?."
  (save-excursion
    (goto-char start)
    (forward-line)
    (while (and (not insert-text-mode?) (< (point) end))
      (unless (eolp)
        (indent-according-to-mode))
      (forward-line))))

;;; Request parameters

(defun lsp-proxy--request-or-notify-params (params &rest args)
  "Wrap request or notify params base PARAMS and add extra ARGS."
  (require 'eglot)
  (let ((rest (apply 'append args)))
    (append (append (eglot--TextDocumentIdentifier) `(:params ,params)) rest)))


;;; Hash table project management utilities

(defun lsp-proxy--add-project (project-root-path project-map)
  "Add PROJECT-ROOT-PATH to PROJECT-MAP."
  (puthash project-root-path (make-hash-table :test 'equal) project-map))

(defun lsp-proxy--remove-project (project-root-path project-map)
  "Remove PROJECT-ROOT-PATH from PROJECT-MAP."
  (remhash project-root-path project-map))

(defun lsp-proxy--ensure-project-map (project-root project-map)
  "Ensure PROJECT-ROOT exists in PROJECT-MAP, creating if necessary.
Returns the hash table for the project."
  (or (gethash project-root project-map)
      (puthash project-root (make-hash-table :test 'equal) project-map)))

;;; Formatting utilities

(defun lsp-proxy--format-file-size (bytes)
  "Format file size BYTES in human readable format."
  (cond
   ((< bytes 1024) (format "%dB" bytes))
   ((< bytes (* 1024 1024)) (format "%.1fKB" (/ bytes 1024.0)))
   ((< bytes (* 1024 1024 1024)) (format "%.1fMB" (/ bytes 1024.0 1024.0)))
   (t (format "%.1fGB" (/ bytes 1024.0 1024.0 1024.0)))))

;;; Text Edit utilities

(defun lsp-proxy--create-apply-text-edits-handlers ()
  "Create (handler cleanup-fn) for applying text edits in async request.
Only works when mode is `tick or `alive."
  (let* (first-edited
         (func (lambda (start &rest _)
                 (setq first-edited (if first-edited
                                        (min start first-edited)
                                      start)))))
    (add-hook 'before-change-functions func nil t)
    (list
     (lambda (edits)
       (if (and first-edited
                (seq-find (lambda (edit) (let* ((range (plist-get edit :range))
                                                (end (plist-get range :end))
                                                (end-point (eglot--lsp-position-to-point end)))
                                           (> end-point first-edited)))
                          edits))
           (lsp-proxy--warn "%s" "TextEdits will not be applied since document has been modified before of them.")
         (eglot--apply-text-edits edits)))
     (lambda ()
       (remove-hook 'before-change-functions func t)))))

;;; Idle handling
(defvar-local lsp-proxy--on-idle-timer nil)

(defun lsp-proxy--idle-reschedule (buffer)
  "Reschedule idle timer for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when lsp-proxy--on-idle-timer
        (cancel-timer lsp-proxy--on-idle-timer))
      (setq lsp-proxy--on-idle-timer
            (run-with-idle-timer lsp-proxy-idle-delay nil
                                 #'lsp-proxy--on-idle buffer)))))

(defun lsp-proxy--on-idle (buffer)
  "Handle idle timeout for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq lsp-proxy--on-idle-timer nil)
      (when lsp-proxy-mode
        (run-hooks 'lsp-proxy-on-idle-hook)))))

(provide 'lsp-proxy-utils)
;;; lsp-proxy-utils.el ends here
