;;; lsp-proxy.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023-2025 JadeStrong
;;
;; Author: JadeStrong <jadestrong@163.com>
;; Maintainer: JadeStrong <jadestrong@163.com>
;; Created: December 15, 2023
;; Modified: December 15, 2023
;; Version: 0.4.0
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jadestrong/lsp-proxy
;; Package-Requires: ((emacs "30.1") (s "1.13.1") (eldoc "1.14.0") (ht "2.4") (dash "2.19.1") (f "0.21.0") (yasnippet "0.14.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:

;;  Description
;;
;;; Code:
(require 'cl-lib)
(require 'json)
(require 'jsonrpc)
(require 'xref)
(require 'compile)
(require 'seq)
(require 'url-util)
(require 'project)
(require 'eglot)

(require 's)
(require 'f)
(require 'ht)
(require 'dash)
(require 'yasnippet)

(declare-function yas-expand-snippet "ext:yasnippet")

(declare-function flycheck-buffer "ext:flycheck")
(declare-function flycheck-mode "ext:flycheck")
(declare-function flycheck-define-generic-checker
                  "ext:flycheck" (symbol docstring &rest properties))
(declare-function flycheck-error-new "ext:flycheck" t t)
(declare-function flycheck-error-message "ext:flycheck" (err) t)
(declare-function flycheck-define-error-level "ext:flycheck" (level &rest properties))
(declare-function flycheck-valid-checker-p "ext:flycheck")
(declare-function flycheck-stop "ext:flycheck")
(declare-function flycheck-checker-supports-major-mode-p "ext:flycheck")
(declare-function flycheck-add-mode "ext:flycheck")


(defvar flycheck-mode)
(defvar flycheck-check-syntax-automatically)
(defvar flycheck-checker)
(defvar flycheck-checkers)

(defvar lsp-proxy-mode)

(defgroup lsp-proxy nil
  "Interaction with Lsp Proxy Server."
  :prefix "lsp-proxy-"
  :group 'tools)

(defcustom lsp-proxy-user-languages-config (expand-file-name (concat user-emacs-directory (file-name-as-directory "lsp-proxy") "languages.toml"))
  "The user config file to store custom language config."
  :type 'string
  :group 'lsp-proxy)

(defcustom lsp-proxy-log-file-directory temporary-file-directory
  "The directory for `lsp-proxy` server to generate log file."
  :type 'string
  :group 'lsp-proxy)

(defcustom lsp-proxy-log-max 0
  "Max size of events buffer. 0 disables, nil means infinite.
Enabling event logging may slightly affect performance."
  :group 'lsp-proxy
  :type 'integer)

(defcustom lsp-proxy-log-buffer-max message-log-max
  "Maximum number of lines to keep in th elog buffer.
If nil, disable message logging.  If t, log messages but don’t truncate
the buffer when it becomes large."
  :group 'lsp-proxy
  :type '(choice (const :tag "Disable" nil)
          (integer :tag "lines")
          (const :tag "Unlimited" t)))

(defcustom lsp-proxy--send-changes-idle-time 0
  "Don't tell server of changes before Emacs's been idle for this many seconds."
  :group 'lsp-proxy
  :type 'number)

(defcustom lsp-proxy-idle-delay 0.500
  "Debounce interval for `after-change-functions'."
  :type 'number
  :group 'lsp-proxy)

(defcustom lsp-proxy-on-idle-hook nil
  "Hooks to run after `lsp-proxy-idle-delay'."
  :type 'hook
  :group 'lsp-proxy)

(defcustom lsp-proxy-hover-buffer "*lsp-proxy-help*"
  "Buffer for display hover info."
  :type 'string
  :group 'lsp-proxy)

(defcustom lsp-proxy-diagnostics-buffer "*lsp-proxy-diagnostics*"
  "Buffer for display diagnostics."
  :type 'string
  :group 'lsp-proxy)

(defcustom lsp-proxy-trim-trailing-whitespace t
  "Trim trailing whitespace on a line."
  :group 'lsp-proxy
  :type 'boolean)

(defcustom lsp-proxy-insert-final-newline t
  "Insert a newline character at the end of the file if one does not exist."
  :group 'lsp-proxy
  :type 'boolean)

(defcustom lsp-proxy-trim-final-newlines t
  "Trim all newlines after the final newline at the end of the file."
  :group 'lsp-proxy
  :type 'boolean)

(defcustom lsp-proxy-log-level 0
  "A number indicating the log level. Defaults to 0."
  :type '(choice (const :tag "Error" 0)
          (const :tag "Info" 1)
          (const :tag "Debug" 2)
          (const :tag "Trace" 3))
  :group 'lsp-proxy)

(defface lsp-proxy-hover-posframe
  '((t :inherit tooltip))
  "Background and foreground for `lsp-proxy-hover-posframe'."
  :group 'lsp-proxy)

(defcustom lsp-proxy-diagnostics-provider :auto
  "The checker backend provider."
  :type
  '(choice
    (const :tag "Pick flycheck if present and fallback to flymake" :auto)
    (const :tag "Pick flycheck" :flycheck)
    (const :tag "Pick flymake" :flymake)
    (const :tag "Use neither flymake nor lsp" :none)
    (const :tag "Prefer flymake" t)
    (const :tag "Prefer flycheck" nil))
  :group 'lsp-proxy)

(defcustom lsp-proxy-inlay-hints-mode-config nil
  "Configuration for enabling inlay hints mode in specific contexts.
The value can be:
- nil: Disable inlay hints completely
- t:   Enable inlay hints for all buffers
- (mode1 mode2 ...): Enable only for specified major modes
Example: `(`emacs-lisp-mode' `python-mode')'"
  :type
  '(choice
    (const :tag "Disabled" nil)
    (const :tag "Enabled for all buffers" t)
    (repeat :tag "Enabled only for specific modes" symbol))
  :group 'lsp-proxy)

(defcustom lsp-proxy-enable-symbol-highlighting t
  "Highlight references of the symbol at point."
  :type 'boolean
  :group 'lsp-proxy)

(defcustom lsp-proxy-inline-completion-idle-delay 0
  "Idle delay for inline-completion.

Complete immediately if set to 0.
Disable idle completion if set to nil."
  :type '(choice
          (number :tag "Seconds of delay")
          (const :tag "Idle completion disabled" nil))
  :group 'lsp-proxy)

(defcustom lsp-proxy-inline-completion-trigger-characters '(?\( ?\" ?\' ?\{)
  "Inline Completion trigger characters."
  :type '(repeat character)
  :group 'lsp-proxy)

(defcustom lsp-proxy-enable-imenu t
  "Enable imenu."
  :type 'boolean
  :group 'lsp-proxy)

(defcustom lsp-proxy-progress-prefix "⌛ "
  "Progress prefix."
  :group 'lsp-proxy-mode
  :type 'string)

(defvar lsp-proxy--exec-file (expand-file-name (if (eq system-type 'windows-nt)
                                                   "./lsp-proxy.exe"
                                                 "./lsp-proxy")
                                               (if load-file-name
                                                   (file-name-directory load-file-name)
                                                 default-directory)))
(defvar-local lsp-proxy--on-idle-timer nil)

(defvar-local lsp-proxy--inline-completion-trigger-by 1
  "How inline completion is actually triggered.")

(defvar lsp-proxy--log-file nil
  "The log file name.")

(defvar lsp-proxy--connection nil
  "Lsp Proxy agent jsonrpc connection instance.")

(defvar lsp-proxy--opened-buffers nil
  "List of buffers that have been opened in Lsp Proxy.")

(defvar-local lsp-proxy--recent-changes nil
  "Recent buffer changes as collected by `lsp-proxy--before-change'.")

(defvar-local lsp-proxy--change-idle-timer nil
  "Idle timer for didChange signals.")

(defvar-local lsp-proxy-enable-relative-indentation nil
  "Enable relative indentation when insert texts, snippets ...
from language server.")

(defvar-local lsp-proxy-diagnostics--flycheck-enabled nil
  "Non-nil when flycheck integration has been enabled in this buffer.")

(defvar-local lsp-proxy-diagnostics--flymake-enabled nil
  "Non-nil when flymake integration has been enabled in this buffer.")

(defvar-local lsp-proxy-diagnostics--flycheck-checker nil
  "The value of flycheck-checker before lsp-proxy diagnostics was activated.")

(defvar-local lsp-proxy--completion-trigger-characters nil
  "Completion trigger characters.")

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

(defvar-local lsp-proxy--enable-symbol-highlighting t
  "Automatically disable when the number of buffer lines exceeds 10000,
since the `eglot--lsp-position-to-point' method is costly.")

(defvar-local lsp-proxy--support-pull-diagnostic nil
  "Is there any server associated with this buffer
that support `textDocument/diagnostic' request.")

(defvar lsp-proxy--show-message t
  "If non-nil, show debug message from `lsp-proxy-mode'.")

(defvar lsp-proxy--formatting-indent-alist
  ;; Taken from `dtrt-indent-mode'
  '(
    (ada-mode                   . ada-indent)                       ; Ada
    (c++-mode                   . c-basic-offset)                   ; C++
    (c++-ts-mode                . c-ts-mode-indent-offset)
    (c-mode                     . c-basic-offset)                   ; C
    (c-ts-mode                  . c-ts-mode-indent-offset)
    (cperl-mode                 . cperl-indent-level)               ; Perl
    (crystal-mode               . crystal-indent-level)             ; Crystal (Ruby)
    (csharp-mode                . c-basic-offset)                   ; C#
    (csharp-tree-sitter-mode    . csharp-tree-sitter-indent-offset) ; C#
    (csharp-ts-mode             . csharp-ts-mode-indent-offset)     ; C# (tree-sitter, Emacs29)
    (css-mode                   . css-indent-offset)                ; CSS
    (d-mode                     . c-basic-offset)                   ; D
    (enh-ruby-mode              . enh-ruby-indent-level)            ; Ruby
    (erlang-mode                . erlang-indent-level)              ; Erlang
    (ess-mode                   . ess-indent-offset)                ; ESS (R)
    (go-ts-mode                 . go-ts-mode-indent-offset)
    (hack-mode                  . hack-indent-offset)               ; Hack
    (java-mode                  . c-basic-offset)                   ; Java
    (java-ts-mode               . java-ts-mode-indent-offset)
    (jde-mode                   . c-basic-offset)                   ; Java (JDE)
    (js-mode                    . js-indent-level)                  ; JavaScript
    (js2-mode                   . js2-basic-offset)                 ; JavaScript-IDE
    (js3-mode                   . js3-indent-level)                 ; JavaScript-IDE
    (json-mode                  . js-indent-level)                  ; JSON
    (json-ts-mode               . json-ts-mode-indent-offset)
    (lua-mode                   . lua-indent-level)                 ; Lua
    (nxml-mode                  . nxml-child-indent)                ; XML
    (objc-mode                  . c-basic-offset)                   ; Objective C
    (pascal-mode                . pascal-indent-level)              ; Pascal
    (perl-mode                  . perl-indent-level)                ; Perl
    (php-mode                   . c-basic-offset)                   ; PHP
    (powershell-mode            . powershell-indent)                ; PowerShell
    (raku-mode                  . raku-indent-offset)               ; Perl6/Raku
    (ruby-mode                  . ruby-indent-level)                ; Ruby
    (rust-mode                  . rust-indent-offset)               ; Rust
    (rust-ts-mode               . rust-ts-mode-indent-offset)
    (rustic-mode                . rustic-indent-offset)             ; Rust
    (scala-mode                 . scala-indent:step)                ; Scala
    (sgml-mode                  . sgml-basic-offset)                ; SGML
    (sh-mode                    . sh-basic-offset)                  ; Shell Script
    (toml-ts-mode               . toml-ts-mode-indent-offset)
    (typescript-mode            . typescript-indent-level)          ; Typescript
    (typescript-ts-mode         . typescript-ts-mode-indent-offset) ; Typescript (tree-sitter, Emacs29)
    (yaml-mode                  . yaml-indent-offset)               ; YAML

    (default                    . standard-indent))                 ; default fallback
  "A mapping from `major-mode' to its indent variable.")

;; diagnostics map
(defvar lsp-proxy--diagnostics-map (make-hash-table :test 'equal))

;;
;; schedule
;;
(defun lsp-proxy--idle-reschedule (buffer)
  "LSP proxy idle schedule on current BUFFER."
  (when lsp-proxy--on-idle-timer
    (cancel-timer lsp-proxy--on-idle-timer))
  (setq-local lsp-proxy--on-idle-timer (run-with-idle-timer
                                        lsp-proxy-idle-delay
                                        nil
                                        #'lsp-proxy--on-idle
                                        buffer)))
(defun lsp-proxy--on-idle (buffer)
  "Start post command loop on current BUFFER."
  (when (and (buffer-live-p buffer)
             (equal buffer (current-buffer))
             lsp-proxy-mode)
    (run-hooks 'lsp-proxy-on-idle-hook)))

;;
;; log message
;;

(defconst lsp-proxy--message-type-face
  `((1 . ,compilation-error-face)
    (2 . ,compilation-warning-face)
    (3 . ,compilation-message-face)
    (4 . ,compilation-info-face)))

(defun lsp-proxy--message  (format &rest args)
  "Wrapper for `message'

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

;; Buffer local variable for storing number of lines.
(defvar lsp-proxy--log-lines)
(defun lsp-proxy-log (format &rest args)
  "Log message to the *lsp-proxy-log* buffer.
FORMAT and ARGS is the same as for `messsage'."
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

;; project root
(defvar-local lsp-proxy--cur-project-root nil)
(defun lsp-proxy-project-root ()
  "Return the project root of current project."
  (if lsp-proxy--cur-project-root
      lsp-proxy--cur-project-root
    (let* ((project (project-current))
           (root (and project (project-root project)))
           (root-path (and root (directory-file-name root))))
      (setq lsp-proxy--cur-project-root root-path)
      root-path)))
;;
;; utils
;;
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

(defun lsp-proxy--fix-path-casing (path)
  "On windows, downcases path because the windows file system is
case-insensitive.

On other systems, returns path without change."
  (if (eq system-type 'window-nt) (downcase path) path))

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
  (let* ((remote-prefix (and lsp-proxy--cur-project-root (file-remote-p lsp-proxy--cur-project-root)))
         (url (url-generic-parse-url uri)))
    ;; Only parse file:// URIs, leave other URI untouched as
    ;; `file-name-handler-alist' should know how to handle them
    ;; (bug#58790).
    (if (string= "file" (url-type url))
        (let* ((retval (url-unhex-string (url-filename url)))
               (normalized (if (and (not remote-prefix)
                                    (eq system-type 'windows-nt)
                                    (cl-plusp (length retval)))
                               (w32-long-file-name (substring retval 1))
                             retval)))
          (concat remote-prefix normalized))
      uri)))

(defun lsp-proxy--expand-snippet (snippet &optional start end expand-env)
  "Wrapper of `yas-expand-snippet' with all of it arguments.
The snippet will be convert to LSP style and indent according to
LSP server result."
  (let* ((inhibit-field-text-motion t)
         (yas-wrap-around-region nil)
         (yas-indent-line 'none)
         (yas-also-auto-indent-first-line nil))
    (yas-expand-snippet snippet start end expand-env)))

(defun lsp-proxy--indent-lines (start end &optional insert-text-mode?)
  "Indent from START to END based on INSERT-TEXT-MODE? value.
- When INSERT-TEXT-MODE? is provided
  - if it's `lsp/insert-text-mode-as-it', do no editor indentation.
  - if it's `lsp/insert-text-mode-adjust-indentation', adjust leading
    whitespaces to match the line where text is inserted.
- When it's not provided, using `indent-line-function' for each line."
  (save-excursion
    (goto-char end)
    (let* ((end-line (line-number-at-pos))
           (offset (save-excursion
                     (goto-char start)
                     (current-indentation)))
           (indent-line-function
            (cond ((eql insert-text-mode? 1)
                   #'ignore)
                  ((or (equal insert-text-mode? 2)
                       lsp-proxy-enable-relative-indentation
                       ;; Indenting snippets is extremely slow in `org-mode' buffers
                       ;; since it has to calculate indentation based on SRC block
                       ;; position.  Thus we use relative indentation as default.
                       (derived-mode-p 'org-mode))
                   (lambda () (save-excursion
                                (beginning-of-line)
                                (indent-to-column offset))))
                  (t indent-line-function))))
      (goto-char start)
      (forward-line)
      (while (and (not (eobp))
                  (<= (line-number-at-pos) end-line))
        (funcall indent-line-function)
        (forward-line)))))

(defun lsp-proxy--request-or-notify-params (params &rest args)
  "Wrap request or notify params base PARAMS and add extra ARGS."
  (let ((rest (apply 'append args)))
    (append (append (eglot--TextDocumentIdentifier) `(:params ,params)) rest)))

(defun lsp-proxy--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-proxy--advice-json-parse)

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

;;
;; modeline progress
;;
(defvar lsp-proxy--project-hashmap (make-hash-table :test 'equal))

(defun lsp-proxy--add-project (project-root-path project-map)
  (puthash project-root-path (make-hash-table :test 'equal) project-map))

(defun lsp-proxy--remove-project (project-root-path project-map)
  (if project-root-path
      (remhash project-root-path project-map)))

(defun lsp-proxy--get-or-create-project (project-root-path project-map)
  (or (gethash project-root-path project-map)
      (lsp-proxy--add-project project-root-path project-map)
      (gethash project-root-path project-map)))

(defun lsp-proxy--set-work-done-token (project-root-path token value)
  (let ((project (lsp-proxy--get-or-create-project project-root-path lsp-proxy--project-hashmap)))
    (if project
        (puthash token value project)
      (error "Project not found: %s" project-root-path))))

(defun lsp-proxy--rem-work-done-token (project-root-path token)
  (let ((project (gethash project-root-path lsp-proxy--project-hashmap)))
    (if project
        (remhash token project)
      (error "Project not found: %s" project-root-path))))

(defun lsp-proxy--progressing-p (project-root-path)
  "Check if the server at PROJECT-ROOT-PATH is in progress."
  (let ((project (gethash project-root-path lsp-proxy--project-hashmap)))
    (and project (not (hash-table-empty-p project)))))

(defun lsp-proxy--progress-status ()
  "Return the status of the progress for the current workspaces."
  (when lsp-proxy-mode
    (let ((progress-status
           (when-let* ((tokens (gethash (lsp-proxy--fix-path-casing (lsp-proxy-project-root)) lsp-proxy--project-hashmap)))
             (unless (ht-empty? tokens)
               (mapconcat
                (lambda (value)
                  (let* ((msg (plist-get value :message))
                         (title (plist-get value :title))
                         (percentage (plist-get value :percentage)))
                    (concat (if percentage
                                (if (numberp percentage)
                                    (format "%.0f%%%% " percentage)
                                  (format "%s%%%% " percentage))
                              "")
                            (or msg title))))
                (ht-values tokens)
                "|")))))
      (unless (s-blank? progress-status)
        (concat lsp-proxy-progress-prefix progress-status " ")))))
;;
;; agent
;;
(defconst lsp-proxy--ignore-response
  (lambda (_))
  "Simply ignore the response")

(defconst lsp-proxy--show-error
  (lambda (err)
    (lsp-proxy--error "%s" (or (and err (plist-get err :message)) err)))
  "Default handler for error message.")

(defconst lsp-proxy--show-timeout
  (lambda (method)
    (lsp-proxy--error "%s(%s)" "Request timeout" method))
  "Default handler for timeout.")

(defsubst lsp-proxy--connection-alivep ()
  "Non-nil if the `lsp-proxy--connection' is alive."
  (and lsp-proxy--connection
       (zerop (process-exit-status (jsonrpc--process lsp-proxy--connection)))))

(defmacro lsp-proxy--request (&rest args)
  "Send a request to the lsp proxy agent with ARGS."
  `(progn
     (when lsp-proxy-mode
       (unless (lsp-proxy--connection-alivep)
         (lsp-proxy--start-server))
       (lsp-proxy--send-did-change)
       (unless (-contains-p lsp-proxy--opened-buffers (current-buffer))
         (lsp-proxy--on-doc-open))
       (jsonrpc-request lsp-proxy--connection ,@args))))

(defmacro lsp-proxy--notify (method &rest params)
  "Send a notification to the lsp proxy agent with ARGS."
  `(progn
     (unless (lsp-proxy--connection-alivep)
       (lsp-proxy--start-server))
     (if (or (eq ,method 'textDocument/didOpen) (eq ,method 'textDocument/willSave) (eq ,method 'textDocument/didSave) (-contains-p lsp-proxy--opened-buffers (current-buffer)))
         (let ((new-params (append (eglot--TextDocumentIdentifier) (list :params ,@params))))
           (jsonrpc-notify lsp-proxy--connection ,method new-params))
       (lsp-proxy--on-doc-open))))

(cl-defmacro lsp-proxy--async-request (method params &rest args &key (success-fn #'lsp-proxy--ignore-response) (error-fn #'lsp-proxy--show-error) (timeout-fn #'lsp-proxy--show-timeout) &allow-other-keys)
  "Send an asynchronous request to the lsp proxy agent."
  `(progn
     (unless (lsp-proxy--connection-alivep)
       (lsp-proxy--start-server))
     (if (not (eq ,method 'textDocument/diagnostic))
         (lsp-proxy--send-did-change))
     (unless (-contains-p lsp-proxy--opened-buffers (current-buffer))
       (lsp-proxy--on-doc-open))
     ;; jsonrpc will use temp buffer for callbacks, so we nned to save the current buffer and restore it inside callback
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

(defun lsp-proxy--make-connection ()
  "Establish proxy jsonrpc connection."
  (let ((make-fn (apply-partially
                  #'make-instance
                  'jsonrpc-process-connection
                  :name "lsp proxy"
                  :notification-dispatcher #'lsp-proxy--handle-notification
                  :request-dispatcher #'lsp-proxy--handle-request
                  :process (make-process :name "lsp proxy agent"
                                         :command (list lsp-proxy--exec-file "--config" lsp-proxy-user-languages-config "--log-level" (number-to-string lsp-proxy-log-level) "--log" lsp-proxy--log-file)
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
    (if (file-exists-p lsp-proxy--exec-file)
        (progn
          (setq lsp-proxy--connection (lsp-proxy--make-connection))
          (message "Lsp proxy server started."))
      (lsp-proxy--error "No lsp-proxy file found, please check your `lsp-proxy--exec-file'"))))

(defun lsp-proxy--handle-notification (_ method msg)
  "Handle MSG of type METHOD."
  (when (eql method 'textDocument/publishDiagnostics)
    (lsp-proxy--dbind (:uri uri :diagnostics diagnostics) msg
      (let ((filepath (lsp-proxy--uri-to-path uri)))
        (if (f-exists-p filepath)
            (with-current-buffer (find-file-noselect filepath)
              (let ((workspace-diagnostics (lsp-proxy--get-or-create-project
                                            (lsp-proxy-project-root)
                                            lsp-proxy--diagnostics-map))
                    (file (lsp-proxy--fix-path-casing filepath)))
                (if (seq-empty-p diagnostics)
                    (remhash file workspace-diagnostics)
                  (puthash file (append diagnostics nil) workspace-diagnostics)))
              (cond (lsp-proxy-diagnostics--flycheck-enabled
                     (add-hook 'lsp-proxy-on-idle-hook #'lsp-proxy-diagnostics--flycheck-buffer nil t)
                     (lsp-proxy--idle-reschedule (current-buffer)))
                    (lsp-proxy-diagnostics--flymake-enabled
                     (lsp-proxy-diagnostics--flymake-after-diagnostics))
                    (t (lsp-proxy--warn "No diagnostics mode enabled for this buffer. Ensure Flycheck or Flymake is active."))))
          (if (> lsp-proxy-log-level 1)
              (lsp-proxy--error "The file not found %s (uri=%s)" filepath uri))))))
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
        (when (f-exists? filepath)
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

;;
;; lsp request/notification
;;
(defun lsp-proxy--on-doc-focus (window)
  "Notify that the document has been focussed or opened."
  ;; When switching windows, this function is called twice, once for the
  ;; window losing and once for the window gaining focus. We only want to
  ;; send a notification for the window gaining focus and only if the buffer has
  ;; lsp-proxy-mode enabled.
  (when (and lsp-proxy-mode (eq window (selected-window)))
    (if (-contains-p lsp-proxy--opened-buffers (current-buffer))
        (lsp-proxy--notify ':textDocument/didFocus
                           (list :textDocument (eglot--TextDocumentIdentifier)))
      (lsp-proxy--on-doc-open))))

(defun lsp-proxy--on-doc-open ()
  "On doc open."
  (setq lsp-proxy--recent-changes nil
        eglot--versioned-identifier 0)
  (when buffer-file-name
    (when (not (f-exists? buffer-file-name))
      (save-buffer))
    (add-to-list 'lsp-proxy--opened-buffers (current-buffer))
    (setq lsp-proxy--enable-symbol-highlighting (< (line-number-at-pos (point-max)) 10000))
    (lsp-proxy--notify 'textDocument/didOpen
                       (list :textDocument (append (eglot--TextDocumentIdentifier)
                                                   (list
                                                    :text (eglot--widening
                                                           (buffer-substring-no-properties (point-min) (point-max)))
                                                    :languageId ""
                                                    :version eglot--versioned-identifier))))))

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
                     (list :textDocument (eglot--TextDocumentIdentifier) :reason 1 )))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xref integration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lsp-proxy--xref-backend () "lsp-proxy xref backend." 'xref-lsp-proxy)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-lsp-proxy)))
  (propertize (or (thing-at-point 'symbol) "")
              'identifier-at-point t))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp-proxy)))
  (list (propertize (or (thing-at-point 'symbol) "")
                    'identifier-at-point t)))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-lsp-proxy)) _identifier)
  (save-excursion
    (lsp-proxy-find-definition)))

(cl-defmethod xref-backend-references ((_backend (eql xref-lsp-proxy)) _identifier)
  (save-excursion
    (lsp-proxy-find-references)))

(cl-defmethod xref-backend-implementations ((_backend (eql xref-lsp-proxy)) _identifier)
  (save-excursion
    (lsp-proxy-find-implementations)))

(cl-defmethod xref-backend-type-definitions ((_backend (eql xref-lsp-proxy)) _identifier)
  (save-excursion
    (lsp-proxy-find-type-definition)))

(defun lsp-proxy-show-xrefs (xrefs display-action references?)
  (unless (region-active-p) (push-mark nil t))
  (if (boundp 'xref-show-definitions-function)
      (with-no-warnings
        (xref-push-marker-stack)
        (funcall (if (and references? (not lsp-xref-force-references))
                     xref-show-xrefs-function
                   xref-show-definitions-function)
                 (-const xrefs)
                 `((window . ,(selected-window))
                   (display-action . ,display-action)
                   ,(if (and references? (not lsp-xref-force-references))
                        `(auto-jump . ,xref-auto-jump-to-first-xref)
                      `(auto-jump . ,xref-auto-jump-to-first-definition)))))
    (xref--show-xrefs xrefs display-action)))

(defcustom lsp-proxy-lazy-xref-threshold 10000
  "Threshold for using lazy xref evaluation.
Files with more than this many lines will use optimized evaluation to improve performance.
Set to nil to always use lazy evaluation, or a very large number to disable it."
  :type '(choice (const :tag "Always lazy" nil)
                 (integer :tag "Line count threshold"))
  :group 'lsp-proxy)

(defcustom lsp-proxy-xref-optimization-strategy 'optimized
  "Strategy for handling xref in large files.
- 'eager: Always use original method (may be slow for large files)
- 'lazy: Use lazy evaluation (no preview, fastest)
- 'optimized: Use optimized method with preview (balanced)"
  :type '(choice (const :tag "Eager (original)" eager)
                 (const :tag "Lazy (no preview)" lazy)
                 (const :tag "Optimized (fast with preview)" optimized))
  :group 'lsp-proxy)

;; Custom xref location class for lazy evaluation
(cl-defstruct (lsp-proxy--lazy-location
               (:constructor lsp-proxy--lazy-location-create)
               (:conc-name lsp-proxy--lazy-location-))
  filepath range)

(cl-defmethod xref-location-marker ((location lsp-proxy--lazy-location))
  "Return the marker for lazy LOCATION, computing it on-demand."
  (let* ((filepath (lsp-proxy--lazy-location-filepath location))
         (range (lsp-proxy--lazy-location-range location))
         (start (plist-get range :start)))
    (with-current-buffer (find-file-noselect filepath)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (eglot--lsp-position-to-point start))
          (point-marker))))))

(cl-defmethod xref-location-group ((location lsp-proxy--lazy-location))
  "Return the group (file) for lazy LOCATION."
  (lsp-proxy--lazy-location-filepath location))

(cl-defmethod xref-location-line ((location lsp-proxy--lazy-location))
  "Return the line number for lazy LOCATION."
  (let* ((range (lsp-proxy--lazy-location-range location))
         (start (plist-get range :start)))
    (1+ (plist-get start :line))))

(defun lsp-proxy--should-use-lazy-xref-p (filepath)
  "Determine if FILEPATH should use lazy xref evaluation based on file size."
  (and lsp-proxy-lazy-xref-threshold
       (file-exists-p filepath)
       (let ((visiting (find-buffer-visiting filepath)))
         (cond
          (visiting
           ;; If buffer is already open, check line count directly
           (with-current-buffer visiting
             (> (line-number-at-pos (point-max)) lsp-proxy-lazy-xref-threshold)))
          (t
           ;; For unopened files, estimate based on file size (rough heuristic)
           ;; Assume average ~50 characters per line
           (> (/ (nth 7 (file-attributes filepath)) 50) lsp-proxy-lazy-xref-threshold))))))

(defun lsp-proxy--process-location-eager (location-data)
  "Process a single location using eager evaluation (original method)."
  (let* ((uri (plist-get location-data :uri))
         (filepath (lsp-proxy--uri-to-path uri))
         (visiting (find-buffer-visiting filepath))
         (range (plist-get location-data :range))
         (start (plist-get range :start))
         (end (plist-get range :end))
         (start-line (plist-get start :line))
         (start-column (plist-get start :character))
         (collect (lambda ()
                    (save-excursion
                      (save-restriction
                        (widen)
                        (let* ((beg (eglot--lsp-position-to-point start))
                               (end-pos (eglot--lsp-position-to-point end))
                               (bol (progn (goto-char beg) (line-beginning-position)))
                               (summary (buffer-substring bol (line-end-position)))
                               (hi-beg (- beg bol))
                               (hi-end (- (min (line-end-position) end-pos) bol)))
                          (when summary
                            (add-face-text-property hi-beg hi-end 'xref-match t summary))
                          (xref-make summary
                                     (xref-make-file-location filepath (1+ start-line) start-column))))))))
    (cond
     (visiting (with-current-buffer visiting (funcall collect)))
     ((file-readable-p filepath)
      (with-temp-buffer
        (insert-file-contents-literally filepath)
        (funcall collect)))
     (t (lsp-proxy--warn "Failed to process xref entry for file %s" filepath)
        nil))))

(defun lsp-proxy--process-location-optimized (location-data)
  "Process a single location using optimized method for large files."
  (let* ((uri (plist-get location-data :uri))
         (filepath (lsp-proxy--uri-to-path uri))
         (visiting (find-buffer-visiting filepath))
         (range (plist-get location-data :range))
         (start (plist-get range :start))
         (end (plist-get range :end))
         (start-line (plist-get start :line))
         (start-column (plist-get start :character))
         (end-line (plist-get end :line))
         (end-column (plist-get end :character)))
    (cond
     (visiting
      ;; For visited buffers, use optimized line-based calculation
      (with-current-buffer visiting
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (forward-line start-line)  ; Much faster than eglot--lsp-position-to-point
            (let* ((bol (line-beginning-position))
                   (eol (line-end-position))
                   (summary (buffer-substring bol eol))
                   (hi-beg start-column)
                   (hi-end (if (= start-line end-line)
                              (min end-column (length summary))
                            (length summary))))
              (when (and summary (> hi-end hi-beg))
                (add-face-text-property hi-beg hi-end 'xref-match t summary))
              (xref-make (if (string-blank-p summary)
                            (format "%s:%d:%d" 
                                   (file-name-nondirectory filepath)
                                   (1+ start-line) 
                                   (1+ start-column))
                          summary)
                         (xref-make-file-location filepath (1+ start-line) start-column)))))))
     ((file-readable-p filepath)
      ;; For unvisited files, use efficient line reading
      (let ((summary (lsp-proxy--get-line-content filepath start-line start-column end-line end-column)))
        (xref-make (if (string-blank-p summary)
                      (format "%s:%d:%d" 
                             (file-name-nondirectory filepath)
                             (1+ start-line) 
                             (1+ start-column))
                    summary)
                   (xref-make-file-location filepath (1+ start-line) start-column))))
     (t (lsp-proxy--warn "Failed to process xref entry for file %s" filepath)
        nil))))

(defun lsp-proxy--get-line-content (filepath start-line start-column end-line end-column)
  "Efficiently get line content from FILEPATH without loading entire file."
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (large-file-warning-threshold nil))
      ;; Only read the lines we need
      (insert-file-contents filepath nil)
      (goto-char (point-min))
      (forward-line start-line)
      (let* ((bol (line-beginning-position))
             (eol (line-end-position))
             (summary (buffer-substring bol eol))
             (hi-beg start-column)
             (hi-end (if (= start-line end-line)
                        (min end-column (length summary))
                      (length summary))))
        (when (and summary (> hi-end hi-beg) (>= hi-beg 0))
          (add-face-text-property hi-beg hi-end 'xref-match t summary))
        summary))))

(defun lsp-proxy--process-location-lazy (location-data)
  "Process a single location using lazy evaluation."
  (let* ((uri (plist-get location-data :uri))
         (filepath (lsp-proxy--uri-to-path uri))
         (range (plist-get location-data :range))
         (start (plist-get range :start))
         (start-line (plist-get start :line))
         (start-column (plist-get start :character))
         (location (lsp-proxy--lazy-location-create
                    :filepath filepath
                    :range range))
         (summary (format "%s:%d:%d" 
                         (file-name-nondirectory filepath)
                         (1+ start-line) 
                         (1+ start-column))))
    (when (file-exists-p filepath)
      (xref-make summary location))))

(defun lsp-proxy--batch-process-locations-optimized (locations-by-file)
  "Batch process locations grouped by file for optimal performance."
  (let (results)
    (maphash 
     (lambda (filepath file-locations)
       (let ((visiting (find-buffer-visiting filepath)))
         (cond
          (visiting
           ;; Process all locations in one buffer operation
           (with-current-buffer visiting
             (save-excursion
               (save-restriction
                 (widen)
                 (dolist (location-data file-locations)
                   (let* ((range (plist-get location-data :range))
                          (start (plist-get range :start))
                          (end (plist-get range :end))
                          (start-line (plist-get start :line))
                          (start-column (plist-get start :character))
                          (end-line (plist-get end :line))
                          (end-column (plist-get end :character)))
                     (goto-char (point-min))
                     (forward-line start-line)
                     (let* ((bol (line-beginning-position))
                            (eol (line-end-position))
                            (summary (buffer-substring bol eol))
                            (hi-beg start-column)
                            (hi-end (if (= start-line end-line)
                                       (min end-column (length summary))
                                     (length summary))))
                       (when (and summary (> hi-end hi-beg))
                         (add-face-text-property hi-beg hi-end 'xref-match t summary))
                       (push (xref-make (if (string-blank-p summary)
                                           (format "%s:%d:%d" 
                                                  (file-name-nondirectory filepath)
                                                  (1+ start-line) 
                                                  (1+ start-column))
                                         summary)
                                        (xref-make-file-location filepath (1+ start-line) start-column))
                             results))))))))
          ((file-readable-p filepath)
           ;; Batch read file once for all locations
           (let ((file-lines (lsp-proxy--read-file-lines filepath file-locations)))
             (dolist (location-data file-locations)
               (let* ((range (plist-get location-data :range))
                      (start (plist-get range :start))
                      (start-line (plist-get start :line))
                      (start-column (plist-get start :character))
                      (summary (and (< start-line (length file-lines))
                                   (aref file-lines start-line))))
                 (when summary
                   (let* ((hi-beg start-column)
                          (hi-end (min (+ start-column 10) (length summary)))) ; Simple highlight
                     (when (and (> hi-end hi-beg) (>= hi-beg 0))
                       (add-face-text-property hi-beg hi-end 'xref-match t summary))))
                 (push (xref-make (or summary
                                     (format "%s:%d:%d" 
                                            (file-name-nondirectory filepath)
                                            (1+ start-line) 
                                            (1+ start-column)))
                                  (xref-make-file-location filepath (1+ start-line) start-column))
                       results)))))
          (t
           (lsp-proxy--warn "Failed to process xref entry for file %s" filepath)))))
     locations-by-file)
    (nreverse results)))

(defun lsp-proxy--read-file-lines (filepath locations)
  "Read only the required lines from FILEPATH for given LOCATIONS."
  (let ((line-numbers (mapcar (lambda (loc) 
                               (plist-get (plist-get (plist-get loc :range) :start) :line))
                             locations)))
    (with-temp-buffer
      (let ((inhibit-read-only t)
            (large-file-warning-threshold nil)
            (lines (make-hash-table :test 'equal)))
        (insert-file-contents filepath nil)
        (goto-char (point-min))
        (let ((current-line 0))
          (while (not (eobp))
            (when (memq current-line line-numbers)
              (puthash current-line (buffer-substring (line-beginning-position) (line-end-position)) lines))
            (forward-line 1)
            (setq current-line (1+ current-line))))
        ;; Convert hash table to list indexed by line number
        (let ((max-line (apply #'max line-numbers))
              (result-lines (make-vector (1+ max-line) nil)))
          (maphash (lambda (line-num content)
                    (aset result-lines line-num content))
                  lines)
          result-lines)))))

(defun lsp-proxy--process-locations (locations)
  "Process LOCATIONS and show xrefs, using optimized batch processing."
  (if (seq-empty-p locations)
      (lsp-proxy--error "Not found for: %s" (or (thing-at-point 'symbol t) ""))
    (let ((locations-vec (if (vectorp locations) locations (vector locations))))
      ;; Group locations by file and strategy
      (let ((file-strategy-map (make-hash-table :test 'equal))
            (eager-files (make-hash-table :test 'equal))
            (optimized-files (make-hash-table :test 'equal))
            (lazy-results nil))
        
        ;; First pass: group locations by file and determine strategy
        (cl-loop for location across locations-vec
                 for uri = (plist-get location :uri)
                 for filepath = (lsp-proxy--uri-to-path uri)
                 do (let ((strategy (or (gethash filepath file-strategy-map)
                                       (let ((is-large (lsp-proxy--should-use-lazy-xref-p filepath)))
                                         (puthash filepath 
                                                  (cond
                                                   ((eq lsp-proxy-xref-optimization-strategy 'eager) 'eager)
                                                   ((eq lsp-proxy-xref-optimization-strategy 'lazy) (if is-large 'lazy 'eager))
                                                   ((eq lsp-proxy-xref-optimization-strategy 'optimized) (if is-large 'optimized 'eager))
                                                   (t 'eager))
                                                  file-strategy-map)))))
                      (pcase strategy
                        ('eager 
                         (let ((current-list (gethash filepath eager-files)))
                           (puthash filepath (cons location current-list) eager-files)))
                        ('optimized 
                         (let ((current-list (gethash filepath optimized-files)))
                           (puthash filepath (cons location current-list) optimized-files)))
                        ('lazy
                         (push (lsp-proxy--process-location-lazy location) lazy-results)))))
        
        ;; Process each strategy group in batch
        (let ((all-results lazy-results))
          ;; Process eager files (original method, but batched)
          (when (> (hash-table-count eager-files) 0)
            (maphash (lambda (filepath file-locations)
                      (dolist (location (nreverse file-locations))
                        (when-let ((result (lsp-proxy--process-location-eager location)))
                          (push result all-results))))
                    eager-files))
          
          ;; Process optimized files (batch optimized method)
          (when (> (hash-table-count optimized-files) 0)
            ;; Reverse the lists in optimized-files for correct order
            (let ((corrected-optimized-files (make-hash-table :test 'equal)))
              (maphash (lambda (filepath file-locations)
                        (puthash filepath (nreverse file-locations) corrected-optimized-files))
                      optimized-files)
              (setq all-results (append (lsp-proxy--batch-process-locations-optimized corrected-optimized-files) all-results))))
          
          (when all-results
            (lsp-proxy-show-xrefs (delq nil all-results) nil nil)))))))

(defun lsp-proxy-find-definition ()
  "Find definition."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/definition
   (lsp-proxy--request-or-notify-params (eglot--TextDocumentPositionParams))
   :success-fn #'lsp-proxy--process-locations))

(defun lsp-proxy-find-references ()
  "Find references."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/references
   (lsp-proxy--request-or-notify-params
    (append (eglot--TextDocumentPositionParams) `(:context (:includeDeclaration t))))
   :success-fn #'lsp-proxy--process-locations))

(defun lsp-proxy-find-declaration ()
  "Find declaration."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/declaration
   (lsp-proxy--request-or-notify-params (eglot--TextDocumentPositionParams))
   :success-fn #'lsp-proxy--process-locations))

(defun lsp-proxy-find-type-definition ()
  "Find type definition."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/typeDefinition
   (lsp-proxy--request-or-notify-params (eglot--TextDocumentPositionParams))
   :success-fn #'lsp-proxy--process-locations))

(defun lsp-proxy-find-implementations ()
  "Find definition."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/implementation
   (lsp-proxy--request-or-notify-params (eglot--TextDocumentPositionParams))
   :success-fn #'lsp-proxy--process-locations))

;;
;; hover
;;
(define-derived-mode lsp-proxy-help-mode help-mode "LspProxyHelp"
  "Major mode for displaying lsp help.")

(defun lsp-proxy-describe-thing-at-point ()
  "Display the type signature and documentation of the thing at point."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/hover
   (lsp-proxy--request-or-notify-params (eglot--TextDocumentPositionParams))
   :success-fn (lambda (hover-help)
                 (if (and hover-help (not (equal hover-help "")))
                     (with-current-buffer (get-buffer-create lsp-proxy-hover-buffer)
                       (let ((delay-mode-hooks t))
                         (lsp-proxy-help-mode)
                         (with-help-window lsp-proxy-hover-buffer
                           (insert (eglot--format-markup hover-help))))
                       (run-mode-hooks))
                   (lsp-proxy--info "%s" "No content at point.")))))

;;
;; symbol highlight
;;
(defvar lsp-proxy--highlights nil "Overlays for textDocument/documentHighlight.")
(defvar-local lsp-proxy--symbol-bounds-of-last-highlight-invocation nil
  "The bounds of the symbol from which `lsp-proxy-hover-eldoc-function'
  most recently requested highlights.")

(defun lsp-proxy--point-on-highlight? ()
  (-some? (lambda (overlay)
            (overlay-get overlay 'lsp-proxy-highlight))
          (overlays-at (point))))

(defun lsp-proxy--cleanup-highlights-if-needed ()
  (when (and lsp-proxy--highlights
             (not (lsp-proxy--point-on-highlight?)))
    (mapc #'delete-overlay lsp-proxy--highlights)
    (setq lsp-proxy--highlights nil)))

(defun lsp-proxy-hover-eldoc-function (_cb)
  "A member of `eldoc-documentation-function', for hover."
  (when (and lsp-proxy--support-document-highlight lsp-proxy--enable-symbol-highlighting (not (lsp-proxy--progressing-p (lsp-proxy-project-root))))
    (let ((buf (current-buffer))
          (curr-sym-bounds (bounds-of-thing-at-point 'symbol)))
      (unless (or (looking-at-p "[[:space:]\n]")
                  (and curr-sym-bounds
                       (equal curr-sym-bounds
                              lsp-proxy--symbol-bounds-of-last-highlight-invocation)))
        (setq lsp-proxy--symbol-bounds-of-last-highlight-invocation curr-sym-bounds)
        (lsp-proxy--async-request
         'textDocument/documentHighlight
         (lsp-proxy--request-or-notify-params (eglot--TextDocumentPositionParams))
         :success-fn
         (lambda (highlights)
           (mapc #'delete-overlay lsp-proxy--highlights)
           (let ((wins-visible-pos (-map (lambda (win)
                                           (cons (1- (line-number-at-pos (window-start win) t))
                                                 (1+ (line-number-at-pos (min (window-end win)
                                                                              (with-current-buffer (window-buffer win)
                                                                                (buffer-end +1)))
                                                                         t))))
                                         (get-buffer-window-list nil nil 'visible))))
             (setq lsp-proxy--highlights
                   (eglot--when-buffer-window buf
                     (cl-loop for highlight across highlights
                              for range = (plist-get highlight :range)
                              for start = (plist-get range :start)
                              for start-line = (plist-get start :line)
                              for end = (plist-get range :end)
                              for end-line = (plist-get end :line)
                              when (cl-loop for (start-win . end-win) in wins-visible-pos
                                            thereis (and (> (1+ start-line) start-win)
                                                         (< (1+ end-line) end-win)))
                              collect
                              (pcase-let ((`(,beg . ,end)
                                           (eglot-range-region range)))
                                (let ((ov (make-overlay beg end)))
                                  (overlay-put ov 'face 'lsp-proxy-highlight-symbol-face)
                                  (overlay-put ov 'modification-hooks
                                               `(,(lambda (o &rest _) (delete-overlay o))))
                                  (overlay-put ov 'lsp-proxy-highlight t)
                                  ov))))))
           ;; (message "Filter length %s" (length lsp-proxy--highlights))
           )
         :deferred 'textDocument/documentHighlight))
      nil)
    t))

;;
;; format
;;
(defun lsp-proxy--get-indent-width (mode)
  "Get indentation offset for MODE."
  (or (alist-get mode lsp-proxy--formatting-indent-alist)
      (lsp-proxy--get-indent-width (or (get mode 'derived-mode-parent) 'default))))

(defun lsp-proxy-format-buffer ()
  "Ask the server to format this document."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/formatting
   (lsp-proxy--request-or-notify-params
    (list
     :options (list
               :tabSize (symbol-value (lsp-proxy--get-indent-width major-mode))
               :insertSpaces (not indent-tabs-mode)
               :trimTrailingWhitespace lsp-proxy-trim-trailing-whitespace
               :insertFinalNewline lsp-proxy-insert-final-newline
               :trimFinalNewlinesmm lsp-proxy-trim-final-newlines)
     :textDocument (eglot--TextDocumentIdentifier)))
   :success-fn (lambda (edits)
                 (if (and edits (> (length edits) 0))
                     (progn
                       (eglot--apply-text-edits edits)
                       (save-buffer))
                   (lsp-proxy--info "%s" "No formatting changes provided")))))

;;
;; completion
;;
(defun lsp-proxy-passthrough-all-completions (_string table pred _point)
  "Like `completion-basic-all-completions' but have prefix ignored.
TABLE PRED"
  (completion-basic-all-completions "" table pred 0))

(defun lsp-proxy--dumb-tryc (pat table pred point)
  "Like `completion-basic-try-completion' but passthrough all completion.
Without common substring required. PAT TABLE PRED POINT."
  (let ((probe (funcall table pat pred nil)))
    (cond ((eq probe t) t)
          (probe (cons probe (length probe)))
          (t (cons pat point)))))


(defvar-local lsp-proxy--last-inserted-char nil
  "If non-nil, value of the last inserted character in buffer.")

(defun lsp-proxy--post-self-insert-hook ()
  "Set `lsp-proxy--last-inserted-char'."
  (setq lsp-proxy--last-inserted-char last-input-event))

(defun lsp-proxy--pre-command-hook ()
  "Rest some temporary variables."
  (setq lsp-proxy--last-inserted-char nil))

(defun lsp-proxy--get-english-dash-string-boundaries ()
  "Return the boundaries of the English and dash string before point.
Or nil if none."
  (save-excursion
    (let ((end (point))
          (start (re-search-backward "[^a-zA-Z0-9-]" nil t)))
      (if start
          (progn
            (forward-char)
            (setq start (point)))
        (setq start (point-min)))
      (goto-char end)
      (if (looking-back "[a-zA-Z0-9]+-[a-zA-Z0-9-]*" start)
          (cons start end)
        nil))))

(defun lsp-proxy-completion-at-point ()
  "Get lsp completions."
  ;; (when (not (nth 4 (syntax-ppss)))
  (let* ((trigger-characters lsp-proxy--completion-trigger-characters)
         (bounds-start (if-let* ((bounds (lsp-proxy--get-english-dash-string-boundaries)))
                           (cl-first bounds)
                         (or (cl-first (bounds-of-thing-at-point 'symbol))
                             (point))))
         (candidates
          (lambda ()
            (let* ((prefix (buffer-substring-no-properties bounds-start (point)))
                   (resp (lsp-proxy--request
                          'textDocument/completion
                          (lsp-proxy--request-or-notify-params
                           (eglot--TextDocumentPositionParams)
                           `(:context
                             (:line ,(buffer-substring-no-properties (line-beginning-position) (line-end-position))
                              :prefix ,prefix
                              :boundsStart ,bounds-start
                              :startPoint ,(point)
                              :triggerKind ,(if (null lsp-proxy--last-inserted-char) 1 2)))) ;; 只用来区分是否是空字符触发的，如果是空认为是主动触发，否则就是自动触发
                          :cancel-on-input t))
                   (items (mapcar (lambda (candidate)
                                    (let* ((item (plist-get candidate :item))
                                           (label (plist-get item :label)))
                                      (propertize label 'lsp-proxy--item candidate)))
                                  resp)))
              items))))
    (list
     bounds-start
     (point)
     (lambda (probe pred action)
       (cond
        ((eq action 'metadata)
         '(metadata (category . lsp-proxy-capf)
           (display-sort-function . identity)
           (cycle-sort-function . identity)))
        ((eq (car-safe action) 'boundaries) nil)
        (t
         (complete-with-action action (funcall candidates) probe pred))))
     :annotation-function #'lsp-proxy--annotate
     :company-kind #'lsp-proxy--candidate-kind
     :company-require-match 'nerver
     :company-prefix-length
     (save-excursion
       (and (lsp-proxy--looking-back-trigger-characterp trigger-characters) t))
     :company-doc-buffer #'lsp-proxy--doc-buffer
     :exit-function #'lsp-proxy--company-post-completion)))

(defun lsp-proxy--looking-back-trigger-characterp (trigger-characters)
  "Return character if text before point match any of the TRIGGER-CHARACTERS."
  (unless (= (point) (line-beginning-position))
    (cl-some
     (lambda (trigger-char)
       (and (equal (buffer-substring-no-properties (- (point) (length trigger-char)) (point))
                   trigger-char)
            trigger-char))
     trigger-characters)))

(defun lsp-proxy--company-post-completion (candidate status)
  "Replace a CompletionItem's label with its insertText.
Apply text edits in CANDIDATE when STATUS is finished or exact."
  (when (memq status '(finished exact))
    (let* ((proxy-item (get-text-property 0 'lsp-proxy--item candidate))
           (resolved-item (get-text-property 0 'resolved-item candidate))
           (language-server-name (plist-get proxy-item :language_server_name))
           (marker (copy-marker (point) t)))
      (unless proxy-item
        (message "no lsp-proxy--item in post-completion %s" proxy-item))
      (if (equal language-server-name "typescript-language-server")
          (if resolved-item
              (lsp-proxy--company-post-completion-item resolved-item candidate marker)
            (let ((resolved (lsp-proxy--sync-resolve proxy-item)))
              (put-text-property 0 (length candidate) 'resolved-item resolved candidate)
              (lsp-proxy--company-post-completion-item (or resolved proxy-item) candidate marker)))
        (lsp-proxy--company-post-completion-item (or resolved-item proxy-item) candidate marker)))))

(defun lsp-proxy--company-post-completion-item (proxy-item candidate marker)
  "Complete CANDIDATE of PROXY-ITEM from MARKER."
  (let* ((item (plist-get proxy-item :item))
         (label (plist-get item :label))
         (insertText (plist-get item :insertText))
         ;; 1 = plaintext, 2 = snippet
         (insertTextFormat (plist-get item :insertTextFormat))
         (textEdit (plist-get item :textEdit))
         (additionalTextEdits (plist-get item :additionalTextEdits))
         (startPoint (- marker (length candidate)))
         (insertTextMode (plist-get item :insertTextMode))
         (start (plist-get proxy-item :start))
         (end (plist-get proxy-item :end)))
    (cond (textEdit
           (let* ((range (plist-get textEdit :range))
                  (replaceStart (eglot--lsp-position-to-point (plist-get range :start)))
                  (replaceEnd (eglot--lsp-position-to-point (plist-get range :end)))
                  (newText (plist-get textEdit :newText))
                  (insertText (s-replace "\r" "" (or newText ""))))
             (delete-region start end)
             (delete-region replaceStart replaceEnd)
             (insert insertText)))
          ;; (snippet-fn
          ;; A snippet should be inserted, but using plain
          ;; `insertText'.  This requires us to delete the
          ;; whole completion, since `insertText' is the full
          ;; completion's text.
          (insertText
           (delete-region (- end (length candidate)) end)
           (insert (or insertText label))))
    (lsp-proxy--indent-lines startPoint (point) insertTextMode)
    (when (eq insertTextFormat 2)
      (lsp-proxy--expand-snippet (buffer-substring startPoint (point))
                                 startPoint
                                 (point)))
    (if (cl-plusp (length additionalTextEdits))
        (eglot--apply-text-edits additionalTextEdits)
      (if-let* ((resolved-item (get-text-property 0 'resolved-item candidate)))
          (if-let* ((additionalTextEdits (plist-get resolved-item :additionalTextEdits)))
              (eglot--apply-text-edits additionalTextEdits))
        (-let [(callback cleanup-fn) (lsp-proxy--create-apply-text-edits-handlers)]
          (lsp-proxy--async-resolve proxy-item callback cleanup-fn))))))

(defun lsp-proxy--candidate-kind (item)
  "Return ITEM's kind."
  (let* ((proxy-item (get-text-property 0 'lsp-proxy--item item))
         (completion-item (plist-get proxy-item :item))
         (kind (and completion-item (plist-get completion-item :kind))))
    (alist-get kind eglot--kind-names)))

(defun lsp-proxy--annotate (item)
  "Annotate ITEM detail."
  (let* ((proxy-item (get-text-property 0 'lsp-proxy--item item))
         (completion-item (plist-get proxy-item :item))
         (kind (and completion-item (plist-get completion-item :kind)))
         (detail (and completion-item (plist-get completion-item :detail)))
         (label-detail (and completion-item (plist-get completion-item :labelDetails))))
    (concat
     (when detail
       (concat " " (s-replace "\r" "" detail)))
     (when-let* ((label--detail (and label-detail (plist-get label-detail :detail))))
       (format " %s" label--detail))
     (when-let* ((description (and label-detail (plist-get label-detail :description))))
       (format " %s" description))
     (when-let* ((kind-name (alist-get kind eglot--kind-names)))
       (format " (%s)" kind-name)))))

(defun lsp-proxy--doc-buffer (item)
  "Get ITEM doc."
  (when-let* ((proxy-item (get-text-property 0 'lsp-proxy--item item))
              (langauge-sever-id (plist-get proxy-item :language_server_id))
              (completion-item (plist-get proxy-item :item)))
    (let ((documentation (plist-get completion-item :documentation)))
      (unless (or documentation (get-text-property 0 'resolved-item item))
        (let* ((resolved-item (lsp-proxy--sync-resolve proxy-item))) ;; (read item) 去掉了属性？
          (put-text-property 0 (length item) 'resolved-item resolved-item item)))))
  (when-let* ((resolved-item (or (get-text-property 0 'resolved-item item) (get-text-property 0 'lsp-proxy--item item)))
              (completion-item (plist-get resolved-item :item))
              (documentation (plist-get completion-item :documentation))
              (formatted (eglot--format-markup documentation)))
    (with-current-buffer (get-buffer-create "*lsp-proxy-doc*")
      (erase-buffer)
      (insert formatted)
      (current-buffer))))

(defun lsp-proxy--sync-resolve (proxy-item)
  "Request `completionItem/resolve' of PROXY-ITEM synchronously."
  (when-let* ((language-server-id (plist-get proxy-item :language_server_id))
              (start (plist-get proxy-item :start))
              (end (plist-get proxy-item :end))
              (item (plist-get proxy-item :item)))
    (lsp-proxy--request
     'completionItem/resolve
     (lsp-proxy--request-or-notify-params
      item
      `(:context (:language-server-id ,language-server-id :start ,start :end ,end)))
     :cancel-on-input t)))

(defun lsp-proxy--async-resolve (proxy-item callback &optional cleanup-fn)
  "Resolve completion PROXY-ITEM asynchronously with CALLBACK.
The CLEANUP-FN will be called to cleanup."
  (when-let* ((language-server-id (plist-get proxy-item :language_server_id))
              (start (plist-get proxy-item :start))
              (end (plist-get proxy-item :end))
              (item (plist-get proxy-item :item)))
    (lsp-proxy--async-request
     'completionItem/resolve
     (lsp-proxy--request-or-notify-params item `(:context (:language-server-id ,language-server-id :start ,start :end ,end)))
     :success-fn (lambda (resolved-item)
                   (if-let* ((complete-item (plist-get resolved-item :item))
                             (additionalTextEdits (plist-get complete-item :additionalTextEdits)))
                       (funcall callback additionalTextEdits))
                   (when cleanup-fn (funcall cleanup-fn))))
    :error-fn cleanup-fn
    :timeout-fn cleanup-fn))

;;;
;;; inline completion
;;;
(defcustom lsp-proxy-inline-completion-enable-predicates '(evil-insert-state-p)
  "A list of predicate functions with no argument to enable inlineCompletion.
InlineCompletion will be triggered only if all predicates return t."
  :type '(repeat function)
  :group 'lsp-proxy)

(defcustom lsp-proxy-inline-completion-disable-predicates nil
  "A list of predicate functions with no argument to disable inlineCompletion.
InlineCompletion will not be triggered if any predicate returns t."
  :type '(repeat function)
  :group 'lsp-proxy)

(defface lsp-proxy-inline-completion-overlay-face
  '((t :inherit shadow))
  "Face for displaying inline completion."
  :group 'lsp-proxy)

(defvar-local lsp-proxy-inline-completion--items nil "The completions provided by the server.")
(defvar-local lsp-proxy-inline-completion--current nil "The current suggestion to be displayed.")
(defvar-local lsp-proxy-inline-completion--overlay nil "The overlay displaying code suggestions.")
(defvar-local lsp-proxy-inline-completion--start-point nil "The point where the completion started.")

(defmacro lsp-proxy-inline-completion--satisfy-predicates (enable disable)
  "Return t if satisfy all predicates in ENABLE and none in DISABLE."
  `(and (cl-every (lambda (pred)
                    (if (functionp pred) (funcall pred) t))
                  ,enable)
        (cl-notany (lambda (pred)
                     (if (functionp pred) (funcall pred) nil))
                   ,disable)))

(defun lsp-proxy-inline-completion--satisfy-trigger-predicates ()
  "Return t if all trigger predicates are satisfied."
  (lsp-proxy-inline-completion--satisfy-predicates
   lsp-proxy-inline-completion-enable-predicates
   lsp-proxy-inline-completion-disable-predicates))

(defvar lsp-proxy-inline-completion-active-map
  (let ((map (make-sparse-keymap)))
    ;; accpet
    (define-key map (kbd "C-<return>") #'lsp-proxy-inline-completion-accept)
    (define-key map (kbd "<tab>") #'lsp-proxy-inline-completion-accept)
    (define-key map (kbd "TAB") #'lsp-proxy-inline-completion-accept)
    (define-key map (kbd "C-<tab>") #'lsp-proxy-inline-completion-accept-by-word)
    (define-key map (kbd "C-TAB") #'lsp-proxy-inline-completion-accept-by-word)
    (define-key map (kbd "<backtab>") #'lsp-proxy-inline-completion-accept-by-line)
    ;; navigate
    (define-key map (kbd "C-n") #'lsp-proxy-inline-completion-next)
    (define-key map (kbd "C-p") #'lsp-proxy-inline-completion-prev)
    ;; useful -- recenter without loosing the completion
    (define-key map (kbd "C-l") #'recenter-top-bottom)
    ;; ignore
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [up-mouse-1] #'ignore)
    (define-key map [mouse-movement] #'ignore)
    map)
  "Keymap active when showing inline code suggestions.")

(defvar-local lsp-proxy-inline-completion--keymap-overlay nil
  "Overlay used to surround point.
Make lsp-proxy-inline-completion-active-map activate.")

(defsubst lsp-proxy-inline-completion--overlay-visible ()
  "Return whether the `overlay' is avaiable."
  (and (overlayp lsp-proxy-inline-completion--overlay)
       (overlay-buffer lsp-proxy-inline-completion--overlay)))


(defun lsp-proxy-inline-completion--clear-overlay ()
  "Clear inline completion suggestion overlay."
  (when (lsp-proxy-inline-completion--overlay-visible)
    (delete-overlay lsp-proxy-inline-completion--keymap-overlay)
    (delete-overlay lsp-proxy-inline-completion--overlay))
  (setq-local lsp-proxy-inline-completion--real-posn nil))

(defun lsp-proxy-inline-completion--get-or-create-keymap-overlay ()
  "Make or return the local lsp-proxy-inline-completion--keymap-overlay."
  (unless (overlayp lsp-proxy-inline-completion--keymap-overlay)
    (setq lsp-proxy-inline-completion--keymap-overlay (make-overlay 1 1 nil nil t))
    (overlay-put lsp-proxy-inline-completion--keymap-overlay 'keymap lsp-proxy-inline-completion-active-map)
    (overlay-put lsp-proxy-inline-completion--keymap-overlay 'priority 101))
  lsp-proxy-inline-completion--keymap-overlay)

(defun lsp-proxy-inline-completion--get-overlay ()
  "Build the suggestion overlay with BEG and END."
  (unless (overlayp lsp-proxy-inline-completion--overlay)
    (setq lsp-proxy-inline-completion--overlay (make-overlay 1 1 nil nil t))
    (overlay-put lsp-proxy-inline-completion--overlay 'keymap-overlay (lsp-proxy-inline-completion--get-or-create-keymap-overlay)))
  lsp-proxy-inline-completion--overlay)

(defun lsp-proxy-inline-completion--self-insert (command)
  "Handle the case where the char just inserted is the start of the completion.
If so, update the overlays and continue. COMMAND is the command that triggered
in `post-command-hook'."
  (when (and (eq command 'self-insert-command)
             (lsp-proxy-inline-completion--overlay-visible))
    (let* ((ov lsp-proxy-inline-completion--overlay)
           (completion (overlay-get ov 'completion))
           (completion-end (overlay-get lsp-proxy-inline-completion--overlay 'completion-end))
           (completion-start (overlay-get lsp-proxy-inline-completion--overlay 'completion-start))
           ;; the last point before self-insert
           (prev-point (max (point-min) (1- (point)))))
      (when (and (eq last-command-event (elt completion 0)) (not (memq last-command-event lsp-proxy-inline-completion-trigger-characters)))
        (if (= (length completion) 1)
            ;; If there is only one char in the completion, accept it
            (progn
              (overlay-put ov 'completion (substring completion 1))
              (lsp-proxy-inline-completion-accept))
          (when (and (> completion-end prev-point) (>= completion-start prev-point))
            (message "Wrong~~")
            (overlay-put ov 'completion-start (+ completion-start 1))
            (overlay-put ov 'completion-end (+ completion-end 1)))
          (lsp-proxy-inline-completion--set-overlay-text ov (substring completion 1)))))))

(defun lsp-proxy-inline-completion--post-command-debounce (buffer)
  (when (and lsp-proxy-inline-completion-mode
             (buffer-live-p buffer)
             (equal buffer (current-buffer))
             (lsp-proxy-inline-completion--satisfy-trigger-predicates))
    (lsp-proxy-inline-completion-display 'implicit)))

(defun lsp-proxy-inline-completion-display (&optional implicit)
  "Display the inline completions overlay."
  (interactive)
  (condition-case err
      (lsp-proxy--async-request
       'textDocument/inlineCompletion
       (lsp-proxy--request-or-notify-params
        (append (eglot--TextDocumentPositionParams)
                `(:context (:triggerKind ,(if implicit 2 1))))
        `(:context
          (:triggerKind ,(if implicit 2 1)
           :selectedCompletionInfo nil
           :line ,(buffer-substring-no-properties (line-beginning-position) (line-end-position))
           :docVersion ,eglot--versioned-identifier)))
       :success-fn
       (lambda (resp)
         (when resp
           (let* ((doc-version (plist-get resp :docVersion))
                  (items (plist-get resp :items)))
             (if (= doc-version eglot--versioned-identifier)
                 (when (and items (> (length items) 0))
                   (setq lsp-proxy-inline-completion--items items)
                   (setq lsp-proxy-inline-completion--current 0)
                   (setq lsp-proxy-inline-completion--start-point (point))
                   (lsp-proxy-inline-completion-show-completion))
               (lsp-proxy--warn "%s" "Mismatch version.")))))
       :timeout-fn #'ignore)
    (t (lsp-proxy--error "Could not fetch completions: %s" err))))

(defun lsp-proxy-inline-completion--string-common-prefix (str1 str2)
  "Find the common prefix of STR1 and STR2 directly."
  (let ((min-len (min (length str1) (length str2)))
        (i 0))
    (while (and (< i min-len)
                (= (aref str1 i) (aref str2 i)))
      (setq i (1+ i)))
    (substring str1 0 i)))

(defun lsp-proxy-inline-completion-show-completion ()
  "Make the suggestion overlay visible."
  (unless (and lsp-proxy-inline-completion--items
               (> (length lsp-proxy-inline-completion--items) 0)
               (numberp lsp-proxy-inline-completion--current))
    (error "No completions to show"))
  (save-excursion
    (save-restriction
      (widen)
      (-let* ((p (point))
              (suggestion
               (elt lsp-proxy-inline-completion--items
                    lsp-proxy-inline-completion--current))
              (insert-text (plist-get suggestion :insertText))
              (line (map-nested-elt suggestion '(:range :start :line)))
              (start-char (map-nested-elt suggestion '(:range :start :character)))
              (end-char (map-nested-elt suggestion '(:range :end :character)))
              (goto-line! (lambda ()
                            (goto-char (point-min))
                            (forward-line line)))
              (start (progn
                       (funcall goto-line!)
                       (forward-char start-char)
                       (let* ((cur-line (buffer-substring-no-properties (point) (line-end-position)))
                              (common-prefix-len (length (lsp-proxy-inline-completion--string-common-prefix insert-text cur-line))))
                         ;; (message "common-prefix-len %s" common-prefix-len)
                         ;; (message "%s" suggestion)
                         (setq insert-text (substring insert-text common-prefix-len))
                         (forward-char common-prefix-len)
                         (point))))
              (end (progn
                     (funcall goto-line!)
                     (forward-char end-char)
                     (point))))
        (goto-char p)
        (lsp-proxy-inline-completion--display-overlay-completion insert-text start end)))))

(defun lsp-proxy-inline-completion--display-overlay-completion (completion start end)
  "Show COMPLETION between START and END."
  (lsp-proxy-inline-completion--clear-overlay)
  (when (and (not (string-blank-p completion))
             (or (<= start (point))))
    (let* ((ov (lsp-proxy-inline-completion--get-overlay)))
      (overlay-put ov 'tail-length (- (line-end-position) end))
      (overlay-put ov 'completion-start start)
      (overlay-put ov 'completion-end end)
      (lsp-proxy-inline-completion--set-overlay-text ov completion))))


(defun lsp-proxy-inline-completion--overlay-end (ov)
  "Return the end position of overlay OV."
  (- (line-end-position) (overlay-get ov 'tail-length)))

(defvar-local lsp-proxy-inline-completion--real-posn nil
  "Posn information without overlay.
To work around posn problems with after-string property.")


(defun lsp-proxy-inline-completion--set-overlay-text (ov completion)
  "Set overlay OV with COMPLETION."
  (move-overlay ov (point) (line-end-position))

  ;; set overlay position for the keymap, to activate lsp-proxy-inline-completion-active-map
  ;;
  ;; if the point is at the end of the buffer, we will create a
  ;; 0-length buffer. But this is ok, since the keymap will still
  ;; activate _so long_ as no other overlay contains the point.
  ;;
  ;; see https://github.com/copilot-emacs/copilot.el/issues/251 for details.
  (move-overlay (overlay-get ov 'keymap-overlay) (point) (min (point-max) (+ 1 (point))))
  (let* ((tail (buffer-substring (lsp-proxy-inline-completion--overlay-end ov) (line-end-position)))
         (p-completion (concat (propertize completion 'face 'lsp-proxy-inline-completion-overlay-face)
                               tail)))
    (if (eolp)
        (progn
          (overlay-put ov 'after-string "") ; make sure posn is correct
          (setq lsp-proxy-inline-completion--real-posn (cons (point) (posn-at-point)))
          (put-text-property 0 1 'cursor t p-completion)
          (overlay-put ov 'display "")
          (overlay-put ov 'after-string p-completion))
      (overlay-put ov 'display (substring p-completion 0 1))
      (overlay-put ov 'after-string (substring p-completion 1)))
    (overlay-put ov 'completion completion)
    (overlay-put ov 'start (point))))

(defun lsp-proxy-inline-completion--show-keys ()
  "Show active keymap hints in the minibuffer."
  (unless (and lsp-proxy-inline-completion--items
               (numberp lsp-proxy-inline-completion--current))
    (error "No completions to show"))
  (let ((message-log-max nil))
    (message (concat "Completion "
                     (propertize (format "%d" (1+ lsp-proxy-inline-completion--current)) 'face 'bold)
                     "/"
                     (propertize (format "%d" (length lsp-proxy-inline-completion--items)) 'face 'bold)
                     (if (> (length lsp-proxy-inline-completion--items) 1)
                         (-when-let (keys (where-is-internal #'lsp-proxy-inline-completion-next lsp-proxy-inline-completion-active-map))
                           (concat ". "
                                   (propertize " Next" 'face 'italic)
                                   (format ": [%s]"
                                           (string-join (--map (propertize (key-description it) 'face 'help-key-binding)
                                                               keys)
                                                        "/")))))
                     (-when-let (keys (where-is-internal #'lsp-proxy-inline-completion-accept lsp-proxy-inline-completion-active-map))
                       (concat (propertize " Accept" 'face 'italic)
                               (format ": [%s]"
                                       (string-join (--map (propertize (key-description it) 'face 'help-key-binding)
                                                           keys)
                                                    "/"))))))))

(defun lsp-proxy-inline-completion-next ()
  "Display the next inline completion."
  (interactive)
  (unless (lsp-proxy-inline-completion--overlay-visible)
    (error "Not showing suggestions"))
  (setq lsp-proxy-inline-completion--current
        (mod (1+ lsp-proxy-inline-completion--current)
             (length lsp-proxy-inline-completion--items)))

  (lsp-proxy-inline-completion-show-completion))

(defun lsp-proxy-inline-completion-prev ()
  "Display the previous inline completion."
  (interactive)
  (unless (lsp-proxy-inline-completion--overlay-visible)
    (error "Not showing suggestions"))
  (setq lsp-proxy-inline-completion--current
        (mod (1- lsp-proxy-inline-completion--current)
             (length lsp-proxy-inline-completion--items)))

  (lsp-proxy-inline-completion-show-completion))

(defun lsp-proxy-inline-completion-cancel ()
  "Cancel current inline completion, if any."
  (interactive)
  (when (lsp-proxy-inline-completion--overlay-visible)
    (lsp-proxy-inline-completion--clear-overlay)

    (when lsp-proxy-inline-completion--start-point
      (goto-char lsp-proxy-inline-completion--start-point))))

(defun lsp-proxy-inline-completion-accept (&optional transform-fn)
  "Accept the current suggestion.
Return t if there is a completion. Use TRANSFORM-FN to transform completion if
provided."
  (interactive)
  (unless (lsp-proxy-inline-completion--overlay-visible)
    (error "Not showing suggestions"))
  (-let* (
          ;; (suggestion (elt lsp-proxy-inline-completion--items lsp-proxy-inline-completion--current))
          (insert-text (overlay-get lsp-proxy-inline-completion--overlay 'completion))
          (t-insert-text (funcall (or transform-fn #'identity) insert-text))
          ;; (start (overlay-get lsp-proxy-inline-completion--overlay 'start))
          ;; (end (lsp-proxy-inline-completion--overlay-end lsp-proxy-inline-completion--overlay))
          (completion-start (overlay-get lsp-proxy-inline-completion--overlay 'completion-start))
          (completion-end (overlay-get lsp-proxy-inline-completion--overlay 'completion-end)))
    (delete-region completion-start completion-end)
    (insert t-insert-text)
    (lsp-proxy-inline-completion--clear-overlay)
    ;; if it is a partial completion
    (when (and (string-prefix-p t-insert-text insert-text)
               (not (string-equal t-insert-text insert-text)))
      (lsp-proxy-inline-completion--set-overlay-text (lsp-proxy-inline-completion--get-overlay) (string-remove-prefix t-insert-text insert-text)))
    t))

(defmacro lsp-proxy-inline-completion--define-accept-completion-by-action (func-name action)
  "Define function FUNC-NAME to accept completion by ACTION."
  `(defun ,func-name (&optional n)
     (interactive "p")
     (setq n (or n 1))
     (lsp-proxy-inline-completion-accept (lambda (completion)
                                  (with-temp-buffer
                                    (insert completion)
                                    (goto-char (point-min))
                                    (funcall ,action n)
                                    (buffer-substring-no-properties (point-min) (point)))))))

(lsp-proxy-inline-completion--define-accept-completion-by-action lsp-proxy-inline-completion-accept-by-word #'forward-word)
(lsp-proxy-inline-completion--define-accept-completion-by-action lsp-proxy-inline-completion-accept-by-line #'forward-line)

;; (defun lsp-proxy-inline-completion--insert-suggestion (text kind start end command?)
;;   (let* ((text-insert-start (or start lsp-proxy-inline-completion--start-point))
;;          text-insert-end)
;;     (when text-insert-start
;;       (goto-char text-insert-start))

;;     ;; When range is provided, must replace the text of the range by the text
;;     ;; to insert
;;     (when (and start end (/= start end))
;;       (delete-region start end))

;;     ;; Insert suggestion, keeping the cursor at the start point
;;     (insert text)

;;     (setq text-insert-end (point))

;;     ;; If a template, format it -- keep track of the end position!
;;     (when (eq kind 'snippet)
;;       (let ((end-marker (set-marker (make-marker) (point))))
;;         (lsp-proxy--expand-snippet (buffer-substring text-insert-start text-insert-end)
;;                                    text-insert-start
;;                                    text-insert-end)
;;         (setq text-insert-end (marker-position end-marker))
;;         (set-marker end-marker nil)))

;;     ;; Post command
;;     (when command?
;;       (lsp-proxy--execute-command command? nil))

;;     ;; hooks
;;     ;; (run-hook-with-args 'lsp-inline-completion-accepted-functions text text-insert-start text-insert-end)
;;     ))

(defun lsp-proxy-inline-completion--posn-advice (&rest args)
  "Remap posn if in lsp-proxy-inline-completion-mode with ARGS."
  (when lsp-proxy-inline-completion-mode
    (let ((pos (or (car-safe args) (point))))
      (when (and lsp-proxy-inline-completion--real-posn
                 (eq pos (car lsp-proxy-inline-completion--real-posn)))
        (cdr lsp-proxy-inline-completion--real-posn)))))

;;;###autoload
(define-minor-mode lsp-proxy-inline-completion-mode
  "Auto inline complete mode."
  :init-value nil
  :lighter "CP"
  (lsp-proxy-inline-completion--clear-overlay)
  (advice-add 'posn-at-point :before-until #'lsp-proxy-inline-completion--posn-advice)
  (unless lsp-proxy-inline-completion-mode
    (lsp-proxy-inline-completion-cancel)))

;;
;; Signature
;;
(defun lsp-proxy-signature-eldoc-function (cb)
  "A member of `eldoc-documentation-functions', for signatures."
  (when lsp-proxy--support-signature-help
    (let ((buf (current-buffer)))
      (lsp-proxy--async-request
       'textDocument/signatureHelp
       (lsp-proxy--request-or-notify-params
        (eglot--TextDocumentPositionParams))
       :success-fn
       (eglot--lambda ((SignatureHelp)
                       signatures activeSignature (activeParameter 0))
         (eglot--when-buffer-window buf
           (let ((active-sig (and (cl-plusp (length signatures))
                                  (aref signatures (or activeSignature 0)))))
             (if (not active-sig) (funcall cb nil)
               (funcall
                cb (mapconcat (lambda (s)
                                (eglot--sig-info s (and (eq s active-sig)
                                                        activeParameter)
                                                 nil))
                              signatures "\n")
                :echo (eglot--sig-info active-sig activeParameter t))))))
       :deferred :textDocument/signatureHelp))
    t))

;;
;; rename
;;
(unless (fboundp 'eglot--format)
  (defun eglot--format (format &rest args)
    "Like `format`, but substitutes quotes."
    (apply #'format (if (functionp 'substitute-quotes)
                        (substitute-quotes format)
                      format)
           args)))

(defun lsp-proxy-rename (newname)
  "Rename the symbol (and all references to it) under point to NEWNAME."
  ;; (interactive (list (lsp-proxy--read-rename (lsp-proxy--get-symbol-to-rename))))
  (interactive
   (list (read-from-minibuffer
          (eglot--format "Rename `%s' to: "
                         (or (thing-at-point 'symbol t)
                             "unknown symbol"))
          nil nil nil nil
          (symbol-name (symbol-at-point)))))
  (lsp-proxy--async-request
   'textDocument/rename
   (lsp-proxy--request-or-notify-params
    (append (eglot--TextDocumentPositionParams) `(:newName ,newname)))
   :success-fn (lambda (edits)
                 (if edits
                     (eglot--apply-workspace-edit edits this-command)
                   (lsp-proxy--warn "%s" "Server does not support rename.")))))

;;
;; PullDiagnostic
;;
(defun lsp-proxy-diagnostics--request-pull-diagnostics ()
  (when lsp-proxy--support-pull-diagnostic
    (lsp-proxy--async-request
     'textDocument/diagnostic
     (lsp-proxy--request-or-notify-params
      (list :textDocument (eglot--TextDocumentIdentifier)))
     :timeout-fn #'ignore)))


;;
;; Flycheck
;;
(defun lsp-proxy-diagnostics--flycheck-buffer ()
  "Trigger flycheck on buffer."
  (remove-hook 'lsp-proxy-on-idle-hook #'lsp-proxy-diagnostics--flycheck-buffer t)
  (when (bound-and-true-p flycheck-mode)
    (flycheck-buffer)))

(defun lsp-proxy-diagnostics--flycheck-start (checker callback)
  "Start an LSP syntax check with CHECKER.
CALLBACK is the status callback passed by Flycheck."
  (remove-hook 'lsp-proxy-on-idle-hook #'lsp-proxy-diagnostics--flycheck-buffer t)
  (let* ((workspace-diagnostics (lsp-proxy--get-or-create-project (lsp-proxy-project-root) lsp-proxy--diagnostics-map))
         (buffer-diagnostics (gethash (lsp-proxy--fix-path-casing buffer-file-name) workspace-diagnostics '()))
         (errors (mapcar
                  (lambda (diagnostic)
                    (let* ((range (plist-get diagnostic :range))
                           (start (plist-get range :start))
                           (end (plist-get range :end)))
                      (flycheck-error-new
                       :buffer (current-buffer)
                       :checker checker
                       :filename (buffer-file-name)
                       :message (plist-get diagnostic :message)
                       :level (pcase (plist-get diagnostic :severity)
                                (1 'error)
                                (2 'warning)
                                (3 'info)
                                (4 'info)
                                (_ 'error))
                       :id (plist-get diagnostic :code)
                       :group (plist-get diagnostic :source)
                       :line (1+ (plist-get start :line))
                       :column (1+ (plist-get start :character))
                       :end-line (1+ (plist-get end :line))
                       :end-column (1+ (plist-get end :character)))))
                  buffer-diagnostics)))
    (funcall callback 'finished errors)))

;;;###autoload
(defun lsp-proxy-diagnostics-lsp-proxy-checker-if-needed ()
  "Create a `lsp-proxy' checker of flycheck."
  (unless (flycheck-valid-checker-p 'lsp-proxy)
    (flycheck-define-generic-checker 'lsp-proxy
      "A syntax checker using the langauge server protocol provided by lsp-proxy."
      :start #'lsp-proxy-diagnostics--flycheck-start
      :modes '(lsp-proxy-placeholder-mode)
      :predicate (lambda () lsp-proxy-mode))))

(defun lsp-proxy-diagnostics-flycheck-enable (&rest _)
  "Enable flycheck integration for the current buffer."
  (require 'flycheck)
  (lsp-proxy-diagnostics-lsp-proxy-checker-if-needed)
  (unless lsp-proxy-diagnostics--flycheck-enabled
    (setq-local lsp-proxy-diagnostics--flycheck-enabled t)
    (add-to-list 'flycheck-checkers 'lsp-proxy)
    (unless (flycheck-checker-supports-major-mode-p 'lsp-proxy major-mode)
      (flycheck-add-mode 'lsp-proxy major-mode)))
  (flycheck-mode 1))

(defun lsp-proxy-diagnostics-flycheck-disable (&rest _)
  "Disable flycheck integartion for the current buffer."
  (when lsp-proxy-diagnostics--flycheck-enabled
    (setq-local lsp-proxy-diagnostics--flycheck-enabled nil)))

;; Flycheck integration
(declare-function flymake-mode "ext:flymake")
(declare-function flymake-make-diagnostic "ext:flymake")
(declare-function flymake-diag-region "ext:flymake")

(defvar flymake-diagnostic-functions)
(defvar flymake-mode)
(defvar-local lsp-proxy-diagnostics--flymake-report-fn nil)

(defun lsp-proxy-diagnostics-flymake-enable ()
  "Setup flymake."
  (setq lsp-proxy-diagnostics--flymake-report-fn nil)
  (unless lsp-proxy-diagnostics--flymake-enabled
    (setq-local lsp-proxy-diagnostics--flymake-enabled t)
    (add-hook 'flymake-diagnostic-functions 'lsp-proxy-diagnostics--flymake-backend nil t))
  (flymake-mode 1))

(defun lsp-proxy-diagnostics-flymake-disable ()
  "Disable flymake integartion for the current buffer."
  (when lsp-proxy-diagnostics--flymake-enabled
    (setq-local lsp-proxy-diagnostics--flymake-enabled nil)))

(defun lsp-proxy-diagnostics--flymake-after-diagnostics ()
  "Handler for diagnostics update."
  (cond
   ((and lsp-proxy-diagnostics--flymake-report-fn flymake-mode)
    (lsp-proxy-diagnostics--flymake-update-diagnostics))
   ((not flymake-mode)
    (setq lsp-proxy-diagnostics--flymake-report-fn nil))))

(defun lsp-proxy-diagnostics--flymake-backend (report-fn &rest _args)
  "Flymake backend using REPORT-FN."
  (let ((first-run (null lsp-proxy-diagnostics--flymake-report-fn)))
    (setq lsp-proxy-diagnostics--flymake-report-fn report-fn)
    (when first-run
      (lsp-proxy-diagnostics--flymake-update-diagnostics))))

(defun lsp-proxy-diagnostics--flymake-update-diagnostics ()
  "Report new diagnostics to flymake."
  (let* ((workspace-diagnostics (lsp-proxy--get-or-create-project (lsp-proxy-project-root) lsp-proxy--diagnostics-map))
         (buffer-diagnostics (gethash (buffer-file-name) workspace-diagnostics '()))
         (diags (mapcar
                 (lambda (diagnostic)
                   (let* ((message (plist-get diagnostic :message))
                          (severity (plist-get diagnostic :severity))
                          (range (plist-get diagnostic :range))
                          (start (plist-get range :start))
                          (end (plist-get range :end))
                          (start-line (plist-get start :line))
                          (character (plist-get start :character))
                          (end-line (plist-get end :line))
                          (start-point (eglot--lsp-position-to-point start))
                          (end-point (eglot--lsp-position-to-point end)))
                     (when (= start-point end-point)
                       (if-let* ((region (flymake-diag-region (current-buffer)
                                                              (1+ start-line)
                                                              character)))
                           (setq start-point (car region)
                                 end-point (cdr region))
                         (eglot--widening
                          (goto-char (point-min))
                          (setq start-point (line-beginning-position (1+ start-line))
                                end-point (line-end-position (1+ end-line))))))
                     (flymake-make-diagnostic (current-buffer)
                                              start-point
                                              end-point
                                              (cl-case severity
                                                (1 :error)
                                                (2 :warning)
                                                (t :note))
                                              message)))
                 buffer-diagnostics)))
    (funcall lsp-proxy-diagnostics--flymake-report-fn
             diags
             ;; This :region keyword forces flymake to delete old diagnostics in
             ;; case the buffer hasn't changed since the last call to the report
             ;; function. See https://github.com/joaotavora/eglot/issues/159
             :region (cons (point-min) (point-max)))))

;; project diagnostics
(defvar lsp-proxy-diagnostics-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'lsp-proxy-show-diagnostic)
    (define-key map (kbd "o") 'lsp-proxy-goto-diagnostic)
    map))

(defun lsp-proxy-show-diagnostic (pos &optional other-window)
  "Show location of diagnostic at POS.
If OTHER-WINDOW is non nil, show diagnosis in a new window."
  (interactive (list (point) t))
  (let* ((id (or (tabulated-list-get-id pos)
                 (user-error "Nothing at point")))
         (filepath (plist-get id :filepath))
         (diag (plist-get id :diag))
         (range (plist-get diag :range))
         (start (plist-get range :start))
         (end (plist-get range :end)))
    (with-current-buffer (find-file-noselect filepath)
      (with-selected-window
          (display-buffer (current-buffer) other-window)
        (save-excursion
          (save-restriction
            (widen)
            (let* ((b (eglot--lsp-position-to-point start))
                   (e (eglot--lsp-position-to-point end)))
              (goto-char b)
              (pulse-momentary-highlight-region (point)
                                                (or e (line-end-position)) 'highlight)))))
      (current-buffer))))

(defun lsp-proxy-goto-diagnostic (pos)
  "Show location of diagnostic at POS."
  (interactive "d")
  (pop-to-buffer
   (lsp-proxy-show-diagnostic pos)))

(defvar lsp-proxy--diagnostics-base-tabulated-list-format
  `[("Type" 8 nil)
    ("File" 40 nil)
    ("Backend" 50 t)
    ("Message" 0 t)])

(define-derived-mode lsp-proxy-diagnostics-buffer-mode tabulated-list-mode
  "Lsp proxy diagnostics"
  "A mode for listing Lsp proxy diagnostics."
  :interactive nil
  (setq tabulated-list-format lsp-proxy--diagnostics-base-tabulated-list-format)
  ;; (setq tabulated-list-entries 'lsp-proxy--diagnostics-buffer-entries)
  (tabulated-list-init-header))

(defun lsp-proxy-show-project-diagnostics ()
  "Show a list of diagnostics for current project."
  (interactive)
  (unless lsp-proxy-mode
    (user-error "Lsp proxy mode is not enabled in the current buffer"))
  (let ((workspace-diagnostics (lsp-proxy--get-or-create-project
                                (lsp-proxy-project-root)
                                lsp-proxy--diagnostics-map))
        (target (or (get-buffer lsp-proxy-diagnostics-buffer)
                    (with-current-buffer (get-buffer-create lsp-proxy-diagnostics-buffer)
                      (lsp-proxy-diagnostics-buffer-mode)
                      (current-buffer))))
        rows)
    (maphash (lambda (filepath diags)
               (setq rows (append
                           rows
                           (mapcar (lambda (diag)
                                     (let* ((source (plist-get diag :source))
                                            (code (plist-get diag :code))
                                            (severity (plist-get diag :severity))
                                            (msg (plist-get diag :message))
                                            (range (plist-get diag :range))
                                            (start (plist-get range :start))
                                            (line (plist-get start :line))
                                            (character (plist-get start :character)))
                                       (list (list :diag diag :filepath filepath)
                                             (vector (number-to-string severity)
                                                     (format "/%s/%s" (file-name-nondirectory (directory-file-name (file-name-directory filepath))) (file-name-nondirectory filepath))
                                                     (format "%s(%s)" source code)
                                                     (format "%s [Ln%s,Col%s]" msg (1+ line) (1+ character)))))) diags))))
             workspace-diagnostics)
    (with-current-buffer target
      (display-buffer (current-buffer))
      (setq tabulated-list-entries rows)
      (tabulated-list-print t)
      (revert-buffer))))
;;
;; code action
;;
(defun lsp-proxy--code-action-transform (it)
  "Transform code action IT to a `(title . it)' format."
  (let* ((item (plist-get it :lsp_item))
         (ls-name (plist-get it :language_server_name))
         (title (plist-get item :title)))
    (cons (format "%s - (%s)" title ls-name) it)))

(defun lsp-proxy--select-action (actions)
  "Select an action to execute from ACTIONS."
  (cond
   ((seq-empty-p actions) (lsp-proxy--info "%s" "No code actions found.") nil)
   (t (let* ((completion-ignore-case t)
             (collection (seq-into actions 'list))
             (col (mapcar #'lsp-proxy--code-action-transform collection))
             (completion (completing-read "Select code actions: "
                                          (lambda (string pred action)
                                            (if (eq action 'metadata)
                                                `(metadata (display-sort-function . identity))
                                              (complete-with-action action col string pred))) nil t)))
        (cdr (assoc completion col))))))


(defun lsp-proxy--region-range (start end)
  "Make Range object for the current region START and END."
  (list :start (eglot--pos-to-lsp-position start)
        :end (eglot--pos-to-lsp-position end)))

(defun lsp-proxy--code-actions-at-point ()
  "Retrieve the code actions for the active region or the current line."
  (lsp-proxy--request
   'textDocument/codeAction
   (lsp-proxy--request-or-notify-params
    (list
     :textDocument (eglot--TextDocumentIdentifier)
     :range (if (use-region-p)
                (lsp-proxy--region-range (region-beginning) (region-end))
              (lsp-proxy--region-range (point) (point)))
     :context (list :diagnostics (vector))))))

(defun lsp-proxy--execute-command (command arguments &optional server-id)
  "Ask SERVER-ID to execute COMMAND with ARGUMENTS."
  (let ((params (list :command command :arguments arguments)))
    (lsp-proxy--async-request
     'workspace/executeCommand
     (lsp-proxy--request-or-notify-params
      params
      `(:context (:language-server-id ,server-id))))))

(defun lsp-proxy-execute-code-action (action)
  "Execute code action ACTION.
If ACTION is not set it will be selected
from `lsp-proxy--code-actions-at-point'.
Request codeAction/resolve for more info if server supports."
  (interactive (list (lsp-proxy--select-action (lsp-proxy--code-actions-at-point))))
  (when action
    (let* ((item (plist-get action :lsp_item))
           (ls-id (plist-get action :language_server_id))
           (command (plist-get item :command))
           (edit (plist-get item :edit)))
      (if (and (not command) (not edit))
          (lsp-proxy--async-request
           'codeAction/resolve
           (lsp-proxy--request-or-notify-params item `(:context (:language-server-id ,ls-id)))
           :success-fn (lambda (action)
                         (if action
                             (lsp-proxy--execute-code-action action)
                           (lsp-proxy--info "%s" "No code action found."))))
        (lsp-proxy--execute-code-action action)))))

(defun lsp-proxy--execute-code-action (action)
  "Execute code action ACTION."
  (let* ((item (plist-get action :lsp_item))
         (ls-id (plist-get action :language_server_id))
         (command (plist-get item :command))
         (edit (plist-get item :edit)))
    (when edit
      (eglot--apply-workspace-edit edit this-command))
    (when command
      (lsp-proxy--execute-command (plist-get command :command) (plist-get command :arguments) ls-id))))

;;
;; inlay hints
;;
(defface lsp-proxy-inlay-hint-face '((t (:height 0.8 :inherit shadow)))
  "Face used for inlay hint overlays." :group 'lsp-proxy-mode)

(defface lsp-proxy-type-hint-face '((t (:inherit lsp-proxy-inlay-hint-face)))
  "Face used for type inlay hint overlays." :group 'lsp-proxy-mode)

(defface lsp-proxy-parameter-hint-face '((t (:inherit lsp-proxy-inlay-hint-face)))
  "Face used for parameter inlay hint overlays." :group 'lsp-proxy-mode)

(defface lsp-proxy-highlight-symbol-face
  '((t (:inherit bold)))
  "Face used to highlight the symbol at point." :group 'lsp-proxy-mode)

(defvar-local lsp-proxy--outstanding-inlay-hints-region (cons nil nil)
  "Jit-lock-calculated (FROM . TO) region with potentially outdated hints.")

(defvar-local lsp-proxy--outstanding-inlay-hints-last-region nil)

(defvar-local lsp-proxy--outstanding-inlay-regions-timer nil
  "Helper timer for `lsp-proxy--update-hints'.")

(defun lsp-proxy--update-inlay-hints (from to)
  "Jit-lock function for lsp-proxy inlay hints.
Update the range of `(FROM TO)'."
  (when lsp-proxy--support-inlay-hints
    (cl-symbol-macrolet ((region lsp-proxy--outstanding-inlay-hints-region)
                         (last-region lsp-proxy--outstanding-inlay-hints-last-region)
                         (timer lsp-proxy--outstanding-inlay-regions-timer))
      (setcar region (min (or (car region) (point-max)) from))
      (setcdr region (max (or (cdr region) (point-min)) to))
      ;; HACK: We're relying on knowledge of jit-lock internals here.  The
      ;; condition comparing `jit-lock-context-unfontify-pos' to
      ;; `point-max' is a heuristic for telling whether this call to
      ;; `jit-lock-functions' happens after `jit-lock-context-timer' has
      ;; just run.  Only after this delay should we start the smoothing
      ;; timer that will eventually call `lsp-proxy--update-hints-1' with the
      ;; coalesced region.  I wish we didn't need the timer, but sometimes
      ;; a lot of "non-contextual" calls come in all at once and do verify
      ;; the condition.  Notice it is a 0 second timer though, so we're
      ;; not introducing any more delay over jit-lock's timers.
      (when (= jit-lock-context-unfontify-pos (point-max))
        (if timer (cancel-timer timer))
        (let ((buf (current-buffer)))
          (setq timer (run-at-time
                       0 nil
                       (lambda ()
                         (eglot--when-live-buffer buf
                           ;; HACK: In some pathological situations
                           ;; (Emacs's own coding.c, for example),
                           ;; jit-lock is calling `lsp-proxy--update-hints'
                           ;; repeatedly with same sequence of
                           ;; arguments, which leads to
                           ;; `lsp-proxy--update-hints-1' being called with
                           ;; the same region repeatedly.  This happens
                           ;; even if the hint-painting code does
                           ;; nothing else other than widen, narrow,
                           ;; move point then restore these things.
                           ;; Possible Emacs bug, but this fixes it.
                           (unless (equal last-region region)
                             (lsp-proxy--update-hints-1 (max (car region) (point-min))
                                                        (min (cdr region) (point-max)))
                             (setq last-region region))
                           (setq region (cons nil nil)
                                 timer nil))))))))))

(defun lsp-proxy--update-hints-1 (from to)
  "Do most work for `lsp-proxy--update-hints', including LSP request."
  (let* ((buf (current-buffer))
         (paint-hint
          (lambda (hint)
            (cl-block nil
              (let* ((position (plist-get hint :position))
                     (paddingLeft (plist-get hint :paddingLeft))
                     (paddingRight (plist-get hint :paddingRight))
                     (kind (plist-get hint :kind))
                     (label (plist-get hint :label)))
                (goto-char (eglot--lsp-position-to-point position))
                (when (or (> (point) to) (< (point) from)) (cl-return))
                (let* ((left-pad (and paddingLeft
                                      (not (eq paddingLeft :json-false))
                                      (not (memq (char-before) '(32 9))) " "))
                       (right-pad (and paddingRight
                                       (not (eq paddingRight :json-false))
                                       (not (memq (char-after) '(32 9))) " "))
                       (peg-after-p (eql kind 1)))
                  (cl-labels
                      ((make-ov ()
                         (if peg-after-p
                             (make-overlay (point) (1+ (point)) nil t)
                           (make-overlay (1- (point)) (point) nil nil nil)))
                       (do-it (label lpad rpad i n)
                         (let* ((firstp (zerop i))
                                (tweak-cursor-p (and firstp peg-after-p))
                                (ov (make-ov))
                                (text (concat lpad label rpad)))
                           (when tweak-cursor-p (put-text-property 0 1 'cursor 1 text))
                           (overlay-put ov (if peg-after-p 'before-string 'after-string)
                                        (propertize
                                         text
                                         'face (pcase kind
                                                 (1 'lsp-proxy-type-hint-face)
                                                 (2 'lsp-proxy-parameter-hint-face)
                                                 (_ 'lsp-proxy-inlay-hint-face))))
                           (overlay-put ov 'priority (if peg-after-p i (- n i)))
                           (overlay-put ov 'lsp-proxy--inlay-hint t)
                           (overlay-put ov 'evaporate t)
                           (overlay-put ov 'lsp-proxy--overlay t))))
                    (if (stringp label)
                        (do-it label left-pad right-pad 0 1)
                      (cl-loop
                       for i from 0 for ldetail across label
                       do (lsp-proxy--dbind (:value value) ldetail
                            (do-it value
                                   (and (zerop i) left-pad)
                                   (and (= i (1- (length label))) right-pad)
                                   i (length label))))))))))))
    (lsp-proxy--async-request
     'textDocument/inlayHint
     (lsp-proxy--request-or-notify-params
      (list
       :textDocument (eglot--TextDocumentIdentifier)
       :range (list :start (eglot--pos-to-lsp-position from)
                    :end (eglot--pos-to-lsp-position to))))
     :success-fn (lambda (hints)
                   (eglot--when-live-buffer buf
                     (save-excursion
                       (save-restriction
                         (widen)
                         ;; Overlays ending right at FROM with an
                         ;; `after-string' property logically belong to
                         ;; the (FROM TO) region.  Likewise, such
                         ;; overlays ending at TO don't logically belong
                         ;; to it.
                         (dolist (o (overlays-in (1- from) to))
                           (when (and (overlay-get o 'lsp-proxy--inlay-hint)
                                      (cond ((eq (overlay-end o) from)
                                             (overlay-get o 'after-string))
                                            ((eq (overlay-end o) to)
                                             (overlay-get o 'before-string))
                                            (t)))
                             (delete-overlay o)))
                         (mapc paint-hint hints)))))
     :deferred 'lsp-proxy--update-hints-1)))

(defun lsp-proxy-activate-inlay-hints-mode ()
  "Activate `lsp-proxy-inlay-hints-mode` for the current buffer
if `lsp-proxy-inlay-hints-mode-config` allows it."
  (when (and lsp-proxy--support-inlay-hints
             (boundp 'lsp-proxy-inlay-hints-mode-config)
             (or (eq lsp-proxy-inlay-hints-mode-config t)
                 (and (listp lsp-proxy-inlay-hints-mode-config)
                      (member major-mode lsp-proxy-inlay-hints-mode-config))))
    (lsp-proxy-inlay-hints-mode 1)))

(define-minor-mode lsp-proxy-inlay-hints-mode
  "Mode for displaying inlay hint."
  :lighter nil
  (cond
   (lsp-proxy-inlay-hints-mode
    (jit-lock-register #'lsp-proxy--update-inlay-hints 'contextual))
   (t
    (jit-lock-unregister #'lsp-proxy--update-inlay-hints)
    (remove-overlays nil nil 'lsp-proxy--inlay-hint t))))

;; imenu
(cl-defun lsp-proxy-imenu ()
  "Lsp-Proxy's `imenu-create-index-function'.
Returns a list as described in docstring of `imenu--index-alist'."
  (unless lsp-proxy--support-document-symbols
    (cl-return-from lsp-proxy-imenu))
  (let* ((res (lsp-proxy--request 'textDocument/documentSymbol
                                  (lsp-proxy--request-or-notify-params (list :textDocument (eglot--TextDocumentIdentifier)))
                                  :cancel-on-input non-essential))
         ;; (head (and (cl-plusp (length res)) (elt res 0)))
         )
    ;; (when head
    ;;   (eglot--dcase head
    ;;     (((SymbolInformation)) (eglot--imenu-SymbolInformation res))
    ;;     (((DocumentSymbol)) (eglot--imenu-DocumentSymbol res))))
    (lsp-proxy--convert-imenu-format res)))

(defun lsp-proxy--convert-imenu-format (symbols)
  "Convert custom parsed LSP-style SYMBOLS into imenu-compatible cons cell format."
  (mapcar
   (lambda (item)
     (let* ((name (aref item 0))
            (content (aref item 1))
            (group (plist-get content :Group))
            (pos (plist-get content :Position)))
       (cond
        (group
         (cons name (lsp-proxy--convert-imenu-format group)))
        (pos
         (cons name (cons (aref pos 0) (aref pos 1))))
        (t
         (cons name content)))))
   symbols))

(defun lsp-proxy--imenu-lsp-goto (name pos)
  "Jump to the position specified by POS.
If POS is in LSP format (line . column), convert and jump to that position.
Otherwise use the original imenu goto function.
NAME is the imenu item name."
  (if (and (consp pos) (numberp (car pos)) (numberp (cdr pos)))
      ;; LSP format position (line . column)
      (goto-char (eglot--lsp-position-to-point (list :line (car pos) :character (cdr pos))))
    ;; Regular point position, use original imenu goto function
    (funcall #'imenu-default-goto-function name pos)))

;;
;; commands
;;
(defun lsp-proxy--get-commands ()
  "Get support commands from server."
  (lsp-proxy--request 'emacs/getCommands (lsp-proxy--request-or-notify-params nil)))

(defun lsp-proxy--select-command (commands)
  "Select a command to execute from COMMANDS."
  (cond
   ((seq-empty-p commands) (lsp-proxy--info "%s" "No command found.") nil)
   (t (let* ((completion-ignore-case t)
             (collection (seq-into commands 'list))
             (col (mapcar (lambda (it) (cons (plist-get it :id) it)) collection))
             (completion (completing-read "Select command: "
                                          (lambda (string pred command)
                                            (if (eq command 'metadata)
                                                `(metadata (display-sort-function . identity))
                                              (complete-with-action command col string pred))) nil t)))
        (cdr (assoc completion col))))))

(defun lsp-proxy-execute-command (command)
  "Execute COMMAND."
  (interactive (list (lsp-proxy--select-command (lsp-proxy--get-commands))))
  (when command
    (lsp-proxy--execute-command
     (plist-get command :id)
     (vector)
     (plist-get command :language_server_id))))

;; rust-analyzer
(defun lsp-proxy--view-file-text ()
  "RustAnalyzer ViewFileText."
  (interactive)
  (lsp-proxy--async-request
   'rust-analyzer/viewFileText
   (lsp-proxy--request-or-notify-params (eglot--TextDocumentIdentifier))
   :success-fn (lambda (resp)
                 (message "resp %s" resp))))

;;
;; hooks
;;
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

(defun lsp-proxy--before-revert-hook ()
  "Hook of `before-revert-hook'."
  (lsp-proxy--on-doc-close))

(defun lsp-proxy--after-revert-hook ()
  "Hook of `after-revert-hook'."
  (lsp-proxy--on-doc-focus (selected-window)))

(defvar lsp-proxy--inline-completion-debounce-timer nil)
(defun lsp-proxy--post-command-hook ()
  "Post command hook."
  (lsp-proxy--cleanup-highlights-if-needed)
  (lsp-proxy--idle-reschedule (current-buffer))
  (when (and this-command
             (not (and (symbolp this-command)
                       (or (string-prefix-p "lsp-proxy-inline-completion-" (symbol-name this-command))
                           (lsp-proxy-inline-completion--self-insert this-command)))))
    (lsp-proxy-inline-completion--clear-overlay)
    (when lsp-proxy--inline-completion-debounce-timer
      (cancel-timer lsp-proxy--inline-completion-debounce-timer))
    (when (numberp lsp-proxy-inline-completion-idle-delay)
      (setq lsp-proxy--inline-completion-debounce-timer
            (run-with-idle-timer
             lsp-proxy-inline-completion-idle-delay
             nil
             #'lsp-proxy-inline-completion--post-command-debounce
             (current-buffer))))))

(defun lsp-proxy--mode-off ()
  "Turn off `lsp-proxy-mode' unconditionally."
  (remove-overlays nil nil 'lsp-proxy--overlay t)
  (lsp-proxy-inlay-hints-mode -1)
  (lsp-proxy-inline-completion-mode -1)
  (lsp-proxy-mode -1))

(defconst lsp-proxy--internal-hooks
  '((before-change-functions . lsp-proxy--before-change)
    (after-change-functions . lsp-proxy--after-change)
    (before-revert-hook . lsp-proxy--before-revert-hook)
    (after-revert-hook . lsp-proxy--after-revert-hook)
    (kill-buffer-hook . lsp-proxy--mode-off)
    (kill-buffer-hook . lsp-proxy--on-doc-close)
    (xref-backend-functions . lsp-proxy--xref-backend)
    (before-save-hook . lsp-proxy--will-save)
    (after-save-hook . lsp-proxy--did-save)
    (post-command-hook . lsp-proxy--post-command-hook)
    (post-self-insert-hook . lsp-proxy--post-self-insert-hook)
    (pre-command-hook . lsp-proxy--pre-command-hook)
    (change-major-mode-hook . lsp-proxy--mode-off)))

;;
;; mode
;;
(defun lsp-proxy--buffer-visible-p ()
  "Return non nil if current buffer is visible."
  (or (buffer-modified-p) (get-buffer-window nil t)))

(defun lsp-proxy--init-if-visible ()
  "Run `lsp-proxy--on-doc-focus' for the current buffer if the buffer is visible.
Return non nil if `lsp-proxy--on-doc-focus' was run for the buffer."
  (when (lsp-proxy--buffer-visible-p)
    (remove-hook 'window-configuration-change-hook #'lsp-proxy--init-if-visible t)
    (lsp-proxy--on-doc-focus (selected-window))
    t))

(defun lsp-proxy--mode-enter ()
  "Set up lsp proxy mode when entering."
  ;; Do add hook
  (when buffer-file-name
    (dolist (hook lsp-proxy--internal-hooks)
      (add-hook (car hook) (cdr hook) nil t))
    (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
    (add-hook 'eldoc-documentation-functions #'lsp-proxy-hover-eldoc-function nil t)
    (add-hook 'eldoc-documentation-functions #'lsp-proxy-signature-eldoc-function nil t)
    (eldoc-mode 1)
    ;; Ensure that `lsp-proxy-completion-at-point' the first CAPF to be tried,
    ;; unless user has put it elsewhere in the list by their own
    (add-hook 'completion-at-point-functions #'lsp-proxy-completion-at-point -50 t)
    ;; (completion-at-point-functions . lsp-proxy-completion-at-point)
    ;; Hook onto both window-selection-change-functions and window-buffer-change-functions
    ;; since both are separate ways of 'focussing' a buffer.
    (add-hook 'window-selection-change-functions #'lsp-proxy--on-doc-focus nil 'local)
    (add-hook 'window-buffer-change-functions #'lsp-proxy--on-doc-focus nil 'local)
    (make-local-variable 'completion-category-defaults)
    (setf (alist-get 'lsp-proxy-capf completion-category-defaults) '((styles . (lsp-proxy-passthrough))))
    (make-local-variable 'completion-styles-alist)
    (setf (alist-get 'lsp-proxy-passthrough completion-styles-alist)
          '(lsp-proxy--dumb-tryc
            lsp-proxy-passthrough-all-completions
            "Passthrough completion."))
    (if lsp-proxy-enable-imenu
        (progn
          (add-function :before-until (local 'imenu-create-index-function)
                        #'lsp-proxy-imenu)
          ;; Register imenu goto function
          (setq-local imenu-default-goto-function #'lsp-proxy--imenu-lsp-goto)))
    (cond
     ((and (or
            (and (eq lsp-proxy-diagnostics-provider :auto)
                 (functionp 'flycheck-mode))
            (and (eq lsp-proxy-diagnostics-provider :flycheck)
                 (or (functionp 'flycheck-mode)
                     (user-error "The lsp-proxy-diagnostics-provider is set to :flycheck but flycheck is not installed?"))))
           (require 'flycheck nil t))
      (lsp-proxy-diagnostics-flycheck-enable))
     ((or (eq lsp-proxy-diagnostics-provider :auto)
          (eq lsp-proxy-diagnostics-provider :flymake)
          (eq lsp-proxy-diagnostics-provider t))
      (require 'flymake)
      (lsp-proxy-diagnostics-flymake-enable))
     ((not (eq lsp-proxy-diagnostics-provider :none))
      (lsp-proxy--warn "%s" "Unable to autoconfigure flycheck/flymake. The diagnostics won't be rendered."))
     (t (lsp-proxy--warn "%s" "Unable to configuration flycheck. The diagnostics won't be rendered.")))
    (let ((buffer (current-buffer)))
      (run-with-idle-timer 0 nil (lambda ()
                                   (when (buffer-live-p buffer)
                                     (with-current-buffer buffer
                                       (unless (lsp-proxy--init-if-visible)
                                         (add-hook 'window-configuration-change-hook #'lsp-proxy--init-if-visible)))))))))

(defun lsp-proxy--mode-exit ()
  "Clean up lsp proxy mode when exising."
  ;; remove hook
  (dolist (hook lsp-proxy--internal-hooks)
    (remove-hook (car hook) (cdr hook) t))
  (remove-hook 'completion-at-point-functions #'lsp-proxy-completion-at-point 'local)
  (remove-hook 'window-selection-change-functions #'lsp-proxy--on-doc-focus 'local)
  (remove-hook 'window-buffer-change-functions #'lsp-proxy--on-doc-focus 'local)
  (remove-hook 'eldoc-documentation-functions #'lsp-proxy-hover-eldoc-function 'local)
  (remove-hook 'eldoc-documentation-functions #'lsp-proxy-signature-eldoc-function 'local)
  (remove-function (local 'imenu-create-index-function) #'lsp-proxy-imenu)
  ;; Reset imenu goto function
  (kill-local-variable 'imenu-default-goto-function)
  (setq-local completion-category-defaults
              (cl-remove 'lsp-proxy-capf completion-category-defaults :key #'cl-first))
  (setq-local completion-styles-alist
              (cl-remove 'lsp-proxy-passthrough completion-styles-alist :key #'cl-first))
  (lsp-proxy-diagnostics-flycheck-disable)
  (lsp-proxy-diagnostics-flymake-disable)
  (when lsp-proxy--highlights
    (mapc #'delete-overlay lsp-proxy--highlights))
  (if lsp-proxy-inlay-hints-mode
      (lsp-proxy-inlay-hints-mode -1))

  ;; Send the close event for the active buffer since activating the mode will open it again.
  (lsp-proxy--on-doc-close))

;; Rename file
(defun lsp-proxy--on-set-visited-file-name (old-func &rest args)
  "Advice around function `set-visited-file-name'.

This advice sends textDocument/didClose for the old file and
textDocument/didOpen for the new file."
  (when lsp-proxy-mode
    (lsp-proxy--on-doc-close))
  (prog1 (apply old-func args)
    (when lsp-proxy-mode
      (lsp-proxy--on-doc-open))))

(advice-add 'set-visited-file-name :around #'lsp-proxy--on-set-visited-file-name)

(defun lsp-proxy-restart ()
  "Restart."
  (interactive)
  (unwind-protect
      (progn
        (lsp-proxy--request 'shutdown (lsp-proxy--request-or-notify-params nil) :timeout 1.5)
        (jsonrpc-notify lsp-proxy--connection 'exit (lsp-proxy--request-or-notify-params nil)))
    (jsonrpc-shutdown lsp-proxy--connection)
    (setq lsp-proxy--connection nil))
  (setq lsp-proxy--opened-buffers nil)
  ;; progress map
  (clrhash lsp-proxy--project-hashmap)
  ;; diagnostics
  (clrhash lsp-proxy--diagnostics-map)
  ;; document highlights
  (when lsp-proxy--highlights
    (mapc #'delete-overlay lsp-proxy--highlights))
  ;; inlay hints
  (remove-overlays nil nil 'lsp-proxy--inlay-hint t)
  (lsp-proxy--on-doc-focus (selected-window))
  (message "[LSP-PROXY] Process restarted."))

(defun lsp-proxy-doctor ()
  "Check diagnostics functionality and print system debug information."
  (interactive)
  (let ((debug-buffer (get-buffer-create "*lsp-proxy-doctor*"))
        (buffer (current-buffer)))
    (with-current-buffer buffer
      (with-current-buffer debug-buffer
        (erase-buffer)
        (insert (format "LSP Proxy Doctor Report - %s\n\n" (current-time-string)))

        ;; System information
        (insert "=== System Information ===\n")
        (insert (format "Emacs Version: %s\n" emacs-version))
        (insert (format "System Type: %s\n" system-type))
        (insert (format "Window System: %s\n" window-system))
        (insert "\n"))

      (let ((enable (if (bound-and-true-p lsp-proxy-mode) "Enabled" "Disabled"))
            (alive (if (lsp-proxy--connection-alivep) "Alive" "Not Alive"))
            (diagnostics (if (and (hash-table-p lsp-proxy--diagnostics-map)
                                  (> (hash-table-count lsp-proxy--diagnostics-map) 0))
                             "Contains diagnostics" "Empty"))
            (flymake-status (if (bound-and-true-p lsp-proxy-diagnostics--flymake-enabled) "Yes" "No"))
            (flycheck-status (if (bound-and-true-p lsp-proxy-diagnostics--flycheck-enabled) "Yes" "No")))
        (with-current-buffer debug-buffer
          ;; LSP Proxy configuration
          (insert "=== LSP Proxy Configuration ===\n")
          (insert (format "lsp-proxy-mode: %s\n" enable))
          (insert (format "lsp-proxy--connection: %s\n" alive))
          (insert (format "lsp-proxy--diagnostics-map: %s\n" diagnostics))
          (insert "\n")
          ;; Diagnostics status
          (insert "=== Diagnostics Status ===\n")
          (insert (format "Flycheck Enabled: %s\n" flycheck-status))
          (insert (format "Flymake Enabled: %s\n" flymake-status))))


      ;; Print diagnostics for current buffer if available
      (when (and buffer-file-name (hash-table-p lsp-proxy--diagnostics-map))
        (let* ((file (lsp-proxy--fix-path-casing buffer-file-name))
               (diagnostics (gethash file (lsp-proxy--get-or-create-project
                                           (lsp-proxy-project-root)
                                           lsp-proxy--diagnostics-map))))
          (with-current-buffer debug-buffer
            (insert (format "\nCurrent Buffer Diagnostics (%s):\n" file))
            (if diagnostics
                (dolist (diag diagnostics)
                  (insert (format "- %s: %s\n"
                                  (plist-get diag :severity)
                                  (plist-get diag :message))))
              (insert "No diagnostics found\n"))
            (insert "\n=== End of Report ===\n")
            ;; (view-mode 1)
            ))))
    (display-buffer debug-buffer)))

(defun lsp-proxy-toggle-trace-io ()
  "Toggle jsonrpc logging."
  (interactive)
  (setq lsp-proxy-log-max (if lsp-proxy-log-max nil 0))
  (lsp-proxy-restart)
  (lsp-proxy--info "JSON-RPC logging %s." (if lsp-proxy-log-max "disabled" "enabled")))

(defun lsp-proxy-workspace-restart ()
  "Restart SERVER."
  (interactive)
  (lsp-proxy--async-request
   'emacs/workspaceRestart
   (lsp-proxy--request-or-notify-params nil)
   :success-fn (lambda (data)
                 ;; 清理所有已经打开的该项目下的文件
                 (let ((paths (seq-into data 'list)))
                   (setq lsp-proxy--opened-buffers
                         (cl-remove-if
                          (lambda (elt)
                            (member (buffer-file-name elt) paths))
                          lsp-proxy--opened-buffers)))
                 ;; 清理所有 buffer 存在的 diagnostic 信息
                 (lsp-proxy--remove-project (lsp-proxy-project-root) lsp-proxy--diagnostics-map)
                 ;; 清理记录的当前项目的 progress 信息
                 (lsp-proxy--remove-project (lsp-proxy-project-root) lsp-proxy--project-hashmap)
                 (revert-buffer))))

(defun lsp-proxy-open-config-file ()
  "Open the configuration file. If it does not exist, create it first."
  (interactive)
  (unless (file-exists-p lsp-proxy-user-languages-config)
    (with-temp-buffer lsp-proxy-user-languages-config))
  (find-file lsp-proxy-user-languages-config))

(defun lsp-proxy-open-log-file ()
  "Open the log file. If it does not exist, create it first."
  (interactive)
  (unless (file-exists-p lsp-proxy--log-file)
    (with-temp-buffer lsp-proxy--log-file))
  (find-file lsp-proxy--log-file))

(defvar lsp-proxy-mode-map (make-sparse-keymap)
  "Keymap for lsp proxy minor mode.
Use this for custom bindings in `lsp-prox-mode'.")

;;;###autoload
(define-minor-mode lsp-proxy-mode
  "Minor mode for Lsp-Proxy."
  :init-value nil
  :map lsp-proxy-mode-map
  :lighter " Lsp Proxy"
  (if lsp-proxy-mode
      (lsp-proxy--mode-enter)
    (lsp-proxy--mode-exit)))

;;;###autoload
(define-global-minor-mode global-lsp-proxy-mode
  lsp-proxy-mode lsp-proxy-turn-on-unless-buffer-read-only)

(defun lsp-proxy-turn-on-unless-buffer-read-only ()
  "Turn on `lsp-proxy-mode' if the buffer is writable."
  (unless buffer-read-only
    (lsp-proxy-mode 1)))

(provide 'lsp-proxy)
;;; lsp-proxy.el ends here
