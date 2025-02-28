;;; lsp-proxy.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 JadeStrong
;;
;; Author: JadeStrong <jadestrong@163.com>
;; Maintainer: JadeStrong <jadestrong@163.com>
;; Created: December 15, 2023
;; Modified: December 15, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jadestrong/lsp-proxy
;; Package-Requires: ((emacs "29.1") (s "1.13.1") (eldoc "1.14.0") (ht "2.4") (posframe "1.4.4") (dash "2.19.1") (f "0.21.0") (yasnippet "0.14.1"))
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

(require 's)
(require 'f)
(require 'ht)
(require 'dash)
(require 'posframe)
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

(defcustom lsp-proxy-signature-buffer " *lsp-proxy-signature*"
  "Buffer for display signature help info."
  :type 'string
  :group 'lsp-proxy)

(defcustom lsp-proxy-signature-auto-active nil
  "If auto active signature help."
  :type 'boolean
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

(defcustom lsp-proxy-log-level 1
  "A number indicating the log level. Defaults to 1."
  :type '(choice (const :tag "Warn" 0)
          (const :tag "Info" 1)
          (const :tag "Debug" 2)
          (const :tag "Trace" 3))
  :group 'lsp-proxy)

(defcustom lsp-proxy-inline-completion-trigger-kind 1
  "Describes how an `InlineCompletionItemProvider' was triggered.
0 means to invoke inline completion manually, 2 means invoke it automatically.")

(defface lsp-proxy-hover-posframe
  '((t :inherit tooltip))
  "Background and foreground for `lsp-proxy-hover-posframe'."
  :group 'lsp-proxy)

(defcustom lsp-proxy-signature-retrigger-keys '(return)
  "Character strings used to retrigger a new textDocument/signatureHelp request."
  :type 'list
  :group 'lsp-proxy-mode)

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
- nil: Inlay hints mode is disabled.
- t: Inlay hints mode is enabled for all buffers.
- A list of major modes (e.g., '(emacs-lisp-mode python-mode)): Inlay hints mode is enabled only for these modes."
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

(defcustom lsp-proxy-inline-completion-idle-delay
  0.5 ;; maybe use lsp-proxy-idle-delay as default?
  "Idle delay for inline-completion."
  :type 'number
  :group 'lsp-proxy)

(defcustom lsp-proxy-inline-completion-enable-predicates '(evil-insert-state-p)
  "A list of predicate functions with no argument to enable auto inline completion.
Auto inline complete will be triggered only if all predicates return t."
  :type '(repeat function)
  :group 'lsp-proxy)

(defcustom lsp-proxy-inline-completion-disable-predicates nil
  "A list of predicate functions with no argument to disable auto inline completion.
Auto inline complete will be not triggered if any predicates return t."
  :type '(repeat function)
  :group 'lsp-proxy)


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
  "Lsp Proxy agent jsonrcp connection instnace.")

(defvar lsp-proxy--opened-buffers nil
  "List of buffers that have been opened in Lsp Proxy.")

(defvar lsp-proxy--inline-completion-commands
  '(lsp-proxy-inline-completion-complete
    lsp-proxy-inline-completion-trigger
    lsp-proxy-inline-completion-cancel
    )
  "Completion commands that needs to be ignored in `lsp-proxy--inline-completion-post-command'.")

(defvar-keymap lsp-proxy--inline-completion-active-mode-map
  "M-i" 'lsp-proxy-inline-completion-complete
  "C-i" 'lsp-proxy-inline-completion-trigger
  )

(defvar-local lsp-proxy--doc-version 0
  "The document version of the current buffer. Incremented after each change.")

(defvar-local lsp-proxy--recent-changes nil
  "Recent buffer changes as collected by `lsp-proxy--before-change'.")

(defvar-local lsp-proxy--inline-completion-preview-overlay nil
  "Overlay used by `lsp-proxy-inline-completion-mode'.")

(defvar-local lsp-proxy--inline-completion-preview-timer nil
  "Timer used by `lsp-proxy-inline-completion-mode'.")

(defvar-local lsp-proxy--change-idle-timer nil
  "Idle timer for didChange signals.")

(defvar-local lsp-proxy--completion-trigger-characters nil
  "Completion trigger characters.")

(defvar-local lsp-proxy--signature-trigger-characters nil
  "Signature trigger characters.")

(defvar-local lsp-proxy-enable-relative-indentation nil
  "Enable relative indentation when insert texts, snippets ...
from language server.")

(defvar-local lsp-proxy-diagnostics--flycheck-enabled nil
  "True when lsp-proxy diagnostics flycheck integration
 has been enabled in this buffer.")

(defvar-local lsp-proxy-diagnostics--flymake-enabled nil
  "True when lsp-proxy diagnostics flymake integration
 has been enabled in this buffer.")

(defvar-local lsp-proxy-diagnostics--flycheck-checker nil
  "The value of flycheck-checker before lsp-proxy diagnostics was activated.")

(defvar-local lsp-proxy--signature-last nil)
(defvar-local lsp-proxy--signature-last-index nil)
(defvar lsp-proxy--signature-last-buffer nil)

(defvar-local lsp-proxy--support-inlay-hints nil
  "Is there any server associated with this buffer that support `textDocument/inlayHint' request.")

(defvar-local lsp-proxy--support-document-highlight nil
  "Is there any server associated with this buffer that support `textDocument/documentHighlight' request.")

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

(defconst lsp-proxy--kind->symbol
  '((1 . text)
    (2 . method)
    (3 . function)
    (4 . constructor)
    (5 . field)
    (6 . variable)
    (7 . class)
    (8 . interface)
    (9 . module)
    (10 . property)
    (11 . unit)
    (12 . value)
    (13 . enum)
    (14 . keyword)
    (15 . snippet)
    (16 . color)
    (17 . file)
    (18 . reference)
    (19 . folder)
    (20 . enum-member)
    (21 . constant)
    (22 . struct)
    (23 . event)
    (24 . operator)
    (25 . type-parameter)))

(defconst lsp-proxy--message-type-face
  `((1 . ,compilation-error-face)
    (2 . ,compilation-warning-face)
    (3 . ,compilation-message-face)
    (4 . ,compilation-info-face)))

;; progress token map
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

;; log message
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

(defvar lsp-proxy--already-widened nil)
(defmacro lsp-proxy--save-restriction-and-excursion (&rest form)
  (declare (indent 0) (debug t))
  `(if lsp-proxy--already-widened
       (save-excursion ,@form)
     (let* ((lsp-proxy--already-widened t))
       (save-restriction
         (widen)
         (save-excursion ,@form)))))

(cl-defmacro lsp-proxy--when-live-buffer (buf &rest body)
  "Check BUF live, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf)) (if (buffer-live-p ,b) (with-current-buffer ,b ,@body)))))

(cl-defmacro lsp-proxy--when-buffer-window (buf &body body)
  "Check BUF showing somewhere, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf))
       ;;notice the exception when testing with `ert'
       (when (or (get-buffer-window ,b) (ert-running-test))
         (with-current-buffer ,b ,@body)))))

(defun lsp-proxy--calculate-column ()
  "Calculate character offset of cursor in current line."
  (/ (- (length
         (encode-coding-region
          (line-beginning-position)
          (min (point) (point-max)) 'utf-16 t))
        2)
     2))

(defun lsp-proxy--get-uri ()
  "Get URI of current buffer."
  (cond
   ((not buffer-file-name)
    (concat "buffer://" (url-encode-url (buffer-name (current-buffer)))))
   ((and (eq system-type 'windows-nt)
         (not (s-starts-with-p "/" buffer-file-name)))
    (concat "file:///" (url-encode-url buffer-file-name)))
   (t
    (concat "file://" (url-encode-url buffer-file-name)))))

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

(defun lsp-proxy--get-source ()
  "Get source code from current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun lsp-proxy--position ()
  (list :line (1- (line-number-at-pos)) :character (lsp-proxy--calculate-column)))

(defun lsp-proxy--point-position (point)
  "Get position of the POINT."
  (lsp-proxy--save-restriction-and-excursion
    (goto-char point)
    (lsp-proxy--position)))

(defun lsp-proxy--position-point (pos)
  "Convert `Position' object POS to a point."
  (let* ((line (plist-get pos :line))
         (character (plist-get pos :character)))
    (lsp-proxy--line-character-to-point line character)))

(defun lsp-proxy--line-character-to-point (line character)
  "Return the point for character CHARACTER on line LINE."
  (let ((inhibit-field-text-motion t))
    (lsp-proxy--save-restriction-and-excursion
      (goto-char (point-min))
      (forward-line line)
      ;; server may send character position beyond the current line and we
      ;; sould fallback to line end.
      (let* ((line-end (line-end-position)))
        (if (> character (- line-end (point)))
            line-end
          (forward-char character)
          (point))))))

(defun lsp-proxy--position-equal (pos-a pos-b)
  "Return whether POS-A and POS-B positions are equal."
  (and (= (plist-get pos-a :line) (plist-get pos-b :line))
       (= (plist-get pos-a :character) (plist-get pos-b :character))))

(defun lsp-proxy--position-compare (pos-a pos-b)
  "Return t if POS-A if greater thatn POS-B."
  (let* ((line-a (plist-get pos-a :line))
         (line-b (plist-get pos-b :line)))
    (if (= line-a line-b)
        (> (plist-get pos-a :character) (plist-get pos-b :character))
      (> line-a line-b))))

;; TODO fix point if the line or charactor is -1
(defun lsp-proxy--range-region (range)
  "Return region (BEG . END) that represents LSP RANGE.
If optional MARKERS, make markers."
  (let ((beg (lsp-proxy--position-point (plist-get range :start)))
        (end (lsp-proxy--position-point (plist-get range :end))))
    (cons beg end)))

(defun lsp-proxy--region-range (start end)
  "Make Range object for the current region START and END."
  (list :start (lsp-proxy--point-position start)
        :end (lsp-proxy--point-position end)))

(defun lsp-proxy--region-or-line ()
  "The active region or the current line."
  (if (use-region-p)
      (lsp-proxy--region-range (region-beginning) (region-end))
    (lsp-proxy--region-range (line-beginning-position) (line-end-position))))

(defun lsp-proxy--format-markup (markup)
  "Format MARKUP according to LSP's spec."
  (pcase-let ((`(,string ,mode)
               (if (stringp markup) (list markup 'gfm-view-mode)
                 (list (plist-get markup :value)
                       (pcase (plist-get markup :kind)
                         ("markdown" 'gfm-view-mode)
                         ("plaintext" 'text-mode)
                         (_ major-mode))))))
    (with-temp-buffer
      (setq-local markdown-fontify-code-blocks-natively t)
      (insert string)
      (let ((inhibit-message t)
            (message-log-max nil))
        (ignore-errors (delay-mode-hooks (funcall mode))))
      (font-lock-ensure)
      (string-trim (buffer-string)))))

(defun lsp-proxy--markdown-render ()
  (when (fboundp 'gfm-view-mode)
    (let ((inhibit-message t))
      (setq-local markdown-fontify-code-blocks-natively t)
      (set-face-background 'markdown-code-face (face-attribute 'lsp-proxy-hover-posframe :background nil t))
      ;; (set-face-attribute 'markdown-code-face nil :height 230)
      (gfm-view-mode)))
  (read-only-mode 0)
  (prettify-symbols-mode 1)
  (display-line-numbers-mode -1)
  (font-lock-ensure)

  (setq-local mode-line-format nil))

(defun lsp-proxy--expand-snippet (snippet &optional start end expand-env)
  "Wrapper of `yas-expand-snippet' with all of it arguments.
The snippet will be convert to LSP style and indent according to
LSP server according to
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

(defun lsp-proxy--get-file-contents-from-list (paths)
  "Get all file content of PATHS list."
  (apply #'vector
         (mapcar
          (lambda (path)
            (let ((buffer (find-file-noselect path)))
              (with-current-buffer buffer
                (list :path path :content (buffer-substring-no-properties (point-min) (point-max)))))) paths)))

(defun lsp-proxy--TextDocumentIdentifier ()
  "Make a TextDocumentIdentifier object."
  `(:textDocument
    (:uri ,(lsp-proxy--get-uri))))

(defun lsp-proxy--TextDocumentPosition ()
  "Make a TextDocumentPosition object."
  (append `(:position ,(lsp-proxy--position))
          (lsp-proxy--TextDocumentIdentifier)))

(defun lsp-proxy--request-or-notify-params (params &rest args)
  "Wrap request or notify params base PARAMS and add extra ARGS."
  (let ((rest (apply 'append args)))
    (append (list :uri (lsp-proxy--get-uri) :params params) rest)))

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


(defcustom lsp-proxy-xref-force-references nil
  "If non-nil threat everything as references(e. g. jump if only one item.)"
  :group 'lsp-proxy
  :type 'boolean)

(defcustom lsp-proxy-progress-prefix "⌛ "
  "Progress prefix."
  :group 'lsp-proxy-mode
  :type 'string)

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

(defun lsp-proxy--process-locations (locations)
  "Process LOCATIONS and show xrefs."
  (if (seq-empty-p locations)
      (lsp-proxy--error "Not found for: %s" (or (thing-at-point 'symbol t) ""))
    (when-let* ((locs (cl-mapcar (lambda (it)
                                   (let* ((uri (plist-get it :uri))
                                          (filepath (lsp-proxy--uri-to-path uri))
                                          (visiting (find-buffer-visiting filepath))
                                          (range (plist-get it :range))
                                          (start (plist-get range :start))
                                          (end (plist-get range :end))
                                          (start-line (plist-get start :line))
                                          (start-column (plist-get start :character))
                                          (_end-line (plist-get end :line))
                                          (_end-column (plist-get end :character))
                                          (collect (lambda ()
                                                     (save-excursion
                                                       (save-restriction
                                                         (widen)
                                                         (let* ((beg (lsp-proxy--position-point start))
                                                                (end (lsp-proxy--position-point end))
                                                                (bol (progn (goto-char beg) (line-beginning-position)))
                                                                (summary (buffer-substring bol (line-end-position)))
                                                                (hi-beg (- beg bol))
                                                                (hi-end (- (min (line-end-position) end) bol)))
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
                                      (t (lsp-proxy--warn "Failed  to process xref entry for file %s" filepath)))))
                                 (if (vectorp locations) locations (vector locations)))))
      (lsp-proxy-show-xrefs locs nil nil))))

;;
;; text-edit
;;
(defun lsp-proxy--apply-text-document-edit (change)
  "Apply CHANGE."
  (let* ((kind (gethash "kind" change))
         (options (gethash "options" change))
         uri
         filename
         new-uri new-filename
         overwrite ignoreIfExists recursive ignoreIfNotExists)
    (cond
     ((equal kind "create")
      (setq uri (gethash "uri" change))
      (setq filename (lsp-proxy--uri-to-path uri))
      (when options
        (setq overwrite (gethash "overwrite" options)
              ignoreIfExists (gethash "ignoreIfExists" options)))
      (if (file-exists-p filename)
          (if (or overwrite
                  (not ignoreIfExists))
              (progn
                (when (find-buffer-visiting filename)
                  (with-current-buffer (find-buffer-visiting filename)
                    (save-buffer)
                    (kill-buffer)))
                (delete-file filename t)
                (with-current-buffer (find-file-noselect filename)
                  (save-buffer)))
            (lsp-proxy--warn "Cannot create file %s." filename))
        (when (find-buffer-visiting filename)
          (with-current-buffer (find-buffer-visiting filename)
            (save-buffer)
            (kill-buffer)))
        (delete-file filename t)
        (with-current-buffer (find-file-noselect new-filename)
          (save-buffer))))
     ((equal kind "rename")
      (setq uri (gethash "oldUri" change))
      (setq filename (lsp-proxy--uri-to-path uri))
      (setq new-uri (gethash "newUri" change))
      (setq new-filename (lsp-proxy--uri-to-path new-uri))
      (when options
        (setq overwrite (gethash "overwrite" options)
              ignoreIfExists (gethash "ignoreIfExists" options)))
      (if (file-exists-p new-filename)
          (if (or overwrite
                  (not ignoreIfExists))
              (progn
                (when (find-buffer-visiting filename)
                  (with-current-buffer (find-buffer-visiting filename)
                    (save-buffer)
                    (kill-buffer)))
                (when (find-buffer-visiting new-filename)
                  (with-current-buffer (find-buffer-visiting new-filename)
                    (save-buffer)
                    (kill-buffer)))
                (rename-file filename new-filename t))
            (lsp-proxy--warn "Cannot rename %s to %s" filename new-filename))
        (if (find-buffer-visiting filename) ;; new filename not existing
            (progn
              (with-current-buffer (find-buffer-visiting filename)
                (save-buffer)
                (kill-buffer))
              (rename-file filename new-filename t)
              (find-file new-filename))
          (rename-file filename new-filename t))))
     ((equal kind "delete")
      (setq uri (gethash "uri" change))
      (setq filename (lsp-proxy--uri-to-path uri))
      (when options
        (setq recursive (gethash "recursive" options)
              ignoreIfNotExists (gethash "ignoreIfNotExists" options)))
      (when (file-exists-p filename)
        (if (file-directory-p filename)
            (progn
              (if recursive
                  (progn
                    (dolist (buf (buffer-list))
                      (with-current-buffer buf
                        (when (and buffer-file-name
                                   (f-parent-of-p filename buffer-file-name))
                          (save-buffer)
                          (kill-buffer))))
                    (delete-directory filename t t))
                (lsp-proxy--warn "Cannot delete directory %s" filename)))
          (if (find-buffer-visiting filename)
              (with-current-buffer (find-buffer-visiting filename)
                (save-buffer)
                (kill-buffer)))
          (delete-file filename t)))))))

(defun lsp-proxy--sort-edits (edits)
  (sort edits #'(lambda (edit-a edit-b)
                  (let* ((range-a (plist-get edit-a :range))
                         (range-b (plist-get edit-b :range))
                         (start-a (plist-get range-a :start))
                         (start-b (plist-get range-b :start))
                         (end-a (plist-get range-a :end))
                         (end-b (plist-get range-a :end)))
                    (if (lsp-proxy--position-equal start-a start-b)
                        (lsp-proxy--position-compare end-a end-b)
                      (lsp-proxy--position-compare start-a start-b))))))

(defun lsp-proxy--apply-text-edit (edit)
  "Apply the edits ddescribed in the TextEdit objet in TEXT-EDIT."
  (let* ((start (lsp-proxy--position-point (plist-get (plist-get edit :range) :start)))
         (end (lsp-proxy--position-point (plist-get (plist-get edit :range) :end)))
         (new-text (plist-get edit :newText)))
    (setq new-text (s-replace "\r" "" (or new-text "")))
    (plist-put edit :newText new-text)
    (goto-char start)
    (delete-region start end)
    (insert new-text)))

(defun lsp-proxy--apply-text-edit-replace-buffer-contents (edit)
  "Apply the edits described in the TextEdit object in TEXT-EDIT.
The method uses `replace-buffer-contents'."
  (let* (
         (source (current-buffer))
         (new-text (plist-get edit :newText))
         (region (lsp-proxy--range-region (plist-get edit :range)))
         (beg (car region))
         (end (cdr region))
         ;; ((beg . end) (lsp--range-to-region (lsp-make-range :start (lsp--fix-point start)
         ;;                                      :end (lsp--fix-point end))))
         )
    (setq new-text (s-replace "\r" "" (or new-text "")))
    (plist-put edit :newText new-text)
    (with-temp-buffer
      (insert new-text)
      (let ((temp (current-buffer)))
        (with-current-buffer source
          (save-excursion
            (save-restriction
              (narrow-to-region beg end)

              ;; On emacs versions < 26.2,
              ;; `replace-buffer-contents' is buggy - it calls
              ;; change functions with invalid arguments - so we
              ;; manually call the change functions here.
              ;;
              ;; See emacs bugs #32237, #32278:
              ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32237
              ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32278
              (let ((inhibit-modification-hooks t)
                    (length (- end beg)))
                (run-hook-with-args 'before-change-functions
                                    beg end)
                (replace-buffer-contents temp)
                (run-hook-with-args 'after-change-functions
                                    beg (+ beg (length new-text))
                                    length)))))))))

(defun lsp-proxy--apply-text-edits (edits &optional version)
  "Apply the EDITS of VERSION described in the TextEdit[] object."
  (unless (seq-empty-p edits)
    (atomic-change-group
      (let* ((change-group (prepare-change-group))
             (howmany (length edits))
             (message (format "Applying %s edits to `%s' ..." howmany (current-buffer)))
             (_ (message message))
             (reporter (make-progress-reporter message 0 howmany))
             (done 0))
        (unwind-protect
            (mapc (lambda (edit)
                    (progress-reporter-update reporter (cl-incf done))
                    (lsp-proxy--apply-text-edit-replace-buffer-contents edit)
                    (when-let* ((insert-text-format (plist-get edit :insertTextFormat))
                                (start (lsp-proxy--position-point (plist-get (plist-get edit :range) :start)))
                                (new-text (plist-get edit :newText)))
                      (when (eq insert-text-format 2)
                        ;; No `save-excursion' needed since expand snippet will change point anyway
                        (goto-char (+ start (length new-text)))
                        (lsp-proxy--indent-lines start (point))
                        (lsp-proxy--expand-snippet new-text start (point))))) (reverse edits))
          (undo-amalgamate-change-group change-group)
          (progress-reporter-done reporter))))))

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
                                                (end-point (lsp-proxy--position-point end)))
                                           (message "range %s end %s" range end)
                                           (> end-point first-edited)))
                          edits))
           (lsp-proxy--warn "%s" "TextEdits will not be applied since document has been modified before of them.")
         (lsp-proxy--apply-text-edits edits)))
     (lambda ()
       (remove-hook 'before-change-functions func t)))))

;;
;; modeline
;;
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
  (lambda ()
    (lsp-proxy--error "%s" "Request timeout"))
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
         (lsp-proxy--start-agent))
       (lsp-proxy--send-did-change)
       (unless (-contains-p lsp-proxy--opened-buffers (current-buffer))
         (lsp-proxy--on-doc-open))
       (jsonrpc-request lsp-proxy--connection ,@args))))

(defmacro lsp-proxy--notify (method &rest params)
  "Send a notification to the lsp proxy agent with ARGS."
  `(progn
     (unless (lsp-proxy--connection-alivep)
       (lsp-proxy--start-agent))
     (if (or (eq ,method 'textDocument/didOpen) (eq ,method 'textDocument/willSave) (eq ,method 'textDocument/didSave) (-contains-p lsp-proxy--opened-buffers (current-buffer)))
         (let ((new-params (list :uri (lsp-proxy--get-uri) :params ,@params)))
           (jsonrpc-notify lsp-proxy--connection ,method new-params))
       (lsp-proxy--on-doc-open))))

(cl-defmacro lsp-proxy--async-request (method params &rest args &key (success-fn #'lsp-proxy--ignore-response) (error-fn #'lsp-proxy--show-error) (timeout-fn #'lsp-proxy--show-timeout) &allow-other-keys)
  "Send an asynchronous request to the lsp proxy agent."
  `(progn
     (unless (lsp-proxy--connection-alivep)
       (lsp-proxy--start-agent))
     (lsp-proxy--send-did-change)
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
                                              (funcall ,timeout-fn)))
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

(defun lsp-proxy--start-agent ()
  "Start the lsp proxy agent process in local."
  (let* ((timestamp (format-time-string "%Y%m%d%H%M%S"))
         (random-num (random 100000))
         (filename (format "lsp-proxy-%s-%05d.log" timestamp random-num)))
    (setq lsp-proxy--log-file (concat lsp-proxy-log-file-directory filename))
    (if (file-exists-p lsp-proxy--exec-file)
        (progn
          (setq lsp-proxy--connection (lsp-proxy--make-connection))
          (message "Lsp proxy agent started."))
      (lsp-proxy--error "No lsp-proxy file found, please check your `lsp-proxy--exec-file'"))))

(defun lsp-proxy--handle-notification (_ method msg)
  "Handle MSG of type METHOD."
  (when (eql method 'textDocument/publishDiagnostics)
    (lsp-proxy--dbind (:uri uri :diagnostics diagnostics) (plist-get msg :params)
      (let ((filepath (lsp-proxy--uri-to-path uri)))
        (when (f-exists-p filepath)
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
                   (lsp-proxy-diagnostics--flymake-after-diagnostics))))))))
  (when  (eql method 'window/logMessage)
    (lsp-proxy--dbind (:type type :message message) (plist-get msg :params)
      (lsp-proxy-log "%s" (lsp-proxy--propertize message type))))
  (when  (eql method 'window/showMessage)
    (lsp-proxy--dbind (:type type :message message) (plist-get msg :params)
      (lsp-proxy--info "%s" (lsp-proxy--propertize message type))))
  (when (eql method 'emacs/triggerCharacters)
    (lsp-proxy--dbind (:uri uri
                         :triggerCharacters trigger-characters
                         :signatureTriggerCharacters signature-trigger-characters
                         :supportInlayHints support-inlay-hints
                         :supportDocumentHighlight support-document-highlight)
        (plist-get msg :params)
      (let* ((filepath (lsp-proxy--uri-to-path uri)))
        (when (f-exists? filepath)
          (with-current-buffer (find-file-noselect filepath)
            (setq-local lsp-proxy--completion-trigger-characters trigger-characters)
            (setq-local lsp-proxy--signature-trigger-characters signature-trigger-characters)
            (setq-local lsp-proxy--support-inlay-hints (not (eq support-inlay-hints :json-false)))
            (setq-local lsp-proxy--support-document-highlight (not (eq support-document-highlight :json-false)))
            (lsp-proxy-activate-inlay-hints-mode)
            ;; TODO when support and enable, add a idle hook and reschedule this buffer
            )))))
  (when (eql method '$/progress)
    (add-to-list 'global-mode-string '(t (:eval (lsp-proxy--progress-status))))
    (lsp-proxy--dbind (:rootPath root-path :params params) (plist-get msg :params)
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
    (lsp-proxy--dbind (:edit edit) (plist-get msg :params)
      (lsp-proxy--apply-workspace-edit edit)))
  (when (eql method 'eslint/openDoc)
    (lsp-proxy--dbind (:url url) (plist-get msg :params)
      (browse-url url)))
  (when (eql method 'emacs/getFiles)
    (lsp-proxy--dbind (:paths paths) (plist-get msg :params)
      (list :files (lsp-proxy--get-file-contents-from-list (seq-into paths 'list))))))

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
                             (list :textDocument (list :uri (lsp-proxy--get-uri))))
      (lsp-proxy--on-doc-open))))

(defun lsp-proxy--on-doc-open ()
  "On doc open."
  (setq lsp-proxy--recent-changes nil
        lsp-proxy--doc-version 0)
  (when buffer-file-name
    (when (not (f-exists? buffer-file-name))
      (save-buffer))
    (add-to-list 'lsp-proxy--opened-buffers (current-buffer))
    (lsp-proxy--notify 'textDocument/didOpen
                         (list :textDocument (list :uri (lsp-proxy--get-uri)
                                                   :text (lsp-proxy--get-source))))))

(defun lsp-proxy--on-doc-close (&rest _args)
  "Notify that the document has been closed."
  (when (-contains-p lsp-proxy--opened-buffers (current-buffer))
    (lsp-proxy--notify 'textDocument/didClose
                         (list :textDocument (list :uri (lsp-proxy--get-uri))))
    (setq lsp-proxy--opened-buffers (delete (current-buffer) lsp-proxy--opened-buffers))))


(defun lsp-proxy--will-save ()
  "Send textDocument/willSave notification."
  (lsp-proxy--notify 'textDocument/willSave
                       ;; 1 Manual, 2 AfterDelay, 3 FocusOut
                       (append '(:reason 1) (lsp-proxy--TextDocumentIdentifier))))

(defun lsp-proxy--did-save ()
  "Send textDocument/didSave notification."
  (lsp-proxy--notify 'textDocument/didSave
                       (lsp-proxy--TextDocumentIdentifier)))

(defun lsp-proxy--send-did-change ()
  "Send textDocument/didChange to server."
  (when lsp-proxy--recent-changes
    (let ((full-sync-p (eq :emacs-messup lsp-proxy--recent-changes)))
      (lsp-proxy--notify 'textDocument/didChange
                           (list :textDocument
                                 (list :uri (lsp-proxy--get-uri) :version lsp-proxy--doc-version)
                                 :contentChanges
                                 (if full-sync-p
                                     (vector (list :text (lsp-proxy--save-restriction-and-excursion
                                                           (buffer-substring-no-properties (point-min)
                                                                                           (point-max)))))
                                   (cl-loop for (beg end len text) in (reverse lsp-proxy--recent-changes)
                                            when (numberp len)
                                            vconcat `[,(list :range `(:start ,beg :end ,end)
                                                             :rangeLength len :text text)]))))
      (setq lsp-proxy--recent-changes nil))))

(defun lsp-proxy-find-definition ()
  "Find definition."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/definition
   (lsp-proxy--request-or-notify-params (lsp-proxy--TextDocumentPosition))
   :success-fn #'lsp-proxy--process-locations))

(defun lsp-proxy-find-references ()
  "Find references."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/references
   (lsp-proxy--request-or-notify-params (append (lsp-proxy--TextDocumentPosition) `(:context (:includeDeclaration t))))
   :success-fn #'lsp-proxy--process-locations))

(defun lsp-proxy-find-declaration ()
  "Find declaration."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/declaration
   (lsp-proxy--request-or-notify-params (lsp-proxy--TextDocumentPosition))
   :success-fn #'lsp-proxy--process-locations))

(defun lsp-proxy-find-type-definition ()
  "Find type definition."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/typeDefinition
   (lsp-proxy--request-or-notify-params (lsp-proxy--TextDocumentPosition))
   :success-fn #'lsp-proxy--process-locations))

(defun lsp-proxy-find-implementations ()
  "Find definition."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/implementation
   (lsp-proxy--request-or-notify-params (lsp-proxy--TextDocumentPosition))
   :success-fn #'lsp-proxy--process-locations))

(define-derived-mode lsp-proxy-help-mode help-mode "LspProxyHelp"
  "Major mode for displaying lsp help.")

(defun lsp-proxy-describe-thing-at-point ()
  "Display the type signature and documentation of the thing at point."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/hover
   (lsp-proxy--request-or-notify-params (lsp-proxy--TextDocumentPosition))
   :success-fn (lambda (hover-help)
                 (if (and hover-help (not (equal hover-help "")))
                     (with-current-buffer (get-buffer-create lsp-proxy-hover-buffer)
                       (let ((delay-mode-hooks t))
                         (lsp-proxy-help-mode)
                         (with-help-window lsp-proxy-hover-buffer
                           (insert (lsp-proxy--format-markup hover-help))))
                       (run-mode-hooks))
                   (lsp-proxy--info "%s" "No content at point.")))))

(defvar lsp-proxy--highlights nil "Overlays for textDocument/documentHighlight.")

(defun lsp-proxy-hover-eldoc-function (_cb)
  "A member of `eldoc-documentation-function', for hover."
  (when (and lsp-proxy--support-document-highlight (not (lsp-proxy--progressing-p (lsp-proxy-project-root))))
    (let ((buf (current-buffer)))
      (lsp-proxy--async-request
       'textDocument/documentHighlight
       (lsp-proxy--request-or-notify-params (lsp-proxy--TextDocumentPosition))
       :success-fn
       (lambda (highlights)
         (mapc #'delete-overlay lsp-proxy--highlights)
         (setq lsp-proxy--highlights
               (lsp-proxy--when-buffer-window buf
                 (mapcar (lambda (highlight)
                           (let* ((range (plist-get highlight :range)))
                             (pcase-let ((`(,beg . ,end)
                                          (lsp-proxy--range-region range)))
                               (let ((ov (make-overlay beg end)))
                                 (overlay-put ov 'face 'lsp-proxy-highlight-symbol-face)
                                 (overlay-put ov 'modification-hooks
                                              `(,(lambda (o &rest _) (delete-overlay o))))
                                 ov))))
                         highlights))))
       :deferred 'textDocument/documentHighlight)
      nil))
  t)

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
    (append (list
             :options
             (list
              :tabSize (symbol-value (lsp-proxy--get-indent-width major-mode))
              :insertSpaces (not indent-tabs-mode)
              :trimTrailingWhitespace lsp-proxy-trim-trailing-whitespace
              :insertFinalNewline lsp-proxy-insert-final-newline
              :trimFinalNewlinesmm lsp-proxy-trim-final-newlines))
            (lsp-proxy--TextDocumentIdentifier)))
   :success-fn (lambda (edits)
                 (if (and edits (> (length edits) 0))
                     (progn
                       (lsp-proxy--apply-text-edits edits)
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
  (setq lsp-proxy--last-inserted-char last-input-event)
  (lsp-proxy--maybe-enable-signature-help))

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

;;; inline completion

(defface lsp-proxy-inline-completion-overlay
  '((t :inherit shadow))
  "Face for displaying inline completion."
  :group 'lsp-proxy)

;; these functions are copied from copilot.el
(defmacro lsp-proxy--satisfy-predicates (enable disable)
  "Return t if satisfy all predicates in ENABLE and none in DISABLE."
  `(and (cl-every (lambda (pred)
                    (if (functionp pred) (funcall pred) t))
                  ,enable)
        (cl-notany (lambda (pred)
                     (if (functionp pred) (funcall pred) nil))
                   ,disable)))

(defun lsp-proxy--inline-completion-satisfy-predicates ()
  (lsp-proxy--satisfy-predicates lsp-proxy-inline-completion-enable-predicates
                                 lsp-proxy-inline-completion-disable-predicates))


(defun lsp-proxy--inline-completion-delete-overlay ()
  (when lsp-proxy--inline-completion-preview-overlay
    (delete-overlay lsp-proxy--inline-completion-preview-overlay)
    (setq-local lsp-proxy--inline-completion-preview-overlay nil)))

(defun lsp-proxy--inline-completion-overlay-avaible ()
  (and lsp-proxy--inline-completion-preview-overlay
       (overlayp lsp-proxy--inline-completion-preview-overlay)
       (overlay-buffer lsp-proxy--inline-completion-preview-overlay)))

;; TODO implement SelectedCompletionInfo
(defun lsp-proxy--inline-completion (&optional trigger-kind)
  (lsp-proxy--async-request
   'textDocument/inlineCompletion
   (lsp-proxy--request-or-notify-params
    (append (lsp-proxy--TextDocumentPosition)
            `(:context (:triggerKind ,lsp-proxy-inline-completion-trigger-kind)))
    `(:context
      (:triggerKind ,(or trigger-kind lsp-proxy--inline-completion-trigger-by)
                    :selectedCompletionInfo nil)))
   :success-fn
   (lambda (resp)
     (let* ((cands (--map (plist-get it :insertText) resp))
            (cand (car cands))) ;; TODO make all candidates available
       (when cand (lsp-proxy--inline-completion-update-overlay cand))))))

(defun lsp-proxy--inline-completion-update-overlay (&optional cand)
  "Updates the inline preview overlay.
Update existing overlay when inserting new character,or make a new one
if CAND is non-nil."
  (if cand
      (let* ((pos (point))
             (ov (make-overlay pos pos)))
        (add-text-properties 0 (length cand) '(cursor 1) cand)
        (add-text-properties 0 (length cand)
                             '(face lsp-proxy-inline-completion-overlay) cand)
        (overlay-put ov 'after-string cand)
        (lsp-proxy--inline-completion-delete-overlay)
        (setq-local lsp-proxy--inline-completion-preview-overlay ov))

    (when (and (eq this-command 'self-insert-command)
               (overlayp lsp-proxy--inline-completion-preview-overlay))
      (let* ((ov lsp-proxy--inline-completion-preview-overlay)
             (completion (overlay-get ov 'after-string)))
        (when (eq last-command-event (elt completion 0))
              (if (= (length completion) 1)
                  (lsp-proxy--inline-completion-delete-overlay) ;; completion complete
                (move-overlay ov (point) (point))
                (overlay-put ov 'after-string (substring cand 1)))))))

  (if (lsp-proxy--inline-completion-overlay-avaible)
      (lsp-proxy--inline-completion-active-mode 1)))

(defun lsp-proxy--inline-completion-post-command ()
  (let ((internal-p (and (memq this-command lsp-proxy--inline-completion-commands))))

    (unless internal-p
      (lsp-proxy--inline-completion-update-overlay)
      (lsp-proxy--inline-completion-active-mode -1))

    (when (timerp lsp-proxy--inline-completion-preview-timer)
      (cancel-timer lsp-proxy--inline-completion-preview-timer)
      (setq lsp-proxy--inline-completion-preview-timer nil))

    (when (and (numberp lsp-proxy-inline-completion-idle-delay)
               (lsp-proxy--inline-completion-satisfy-predicates))
      (setq lsp-proxy--inline-completion-preview-timer
            (run-with-idle-timer lsp-proxy-inline-completion-idle-delay
                                 nil
                                 'lsp-proxy--inline-completion
                                 2
                                 )))))

;;;###autoload
(defun lsp-proxy-inline-completion-trigger ()
  "Manually trigger inline completion.
This command can be used without activating `lsp-proxy-inline-completion-mode'."
  (interactive)
  (lsp-proxy--inline-completion 1))

;;;###autoload
(defun lsp-proxy-inline-completion-cancel ()
  "Cancel current inline completion, if any."
  (lsp-proxy--inline-completion-delete-overlay)
  (lsp-proxy--inline-completion-active-mode -1))

;; TODO accept word-wise, line-wise, etc
;;;###autoload
(defun lsp-proxy-inline-completion-complete ()
  "Complete shown text."
  (interactive)
  (when (overlayp lsp-proxy--inline-completion-preview-overlay)
    (let* ((ov lsp-proxy--inline-completion-preview-overlay)
           (content (overlay-get ov 'after-string)))
      (insert-and-inherit (substring-no-properties content)))
    (lsp-proxy--inline-completion-delete-overlay))
  (lsp-proxy--inline-completion-active-mode -1))

;; NOTE check `completion-preview-active-mode'.
;; copilot.el use a 1-width invisible overlay to obtain keymap precedence.
;; See discussions at https://github.com/copilot-emacs/copilot.el/issues/251.
;; I prefer to create a new minor-mode to do this, as `completion-preview-mode'
;; and `corfu-mode'.
(define-minor-mode lsp-proxy--inline-completion-active-mode
  "Active mode for inline-completion.
This is used for keymapping."
  :interactive nil
  (if lsp-proxy--inline-completion-active-mode
      (progn
        (setf (alist-get 'lsp-proxy--inline-completion-active-mode
                         minor-mode-overriding-map-alist)
              lsp-proxy--inline-completion-active-mode-map))
    (lsp-proxy--inline-completion-delete-overlay)))

;;;###autoload
(define-minor-mode lsp-proxy-inline-completion-mode
  "Auto inline complete mode.
Check `lsp-proxy--inline-completion-active-mode-map' for keys and available commands."
  :lighter "CP"
  (if lsp-proxy-inline-completion-mode
      (add-hook 'post-command-hook 'lsp-proxy--inline-completion-post-command nil t)
    (remove-hook 'post-command-hook 'lsp-proxy--inline-completion-post-command t)
    (when lsp-proxy--inline-completion-active-mode
      (lsp-proxy--inline-completion-active-mode -1))
    (when (timerp lsp-proxy--inline-completion-preview-timer)
          (cancel-timer lsp-proxy--inline-completion-preview-timer)
          (setq lsp-proxy--inline-completion-preview-timer nil))))


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
                           (lsp-proxy--TextDocumentPosition)
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
                  (replaceStart (lsp-proxy--position-point (plist-get range :start)))
                  (replaceEnd (lsp-proxy--position-point (plist-get range :end)))
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
        (lsp-proxy--apply-text-edits additionalTextEdits)
      (if-let* ((resolved-item (get-text-property 0 'resolved-item candidate)))
          (if-let* ((additionalTextEdits (plist-get resolved-item :additionalTextEdits)))
              (lsp-proxy--apply-text-edits additionalTextEdits))
        (-let [(callback cleanup-fn) (lsp-proxy--create-apply-text-edits-handlers)]
          (lsp-proxy--async-resolve proxy-item callback cleanup-fn))))))

(defun lsp-proxy--candidate-kind (item)
  "Return ITEM's kind."
  (let* ((proxy-item (get-text-property 0 'lsp-proxy--item item))
         (completion-item (plist-get proxy-item :item))
         (kind (and completion-item (plist-get completion-item :kind))))
    (alist-get kind lsp-proxy--kind->symbol)))

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
     (when-let* ((kind-name (alist-get kind lsp-proxy--kind->symbol)))
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
              (formatted (lsp-proxy--format-markup documentation)))
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

;;
;; Signature
;;
(defvar lsp-proxy-signature-mode-map
  (-doto (make-sparse-keymap)
    (define-key (kbd "M-n") #'lsp-proxy-signature-next)
    (define-key (kbd "M-p") #'lsp-proxy-signature-previous)
    ;; (define-key (kbd "M-a") #'lsp-proxy-signature-toggle-full-docs)
    (define-key (kbd "C-c C-k") #'lsp-proxy-signature-stop)
    (define-key (kbd "M-k") #'lsp-proxy-signature-stop)
    ;; (define-key (kbd "<ESC>") #'lsp-proxy-signature-stop)
    (define-key (kbd "C-g") #'lsp-proxy-signature-stop))
  "Keymap for `lsp-proxy-signature-mode'.")

(defun lsp-proxy-signature-next ()
  "Show next signature."
  (interactive)
  (let ((nsigs (length (plist-get lsp-proxy--signature-last :signatures))))
    (when (and lsp-proxy--signature-last-index
               lsp-proxy--signature-last
               nsigs)
      (setq lsp-proxy--signature-last-index (% (1+ lsp-proxy--signature-last-index) nsigs))
      (lsp-proxy-signature-posframe (lsp-proxy--signature->message lsp-proxy--signature-last)))))

(defun lsp-proxy-signature-previous ()
  "Show previous signature."
  (interactive)
  (when (and lsp-proxy--signature-last-index
             lsp-proxy--signature-last)
    (setq lsp-proxy--signature-last-index (1- (if (zerop lsp-proxy--signature-last-index)
                                                    (length (plist-get lsp-proxy--signature-last :signatures))
                                                  lsp-proxy--signature-last-index)))
    (lsp-proxy-signature-posframe (lsp-proxy--signature->message lsp-proxy--signature-last))))

(define-minor-mode lsp-proxy-signature-mode
  "Mode used to show signature popup."
  :keymap lsp-proxy-signature-mode-map
  :lighter ""
  :group 'lsp-proxy-mode)

(defun lsp-proxy-signature-stop ()
  "Stop showing current signature help."
  (interactive)
  (setq lsp-proxy--signature-last nil)
  ;; TODO cancel request?
  (remove-hook 'post-command-hook #'lsp-proxy-signature)
  (lsp-proxy-signature-posframe nil)
  (lsp-proxy-signature-mode -1))

(declare-function page-break-lines--update-display-tables "ext:page-break-lines")

(defun lsp-proxy--setup-page-break-mode-if-present ()
  "Enable `page-break-lines-mode' in current buffer."
  (when (fboundp 'page-break-lines-mode)
    (page-break-lines-mode)
    ;; force page-break-lines-mode to update the display tables.
    (page-break-lines--update-display-tables)))

(declare-function posframe-show "ext:posframe")
(declare-function posframe-hide "ext:posframe")
(declare-function posframe-poshandler-point-bottom-left-corner-upward "ext:posframe")

(defface lsp-proxy-signature-posframe
  '((t :inherit tooltip))
  "Background and foreground for `lsp-proxy-signature-posframe'."
  :group 'lsp-mode)

(defvar lsp-proxy-signature-posframe-params
  (list :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
        :height 10
        :width 60
        :border-width 1
        :min-width 60)
  "Params for signature and `posframe-show'.")

(defun lsp-proxy-signature-posframe (str)
  "Use posframe to show the STR signatureHelp string."
  (if str
      (apply #'posframe-show
             (with-current-buffer (get-buffer-create lsp-proxy-signature-buffer)
               (erase-buffer)
               (insert str)
               (visual-line-mode)
               (lsp-proxy--setup-page-break-mode-if-present)
               (current-buffer))
             (append
              lsp-proxy-signature-posframe-params
              (list :position (point)
                    :background-color (face-attribute 'lsp-proxy-signature-posframe :background nil t)
                    :foreground-color (face-attribute 'lsp-proxy-signature-posframe :foreground nil t)
                    :border-color (face-attribute 'font-lock-comment-face :foreground nil t))))
    (posframe-hide lsp-proxy-signature-buffer)))

(defun lsp-proxy-signature-activate ()
  "Activate signature help.
It will show up only if current point has signature help."
  (interactive)
  (setq lsp-proxy--signature-last nil
        lsp-proxy--signature-last-index nil
        lsp-proxy--signature-last-buffer (current-buffer))
  (add-hook 'post-command-hook #'lsp-proxy-signature)
  (lsp-proxy-signature-mode t))

(defun lsp-proxy--maybe-enable-signature-help ()
  "Hook function of `post-self-insert-hook'."
  (when (and lsp-proxy-signature-auto-active lsp-proxy--signature-trigger-characters)
    (let ((ch last-command-event))
      (when (cl-find ch lsp-proxy--signature-trigger-characters :key #'string-to-char)
        (lsp-proxy-signature-activate)))))

(defun lsp-proxy--signature->message (signature-help)
  "Generate eldoc message form SIGNATURE-HELP response."
  (setq lsp-proxy--signature-last signature-help)
  (when (and signature-help (not (seq-empty-p (plist-get signature-help :signatures))))
    (let* ((signatures (plist-get signature-help :signatures))
           (active-signature (plist-get signature-help :activeSignature))
           (active-parameter (plist-get signature-help :activeParameter))
           (active-signature (or lsp-proxy--signature-last-index active-signature 0))
           (_ (setq lsp-proxy--signature-last-index active-signature))
           (signature (seq-elt signatures active-signature))
           (label (plist-get signature :label))
           (parameters (plist-get signature :parameters))
           (prefix (if (= (length signatures) 1)
                       ""
                     (concat (propertize (format " %s/%s"
                                                 (1+ active-signature)
                                                 (length signatures))
                                         'face 'success)
                             " "))))
      (when (and active-parameter (not (seq-empty-p parameters)))
        (when-let* ((param (when (and (< -1 active-parameter (length parameters)))
                             (seq-elt parameters active-parameter)))
                    (selected-param-label (let ((label (plist-get param :label)))
                                            (if (stringp label) label (append label nil))))
                    (start (if (stringp selected-param-label)
                               (s-index-of selected-param-label label)
                             (cl-first selected-param-label)))
                    (end (if (stringp selected-param-label)
                             (+ start (length selected-param-label))
                           (cl-second selected-param-label))))
          (add-face-text-property start end 'eldoc-highlight-function-argument nil label)))
      (concat prefix label))))

(defun lsp-proxy--handle-signature-update (signature)
  "Update SIGNATURE."
  (let ((message (lsp-proxy--signature->message signature)))
    (if (and (s-present? message) lsp-proxy-signature-mode)
        (lsp-proxy-signature-posframe message)
      (lsp-proxy-signature-stop))))

(defun lsp-proxy-signature ()
  "Display signature info (based on `textDocument/signatureHelp')."
  ;; (message "input %s char %s charp %s" last-input-event last-command-event (characterp last-command-event))
  ;; (message "type %s" (event-basic-type last-command-event))
  ;; (message "string type %s" (string (event-basic-type last-command-event)))
  (if (and lsp-proxy--signature-last-buffer
           (not (equal (current-buffer) lsp-proxy--signature-last-buffer)))
      (lsp-proxy-signature-stop)

    (lsp-proxy--async-request
     'textDocument/signatureHelp
     (lsp-proxy--request-or-notify-params
      (lsp-proxy--TextDocumentPosition))
     :success-fn #'lsp-proxy--handle-signature-update)
    ;; (message "char %s" (char-to-string last-inp))
    ;; 非输入字符则重新触发一次请求
    ;; (if (or (characterp last-command-event) (memq (event-basic-type last-command-event) lsp-proxy-signature-retrigger-keys))
    ;;     (if (and lsp-proxy--signature-last)
    ;;         (lsp-proxy--handle-signature-update lsp-proxy--signature-last)
    ;;       (lsp-proxy--async-request
    ;;        'textDocument/signatureHelp
    ;;        (lsp-proxy--request-or-notify-params
    ;;         (lsp-proxy--TextDocumentPosition)
    ;;         ;; `(:context (:signature-trigger-character ,(char-to-string last-command-event)))
    ;;         )
    ;;        :success-fn #'lsp-proxy--handle-signature-update))
    ;;   ;; (lsp-proxy-signature-stop)
    ;;   (message "----")
    ;;   )
    ))

;;
;; rename
;;
(defun lsp-proxy--get-symbol-to-rename ()
  "Get a symbol to rename and placeholder at point.
Returns a cons ((START . END) . PLACEHOLDER?), and nil if
renaming is generally supported but cannot be done at point.
START and END are the bounds of the identifiers being renamed,
while PLACEHOLDER?, is either nil or a string suggested by the
language server as the initial input of a new-name prompt."
  (when-let* ((bounds (bounds-of-thing-at-point 'symbol)))
    (cons bounds nil)))

(defface lsp-proxy-face-rename '((t :underline t))
  "Face used to highlight the identifier being renamed.
Renaming can be done using `lsp-rename'."
  :group 'lsp-proxy-mode)

(defface lsp-proxy-rename-placeholder-face '((t :inherit font-lock-variable-name-face))
  "Face used to display the rename placeholder in.
When calling `lsp-rename' interactively, this will be the face of
the new name."
  :group 'lsp-proxy-mode)

(defun lsp-proxy--read-rename (at-point)
  "Read a new name for a `lsp-proxy-rename' at `point' from the user.
AT-POINT shall be a structure as returned by
`lsp-proxy--get-symbol-to-rename'.

Returns a string, which should be the new name for the identifier at point.
If renaming cannot be done at point (as determined from AT-POINT),
throw a `user-error'.

This function is for use in `lsp-proxy-rename' only, and shall not be
relied upon."
  (unless at-point
    (user-error "`lsp-proxy-rename' is invalid here"))
  (-let* ((((start . end) . placeholder?) at-point)
          ;; Do the `buffer-substring' first to not include `lsp-face-rename'
          (rename-me (buffer-substring start end))
          (placeholder (or placeholder? rename-me))
          (placeholder (propertize placeholder 'face 'lsp-proxy-rename-placeholder-face))
          overlay)
    ;; We need unwind protect, as the user might cancel here, causing the
    ;; overlay to linger.
    (unwind-protect
        (progn
          (setq overlay (make-overlay start end))
          (overlay-put overlay 'face 'lsp-proxy-face-rename)

          (read-string (format "Rename %s to: " rename-me) placeholder
                       'lsp-rename-history))
      (and overlay (delete-overlay overlay)))))

(defun lsp-proxy-rename (newname)
  "Rename the symbol (and all references to it) under point to NEWNAME."
  (interactive (list (lsp-proxy--read-rename (lsp-proxy--get-symbol-to-rename))))
  (lsp-proxy--async-request
   'textDocument/rename
   (lsp-proxy--request-or-notify-params
    (append (lsp-proxy--TextDocumentPosition) `(:newName ,newname)))
   :success-fn (lambda (edits)
                 (if edits
                     (lsp-proxy--apply-workspace-edit edits t)
                   (lsp-proxy--warn "%s" "Server does not support rename.")))))

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
                          (start-point (lsp-proxy--position-point start))
                          (end-point (lsp-proxy--position-point end)))
                     (when (= start-point end-point)
                       (if-let* ((region (flymake-diag-region (current-buffer)
                                                              (1+ start-line)
                                                              character)))
                           (setq start-point (car region)
                                 end-point (cdr region))
                         (lsp-proxy--save-restriction-and-excursion
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
            (let* ((b (lsp-proxy--position-point start))
                   (e (lsp-proxy--position-point end)))
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
(defun lsp-proxy--apply-workspace-edit (wedit &optional confirm)
  "Apply workspace edit WEDIT with CONFIRM."
  (let ((changes (plist-get wedit :changes))
        (documentChanges (plist-get wedit :documentChanges))
        (confirmed t)
        kind
        all-edits)
    (if documentChanges
        (dolist (dc (seq-into documentChanges 'list))
          (setq kind (plist-get dc :kind))
          (if kind
              (cl-pushnew (list :kind kind :change dc) all-edits)
            (cl-pushnew (list :kind "documentChange" :change dc) all-edits)))
      (when changes
        (cl-loop for (uri edits) on changes by #'cddr
                 do (push (list :kind "change" :change (list :uri uri :edits edits)) all-edits))))
    (setq all-edits (reverse all-edits))
    (when confirm
      (if (length> all-edits 0)
          (unless (y-or-n-p
                   (format "[LSP-PROXY] Server wants to:\n %s\n Proceed? "
                           (mapconcat #'identity (mapcar (lambda (edit)
                                                           (let ((kind (plist-get edit :kind))
                                                                 (change (plist-get edit :change)))
                                                             (cond
                                                              ((equal kind "change")
                                                               (format "edit %s" (plist-get change :uri)))
                                                              ((equal kind "documentChange")
                                                               (format "edit %s" (plist-get (plist-get change :textDocument) :uri)))
                                                              ((equal kind "rename")
                                                               (format "rename %s to %s" (plist-get change :oldUri) (plist-get change :newUri)))
                                                              ((equal kind "delete")
                                                               (format "delete %s" (plist-get "uri" change)))
                                                              ((equal kind "create")
                                                               (format "create %s" (plist-get change :uri))))))
                                                         all-edits)
                                      "\n ")))
            (setq confirmed nil)
            (lsp-proxy--info "%s" "User cancelled server edit"))
        (lsp-proxy--info "%s" "No edits to apply")
        (setq confirmed nil)))
    (when (and confirmed (length> all-edits 0))
      (let (change
            kind
            textDocument filename edits version)
        (dolist (aedits all-edits)
          (setq change (plist-get aedits :change))
          (setq kind (plist-get aedits :kind))
          (cond
           ((equal kind "change")
            (setq filename (lsp-proxy--uri-to-path (plist-get change :uri))
                  edits (plist-get change :edits)
                  version nil)
            (with-current-buffer (find-file-noselect filename)
              (lsp-proxy--info "lsp-proxy--apply-text-edit filename %s" filename)
              (lsp-proxy--apply-text-edits edits version)))
           ((equal kind "documentChange")
            (setq textDocument (plist-get change :textDocument)
                  edits (plist-get change :edits))
            (setq filename (lsp-proxy--uri-to-path (plist-get textDocument :uri))
                  version (plist-get textDocument :version))
            (with-current-buffer (find-file-noselect filename)
              (lsp-proxy--warn "lsp-proxy--apply-text-edit filename %s" filename)
              (lsp-proxy--apply-text-edits edits version)))
           (t
            (lsp-proxy--warn "lsp-proxy--apply-file-edits filename %s" filename)
            (lsp-proxy--apply-text-document-edit change))))))))

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

(defun lsp-proxy--text-document-code-action-params ()
  "Code action params."
  (append
   `(:range ,(if (use-region-p)
                 (lsp-proxy--region-range (region-beginning) (region-end))
               (lsp-proxy--region-range (point) (point)))
     :context (:diagnostics ,(vector)))
   (lsp-proxy--TextDocumentIdentifier)))

(defun lsp-proxy--code-actions-at-point ()
  "Retrieve the code actions for the active region or the current line."
  (lsp-proxy--request 'textDocument/codeAction (lsp-proxy--request-or-notify-params (lsp-proxy--text-document-code-action-params))))

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
      (lsp-proxy--apply-workspace-edit edit))
    (when command
      (lsp-proxy--execute-command (plist-get command :command) (plist-get command :arguments) ls-id))))

;; inlay hints
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
                         (lsp-proxy--when-live-buffer buf
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
  ;; (message "from %s to %s start %s end %s" from to (window-start) (window-end nil t))
  (let* ((buf (current-buffer))
         (paint-hint
          (lambda (hint)
            (cl-block nil
              (let* ((position (plist-get hint :position))
                     (paddingLeft (plist-get hint :paddingLeft))
                     (paddingRight (plist-get hint :paddingRight))
                     (kind (plist-get hint :kind))
                     (label (plist-get hint :label)))
                (goto-char (lsp-proxy--position-point position))
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
      (append `(:range (:start ,(lsp-proxy--point-position from)
                        :end ,(lsp-proxy--point-position to)))
              (lsp-proxy--TextDocumentIdentifier)))
     :success-fn (lambda (hints)
                   (lsp-proxy--when-live-buffer buf
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
   (lsp-proxy--request-or-notify-params `(:uri ,(lsp-proxy--get-uri)))
   :success-fn (lambda (resp)
                 (message "resp %s" resp))))

;;
;; hooks
;;
(defun lsp-proxy--before-change (beg end)
  "Hook onto `before-change-functions' with BEG and END."
  (when (listp lsp-proxy--recent-changes)
    (push `(,(lsp-proxy--point-position beg)
            ,(lsp-proxy--point-position end)
            (,beg . ,(copy-marker beg nil))
            (,end . ,(copy-marker end t)))
          lsp-proxy--recent-changes)))

(defun lsp-proxy--after-change (beg end pre-change-length)
  "Hook onto `after-change-functions'.
Records BEG, END and PRE-CHANGE-LENGTH locally."
  (cl-incf lsp-proxy--doc-version)
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
           nil (lambda () (lsp-proxy--when-live-buffer buf
                            (when lsp-proxy-mode
                              (lsp-proxy--send-did-change)
                              (setq lsp-proxy--change-idle-timer nil))))))))

(defun lsp-proxy--before-revert-hook ()
  "Hook of `before-revert-hook'."
  (lsp-proxy--on-doc-close))

(defun lsp-proxy--after-revert-hook ()
  "Hook of `after-revert-hook'."
  (lsp-proxy--on-doc-focus (selected-window)))

(defun lsp-proxy--post-command-hook ()
  "Post command hook."
  (lsp-proxy--idle-reschedule (current-buffer))
  (let ((this-command-string (format "%s" this-command)))
    (when lsp-proxy-mode
      (posframe-hide lsp-proxy-hover-buffer))))

(defun lsp-proxy--mode-off ()
  "Turn off `lsp-proxy-mode' unconditionally."
  (remove-overlays nil nil 'lsp-proxy--overlay t)
  (lsp-proxy-inlay-hints-mode -1)
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
    (setq eldoc-documentation-strategy #'eldoc-documentation-compose)
    (add-hook 'eldoc-documentation-functions #'lsp-proxy-hover-eldoc-function nil t)
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
  (when lsp-proxy--connection
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

(defun lsp-proxy-toggle-trace-io ()
  "Toggle jsonrpc logging."
  (interactive)
  (setq lsp-proxy-log-max (if lsp-proxy-log-max nil 0))
  (lsp-proxy-restart)
  (lsp-proxy--info "JSON-RPC logging %s." (if lsp-proxy-log-max "disabled" "enabled")))

(defun lsp-proxy--server-transform (it)
  "Transform server IT to a `(name . it)' format."
  (let* ((name (plist-get it :name)))
    (cons (format "%s" name) it)))

(defun lsp-proxy--select-server (servers)
  "Select a server in SERVERS to restart."
  (cond
   ((seq-empty-p servers) (lsp-proxy--info "%s" "No server associated.") nil)
   (t (let* ((completion-ignore-case t)
             (collection (seq-into servers 'list))
             (col (mapcar #'lsp-proxy--server-transform collection))
             (completion (completing-read "Select a server: "
                                          (lambda (string pred action)
                                            (if (eq action 'metadata)
                                                `(metadata (display-sort-function . identity))
                                              (complete-with-action action col string pred))) nil t)))
        (cdr (assoc completion col))))))

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

(defvar lsp-proxy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-S-SPC") #'lsp-proxy-signature-activate)
    (define-key map (kbd "M-,") #'lsp-proxy-signature-activate)
    map))

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

;;;###autoload
(define-minor-mode lsp-proxy-mode
  "Minor mode for Lsp-Proxy."
  :map lsp-proxy-mode-map
  :init-value nil
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
