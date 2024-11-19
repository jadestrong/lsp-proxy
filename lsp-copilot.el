;;; lsp-copilot.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 JadeStrong
;;
;; Author: JadeStrong <jadestrong@163.com>
;; Maintainer: JadeStrong <jadestrong@163.com>
;; Created: December 15, 2023
;; Modified: December 15, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/bytedance/lsp-copilot
;; Package-Requires: ((emacs "29.1") (s "1.13.1") (ht "2.4") (posframe "1.4.4") (dash "2.19.1") (f "0.21.0") (yasnippet "0.14.1"))
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

(declare-function projectile-project-root "ext:projectile")
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

(defcustom lsp-copilot-user-languages-config (expand-file-name (concat user-emacs-directory (file-name-as-directory "lsp-copilot") "languages.toml"))
  "The user config file to store custom language config."
  :type 'string
  :group 'lsp-copilot)

(defcustom lsp-copilot-log-file-directory temporary-file-directory
  "The directory for `lsp-copilot` server to generate log file."
  :type 'string
  :group 'lsp-copilot)

(defcustom lsp-copilot-log-max 0
  "Max size of events buffer. 0 disables, nil means infinite.
Enabling event logging may slightly affect performance."
  :group 'lsp-copilot
  :type 'integer)

(defcustom lsp-copilot-log-buffer-max message-log-max
  "Maximum number of lines to keep in th elog buffer.
If nil, disable message logging.  If t, log messages but don’t truncate
the buffer when it becomes large."
  :group 'lsp-copilot
  :type '(choice (const :tag "Disable" nil)
          (integer :tag "lines")
          (const :tag "Unlimited" t)))

(defcustom lsp-copilot--send-changes-idle-time 0
  "Don't tell server of changes before Emacs's been idle for this many seconds."
  :group 'lsp-copilot
  :type 'number)

(defcustom lsp-copilot-idle-delay 0.500
  "Debounce interval for `after-change-functions'."
  :type 'number
  :group 'lsp-copilot)

(defcustom lsp-copilot-on-idle-hook nil
  "Hooks to run after `lsp-copilot-idle-delay'."
  :type 'hook
  :group 'lsp-copilot)

(defcustom lsp-copilot-hover-buffer "*lsp-copilot-help*"
  "Buffer for display hover info."
  :type 'string
  :group 'lsp-copilot)

(defcustom lsp-copilot-diagnostics-buffer "*lsp-copilot-diagnostics*"
  "Buffer for display diagnostics."
  :type 'string
  :group 'lsp-copilot)

(defcustom lsp-copilot-signature-buffer " *lsp-copilot-signature*"
  "Buffer for display signature help info."
  :type 'string
  :group 'lsp-copilot)

(defcustom lsp-copilot-signature-auto-active nil
  "If auto active signature help."
  :type 'boolean
  :group 'lsp-copilot)

(defcustom lsp-copilot-trim-trailing-whitespace t
  "Trim trailing whitespace on a line."
  :group 'lsp-copilot
  :type 'boolean)

(defcustom lsp-copilot-insert-final-newline t
  "Insert a newline character at the end of the file if one does not exist."
  :group 'lsp-copilot
  :type 'boolean)

(defcustom lsp-copilot-trim-final-newlines t
  "Trim all newlines after the final newline at the end of the file."
  :group 'lsp-copilot
  :type 'boolean)

(defcustom lsp-copilot-log-level 1
  "A number indicating the log level. Defaults to 1."
  :type '(choice (const :tag "Warn" 0)
          (const :tag "Info" 1)
          (const :tag "Debug" 2)
          (const :tag "Trace" 3))
  :group 'lsp-copilot)

(defface lsp-copilot-hover-posframe
  '((t :inherit tooltip))
  "Background and foreground for `lsp-copilot-hover-posframe'."
  :group 'lsp-copilot)

(defcustom lsp-copilot-signature-retrigger-keys '(return)
  "Character strings used to retrigger a new textDocument/signatureHelp request."
  :type 'list
  :group 'lsp-copilot-mode)

(defcustom lsp-copilot-diagnostics-provider :auto
  "The checker backend provider."
  :type
  '(choice
    (const :tag "Pick flycheck if present and fallback to flymake" :auto)
    (const :tag "Pick flycheck" :flycheck))
  :group 'lsp-copilot)

(defvar lsp-copilot--exec-file (expand-file-name (if (eq system-type 'windows-nt)
                                                     "./lsp-copilot.exe"
                                                   "./lsp-copilot")
                                                 (if load-file-name
                                                     (file-name-directory load-file-name)
                                                   default-directory)))
(defvar-local lsp-copilot--on-idle-timer nil)

(defvar lsp-copilot--log-file nil
  "The log file name.")

(defvar lsp-copilot--connection nil
  "Lsp Copilot agent jsonrcp connection instnace.")

(defvar lsp-copilot--opened-buffers nil
  "List of buffers that have been opened in Lsp Copilot.")

(defvar-local lsp-copilot--doc-version 0
  "The document version of the current buffer. Incremented after each change.")

(defvar-local lsp-copilot--recent-changes nil
  "Recent buffer changes as collected by `lsp-copilot--before-change'.")

(defvar-local lsp-copilot--change-idle-timer nil
  "Idle timer for didChange signals.")

(defvar-local lsp-copilot--completion-trigger-characters nil
  "Completion trigger characters.")

(defvar-local lsp-copilot--signature-trigger-characters nil
  "Signature trigger characters.")

(defvar-local lsp-copilot-enable-relative-indentation nil
  "Enable relative indentation when insert texts, snippets ...
from language server.")

(defvar-local lsp-copilot-diagnostics--flycheck-enabled nil
  "True when lsp-copilot diagnostics flycheck integration
 has been enabled in this buffer.")

(defvar-local lsp-copilot-diagnostics--flycheck-checker nil
  "The value of flycheck-checker before lsp-copilot diagnostics was activated.")

(defvar-local lsp-copilot--signature-last nil)
(defvar-local lsp-copilot--signature-last-index nil)
(defvar lsp-copilot--signature-last-buffer nil)

(defvar-local lsp-copilot--support-inlay-hints nil
  "Is there any server associated with this buffer that support `textDocument/inlayHint' request.")

(defvar lsp-copilot--show-message t
  "If non-nil, show debug message from `lsp-copilot-mode'.")

(defvar lsp-copilot--formatting-indent-alist
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

(defconst lsp-copilot--kind->symbol
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

(defconst lsp-copilot--message-type-face
  `((1 . ,compilation-error-face)
    (2 . ,compilation-warning-face)
    (3 . ,compilation-message-face)
    (4 . ,compilation-info-face)))

;; progress token map
(defvar lsp-copilot--project-hashmap (make-hash-table :test 'equal))

(defun lsp-copilot--add-project (project-root-path project-map)
  (puthash project-root-path (make-hash-table :test 'equal) project-map))

(defun lsp-copilot--remove-project (project-root-path project-map)
  (if project-root-path
      (remhash project-root-path project-map)))

(defun lsp-copilot--get-or-create-project (project-root-path project-map)
  (or (gethash project-root-path project-map)
      (lsp-copilot--add-project project-root-path project-map)
      (gethash project-root-path project-map)))

(defun lsp-copilot--set-work-done-token (project-root-path token value)
  (let ((project (lsp-copilot--get-or-create-project project-root-path lsp-copilot--project-hashmap)))
    (if project
        (puthash token value project)
      (error "Project not found: %s" project-root-path))))

(defun lsp-copilot--rem-work-done-token (project-root-path token)
  (let ((project (gethash project-root-path lsp-copilot--project-hashmap)))
    (if project
        (remhash token project)
      (error "Project not found: %s" project-root-path))))

;; diagnostics map
(defvar lsp-copilot--diagnostics-map (make-hash-table :test 'equal))

;;
;; schedule
;;
(defun lsp-copilot--idle-reschedule (buffer)
  "LSP copilot idle schedule on current BUFFER."
  (when lsp-copilot--on-idle-timer
    (cancel-timer lsp-copilot--on-idle-timer))
  (setq-local lsp-copilot--on-idle-timer (run-with-idle-timer
                                          lsp-copilot-idle-delay
                                          nil
                                          #'lsp-copilot--on-idle
                                          buffer)))
(defun lsp-copilot--on-idle (buffer)
  "Start post command loop on current BUFFER."
  (when (and (buffer-live-p buffer)
             (equal buffer (current-buffer))
             lsp-copilot-mode)
    (run-hooks 'lsp-copilot-on-idle-hook)))

;; log message
(defun lsp-copilot--message  (format &rest args)
  "Wrapper for `message'

We `inhibit-message' the message when the cursor is in the
minibuffer and when emacs version is before emacs 27 due to the
fact that we often use `lsp--info', `lsp--warn' and `lsp--error'
in async context and the call to these function is removing the
minibuffer prompt. The issue with async messages is already fixed
in emacs 27.

See #2049"
  (when lsp-copilot--show-message
    (let ((inhibit-message (or inhibit-message
                               (and (minibufferp)
                                    (version< emacs-version "27.0")))))
      (apply #'message format args))))

(defun lsp-copilot--info (format &rest args)
  "Display lsp info message with FORMAT with ARGS."
  (lsp-copilot--message "%s :: %s" (propertize "LSP-COPILOT" 'face 'success) (apply #'format format args)))

(defun lsp-copilot--warn (format &rest args)
  "Display lsp warn message with FORMAT with ARGS."
  (lsp-copilot--message "%s :: %s" (propertize "LSP-COPILOT" 'face 'warning) (apply #'format format args)))

(defun lsp-copilot--error (format &rest args)
  "Display lsp error message with FORMAT with ARGS."
  (lsp-copilot--message "%s :: %s" (propertize "LSP-COPILOT" 'face 'error) (apply #'format format args)))

(defun lsp-copilot--propertize (str type)
  "Propertize STR as per TYPE."
  (propertize str 'face (alist-get type lsp-copilot--message-type-face)))

;; Buffer local variable for storing number of lines.
(defvar lsp-copilot--log-lines)
(defun lsp-copilot-log (format &rest args)
  "Log message to the *lsp-copilot-log* buffer.
FORMAT and ARGS is the same as for `messsage'."
  (when lsp-copilot-log-buffer-max
    (let ((log-buffer (get-buffer "*lsp-copilot-log*"))
          (inhibit-read-only t))
      (unless log-buffer
        (setq log-buffer (get-buffer-create "*lsp-copilot-log*"))
        (with-current-buffer log-buffer
          (buffer-disable-undo)
          (view-mode 1)
          (set (make-local-variable 'lsp-copilot--log-lines) 0)))
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

            (setq lsp-copilot--log-lines (+ lsp-copilot--log-lines newlines))

            (when (and (integerp lsp-copilot-log-buffer-max) (> lsp-copilot--log-lines lsp-copilot-log-buffer-max))
              (let ((to-delete (- lsp-copilot--log-lines lsp-copilot-log-buffer-max)))
                (goto-char (point-min))
                (forward-line to-delete)
                (delete-region (point-min) (point))
                (setq lsp-copilot--log-lines lsp-copilot-log-buffer-max)))))))))

;; project root
(defvar-local lsp-copilot--cur-project-root nil)
(defun lsp-copilot-project-root ()
  "Return the project root of current project."
  (if lsp-copilot--cur-project-root
      lsp-copilot--cur-project-root
    ;; TODO make an customizable option
    (let* ((root (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                     (project-root (project-current))))
           (root-path (and root (directory-file-name root))))
      (setq lsp-copilot--cur-project-root root-path)
      root-path)))
;;
;; utils
;;
(eval-and-compile
  (defun lsp-copilot--transform-pattern (pattern)
    "Transform PATTERN to (&plist PATTERN) recursively."
    (cons '&plist
          (mapcar (lambda (p)
                    (if (listp p)
                        (lsp-copilot--transform-pattern p)
                      p))
                  pattern))))

(defmacro lsp-copilot--dbind (pattern source &rest body)
  "Destructure SOURCE against plist PATTERN and eval BODY."
  (declare (indent 2))
  `(-let ((,(lsp-copilot--transform-pattern pattern) ,source))
     ,@body))

(defvar lsp-copilot--already-widened nil)
(defmacro lsp-copilot--save-restriction-and-excursion (&rest form)
  (declare (indent 0) (debug t))
  `(if lsp-copilot--already-widened
       (save-excursion ,@form)
     (let* ((lsp-copilot--already-widened t))
       (save-restriction
         (widen)
         (save-excursion ,@form)))))

(cl-defmacro lsp-copilot--when-live-buffer (buf &rest body)
  "Check BUF live, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf)) (if (buffer-live-p ,b) (with-current-buffer ,b ,@body)))))

(cl-defmacro lsp-copilot--when-buffer-window (buf &body body)
  "Check BUF showing somewhere, then do BODY in it." (declare (indent 1) (debug t))
  (let ((b (cl-gensym)))
    `(let ((,b ,buf))
       ;;notice the exception when testing with `ert'
       (when (or (get-buffer-window ,b) (ert-running-test))
         (with-current-buffer ,b ,@body)))))

(defun lsp-copilot--calculate-column ()
  "Calculate character offset of cursor in current line."
  (/ (- (length
         (encode-coding-region
          (line-beginning-position)
          (min (point) (point-max)) 'utf-16 t))
        2)
     2))

(defun lsp-copilot--get-uri ()
  "Get URI of current buffer."
  (cond
   ((not buffer-file-name)
    (concat "buffer://" (url-encode-url (buffer-name (current-buffer)))))
   ((and (eq system-type 'windows-nt)
         (not (s-starts-with-p "/" buffer-file-name)))
    (concat "file:///" (url-encode-url buffer-file-name)))
   (t
    (concat "file://" (url-encode-url buffer-file-name)))))

(defun lsp-copilot--uri-to-path (uri)
  "Convert URI to a file path."
  (when (keywordp uri)
    (setq uri (substring (symbol-name uri) 1)))
  (let ((retval (url-unhex-string (url-filename (url-generic-parse-url uri)))))
    (if (eq system-type 'windows-nt)
        (substring retval 1)
      retval)))

(defun lsp-copilot--get-source ()
  "Get source code from current buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun lsp-copilot--position ()
  (list :line (1- (line-number-at-pos)) :character (lsp-copilot--calculate-column)))

(defun lsp-copilot--point-position (point)
  "Get position of the POINT."
  (lsp-copilot--save-restriction-and-excursion
    (goto-char point)
    (lsp-copilot--position)))

(defun lsp-copilot--position-point (pos)
  "Convert `Position' object POS to a point"
  (let* ((line (plist-get pos :line))
         (character (plist-get pos :character)))
    (lsp-copilot--line-character-to-point line character)))

(defun lsp-copilot--line-character-to-point (line character)
  "Return the point for character CHARACTER on line LINE."
  (let ((inhibit-field-text-motion t))
    (lsp-copilot--save-restriction-and-excursion
      (goto-char (point-min))
      (forward-line line)
      ;; server may send character position beyond the current line and we
      ;; sould fallback to line end.
      (let* ((line-end (line-end-position)))
        (if (> character (- line-end (point)))
            line-end
          (forward-char character)
          (point))))))

(defun lsp-copilot--position-equal (pos-a pos-b)
  "Return whether POS-A and POS-B positions are equal."
  (and (= (plist-get pos-a :line) (plist-get pos-b :line))
       (= (plist-get pos-a :character) (plist-get pos-b :character))))

(defun lsp-copilot--position-compare (pos-a pos-b)
  "Return t if POS-A if greater thatn POS-B."
  (let* ((line-a (plist-get pos-a :line))
         (line-b (plist-get pos-b :line)))
    (if (= line-a line-b)
        (> (plist-get pos-a :character) (plist-get pos-b :character))
      (> line-a line-b))))

;; TODO fix point if the line or charactor is -1
(defun lsp-copilot--range-region (range)
  "Return region (BEG . END) that represents LSP RANGE.
If optional MARKERS, make markers."
  (let ((beg (lsp-copilot--position-point (plist-get range :start)))
        (end (lsp-copilot--position-point (plist-get range :end))))
    (cons beg end)))

(defun lsp-copilot--region-range (start end)
  "Make Range object for the current region."
  (list :start (lsp-copilot--point-position start)
        :end (lsp-copilot--point-position end)))

(defun lsp-copilot--region-or-line ()
  "The active region or the current line."
  (if (use-region-p)
      (lsp-copilot--region-range (region-beginning) (region-end))
    (lsp-copilot--region-range (line-beginning-position) (line-end-position))))

(defun lsp-copilot--format-markup (markup)
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

(defun lsp-copilot--markdown-render ()
  (when (fboundp 'gfm-view-mode)
    (let ((inhibit-message t))
      (setq-local markdown-fontify-code-blocks-natively t)
      (set-face-background 'markdown-code-face (face-attribute 'lsp-copilot-hover-posframe :background nil t))
      ;; (set-face-attribute 'markdown-code-face nil :height 230)
      (gfm-view-mode)))
  (read-only-mode 0)
  (prettify-symbols-mode 1)
  (display-line-numbers-mode -1)
  (font-lock-ensure)

  (setq-local mode-line-format nil))

(defun lsp-copilot--expand-snippet (snippet &optional start end expand-env)
  "Wrapper of `yas-expand-snippet' with all of it arguments.
The snippet will be convert to LSP style and indent according to
LSP server according to
LSP server result."
  (let* ((inhibit-field-text-motion t)
         (yas-wrap-around-region nil)
         (yas-indent-line 'none)
         (yas-also-auto-indent-first-line nil))
    (yas-expand-snippet snippet start end expand-env)))

(defun lsp-copilot--indent-lines (start end &optional insert-text-mode?)
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
                       lsp-copilot-enable-relative-indentation
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

(defun lsp-copilot--get-file-contents-from-list (paths)
  "Get all file content of PATHS list."
  (apply #'vector
         (mapcar
          (lambda (path)
            (let ((buffer (find-file-noselect path)))
              (with-current-buffer buffer
                (list :path path :content (buffer-substring-no-properties (point-min) (point-max)))))) paths)))

(defun lsp-copilot--TextDocumentIdentifier ()
  "Make a TextDocumentIdentifier object."
  `(:textDocument
    (:uri ,(lsp-copilot--get-uri))))

(defun lsp-copilot--TextDocumentPosition ()
  "Make a TextDocumentPosition object."
  (append `(:position ,(lsp-copilot--position))
          (lsp-copilot--TextDocumentIdentifier)))

(defun lsp-copilot--request-or-notify-params (params &rest args)
  "Wrap request or notify params base PARAMS and add extra ARGS."
  (let ((rest (apply 'append args)))
    (append (list :uri (lsp-copilot--get-uri) :params params) rest)))

(defun lsp-copilot--advice-json-parse (old-fn &rest args)
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
            #'lsp-copilot--advice-json-parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; xref integration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lsp-copilot--xref-backend () "lsp-copilot xref backend." 'xref-lsp-copilot)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-lsp-copilot)))
  (propertize (or (thing-at-point 'symbol) "")
              'identifier-at-point t))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp-copilot)))
  (list (propertize (or (thing-at-point 'symbol) "")
                    'identifier-at-point t)))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-lsp-copilot)) _identifier)
  (save-excursion
    (lsp-copilot-find-definition)))

(cl-defmethod xref-backend-references ((_backend (eql xref-lsp-copilot)) _identifier)
  (save-excursion
    (lsp-copilot-find-references)))

(cl-defmethod xref-backend-implementations ((_backend (eql xref-lsp-copilot)) _identifier)
  (save-excursion
    (lsp-copilot-find-implementations)))

(cl-defmethod xref-backend-type-definitions ((_backend (eql xref-lsp-copilot)) _identifier)
  (save-excursion
    (lsp-copilot-find-type-definition)))


(defcustom lsp-copilot-xref-force-references nil
  "If non-nil threat everything as references(e. g. jump if only one item.)"
  :group 'lsp-copilot
  :type 'boolean)

(defcustom lsp-copilot-progress-prefix "⌛ "
  "Progress prefix."
  :group 'lsp-copilot-mode
  :type 'string)

(defun lsp-copilot-show-xrefs (xrefs display-action references?)
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

(defun lsp-copilot--process-locations (locations)
  "Process LOCATIONS and show xrefs."
  (if (seq-empty-p locations)
      (lsp-copilot--error "Not found for: %s" (or (thing-at-point 'symbol t) ""))
    (when-let* ((locs (cl-mapcar (lambda (it)
                                   (let* ((uri (plist-get it :uri))
                                          (filepath (lsp-copilot--uri-to-path uri))
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
                                                         (let* ((beg (lsp-copilot--position-point start))
                                                                (end (lsp-copilot--position-point end))
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
                                      (t (lsp-copilot--warn "Failed  to process xref entry for file %s" filepath)))))
                                 locations)))
      (lsp-copilot-show-xrefs locs nil nil))))

;;
;; text-edit
;;
(defun lsp-copilot--apply-text-document-edit (change)
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
      (setq filename (lsp-copilot--uri-to-path uri))
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
            (lsp-copilot--warn "Cannot create file %s." filename))
        (when (find-buffer-visiting filename)
          (with-current-buffer (find-buffer-visiting filename)
            (save-buffer)
            (kill-buffer)))
        (delete-file filename t)
        (with-current-buffer (find-file-noselect new-filename)
          (save-buffer))))
     ((equal kind "rename")
      (setq uri (gethash "oldUri" change))
      (setq filename (lsp-copilot--uri-to-path uri))
      (setq new-uri (gethash "newUri" change))
      (setq new-filename (lsp-copilot--uri-to-path new-uri))
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
            (lsp-copilot--warn "Cannot rename %s to %s" filename new-filename))
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
      (setq filename (lsp-copilot--uri-to-path uri))
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
                (lsp-copilot--warn "Cannot delete directory %s" filename)))
          (if (find-buffer-visiting filename)
              (with-current-buffer (find-buffer-visiting filename)
                (save-buffer)
                (kill-buffer)))
          (delete-file filename t)))))))

(defun lsp-copilot--sort-edits (edits)
  (sort edits #'(lambda (edit-a edit-b)
                  (let* ((range-a (plist-get edit-a :range))
                         (range-b (plist-get edit-b :range))
                         (start-a (plist-get range-a :start))
                         (start-b (plist-get range-b :start))
                         (end-a (plist-get range-a :end))
                         (end-b (plist-get range-a :end)))
                    (if (lsp-copilot--position-equal start-a start-b)
                        (lsp-copilot--position-compare end-a end-b)
                      (lsp-copilot--position-compare start-a start-b))))))

(defun lsp-copilot--apply-text-edit (edit)
  "Apply the edits ddescribed in the TextEdit objet in TEXT-EDIT."
  (let* ((start (lsp-copilot--position-point (plist-get (plist-get edit :range) :start)))
         (end (lsp-copilot--position-point (plist-get (plist-get edit :range) :end)))
         (new-text (plist-get edit :newText)))
    (setq new-text (s-replace "\r" "" (or new-text "")))
    (plist-put edit :newText new-text)
    (goto-char start)
    (delete-region start end)
    (insert new-text)))

(defun lsp-copilot--apply-text-edit-replace-buffer-contents (edit)
  "Apply the edits described in the TextEdit object in TEXT-EDIT.
The method uses `replace-buffer-contents'."
  (let* (
         (source (current-buffer))
         (new-text (plist-get edit :newText))
         (region (lsp-copilot--range-region (plist-get edit :range)))
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

(defun lsp-copilot--apply-text-edits (edits &optional version)
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
                    (lsp-copilot--apply-text-edit-replace-buffer-contents edit)
                    (when-let* ((insert-text-format (plist-get edit :insertTextFormat))
                                (start (lsp-copilot--position-point (plist-get (plist-get edit :range) :start)))
                                (new-text (plist-get edit :newText)))
                      (when (eq insert-text-format 2)
                        ;; No `save-excursion' needed since expand snippet will change point anyway
                        (goto-char (+ start (length new-text)))
                        (lsp-copilot--indent-lines start (point))
                        (lsp-copilot--expand-snippet new-text start (point))))) (reverse edits))
          (undo-amalgamate-change-group change-group)
          (progress-reporter-done reporter))))))

(defun lsp-copilot--create-apply-text-edits-handlers ()
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
                                                (end-point (lsp-copilot--position-point end)))
                                           (message "range %s end %s" range end)
                                           (> end-point first-edited)))
                          edits))
           (lsp-copilot--warn "TextEdits will not be applied since document has been modified before of them.")
         (lsp-copilot--apply-text-edits edits)))
     (lambda ()
       (remove-hook 'before-change-functions func t)))))

;;
;; modeline
;;
(defun lsp-copilot--progress-status ()
  "Return the status of the progress for the current workspaces."
  (when lsp-copilot-mode
    (let ((progress-status
           (when-let ((tokens (gethash (lsp-copilot-project-root) lsp-copilot--project-hashmap)))
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
        (concat lsp-copilot-progress-prefix progress-status " ")))))
;;
;; agent
;;
(defconst lsp-copilot--ignore-response
  (lambda (_))
  "Simply ignore the response")

(defconst lsp-copilot--show-error
  (lambda (err)
    (lsp-copilot--error "%s" (or (and err (plist-get err :message)) err)))
  "Default handler for error message.")

(defsubst lsp-copilot--connection-alivep ()
  "Non-nil if the `lsp-copilot--connection' is alive."
  (and lsp-copilot--connection
       (zerop (process-exit-status (jsonrpc--process lsp-copilot--connection)))))

(defmacro lsp-copilot--request (&rest args)
  "Send a request to the lsp copilot agent with ARGS."
  `(progn
     (when lsp-copilot-mode
       (unless (lsp-copilot--connection-alivep)
         (lsp-copilot--start-agent))
       (lsp-copilot--send-did-change)
       (unless (-contains-p lsp-copilot--opened-buffers (current-buffer))
         (lsp-copilot--on-doc-open))
       (jsonrpc-request lsp-copilot--connection ,@args))))

(defmacro lsp-copilot--notify (method &rest params)
  "Send a notification to the lsp copilot agent with ARGS."
  `(progn
     (unless (lsp-copilot--connection-alivep)
       (lsp-copilot--start-agent))
     (if (or (eq ,method 'textDocument/didOpen) (eq ,method 'textDocument/willSave) (eq ,method 'textDocument/didSave) (-contains-p lsp-copilot--opened-buffers (current-buffer)))
         (let ((new-params (list :uri (lsp-copilot--get-uri) :params ,@params)))
           (jsonrpc-notify lsp-copilot--connection ,method new-params))
       (lsp-copilot--on-doc-open))))

(cl-defmacro lsp-copilot--async-request (method params &rest args &key (success-fn #'lsp-copilot--ignore-response) (error-fn #'lsp-copilot--show-error) &allow-other-keys)
  "Send an asynchronous request to the lsp copilot agent."
  `(progn
     (unless (lsp-copilot--connection-alivep)
       (lsp-copilot--start-agent))
     (lsp-copilot--send-did-change)
     (unless (-contains-p lsp-copilot--opened-buffers (current-buffer))
       (lsp-copilot--on-doc-open))
     ;; jsonrpc will use temp buffer for callbacks, so we nned to save the current buffer and restore it inside callback
     (let ((buf (current-buffer)))
       (jsonrpc-async-request lsp-copilot--connection
                              ,method ,params
                              :success-fn (lambda (result)
                                            (with-current-buffer buf
                                              (funcall ,success-fn result)))
                              :error-fn (lambda (err)
                                          (funcall ,error-fn err))
                              ,@args))))

(defun lsp-copilot--make-connection ()
  "Establish copilot jsonrpc connection."
  (let ((make-fn (apply-partially
                  #'make-instance
                  'jsonrpc-process-connection
                  :name "lsp copilot"
                  :notification-dispatcher #'lsp-copilot--handle-notification
                  :request-dispatcher #'lsp-copilot--handle-request
                  :process (make-process :name "lsp copilot agent"
                                         :command (list lsp-copilot--exec-file "--config" lsp-copilot-user-languages-config "--log-level" (number-to-string lsp-copilot-log-level) "--log" lsp-copilot--log-file)
                                         :coding 'utf-8-emacs-unix
                                         :connection-type 'pipe
                                         :stderr (get-buffer-create "*lsp copilot stderr*")
                                         :noquery t))))
    (condition-case nil
        (funcall make-fn :events-buffer-config `(:size ,lsp-copilot-log-max))
      (invalid-slot-name
       ;; handle older jsonrpc versions
       (funcall make-fn :events-buffer-scrollback-size lsp-copilot-log-max)))))

(defun lsp-copilot--start-agent ()
  "Start the lsp copilot agent process in local."
  (let* ((timestamp (format-time-string "%Y%m%d%H%M%S"))
         (random-num (random 100000))
         (filename (format "lsp-copilot-%s-%05d.log" timestamp random-num)))
    (setq lsp-copilot--log-file (concat lsp-copilot-log-file-directory filename))
    (if (file-exists-p lsp-copilot--exec-file)
        (progn
          (setq lsp-copilot--connection (lsp-copilot--make-connection))
          (message "Lsp copilot agent started."))
      (lsp-copilot--error "No lsp-copilot file found, please check your `lsp-copilot--exec-file'"))))

(defun lsp-copilot--handle-notification (_ method msg)
  "Handle MSG of type METHOD."
  (when (eql method 'textDocument/publishDiagnostics)
    (lsp-copilot--dbind (:uri uri :diagnostics diagnostics) (plist-get msg :params)
      (let ((filepath (lsp-copilot--uri-to-path uri)))
        (when (f-exists-p filepath)
          (with-current-buffer (find-file-noselect filepath)
            (let ((workspace-diagnostics (lsp-copilot--get-or-create-project
                                          (lsp-copilot-project-root)
                                          lsp-copilot--diagnostics-map)))
              (if (seq-empty-p diagnostics)
                  (remhash filepath workspace-diagnostics)
                (puthash filepath (append diagnostics nil) workspace-diagnostics)))
            ;; (message "%s publish %s" (format-time-string "%H:%M:%S.%6N") uri)
            ;; (setq-local lsp-copilot--record-diagnostics diagnostics)
            (add-hook 'lsp-copilot-on-idle-hook #'lsp-copilot-diagnostics--flycheck-buffer nil t)
            (lsp-copilot--idle-reschedule (current-buffer)))))))
  (when  (eql method 'window/logMessage)
    (lsp-copilot--dbind (:type type :message message) (plist-get msg :params)
      (lsp-copilot-log (lsp-copilot--propertize message type))))
  (when  (eql method 'window/showMessage)
    (lsp-copilot--dbind (:type type :message message) (plist-get msg :params)
      (lsp-copilot--info (lsp-copilot--propertize message type))))
  (when (eql method 'emacs/triggerCharacters)
    (lsp-copilot--dbind (:uri uri :triggerCharacters trigger-characters :signatureTriggerCharacters signature-trigger-characters :supportInlayHints support-inlay-hints) (plist-get msg :params)
      (let* ((filepath (lsp-copilot--uri-to-path uri)))
        (when (f-exists? filepath)
          (with-current-buffer (find-file-noselect filepath)
            (setq-local lsp-copilot--completion-trigger-characters trigger-characters)
            (setq-local lsp-copilot--signature-trigger-characters signature-trigger-characters)
            (setq-local lsp-copilot--support-inlay-hints support-inlay-hints)
            ;; TODO when support and enable, add a idle hook and reschedule this buffer
            )))))
  (when (eql method '$/progress)
    (add-to-list 'global-mode-string '(t (:eval (lsp-copilot--progress-status))))
    (lsp-copilot--dbind (:rootPath root-path :params params) (plist-get msg :params)
      (let* ((token (plist-get params :token))
             (value (plist-get params :value))
             (kind (plist-get value :kind)))
        (pcase kind
          ("begin" (lsp-copilot--set-work-done-token root-path token value))
          ("report" (lsp-copilot--set-work-done-token root-path token value))
          ("end" (lsp-copilot--rem-work-done-token root-path token)))))))

(defun lsp-copilot--handle-request (_ method msg)
  "Handle MSG of type METHOD."
  (when (eql method 'workspace/applyEdit)
    (lsp-copilot--dbind (:edit edit) (plist-get msg :params)
      (lsp-copilot--apply-workspace-edit edit)))
  (when (eql method 'eslint/openDoc)
    (lsp-copilot--dbind (:url url) (plist-get msg :params)
      (browse-url url)))
  (when (eql method 'emacs/getFiles)
    (lsp-copilot--dbind (:paths paths) (plist-get msg :params)
      (list :files (lsp-copilot--get-file-contents-from-list (seq-into paths 'list))))))

;;
;; lsp request/notification
;;
(defun lsp-copilot--on-doc-focus (window)
  "Notify that the document has been focussed or opened."
  ;; When switching windows, this function is called twice, once for the
  ;; window losing and once for the window gaining focus. We only want to
  ;; send a notification for the window gaining focus and only if the buffer has
  ;; lsp-copilot-mode enabled.
  (when (and lsp-copilot-mode (eq window (selected-window)))
    (if (-contains-p lsp-copilot--opened-buffers (current-buffer))
        (lsp-copilot--notify ':textDocument/didFocus
                             (list :textDocument (list :uri (lsp-copilot--get-uri))))
      (lsp-copilot--on-doc-open))))

(defun lsp-copilot--on-doc-open ()
  "On doc open."
  (setq lsp-copilot--recent-changes nil
        lsp-copilot--doc-version 0)
  (when buffer-file-name
    (when (not (f-exists? buffer-file-name))
      (save-buffer))
    (add-to-list 'lsp-copilot--opened-buffers (current-buffer))
    (lsp-copilot--notify 'textDocument/didOpen
                         (list :textDocument (list :uri (lsp-copilot--get-uri)
                                                   :text (lsp-copilot--get-source))))))

(defun lsp-copilot--on-doc-close (&rest _args)
  "Notify that the document has been closed."
  (when (-contains-p lsp-copilot--opened-buffers (current-buffer))
    (lsp-copilot--notify 'textDocument/didClose
                         (list :textDocument (list :uri (lsp-copilot--get-uri))))
    (setq lsp-copilot--opened-buffers (delete (current-buffer) lsp-copilot--opened-buffers))))


(defun lsp-copilot--will-save ()
  "Send textDocument/willSave notification."
  (lsp-copilot--notify 'textDocument/willSave
                       ;; 1 Manual, 2 AfterDelay, 3 FocusOut
                       (append '(:reason 1) (lsp-copilot--TextDocumentIdentifier))))

(defun lsp-copilot--did-save ()
  "Send textDocument/didSave notification."
  (lsp-copilot--notify 'textDocument/didSave
                       (lsp-copilot--TextDocumentIdentifier)))

(defun lsp-copilot--send-did-change ()
  "Send textDocument/didChange to server."
  (when lsp-copilot--recent-changes
    (let ((full-sync-p (eq :emacs-messup lsp-copilot--recent-changes)))
      (lsp-copilot--notify 'textDocument/didChange
                           (list :textDocument
                                 (list :uri (lsp-copilot--get-uri) :version lsp-copilot--doc-version)
                                 :contentChanges
                                 (if full-sync-p
                                     (vector (list :text (lsp-copilot--save-restriction-and-excursion
                                                           (buffer-substring-no-properties (point-min)
                                                                                           (point-max)))))
                                   (cl-loop for (beg end len text) in (reverse lsp-copilot--recent-changes)
                                            when (numberp len)
                                            vconcat `[,(list :range `(:start ,beg :end ,end)
                                                             :rangeLength len :text text)]))))
      (setq lsp-copilot--recent-changes nil))))

(defun lsp-copilot-find-definition ()
  "Find definition."
  (interactive)
  (lsp-copilot--async-request
   'textDocument/definition
   (lsp-copilot--request-or-notify-params (lsp-copilot--TextDocumentPosition))
   :success-fn #'lsp-copilot--process-locations))

(defun lsp-copilot-find-references ()
  "Find references."
  (interactive)
  (lsp-copilot--async-request
   'textDocument/references
   (lsp-copilot--request-or-notify-params (append (lsp-copilot--TextDocumentPosition) `(:context (:includeDeclaration t))))
   :success-fn #'lsp-copilot--process-locations))

(defun lsp-copilot-find-declaration ()
  "Find declaration."
  (interactive)
  (lsp-copilot--async-request
   'textDocument/declaration
   (lsp-copilot--request-or-notify-params (lsp-copilot--TextDocumentPosition))
   :success-fn #'lsp-copilot--process-locations))

(defun lsp-copilot-find-type-definition ()
  "Find type definition."
  (interactive)
  (lsp-copilot--async-request
   'textDocument/typeDefinition
   (lsp-copilot--request-or-notify-params (lsp-copilot--TextDocumentPosition))
   :success-fn #'lsp-copilot--process-locations))

(defun lsp-copilot-find-implementations ()
  "Find definition."
  (interactive)
  (lsp-copilot--async-request
   'textDocument/implementation
   (lsp-copilot--request-or-notify-params (lsp-copilot--TextDocumentPosition))
   :success-fn #'lsp-copilot--process-locations))

(define-derived-mode lsp-copilot-help-mode help-mode "LspCopilotHelp"
  "Major mode for displaying lsp help.")

(defun lsp-copilot-describe-thing-at-point ()
  "Display the type signature and documentation of the thing at point."
  (interactive)
  (lsp-copilot--async-request
   'textDocument/hover
   (lsp-copilot--request-or-notify-params (lsp-copilot--TextDocumentPosition))
   :success-fn (lambda (hover-help)
                 (if (and hover-help (not (equal hover-help "")))
                     (with-current-buffer (get-buffer-create lsp-copilot-hover-buffer)
                       (let ((delay-mode-hooks t))
                         (lsp-copilot-help-mode)
                         (with-help-window lsp-copilot-hover-buffer
                           (insert (lsp-copilot--format-markup hover-help))))
                       (run-mode-hooks))
                   (lsp-copilot--info "No content at point.")))))

(defun lsp-copilot--get-indent-width (mode)
  "Get indentation offset for MODE."
  (or (alist-get mode lsp-copilot--formatting-indent-alist)
      (lsp-copilot--get-indent-width (or (get mode 'derived-mode-parent) 'default))))

(defun lsp-copilot-format-buffer ()
  "Ask the server to format this document."
  (interactive)
  (lsp-copilot--async-request
   'textDocument/formatting
   (lsp-copilot--request-or-notify-params
    (append (list
             :options
             (list
              :tabSize (symbol-value (lsp-copilot--get-indent-width major-mode))
              :insertSpaces (not indent-tabs-mode)
              :trimTrailingWhitespace lsp-copilot-trim-trailing-whitespace
              :insertFinalNewline lsp-copilot-insert-final-newline
              :trimFinalNewlinesmm lsp-copilot-trim-final-newlines))
            (lsp-copilot--TextDocumentIdentifier)))
   :success-fn (lambda (edits)
                 (if (and edits (> (length edits) 0))
                     (progn
                       (lsp-copilot--apply-text-edits edits)
                       (save-buffer))
                   (lsp-copilot--info "No formatting changes provided")))))

;;
;; completion
;;
(defun lsp-copilot-passthrough-all-completions (_string table pred _point)
  "Like `completion-basic-all-completions' but have prefix ignored.
TABLE PRED"
  (completion-basic-all-completions "" table pred 0))

(defun lsp-copilot--dumb-tryc (pat table pred point)
  "Like `completion-basic-try-completion' but passthrough all completion.
Without common substring required. PAT TABLE PRED POINT."
  (let ((probe (funcall table pat pred nil)))
    (cond ((eq probe t) t)
          (probe (cons probe (length probe)))
          (t (cons pat point)))))


(defvar-local lsp-copilot--last-inserted-char nil
  "If non-nil, value of the last inserted character in buffer.")

(defun lsp-copilot--post-self-insert-hook ()
  "Set `lsp-copilot--last-inserted-char'."
  (setq lsp-copilot--last-inserted-char last-input-event)
  (lsp-copilot--maybe-enable-signature-help))

(defun lsp-copilot--pre-command-hook ()
  "Rest some temporary variables."
  (setq lsp-copilot--last-inserted-char nil))

(defun lsp-copilot--get-english-dash-string-boundaries ()
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

(defun lsp-copilot-completion-at-point ()
  "Get lsp completions."
  ;; (when (not (nth 4 (syntax-ppss)))
  (let* ((trigger-characters lsp-copilot--completion-trigger-characters)
         (bounds-start (if-let ((bounds (lsp-copilot--get-english-dash-string-boundaries)))
                           (cl-first bounds)
                         (or (cl-first (bounds-of-thing-at-point 'symbol))
                             (point))))
         (prefix (buffer-substring-no-properties bounds-start (point)))
         (candidates
          (lambda ()
            (let* ((resp (lsp-copilot--request
                          'textDocument/completion
                          (lsp-copilot--request-or-notify-params
                           (lsp-copilot--TextDocumentPosition)
                           `(:context
                             (:line ,(buffer-substring-no-properties (line-beginning-position) (line-end-position))
                              :prefix ,prefix
                              :boundsStart ,bounds-start
                              :startPoint ,(point)
                              :triggerKind ,(if (null lsp-copilot--last-inserted-char) 1 2)))) ;; 只用来区分是否是空字符触发的，如果是空认为是主动触发，否则就是自动触发
                          :cancel-on-input t))
                   (items (mapcar (lambda (candidate)
                                    (let* ((item (plist-get candidate :item))
                                           (label (plist-get item :label)))
                                      (propertize label 'lsp-copilot--item candidate)))
                                  resp)))
              items))))
    (list
     bounds-start
     (point)
     (lambda (probe pred action)
       (cond
        ((eq action 'metadata)
         '(metadata (category . lsp-copilot-capf)
           (display-sort-function . identity)
           (cycle-sort-function . identity)))
        ((eq (car-safe action) 'boundaries) nil)
        (t
         (complete-with-action action (funcall candidates) probe pred))))
     :annotation-function #'lsp-copilot--annotate
     :company-kind #'lsp-copilot--candidate-kind
     :company-require-match 'nerver
     :company-prefix-length
     (save-excursion
       (and (lsp-copilot--looking-back-trigger-characterp trigger-characters) t))
     :company-doc-buffer #'lsp-copilot--doc-buffer
     :exit-function #'lsp-copilot--company-post-completion)))

(defun lsp-copilot--looking-back-trigger-characterp (trigger-characters)
  "Return character if text before point match any of the TRIGGER-CHARACTERS."
  (unless (= (point) (line-beginning-position))
    (cl-some
     (lambda (trigger-char)
       (and (equal (buffer-substring-no-properties (- (point) (length trigger-char)) (point))
                   trigger-char)
            trigger-char))
     trigger-characters)))

(defun lsp-copilot--company-post-completion (candidate status)
  "Replace a CompletionItem's label with its insertText.  Apply text edits.

CANDIDATE is a string returned by `company-lsp--make-candidate'."
  (when (memq status '(finished exact))
    (let* ((copilot-item (get-text-property 0 'lsp-copilot--item candidate))
           (resolved-item (get-text-property 0 'resolved-item candidate))
           (language-server-name (plist-get copilot-item :language_server_name))
           (marker (copy-marker (point) t)))
      (unless copilot-item
        (message "no lsp-copilot--item in post-completion %s" copilot-item))
      (if (equal language-server-name "typescript-language-server")
          (if resolved-item
              (lsp-copilot--company-post-completion-item resolved-item candidate marker)
            (let ((resolved (lsp-copilot--sync-resolve copilot-item)))
              (put-text-property 0 (length candidate) 'resolved-item resolved candidate)
              (lsp-copilot--company-post-completion-item (or resolved copilot-item) candidate marker)))
        (lsp-copilot--company-post-completion-item (or resolved-item copilot-item) candidate marker)))))

(defun lsp-copilot--company-post-completion-item (copilot-item candidate marker)
  "Complete CANDIDATE of ITEM from MARKER."
  (let* ((item (plist-get copilot-item :item))
         (label (plist-get item :label))
         (insertText (plist-get item :insertText))
         ;; 1 = plaintext, 2 = snippet
         (insertTextFormat (plist-get item :insertTextFormat))
         (textEdit (plist-get item :textEdit))
         (additionalTextEdits (plist-get item :additionalTextEdits))
         (startPoint (- marker (length candidate)))
         (insertTextMode (plist-get item :insertTextMode))
         (start (plist-get copilot-item :start))
         (end (plist-get copilot-item :end))
         (serverName (plist-get copilot-item :language_server_name)))
    (cond (textEdit
           (let* ((range (plist-get textEdit :range))
                  (replaceStart (lsp-copilot--position-point (plist-get range :start)))
                  (replaceEnd (lsp-copilot--position-point (plist-get range :end)))
                  (newText (plist-get textEdit :newText))
                  (insertText (s-replace "\r" "" (or newText ""))))
             ;; 支持 vscode 的 JSDoc 补传
             (if (and (equal serverName "vtsls") (equal label "/** */"))
                 (progn
                   (delete-region start end)
                   (delete-region replaceStart replaceEnd))
               (delete-region replaceStart end))
             (insert insertText)))
          ;; (snippet-fn
          ;; A snippet should be inserted, but using plain
          ;; `insertText'.  This requires us to delete the
          ;; whole completion, since `insertText' is the full
          ;; completion's text.
          (insertText
           (delete-region (- end (length candidate)) end)
           (insert (or insertText label))))
    (lsp-copilot--indent-lines startPoint (point) insertTextMode)
    (when (eq insertTextFormat 2)
      (lsp-copilot--expand-snippet (buffer-substring startPoint (point))
                                   startPoint
                                   (point)))
    (if (cl-plusp (length additionalTextEdits))
        (lsp-copilot--apply-text-edits additionalTextEdits)
      (if-let ((resolved-item (get-text-property 0 'resolved-item candidate)))
          (if-let (additionalTextEdits (plist-get resolved-item :additionalTextEdits))
              (lsp-copilot--apply-text-edits additionalTextEdits))
        (-let [(callback cleanup-fn) (lsp-copilot--create-apply-text-edits-handlers)]
          (lsp-copilot--async-resolve copilot-item callback cleanup-fn))))))

(defun lsp-copilot--candidate-kind (item)
  "Return ITEM's kind."
  (let* ((copilot-item (get-text-property 0 'lsp-copilot--item item))
         (completion-item (plist-get copilot-item :item))
         (kind (and completion-item (plist-get completion-item :kind))))
    (alist-get kind lsp-copilot--kind->symbol)))

(defun lsp-copilot--annotate (item)
  "Annotate ITEM detail."
  (let* ((copilot-item (get-text-property 0 'lsp-copilot--item item))
         (completion-item (plist-get copilot-item :item))
         (kind (and completion-item (plist-get completion-item :kind)))
         (detail (and completion-item (plist-get completion-item :detail)))
         (label-detail (and completion-item (plist-get completion-item :labelDetails))))
    (concat
     (when detail
       (concat " " (s-replace "\r" "" detail)))
     (when-let (label--detail (and label-detail (plist-get label-detail :detail)))
       (format " %s" label--detail))
     (when-let (description (and label-detail (plist-get label-detail :description)))
       (format " %s" description))
     (when-let ((kind-name (alist-get kind lsp-copilot--kind->symbol)))
       (format " (%s)" kind-name)))))

(defun lsp-copilot--doc-buffer (item)
  "Get ITEM doc."
  (when-let* ((copilot-item (get-text-property 0 'lsp-copilot--item item))
              (langauge-sever-id (plist-get copilot-item :language_server_id))
              (completion-item (plist-get copilot-item :item)))
    (let ((documentation (plist-get completion-item :documentation)))
      (unless (or documentation (get-text-property 0 'resolved-item item))
        (let* ((resolved-item (lsp-copilot--sync-resolve copilot-item))) ;; (read item) 去掉了属性？
          (put-text-property 0 (length item) 'resolved-item resolved-item item)))))
  (when-let* ((resolved-item (or (get-text-property 0 'resolved-item item) (get-text-property 0 'lsp-copilot--item item)))
              (completion-item (plist-get resolved-item :item))
              (documentation (plist-get completion-item :documentation))
              (formatted (lsp-copilot--format-markup documentation)))
    (with-current-buffer (get-buffer-create "*lsp-copilot-doc*")
      (erase-buffer)
      (insert formatted)
      (current-buffer))))

(defun lsp-copilot--sync-resolve (copilot-item)
  (when-let* ((language-server-id (plist-get copilot-item :language_server_id))
              (start (plist-get copilot-item :start))
              (end (plist-get copilot-item :end))
              (item (plist-get copilot-item :item)))
    (lsp-copilot--request
     'completionItem/resolve
     (lsp-copilot--request-or-notify-params
      item
      `(:context (:language-server-id ,language-server-id :start ,start :end ,end)))
     :cancel-on-input t)))

(defun lsp-copilot--async-resolve (copilot-item callback &optional cleanup-fn)
  "Resolve completion COPILOT-ITEM asynchronously with CALLBACK.
The CLEANUP-FN will be called to cleanup."
  (when-let* ((language-server-id (plist-get copilot-item :language_server_id))
              (start (plist-get copilot-item :start))
              (end (plist-get copilot-item :end))
              (item (plist-get copilot-item :item)))
    (lsp-copilot--async-request
     'completionItem/resolve
     (lsp-copilot--request-or-notify-params item `(:context (:language-server-id ,language-server-id :start ,start :end ,end)))
     :success-fn (lambda (resolved-item)
                   (if-let ((complete-item (plist-get resolved-item :item))
                            (additionalTextEdits (plist-get complete-item :additionalTextEdits)))
                       (funcall callback additionalTextEdits))
                   (when cleanup-fn (funcall cleanup-fn))))
    :error-fn cleanup-fn
    :timeout-fn cleanup-fn))

;;
;; Signature
;;
(defvar lsp-copilot-signature-mode-map
  (-doto (make-sparse-keymap)
    (define-key (kbd "M-n") #'lsp-copilot-signature-next)
    (define-key (kbd "M-p") #'lsp-copilot-signature-previous)
    ;; (define-key (kbd "M-a") #'lsp-copilot-signature-toggle-full-docs)
    (define-key (kbd "C-c C-k") #'lsp-copilot-signature-stop)
    (define-key (kbd "M-k") #'lsp-copilot-signature-stop)
    ;; (define-key (kbd "<ESC>") #'lsp-copilot-signature-stop)
    (define-key (kbd "C-g") #'lsp-copilot-signature-stop))
  "Keymap for `lsp-copilot-signature-mode'.")

(defun lsp-copilot-signature-next ()
  "Show next signature."
  (interactive)
  (let ((nsigs (length (plist-get lsp-copilot--signature-last :signatures))))
    (when (and lsp-copilot--signature-last-index
               lsp-copilot--signature-last
               nsigs)
      (setq lsp-copilot--signature-last-index (% (1+ lsp-copilot--signature-last-index) nsigs))
      (lsp-copilot-signature-posframe (lsp-copilot--signature->message lsp-copilot--signature-last)))))

(defun lsp-copilot-signature-previous ()
  "Show previous signature."
  (interactive)
  (when (and lsp-copilot--signature-last-index
             lsp-copilot--signature-last)
    (setq lsp-copilot--signature-last-index (1- (if (zerop lsp-copilot--signature-last-index)
                                                    (length (plist-get lsp-copilot--signature-last :signatures))
                                                  lsp-copilot--signature-last-index)))
    (lsp-copilot-signature-posframe (lsp-copilot--signature->message lsp-copilot--signature-last))))

(define-minor-mode lsp-copilot-signature-mode
  "Mode used to show signature popup."
  :keymap lsp-copilot-signature-mode-map
  :lighter ""
  :group 'lsp-copilot-mode)

(defun lsp-copilot-signature-stop ()
  "Stop showing current signature help."
  (interactive)
  (setq lsp-copilot--signature-last nil)
  ;; TODO cancel request?
  (remove-hook 'post-command-hook #'lsp-copilot-signature)
  (lsp-copilot-signature-posframe nil)
  (lsp-copilot-signature-mode -1))

(declare-function page-break-lines--update-display-tables "ext:page-break-lines")

(defun lsp-copilot--setup-page-break-mode-if-present ()
  "Enable `page-break-lines-mode' in current buffer."
  (when (fboundp 'page-break-lines-mode)
    (page-break-lines-mode)
    ;; force page-break-lines-mode to update the display tables.
    (page-break-lines--update-display-tables)))

(declare-function posframe-show "ext:posframe")
(declare-function posframe-hide "ext:posframe")
(declare-function posframe-poshandler-point-bottom-left-corner-upward "ext:posframe")

(defface lsp-copilot-signature-posframe
  '((t :inherit tooltip))
  "Background and foreground for `lsp-copilot-signature-posframe'."
  :group 'lsp-mode)

(defvar lsp-copilot-signature-posframe-params
  (list :poshandler #'posframe-poshandler-point-bottom-left-corner-upward
        :height 10
        :width 60
        :border-width 1
        :min-width 60)
  "Params for signature and `posframe-show'.")

(defun lsp-copilot-signature-posframe (str)
  "Use posframe to show the STR signatureHelp string."
  (if str
      (apply #'posframe-show
             (with-current-buffer (get-buffer-create lsp-copilot-signature-buffer)
               (erase-buffer)
               (insert str)
               (visual-line-mode)
               (lsp-copilot--setup-page-break-mode-if-present)
               (current-buffer))
             (append
              lsp-copilot-signature-posframe-params
              (list :position (point)
                    :background-color (face-attribute 'lsp-copilot-signature-posframe :background nil t)
                    :foreground-color (face-attribute 'lsp-copilot-signature-posframe :foreground nil t)
                    :border-color (face-attribute 'font-lock-comment-face :foreground nil t))))
    (posframe-hide lsp-copilot-signature-buffer)))

(defun lsp-copilot-signature-activate ()
  "Activate signature help.
It will show up only if current point has signature help."
  (interactive)
  (setq lsp-copilot--signature-last nil
        lsp-copilot--signature-last-index nil
        lsp-copilot--signature-last-buffer (current-buffer))
  (add-hook 'post-command-hook #'lsp-copilot-signature)
  (lsp-copilot-signature-mode t))

(defun lsp-copilot--maybe-enable-signature-help ()
  "Hook function of `post-self-insert-hook'."
  (when (and lsp-copilot-signature-auto-active lsp-copilot--signature-trigger-characters)
    (let ((ch last-command-event))
      (when (cl-find ch lsp-copilot--signature-trigger-characters :key #'string-to-char)
        (lsp-copilot-signature-activate)))))

(defun lsp-copilot--signature->message (signature-help)
  "Generate eldoc message form SIGNATURE-HELP response."
  (setq lsp-copilot--signature-last signature-help)
  (when (and signature-help (not (seq-empty-p (plist-get signature-help :signatures))))
    (let* ((signatures (plist-get signature-help :signatures))
           (active-signature (plist-get signature-help :activeSignature))
           (active-parameter (plist-get signature-help :activeParameter))
           (active-signature (or lsp-copilot--signature-last-index active-signature 0))
           (_ (setq lsp-copilot--signature-last-index active-signature))
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

(defun lsp-copilot--handle-signature-update (signature)
  "Update SIGNATURE."
  (let ((message (lsp-copilot--signature->message signature)))
    (if (and (s-present? message) lsp-copilot-signature-mode)
        (lsp-copilot-signature-posframe message)
      (lsp-copilot-signature-stop))))

(defun lsp-copilot-signature ()
  "Display signature info (based on `textDocument/signatureHelp')."
  ;; (message "input %s char %s charp %s" last-input-event last-command-event (characterp last-command-event))
  ;; (message "type %s" (event-basic-type last-command-event))
  ;; (message "string type %s" (string (event-basic-type last-command-event)))
  (if (and lsp-copilot--signature-last-buffer
           (not (equal (current-buffer) lsp-copilot--signature-last-buffer)))
      (lsp-copilot-signature-stop)

    (lsp-copilot--async-request
     'textDocument/signatureHelp
     (lsp-copilot--request-or-notify-params
      (lsp-copilot--TextDocumentPosition))
     :success-fn #'lsp-copilot--handle-signature-update)
    ;; (message "char %s" (char-to-string last-inp))
    ;; 非输入字符则重新触发一次请求
    ;; (if (or (characterp last-command-event) (memq (event-basic-type last-command-event) lsp-copilot-signature-retrigger-keys))
    ;;     (if (and lsp-copilot--signature-last)
    ;;         (lsp-copilot--handle-signature-update lsp-copilot--signature-last)
    ;;       (lsp-copilot--async-request
    ;;        'textDocument/signatureHelp
    ;;        (lsp-copilot--request-or-notify-params
    ;;         (lsp-copilot--TextDocumentPosition)
    ;;         ;; `(:context (:signature-trigger-character ,(char-to-string last-command-event)))
    ;;         )
    ;;        :success-fn #'lsp-copilot--handle-signature-update))
    ;;   ;; (lsp-copilot-signature-stop)
    ;;   (message "----")
    ;;   )
    ))

;;
;; rename
;;
(defun lsp-copilot--get-symbol-to-rename ()
  "Get a symbol to rename and placeholder at point.
Returns a cons ((START . END) . PLACEHOLDER?), and nil if
renaming is generally supported but cannot be done at point.
START and END are the bounds of the identifiers being renamed,
while PLACEHOLDER?, is either nil or a string suggested by the
language server as the initial input of a new-name prompt."
  (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
    (cons bounds nil)))

(defface lsp-copilot-face-rename '((t :underline t))
  "Face used to highlight the identifier being renamed.
Renaming can be done using `lsp-rename'."
  :group 'lsp-copilot-mode)

(defface lsp-copilot-rename-placeholder-face '((t :inherit font-lock-variable-name-face))
  "Face used to display the rename placeholder in.
When calling `lsp-rename' interactively, this will be the face of
the new name."
  :group 'lsp-copilot-mode)

(defun lsp-copilot--read-rename (at-point)
  "Read a new name for a `lsp-copilot-rename' at `point' from the user.
AT-POINT shall be a structure as returned by
`lsp-copilot--get-symbol-to-rename'.

Returns a string, which should be the new name for the identifier at point. If renaming
cannot be done at point (as determined from AT-POINT), throw a `user-error'.

This function is for use in `lsp-copilot-rename' only, and shall not be
relied upon."
  (unless at-point
    (user-error "`lsp-copilot-rename' is invalid here"))
  (-let* ((((start . end) . placeholder?) at-point)
          ;; Do the `buffer-substring' first to not include `lsp-face-rename'
          (rename-me (buffer-substring start end))
          (placeholder (or placeholder? rename-me))
          (placeholder (propertize placeholder 'face 'lsp-copilot-rename-placeholder-face))
          overlay)
    ;; We need unwind protect, as the user might cancel here, causing the
    ;; overlay to linger.
    (unwind-protect
        (progn
          (setq overlay (make-overlay start end))
          (overlay-put overlay 'face 'lsp-copilot-face-rename)

          (read-string (format "Rename %s to: " rename-me) placeholder
                       'lsp-rename-history))
      (and overlay (delete-overlay overlay)))))

(defun lsp-copilot-rename (newname)
  "Rename the symbol (and all references to it) under point to NEWNAME."
  (interactive (list (lsp-copilot--read-rename (lsp-copilot--get-symbol-to-rename))))
  (lsp-copilot--async-request
   'textDocument/rename
   (lsp-copilot--request-or-notify-params
    (append (lsp-copilot--TextDocumentPosition) `(:newName ,newname)))
   :success-fn (lambda (edits)
                 (if edits
                     (lsp-copilot--apply-workspace-edit edits t)
                   (lsp-copilot--warn "Server does not support rename.")))))

;;
;; Flycheck
;;
(defun lsp-copilot-diagnostics--flycheck-buffer ()
  "Trigger flycheck on buffer."
  (remove-hook 'lsp-copilot-on-idle-hook #'lsp-copilot-diagnostics--flycheck-buffer t)
  (when (bound-and-true-p flycheck-mode)
    (flycheck-buffer)))

(defun lsp-copilot-diagnostics--flycheck-start (checker callback)
  "Start an LSP syntax check with CHECKER.
CALLBACK is the status callback passed by Flycheck."
  (remove-hook 'lsp-copilot-on-idle-hook #'lsp-copilot-diagnostics--flycheck-buffer t)
  (let* ((workspace-diagnostics (lsp-copilot--get-or-create-project (lsp-copilot-project-root) lsp-copilot--diagnostics-map))
         (buffer-diagnostics (gethash (buffer-file-name) workspace-diagnostics '()))
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
(defun lsp-copilot-diagnostics-lsp-copilot-checker-if-needed ()
  (unless (flycheck-valid-checker-p 'lsp-copilot)
    (flycheck-define-generic-checker 'lsp-copilot
      "A syntax checker using the langauge server protocol provided by lsp-copilot."
      :start #'lsp-copilot-diagnostics--flycheck-start
      :modes '(lsp-copilot-placeholder-mode)
      :predicate (lambda () lsp-copilot-mode))))

(defun lsp-copilot-diagnostics-flycheck-enable (&rest _)
  "Enable flycheck integration for the current buffer."
  (require 'flycheck)
  (lsp-copilot-diagnostics-lsp-copilot-checker-if-needed)
  (unless lsp-copilot-diagnostics--flycheck-enabled
    (setq-local lsp-copilot-diagnostics--flycheck-enabled t)
    (add-to-list 'flycheck-checkers 'lsp-copilot)
    (unless (flycheck-checker-supports-major-mode-p 'lsp-copilot major-mode)
      (flycheck-add-mode 'lsp-copilot major-mode)))
  (flycheck-mode 1))

(defun lsp-copilot-diagnostics-flycheck-disable (&rest _)
  "Disable flycheck integartion for the current buffer."
  (when lsp-copilot-diagnostics--flycheck-enabled
    (setq-local lsp-copilot-diagnostics--flycheck-enabled nil)))

;; project diagnostics
(defvar lsp-copilot-diagnostics-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'lsp-copilot-show-diagnostic)
    (define-key map (kbd "o") 'lsp-copilot-goto-diagnostic)
    map))

(defun lsp-copilot-show-diagnostic (pos &optional other-window)
  "Show location of diagnostic at POS."
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
            (let* ((b (lsp-copilot--position-point start))
                   (e (lsp-copilot--position-point end)))
              (goto-char b)
              (pulse-momentary-highlight-region (point)
                                                (or e (line-end-position)) 'highlight)))))
      (current-buffer))))

(defun lsp-copilot-goto-diagnostic (pos)
  "Show location of diagnostic at POS."
  (interactive "d")
  (pop-to-buffer
   (lsp-copilot-show-diagnostic pos)))

(defvar lsp-copilot--diagnostics-base-tabulated-list-format
  `[("Type" 8 nil)
    ("File" 40 nil)
    ("Backend" 50 t)
    ("Message" 0 t)])

(define-derived-mode lsp-copilot-diagnostics-buffer-mode tabulated-list-mode
  "Lsp copilot diagnostics"
  "A mode for listing Lsp copilot diagnostics."
  :interactive nil
  (setq tabulated-list-format lsp-copilot--diagnostics-base-tabulated-list-format)
  ;; (setq tabulated-list-entries 'lsp-copilot--diagnostics-buffer-entries)
  (tabulated-list-init-header))

(defun lsp-copilot-show-project-diagnostics ()
  "Show a list of diagnostics for current project."
  (interactive)
  (unless lsp-copilot-mode
    (user-error "Lsp copilot mode is not enabled in the current buffer"))
  (let ((workspace-diagnostics (lsp-copilot--get-or-create-project
                                (lsp-copilot-project-root)
                                lsp-copilot--diagnostics-map))
        (target (or (get-buffer lsp-copilot-diagnostics-buffer)
                    (with-current-buffer (get-buffer-create lsp-copilot-diagnostics-buffer)
                      (lsp-copilot-diagnostics-buffer-mode)
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
(defun lsp-copilot--apply-workspace-edit (wedit &optional confirm)
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
                   (format "[LSP-COPILOT] Server wants to:\n %s\n Proceed? "
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
            (lsp-copilot--info "User cancelled server edit"))
        (lsp-copilot--info "No edits to apply")
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
            (setq filename (lsp-copilot--uri-to-path (plist-get change :uri))
                  edits (plist-get change :edits)
                  version nil)
            (with-current-buffer (find-file-noselect filename)
              (lsp-copilot--info "lsp-copilot--apply-text-edit filename %s" filename)
              (lsp-copilot--apply-text-edits edits version)))
           ((equal kind "documentChange")
            (setq textDocument (plist-get change :textDocument)
                  edits (plist-get change :edits))
            (setq filename (lsp-copilot--uri-to-path (plist-get textDocument :uri))
                  version (plist-get textDocument :version))
            (with-current-buffer (find-file-noselect filename)
              (lsp-copilot--warn "lsp-copilot--apply-text-edit filename %s" filename)
              (lsp-copilot--apply-text-edits edits version)))
           (t
            (lsp-copilot--warn "lsp-copilot--apply-file-edits filename %s" filename)
            (lsp-copilot--apply-text-document-edit change))))))))

(defun lsp-copilot--code-action-transform (it)
  "Transform code action IT to a `(title . it)' format."
  (let* ((item (plist-get it :lsp_item))
         (ls-name (plist-get it :language_server_name))
         (title (plist-get item :title)))
    (cons (format "%s - (%s)" title ls-name) it)))

(defun lsp-copilot--select-action (actions)
  "Select an action to execute from ACTIONS."
  (cond
   ((seq-empty-p actions) (lsp-copilot--info "No code actions found.") nil)
   (t (let* ((completion-ignore-case t)
             (collection (seq-into actions 'list))
             (col (mapcar #'lsp-copilot--code-action-transform collection))
             (completion (completing-read "Select code actions: "
                                          (lambda (string pred action)
                                            (if (eq action 'metadata)
                                                `(metadata (display-sort-function . identity))
                                              (complete-with-action action col string pred))) nil t)))
        (cdr (assoc completion col))))))

(defun lsp-copilot--text-document-code-action-params ()
  "Code action params."
  (append
   `(:range ,(if (use-region-p)
                 (lsp-copilot--region-range (region-beginning) (region-end))
               (lsp-copilot--region-range (point) (point)))
     :context (:diagnostics ,(vector)))
   (lsp-copilot--TextDocumentIdentifier)))

(defun lsp-copilot--code-actions-at-point ()
  "Retrieve the code actions for the active region or the current line."
  (lsp-copilot--request 'textDocument/codeAction (lsp-copilot--request-or-notify-params (lsp-copilot--text-document-code-action-params))))

(defun lsp-copilot--execute-command (command arguments &optional server-id)
  "Ask SERVER-ID to execute COMMAND with ARGUMENTS."
  (let ((params (list :command command :arguments arguments)))
    (lsp-copilot--async-request
     'workspace/executeCommand
     (lsp-copilot--request-or-notify-params
      params
      `(:context (:language-server-id ,server-id))))))

(defun lsp-copilot-execute-code-action (action)
  "Execute code action ACTION.
If ACTION is not set it will be selected
from `lsp-copilot--code-actions-at-point'.
Request codeAction/resolve for more info if server supports."
  (interactive (list (lsp-copilot--select-action (lsp-copilot--code-actions-at-point))))
  (when action
    (let* ((item (plist-get action :lsp_item))
           (ls-id (plist-get action :language_server_id))
           (command (plist-get item :command))
           (edit (plist-get item :edit)))
      (if (and (not command) (not edit))
          (lsp-copilot--async-request
           'codeAction/resolve
           (lsp-copilot--request-or-notify-params item `(:context (:language-server-id ,ls-id)))
           :success-fn (lambda (action)
                         (if action
                             (lsp-copilot--execute-code-action action)
                           (lsp-copilot--info "No code action found."))))
        (lsp-copilot--execute-code-action action)))))

(defun lsp-copilot--execute-code-action (action)
  "Execute code action ACTION."
  (let* ((item (plist-get action :lsp_item))
         (ls-id (plist-get action :language_server_id))
         (command (plist-get item :command))
         (edit (plist-get item :edit)))
    (when edit
      (lsp-copilot--apply-workspace-edit edit))
    (when command
      (lsp-copilot--execute-command (plist-get command :command) (plist-get command :arguments) ls-id))))

;; inlay hints
(defface lsp-copilot-inlay-hint-face '((t (:height 0.8 :inherit shadow)))
  "Face used for inlay hint overlays.")

(defface lsp-copilot-type-hint-face '((t (:inherit lsp-copilot-inlay-hint-face)))
  "Face used for type inlay hint overlays.")

(defface lsp-copilot-parameter-hint-face '((t (:inherit lsp-copilot-inlay-hint-face)))
  "Face used for parameter inlay hint overlays.")

(defvar-local lsp-copilot--outstanding-inlay-hints-region (cons nil nil)
  "Jit-lock-calculated (FROM . TO) region with potentially outdated hints.")

(defvar-local lsp-copilot--outstanding-inlay-hints-last-region nil)

(defvar-local lsp-copilot--outstanding-inlay-regions-timer nil
  "Helper timer for `lsp-copilot--update-hints'.")

(defun lsp-copilot--update-inlay-hints (from to)
  "Jit-lock function for Eglot inlay hints."
  (cl-symbol-macrolet ((region lsp-copilot--outstanding-inlay-hints-region)
                       (last-region lsp-copilot--outstanding-inlay-hints-last-region)
                       (timer lsp-copilot--outstanding-inlay-regions-timer))
    (setcar region (min (or (car region) (point-max)) from))
    (setcdr region (max (or (cdr region) (point-min)) to))
    ;; HACK: We're relying on knowledge of jit-lock internals here.  The
    ;; condition comparing `jit-lock-context-unfontify-pos' to
    ;; `point-max' is a heuristic for telling whether this call to
    ;; `jit-lock-functions' happens after `jit-lock-context-timer' has
    ;; just run.  Only after this delay should we start the smoothing
    ;; timer that will eventually call `lsp-copilot--update-hints-1' with the
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
                       (lsp-copilot--when-live-buffer buf
                         ;; HACK: In some pathological situations
                         ;; (Emacs's own coding.c, for example),
                         ;; jit-lock is calling `lsp-copilot--update-hints'
                         ;; repeatedly with same sequence of
                         ;; arguments, which leads to
                         ;; `lsp-copilot--update-hints-1' being called with
                         ;; the same region repeatedly.  This happens
                         ;; even if the hint-painting code does
                         ;; nothing else other than widen, narrow,
                         ;; move point then restore these things.
                         ;; Possible Emacs bug, but this fixes it.
                         (unless (equal last-region region)
                           (lsp-copilot--update-hints-1 (max (car region) (point-min))
                                                        (min (cdr region) (point-max)))
                           (setq last-region region))
                         (setq region (cons nil nil)
                               timer nil)))))))))

(defun lsp-copilot--update-hints-1 (from to)
  "Do most work for `lsp-copilot--update-hints', including LSP request."
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
                (goto-char (lsp-copilot--position-point position))
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
                                                 (1 'lsp-copilot-type-hint-face)
                                                 (2 'lsp-copilot-parameter-hint-face)
                                                 (_ 'lsp-copilot-inlay-hint-face))))
                           (overlay-put ov 'priority (if peg-after-p i (- n i)))
                           (overlay-put ov 'lsp-copilot--inlay-hint t)
                           (overlay-put ov 'evaporate t)
                           (overlay-put ov 'lsp-copilot--overlay t))))
                    (if (stringp label)
                        (do-it label left-pad right-pad 0 1)
                      (cl-loop
                       for i from 0 for ldetail across label
                       do (lsp-copilot--dbind (:value value) ldetail
                            (do-it value
                                   (and (zerop i) left-pad)
                                   (and (= i (1- (length label))) right-pad)
                                   i (length label))))))))))))
    (lsp-copilot--async-request
     'textDocument/inlayHint
     (lsp-copilot--request-or-notify-params
      (append `(:range (:start ,(lsp-copilot--point-position from)
                        :end ,(lsp-copilot--point-position to)))
              (lsp-copilot--TextDocumentIdentifier)))
     :success-fn (lambda (hints)
                   (lsp-copilot--when-live-buffer buf
                     (save-excursion
                       (save-restriction
                         (widen)
                         ;; Overlays ending right at FROM with an
                         ;; `after-string' property logically belong to
                         ;; the (FROM TO) region.  Likewise, such
                         ;; overlays ending at TO don't logically belong
                         ;; to it.
                         (dolist (o (overlays-in (1- from) to))
                           (when (and (overlay-get o 'lsp-copilot--inlay-hint)
                                      (cond ((eq (overlay-end o) from)
                                             (overlay-get o 'after-string))
                                            ((eq (overlay-end o) to)
                                             (overlay-get o 'before-string))
                                            (t)))
                             (delete-overlay o)))
                         (mapc paint-hint hints)))))
     :deferred 'lsp-copilot--update-hints-1)))

(define-minor-mode lsp-copilot-inlay-hints-mode
  "Mode for displaying inlay hint."
  :lighter nil
  (cond
   ((and lsp-copilot-inlay-hints-mode lsp-copilot-mode)
    (jit-lock-register #'lsp-copilot--update-inlay-hints 'contextual))
   (t
    (jit-lock-unregister #'lsp-copilot--update-inlay-hints)
    (remove-overlays nil nil 'lsp-copilot--inlay-hint t))))

;;
;; commands
;;
(defun lsp-copilot--get-commands ()
  "Get support commands from server."
  (lsp-copilot--request 'emacs/getCommands (lsp-copilot--request-or-notify-params nil)))

(defun lsp-copilot--select-command (commands)
  "Select a command to execute from COMMANDS."
  (cond
   ((seq-empty-p commands) (lsp-copilot--info "No command found.") nil)
   (t (let* ((completion-ignore-case t)
             (collection (seq-into commands 'list))
             (col (mapcar (lambda (it) (cons (plist-get it :id) it)) collection))
             (completion (completing-read "Select command: "
                                          (lambda (string pred command)
                                            (if (eq command 'metadata)
                                                `(metadata (display-sort-function . identity))
                                              (complete-with-action command col string pred))) nil t)))
        (cdr (assoc completion col))))))

(defun lsp-copilot-execute-command (command)
  "Execute COMMAND."
  (interactive (list (lsp-copilot--select-command (lsp-copilot--get-commands))))
  (when command
    (lsp-copilot--execute-command
     (plist-get command :id)
     (vector)
     (plist-get command :language_server_id))))

;; rust-analyzer
(defun lsp-copilot--view-file-text ()
  "RustAnalyzer ViewFileText."
  (interactive)
  (lsp-copilot--async-request
   'rust-analyzer/viewFileText
   (lsp-copilot--request-or-notify-params `(:uri ,(lsp-copilot--get-uri)))
   :success-fn (lambda (resp)
                 (message "resp %s" resp))))

;;
;; hooks
;;
(defun lsp-copilot--before-change (beg end)
  "Hook onto `before-change-functions' with BEG and END."
  (when (listp lsp-copilot--recent-changes)
    (push `(,(lsp-copilot--point-position beg)
            ,(lsp-copilot--point-position end)
            (,beg . ,(copy-marker beg nil))
            (,end . ,(copy-marker end t)))
          lsp-copilot--recent-changes)))

(defun lsp-copilot--after-change (beg end pre-change-length)
  "Hook onto `after-change-functions'.
Records BEG, END and PRE-CHANGE-LENGTH locally."
  (cl-incf lsp-copilot--doc-version)
  (pcase (and (listp lsp-copilot--recent-changes)
              (car lsp-copilot--recent-changes))
    (`(,lsp-beg ,lsp-end
       (,b-beg . ,b-beg-marker)
       (,b-end . ,b-end-marker))
     (if (and (= b-end b-end-marker) (= b-beg b-beg-marker)
              (or (/= beg b-beg) (/= end b-end)))
         (setcar lsp-copilot--recent-changes
                 `(,lsp-beg ,lsp-end ,(- b-end-marker b-beg-marker)
                   ,(buffer-substring-no-properties b-beg-marker b-end-marker)))
       (setcar lsp-copilot--recent-changes
               `(,lsp-beg ,lsp-end ,pre-change-length
                 ,(buffer-substring-no-properties beg end)))))
    (_ (setf lsp-copilot--recent-changes :emacs-messup)))
  (when lsp-copilot--change-idle-timer (cancel-timer lsp-copilot--change-idle-timer))
  (let ((buf (current-buffer)))
    (setq lsp-copilot--change-idle-timer
          (run-with-idle-timer
           lsp-copilot--send-changes-idle-time
           nil (lambda () (lsp-copilot--when-live-buffer buf
                            (when lsp-copilot-mode
                              (lsp-copilot--send-did-change)
                              (setq lsp-copilot--change-idle-timer nil))))))))

(defun lsp-copilot--before-revert-hook ()
  (lsp-copilot--on-doc-close))

(defun lsp-copilot--after-revert-hook ()
  (lsp-copilot--on-doc-focus (selected-window)))

(defun lsp-copilot--post-command-hook ()
  "Post command hook."
  (lsp-copilot--idle-reschedule (current-buffer))
  (let ((this-command-string (format "%s" this-command)))
    (when lsp-copilot-mode
      (posframe-hide lsp-copilot-hover-buffer))))

(defun lsp-copilot--mode-off ()
  "Turn off `lsp-copilot-mode' unconditionally."
  (remove-overlays nil nil 'lsp-copilot--overlay t)
  (lsp-copilot-inlay-hints-mode -1)
  (lsp-copilot-mode -1))

(defconst lsp-copilot--internal-hooks
  '((before-change-functions . lsp-copilot--before-change)
    (after-change-functions . lsp-copilot--after-change)
    (before-revert-hook . lsp-copilot--before-revert-hook)
    (after-revert-hook . lsp-copilot--after-revert-hook)
    (kill-buffer-hook . lsp-copilot--mode-off)
    (kill-buffer-hook . lsp-copilot--on-doc-close)
    (xref-backend-functions . lsp-copilot--xref-backend)
    (before-save-hook . lsp-copilot--will-save)
    (after-save-hook . lsp-copilot--did-save)
    (post-command-hook . lsp-copilot--post-command-hook)
    (post-self-insert-hook . lsp-copilot--post-self-insert-hook)
    (pre-command-hook . lsp-copilot--pre-command-hook)
    (change-major-mode-hook . lsp-copilot--mode-off)))

;;
;; mode
;;
(defun lsp-copilot--buffer-visible-p ()
  "Return non nil if current buffer is visible."
  (or (buffer-modified-p) (get-buffer-window nil t)))

(defun lsp-copilot--init-if-visible ()
  "Run `lsp-copilot--on-doc-focus' for the current buffer if the buffer is visible.
Return non nil if `lsp-copilot--on-doc-focus' was run for the buffer."
  (when (lsp-copilot--buffer-visible-p)
    (remove-hook 'window-configuration-change-hook #'lsp-copilot--init-if-visible t)
    (lsp-copilot--on-doc-focus (selected-window))
    t))

(defun lsp-copilot--mode-enter ()
  "Set up lsp copilot mode when entering."
  ;; Do add hook
  (when buffer-file-name
    (dolist (hook lsp-copilot--internal-hooks)
      (add-hook (car hook) (cdr hook) nil t))
    ;; Ensure that `lsp-copilot-completion-at-point' the first CAPF to be tried,
    ;; unless user has put it elsewhere in the list by their own
    (add-hook 'completion-at-point-functions #'lsp-copilot-completion-at-point -50 t)
    ;; (completion-at-point-functions . lsp-copilot-completion-at-point)
    ;; Hook onto both window-selection-change-functions and window-buffer-change-functions
    ;; since both are separate ways of 'focussing' a buffer.
    (add-hook 'window-selection-change-functions #'lsp-copilot--on-doc-focus nil 'local)
    (add-hook 'window-buffer-change-functions #'lsp-copilot--on-doc-focus nil 'local)
    (make-local-variable 'completion-category-defaults)
    (setf (alist-get 'lsp-copilot-capf completion-category-defaults) '((styles . (lsp-copilot-passthrough))))
    (make-local-variable 'completion-styles-alist)
    (setf (alist-get 'lsp-copilot-passthrough completion-styles-alist)
          '(lsp-copilot--dumb-tryc
            lsp-copilot-passthrough-all-completions
            "Passthrough completion."))
    (cond
     ((and (or
            (and (eq lsp-copilot-diagnostics-provider :auto)
                 (functionp 'flycheck-mode))
            (and (eq lsp-copilot-diagnostics-provider :flycheck)
                 (or (functionp 'flycheck-mode)
                     (user-error "The lsp-copilot-diagnostics-provider is set to :flycheck but flycheck is not installed?"))))
           (require 'flycheck nil t))
      (lsp-copilot-diagnostics-flycheck-enable))
     (t (lsp-copilot--warn "Unable to configuration flycheck. The diagnostics won't be rendered.")))
    (let ((buffer (current-buffer)))
      (run-with-idle-timer 0 nil (lambda ()
                                   (when (buffer-live-p buffer)
                                     (with-current-buffer buffer
                                       (unless (lsp-copilot--init-if-visible)
                                         (add-hook 'window-configuration-change-hook #'lsp-copilot--init-if-visible)))))))))

(defun lsp-copilot--mode-exit ()
  "Clean up lsp copilot mode when exising."
  ;; remove hook
  (dolist (hook lsp-copilot--internal-hooks)
    (remove-hook (car hook) (cdr hook) t))
  (remove-hook 'completion-at-point-functions #'lsp-copilot-completion-at-point)
  (remove-hook 'window-selection-change-functions #'lsp-copilot--on-doc-focus 'local)
  (remove-hook 'window-buffer-change-functions #'lsp-copilot--on-doc-focus 'local)
  (setq-local completion-category-defaults
              (cl-remove 'lsp-copilot-capf completion-category-defaults :key #'cl-first))
  (setq-local completion-styles-alist
              (cl-remove 'lsp-copilot-passthrough completion-styles-alist :key #'cl-first))
  (lsp-copilot-diagnostics-flycheck-disable)
  ;; Send the close event for the active buffer since activating the mode will open it again.
  (lsp-copilot--on-doc-close))

;; Rename file
(defun lsp-copilot--on-set-visited-file-name (old-func &rest args)
  "Advice around function `set-visited-file-name'.

This advice sends textDocument/didClose for the old file and
textDocument/didOpen for the new file."
  (when lsp-copilot-mode
    (lsp-copilot--on-doc-close))
  (prog1 (apply old-func args)
    (when lsp-copilot-mode
      (lsp-copilot--on-doc-open))))

(advice-add 'set-visited-file-name :around #'lsp-copilot--on-set-visited-file-name)

(defun lsp-copilot-restart ()
  "Restart."
  (interactive)
  (when lsp-copilot--connection
    (jsonrpc-shutdown lsp-copilot--connection)
    (setq lsp-copilot--connection nil))
  (setq lsp-copilot--opened-buffers nil)
  ;; progress map
  (clrhash lsp-copilot--project-hashmap)
  ;; diagnostics
  (clrhash lsp-copilot--diagnostics-map)
  (lsp-copilot--on-doc-focus (selected-window))
  (message "[LSP-COPILOT] Process restarted."))

(defun lsp-copilot-toggle-trace-io ()
  "Toggle jsonrpc logging."
  (interactive)
  (setq lsp-copilot-log-max (if lsp-copilot-log-max nil 0))
  (lsp-copilot-restart)
  (lsp-copilot--info "JSON-RPC logging %s." (if lsp-copilot-log-max "disabled" "enabled")))

(defun lsp-copilot--server-transform (it)
  "Transform server IT to a `(name . it)' format."
  (let* ((name (plist-get it :name)))
    (cons (format "%s" name) it)))

(defun lsp-copilot--select-server (servers)
  "Select a server in SERVERS to restart."
  (cond
   ((seq-empty-p servers) (lsp-copilot--info "No server associated.") nil)
   (t (let* ((completion-ignore-case t)
             (collection (seq-into servers 'list))
             (col (mapcar #'lsp-copilot--server-transform collection))
             (completion (completing-read "Select a server: "
                                          (lambda (string pred action)
                                            (if (eq action 'metadata)
                                                `(metadata (display-sort-function . identity))
                                              (complete-with-action action col string pred))) nil t)))
        (cdr (assoc completion col))))))

(defun lsp-copilot-workspace-restart ()
  "Restart SERVER."
  (interactive)
  (lsp-copilot--async-request
   'emacs/workspaceRestart
   (lsp-copilot--request-or-notify-params nil)
   :success-fn (lambda (data)
                 (let ((paths (seq-into data 'list)))
                   (setq lsp-copilot--opened-buffers
                         (cl-remove-if
                          (lambda (elt)
                            (member (buffer-file-name elt) paths))
                          lsp-copilot--opened-buffers)))
                 ;; 清理所有 buffer 存在的 diagnostic 信息
                 (lsp-copilot--remove-project (lsp-copilot-project-root) lsp-copilot--diagnostics-map)
                 ;; 清理记录的当前项目的 progress 信息
                 (lsp-copilot--remove-project (lsp-copilot-project-root) lsp-copilot--project-hashmap))))

(defvar lsp-copilot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-S-SPC") #'lsp-copilot-signature-activate)
    (define-key map (kbd "M-,") #'lsp-copilot-signature-activate)
    map))

(defun lsp-copilot-open-config-file ()
  "Open the configuration file. If it does not exist, create it first."
  (interactive)
  (unless (file-exists-p lsp-copilot-user-languages-config)
    (with-temp-buffer lsp-copilot-user-languages-config))
  (find-file lsp-copilot-user-languages-config))

(defun lsp-copilot-open-log-file ()
  "Open the log file. If it does not exist, create it first."
  (interactive)
  (unless (file-exists-p lsp-copilot--log-file)
    (with-temp-buffer lsp-copilot--log-file))
  (find-file lsp-copilot--log-file))

;;;###autoload
(define-minor-mode lsp-copilot-mode
  "Minor mode for Lsp-Copilot."
  :map lsp-copilot-mode-map
  :init-value nil
  :lighter " Lsp Copilot"
  (if lsp-copilot-mode
      (lsp-copilot--mode-enter)
    (lsp-copilot--mode-exit)))

;;;###autoload
(define-global-minor-mode global-lsp-copilot-mode
  lsp-copilot-mode lsp-copilot-turn-on-unless-buffer-read-only)

(defun lsp-copilot-turn-on-unless-buffer-read-only ()
  "Turn on `lsp-copilot-mode' if the buffer is writable."
  (unless buffer-read-only
    (lsp-copilot-mode 1)))

(provide 'lsp-copilot)
;;; lsp-copilot.el ends here
