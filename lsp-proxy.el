;;; lsp-proxy.el --- LSP Proxy client for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Maintainer: JadeStrong <jadestrong@163.com>
;; Created: December 15, 2023
;; Modified: December 15, 2023
;; Version: 0.4.0
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/jadestrong/lsp-proxy
;; Package-Requires: ((emacs "30.1") (s "1.13.1") (eldoc "1.14.0") (ht "2.4") (dash "2.19.1") (f "0.21.0") (yasnippet "0.14.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; LSP Proxy is a client for Language Server Protocol that acts as a proxy
;; between Emacs and LSP servers, providing enhanced features and better
;; performance for large codebases.

;;; Code:

;;; Dependencies

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

;; Load lsp-proxy modules
(require 'lsp-proxy-utils)
(require 'lsp-proxy-core)
(require 'lsp-proxy-diagnostics)
(require 'lsp-proxy-completion)
(require 'lsp-proxy-large-file)
(require 'lsp-proxy-xref)
(require 'lsp-proxy-signature)
(require 'lsp-proxy-imenu)
(require 'lsp-proxy-inlay-hints)
(require 'lsp-proxy-inline-completion)

(defvar lsp-proxy-mode)

;;; Configuration

(defgroup lsp-proxy nil
  "Interaction with Lsp Proxy Server."
  :prefix "lsp-proxy-"
  :group 'tools)

(defcustom lsp-proxy-hover-buffer "*lsp-proxy-help*"
  "Buffer for display hover information."
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

(defcustom lsp-proxy-enable-symbol-highlighting t
  "Enable automatic symbol highlighting at point."
  :type 'boolean
  :group 'lsp-proxy)

(defcustom lsp-proxy-progress-prefix "⌛ "
  "Prefix for progress messages."
  :type 'string
  :group 'lsp-proxy)

;;; Variables

(defvar-local lsp-proxy--highlights nil
  "Current document highlights for this buffer.")

;;; Hash tables for project management

(defvar lsp-proxy--project-hashmap (make-hash-table :test 'equal)
  "Hash table for project work-done tokens.")

;;; Faces

(defface lsp-proxy-highlight-symbol-face
  '((t (:inherit bold)))
  "Face used to highlight the symbol at point."
  :group 'lsp-proxy)

;;; Work-done progress tracking

(defun lsp-proxy--set-work-done-token (project-root-path token value)
  "Set work-done TOKEN with VALUE for PROJECT-ROOT-PATH."
  (let ((project-map (lsp-proxy--ensure-project-map project-root-path lsp-proxy--project-hashmap)))
    (puthash token value project-map)))

(defun lsp-proxy--rem-work-done-token (project-root-path token)
  "Remove work-done TOKEN for PROJECT-ROOT-PATH."
  (let ((project-map (lsp-proxy--ensure-project-map project-root-path lsp-proxy--project-hashmap)))
    (remhash token project-map)))

(defun lsp-proxy--progressing-p (project-root-path)
  "Check if the server at PROJECT-ROOT-PATH is in progress."
  (let ((project (gethash project-root-path lsp-proxy--project-hashmap)))
    (and project (not (ht-empty? project)))))

(defun lsp-proxy--progress-status ()
  "Return the status of the progress for the current workspaces."
  (when lsp-proxy-mode
    (let ((progress-status
           (when-let* ((project-root (lsp-proxy-project-root))
                       (tokens (gethash (lsp-proxy--fix-path-casing project-root) lsp-proxy--project-hashmap)))
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
                            (or msg title ""))))  ; Ensure we always have a string
                (ht-values tokens)
                "|")))))
      (unless (s-blank? progress-status)
        (concat lsp-proxy-progress-prefix progress-status " ")))))

;;; Internal hooks

(defconst lsp-proxy--internal-hooks
  '((before-change-functions . lsp-proxy--before-change)
    (after-change-functions . lsp-proxy--after-change)
    (before-revert-hook . lsp-proxy--before-revert-hook)
    (after-revert-hook . lsp-proxy--after-revert-hook)
    (kill-buffer-hook . lsp-proxy--mode-off)
    (xref-backend-functions . lsp-proxy--xref-backend)
    (before-save-hook . lsp-proxy--will-save)
    (after-save-hook . lsp-proxy--did-save)
    (post-command-hook . lsp-proxy--post-command-hook)
    (post-self-insert-hook . lsp-proxy--post-self-insert-hook)
    (pre-command-hook . lsp-proxy--pre-command-hook)
    (change-major-mode-hook . lsp-proxy--mode-off))
  "Internal hooks for lsp-proxy mode.")

;;; Rust analyzer functions

(defvar lsp-proxy-rust-analyzer-expand-macro-buffer "*lsp-proxy-expandMacro*"
  "Buffer for rust-analyzer/expandMacro.")

;;;###autoload
(defun lsp-proxy-rust-analyzer-expand-macro ()
  "Expands the macro call at point recursively."
  (interactive)
  (let ((orig-major-mode major-mode))
    (lsp-proxy--async-request
     'rust-analyzer/expandMacro
     (lsp-proxy--request-or-notify-params (eglot--TextDocumentPositionParams))
     :success-fn
     (lambda (resp)
       (-if-let* ((expansion (plist-get resp :expansion))
                  (buf (get-buffer-create lsp-proxy-rust-analyzer-expand-macro-buffer)))
           (progn
             (with-current-buffer buf
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert expansion)
                 (funcall orig-major-mode)))
             (pop-to-buffer buf))
         (lsp-proxy--error "No macro found at point, or it could not be expanded"))))))

;;; Hook functions

(defun lsp-proxy--before-revert-hook ()
  "Handle before revert hook."
  (when lsp-proxy-mode
    (lsp-proxy--on-doc-close)))

(defun lsp-proxy--after-revert-hook ()
  "Handle after revert hook."
  (when lsp-proxy-mode
    (lsp-proxy--on-doc-open)))

(defun lsp-proxy--post-self-insert-hook ()
  "Handle after `self-insert-command'."
  (lsp-proxy--track-last-input))

(defun lsp-proxy--pre-command-hook ()
  "Handle after `pre-command-hook'."
  (lsp-proxy--reset-input-tracking))

(defun lsp-proxy--post-command-hook ()
  "Post command hook."
  (lsp-proxy--cleanup-highlights-if-needed)
  (lsp-proxy--idle-reschedule (current-buffer))
  (when this-command
    (lsp-proxy-inline-completion-handle-command)))

(defun lsp-proxy--mode-off ()
  "Turn off lsp-proxy mode."
  (when lsp-proxy-mode
    (lsp-proxy-mode -1)))

(defun lsp-proxy--buffer-visible-p ()
  "Check if current buffer is visible."
  (get-buffer-window (current-buffer) 'visible))

(defun lsp-proxy--init-if-visible ()
  "Initialize if buffer is visible."
  (when (lsp-proxy--buffer-visible-p)
    (remove-hook 'window-configuration-change-hook #'lsp-proxy--init-if-visible t)
    (lsp-proxy--on-doc-focus (selected-window))
    t))

;;; Mode entry and exit

(defun lsp-proxy--mode-enter ()
  "Set up lsp proxy mode when entering."
  ;; Add hooks
  (when buffer-file-name
    (dolist (hook lsp-proxy--internal-hooks)
      (add-hook (car hook) (cdr hook) nil t))

    (lsp-proxy--setup-large-file-handling (current-buffer))

    (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
    (add-hook 'eldoc-documentation-functions #'lsp-proxy-hover-eldoc-function nil t)
    (add-hook 'eldoc-documentation-functions #'lsp-proxy-signature-eldoc-function nil t)
    (eldoc-mode 1)

    ;; Hook onto window change functions
    (add-hook 'window-selection-change-functions #'lsp-proxy--on-doc-focus nil 'local)
    (add-hook 'window-buffer-change-functions #'lsp-proxy--on-doc-focus nil 'local)

    ;; Setup completion
    (lsp-proxy--completion-setup)

    ;; Setup imenu if enabled
    (lsp-proxy--imenu-setup)

    ;; Setup diagnostics
    (lsp-proxy--diagnostics-setup)

    (let ((buffer (current-buffer)))
      (run-with-idle-timer 0 nil (lambda ()
                                   (when (buffer-live-p buffer)
                                     (with-current-buffer buffer
                                       (unless (lsp-proxy--init-if-visible)
                                         (add-hook 'window-configuration-change-hook #'lsp-proxy--init-if-visible)))))))))
(defun lsp-proxy--cleanup ()
  "Clean up when restart."
  ;; clear all opened buffer
  (setq lsp-proxy--opened-buffers nil)
  ;; clear all progress in map
  (clrhash lsp-proxy--project-hashmap)
  ;; clear all diagnostics
  (clrhash lsp-proxy--diagnostics-map)
  ;; clear current buffer's highlights
  (when lsp-proxy--highlights
    (mapc #'delete-overlay lsp-proxy--highlights))
  ;; clear current buffer's inlay hints
  (remove-overlays nil nil 'lsp-proxy--inlay-hint t))

(defun lsp-proxy--mode-exit ()
  "Clean up lsp proxy mode when exiting."
  ;; Remove hooks
  (dolist (hook lsp-proxy--internal-hooks)
    (remove-hook (car hook) (cdr hook) t))
  (remove-hook 'window-selection-change-functions #'lsp-proxy--on-doc-focus 'local)
  (remove-hook 'window-buffer-change-functions #'lsp-proxy--on-doc-focus 'local)
  (remove-hook 'eldoc-documentation-functions #'lsp-proxy-hover-eldoc-function 'local)
  (remove-hook 'eldoc-documentation-functions #'lsp-proxy-signature-eldoc-function 'local)

  (lsp-proxy--imenu-teardown)
  (lsp-proxy--completion-teardown)
  (lsp-proxy--diagnostics-teardown)

  (lsp-proxy-cancel-large-file-loading)

  ;; document highlights
  (when lsp-proxy--highlights
    (mapc #'delete-overlay lsp-proxy--highlights))
  ;; inlay hints
  (remove-overlays nil nil 'lsp-proxy--inlay-hint t)

  ;; Send the close event for the active buffer
  (lsp-proxy--on-doc-close))


;;; Doctor

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
               (diagnostics (gethash file (lsp-proxy--ensure-project-map
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

;;; Workspace restart

;;;###autoload
(defun lsp-proxy-workspace-restart ()
  "Restart SERVER."
  (interactive)
  (lsp-proxy--async-request
   'emacs/workspaceRestart
   (lsp-proxy--request-or-notify-params nil)
   :success-fn (lambda (data)
                 ;; Clean up opened files for the project
                 (let ((paths (seq-into data 'list)))
                   (setq lsp-proxy--opened-buffers
                         (cl-remove-if
                          (lambda (elt)
                            (member (buffer-file-name elt) paths))
                          lsp-proxy--opened-buffers)))
                 ;; Clean up diagnostics information
                 (lsp-proxy--remove-project (lsp-proxy-project-root) lsp-proxy--diagnostics-map)
                 ;; Clean up progress information
                 (lsp-proxy--remove-project (lsp-proxy-project-root) lsp-proxy--project-hashmap)
                 (revert-buffer))))

;;; Command execution

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

(defun lsp-proxy--execute-command (command arguments &optional server-id)
  "Ask SERVER-ID to execute COMMAND with ARGUMENTS."
  (let ((params (list :command command :arguments arguments)))
    (lsp-proxy--async-request
     'workspace/executeCommand
     (lsp-proxy--request-or-notify-params
      params
      `(:context (:language-server-id ,server-id))))))

;;;###autoload
(defun lsp-proxy-execute-command (command)
  "Execute COMMAND."
  (interactive (list (lsp-proxy--select-command (lsp-proxy--get-commands))))
  (when command
    (lsp-proxy--execute-command
     (plist-get command :id)
     (vector)
     (plist-get command :language_server_id))))

;;; Code Actions

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

;;;###autoload
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

;;; Rename

(unless (fboundp 'eglot--format)
  (defun eglot--format (format &rest args)
    "Like `format`, but substitutes quotes."
    (apply #'format (if (functionp 'substitute-quotes)
                        (substitute-quotes format)
                      format)
           args)))

;;;###autoload
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

;;; Document formatting

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

(defun lsp-proxy--get-indent-width (mode)
  "Get indentation offset for MODE."
  (or (alist-get mode lsp-proxy--formatting-indent-alist)
      (lsp-proxy--get-indent-width (or (get mode 'derived-mode-parent) 'default))))

;;;###autoload
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
               :trimFinalNewlines lsp-proxy-trim-final-newlines)
     :textDocument (eglot--TextDocumentIdentifier)))
   :success-fn (lambda (edits)
                 (when (buffer-live-p (current-buffer))
                   (if (and edits (> (length edits) 0))
                       (progn
                         (eglot--apply-text-edits edits)
                         (save-buffer))
                     (lsp-proxy--info "%s" "No formatting changes provided"))))))

;;; Hover support

(define-derived-mode lsp-proxy-help-mode help-mode "LspProxyHelp"
  "Major mode for displaying lsp help.")

;;;###autoload
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

;;; Symbol highlighting

(defvar-local lsp-proxy--symbol-bounds-of-last-highlight-invocation nil
  "The bounds of the symbol from which `lsp-proxy-hover-eldoc-function'
most recently requested highlights.")

(defun lsp-proxy--point-on-highlight? ()
  "Check if point is on a highlight overlay."
  (-some? (lambda (overlay)
            (overlay-get overlay 'lsp-proxy-highlight))
          (overlays-at (point))))

(defun lsp-proxy--cleanup-highlights-if-needed ()
  "Clean up highlights if point is not on a highlight."
  (when (and lsp-proxy--highlights
             (not (lsp-proxy--point-on-highlight?)))
    (mapc #'delete-overlay lsp-proxy--highlights)
    (setq lsp-proxy--highlights nil)))

(defun lsp-proxy-hover-eldoc-function (_cb)
  "A member of `eldoc-documentation-function', for hover."
  (when (and lsp-proxy--support-document-highlight
             lsp-proxy-enable-symbol-highlighting
             (not (lsp-proxy--progressing-p (lsp-proxy-project-root))))
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
           nil)
         :deferred 'textDocument/documentHighlight))
      nil)))


;;; Mode definition

(defvar lsp-proxy-mode-map (make-sparse-keymap)
  "Keymap for lsp proxy minor mode.
Use this for custom bindings in `lsp-proxy-mode'.")

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

;;; JSON parsing setup for handling bytecode from server

(defun lsp-proxy--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (and lsp-proxy-enable-bytecode (equal (following-char) ?#))
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

;;; File renaming advice

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

(provide 'lsp-proxy)
;;; lsp-proxy.el ends here
