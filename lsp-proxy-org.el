;;; lsp-proxy-org.el --- Org integration for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: org, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(defcustom lsp-proxy-enable-org-babel nil
  "Enable LSP support in org-babel code blocks.
When non-nil, lsp-proxy will provide code completion and other
LSP features inside org-mode source blocks."
  :type 'boolean
  :group 'lsp-proxy)

(defcustom lsp-proxy-org-babel-language-map
  '(("shell" . "bash")
    ("sh" . "bash")
    ("tsx-ts" . "tsx"))
  "Mapping from org-babel language names to LSP language IDs.
Each entry is a cons cell (ORG-LANG . LSP-LANG) where ORG-LANG is
the language identifier used in org-mode source blocks, and LSP-LANG
is the corresponding language ID for the language server."
  :type '(alist :key-type string :value-type string)
  :group 'lsp-proxy)

(defcustom lsp-proxy-org-babel-enabled-languages
  '("python" "typescript" "javascript" "tsx" "bash")
  "List of languages to enable LSP support for in org-babel blocks.
Only source blocks with languages in this list will have LSP features enabled.
Use the language IDs after applying `lsp-proxy-org-babel-language-map'."
  :type '(repeat string)
  :group 'lsp-proxy)

(declare-function eglot--TextDocumentIdentifier "ext:eglot")
(declare-function org-element-context "ext:org")
(declare-function org-element-type "ext:org")
(declare-function org-element-property "ext:org")

(declare-function lsp-proxy--notify "lsp-proxy-core")

;; External variables from lsp-proxy-completion.el
(defvar lsp-proxy--completion-trigger-characters)

;; org babel cache
(defvar-local lsp-proxy-org-babel--info-cache nil)
(defvar-local lsp-proxy-org-babel--block-bop nil)
(defvar-local lsp-proxy-org-babel--block-eop nil)
(defvar-local lsp-proxy-org-babel--update-file-before-change nil)
(defvar-local lsp-proxy-org-babel--idle-timer nil
  "Idle timer for preemptively starting LSP server in org babel blocks.")
(defvar-local lsp-proxy-org-babel--saved-trigger-characters nil
  "Saved trigger characters from the org file's original LSP server.
Used to restore when leaving a code block.")
(defvar-local lsp-proxy-org-babel--check-timer nil
  "Idle timer for checking if cursor entered/left a code block.
Used to defer expensive `org-element-context' calls.")

(defun lsp-proxy-org-babel-in-block-p (pos)
  "Check if POS is in org babel block."
  (and lsp-proxy-org-babel--block-bop
       lsp-proxy-org-babel--block-eop
       (>= pos lsp-proxy-org-babel--block-bop)
       (<= pos lsp-proxy-org-babel--block-eop)))

(defun lsp-proxy-org-babel-clean-cache ()
  "Clean org babel cache and restore original trigger characters."
  (when lsp-proxy-org-babel--idle-timer
    (cancel-timer lsp-proxy-org-babel--idle-timer)
    (setq-local lsp-proxy-org-babel--idle-timer nil))
  (when lsp-proxy-org-babel--check-timer
    (cancel-timer lsp-proxy-org-babel--check-timer)
    (setq-local lsp-proxy-org-babel--check-timer nil))
  ;; Restore original trigger characters when leaving code block
  (when lsp-proxy-org-babel--saved-trigger-characters
    (setq-local lsp-proxy--completion-trigger-characters
                lsp-proxy-org-babel--saved-trigger-characters)
    (setq-local lsp-proxy-org-babel--saved-trigger-characters nil))
  (setq-local lsp-proxy-org-babel--info-cache nil)
  (setq-local lsp-proxy-org-babel--block-bop nil)
  (setq-local lsp-proxy-org-babel--block-eop nil)
  (setq-local lsp-proxy--support-signature-help nil)
  (setq-local lsp-proxy--support-hover nil)
  (setq-local lsp-proxy--has-any-servers nil))


(defun lsp-proxy--inside-block-p ()
  "Return the language if inside a code block, nil otherwise.
Uses text property check first for fast path."
  (when-let* ((face (get-text-property (point) 'face)))
    (when (if (listp face)
              (memq 'org-block face)
            (eq 'org-block face))
      ;; Return t to indicate we're in a block, actual element will be fetched later
      t)))

(defun lsp-proxy-org-babel-check-lsp-server ()
  "Check if current point is in org babel block.
If in a new block, schedule an idle timer to preemptively start LSP server.
If leaving a block, clean up the cache.

This function is called from `post-command-hook', so it uses a fast path
to check if we're still in the same block, and defers expensive
`org-element-context' calls to an idle timer."
  ;; Early return if not in org-mode or org-babel disabled
  (when (and lsp-proxy-enable-org-babel
             (eq major-mode 'org-mode))
    (cond
     ;; Fast path: still in the same cached block
     ((and lsp-proxy-org-babel--info-cache
           (lsp-proxy-org-babel-in-block-p (point)))
      lsp-proxy-org-babel--info-cache)

     ;; Might have entered/left a block - schedule deferred check
     ((lsp-proxy--inside-block-p)
      ;; Cancel any pending check timer and schedule a new one
      (when lsp-proxy-org-babel--check-timer
        (cancel-timer lsp-proxy-org-babel--check-timer))
      (setq-local lsp-proxy-org-babel--check-timer
                  (run-with-idle-timer
                   0.1 nil
                   #'lsp-proxy-org-babel--deferred-check
                   (current-buffer)
                   (point))))

     ;; Not in any block, clean up immediately if we were in one before
     (lsp-proxy-org-babel--info-cache
      (lsp-proxy-org-babel-clean-cache)))))

(defun lsp-proxy-org-babel--deferred-check (buffer pos)
  "Deferred check for org babel block at POS in BUFFER.
Called by idle timer to avoid expensive `org-element-context' on every keystroke."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local lsp-proxy-org-babel--check-timer nil)
      ;; Only proceed if point hasn't moved significantly
      (when (and lsp-proxy-enable-org-babel
                 (eq major-mode 'org-mode)
                 ;; Check if we're still roughly at the same position
                 (<= (abs (- (point) pos)) 5))
        (let* ((element (org-element-context))
               (language (org-element-property :language element)))
          (if (and (eq (org-element-type element) 'src-block)
                   language
                   ;; Check if this language is enabled for LSP support
                   (lsp-proxy-org-babel--language-enabled-p language))
              ;; Confirmed in a src-block with enabled language
              (unless (and lsp-proxy-org-babel--info-cache
                           (eq (org-element-property :begin element)
                               (org-element-property :begin lsp-proxy-org-babel--info-cache)))
                ;; Clean previous block state if switching blocks
                (when lsp-proxy-org-babel--info-cache
                  (lsp-proxy-org-babel-clean-cache))
                (setq-local lsp-proxy-org-babel--info-cache element)
                (save-excursion
                  (goto-char (org-element-property :post-affiliated element))
                  (setq-local lsp-proxy-org-babel--block-bop (1+ (line-end-position))))
                (setq-local lsp-proxy-org-babel--block-eop
                            (+ lsp-proxy-org-babel--block-bop -1
                               (length (org-element-property :value element))))
                (setq-local lsp-proxy-org-babel--update-file-before-change t)
                ;; Schedule idle timer to preemptively start LSP server
                (lsp-proxy-org-babel--schedule-lsp-start))
            ;; Not a src-block, or language not enabled, clean up
            (when lsp-proxy-org-babel--info-cache
              (lsp-proxy-org-babel-clean-cache))))))))

(defun lsp-proxy-org-babel--schedule-lsp-start ()
  "Schedule an idle timer to start LSP server for current org babel block.
This allows the LSP server to start before the user begins editing,
reducing latency for the first completion request."
  (when lsp-proxy-org-babel--idle-timer
    (cancel-timer lsp-proxy-org-babel--idle-timer))
  (setq-local lsp-proxy-org-babel--idle-timer
              (run-with-idle-timer
               0.2 nil
               #'lsp-proxy-org-babel--idle-start-lsp
               (current-buffer))))

(defun lsp-proxy-org-babel--idle-start-lsp (buffer)
  "Start LSP server for org babel block in BUFFER after idle.
Called by idle timer to preemptively initialize the language server."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local lsp-proxy-org-babel--idle-timer nil)
      (when (and lsp-proxy-enable-org-babel
                 (eq major-mode 'org-mode)
                 lsp-proxy-org-babel--update-file-before-change)
        (lsp-proxy-org-babel-send-src-block-to-lsp-server)))))

(defun lsp-proxy-org-babel-send-src-block-to-lsp-server ()
  "Send current org babel src block to LSP server.
Sends didOpen with the block content as a virtual document.
The server side will handle didClose if needed when reusing servers."
  (when (and (eq major-mode 'org-mode)
             lsp-proxy-org-babel--block-bop
             lsp-proxy-org-babel--update-file-before-change)
    (setq-local lsp-proxy-org-babel--update-file-before-change nil)
    (let* ((orig-language (org-element-property :language lsp-proxy-org-babel--info-cache))
           (normalized-language (lsp-proxy-org-babel--normalize-language orig-language)))
      ;; Only send to LSP server if the language is enabled
      (when (lsp-proxy-org-babel--language-enabled-p orig-language)
        ;; Save original trigger characters before they get overwritten by virtual doc's capabilities
        (unless lsp-proxy-org-babel--saved-trigger-characters
          (setq-local lsp-proxy-org-babel--saved-trigger-characters
                      lsp-proxy--completion-trigger-characters))
        (lsp-proxy--notify 'textDocument/didOpen
                           (list :textDocument (append (eglot--TextDocumentIdentifier)
                                                       (list
                                                        :text (org-element-property :value lsp-proxy-org-babel--info-cache)
                                                        :languageId normalized-language
                                                        :version 0))))))))


(defun lsp-proxy-org-babel-monitor-after-change (begin end length)
  "Monitor org babel after change, BEGIN END LENGTH.
BEGIN and END are the bounds of the changed region after the change.
LENGTH is the length of the text that was replaced (0 for pure insertion).

Updates `lsp-proxy-org-babel--block-eop' to track the end of the code block
as edits are made, without needing to re-parse the org element."
  (when (and lsp-proxy-enable-org-babel (eq major-mode 'org-mode)
             lsp-proxy-org-babel--block-bop lsp-proxy-org-babel--block-eop)
    ;; Update block-eop based on the change:
    ;; - (end - begin) is the length of new text inserted
    ;; - length is the length of old text that was deleted
    ;; - net change = (end - begin) - length
    (setq-local lsp-proxy-org-babel--block-eop
                (+ lsp-proxy-org-babel--block-eop (- end begin length)))
    ;; If the change is outside the block or block becomes invalid, clean up
    (when (or (not (lsp-proxy-org-babel-in-block-p begin))
              (<= lsp-proxy-org-babel--block-eop lsp-proxy-org-babel--block-bop))
      (lsp-proxy-org-babel-clean-cache))))

(defun lsp-proxy-org-babel--elisp-language-p ()
  "Return non-nil if current org babel block is elisp or emacs-lisp."
  (when (and (bound-and-true-p lsp-proxy-enable-org-babel)
             (eq major-mode 'org-mode)
             (bound-and-true-p lsp-proxy-org-babel--info-cache))
    (let ((lang (org-element-property :language lsp-proxy-org-babel--info-cache)))
      (member lang '("elisp" "emacs-lisp")))))

;;; Virtual document context utilities

(defun lsp-proxy-org-babel--normalize-language (language)
  "Normalize LANGUAGE using the language map.
Returns the mapped language if it exists in the map, otherwise returns
the original LANGUAGE unchanged."
  (or (cdr (assoc language lsp-proxy-org-babel-language-map))
      language))

(defun lsp-proxy-org-babel--language-enabled-p (language)
  "Check if LANGUAGE is enabled for LSP support in org-babel blocks.
LANGUAGE is first normalized using `lsp-proxy-org-babel-language-map',
then checked against `lsp-proxy-org-babel-enabled-languages'."
  (when language
    (let ((normalized-lang (lsp-proxy-org-babel--normalize-language language)))
      (member normalized-lang lsp-proxy-org-babel-enabled-languages))))

(defun lsp-proxy--make-virtual-doc-context ()
  "Create virtual-doc context if in org babel block.
Returns a plist with :line-bias, :language, and :source-type keys
when the current buffer is in an org-mode babel source block.
Returns nil otherwise.

This context is orthogonal to request-specific context (like completion
triggers) and is used for position translation between the org file
and the virtual document sent to the language server."
  (when (and lsp-proxy-enable-org-babel
             (eq major-mode 'org-mode)
             lsp-proxy-org-babel--info-cache)
    (let* ((orig-language (org-element-property :language lsp-proxy-org-babel--info-cache))
           (normalized-language (lsp-proxy-org-babel--normalize-language orig-language)))
      (when (lsp-proxy-org-babel--language-enabled-p orig-language)
        (list :line-bias (1- (line-number-at-pos lsp-proxy-org-babel--block-bop t))
              :language normalized-language
              :source-type "org-babel")))))


(provide 'lsp-proxy-org)
;;; lsp-proxy-org.el ends here


