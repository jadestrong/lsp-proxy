;;; lsp-proxy-org.el --- Org integration for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: org, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(defcustom lsp-proxy-enable-org-babel t
  "Use `lsp-proxy' in org-babel, default is disable.")

(declare-function org-element-context "ext:org")
(declare-function org-element-type "ext:org")
(declare-function org-element-property "ext:org")

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
  ;; Restore original trigger characters when leaving code block
  (when lsp-proxy-org-babel--saved-trigger-characters
    (setq-local lsp-proxy--completion-trigger-characters
                lsp-proxy-org-babel--saved-trigger-characters)
    (setq-local lsp-proxy-org-babel--saved-trigger-characters nil))
  (setq-local lsp-proxy-org-babel--info-cache nil)
  (setq-local lsp-proxy-org-babel--block-bop nil)
  (setq-local lsp-proxy-org-babel--block-eop nil))


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
If leaving a block, clean up the cache."
  ;; Early return if not in org-mode or org-babel disabled
  (when (and lsp-proxy-enable-org-babel
             (eq major-mode 'org-mode))
    (if (and lsp-proxy-org-babel--info-cache (lsp-proxy-org-babel-in-block-p (point)))
        ;; Still in the same block, return cached info (do nothing)
        lsp-proxy-org-babel--info-cache
      ;; Either no cache or moved out of block, check current position
      (if (lsp-proxy--inside-block-p)
          ;; Might be in a src-block, get element to confirm
          (let ((element (org-element-context)))
            (if (and (eq (org-element-type element) 'src-block)
                     (org-element-property :language element))
                ;; Confirmed in a src-block with language
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
              ;; Not a src-block (maybe other block type), clean up
              (when lsp-proxy-org-babel--info-cache
                (lsp-proxy-org-babel-clean-cache))))
        ;; Not in any block, clean up if we were in one before
        (when lsp-proxy-org-babel--info-cache
          (lsp-proxy-org-babel-clean-cache))))))

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
First sends didClose to avoid duplicate didOpen, then sends didOpen
with the block content as a virtual document."
  (when (and (eq major-mode 'org-mode)
             lsp-proxy-org-babel--block-bop
             lsp-proxy-org-babel--update-file-before-change)
    (setq-local lsp-proxy-org-babel--update-file-before-change nil)
    ;; Save original trigger characters before they get overwritten by virtual doc's capabilities
    (unless lsp-proxy-org-babel--saved-trigger-characters
      (setq-local lsp-proxy-org-babel--saved-trigger-characters
                  lsp-proxy--completion-trigger-characters))
    (let ((virtual-doc-context (list
                                :line-bias (1- (line-number-at-pos lsp-proxy-org-babel--block-bop t))
                                :language (org-element-property :language lsp-proxy-org-babel--info-cache)
                                :source-type "org-babel")))
      (lsp-proxy--notify 'textDocument/didOpen
                         (list :textDocument (append (eglot--TextDocumentIdentifier)
                                                     (list
                                                      :text (org-element-property :value lsp-proxy-org-babel--info-cache)
                                                      :languageId (org-element-property :language lsp-proxy-org-babel--info-cache)
                                                      :version 0)))
                         :virtual-doc virtual-doc-context))))


(defun lsp-proxy-org-babel-monitor-after-change (begin end length)
  "Monitor org babel after change, BEGIN END LENGTH."
  ;; estimate org block end point according change length
  (when (and lsp-proxy-enable-org-babel (eq major-mode 'org-mode)
             lsp-proxy-org-babel--block-bop lsp-proxy-org-babel--block-eop)
    (setq-local lsp-proxy-org-babel--block-eop
                (- lsp-proxy-org-babel--block-eop length (- begin end)))
    ;; end_src or begin_src has been changed, reload block
    (when (or (not (lsp-proxy-org-babel-in-block-p begin))
              (<= lsp-proxy-org-babel--block-eop lsp-proxy-org-babel--block-bop))
      (lsp-proxy-org-babel-clean-cache))))


(provide 'lsp-proxy-org)
;;; lsp-proxy-org.el ends here


