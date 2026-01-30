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

;; org babel cache
(defvar-local lsp-proxy-org-babel--info-cache nil)
(defvar-local lsp-proxy-org-babel--block-bop nil)
(defvar-local lsp-proxy-org-babel--block-eop nil)
(defvar-local lsp-proxy-org-babel--update-file-before-change nil)

(defun lsp-proxy-org-babel-in-block-p (pos)
  "Check if POS is in org babel block."
  (and lsp-proxy-org-babel--block-bop
       lsp-proxy-org-babel--block-eop
       (>= pos lsp-proxy-org-babel--block-bop)
       (<= pos lsp-proxy-org-babel--block-eop)))

(defun lsp-proxy-org-babel-clean-cache ()
  "Clean org babel cache."
  (setq-local lsp-proxy-org-babel--info-cache nil)
  (setq-local lsp-proxy-org-babel--block-bop nil)
  (setq-local lsp-proxy-org-babel--block-eop nil))

(defun lsp-proxy-org-babel-check-lsp-server ()
  "Check if current point is in org babel block. "
  (when (and lsp-proxy-enable-org-babel (eq major-mode 'org-mode))
    (if (and lsp-proxy-org-babel--info-cache (lsp-proxy-org-babel-in-block-p (point)))
        lsp-proxy-org-babel--info-cache
      (setq-local lsp-proxy-org-babel--info-cache (org-element-context))
      ;; TODO support latex block like `latex-environment' nad `latex-block'
      (if (not (eq (org-element-type lsp-proxy-org-babel--info-cache) 'src-block))
          (setq-local lsp-proxy-org-babel--info-cache nil)
        (save-excursion
          (goto-char (org-element-property :post-affiliated lsp-proxy-org-babel--info-cache))
          (setq-local lsp-proxy-org-babel--block-bop (1+ (line-end-position))))
        (setq-local lsp-proxy-org-babel--block-eop (+ lsp-proxy-org-babel--block-bop -1
                                                      (length (org-element-property :value lsp-proxy-org-babel--info-cache))))
        ;; sync it in `lsp-proxy-monitor-before-change'
        (setq-local lsp-proxy-org-babel--update-file-before-change t))))
  ;; (and lsp-proxy-org-babel--info-cache
  ;;      (lsp-proxy-org-babel-get-single-lang-server))
  )

(defun lsp-proxy-org-babel-send-src-block-to-lsp-server ()
  (when (and (eq major-mode 'org-mode)
             lsp-proxy-org-babel--block-bop
             lsp-proxy-org-babel--update-file-before-change)
    (setq-local lsp-proxy-org-babel--update-file-before-change nil)
    ;; didOpen the block, first didClose
    ;; 使用当前 org 文件来做 didOpen 但是 content 是提取出的 org-src-block 内容
    ;; 但是在补全或其他操作中，需要做行偏移，因为在 org 中编辑时，是 org 的实际行号，但是 lsp server 只有 src block 的内容
    ;; 行号是对应不上的，所以需要偏移校正
    ;; 需要携带行号偏移量过去，在 didChange 时计算后再发送给 server
    ;; (1- (line-number-at-pos lsp-proxy-org-babel--block-bop t))
    ;; 另外，携带是否是 virtualDocument ？
    (lsp-proxy--notify 'textDocument/didOpen
                       (list :textDocument (append (eglot--TextDocumentIdentifier)
                                                   (list
                                                    :text (org-element-property :value lsp-proxy-org-babel--info-cache)
                                                    ;; guess FIXME
                                                    :languageId (org-element-property :language lsp-proxy-org-babel--info-cache) ;; get language name in src block
                                                    :version 0)))
                       :virtual-doc (list
                                     :line-bias (1- (line-number-at-pos lsp-proxy-org-babel--block-bop t))
                                     :language (org-element-property :language lsp-proxy-org-babel--info-cache)
                                     :source-type "org-babel"))))


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


