;;; lsp-proxy-imenu.el --- Imenu support for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Imenu integration for lsp-proxy using document symbols.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'lsp-proxy-core)
(require 'lsp-proxy-utils)

(defcustom lsp-proxy-enable-imenu t
  "Enable imenu integration."
  :type 'boolean
  :group 'lsp-proxy)

(cl-defun lsp-proxy-imenu ()
  "LSP-Proxy's `imenu-create-index-function'.
Returns a list as described in docstring of `imenu--index-alist'."
  (unless lsp-proxy--support-document-symbols
    (cl-return-from lsp-proxy-imenu))
  (let* ((res (lsp-proxy--request 'textDocument/documentSymbol
                                  (lsp-proxy--request-or-notify-params
                                   (list :textDocument (eglot--TextDocumentIdentifier)))
                                  :cancel-on-input non-essential)))
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

(defun lsp-proxy--imenu-lsp-goto (_name pos)
  "Jump to imenu entry NAME at POS."
  (if (markerp pos)
      (progn
        (if (or (< pos (point-min))
                (> pos (point-max)))
            ;; Widen if outside narrowing.
            (widen))
        (goto-char pos))
    (let ((line (car pos))
          (character (cdr pos)))
      (goto-char (point-min))
      (forward-line line)
      (forward-char character))))

;;; Setup and teardown

(defun lsp-proxy--imenu-setup ()
  "Setup imenu."
  (when lsp-proxy-enable-imenu
    (add-function :before-until (local 'imenu-create-index-function) #'lsp-proxy-imenu)
    (setq-local imenu-default-goto-function #'lsp-proxy--imenu-lsp-goto)))

(defun lsp-proxy--imenu-teardown ()
  "Teardown imenu."
  (remove-function (local 'imenu-create-index-function) #'lsp-proxy-imenu)
  ;; Reset imenu goto function
  (kill-local-variable 'imenu-default-goto-function))

(provide 'lsp-proxy-imenu)
;;; lsp-proxy-imenu.el ends here
