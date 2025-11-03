;;; lsp-proxy-signature.el --- Signature help support for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Signature help functionality for lsp-proxy.

;;; Code:

(require 'eglot)
(require 'lsp-proxy-core)
(require 'lsp-proxy-utils)

(defun lsp-proxy-signature-eldoc-function (cb)
  "Eldoc function for signature help with callback CB."
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

(provide 'lsp-proxy-signature)
;;; lsp-proxy-signature.el ends here
