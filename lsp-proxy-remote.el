;;; lsp-proxy-remote.el --- Remote development support for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Remote development configuration for lsp-proxy.
;; Provides customization options for connecting to and deploying the
;; lsp-proxy server on remote hosts via SSH.

;;; Code:

(defgroup lsp-proxy-remote nil
  "Remote development settings for lsp-proxy."
  :prefix "lsp-proxy-remote-"
  :group 'lsp-proxy)

(defcustom lsp-proxy-remote-binary-path "~/.cache/emacs/lsp-proxy/emacs-lsp-proxy"
  "Path on the remote host where the emacs-lsp-proxy binary is installed.
When lsp-proxy opens a TRAMP buffer, it auto-deploys its own binary to
this path on the remote host if the binary is absent or outdated.
The path is interpreted by the remote shell, so `~' is expanded on the
remote side.  Change this if your remote home directory is not writable
or you prefer a different installation location."
  :type 'string
  :group 'lsp-proxy-remote)

(provide 'lsp-proxy-remote)
;;; lsp-proxy-remote.el ends here
