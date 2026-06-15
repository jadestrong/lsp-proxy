;;; lsp-proxy-install.el --- Install the emacs-lsp-proxy binary -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Downloads a prebuilt `emacs-lsp-proxy' binary from GitHub Releases and
;; installs it into `lsp-proxy-install-dir', so the server can be obtained
;; without npm, cargo, or a manual download.  The install directory is part
;; of `lsp-proxy-server-executable's search path, so the binary is picked up
;; automatically after installation.

;;; Code:

(require 'url)
(require 'json)
(require 'lsp-proxy-core)

(declare-function lsp-proxy-restart "lsp-proxy-core" ())
(declare-function lsp-proxy--connection-alivep "lsp-proxy-core" ())

;; Defined in `lsp-proxy-core'; declared here for the byte-compiler.
(defvar lsp-proxy-install-dir)

(defconst lsp-proxy-install--github-repo "jadestrong/lsp-proxy"
  "GitHub <owner>/<repo> that publishes emacs-lsp-proxy releases.")

(defcustom lsp-proxy-server-version nil
  "Release version of emacs-lsp-proxy that `lsp-proxy-install-server' installs.
A version string such as \"0.8.1\" (with or without a leading \"v\"), or nil
to install the latest published release."
  :type '(choice (const :tag "Latest" nil) string)
  :group 'lsp-proxy)

;;; Platform detection

(defun lsp-proxy-install--platform ()
  "Return the OS token used in release asset names, or signal an error."
  (pcase system-type
    ('darwin "macos")
    ('gnu/linux "linux")
    ('windows-nt "windows")
    (_ (user-error "No prebuilt emacs-lsp-proxy for system-type `%s'; build from source"
                   system-type))))

(defun lsp-proxy-install--arch ()
  "Return the CPU token used in release asset names, or signal an error."
  (let ((cpu (downcase (car (split-string system-configuration "-")))))
    (pcase cpu
      ((or "x86_64" "amd64") "x86_64")
      ((or "aarch64" "arm64") "arm64")
      (_ (user-error "No prebuilt emacs-lsp-proxy for architecture `%s'; build from source"
                     cpu)))))

(defun lsp-proxy-install--asset-name ()
  "Return the release asset filename for the current platform."
  (let ((os (lsp-proxy-install--platform)))
    (format "lsp-proxy-%s-%s.%s"
            os (lsp-proxy-install--arch)
            (if (string= os "windows") "7z" "tar.gz"))))

;;; Release resolution

(defun lsp-proxy-install--latest-tag ()
  "Query GitHub for the latest release tag (e.g. \"v0.8.1\")."
  (let* ((api (format "https://api.github.com/repos/%s/releases/latest"
                      lsp-proxy-install--github-repo))
         (buf (url-retrieve-synchronously api t t 30)))
    (unless buf (user-error "Failed to query latest release from GitHub"))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (unless (re-search-forward "^$" nil t)
            (user-error "Malformed HTTP response from GitHub"))
          (let* ((json-object-type 'plist)
                 (data (json-read)))
            (or (plist-get data :tag_name)
                (user-error "GitHub response did not include a release tag"))))
      (kill-buffer buf))))

(defun lsp-proxy-install--resolve-tag ()
  "Return the git tag to install based on `lsp-proxy-server-version'."
  (if lsp-proxy-server-version
      (if (string-prefix-p "v" lsp-proxy-server-version)
          lsp-proxy-server-version
        (concat "v" lsp-proxy-server-version))
    (progn
      (message "[lsp-proxy] Querying latest release...")
      (lsp-proxy-install--latest-tag))))

(defun lsp-proxy-install--download-url (tag)
  "Return the asset download URL for release TAG."
  (format "https://github.com/%s/releases/download/%s/%s"
          lsp-proxy-install--github-repo tag (lsp-proxy-install--asset-name)))

;;; Extraction

(defun lsp-proxy-install--extract-all (archive dest-dir)
  "Extract every entry of ARCHIVE into DEST-DIR.
Supports .tar.gz (via `tar') and .7z (via 7z/7za/7zr).  The whole archive is
extracted because asset layouts differ (tar stores the binary at the top
level, 7z keeps a directory prefix); the caller locates the binary by name."
  (cond
   ((string-suffix-p ".tar.gz" archive)
    (unless (zerop (call-process "tar" nil nil nil "-xzf" archive "-C" dest-dir))
      (user-error "`tar' failed to extract %s" archive)))
   ((string-suffix-p ".7z" archive)
    (let ((7z (or (executable-find "7z") (executable-find "7za") (executable-find "7zr"))))
      (unless 7z
        (user-error "7z not found in PATH; install p7zip/7-Zip, or use npm to install the server"))
      (unless (zerop (call-process 7z nil nil nil "x" "-y"
                                   (concat "-o" dest-dir) archive))
        (user-error "7z failed to extract %s" archive))))
   (t (user-error "Unsupported archive type: %s" archive))))

;;; Entry point

;;;###autoload
(defun lsp-proxy-install-server (&optional force)
  "Download and install the prebuilt emacs-lsp-proxy binary from GitHub Releases.

The binary is installed into `lsp-proxy-install-dir', which is part of the
`lsp-proxy-server-executable' search path.  The version installed is
`lsp-proxy-server-version', or the latest release when that is nil.

With a prefix argument FORCE, reinstall without prompting even if a binary
already exists."
  (interactive "P")
  (let* ((exe-name (if (eq system-type 'windows-nt) "emacs-lsp-proxy.exe" "emacs-lsp-proxy"))
         (dest-dir (file-name-as-directory (expand-file-name lsp-proxy-install-dir)))
         (dest (expand-file-name exe-name dest-dir)))
    (when (and (file-exists-p dest) (not force)
               (not (y-or-n-p (format "emacs-lsp-proxy already installed at %s.  Reinstall? "
                                      dest))))
      (user-error "Installation cancelled"))
    (make-directory dest-dir t)
    (let* ((asset (lsp-proxy-install--asset-name))
           (tag (lsp-proxy-install--resolve-tag))
           (url (lsp-proxy-install--download-url tag))
           (archive (make-temp-file "lsp-proxy-" nil (concat "-" asset)))
           (extract-dir (make-temp-file "lsp-proxy-extract-" t)))
      (unwind-protect
          (let (binary)
            (message "[lsp-proxy] Downloading %s ..." url)
            (url-copy-file url archive t)
            ;; A 404 from GitHub is delivered as a tiny HTML body, not an error.
            (when (< (or (file-attribute-size (file-attributes archive)) 0) 100000)
              (user-error "Download too small — asset `%s' for %s may not exist" asset tag))
            (message "[lsp-proxy] Extracting...")
            (lsp-proxy-install--extract-all archive extract-dir)
            (setq binary (car (directory-files-recursively
                               extract-dir (format "\\`%s\\'" (regexp-quote exe-name)))))
            (unless binary
              (user-error "Could not find %s inside the downloaded archive" exe-name))
            (copy-file binary dest t)
            (unless (eq system-type 'windows-nt)
              (set-file-modes dest #o755))
            (message "[lsp-proxy] Installed emacs-lsp-proxy %s to %s" tag dest)
            (when (and (lsp-proxy--connection-alivep)
                       (y-or-n-p "Restart lsp-proxy to use the newly installed binary? "))
              (lsp-proxy-restart)))
        (ignore-errors (delete-file archive))
        (ignore-errors (delete-directory extract-dir t))))))

(provide 'lsp-proxy-install)
;;; lsp-proxy-install.el ends here
