;;; lsp-proxy-java.el --- JVM decompiled-source support for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2026-2026 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Viewing of JDK/library sources behind `jar:'/`jrt:' URIs, as produced by
;; goto-definition against the JetBrains IntelliJ backend.
;;
;; The design follows TRAMP rather than VSCode's
;; `registerTextDocumentContentProvider'.  Two properties of TRAMP are what make
;; remote files feel native in Emacs, and both are copied here:
;;
;; 1. The file name is a genuine *absolute* path.  `tramp-file-name-regexp'
;;    begins with `\\(?:^/\\)' for exactly this reason: `expand-file-name',
;;    `file-relative-name', `locate-dominating-file', `directory-file-name',
;;    project.el, vc and dir-locals then keep working structurally, before any
;;    handler is consulted.  A raw `jar:' URI is not a file name at all
;;    (`file-name-absolute-p' is nil), so exposing one as `buffer-file-name'
;;    breaks that arithmetic everywhere, silently.  The name mapping lives in
;;    `lsp-proxy-utils' (`lsp-proxy--decompiled-uri-to-file-name').
;;
;; 2. Operations that are awkward to emulate are funnelled through a real local
;;    file.  `tramp-handle-insert-file-contents' fetches a `file-local-copy' and
;;    then calls the *real* `insert-file-contents' on it; it never hand-writes
;;    the VISIT/BEG/END/REPLACE or coding-system semantics.  We do the same, so
;;    `revert-buffer' (which passes REPLACE non-nil) and coding detection are
;;    correct for free.
;;
;; Like TRAMP, nothing is cached on disk: the local copy is transient and the
;; decompiled text lives in the buffer, so a server or JDK upgrade is picked up
;; on the next visit instead of being shadowed by a stale cache file.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'lsp-proxy-utils)
(require 'lsp-proxy-core)

(declare-function jsonrpc-request "jsonrpc")
(defvar lsp-proxy-mode)

;;; State

(defvar lsp-proxy-java--content-cache (make-hash-table :test 'equal)
  "In-memory cache mapping a `jar:'/`jrt:' URI to its decompiled content.
Each value is a plist (:code STRING :language STRING).  This only avoids
repeating the RPC within a session; it is never the buffer's source of truth.")

(defun lsp-proxy-java-flush-cache (&optional uri)
  "Drop cached decompiled content for URI, or all of it when URI is nil."
  (interactive)
  (if uri
      (remhash uri lsp-proxy-java--content-cache)
    (clrhash lsp-proxy-java--content-cache)))

;;; Fetching

(defun lsp-proxy-java--routing-uri ()
  "Return a project-file URI the proxy can route the `decompile' command to.
The proxy dispatches `workspace/executeCommand' by an opened document, so we
need the URI of some real project buffer whose language servers include the
decompiler — never the `jar:'/`jrt:' target itself (not an opened document) and
never a decompiled buffer (chained navigation inside decompiled sources has no
server-known file of its own, so borrow one from another active buffer)."
  (cl-flet ((project-file-uri (name)
              (and name
                   (not (lsp-proxy--decompiled-file-name-to-uri name))
                   (lsp-proxy--path-to-uri name))))
    (or (project-file-uri buffer-file-name)
        (catch 'found
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (when (bound-and-true-p lsp-proxy-mode)
                (let ((u (project-file-uri buffer-file-name)))
                  (when u (throw 'found u))))))))))

(defun lsp-proxy-java--content (uri)
  "Return the decompiled content plist for URI, fetching it if necessary."
  (or (gethash uri lsp-proxy-java--content-cache)
      (progn
        (unless lsp-proxy--connection
          (error "No lsp-proxy connection to decompile %s" uri))
        (let* ((doc-uri (or (lsp-proxy-java--routing-uri)
                            (error "No active lsp-proxy document to route decompile of %s" uri)))
               (result (jsonrpc-request
                        lsp-proxy--connection 'workspace/executeCommand
                        (list :uri doc-uri
                              :params (list :command "decompile"
                                            :arguments (vector uri)))))
               (code (plist-get result :code)))
          (unless (stringp code)
            (error "decompile returned no content for %s" uri))
          (let ((content (list :code code :language (plist-get result :language))))
            (puthash uri content lsp-proxy-java--content-cache)
            content)))))

;;; file-name-handler

(defun lsp-proxy-java--run-real (operation args)
  "Run OPERATION on ARGS with this handler inhibited.
The standard `file-name-handler-alist' idiom, equivalent to
`tramp-run-real-handler'."
  (let ((inhibit-file-name-handlers
         (cons #'lsp-proxy-java--file-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun lsp-proxy-java--barf-read-only (name)
  "Signal that NAME belongs to a read-only virtual filesystem."
  (signal 'file-error (list "Decompiled source is read-only" name)))

(defun lsp-proxy-java--file-local-copy (filename)
  "Materialise FILENAME as a real local file and return its name.
The TRAMP contract: the caller owns the returned file and deletes it."
  (let* ((uri (or (lsp-proxy--decompiled-file-name-to-uri filename)
                  (error "Not a decompiled file name: %s" filename)))
         (code (plist-get (lsp-proxy-java--content uri) :code))
         (tmp (make-temp-file "lsp-proxy-decompiled-" nil
                              (concat "." (or (file-name-extension filename) "java")))))
    (let ((coding-system-for-write 'utf-8)
          (write-region-inhibit-fsync t))
      (write-region code nil tmp nil 'silent))
    tmp))

(defun lsp-proxy-java--insert-file-contents (filename &optional visit beg end replace)
  "Handler for `insert-file-contents' on a decompiled FILENAME.
Fetches a transient local copy and delegates to the real primitive, so
VISIT/BEG/END/REPLACE and coding-system detection all behave normally."
  (barf-if-buffer-read-only)
  (let ((local-copy (lsp-proxy-java--file-local-copy filename))
        result)
    (unwind-protect
        ;; VISIT is deliberately not forwarded: the primitive would record the
        ;; temp file as the visited name. We do that bookkeeping ourselves
        ;; below, exactly as `tramp-handle-insert-file-contents' does.
        (setq result (insert-file-contents local-copy nil beg end replace))
      (delete-file local-copy))
    (when visit
      (setq buffer-file-name filename)
      ;; Setting this here — before `normal-mode' and before
      ;; `after-change-major-mode-hook' fires — is what keeps
      ;; `global-lsp-proxy-mode' (via `lsp-proxy-turn-on-unless-buffer-read-only')
      ;; from attaching and sending a didOpen for a non-existent document.
      (setq buffer-read-only (not (file-writable-p filename)))
      (set-visited-file-modtime '(0 0))
      (set-buffer-modified-p nil))
    (list filename (cadr result))))

(defun lsp-proxy-java--file-handler (operation &rest args)
  "`file-name-handler-alist' entry backing decompiled virtual files.
Only operations whose default behaviour is wrong for a virtual read-only file
are implemented; pure file-name arithmetic is left to the primitives, which
already work because these names are ordinary absolute paths."
  (pcase operation
    ('insert-file-contents (apply #'lsp-proxy-java--insert-file-contents args))
    ('file-local-copy (lsp-proxy-java--file-local-copy (car args)))

    ;; Existence: true only for the canonical name of an encoded URI, so probes
    ;; for siblings (`.dir-locals.el', backup files, `.git') correctly miss.
    ((or 'file-exists-p 'file-readable-p 'file-regular-p)
     (and (lsp-proxy--decompiled-file-name-p (car args)) t))
    ((or 'file-directory-p 'file-accessible-directory-p)
     (and (lsp-proxy--decompiled-directory-p (car args)) t))
    ('access-file
     (unless (lsp-proxy--decompiled-file-name-p (car args))
       (signal 'file-missing (list "Opening file" "No such file" (car args))))
     nil)

    ((or 'file-writable-p 'file-executable-p 'file-symlink-p
         'file-newer-than-file-p 'vc-registered 'file-name-case-insensitive-p)
     nil)
    ('file-truename (car args))

    ;; A stable, non-nil attribute list. nil breaks callers that expect a mode
    ;; string or a modtime, and a moving modtime makes auto-revert spin.
    ;;
    ;; The directory form must report attributes too, or we contradict our own
    ;; `file-directory-p': `file-equal-p' — and through it `file-in-directory-p'
    ;; — resolves both operands via `file-attributes' and fails on a nil.
    ('file-attributes
     (let ((name (car args)))
       (cond
        ((lsp-proxy--decompiled-file-name-p name)
         (let* ((uri (lsp-proxy--decompiled-file-name-to-uri name))
                (code (plist-get (gethash uri lsp-proxy-java--content-cache) :code)))
           (list nil 1 0 0 '(0 0) '(0 0) '(0 0)
                 (if (stringp code) (string-bytes code) 0)
                 "-r--r--r--" t 0 0)))
        ((lsp-proxy--decompiled-directory-p name)
         (list t 2 0 0 '(0 0) '(0 0) '(0 0) 0 "dr-xr-xr-x" t 0 0)))))
    ('verify-visited-file-modtime t)

    ;; Nothing can run in a virtual directory; point subprocesses somewhere real.
    ((or 'unhandled-file-name-directory 'temporary-file-directory)
     temporary-file-directory)
    ((or 'process-file 'start-file-process 'shell-command 'make-process)
     (let ((default-directory temporary-file-directory))
       (lsp-proxy-java--run-real operation args)))

    ;; No on-disk file, so no backups, no auto-save, nothing to watch.
    ('make-auto-save-file-name nil)
    ('find-backup-file-name nil)
    ('file-notify-add-watch
     (signal 'file-notify-error (list "Cannot watch decompiled source" (car args))))

    ;; Read-only filesystem.
    ((or 'write-region 'delete-file 'rename-file 'copy-file 'make-directory
         'delete-directory 'set-file-modes 'set-file-times 'add-name-to-file
         'make-symbolic-link 'make-directory-internal 'dired-compress-file)
     (lsp-proxy-java--barf-read-only (car args)))

    ;; Not a browsable directory tree.
    ((or 'directory-files 'directory-files-and-attributes
         'file-name-all-completions 'file-name-completion)
     nil)
    ('insert-directory
     (signal 'file-error (list "Cannot list decompiled source" (car args))))
    ('dired-uncache nil)

    (_ (lsp-proxy-java--run-real operation args))))

;; Declaring the handled operations lets Emacs skip this handler entirely for
;; everything else, instead of routing every primitive through the pcase above.
;; TRAMP does the same (`tramp-file-name-handler' declares 85 operations).
(put #'lsp-proxy-java--file-handler 'operations
     '(insert-file-contents file-local-copy
       file-exists-p file-readable-p file-regular-p
       file-directory-p file-accessible-directory-p access-file
       file-writable-p file-executable-p file-symlink-p
       file-newer-than-file-p vc-registered file-name-case-insensitive-p
       file-truename file-attributes verify-visited-file-modtime
       unhandled-file-name-directory temporary-file-directory
       process-file start-file-process shell-command make-process
       make-auto-save-file-name find-backup-file-name file-notify-add-watch
       write-region delete-file rename-file copy-file make-directory
       delete-directory set-file-modes set-file-times add-name-to-file
       make-symbolic-link make-directory-internal dired-compress-file
       directory-files directory-files-and-attributes
       file-name-all-completions file-name-completion
       insert-directory dired-uncache))

;;; Buffer setup

(defun lsp-proxy-java--before-revert ()
  "Drop the cached content so a revert really re-fetches from the server."
  (when-let* ((uri (lsp-proxy--decompiled-file-name-to-uri buffer-file-name)))
    (lsp-proxy-java-flush-cache uri)))

(defun lsp-proxy-java--setup-buffer ()
  "Finalise a freshly opened decompiled buffer.
The major mode already comes from `auto-mode-alist' via the virtual name's
extension; this only corrects it when the server reports a language that
disagrees, and pins down the read-only / no-backup properties."
  (when-let* ((uri (and buffer-file-name
                        (lsp-proxy--decompiled-file-name-to-uri buffer-file-name))))
    (let ((language (plist-get (gethash uri lsp-proxy-java--content-cache) :language)))
      (when (and (equal language "kotlin")
                 (not (derived-mode-p 'kotlin-mode 'kotlin-ts-mode)))
        (cond ((fboundp 'kotlin-ts-mode) (kotlin-ts-mode))
              ((fboundp 'kotlin-mode) (kotlin-mode)))))
    (setq-local default-directory temporary-file-directory)
    (setq-local buffer-auto-save-file-name nil)
    (setq-local make-backup-files nil)
    (add-hook 'before-revert-hook #'lsp-proxy-java--before-revert nil t)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))

;;; Setup

(add-to-list 'file-name-handler-alist
             (cons lsp-proxy--decompiled-file-name-regexp
                   #'lsp-proxy-java--file-handler))
(add-hook 'find-file-hook #'lsp-proxy-java--setup-buffer)

(defun lsp-proxy-java-unload-function ()
  "Deregister the decompiled-source handler.  See `unload-feature'."
  (setq file-name-handler-alist
        (rassq-delete-all #'lsp-proxy-java--file-handler
                          (copy-sequence file-name-handler-alist)))
  (remove-hook 'find-file-hook #'lsp-proxy-java--setup-buffer)
  nil)

(provide 'lsp-proxy-java)
;;; lsp-proxy-java.el ends here
