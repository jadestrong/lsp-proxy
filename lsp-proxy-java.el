;;; lsp-proxy-java.el --- JVM decompiled-source support for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2026-2026 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Shared Java/JVM support for lsp-proxy: virtual decompiled-source buffers,
;; managed backend installation, project JDK selection and local build helpers.
;; Backend-specific protocols live in `lsp-proxy-java-jdtls' and
;; `lsp-proxy-java-intellij'.
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
(require 'compile)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'url-handlers)
(require 'lsp-proxy-utils)
(require 'lsp-proxy-core)

(declare-function jsonrpc-request "jsonrpc")
;; Defined in `lsp-proxy.el', which requires this file rather than the other
;; way round; only ever reached through `lsp-proxy-java-editor-command-alist'.
(declare-function lsp-proxy-rename "lsp-proxy")
(declare-function lsp-proxy-workspace-restart "lsp-proxy")
(declare-function dape "dape" (config &optional skip-compile))
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
need the URI of some real project buffer whose language servers include a
content provider — never the virtual source target itself (not an opened document) and
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

;;; Content providers
;;
;; Backend modules register URI fetchers here.  The public decompiled-file path
;; stays common; only the protocol used to obtain the text differs between
;; jdtls (`java/classFileContents') and the IntelliJ backend (`decompile').

(defvar lsp-proxy-java--content-providers nil
  "Registered JVM source content providers.
Each entry is (PREDICATE . FUNCTION).  PREDICATE is called with a URI;
FUNCTION is called with (URI DOC-URI) and returns a `(:code :language)' plist.")

(defun lsp-proxy-java--uri-scheme (uri)
  "Return URI's scheme as a string, or nil when it has none."
  (when (and (stringp uri) (string-match "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\):" uri))
    (match-string 1 uri)))

(defun lsp-proxy-java-register-content-provider (predicate function &optional append)
  "Register FUNCTION as a JVM source provider selected by PREDICATE.
When APPEND is non-nil, add it after existing providers.  A fallback provider
should therefore pass APPEND so scheme-specific providers win."
  (let ((entry (cons predicate function)))
    (setq lsp-proxy-java--content-providers
          (if append
              (append lsp-proxy-java--content-providers (list entry))
            (cons entry lsp-proxy-java--content-providers)))))

(defun lsp-proxy-java--content-provider (uri)
  "Return the function that fetches URI's content."
  (or (cl-loop for (predicate . function) in lsp-proxy-java--content-providers
               when (funcall predicate uri)
               return function)
      (error "No Java content provider registered for %s" uri)))

(defun lsp-proxy-java--content (uri)
  "Return decompiled content for URI as a plist with `:code' and `:language'.

The first request is fetched through the backend-specific content provider and
then cached for the rest of the Emacs session.  `DOC-URI' is borrowed from an
ordinary project buffer because language servers route these requests through an
open project document, not through the virtual decompiled file itself."
  (or (gethash uri lsp-proxy-java--content-cache)
      (let* ((doc-uri (or (lsp-proxy-java--routing-uri)
                          (error "No Java project buffer available to fetch %s" uri)))
             (content (funcall (lsp-proxy-java--content-provider uri) uri doc-uri))
             (code (plist-get content :code)))
        (unless (stringp code)
          (error "Java content provider returned no code for %s" uri))
        (puthash uri content lsp-proxy-java--content-cache)
        content)))

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
  (unless (lsp-proxy--decompiled-file-name-p filename)
    (signal 'file-missing (list "Opening file" "No such file" filename)))
  ;; Record it here too, not just where the name was minted: a name restored from
  ;; a previous session (desktop, recentf) is recognised by extension but was
  ;; never registered, so its ancestor directories would otherwise be unknown.
  (lsp-proxy--decompiled-register filename)
  (let* ((uri (lsp-proxy--decompiled-file-name-to-uri filename))
         (content (lsp-proxy-java--content uri))
         (code (plist-get content :code))
         ;; Extension from the *decompiled language*, not from the URI: the URI
         ;; member is often `.class', and the temp file's name is what drives
         ;; coding-system and format detection when we read it back.
         (tmp (make-temp-file "lsp-proxy-decompiled-" nil
                              (if (equal (plist-get content :language) "kotlin")
                                  ".kt" ".java"))))
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

    ;; Existence covers both servable members and the archive directories above
    ;; them, so it never contradicts `file-directory-p'. Disagreeing here breaks
    ;; any caller that sanity-checks a directory — flycheck validates
    ;; `default-directory' with `file-exists-p' and raises
    ;; ":working-directory ... does not exist".
    ;;
    ;; Probes for names we do not serve (`.dir-locals.el', backup files, `.git')
    ;; still miss, because neither predicate accepts them.
    ((or 'file-exists-p 'file-readable-p)
     (and (or (lsp-proxy--decompiled-file-name-p (car args))
              (lsp-proxy--decompiled-directory-p (car args)))
          t))
    ('file-regular-p (and (lsp-proxy--decompiled-file-name-p (car args)) t))
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

    ;; Claiming remoteness is what buys the TRAMP display behaviour: display
    ;; code runs the name through `file-local-name' (defined as
    ;; `(or (file-remote-p f 'localname) f)') and formats the result, which is
    ;; exactly how doom-modeline shortens `/ssh:host:/long/path'. Here it yields
    ;; the member path, e.g. `/modules/java.base/java/lang/System.class'.
    ;;
    ;; The full IDENTIFICATION contract is honoured: PREFIX and 'localname must
    ;; concatenate back to the original name. Returning one fixed string for
    ;; every identification (as an earlier revision did) feeds nonsense to
    ;; callers that ask for 'host or 'method.
    ;;
    ;; This is safe here only because `default-directory' in these buffers is a
    ;; real local directory, so nothing tries to launch a process "on the remote
    ;; host"; the process-related operations below are handled explicitly anyway.
    ('file-remote-p
     (when-let* ((split (lsp-proxy--decompiled-name-split (car args))))
       (pcase (cadr args)
         ('localname (cdr split))
         ('method "lspsrc")
         ('user nil)
         ('host (if (equal (car split) lsp-proxy--decompiled-prefix)
                    "lspsrc"
                  (string-remove-suffix
                   "!" (lsp-proxy--decompiled-unescape
                        (substring (car split)
                                   (length lsp-proxy--decompiled-prefix))))))
         (_ (car split)))))

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
       file-truename file-remote-p file-attributes verify-visited-file-modtime
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

(defun lsp-proxy-java--mode-for-language (language)
  "Return the major mode to use for a decompiled buffer of LANGUAGE.
Honours `major-mode-remap-alist' so a user's tree-sitter preference wins."
  (when-let* ((base (pcase language
                      ("kotlin" (cond ((fboundp 'kotlin-ts-mode) 'kotlin-ts-mode)
                                      ((fboundp 'kotlin-mode) 'kotlin-mode)))
                      ((or "java" 'nil) 'java-mode)
                      (_ nil))))
    (if (fboundp 'major-mode-remap)
        (major-mode-remap base)
      (or (alist-get base major-mode-remap-alist) base))))

(defun lsp-proxy-java--setup-buffer ()
  "Finalise a freshly opened decompiled buffer.
Sets the major mode from the language the server reported, and pins down the
read-only / no-backup properties.

The mode cannot be left to `auto-mode-alist': because these names report as
remote, `set-auto-mode' strips the `file-remote-p' prefix before matching, so
nothing lspsrc-specific survives to key on — and a member is often `X.class',
whose extension says nothing about the decompiled language anyway.  The server's
`:language' is the authoritative answer."
  (when-let* ((uri (and buffer-file-name
                        (lsp-proxy--decompiled-file-name-to-uri buffer-file-name))))
    (when-let* ((display-name (lsp-proxy--decompiled-display-name buffer-file-name)))
      (rename-buffer display-name t))
    (let* ((language (plist-get (gethash uri lsp-proxy-java--content-cache) :language))
           (mode (lsp-proxy-java--mode-for-language language)))
      (when (and mode (not (eq major-mode mode)))
        (funcall mode)))
    ;; `default-directory' is deliberately left as the *virtual* directory that
    ;; `find-file-noselect' derived from the name, which is what TRAMP does too
    ;; (a TRAMP buffer's `default-directory' is the remote directory, not a local
    ;; stand-in).  Two reasons:
    ;;
    ;;  * Display. Code that formats a path falls back to `default-directory'
    ;;    when it cannot find a project — doom-modeline does exactly that — so
    ;;    pointing it at `temporary-file-directory' made the mode-line render the
    ;;    temp dir's last component as a fake project name plus the climb back
    ;;    out of it: "T/../../../../../modules/java.base/java/lang/System.class".
    ;;    Keeping it virtual yields "lang/System.class".
    ;;
    ;;  * Local subprocesses still work: Emacs' `encode_current_directory' runs
    ;;    `default-directory' through `unhandled-file-name-directory' before
    ;;    chdir'ing, and our handler answers that with a real directory. That
    ;;    covers `call-process' as well as the dispatched `process-file' family.
    (setq-local buffer-auto-save-file-name nil)
    (setq-local make-backup-files nil)
    (add-hook 'before-revert-hook #'lsp-proxy-java--before-revert nil t)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))

;;; Server installation
;;
;; Backend modules register their install metadata here.  The download itself
;; lives in the proxy (`src/java_install.rs`): the archives run from ~50 MB
;; (jdtls) to ~370 MB (IntelliJ), and Rust already has tokio (so the editor
;; stays responsive) plus an established progress pipeline that puts a
;; percentage in the mode line. Everything below is the thin client half.
;;
;; The two differ only in where the download comes from, which is entirely the
;; proxy's problem; on this side they differ only by the rows of
;; `lsp-proxy-java--backends'. Both land in `<install-dir>/<version>/bin/', and that
;; `bin/' is what `lsp-proxy--managed-server-path-environment' puts on the proxy's
;; PATH — which is why `languages.toml' never names a path.
;;
;; The proxy's *own* installer stays in Emacs Lisp (`lsp-proxy-install.el`) — that
;; one cannot run in the proxy, which does not exist yet when it runs.

(defvar lsp-proxy-java--backends nil
  "Java language server backends the proxy can install, keyed by server name.
Backend modules register their own descriptors with
`lsp-proxy-java-register-backend'.")

(defun lsp-proxy-java-register-backend (name &rest properties)
  "Register Java language server backend NAME with PROPERTIES.
Properties are the plist consumed by `lsp-proxy-java-install-server', for
example `:install-dir', `:version', `:exe', `:label' and
`:runtime-requires'."
  (setq lsp-proxy-java--backends
        (cons (cons name properties)
              (assoc-delete-all name lsp-proxy-java--backends))))

;;; Project JDK selection

(defun lsp-proxy-java--jdk-home-p (directory)
  "Return non-nil when DIRECTORY looks like a full JDK home."
  (and (stringp directory)
       (file-directory-p directory)
       (file-executable-p (expand-file-name
                           (if (eq system-type 'windows-nt) "bin/java.exe" "bin/java")
                           directory))
       (file-executable-p (expand-file-name
                           (if (eq system-type 'windows-nt) "bin/javac.exe" "bin/javac")
                           directory))))

(defun lsp-proxy-java--jdk-major-version (directory)
  "Return the Java major version provided by DIRECTORY, or nil."
  (let ((release-file (expand-file-name "release" directory)))
    (when (file-readable-p release-file)
      (with-temp-buffer
        (insert-file-contents release-file)
        (goto-char (point-min))
        (when (re-search-forward "^JAVA_VERSION=\"\\(?:1\\.\\)?\\([0-9]+\\)" nil t)
          (match-string 1))))))

(defun lsp-proxy-java--java-home-from-path ()
  "Infer a JDK home from the real `java' executable on PATH."
  (when-let* ((java (executable-find "java"))
              (real-java (file-truename java))
              (_ (not (and (eq system-type 'darwin)
                           (string= real-java "/usr/bin/java"))))
              (home (file-name-directory
                     (directory-file-name
                      (file-name-directory real-java)))))
    (and (lsp-proxy-java--jdk-home-p home)
         (directory-file-name (expand-file-name home)))))

(defun lsp-proxy-java--sdkman-jdk-homes ()
  "Return JDK homes installed by SDKMAN."
  (let ((root (expand-file-name "~/.sdkman/candidates/java/")))
    (when (file-directory-p root)
      (seq-filter
       #'lsp-proxy-java--jdk-home-p
       (directory-files root t directory-files-no-dot-files-regexp)))))

(defun lsp-proxy-java--macos-jdk-homes ()
  "Return JDK homes reported by macOS `java_home'."
  (when (file-executable-p "/usr/libexec/java_home")
    (with-temp-buffer
      (call-process "/usr/libexec/java_home" nil (list t t) nil "-V")
      (let (homes)
        (goto-char (point-min))
        (while (re-search-forward "\\(/[^\n]+/Contents/Home\\)[[:space:]]*$" nil t)
          (push (string-trim (match-string 1)) homes))
        (seq-filter #'lsp-proxy-java--jdk-home-p homes)))))

(defun lsp-proxy-java--installed-jdk-homes ()
  "Return known local JDK homes for project JDK selection."
  (delete-dups
   (seq-filter
    #'lsp-proxy-java--jdk-home-p
    (append
     (list (getenv "JAVA_HOME")
           (lsp-proxy-java--java-home-from-path))
     (lsp-proxy-java--sdkman-jdk-homes)
     (when (eq system-type 'darwin)
       (lsp-proxy-java--macos-jdk-homes))))))

(defun lsp-proxy-java--jdk-choice-label (home)
  "Return a minibuffer label for JDK HOME."
  (format "Java %s  %s"
          (or (lsp-proxy-java--jdk-major-version home) "?")
          (abbreviate-file-name home)))

(defun lsp-proxy-java--read-project-jdk ()
  "Read a project JDK home from known installs or direct input."
  (let* ((homes (lsp-proxy-java--installed-jdk-homes))
         (choices (mapcar (lambda (home)
                            (cons (lsp-proxy-java--jdk-choice-label home) home))
                          homes))
         (choice (completing-read "Project JDK: " choices nil nil)))
    (or (cdr (assoc choice choices))
        (read-directory-name "Project JDK directory: " choice nil t))))

;;;###autoload
(defun lsp-proxy-java-select-project-jdk (&optional clear)
  "Select a project JDK for the current Java workspace.

With prefix argument CLEAR, clear the manual choice and return to automatic
project JDK detection.  The choice affects the Java project runtime model and
editor-launched run/debug sessions; it never changes the JDK used to launch
the language server itself."
  (interactive "P")
  (unless (and (bound-and-true-p lsp-proxy-mode) buffer-file-name)
    (user-error "Open a Java project buffer with lsp-proxy-mode enabled first"))
  (lsp-proxy--ensure-connection)
  (let* ((home (unless clear
                 (directory-file-name
                  (expand-file-name (lsp-proxy-java--read-project-jdk)))))
         (result
          (jsonrpc-request
           lsp-proxy--connection 'emacs/setJavaProjectRuntime
           (lsp-proxy--build-params (if clear nil (list :javaHome home))))))
    (if clear
        (message "[lsp-proxy] Cleared project JDK for %s"
                 (abbreviate-file-name (plist-get result :workspaceRoot)))
      (message "[lsp-proxy] Project JDK for %s: Java %s at %s"
               (abbreviate-file-name (plist-get result :workspaceRoot))
               (or (plist-get result :javaVersion) "?")
               (plist-get result :javaHome)))
    (when (y-or-n-p "Restart this workspace now so language servers use the new project JDK? ")
      (lsp-proxy-workspace-restart))))

(defun lsp-proxy-java--project-java-executable (&optional uri)
  "Return the Project JDK's `java' executable for URI, or nil.
The proxy owns the detection rules so run/debug launch code stays aligned
with the JDK sent during Java backend initialization.  URI, when non-nil,
selects the document context explicitly instead of relying on the current
buffer."
  (when (bound-and-true-p lsp-proxy-mode)
    (condition-case nil
        (let* ((params (lsp-proxy--build-params nil))
               (params (if uri
                           (plist-put params :textDocument (list :uri uri))
                         params))
               (_ (lsp-proxy--ensure-connection))
               (result (jsonrpc-request
                        lsp-proxy--connection 'emacs/getJavaProjectRuntime
                        params)))
          (plist-get result :javaExec))
      (jsonrpc-error nil)
      (user-error nil))))

;;; Build, run and test

(defcustom lsp-proxy-java-maven-default-task "test"
  "Default task offered by `lsp-proxy-java-build' in Maven projects."
  :type 'string
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-gradle-default-task "test"
  "Default task offered by `lsp-proxy-java-build' in Gradle projects."
  :type 'string
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-main-arguments nil
  "Program arguments passed by `lsp-proxy-java-run-main'."
  :type '(repeat string)
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-main-jvm-arguments nil
  "JVM arguments passed by `lsp-proxy-java-run-main'."
  :type '(repeat string)
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-main-environment nil
  "Environment entries added only to `lsp-proxy-java-run-main' processes.
Each entry has the form `NAME=VALUE'."
  :type '(repeat string)
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-test-jvm-arguments nil
  "JVM arguments passed by `lsp-proxy-java-run-test'."
  :type '(repeat string)
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-test-environment nil
  "Environment entries added only to `lsp-proxy-java-run-test' processes.
Each entry has the form `NAME=VALUE'."
  :type '(repeat string)
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-junit-console-jar
  (expand-file-name "java/junit-platform-console-standalone.jar"
                    lsp-proxy-install-dir)
  "JUnit Platform Console standalone JAR used by `lsp-proxy-java-run-test'."
  :type 'file
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-junit-maven-root
  "https://repo1.maven.org/maven2"
  "Maven repository root for the JUnit Platform Console artifact."
  :type 'string
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-junit-version nil
  "Optional JUnit Platform Console version installed by lsp-proxy.
When nil, select a version according to
`lsp-proxy-java-junit-release-line'."
  :type '(choice (const :tag "Latest release" nil)
                 (string :tag "Pinned version"))
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-junit-release-line 'java-8-compatible
  "JUnit release line selected when `lsp-proxy-java-junit-version' is nil."
  :type '(choice (const :tag "Newest Java 8-compatible 1.x release"
                        java-8-compatible)
                 (const :tag "Latest release of any major" latest))
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-debug-console "integratedTerminal"
  "Console used by Java launch debug sessions.
An integrated terminal accepts program input.  The internal console sends
program output to Dape's REPL and cannot accept standard input."
  :type '(choice (const "integratedTerminal") (const "internalConsole"))
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-debug-build-before-launch t
  "Whether Java debug launches wait for a successful jdtls build."
  :type 'boolean
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-debug-build-timeout 120
  "Maximum seconds to wait for the build preceding a Java debug launch."
  :type 'natnum
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-hot-code-replace-mode 'auto
  "Control Hot Code Replace for lsp-proxy Java debug sessions.
Value `auto' asks Java Debug to replace changed classes after completed builds,
`manual' leaves replacement to explicit debugger commands, and `never' disables
the feature."
  :type '(choice (const :tag "After completed builds" auto)
                 (const :tag "Manual command only" manual)
                 (const :tag "Disabled" never))
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-debug-request-timeout 30000
  "Java Debug request timeout in milliseconds."
  :type 'natnum
  :group 'lsp-proxy)

(defconst lsp-proxy-java--build-files
  '("pom.xml" "build.gradle" "build.gradle.kts" "settings.gradle" "settings.gradle.kts")
  "Files that identify a Java build root.")

(defconst lsp-proxy-java--junit-artifact
  "org/junit/platform/junit-platform-console-standalone"
  "Maven path of the JUnit Platform Console standalone artifact.")

(defun lsp-proxy-java--current-file ()
  "Return the current saved Java file."
  (unless buffer-file-name
    (user-error "The current buffer is not visiting a Java file"))
  (when (buffer-modified-p)
    (save-buffer))
  (unless (file-readable-p buffer-file-name)
    (user-error "Save the current Java file before running it"))
  (expand-file-name buffer-file-name))

(defun lsp-proxy-java--project-root ()
  "Return the current project root or `default-directory'."
  (if-let* ((project (project-current nil)))
      (expand-file-name (project-root project))
    (expand-file-name default-directory)))

(defun lsp-proxy-java--nearest-build-root (&optional file)
  "Return the nearest Java build root for FILE or the current buffer."
  (let* ((start (file-name-directory (expand-file-name (or file buffer-file-name default-directory))))
         (project-root (file-name-as-directory (lsp-proxy-java--project-root)))
         (root (locate-dominating-file
                start
                (lambda (dir)
                  (or (seq-some
                       (lambda (name) (file-exists-p (expand-file-name name dir)))
                       lsp-proxy-java--build-files)
                      (equal (file-name-as-directory (expand-file-name dir))
                             project-root))))))
    (file-name-as-directory (expand-file-name (or root project-root)))))

(defun lsp-proxy-java--build-system (root)
  "Return ROOT's build system, either `maven' or `gradle'."
  (cond
   ((file-exists-p (expand-file-name "pom.xml" root)) 'maven)
   ((or (file-exists-p (expand-file-name "build.gradle" root))
        (file-exists-p (expand-file-name "build.gradle.kts" root))
        (file-exists-p (expand-file-name "settings.gradle" root))
        (file-exists-p (expand-file-name "settings.gradle.kts" root)))
    'gradle)
   (t (user-error "No Maven or Gradle build found under %s"
                  (abbreviate-file-name root)))))

(defun lsp-proxy-java--build-command-prefix (root build-system)
  "Return the command prefix for BUILD-SYSTEM rooted at ROOT."
  (let* ((windows (eq system-type 'windows-nt))
         (mvnw (expand-file-name (if windows "mvnw.cmd" "mvnw") root))
         (gradlew (expand-file-name (if windows "gradlew.bat" "gradlew") root)))
    (pcase build-system
      ('maven (if (file-exists-p mvnw)
                  (if (file-executable-p mvnw) (list mvnw) (list "sh" mvnw))
                (list "mvn")))
      ('gradle (if (file-exists-p gradlew)
                   (if (file-executable-p gradlew) (list gradlew) (list "sh" gradlew))
                 (list "gradle"))))))

(defun lsp-proxy-java--command-available-p (program)
  "Return non-nil when PROGRAM can be executed."
  (if (file-name-absolute-p program)
      (file-executable-p program)
    (executable-find program)))

(defun lsp-proxy-java--shell-command (arguments)
  "Return shell command for ARGUMENTS."
  (mapconcat #'shell-quote-argument arguments " "))

(defun lsp-proxy-java--default-task (build-system)
  "Return the default task for BUILD-SYSTEM."
  (pcase build-system
    ('maven lsp-proxy-java-maven-default-task)
    ('gradle lsp-proxy-java-gradle-default-task)))

(defun lsp-proxy-java--url-text (url)
  "Retrieve URL synchronously and return its trimmed text."
  (with-temp-buffer
    (url-insert-file-contents url)
    (string-trim (buffer-string))))

(defun lsp-proxy-java--junit-metadata-url ()
  "Return the configured JUnit Maven metadata URL."
  (format "%s/%s/maven-metadata.xml"
          (string-remove-suffix "/" lsp-proxy-java-junit-maven-root)
          lsp-proxy-java--junit-artifact))

(defun lsp-proxy-java--metadata-release (metadata)
  "Return the release version declared by Maven METADATA XML."
  (when (string-match
         "<release>[[:space:]]*\\([^<[:space:]]+\\)[[:space:]]*</release>"
         metadata)
    (match-string 1 metadata)))

(defun lsp-proxy-java--metadata-versions (metadata)
  "Return all versions declared by Maven METADATA XML."
  (let ((position 0)
        versions)
    (while (string-match
            "<version>[[:space:]]*\\([^<[:space:]]+\\)[[:space:]]*</version>"
            metadata position)
      (push (match-string 1 metadata) versions)
      (setq position (match-end 0)))
    (nreverse versions)))

(defun lsp-proxy-java--latest-java-8-junit-version (metadata)
  "Return the newest stable Java 8-compatible JUnit version in METADATA."
  (car
   (sort
    (seq-filter
     (lambda (version)
       (string-match-p "\\`1\\.[0-9]+\\.[0-9]+\\'" version))
     (lsp-proxy-java--metadata-versions metadata))
    (lambda (first second) (version< second first)))))

(defun lsp-proxy-java--valid-artifact-version-p (version)
  "Return non-nil when VERSION is safe in an artifact URL and file name."
  (and (stringp version)
       (string-match-p "\\`[0-9A-Za-z][0-9A-Za-z._-]*\\'" version)))

(defun lsp-proxy-java--junit-release ()
  "Return release metadata for the configured JUnit Console version."
  (let* ((metadata
          (unless lsp-proxy-java-junit-version
            (lsp-proxy-java--url-text
             (lsp-proxy-java--junit-metadata-url))))
         (version
          (or lsp-proxy-java-junit-version
              (pcase lsp-proxy-java-junit-release-line
                ('java-8-compatible
                 (lsp-proxy-java--latest-java-8-junit-version metadata))
                ('latest (lsp-proxy-java--metadata-release metadata))
                (_ (user-error "Unsupported JUnit release line: %s"
                               lsp-proxy-java-junit-release-line)))))
         (artifact "junit-platform-console-standalone"))
    (unless (lsp-proxy-java--valid-artifact-version-p version)
      (user-error "Could not determine a valid JUnit release version"))
    (let ((url
           (format "%s/%s/%s/%s-%s.jar"
                   (string-remove-suffix "/" lsp-proxy-java-junit-maven-root)
                   lsp-proxy-java--junit-artifact version artifact version)))
      (list :version version :url url :checksum-url (concat url ".sha256")))))

(defun lsp-proxy-java--junit-version-file ()
  "Return the version marker associated with the configured JUnit JAR."
  (concat (expand-file-name lsp-proxy-java-junit-console-jar) ".version"))

(defun lsp-proxy-java--read-version-file (file)
  "Return the first line of FILE, or nil."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (string-trim (buffer-string)))))

(defun lsp-proxy-java--file-sha256 (file)
  "Return the hexadecimal SHA-256 digest for FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun lsp-proxy-java--expected-sha256 (checksum-text)
  "Return the SHA-256 digest found in CHECKSUM-TEXT."
  (let ((case-fold-search t))
    (when (string-match "\\b\\([[:xdigit:]]\\{64\\}\\)\\b" checksum-text)
      (downcase (match-string 1 checksum-text)))))

(defun lsp-proxy-java--verify-download (file checksum-text)
  "Verify FILE against CHECKSUM-TEXT or signal an error."
  (let ((expected (lsp-proxy-java--expected-sha256 checksum-text))
        (actual (lsp-proxy-java--file-sha256 file)))
    (unless expected
      (error "Release source did not provide a valid SHA-256 checksum"))
    (unless (string-equal expected actual)
      (error "SHA-256 mismatch for %s" (file-name-nondirectory file)))
    actual))

(defun lsp-proxy-java--install-junit-release (release)
  "Install JUnit Console RELEASE transactionally."
  (let* ((destination (expand-file-name lsp-proxy-java-junit-console-jar))
         (parent (file-name-directory destination)))
    (make-directory parent t)
    (let* ((temporary (make-temp-file (expand-file-name ".junit-stage-" parent) t))
           (jar (expand-file-name "junit-console.jar" temporary)))
      (unwind-protect
          (progn
            (url-copy-file (plist-get release :url) jar t)
            (lsp-proxy-java--verify-download
             jar
             (lsp-proxy-java--url-text (plist-get release :checksum-url)))
            (rename-file jar destination t)
            (with-temp-file (lsp-proxy-java--junit-version-file)
              (insert (plist-get release :version) "\n")))
        (when (file-exists-p temporary)
          (delete-directory temporary t))))))

;;;###autoload
(defun lsp-proxy-java-install-junit (&optional force)
  "Install or upgrade the JUnit Platform Console standalone JAR.
With prefix argument FORCE, reinstall an unchanged release."
  (interactive "P")
  (let* ((release (lsp-proxy-java--junit-release))
         (version (plist-get release :version))
         (installed (lsp-proxy-java--read-version-file
                     (lsp-proxy-java--junit-version-file))))
    (if (and (not force) (equal installed version)
             (file-regular-p (expand-file-name lsp-proxy-java-junit-console-jar)))
        (message "[lsp-proxy] JUnit Platform Console %s is already installed" version)
      (message "[lsp-proxy] Installing JUnit Platform Console %s..." version)
      (lsp-proxy-java--install-junit-release release)
      (message "[lsp-proxy] Installed JUnit Platform Console %s" version))))

(defun lsp-proxy-java--project-runtime (&optional uri)
  "Return the Project JDK decision plist for URI, or nil."
  (when (bound-and-true-p lsp-proxy-mode)
    (condition-case nil
        (let* ((params (lsp-proxy--build-params nil))
               (params (if uri
                           (plist-put params :textDocument (list :uri uri))
                         params))
               (_ (lsp-proxy--ensure-connection)))
          (jsonrpc-request lsp-proxy--connection 'emacs/getJavaProjectRuntime params))
      (jsonrpc-error nil)
      (user-error nil))))

(defun lsp-proxy-java--project-java-home (&optional uri)
  "Return the Project JDK home for URI, or nil."
  (plist-get (lsp-proxy-java--project-runtime uri) :javaHome))

(defun lsp-proxy-java--environment-value (name environment)
  "Return NAME's value from ENVIRONMENT."
  (when-let* ((entry (seq-find
                      (lambda (item) (string-prefix-p (concat name "=") item))
                      environment)))
    (substring entry (1+ (length name)))))

(defun lsp-proxy-java--environment-with-java-home (java-home &optional environment)
  "Return ENVIRONMENT adjusted to use JAVA-HOME."
  (let* ((environment (copy-sequence (or environment process-environment)))
         (old-java-home (lsp-proxy-java--environment-value "JAVA_HOME" environment))
         (old-java-bin (and old-java-home
                            (directory-file-name
                             (expand-file-name "bin" old-java-home))))
         (java-bin (directory-file-name (expand-file-name "bin" java-home)))
         (path-parts (split-string
                      (or (lsp-proxy-java--environment-value "PATH" environment) "")
                      path-separator t))
         (filtered-path
          (seq-remove
           (lambda (path)
             (let ((expanded (directory-file-name (expand-file-name path))))
               (or (equal expanded java-bin)
                   (and old-java-bin (equal expanded old-java-bin)))))
           path-parts))
         (remaining-environment
          (seq-remove
           (lambda (entry)
             (or (string-prefix-p "JAVA_HOME=" entry)
                 (string-prefix-p "PATH=" entry)))
           environment)))
    (cons (concat "JAVA_HOME=" java-home)
          (cons (concat "PATH="
                        (mapconcat #'identity
                                   (cons java-bin filtered-path)
                                   path-separator))
                remaining-environment))))

(defun lsp-proxy-java--parse-environment-entry (entry)
  "Return ENTRY as a (NAME . VALUE) pair."
  (unless (and (stringp entry)
               (string-match "\\`\\([[:alpha:]_][[:alnum:]_]*\\)=\\(.*\\)\\'" entry))
    (user-error "Invalid process environment entry: %S" entry))
  (cons (match-string 1 entry) (match-string 2 entry)))

(defun lsp-proxy-java--environment-merge (environment additions)
  "Return ENVIRONMENT with NAME=VALUE ADDITIONS applied."
  (let ((result (copy-sequence environment)))
    (dolist (entry additions result)
      (let* ((parsed (lsp-proxy-java--parse-environment-entry entry))
             (prefix (concat (car parsed) "=")))
        (setq result
              (cons entry
                    (seq-remove
                     (lambda (existing) (string-prefix-p prefix existing))
                     result)))))))

(defun lsp-proxy-java--environment-plist (entries)
  "Return ENTRIES as a DAP environment plist."
  (let (result)
    (dolist (entry entries result)
      (pcase-let ((`(,name . ,value)
                   (lsp-proxy-java--parse-environment-entry entry)))
        (setq result (plist-put result (intern (concat ":" name)) value))))))

(defun lsp-proxy-java--ensure-dape ()
  "Load Dape or report that the optional integration is unavailable."
  (unless (or (featurep 'dape) (require 'dape nil t))
    (user-error "Dape is not installed")))

(defun lsp-proxy-java--project-process-environment (&optional uri additions)
  "Return process environment for URI with Project JDK and ADDITIONS."
  (let ((environment (copy-sequence process-environment)))
    (when-let* ((java-home (lsp-proxy-java--project-java-home uri)))
      (setq environment (lsp-proxy-java--environment-with-java-home java-home environment)))
    (lsp-proxy-java--environment-merge environment additions)))

(defun lsp-proxy-java--start-compilation (arguments root operation &optional env-additions uri)
  "Run ARGUMENTS from ROOT, labelling the buffer with OPERATION.
Apply ENV-ADDITIONS and the Project JDK only to the spawned process."
  (let* ((program (car arguments))
         (default-directory root)
         (process-environment
          (lsp-proxy-java--project-process-environment uri env-additions))
         (command (lsp-proxy-java--shell-command arguments))
         (buffer-name (format "*lsp-proxy java %s:%s*"
                              operation
                              (file-name-nondirectory
                               (directory-file-name root)))))
    (unless (lsp-proxy-java--command-available-p program)
      (user-error "Build program is not executable: %s" program))
    (compilation-start command 'compilation-mode
                       (lambda (_mode) buffer-name))))

;;;###autoload
(defun lsp-proxy-java-build (&optional task)
  "Build the current Maven or Gradle module using TASK."
  (interactive)
  (let* ((root (lsp-proxy-java--nearest-build-root))
         (build-system (lsp-proxy-java--build-system root))
         (task (or task
                   (read-string "Task and arguments: "
                                (lsp-proxy-java--default-task build-system))))
         (arguments (append (lsp-proxy-java--build-command-prefix root build-system)
                            (split-string-and-unquote task))))
    (lsp-proxy-java--start-compilation arguments root "build")))

;;;###autoload
(defun lsp-proxy-java-test ()
  "Run the current Maven or Gradle module's standard test task."
  (interactive)
  (lsp-proxy-java-build "test"))

(defun lsp-proxy-java--backend (name)
  "Return the descriptor for backend NAME, or signal when it is unknown."
  (or (assoc name lsp-proxy-java--backends)
      (user-error "Unknown Java backend `%s'; expected one of %s"
                  name
                  (mapconcat #'car lsp-proxy-java--backends ", "))))

(defun lsp-proxy-java--backend-get (name prop)
  "Return PROP of backend NAME."
  (plist-get (cdr (lsp-proxy-java--backend name)) prop))

(defun lsp-proxy-java--backend-exe (name)
  "Return the launcher file name backend NAME installs on this platform."
  (lsp-proxy-java--backend-get
   name (if (eq system-type 'windows-nt) :exe-windows :exe)))

(defun lsp-proxy-java--backend-install-dir (name)
  "Return the configured install directory for backend NAME."
  (symbol-value (lsp-proxy-java--backend-get name :install-dir)))

(defun lsp-proxy-java--managed-launcher (install-dir exe)
  "Return the newest `<version>/bin/EXE' under INSTALL-DIR, or nil if none."
  (car
   (sort
    (seq-filter
     #'file-executable-p
     ;; The proxy flattens every install to `<version>/bin/<exe>', so the depth is
     ;; fixed regardless of how the archive itself was packed.
     (mapcar (lambda (dir) (expand-file-name (concat "bin/" exe) dir))
             (when (file-directory-p install-dir)
               (directory-files install-dir t "\\`[0-9]" t))))
    ;; Newest build first, comparing components numerically so that 263.10.0
    ;; sorts above 263.9.0 (string order gets that backwards).
    (lambda (a b)
      (lsp-proxy-java--version<
       (lsp-proxy-java--version-key b)
       (lsp-proxy-java--version-key a))))))

(defun lsp-proxy-java-backend-launcher (name)
  "Return the newest managed launcher for backend NAME, or nil if none.
Useful for pointing a `languages.toml' entry at the managed install."
  (lsp-proxy-java--managed-launcher (lsp-proxy-java--backend-install-dir name)
                                    (lsp-proxy-java--backend-exe name)))

(defun lsp-proxy-java-backend-bin-directory (name)
  "Return the `bin/' directory of the newest managed backend NAME, or nil."
  (when-let* ((launcher (lsp-proxy-java-backend-launcher name)))
    (file-name-directory launcher)))

(defun lsp-proxy-java-managed-bin-directories ()
  "Return the `bin/' directory of every managed Java backend that is installed.

Prepended to the proxy's PATH at startup so `languages.toml' can name each
server plainly:

  [language-server.intellij]
  command = \"intellij-server\"

  [language-server.jdtls]
  command = \"jdtls\"

That keeps absolute, machine-specific paths out of the config entirely.  Both
may be present at once: a project with Java on one backend and Kotlin on the
IntelliJ one needs both findable."
  (delq nil (mapcar (lambda (entry) (lsp-proxy-java-backend-bin-directory (car entry)))
                    lsp-proxy-java--backends)))

(defun lsp-proxy-java-server-launcher ()
  "Return the newest managed IntelliJ server launcher, or nil if none.
Kept for configurations that referred to it before the installer grew a
second backend; prefer `lsp-proxy-java-backend-launcher'."
  (lsp-proxy-java-backend-launcher "intellij"))

(defun lsp-proxy-java-server-bin-directory ()
  "Return the `bin/' directory of the newest managed IntelliJ server, or nil.
Kept for configurations that referred to it before the installer grew a
second backend; prefer `lsp-proxy-java-backend-bin-directory'."
  (lsp-proxy-java-backend-bin-directory "intellij"))

(defun lsp-proxy-java--version-key (launcher)
  "Return a comparable list of integers for LAUNCHER's version directory.
LAUNCHER is `<version>/bin/<exe>', so the version sits two levels up."
  (let ((dir launcher))
    (dotimes (_ 2)
      (setq dir (directory-file-name (file-name-directory dir))))
    (mapcar #'string-to-number
            (split-string (file-name-nondirectory dir) "\\." t))))

(defun lsp-proxy-java--version< (a b)
  "Return non-nil when version key A orders before B.
Missing components count as zero, so 263.1 precedes 263.1.1."
  (catch 'done
    (while (or a b)
      (let ((x (or (pop a) 0))
            (y (or (pop b) 0)))
        (unless (= x y)
          (throw 'done (< x y)))))
    nil))

(defun lsp-proxy-java--runtime-warning (backend)
  "Return a note about anything BACKEND needs at runtime and cannot find, or nil.

The install cannot detect this for itself: the proxy only unpacks an archive,
and what is inside may still shell out to things it does not ship — `bin/jdtls'
is a `#!/usr/bin/env python3' script that in turn looks for a JDK.  Unreported,
each is a failure to start, surfacing far from its cause."
  (let ((problems (delq nil (mapcar #'funcall
                                    (lsp-proxy-java--backend-get
                                     backend :runtime-requires)))))
    (when problems
      (format "  WARNING: %s cannot start yet — it needs %s."
              backend (mapconcat #'identity problems "; and it needs ")))))

;;;###autoload
(defun lsp-proxy-java-install-server (&optional force backend)
  "Download and install a Java language server BACKEND.

BACKEND is one of the keys of `lsp-proxy-java--backends' — \"intellij\" (which
also serves Kotlin) or \"jdtls\".  Interactively it is read from the
minibuffer, defaulting to \"intellij\"; with a prefix argument FORCE,
reinstall even if that version is already present.

The version installed is the backend's own option
\(`lsp-proxy-java-server-version' or `lsp-proxy-java-jdtls-version'), or the
latest build when that is nil.

The work happens in the proxy; progress is shown in the mode line while it runs
\(hover it for the current phase).  On success the launcher path is reported.
Its `bin/' is put on the proxy's PATH at startup, so `languages.toml' can go on
naming the server plainly — but PATH is fixed when the proxy starts, so a fresh
install needs `lsp-proxy-restart' before it is visible."
  (interactive
   (list current-prefix-arg
         (completing-read "Install Java backend: "
                          (mapcar #'car lsp-proxy-java--backends)
                          nil t nil nil "intellij")))
  (lsp-proxy--ensure-connection)
  (let* ((backend (or backend "intellij"))
         (label (lsp-proxy-java--backend-get backend :label))
         (tag (lsp-proxy-java--backend-get backend :tag))
         (version (symbol-value (lsp-proxy-java--backend-get backend :version)))
         (dir (file-name-as-directory
               (expand-file-name (lsp-proxy-java--backend-install-dir backend)))))
    (message "[lsp-proxy] Installing %s into %s ..." label dir)
    ;; Set immediately rather than waiting for the first progress notification:
    ;; looking up the latest version takes a moment, and an indicator that only
    ;; appears seconds after the command looks like the command did nothing.
    (lsp-proxy--set-global-status tag (format "Starting %s install" label))
    (jsonrpc-async-request
     lsp-proxy--connection 'emacs/installJavaServer
     ;; The proxy wraps every request in a `{uri, context, params}' envelope and
     ;; reads the payload out of the nested `params'; a flat payload lands in
     ;; `req.params.params' as null. Built by hand rather than with
     ;; `lsp-proxy--build-params', which derives a `uri' from `buffer-file-name' —
     ;; installing a server is not about any open file. `uri' is optional server-side.
     (list
      :params
      (append
       (list :installDir dir
             :backend backend
             :force (if force t :json-false))
       ;; Omit the key rather than encoding a null: this connection serializes with
       ;; `:null-object nil', so a `:null' keyword is not a valid JSON value at all.
       ;; The proxy's `version' field is `#[serde(default)]', so absent means latest.
       (when version
         (list :version version))))
     :success-fn
     (lambda (result)
       (lsp-proxy--clear-global-status)
       (let ((path (plist-get result :launcherPath))
             (version (plist-get result :version)))
         (if (eq (plist-get result :alreadyInstalled) t)
             (message "[lsp-proxy] %s %s already installed: %s" label version path)
           ;; The proxy inherits PATH at startup, so a freshly installed server only
           ;; becomes findable after a restart.
           (message "[lsp-proxy] Installed %s %s. Run M-x lsp-proxy-restart to use it (%s)%s"
                    label version path
                    (or (lsp-proxy-java--runtime-warning backend) "")))
         ;; jdtls is unusable on a Lombok project without the agent, so installing
         ;; the server without it would leave the user with a working server that
         ;; reports thousands of errors that are not there. Chained rather than
         ;; required: a failure to fetch it is reported on its own and does not
         ;; retract the server install that just succeeded.
         (when (and (lsp-proxy-java--backend-get backend :wants-lombok)
                    (fboundp 'lsp-proxy-java--install-lombok))
           (lsp-proxy-java--install-lombok
            nil
            (lambda (report)
              (when report (message "[lsp-proxy] %s" report)))))
         (when (and (lsp-proxy-java--backend-get backend :wants-lombok)
                    (fboundp 'lsp-proxy-java--install-debug))
           (lsp-proxy-java--install-debug
            nil
            (lambda (report)
              (when report (message "[lsp-proxy] %s" report)))))))
     :error-fn
     (lambda (err)
       (lsp-proxy--clear-global-status)
       (lsp-proxy--error "%s install failed: %s" label
                         (or (plist-get err :message) err)))
     ;; Was `ignore'. With an hour-long timeout that would leave the mode-line
     ;; indicator up for the rest of the session if it ever fired.
     :timeout-fn
     (lambda ()
       (lsp-proxy--clear-global-status)
       (lsp-proxy--error "%s install timed out" label))
     ;; The transfer is tens to hundreds of megabytes; the default request timeout
     ;; is far too short for it.
     :timeout 3600)))

;;; Missing backend

(defun lsp-proxy-java--install-hint (server-name)
  "Return a generic install hint for SERVER-NAME, or nil when unknown."
  (when (assoc server-name lsp-proxy-java--backends)
    (format "Run M-x lsp-proxy-java-install-server and choose `%s' to install it."
            server-name)))

(add-hook 'lsp-proxy-server-install-hint-functions #'lsp-proxy-java--install-hint)

;;; Setup

(add-to-list 'file-name-handler-alist
             (cons lsp-proxy--decompiled-file-name-regexp
                   #'lsp-proxy-java--file-handler))
(add-hook 'find-file-hook #'lsp-proxy-java--setup-buffer)

;; No `auto-mode-alist' entry on purpose: `set-auto-mode' strips the
;; `file-remote-p' prefix before matching, so a pattern anchored on the lspsrc
;; prefix can never fire. `lsp-proxy-java--setup-buffer' sets the mode instead.

(defun lsp-proxy-java-unload-function ()
  "Deregister the decompiled-source handler.  See `unload-feature'."
  (setq file-name-handler-alist
        (rassq-delete-all #'lsp-proxy-java--file-handler
                          (copy-sequence file-name-handler-alist)))
  (remove-hook 'find-file-hook #'lsp-proxy-java--setup-buffer)
  nil)

(provide 'lsp-proxy-java)

;;; lsp-proxy-java.el ends here
