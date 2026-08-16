;;; lsp-proxy-utils.el --- Utility functions for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utility functions and helpers for lsp-proxy

;;; Code:

(require 'cl-lib)
(require 'url-util)
(require 'project)
(require 'eglot)

(defvar lsp-proxy-mode)
(defvar lsp-proxy-enable-org-babel)
(defvar lsp-proxy-org-babel--info-cache)

(defcustom lsp-proxy-log-buffer-max message-log-max
  "Maximum number of lines to keep in the log buffer.
If nil, disable message logging.  If t, log messages but don't truncate
the buffer when it becomes large."
  :group 'lsp-proxy
  :type '(choice (const :tag "Disable" nil)
          (integer :tag "lines")
          (const :tag "Unlimited" t)))

(defcustom lsp-proxy-idle-delay 0.500
  "Debounce interval for `after-change-functions'."
  :type 'number
  :group 'lsp-proxy)

(defcustom lsp-proxy-on-idle-hook nil
  "Hooks to run after `lsp-proxy-idle-delay'."
  :type 'hook
  :group 'lsp-proxy)

;;; Message and logging functions

(defvar lsp-proxy--show-message t
  "Whether to show lsp-proxy messages.")

(defconst lsp-proxy--message-type-face
  `((1 . ,compilation-error-face)
    (2 . ,compilation-warning-face)
    (3 . ,compilation-message-face)
    (4 . ,compilation-info-face))
  "Faces for different message types.")

(defun lsp-proxy--message (format &rest args)
  "Wrapper for `message'.

We `inhibit-message' the message when the cursor is in the
minibuffer and when emacs version is before emacs 27 due to the
fact that we often use `lsp--info', `lsp--warn' and `lsp--error'
in async context and the call to these function is removing the
minibuffer prompt. The issue with async messages is already fixed
in emacs 27.

See #2049"
  (when lsp-proxy--show-message
    (let ((inhibit-message (or inhibit-message
                               (and (minibufferp)
                                    (version< emacs-version "27.0")))))
      (apply #'message format args))))

(defun lsp-proxy--info (format &rest args)
  "Display lsp info message with FORMAT with ARGS."
  (lsp-proxy--message "%s :: %s" (propertize "LSP-PROXY" 'face 'success) (apply #'format format args)))

(defun lsp-proxy--warn (format &rest args)
  "Display lsp warn message with FORMAT with ARGS."
  (lsp-proxy--message "%s :: %s" (propertize "LSP-PROXY" 'face 'warning) (apply #'format format args)))

(defun lsp-proxy--error (format &rest args)
  "Display lsp error message with FORMAT with ARGS."
  (lsp-proxy--message "%s :: %s" (propertize "LSP-PROXY" 'face 'error) (apply #'format format args)))

(defun lsp-proxy--propertize (str type)
  "Propertize STR as per TYPE."
  (propertize str 'face (alist-get type lsp-proxy--message-type-face)))

;;; Logging functionality

(defvar lsp-proxy--log-lines)

(defun lsp-proxy-log (format &rest args)
  "Log message to the *lsp-proxy-log* buffer.
FORMAT and ARGS is the same as for `message'."
  (when lsp-proxy-log-buffer-max
    (let ((log-buffer (get-buffer "*lsp-proxy-log*"))
          (inhibit-read-only t))
      (unless log-buffer
        (setq log-buffer (get-buffer-create "*lsp-proxy-log*"))
        (with-current-buffer log-buffer
          (buffer-disable-undo)
          (view-mode 1)
          (set (make-local-variable 'lsp-proxy--log-lines) 0)))
      (with-current-buffer log-buffer
        (save-excursion
          (let* ((message (apply 'format format args))
                 ;; Count newlines in message.
                 (newlines (1+ (cl-loop with start = 0
                                        for count from 0
                                        while (string-match "\n" message start)
                                        do (setq start (match-end 0))
                                        finally return count))))
            (goto-char (point-max))

            ;; in case the buffer is not empty insert before last \n to preserve
            ;; the point position(in case it is in the end)
            (if (eq (point) (point-min))
                (progn
                  (insert "\n")
                  (backward-char))
              (backward-char)
              (insert "\n"))
            (insert message)

            (setq lsp-proxy--log-lines (+ lsp-proxy--log-lines newlines))

            (when (and (integerp lsp-proxy-log-buffer-max) (> lsp-proxy--log-lines lsp-proxy-log-buffer-max))
              (let ((to-delete (- lsp-proxy--log-lines lsp-proxy-log-buffer-max)))
                (goto-char (point-min))
                (forward-line to-delete)
                (delete-region (point-min) (point))
                (setq lsp-proxy--log-lines lsp-proxy-log-buffer-max)))))))))

;;; Project management

(defvar-local lsp-proxy--current-project-root nil
  "Cached project root for current buffer.")

(defun lsp-proxy-project-root ()
  "Return the project root of current project."
  (if lsp-proxy--current-project-root
      lsp-proxy--current-project-root
    (let* ((project (project-current))
           (root (and project (project-root project)))
           (root-path (and root (directory-file-name root))))
      (setq lsp-proxy--current-project-root root-path)
      root-path)))

;;; Pattern matching utilities

(eval-and-compile
  (defun lsp-proxy--transform-pattern (pattern)
    "Transform PATTERN to (&plist PATTERN) recursively."
    (cons '&plist
          (mapcar (lambda (p)
                    (if (listp p)
                        (lsp-proxy--transform-pattern p)
                      p))
                  pattern))))

(defmacro lsp-proxy--dbind (pattern source &rest body)
  "Destructure SOURCE against plist PATTERN and eval BODY."
  (declare (indent 2))
  `(-let ((,(lsp-proxy--transform-pattern pattern) ,source))
     ,@body))

;;; Path utilities

(defun lsp-proxy--fix-path-casing (path)
  "On windows, downcases path because the windows file system is
case-insensitive.

On other systems, returns path without change."
  (if (eq system-type 'windows-nt) (downcase path) path))

(defun lsp-proxy--normalize-path (path)
  "On Windows systems, normalize path separators to Unix-style.
If the system is not Windows, return the original path."
  (if (eq system-type 'windows-nt)
      (replace-regexp-in-string "\\\\" "/" path)
    path))

(declare-function w32-long-file-name "w32proc.c" (fn))

;;; Virtual names for decompiled sources
;;
;; Language servers hand us `jar:'/`jrt:' URIs for JDK/library sources that have
;; no path on disk. A URI is not a file name in Emacs' model (`file-name-absolute-p'
;; is nil for it), so exposing one as `buffer-file-name' breaks every piece of
;; path arithmetic in Emacs and in third-party packages.
;;
;; Instead we follow TRAMP: map each URI to a genuine *absolute* file name and
;; let a `file-name-handler-alist' entry (see `lsp-proxy-java') supply the
;; content. The shape is
;;
;;     /lspsrc:/<original-uri, minimally escaped>
;;
;; e.g. jrt:///opt/jdk!/modules/java.base/java/lang/System.class becomes
;;      /lspsrc:/jrt:%2F%2F/opt/jdk!/modules/java.base/java/lang/System.class
;;
;; This mirrors TRAMP's `/method:host:/remote/path': the payload is embedded
;; *verbatim* and stays path-shaped, so it reads like a path, `file-name-nondirectory'
;; gives a real basename, and anything that shortens paths for display (ibuffer,
;; doom-modeline, recentf) has something sensible to shorten.  Escaping is kept to
;; the three characters a file name genuinely cannot carry, which is why the
;; original text remains legible.
;;
;; Two shape constraints are load-bearing:
;;
;;  * The `/' immediately after `lspsrc:' is required.  Without it the name
;;    (`/lspsrc:jrt:...') matches `tramp-file-name-regexp' as method `lspsrc' +
;;    host `jrt' + localname, and TRAMP claims it and fails with
;;    "Method `lspsrc' is not known".  With the slash, TRAMP's mandatory second
;;    colon cannot line up, so the URI may keep its own colons verbatim.
;;
;;  * The escaped form must be a fixed point of `expand-file-name': no run of
;;    two or more slashes, no `.'/`..' path segment, and no leading or trailing
;;    slash in the payload.  Those are exactly what the escaping removes.
;;
;; Everything in this section is a pure string transform: it must never perform
;; I/O or RPC, because `lsp-proxy--uri-to-path' is called once per location in
;; results as large as a project-wide `textDocument/references'.

(defconst lsp-proxy--decompiled-prefix "/lspsrc:/"
  "Prefix marking a virtual file name backed by the `decompile' command.
The trailing slash is required to keep TRAMP from claiming the name; see the
commentary above.")

(defconst lsp-proxy--decompiled-file-name-regexp
  (concat "\\`" (regexp-quote lsp-proxy--decompiled-prefix))
  "Regexp matching a decompiled virtual file name.

Matches the prefix only, not a full name: primitives normalise names before
dispatching — `file-directory-p' routes `directory-file-name' first — so the
handler is also asked about ancestor directories of the file.  Anchoring on
more than the prefix makes those operations silently miss the handler.")

(defun lsp-proxy--decompiled-scheme-p (uri)
  "Return non-nil when URI uses a scheme served via the `decompile' command."
  (let ((u (if (keywordp uri) (substring (symbol-name uri) 1) uri)))
    (and (stringp u) (string-match-p "\\`\\(?:jar\\|jrt\\):" u))))

(defun lsp-proxy--decompiled-escape (uri)
  "Escape URI just enough to be usable as the tail of a virtual file name.
Only `%', slash runs and `.'/`..' segments are touched; every other character —
including the URI's own colons and any non-ASCII — is kept verbatim, which is
what keeps the resulting name readable."
  (let* (;; Escape `%' first so the escapes we add below are unambiguous.
         (s (replace-regexp-in-string "%" "%25" uri t t))
         ;; Collapse runs of slashes: keep one real separator, escape the rest.
         (s (replace-regexp-in-string
             "//+"
             (lambda (run)
               (concat (mapconcat #'identity
                                  (make-list (1- (length run)) "%2F") "")
                       "/"))
             s t t))
         ;; The prefix already supplies the leading separator, and a trailing
         ;; slash would make the name look like a directory.
         (s (replace-regexp-in-string "\\`/" "%2F" s t t))
         (s (replace-regexp-in-string "/\\'" "%2F" s t t)))
    ;; No empty segments remain, so a segment-wise pass is safe and cannot miss
    ;; adjacent dot segments the way a single regexp over the whole string would.
    (mapconcat (lambda (seg)
                 (pcase seg ("." "%2E") (".." "%2E%2E") (_ seg)))
               (split-string s "/")
               "/")))

(defun lsp-proxy--decompiled-unescape (tail)
  "Inverse of `lsp-proxy--decompiled-escape' for TAIL.
Single left-to-right pass, so an escape we introduced is never confused with
one that was already present in the original URI."
  (replace-regexp-in-string
   "%25\\|%2F\\|%2E"
   (lambda (m) (pcase m ("%25" "%") ("%2F" "/") ("%2E" ".")))
   tail t t))

(defvar lsp-proxy--decompiled-known-names (make-hash-table :test 'equal)
  "Set of virtual names handed out by `lsp-proxy--decompiled-uri-to-file-name'.

Needed because the name embeds the URI losslessly, which means every ancestor
directory of a member is itself a well-formed name for its own shorter URI —
so, unlike a scheme that derives the basename, the name's *form* cannot tell a
servable member from one of the directories above it.  Recording what we handed
out answers that exactly, and keeps probes for `.git' or `.dir-locals.el' inside
the tree from being reported as existing files.")

(defconst lsp-proxy--decompiled-source-extensions
  '("java" "kt" "kts" "class" "scala" "groovy" "clj")
  "Extensions treated as servable members when the name is not in the registry.
Only a fallback for names restored from a previous session (desktop, recentf),
where the registry is empty; names produced in this session are matched exactly.")

(defvar lsp-proxy--decompiled-known-dirs (make-hash-table :test 'equal)
  "Set of virtual directories that are ancestors of a name we handed out.

Needed to keep the virtual filesystem self-consistent.  Reporting every
prefix-matching name as a directory would make probes inside the tree — `.git',
`.dir-locals.el' — look like existing directories, and `locate-dominating-file'
would stop at a bogus root.  Reporting none of them makes `file-exists-p' deny a
directory that `file-directory-p' affirms, which breaks any caller that
sanity-checks a directory: flycheck validates `default-directory' with
`file-exists-p' and errors out with \":working-directory ... does not exist\".

Being an ancestor of a name we actually served distinguishes the two exactly.")

(defun lsp-proxy--decompiled-register (name)
  "Record NAME as a served virtual file, plus each of its ancestor directories."
  (puthash name t lsp-proxy--decompiled-known-names)
  (let ((dir (file-name-directory name)))
    (while (and dir
                (string-prefix-p lsp-proxy--decompiled-prefix dir)
                (not (gethash dir lsp-proxy--decompiled-known-dirs)))
      (puthash dir t lsp-proxy--decompiled-known-dirs)
      (let ((parent (file-name-directory (directory-file-name dir))))
        (setq dir (unless (equal parent dir) parent)))))
  name)

(defun lsp-proxy--decompiled-uri-to-file-name (uri)
  "Return the virtual absolute file name representing URI."
  (lsp-proxy--decompiled-register
   (concat lsp-proxy--decompiled-prefix (lsp-proxy--decompiled-escape uri))))

(defun lsp-proxy--decompiled-file-name-to-uri (name)
  "Return the original `jar:'/`jrt:' URI encoded in NAME, or nil.
For an ancestor directory of the virtual file this returns the URI prefix that
directory corresponds to, which is not itself a servable URI; use
`lsp-proxy--decompiled-file-name-p' to test for the real thing."
  (when (and (stringp name)
             (string-prefix-p lsp-proxy--decompiled-prefix name))
    (lsp-proxy--decompiled-unescape
     (substring name (length lsp-proxy--decompiled-prefix)))))

(defun lsp-proxy--decompiled-file-name-p (name)
  "Return non-nil when NAME denotes a servable decompiled member.
Nil for the archive's intermediate directories and for probes at names we never
handed out (`.git', `.dir-locals.el', backup names), so the handler can report
those as nonexistent instead of claiming everything under the prefix."
  (and (stringp name)
       (string-prefix-p lsp-proxy--decompiled-prefix name)
       (or (gethash name lsp-proxy--decompiled-known-names)
           (member (file-name-extension name)
                   lsp-proxy--decompiled-source-extensions))
       t))

(defun lsp-proxy--decompiled-name-split (name)
  "Split NAME into (PREFIX . LOCALNAME) at the archive/member boundary.

PREFIX and LOCALNAME concatenate back to NAME exactly — that is the contract
`file-remote-p' must satisfy, and it is what lets `file-local-name' return just
the member path.  Display code reuses this without knowing anything about us:
doom-modeline, for instance, runs `buffer-file-name' through `file-local-name'
before formatting, which is precisely how it shortens TRAMP names.

The split is at the *last* `!/', so a nested archive yields the innermost
member path."
  (when (and (stringp name)
             (string-prefix-p lsp-proxy--decompiled-prefix name))
    (let ((bang (string-match-p "!/[^!]*\\'" name)))
      (if bang
          (cons (substring name 0 (1+ bang)) (substring name (1+ bang)))
        ;; No member separator (an opaque URI): treat the whole payload as the
        ;; local part, keeping the prefix as the "remote" component.
        (cons (substring lsp-proxy--decompiled-prefix 0 -1)
              (substring name (1- (length lsp-proxy--decompiled-prefix))))))))

(defun lsp-proxy--decompiled-buffer-p (&optional buffer)
  "Return non-nil when BUFFER (default current) shows a decompiled virtual source."
  (and (lsp-proxy--decompiled-file-name-to-uri
        (buffer-local-value 'buffer-file-name (or buffer (current-buffer))))
       t))

(defun lsp-proxy--decompiled-directory-p (name)
  "Return non-nil when NAME is a directory inside a decompiled archive.

True only for ancestors of a name we actually served (see
`lsp-proxy--decompiled-known-dirs'), so path walks see a consistent tree while
probes for files we do not serve still miss."
  (and (stringp name)
       (gethash (file-name-as-directory name) lsp-proxy--decompiled-known-dirs)
       t))

(defun lsp-proxy--path-to-uri (path)
  "Convert PATH to an LSP `file://' URI.
Unlike `eglot-path-to-uri', this preserves a TRAMP prefix (`/ssh:host:')
rather than stripping it. lsp-proxy's Rust backend uses that prefix as
the sole signal for routing the request to a remote LSP server; if we
let eglot drop it, every buffer looks local and remote mode never
engages.

A decompiled virtual name round-trips back to the original `jar:'/`jrt:'
URI rather than being wrapped in a bogus `file:///lspsrc:/...'."
  (or (lsp-proxy--decompiled-file-name-to-uri path)
      (let ((remote-prefix (and path (file-remote-p path))))
        (if remote-prefix
            (concat "file://"
                    remote-prefix
                    (url-hexify-string
                     (substring path (length remote-prefix))
                     url-path-allowed-chars))
          (concat "file://"
                  (if (eq system-type 'windows-nt) "/" "")
                  (url-hexify-string path url-path-allowed-chars))))))

(defun lsp-proxy--TextDocumentIdentifier ()
  "Build a TextDocumentIdentifier for the current buffer.
Drop-in replacement for `eglot--TextDocumentIdentifier' that uses
`lsp-proxy--path-to-uri', so TRAMP-rooted buffers produce URIs the
Rust-side remote router can recognise."
  (let ((path (or buffer-file-name
                  (ignore-errors (buffer-file-name (buffer-base-buffer))))))
    (unless path
      (error "lsp-proxy: buffer has no file name"))
    (list :uri (lsp-proxy--path-to-uri path))))

(defun lsp-proxy--VersionedTextDocumentIdentifier ()
  "Build a VersionedTextDocumentIdentifier for the current buffer.
Mirrors `eglot--VersionedTextDocumentIdentifier' but routes the URI
through `lsp-proxy--path-to-uri' so TRAMP prefixes survive.
Reads the version directly from the eglot-side buffer-local variables
to avoid a circular require on `lsp-proxy-core'."
  (let ((version (cond ((boundp 'eglot--docver) eglot--docver)
                       ((boundp 'eglot--versioned-identifier)
                        eglot--versioned-identifier)
                       (t 0))))
    (append (lsp-proxy--TextDocumentIdentifier)
            (list :version version))))

(defun lsp-proxy--TextDocumentPositionParams ()
  "Build a TextDocumentPositionParams for the current buffer + point.
Mirrors `eglot--TextDocumentPositionParams' but the embedded URI goes
through `lsp-proxy--path-to-uri', keeping TRAMP prefixes intact so the
Rust remote router can dispatch the request."
  (list :textDocument (lsp-proxy--TextDocumentIdentifier)
        :position (eglot--pos-to-lsp-position)))

(defun lsp-proxy--uri-to-path (uri)
  "Convert URI to file path.
When the URI's path already carries a TRAMP method marker (`/ssh:' or
`/rpc:') the Rust backend preserved the remote identity in the URI
itself — we must NOT glue the project's own remote-prefix on top, or
the path ends up with the method/host segment doubled (which then
fails to open on the remote FS)."
  (when (keywordp uri) (setq uri (substring (symbol-name uri) 1)))
  (let* ((project-root lsp-proxy--current-project-root)
         ;; Only a genuine TRAMP prefix may be glued onto a resolved path. A
         ;; decompiled buffer reports `file-remote-p' too (that is what gives it
         ;; TRAMP's short display), so if such a name ever ends up cached as the
         ;; project root, an unguarded `file-remote-p' here would prefix every
         ;; real `file://' location with `/lspsrc:/...!' — corrupting navigation
         ;; out of a decompiled buffer back into project sources.
         (remote-prefix (and project-root
                             (not (lsp-proxy--decompiled-file-name-to-uri project-root))
                             (file-remote-p project-root)))
         (url (url-generic-parse-url uri)))
    ;; Only parse file:// URIs, leave other URIs untouched as
    ;; `file-name-handler-alist' should know how to handle them
    ;; (bug#58790).
    (if (string= "file" (url-type url))
        ;; `url-unhex-string' yields the percent-decoded *bytes*; decoding them
        ;; as UTF-8 is what turns them back into a real Emacs string. This is
        ;; the exact inverse of `url-hexify-string' in `lsp-proxy--path-to-uri',
        ;; which encodes multibyte input as UTF-8, and matches the LSP spec.
        ;;
        ;; Skipping the decode still *opens* the file (Emacs re-encodes the raw
        ;; bytes on the way to the syscall) but produces a string that is not
        ;; `equal' to the same path anywhere else in Emacs. That silently breaks
        ;; every path-keyed lookup for non-ASCII file names — notably
        ;; `lsp-proxy--diagnostics-map', which is written under this path and
        ;; read back under `buffer-file-name', so diagnostics never render.
        (let* ((retval (decode-coding-string
                        (url-unhex-string (url-filename url)) 'utf-8))
               (already-tramp (or (string-prefix-p "/ssh:" retval)
                                  (string-prefix-p "/rpc:" retval)))
               ;; Remove the leading "/" for local MS Windows-style paths.
               (normalized (if (and (not remote-prefix)
                                    (not already-tramp)
                                    (eq system-type 'windows-nt)
                                    (cl-plusp (length retval))
                                    (eq (aref retval 0) ?/))
                               (w32-long-file-name (substring retval 1))
                             retval)))
          (if already-tramp
              normalized
            (concat remote-prefix normalized)))
      ;; `jar:'/`jrt:' become virtual absolute file names whose content the
      ;; `lsp-proxy-java' handler fetches on first read. This is a pure string
      ;; transform on purpose — no RPC here, or a references result spanning the
      ;; JDK would fire one synchronous request per location.
      ;;
      ;; Any other non-file scheme passes through untouched so
      ;; `file-name-handler-alist' can deal with it (bug#58790).
      (if (lsp-proxy--decompiled-scheme-p uri)
          (lsp-proxy--decompiled-uri-to-file-name uri)
        uri))))

;;; Request parameters

(declare-function lsp-proxy--make-virtual-doc-context "lsp-proxy-core")

(defun lsp-proxy--should-skip-request-p ()
  "Return non-nil if LSP request should be skipped.
In org-mode with `lsp-proxy-enable-org-babel' enabled, requests are
only allowed when cursor is inside a code block."
  (and lsp-proxy-enable-org-babel
       (eq major-mode 'org-mode)
       (not lsp-proxy-org-babel--info-cache)))

(defun lsp-proxy--build-params (params &rest args)
  "Build complete request/notify params from base PARAMS and extra ARGS.
Automatically adds virtual-doc context when in org babel block.

The virtual-doc context is orthogonal to request-specific context
and is used for position translation between the org file and the
virtual document sent to the language server."
  (let* ((rest (if (and args (not (sequencep (car args))))
                   ;; If first arg is not a sequence (like :context), treat as plist
                   args
                 ;; Otherwise, flatten as before
                 (apply 'append args)))
         (base-params (append (lsp-proxy--TextDocumentIdentifier)
                              `(:params ,params)
                              rest))
         (virtual-doc (lsp-proxy--make-virtual-doc-context)))
    (if virtual-doc
        (append base-params `(:virtual-doc ,virtual-doc))
      base-params)))


;;; Hash table project management utilities

(defun lsp-proxy--add-project (project-root-path project-map)
  "Add PROJECT-ROOT-PATH to PROJECT-MAP."
  (puthash project-root-path (make-hash-table :test 'equal) project-map))

(defun lsp-proxy--remove-project (project-root-path project-map)
  "Remove PROJECT-ROOT-PATH from PROJECT-MAP."
  (remhash project-root-path project-map))

(defun lsp-proxy--ensure-project-map (project-root project-map)
  "Ensure PROJECT-ROOT exists in PROJECT-MAP, creating if necessary.
Returns the hash table for the project."
  (or (gethash project-root project-map)
      (puthash project-root (make-hash-table :test 'equal) project-map)))

;;; Formatting utilities

(defun lsp-proxy--format-file-size (bytes)
  "Format file size BYTES in human readable format."
  (cond
   ((< bytes 1024) (format "%dB" bytes))
   ((< bytes (* 1024 1024)) (format "%.1fKB" (/ bytes 1024.0)))
   ((< bytes (* 1024 1024 1024)) (format "%.1fMB" (/ bytes 1024.0 1024.0)))
   (t (format "%.1fGB" (/ bytes 1024.0 1024.0 1024.0)))))

;;; Text Edit utilities

(defun lsp-proxy--create-apply-text-edits-handlers ()
  "Create (handler cleanup-fn) for applying text edits in async request.
Only works when mode is `tick or `alive."
  (let* (first-edited
         (func (lambda (start &rest _)
                 (setq first-edited (if first-edited
                                        (min start first-edited)
                                      start)))))
    (add-hook 'before-change-functions func nil t)
    (list
     (lambda (edits)
       (if (and first-edited
                (seq-find (lambda (edit) (let* ((range (plist-get edit :range))
                                                (end (plist-get range :end))
                                                (end-point (eglot--lsp-position-to-point end)))
                                           (> end-point first-edited)))
                          edits))
           (lsp-proxy--warn "%s" "TextEdits will not be applied since document has been modified before of them.")
         (eglot--apply-text-edits edits)))
     (lambda ()
       (remove-hook 'before-change-functions func t)))))

;;; Idle handling
(defvar-local lsp-proxy--on-idle-timer nil)

(defun lsp-proxy--idle-reschedule (buffer)
  "Reschedule idle timer for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when lsp-proxy--on-idle-timer
        (cancel-timer lsp-proxy--on-idle-timer))
      (setq lsp-proxy--on-idle-timer
            (run-with-idle-timer lsp-proxy-idle-delay nil
                                 #'lsp-proxy--on-idle buffer)))))

(defun lsp-proxy--on-idle (buffer)
  "Handle idle timeout for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq lsp-proxy--on-idle-timer nil)
      (when lsp-proxy-mode
        (run-hooks 'lsp-proxy-on-idle-hook)))))

(defun lsp-proxy--propose-changes-as-diff (prepared)
  "Helper for `lsp-proxy--apply-workspace-edit'.
Goal is to popup a `diff-mode' buffer containing all the changes
of PREPARED, ready to apply with C-c C-a.  PREPARED is a
list ((FILENAME EDITS VERSION)...)."
  (with-current-buffer (get-buffer-create "*EGLOT proposed server changes*")
    (buffer-disable-undo (current-buffer))
    (let ((inhibit-read-only t)
          (target (current-buffer)))
      (diff-mode)
      (erase-buffer)
      (pcase-dolist (`(,path ,edits ,_) prepared)
        (with-temp-buffer
          (let* ((diff (current-buffer))
                 (existing-buf (find-buffer-visiting path))
                 (existing-buf-label (prin1-to-string existing-buf)))
            (with-temp-buffer
              (if existing-buf
                  (insert-buffer-substring existing-buf)
                (insert-file-contents path))
              (eglot--apply-text-edits edits nil t)
              (diff-no-select (or existing-buf path) (current-buffer) nil t diff)
              (when existing-buf
                ;; Here we have to pretend the label of the unsaved
                ;; buffer is the actual file, just so that we can
                ;; diff-apply without troubles.  If there's a better
                ;; way, it probably involves changes to `diff.el'.
                (with-current-buffer diff
                  (goto-char (point-min))
                  (while (search-forward existing-buf-label nil t)
                    (replace-match (buffer-file-name existing-buf))))))
            (with-current-buffer target
              (insert-buffer-substring diff))))))
    (setq-local buffer-read-only t)
    (buffer-enable-undo (current-buffer))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))
    (font-lock-ensure)))

(defun lsp-proxy--apply-workspace-edit (wedit origin)
  "Apply (or offer to apply) the workspace edit WEDIT.
ORIGIN is a symbol designating the command that originated this
edit proposed by the server."
  (eglot--dbind ((WorkspaceEdit) changes documentChanges) wedit
    (let ((prepared
           (mapcar (eglot--lambda ((TextDocumentEdit) textDocument edits)
                     (eglot--dbind ((VersionedTextDocumentIdentifier) uri version)
                         textDocument
                       (list (lsp-proxy--uri-to-path uri) edits version)))
                   documentChanges)))
      (unless (and changes documentChanges)
        ;; We don't want double edits, and some servers send both
        ;; changes and documentChanges.  This unless ensures that we
        ;; prefer documentChanges over changes.
        (cl-loop for (uri edits) on changes by #'cddr
                 do (push (list (lsp-proxy--uri-to-path uri) edits) prepared)))
      (cl-flet ((notevery-visited-p ()
                  (cl-notevery #'find-buffer-visiting
                               (mapcar #'car prepared)))
                (accept-p ()
                  (y-or-n-p
                   (format "[eglot] Server wants to edit:\n%sProceed? "
                           (cl-loop
                            for (f eds _) in prepared
                            concat (format
                                    "  %s (%d change%s)\n"
                                    f (length eds)
                                    (if (> (length eds) 1) "s" ""))))))
                (apply ()
                  (cl-loop for edit in prepared
                   for (path edits version) = edit
                   do (with-current-buffer (find-file-noselect path)
                        (eglot--apply-text-edits edits version))
                   finally (eldoc) (eglot--message "Edit successful!"))))
        (let ((decision (eglot--confirm-server-edits origin prepared)))
          (cond
           ((or (eq decision 'diff)
                (and (eq decision 'maybe-diff) (notevery-visited-p)))
            (lsp-proxy--propose-changes-as-diff prepared))
           ((or (memq decision '(t summary))
                (and (eq decision 'maybe-summary) (notevery-visited-p)))
            (when (accept-p) (apply)))
           (t
            (apply))))))))

(provide 'lsp-proxy-utils)
;;; lsp-proxy-utils.el ends here
