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
         ('host (string-remove-suffix
                 "!" (lsp-proxy--decompiled-unescape
                      (substring (car split)
                                 (length lsp-proxy--decompiled-prefix)))))
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

;;; File templates
;;
;; "File and Code Templates": fill a newly created empty file from a template
;; whose variables (`${PACKAGE_NAME}', `${NAME}', the Velocity `#if' directives)
;; are expanded by the language server — the only side that knows the project
;; model, e.g. which source root the file sits under.
;;
;; Same server contract as `decompile': `workspace/executeCommand' with command
;; `interpolateFileTemplate' and arguments [FILE-URI, TEMPLATE], answering with
;; the interpolated text in which a single `|' marks the caret.
;;
;; Ported from community/vscode-extension-core/src/fileTemplates.ts, with three
;; deliberate differences:
;;
;;  * Trigger.  VSCode listens on `workspace.onDidCreateFiles', an explorer event
;;    Emacs has no equivalent of.  The analogue is visiting a file that does not
;;    exist yet, which is how a file gets created here.  Opening an existing empty
;;    file deliberately does not trigger, so browsing never prompts.
;;
;;  * `$' is escaped before the caret marker becomes `$0'.  VSCode hands the whole
;;    reply to `SnippetString', so a literal `$' in the result (a shell variable,
;;    a Kotlin string template) is silently reread as snippet syntax.  Only the
;;    caret marker should be snippet syntax.
;;
;;  * No reformatting pass.  The original runs `editor.action.formatDocument' and
;;    then repairs the caret, because that formatter leaves a stray blank line
;;    where the caret was.  That repair is specific to it; yasnippet already
;;    indents what it expands.

(defcustom lsp-proxy-java-file-templates
  '(
    ("java"
     ("Class"
      . "#if (${PACKAGE_NAME} && ${PACKAGE_NAME} != \"\")package ${PACKAGE_NAME};\n\n#end\npublic class ${NAME} {\n\t|\n}")
     ("Interface"
      . "#if (${PACKAGE_NAME} && ${PACKAGE_NAME} != \"\")package ${PACKAGE_NAME};\n\n#end\npublic interface ${NAME} {\n\t|\n}")
     ("Record"
      . "#if (${PACKAGE_NAME} && ${PACKAGE_NAME} != \"\")package ${PACKAGE_NAME};\n\n#end\npublic record ${NAME}(|) {\n}")
     ("Enum"
      . "#if (${PACKAGE_NAME} && ${PACKAGE_NAME} != \"\")package ${PACKAGE_NAME};\n\n#end\npublic enum ${NAME} {\n\t|\n}")
     ("Annotation"
      . "#if (${PACKAGE_NAME} && ${PACKAGE_NAME} != \"\")package ${PACKAGE_NAME};\n\n#end\npublic @interface ${NAME} {\n\t|\n}")
     ("Exception"
      . "#if (${PACKAGE_NAME} && ${PACKAGE_NAME} != \"\")package ${PACKAGE_NAME};\n\n#end\npublic class ${NAME} extends RuntimeException {\n    public ${NAME}(String message) {\n        super(message);\n    }\n}")
     )
    ("kotlin"
     ("Class"
      . "#if (${PACKAGE_NAME} && ${PACKAGE_NAME} != \"\")package ${PACKAGE_NAME}\n\n#end\nclass ${NAME} {\n\t|\n}")
     ("File"
      . "#if (${PACKAGE_NAME} && ${PACKAGE_NAME} != \"\")package ${PACKAGE_NAME}\n\n#end\n|")
     ("Interface"
      . "#if (${PACKAGE_NAME} && ${PACKAGE_NAME} != \"\")package ${PACKAGE_NAME}\n\n#end\ninterface ${NAME} {\n\t|\n}")
     ("Data Class"
      . "#if (${PACKAGE_NAME} && ${PACKAGE_NAME} != \"\")package ${PACKAGE_NAME}\n\n#end\ndata class ${NAME}(|)\n")
     ("Enum"
      . "#if (${PACKAGE_NAME} && ${PACKAGE_NAME} != \"\")package ${PACKAGE_NAME}\n\n#end\nenum class ${NAME} {\n\t|\n}")
     ("Annotation"
      . "#if (${PACKAGE_NAME} && ${PACKAGE_NAME} != \"\")package ${PACKAGE_NAME}\n\n#end\nannotation class ${NAME}(|)")
     ("Object"
      . "#if (${PACKAGE_NAME} && ${PACKAGE_NAME} != \"\")package ${PACKAGE_NAME}\n\n#end\nobject ${NAME} {\n\t|\n}")
     )
    )
  "File templates for the JVM languages served by the IntelliJ backend.

Keyed by language, then by template name.  Values are templates in the server's
own syntax; their variables are interpolated by the server, not here.  A single
`|' marks where the caret should end up.

The defaults are the full set the backend's own VSCode extension ships (its
`jetbrains.templates.*' settings)."
  :type '(alist :key-type (string :tag "Language")
          :value-type (alist :key-type (string :tag "Name")
                       :value-type (string :tag "Template")))
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-file-templates-on-create t
  "Whether to offer a template when a new JVM source file is created.

Non-nil mirrors the VSCode extension, which applies one as soon as an empty file
appears; with a single template configured it applies without asking.
Set to nil to insert templates only on demand, via
\\[lsp-proxy-java-insert-file-template]."
  :type 'boolean
  :group 'lsp-proxy)

(defun lsp-proxy-java--templates ()
  "Return the templates for this buffer's language, or nil.
Reads `lsp-proxy--language', which `lsp-proxy-mode' sets when it attaches."
  (when-let* ((language (and (boundp 'lsp-proxy--language) lsp-proxy--language)))
    (cdr (assoc language lsp-proxy-java-file-templates))))

(defun lsp-proxy-java--select-template (templates)
  "Return the template text to use from TEMPLATES, or nil if cancelled.
A single entry is used without prompting, matching the VSCode extension."
  (cond ((null templates) nil)
        ((null (cdr templates)) (cdar templates))
        ;; Raised from `find-file-hook', so `this-command' is the unrelated
        ;; find-file command; see `lsp-proxy--completing-read'.
        (t (when-let* ((name (lsp-proxy--completing-read
                              "Select a file template: "
                              (mapcar #'car templates))))
             (cdr (assoc name templates))))))

(defun lsp-proxy-java--template-to-snippet (content)
  "Turn interpolated CONTENT into a snippet string.

Everything the server produced is literal text, so `$' and `\\' are escaped
first; only then does the caret marker become the exit point.  The other order
would let a literal `$' be read as snippet syntax."
  (let ((escaped (replace-regexp-in-string "[\\$]" "\\\\\\&" content)))
    ;; Only the first marker counts — there is exactly one caret.
    (if (string-match "|" escaped)
        (concat (substring escaped 0 (match-beginning 0))
                "$0"
                (substring escaped (match-end 0)))
      escaped)))

(defun lsp-proxy-java--insert-template (content)
  "Insert interpolated CONTENT, honouring the caret marker.
Expands as a snippet when yasnippet is available, otherwise inserts the text and
leaves point where the marker was."
  (if-let* ((snippet-fn (and (fboundp 'eglot--snippet-expansion-fn)
                             (eglot--snippet-expansion-fn))))
      (funcall snippet-fn (lsp-proxy-java--template-to-snippet content))
    (let ((caret (string-search "|" content)))
      (insert (if caret
                  (concat (substring content 0 caret) (substring content (1+ caret)))
                content))
      (when caret (goto-char (+ (point-min) caret))))))

(defun lsp-proxy-java--request-template (template)
  "Interpolate TEMPLATE for this buffer via the server, then insert the result.

The request carries this buffer's own URI so the proxy routes it to this file's
language servers; the server reads the target from the arguments.  Sending it
also opens the document when it is not open yet, which
`lsp-proxy--async-request' does for every request."
  (let ((buffer (current-buffer))
        (uri (plist-get (lsp-proxy--TextDocumentIdentifier) :uri))
        (tick (buffer-chars-modified-tick)))
    (lsp-proxy--async-request
     'workspace/executeCommand
     (lsp-proxy--build-params
      (list :command "interpolateFileTemplate"
            :arguments (vector uri template)))
     :success-fn
     (lambda (content)
       (when (and (buffer-live-p buffer) (stringp content) (not (string-empty-p content)))
         (with-current-buffer buffer
           ;; The reply is asynchronous, so the buffer may have been typed into
           ;; meanwhile. Only a still-empty, untouched buffer may be written to.
           (if (and (zerop (buffer-size)) (eq tick (buffer-chars-modified-tick)))
               (lsp-proxy-java--insert-template content)
             (lsp-proxy--info "File template skipped: %s changed since the request"
                              (buffer-name))))))
     ;; A server without the command is not worth interrupting for; the original
     ;; likewise only logs.
     :error-fn (lambda (err)
                 (lsp-proxy--warn "Could not interpolate file template: %s"
                                  (or (plist-get err :message) err)))
     :timeout-fn #'ignore)))

;;;###autoload
(defun lsp-proxy-java-insert-file-template ()
  "Insert a file template into the current buffer.
Prompts when several templates exist for the buffer's language."
  (interactive)
  (unless (bound-and-true-p lsp-proxy-mode)
    (user-error "lsp-proxy is not active in this buffer"))
  (let ((templates (lsp-proxy-java--templates)))
    (unless templates
      (user-error "No file templates for this language; see `lsp-proxy-java-file-templates'"))
    (if-let* ((template (lsp-proxy-java--select-template templates)))
        (lsp-proxy-java--request-template template)
      (message "No template selected"))))

(defun lsp-proxy-java--maybe-insert-template ()
  "Offer a file template when visiting a newly created, still-empty file.

Runs from `find-file-hook'.  Requires the file not to exist on disk yet: that is
what makes this a file being created rather than an empty file being browsed."
  (when (and lsp-proxy-java-file-templates-on-create
             (bound-and-true-p lsp-proxy-mode)
             buffer-file-name
             (zerop (buffer-size))
             (not (file-exists-p buffer-file-name))
             ;; Virtual decompiled sources are served by us, never created.
             (not (lsp-proxy--decompiled-buffer-p)))
    (when-let* ((templates (lsp-proxy-java--templates))
                (template (lsp-proxy-java--select-template templates)))
      (lsp-proxy-java--request-template template))))

;;; Server installation
;;
;; The IntelliJ backend is a required dependency of this file, so it needs an
;; install path. The download itself lives in the proxy (`src/java_install.rs`):
;; the archive is ~370 MB, and Rust already has tokio (so the editor stays
;; responsive) plus an established `$/progress` pipeline that puts a percentage in
;; the mode line. Everything below is the thin client half.
;;
;; The proxy's *own* installer stays in Emacs Lisp (`lsp-proxy-install.el`) — that
;; one cannot run in the proxy, which does not exist yet when it runs.

(defcustom lsp-proxy-java-server-install-dir
  (expand-file-name "servers/intellij/" lsp-proxy-install-dir)
  "Directory holding managed IntelliJ language server installs.
Each build lands in its own `<version>/' subdirectory, so several can
coexist and a downgrade is just a path change."
  :type 'directory
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-server-version nil
  "IntelliJ server build to install, e.g. \"263.2689.0\", or nil for the latest.

Nil is preferable: the latest build is resolved from JetBrains' own
extension metadata, which supplies the download URL *and* its checksum.
A pinned version has to have its URL constructed instead, so it cannot be
checksum-verified and the archive name is only confirmed for some
platforms."
  :type '(choice (const :tag "Latest" nil) string)
  :group 'lsp-proxy)

(defun lsp-proxy-java-server-launcher ()
  "Return the newest managed IntelliJ server launcher, or nil if none.
Useful for pointing a `languages.toml' entry at the managed install."
  (let ((exe (if (eq system-type 'windows-nt) "intellij-server.exe" "intellij-server")))
    (car
     (sort
      (seq-filter
       #'file-executable-p
       ;; The proxy flattens each install to `<version>/bin/<exe>', so the depth is
       ;; fixed regardless of how the archive itself was packed.
       (mapcar (lambda (dir) (expand-file-name (concat "bin/" exe) dir))
               (when (file-directory-p lsp-proxy-java-server-install-dir)
                 (directory-files lsp-proxy-java-server-install-dir t "\\`[0-9]" t))))
      ;; Newest build first, comparing components numerically so that 263.10.0
      ;; sorts above 263.9.0 (string order gets that backwards).
      (lambda (a b)
        (lsp-proxy-java--version<
         (lsp-proxy-java--version-key b)
         (lsp-proxy-java--version-key a)))))))

(defun lsp-proxy-java-server-bin-directory ()
  "Return the `bin/' directory of the newest managed IntelliJ server, or nil.

Prepended to the proxy's PATH at startup so `languages.toml' can name the server
plainly:

  [language-server.intellij]
  command = \"intellij-server\"
  args = [\"--stdio\"]

That keeps absolute, machine-specific paths out of the config entirely."
  (when-let* ((launcher (lsp-proxy-java-server-launcher)))
    (file-name-directory launcher)))

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

;;;###autoload
(defun lsp-proxy-java-install-server (&optional force)
  "Download and install the IntelliJ language server used for Java and Kotlin.

Installs `lsp-proxy-java-server-version', or the latest build when that is nil.
With a prefix argument FORCE, reinstall even if the version is already present.

The work happens in the proxy; progress is reported in the echo area while it
runs.  On success the launcher path is reported — point your `languages.toml'
at it."
  (interactive "P")
  (lsp-proxy--ensure-connection)
  (let ((dir (file-name-as-directory
              (expand-file-name lsp-proxy-java-server-install-dir))))
    (message "[lsp-proxy] Installing IntelliJ server into %s ..." dir)
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
             :force (if force t :json-false))
       ;; Omit the key rather than encoding a null: this connection serializes with
       ;; `:null-object nil', so a `:null' keyword is not a valid JSON value at all.
       ;; The proxy's `version' field is `#[serde(default)]', so absent means latest.
       (when lsp-proxy-java-server-version
         (list :version lsp-proxy-java-server-version))))
     :success-fn
     (lambda (result)
       (let ((path (plist-get result :launcherPath))
             (version (plist-get result :version)))
         (if (eq (plist-get result :alreadyInstalled) t)
             (message "[lsp-proxy] IntelliJ server %s already installed: %s" version path)
           ;; The proxy inherits PATH at startup, so a freshly installed server only
           ;; becomes findable after a restart.
           (message "[lsp-proxy] Installed IntelliJ server %s. Run M-x lsp-proxy-restart to use it (%s)"
                    version path))))
     :error-fn
     (lambda (err)
       (lsp-proxy--error "IntelliJ server install failed: %s"
                         (or (plist-get err :message) err)))
     :timeout-fn #'ignore
     ;; The transfer is hundreds of megabytes; the default request timeout is far
     ;; too short for it.
     :timeout 3600)))

;;; Setup

(add-to-list 'file-name-handler-alist
             (cons lsp-proxy--decompiled-file-name-regexp
                   #'lsp-proxy-java--file-handler))
(add-hook 'find-file-hook #'lsp-proxy-java--setup-buffer)
(add-hook 'find-file-hook #'lsp-proxy-java--maybe-insert-template)

;; No `auto-mode-alist' entry on purpose: `set-auto-mode' strips the
;; `file-remote-p' prefix before matching, so a pattern anchored on the lspsrc
;; prefix can never fire. `lsp-proxy-java--setup-buffer' sets the mode instead.

(defun lsp-proxy-java-unload-function ()
  "Deregister the decompiled-source handler.  See `unload-feature'."
  (setq file-name-handler-alist
        (rassq-delete-all #'lsp-proxy-java--file-handler
                          (copy-sequence file-name-handler-alist)))
  (remove-hook 'find-file-hook #'lsp-proxy-java--setup-buffer)
  (remove-hook 'find-file-hook #'lsp-proxy-java--maybe-insert-template)
  nil)

(provide 'lsp-proxy-java)
;;; lsp-proxy-java.el ends here
