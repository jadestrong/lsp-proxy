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

(defcustom lsp-proxy-java-class-file-contents-server-name "jdtls"
  "Name of the language server that answers `java/classFileContents'.

Must match the `[language-server.<name>]' section in `languages.toml'.
A name is needed at all only because `emacs/forwardRequest' routes by
server name — that is the proxy's generic forwarding channel and it has no
other addressing.  Which backend serves a given URI is decided from the URI
itself (see `lsp-proxy-java--content-provider'), never from a name."
  :type 'string
  :group 'lsp-proxy)

;;; Content providers
;;
;; Two backends serve JVM sources through different protocols, so the fetch is one
;; entry point (`lsp-proxy-java--content') with the protocol chosen underneath it,
;; rather than two parallel paths the callers would have to choose between.
;;
;; The dispatch key is the *scheme of the URI the server itself minted*, not the name
;; of a server:
;;
;;   jar:/jrt:  an archive or runtime-image member. The server that produced it
;;              advertises a `decompile' command; the proxy already routes
;;              `workspace/executeCommand' to whichever server advertises the
;;              command, so nothing here has to know which one that is.
;;   jdt:       an opaque content identifier. Only a server implementing
;;              `java/classFileContents' can resolve one, and that request answers
;;              with a bare string rather than a structured result.
;;
;; A scheme is a better key than a name for the same reason it is a cheaper one: the
;; server that emitted the URI is by construction the server that can resolve it, and
;; the answer needs no configuration to stay correct when a server is renamed.

(defconst lsp-proxy-java--class-file-contents-scheme "jdt"
  "URI scheme whose contents are fetched with `java/classFileContents'.")

(defun lsp-proxy-java--uri-scheme (uri)
  "Return URI's scheme as a string, or nil when it has none."
  (when (and (stringp uri) (string-match "\\`\\([a-zA-Z][a-zA-Z0-9+.-]*\\):" uri))
    (match-string 1 uri)))

(defun lsp-proxy-java--fetch-via-decompile (uri doc-uri)
  "Fetch URI with the `decompile' command, routed through DOC-URI.

The command answers with `{code, language}'; `language' is authoritative for the
buffer's major mode, because the member is often `.class' and its extension says
nothing about what the source was decompiled into."
  (let* ((result (jsonrpc-request
                  lsp-proxy--connection 'workspace/executeCommand
                  (list :uri doc-uri
                        :params (list :command "decompile"
                                      :arguments (vector uri)))))
         (code (plist-get result :code)))
    (unless (stringp code)
      (error "decompile returned no content for %s" uri))
    (list :code code :language (plist-get result :language))))

(defun lsp-proxy-java--fetch-via-class-file-contents (uri doc-uri)
  "Fetch URI with `java/classFileContents', routed through DOC-URI.

Unlike `decompile', this answers with a *bare string* and names no language,
so the result is wrapped into the same `(:code :language)' shape the rest of
this file consumes. `java' is not a guess: `java/classFileContents' is a
Java-only request, and Kotlin stays on the other backend by decision, so
there is no second language this can return.

An empty answer is not an error. A class with no attached source jar — most
of the JDK internals — legitimately yields nothing, and reporting that as a
failure would turn an expected outcome into a scary message; the buffer
simply opens empty. A non-string answer *is* an error: it means the request
did not land on a server that implements it."
  (let ((result (jsonrpc-request
                 lsp-proxy--connection 'emacs/forwardRequest
                 (list :uri doc-uri
                       :params (list :uri doc-uri
                                     :serverName lsp-proxy-java-class-file-contents-server-name
                                     :method "java/classFileContents"
                                     :params (list :uri uri))))))
    (unless (stringp result)
      (error "java/classFileContents returned no content for %s" uri))
    (list :code result :language "java")))

(defun lsp-proxy-java--content-provider (uri)
  "Return the function that fetches URI's content.
Called with (URI DOC-URI) and returning a `(:code :language)' plist."
  (if (equal (lsp-proxy-java--uri-scheme uri)
             lsp-proxy-java--class-file-contents-scheme)
      #'lsp-proxy-java--fetch-via-class-file-contents
    #'lsp-proxy-java--fetch-via-decompile))

(defun lsp-proxy-java--content (uri)
  "Return the decompiled content plist for URI, fetching it if necessary.

The single entry point for decompiled content: callers never choose a backend,
and the protocol difference is resolved by `lsp-proxy-java--content-provider'
below this line."
  (or (gethash uri lsp-proxy-java--content-cache)
      (progn
        (unless lsp-proxy--connection
          (error "No lsp-proxy connection to decompile %s" uri))
        (let* ((doc-uri (or (lsp-proxy-java--routing-uri)
                            (error "No active lsp-proxy document to route decompile of %s" uri)))
               (content (funcall (lsp-proxy-java--content-provider uri) uri doc-uri)))
          (puthash uri content lsp-proxy-java--content-cache)
          content))))

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
;; Either Java backend is a dependency of this file, so both need an install path.
;; The download itself lives in the proxy (`src/java_install.rs`): the archives run
;; from ~50 MB (jdtls) to ~370 MB (IntelliJ), and Rust already has tokio (so the
;; editor stays responsive) plus an established progress pipeline that puts a
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

(defcustom lsp-proxy-java-jdtls-install-dir
  (expand-file-name "servers/jdtls/" lsp-proxy-install-dir)
  "Directory holding managed Eclipse JDT language server installs.

Laid out exactly like `lsp-proxy-java-server-install-dir': one
`<version>/' subdirectory per build.  The two are named asymmetrically
only because the IntelliJ one predates this and is a user-settable
option, so renaming it would silently discard an existing setting."
  :type 'directory
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-jdtls-version nil
  "jdtls milestone to install, e.g. \"1.61.0\", or nil for the latest.

Unlike the IntelliJ backend, pinning costs nothing here: Eclipse
publishes a checksum per milestone rather than only for the newest
build, so a pinned version is still verified against it.  Nil resolves
the newest milestone number from Homebrew's formula metadata; the
archive name, the checksum and the download itself always come from
Eclipse."
  :type '(choice (const :tag "Latest" nil) string)
  :group 'lsp-proxy)

(defconst lsp-proxy-java--backends
  '(("intellij"
     :install-dir lsp-proxy-java-server-install-dir
     :version     lsp-proxy-java-server-version
     :exe         "intellij-server"
     :exe-windows "intellij-server.exe"
     :tag         "IntelliJ"
     :label       "IntelliJ server"
     ;; The bundle is self-contained, including its own JBR, and understands Lombok
     ;; natively the way the IntelliJ IDE does.
     :wants-lombok nil
     :runtime-requires nil)
    ("jdtls"
     :install-dir lsp-proxy-java-jdtls-install-dir
     :version     lsp-proxy-java-jdtls-version
     :exe         "jdtls"
     ;; jdtls ships a Python script with a `.bat' shim beside it rather than a
     ;; native binary, so this is NOT the Unix name plus `.exe'.  Getting it
     ;; wrong does not fail loudly: the launcher is simply never found.
     :exe-windows "jdtls.bat"
     :tag         "jdtls"
     :label       "Eclipse JDT language server"
     ;; Its compiler is the one the Lombok agent patches.
     :wants-lombok t
     :runtime-requires (lsp-proxy-java--check-python lsp-proxy-java--check-jdk)))
  "Java language server backends the proxy can install, keyed by server name.

The keys are the names `languages.toml' uses, so the value chosen here is
also what ends up on PATH under that name.  Everything that differs
between the backends is reachable from this table; adding a third means
adding a row, not a parallel copy of the code below.

`:install-dir' and `:version' hold the *symbols* of the corresponding
options rather than their values, so customising either is picked up
without reloading this file.  `:runtime-requires' is a list of functions,
each returning nil when satisfied or a description of what is missing;
they cover what the backend needs at run time but the install does not
provide.")

(defconst lsp-proxy-java--jdtls-min-java 21
  "Lowest Java major version `bin/jdtls' will start on.

Enforced by the launcher itself, not by us: `jdtls.py' raises
\"jdtls requires at least Java 21\" and exits before a single LSP message is
exchanged.  The symptom is therefore the server appearing to crash at
startup, with the real reason only in the proxy log — which is exactly why
this is checked up front instead.")

(defun lsp-proxy-java--java-executable ()
  "Return the `java' that `jdtls.py' would pick, or nil if there is none.

Resolved in the launcher's own order — JAVA_HOME first, then PATH — so the
answer is what jdtls will actually run, not merely what exists."
  (let ((home (getenv "JAVA_HOME")))
    (or (and home
             (let ((exe (expand-file-name
                         (if (eq system-type 'windows-nt) "bin/java.exe" "bin/java")
                         home)))
               (and (file-executable-p exe) exe)))
        (executable-find "java"))))

(defun lsp-proxy-java--java-major-version (exe)
  "Return the major version of the Java at EXE, or nil if it cannot be read.
Parses the same `version \"N' field `jdtls.py' matches, so a JDK this accepts
is one the launcher will accept too."
  (with-temp-buffer
    ;; `java -version' writes to stderr, which `call-process' folds into the
    ;; buffer along with stdout.
    (when (eq 0 (ignore-errors (call-process exe nil t nil "-version")))
      (goto-char (point-min))
      (when (re-search-forward "version \"\\([0-9]+\\)" nil t)
        (string-to-number (match-string 1))))))

(defun lsp-proxy-java--check-python ()
  "Return what is missing for `bin/jdtls' to run, or nil when nothing is.
The launcher is a `#!/usr/bin/env python3' script; on Windows the `.bat'
shim calls `python' instead."
  (let ((exe (if (eq system-type 'windows-nt) "python" "python3")))
    (unless (executable-find exe)
      (format "%s (bin/jdtls is a Python script)" exe))))

(defun lsp-proxy-java--check-jdk ()
  "Return what is wrong with the JDK jdtls would use, or nil when nothing is."
  (let* ((min lsp-proxy-java--jdtls-min-java)
         (exe (lsp-proxy-java--java-executable))
         (major (and exe (lsp-proxy-java--java-major-version exe))))
    (cond
     ((null exe) (format "a JDK %d+ on PATH or at JAVA_HOME" min))
     ;; An unreadable version is not proof of a bad JDK, so say what happened
     ;; rather than asserting a version that was never established.
     ((null major) (format "a JDK whose version can be read (`%s -version' gave none)" exe))
     ((< major min)
      (format "a JDK %d+, but %s is Java %d" min exe major)))))

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
with the JDK sent to jdtls during initialization.  URI, when non-nil, selects
the document context explicitly instead of relying on the current buffer."
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

(defun lsp-proxy-java--jdtls-execute (command arguments)
  "Ask jdtls to execute COMMAND with ARGUMENTS."
  (lsp-proxy--request
   'workspace/executeCommand
   (lsp-proxy--build-params
    (list :command command :arguments (vconcat arguments)))))

(defun lsp-proxy-java--jdtls-classpaths (file scope)
  "Return FILE classpaths for SCOPE from jdtls."
  (let* ((response
          (lsp-proxy-java--jdtls-execute
           "java.project.getClasspaths"
           (list (lsp-proxy--path-to-uri file)
                 (json-serialize (list :scope scope)))))
         (classpaths (plist-get response :classpaths))
         (root (lsp-proxy-java--nearest-build-root file))
         (paths (mapcar (lambda (path)
                          (if (file-name-absolute-p path)
                              path
                            (expand-file-name path root)))
                        (seq-filter #'stringp (append classpaths nil)))))
    (unless paths
      (user-error "jdtls returned no %s classpath for %s"
                  scope (file-name-nondirectory file)))
    paths))

(defun lsp-proxy-java--jdtls-test-file-p (file)
  "Return non-nil when jdtls identifies FILE as a test."
  (let ((answer
         (lsp-proxy-java--jdtls-execute
          "java.project.isTestFile"
          (list (lsp-proxy--path-to-uri file)))))
    (not (memq answer '(nil :json-false json-false)))))

(defun lsp-proxy-java--document-symbols ()
  "Return document symbols for the current buffer."
  (or (lsp-proxy--request
       'textDocument/documentSymbol
       (lsp-proxy--build-params
        (list :textDocument (lsp-proxy--TextDocumentIdentifier))))
      (user-error "No document symbols returned")))

(defun lsp-proxy-java--imenu-name (entry)
  "Return lsp-proxy imenu ENTRY's name."
  (and (vectorp entry) (> (length entry) 0) (aref entry 0)))

(defun lsp-proxy-java--imenu-content (entry)
  "Return lsp-proxy imenu ENTRY's content plist."
  (and (vectorp entry) (> (length entry) 1) (aref entry 1)))

(defun lsp-proxy-java--imenu-position-line (entry)
  "Return ENTRY's zero-based position line, or nil."
  (when-let* ((pos (plist-get (lsp-proxy-java--imenu-content entry) :Position)))
    (aref pos 0)))

(defun lsp-proxy-java--imenu-group (entry)
  "Return ENTRY's child group as a list, or nil."
  (when-let* ((group (plist-get (lsp-proxy-java--imenu-content entry) :Group)))
    (append group nil)))

(defun lsp-proxy-java--imenu-group-names (entries)
  "Return names that have group entries in ENTRIES."
  (delq nil
        (mapcar (lambda (entry)
                  (and (lsp-proxy-java--imenu-group entry)
                       (lsp-proxy-java--imenu-name entry)))
                entries)))

(defun lsp-proxy-java--imenu-group-start-line (entries index)
  "Return group start line for ENTRIES at INDEX.
lsp-proxy's symbol response contains separate same-name position and group
entries; use the position entry to recover the declaration line."
  (let* ((entry (nth index entries))
         (name (lsp-proxy-java--imenu-name entry))
         (previous (and (> index 0) (nth (1- index) entries)))
         (next (nth (1+ index) entries)))
    (or (and previous
             (equal name (lsp-proxy-java--imenu-name previous))
             (lsp-proxy-java--imenu-position-line previous))
        (and next
             (equal name (lsp-proxy-java--imenu-name next))
             (lsp-proxy-java--imenu-position-line next))
        (seq-some #'lsp-proxy-java--imenu-position-line
                  (lsp-proxy-java--imenu-group entry)))))

(defun lsp-proxy-java--imenu-next-group-line (entries index start-line)
  "Return the next sibling group line after INDEX and START-LINE."
  (cl-loop for sibling in (nthcdr (1+ index) entries)
           for sibling-index from (1+ index)
           for line = (and (lsp-proxy-java--imenu-group sibling)
                           (lsp-proxy-java--imenu-group-start-line entries sibling-index))
           when (and line (> line start-line))
           return line))

(defun lsp-proxy-java--type-path-at-line (entries line)
  "Return the nested Java type path in imenu ENTRIES containing LINE."
  (cl-loop for entry in entries
           for index from 0
           for children = (lsp-proxy-java--imenu-group entry)
           for start = (and children
                            (lsp-proxy-java--imenu-group-start-line entries index))
           for end = (and start
                          (lsp-proxy-java--imenu-next-group-line entries index start))
           when (and children start
                     (<= start line)
                     (or (null end) (< line end)))
           return (cons (lsp-proxy-java--imenu-name entry)
                        (lsp-proxy-java--type-path-at-line children line))))

(defun lsp-proxy-java--first-type-path (entries)
  "Return the first nested Java type path found in imenu ENTRIES."
  (seq-some
   (lambda (entry)
     (when-let* ((children (lsp-proxy-java--imenu-group entry)))
       (cons (lsp-proxy-java--imenu-name entry)
             (lsp-proxy-java--first-type-path children))))
   entries))

(defun lsp-proxy-java--method-at-line (entries line)
  "Return the nearest method-like imenu entry in ENTRIES at LINE."
  (let ((group-names (lsp-proxy-java--imenu-group-names entries))
        best-name
        best-line)
    (dolist (entry entries best-name)
      (let ((entry-line (lsp-proxy-java--imenu-position-line entry))
            (name (lsp-proxy-java--imenu-name entry)))
        (when (and entry-line
                   (<= entry-line line)
                   (not (member name group-names))
                   (or (null best-line) (>= entry-line best-line)))
          (setq best-line entry-line
                best-name (car (split-string name "(" t))))))))

(defun lsp-proxy-java--package-name ()
  "Return the current buffer's Java package name, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           "^[[:space:]]*package[[:space:]]+\\([[:alnum:]_.]+\\)[[:space:]]*;"
           nil t)
      (match-string-no-properties 1))))

(defun lsp-proxy-java--current-target ()
  "Return a plist with current Java :class and optional :method."
  (let* ((symbols (lsp-proxy-java--document-symbols))
         (line (1- (line-number-at-pos)))
         (type-path (or (lsp-proxy-java--type-path-at-line symbols line)
                        (lsp-proxy-java--first-type-path symbols))))
    (unless type-path
      (user-error "No Java type found in the current file"))
    (let* ((package (or (lsp-proxy-java--package-name) ""))
           (full-type-path type-path)
           (class-type-path (if (and package (equal (car type-path) package))
                                (cdr type-path)
                              type-path))
           (type-name (mapconcat #'identity class-type-path "$"))
           (class-name (if (string-empty-p package)
                           type-name
                         (concat package "." type-name))))
      (list :class class-name
            :method (lsp-proxy-java--method-at-line
                     (or (seq-reduce
                          (lambda (children name)
                            (seq-some
                             (lambda (entry)
                               (and (equal name (lsp-proxy-java--imenu-name entry))
                                    (lsp-proxy-java--imenu-group entry)))
                             children))
                          full-type-path symbols)
                         symbols)
                     line)))))

(defun lsp-proxy-java--java-program (&optional uri)
  "Return the Java executable used for project processes."
  (or (lsp-proxy-java--project-java-executable uri)
      (executable-find "java")
      (user-error "Could not find a Java executable")))

(defun lsp-proxy-java--main-command (file classpaths class)
  "Return a command that runs CLASS from FILE with CLASSPATHS."
  (append (list (lsp-proxy-java--java-program (lsp-proxy--path-to-uri file)))
          lsp-proxy-java-main-jvm-arguments
          (list "-cp" (mapconcat #'identity classpaths path-separator)
                class)
          lsp-proxy-java-main-arguments))

(defun lsp-proxy-java--junit-console-launch (classpaths class method)
  "Return JUnit Console arguments for CLASSPATHS, CLASS and METHOD."
  (let ((jar (expand-file-name lsp-proxy-java-junit-console-jar)))
    (unless (file-regular-p jar)
      (user-error
       "JUnit Console JAR is missing: %s; run M-x lsp-proxy-java-install-junit or configure `lsp-proxy-java-junit-console-jar'"
       jar))
    (list jar "execute"
          "--class-path" (mapconcat #'identity classpaths path-separator)
          (if method "--select-method" "--select-class")
          (if method (concat class "#" method) class))))

(defun lsp-proxy-java--junit-command (file classpaths class method)
  "Return a command that runs JUnit CLASS or METHOD from FILE."
  (append (list (lsp-proxy-java--java-program (lsp-proxy--path-to-uri file)))
          lsp-proxy-java-test-jvm-arguments
          (list "-jar")
          (lsp-proxy-java--junit-console-launch classpaths class method)))

(defun lsp-proxy-java--ensure-dape-launch ()
  "Ensure lsp-proxy's Java Dape launch helpers are available."
  (unless (and (or (featurep 'dape) (require 'dape nil t))
               (fboundp 'lsp-proxy-java--dape-populate-launch-config))
    (user-error "Dape support is not available; load `dape' before `lsp-proxy-java'")))

(defun lsp-proxy-java--dape-base-config (file root environment)
  "Return common Dape launch config for FILE in ROOT with ENVIRONMENT."
  (let ((env (lsp-proxy-java--environment-plist environment)))
    (append
     (list 'host "127.0.0.1"
           'lsp-proxy-uri (lsp-proxy--path-to-uri file)
           'fn #'lsp-proxy-java--dape-populate-launch-config
           :type lsp-proxy-java-dape-adapter-id
           :request "launch"
           :cwd root)
     (when env (list :env env)))))

;;;###autoload
(defun lsp-proxy-java-run-main (&optional debug)
  "Run the current Java class.
With prefix argument DEBUG, launch it through Dape when available."
  (interactive "P")
  (if debug
      (let* ((file (lsp-proxy-java--current-file))
             (root (lsp-proxy-java--nearest-build-root file))
             (target (lsp-proxy-java--current-target)))
        (lsp-proxy-java--ensure-dape-launch)
        (dape
         (append
          (lsp-proxy-java--dape-base-config
           file root lsp-proxy-java-main-environment)
          (list :mainClass (plist-get target :class)
                :args (combine-and-quote-strings
                       lsp-proxy-java-main-arguments)
                :vmArgs (combine-and-quote-strings
                         lsp-proxy-java-main-jvm-arguments)))))
    (let* ((file (lsp-proxy-java--current-file))
           (target (lsp-proxy-java--current-target))
           (class (plist-get target :class))
           (classpaths (lsp-proxy-java--jdtls-classpaths file "runtime"))
           (command (lsp-proxy-java--main-command file classpaths class)))
      (lsp-proxy-java--start-compilation
       command (lsp-proxy-java--nearest-build-root file)
       "main" lsp-proxy-java-main-environment
       (lsp-proxy--path-to-uri file)))))

;;;###autoload
(defun lsp-proxy-java-run-test (&optional debug)
  "Run the JUnit class or method at point.
With prefix argument DEBUG, launch it through Dape when available."
  (interactive "P")
  (let ((file (lsp-proxy-java--current-file)))
    (unless (lsp-proxy-java--jdtls-test-file-p file)
      (user-error "jdtls does not identify the current file as a test"))
    (let* ((root (lsp-proxy-java--nearest-build-root file))
           (target (lsp-proxy-java--current-target))
           (classpaths (lsp-proxy-java--jdtls-classpaths file "test"))
           (launch (lsp-proxy-java--junit-console-launch
                    classpaths
                    (plist-get target :class)
                    (plist-get target :method))))
      (if debug
          (progn
            (lsp-proxy-java--ensure-dape-launch)
            (dape
             (append
              (lsp-proxy-java--dape-base-config
               file root lsp-proxy-java-test-environment)
              (list :mainClass "org.junit.platform.console.ConsoleLauncher"
                    :classPaths (vector (car launch))
                    :args (combine-and-quote-strings (cdr launch))
                    :vmArgs (combine-and-quote-strings
                             lsp-proxy-java-test-jvm-arguments)))))
        (let ((command
               (lsp-proxy-java--junit-command
                file classpaths
                (plist-get target :class)
                (plist-get target :method))))
         (lsp-proxy-java--start-compilation
          command root "test" lsp-proxy-java-test-environment
          (lsp-proxy--path-to-uri file)))))))



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

;;; Lombok java agent
;;
;; jdtls cannot see Lombok-generated members on its own: it ships no Lombok and has
;; no setting for one, so every `@Data' getter is reported as an undefined method.
;; The fix is a java agent on the server's own command line, which is what the VS
;; Code Java extension does too -- `java.jdt.ls.lombokSupport.enabled' is consumed by
;; that extension, never sent to the server, so a client that only forwards the
;; setting (as `lsp-java' does) changes nothing.
;;
;; The download lives in the proxy for the same reason the servers' do; this half
;; only decides where the jar goes and tells the proxy where it ended up.

(defcustom lsp-proxy-java-lombok-install-dir
  (expand-file-name "lombok/" lsp-proxy-install-dir)
  "Directory holding the managed Lombok java agent jar.

A sibling of the managed servers rather than a subdirectory of one: the
agent is not part of any server, and a jdtls installed by some other
means (Homebrew, a distribution package) uses the same jar."
  :type 'directory
  :group 'lsp-proxy)

(defun lsp-proxy-java-lombok-jar ()
  "Return the newest managed Lombok jar, or nil when none is installed.

Passed to the proxy at startup, which appends it to the command line of any
server whose `languages.toml' entry says how it takes a java agent.  Normally
there is exactly one jar here; several are sorted newest-first so a downgrade
is just deleting a file."
  (car
   (sort
    (when (file-directory-p lsp-proxy-java-lombok-install-dir)
      (directory-files lsp-proxy-java-lombok-install-dir t "\\`lombok-[0-9].*\\.jar\\'" t))
    (lambda (a b)
      (lsp-proxy-java--version<
       (lsp-proxy-java--jar-version-key b)
       (lsp-proxy-java--jar-version-key a))))))

(defun lsp-proxy-java--jar-version-key (jar)
  "Return a comparable list of integers for JAR's `lombok-<version>.jar' name."
  (let ((name (file-name-nondirectory jar)))
    (if (string-match "\\`lombok-\\([0-9.]+\\)" name)
        (mapcar #'string-to-number (split-string (match-string 1 name) "\\." t))
      '(0))))

(defun lsp-proxy-java--install-lombok (force on-done)
  "Install the Lombok agent, then call ON-DONE with a report string or nil.

FORCE re-downloads a jar that is already present.  ON-DONE is called with nil
when the install failed, having already reported why: a failure here must not
be mistaken for the caller's own failure, which is why nothing is signalled."
  (let ((dir (file-name-as-directory
              (expand-file-name lsp-proxy-java-lombok-install-dir))))
    (jsonrpc-async-request
     lsp-proxy--connection 'emacs/installLombok
     (list :params (list :installDir dir :force (if force t :json-false)))
     :success-fn
     (lambda (result)
       (let ((version (plist-get result :version))
             (path (plist-get result :jarPath)))
         (funcall on-done
                  (if (eq (plist-get result :alreadyInstalled) t)
                      (format "Lombok %s already installed" version)
                    (format "Installed Lombok %s (java agent): %s" version path)))))
     :error-fn
     (lambda (err)
       (lsp-proxy--error "Lombok install failed: %s" (or (plist-get err :message) err))
       (funcall on-done nil))
     :timeout-fn
     (lambda ()
       (lsp-proxy--error "Lombok install timed out")
       (funcall on-done nil))
     ;; Two megabytes, so nothing like the servers' hour -- but still far past the
     ;; default request timeout on a slow link.
     :timeout 300)))

;;;###autoload
(defun lsp-proxy-java-install-lombok (&optional force)
  "Download the Lombok java agent jdtls needs to see Lombok-generated members.

With a prefix argument FORCE, re-download even if the jar is already present.
`lsp-proxy-java-install-server' installs this too when it installs jdtls; this
command is for a jdtls that came from somewhere else.

The proxy resolves the jar's path at startup, so a fresh install needs
`lsp-proxy-restart' before jdtls is launched with it."
  (interactive "P")
  (lsp-proxy--ensure-connection)
  (lsp-proxy--set-global-status "Lombok" "Installing Lombok")
  (message "[lsp-proxy] Installing Lombok ...")
  (lsp-proxy-java--install-lombok
   force
   (lambda (report)
     (lsp-proxy--clear-global-status)
     (when report
       (message "[lsp-proxy] %s. Run M-x lsp-proxy-restart to use it" report)))))

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
         (when (lsp-proxy-java--backend-get backend :wants-lombok)
           (lsp-proxy-java--install-lombok
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

;;; ModCommand action picker
;;
;; `intellij/chooseAction' arrives when a code action needs the user to pick between
;; several variants (the IntelliJ ModCommand flow). The proxy asks us, we answer with
;; the chosen entry's `index', and it delivers that to the server as
;; `workspace/executeCommand chooseModCommandAction'.
;;
;; Dismissing returns nil, which the proxy leaves unanswered: the server protocol has
;; no cancellation message here, so any index we invented would run an action the user
;; did not pick.

(defun lsp-proxy-java--handle-choose-action (msg)
  "Let the user pick one of MSG's entries; return its `index' or nil."
  (lsp-proxy--dbind (:title title :entries entries) msg
    (let* ((entries (append entries nil))
           (choices (mapcar (lambda (entry)
                              (cons (plist-get entry :name) (plist-get entry :index)))
                            entries)))
      (when choices
        (condition-case nil
            (let ((name (lsp-proxy--completing-read
                         (if (and title (not (string-empty-p title)))
                             (format "%s " title)
                           "Choose action: ")
                         (mapcar #'car choices))))
              ;; `alist-get' with a string key needs `equal'; the index may legitimately
              ;; be 0, so a plain `or' fallback would be wrong.
              (alist-get name choices nil nil #'equal))
          (quit nil))))))

;;; Editor commands driven by the server
;;
;; `intellij/runEditorCommand' arrives when a ModCommand needs the *editor* to do
;; something rather than the document: the motivating case is a refactoring whose
;; last step is an inline rename, which LSP cannot express at all — a server may not
;; send `workspace/executeCommand' to a client, and it cannot start a rename session
;; itself. (Live templates need none of this; they travel as a `SnippetTextEdit',
;; which is ordinary LSP.)
;;
;; The ids are VS Code's, so this is a translation table rather than a dispatcher:
;; only commands with a sensible Emacs equivalent are run, and anything unknown is
;; logged and dropped. Dropping is the right failure here — the document edits of
;; the ModCommand have already been applied by the time this arrives, so refusing
;; the follow-up leaves the buffer correct, just not with a rename prompt open.

(defcustom lsp-proxy-java-editor-command-alist
  '(("editor.action.rename" . lsp-proxy-rename))
  "How `intellij/runEditorCommand' ids map onto Emacs commands.
Each entry is (VSCODE-COMMAND-ID . EMACS-COMMAND).  EMACS-COMMAND is
called interactively, so a command that prompts (like
`lsp-proxy-rename') prompts as usual.  Ids with no entry are ignored."
  :type '(alist :key-type (string :tag "VS Code command id")
                :value-type (function :tag "Emacs command"))
  :group 'lsp-proxy)

(defun lsp-proxy-java--handle-run-editor-command (msg)
  "Run the editor command MSG asks for, if it maps onto one we have.
MSG is the `intellij/runEditorCommand' payload: `{command, arguments?}'.
The arguments are not passed on — every id mapped so far acts on point,
which the server cannot move and does not try to."
  (lsp-proxy--dbind (:command command) msg
    (let ((mapped (alist-get command lsp-proxy-java-editor-command-alist nil nil #'equal)))
      (cond
       ((null mapped)
        (lsp-proxy--message "Ignoring unmapped editor command `%s' (see `lsp-proxy-java-editor-command-alist')"
                            command))
       ((not (commandp mapped))
        (lsp-proxy--warn "`%s' maps to `%s', which is not a command" command mapped))
       (t
        ;; Deferred so the notification handler returns before a prompting
        ;; command takes over the minibuffer; reading input from inside the
        ;; filter would block the connection until the user answered.
        (run-at-time 0 nil (lambda () (call-interactively mapped))))))))

;;; Import log
;;
;; `intellij/importLog' reports the build tool (Maven/Gradle/Bazel) resolving the
;; project. It gets its own buffer rather than going to `*lsp-proxy-log*' because it
;; is a discrete operation whose outcome the user has to act on: until the import
;; succeeds nothing resolves, and the reason it failed is only ever in these lines.

(defcustom lsp-proxy-java-import-log-max-lines 2000
  "Maximum number of lines to keep in the import log buffer.
Nil disables trimming."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-import-log-auto-display 'on-failure
  "When to show the import log buffer without being asked.

VS Code reveals its Build panel both when an import starts and when it fails,
which is unobtrusive there because the panel is a docked tab. In Emacs the
equivalent steals a window, so starting an import — which happens on every
project open — is a poor trigger. A failure is worth interrupting for, since
otherwise nothing tells the user why no symbol resolves."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "When an import fails" on-failure)
                 (const :tag "When an import starts or fails" on-start))
  :group 'lsp-proxy)

(defconst lsp-proxy-java-import-log-buffer-name "*lsp-proxy-java-import*"
  "Name of the buffer holding build-tool import output.")

(defvar lsp-proxy-java--import-log-root nil
  "Workspace root of the entry written to the import log most recently.
Used to insert a heading when output starts coming from a different module.")

(defvar-keymap lsp-proxy-java-import-log-mode-map
  :doc "Keymap for `lsp-proxy-java-import-log-mode'."
  ;; Deliberately not on `g': in `special-mode' that key means revert, and erasing
  ;; the only record of why an import failed is a poor thing to do on a reflex.
  "C-c C-k" #'lsp-proxy-java-import-log-clear)

(define-derived-mode lsp-proxy-java-import-log-mode special-mode "Import Log"
  "Major mode for the build-tool import log.

\\{lsp-proxy-java-import-log-mode-map}"
  (buffer-disable-undo)
  (setq-local window-point-insertion-type t))

(defun lsp-proxy-java--import-log-buffer ()
  "Return the import log buffer, creating it if necessary."
  (or (get-buffer lsp-proxy-java-import-log-buffer-name)
      (with-current-buffer (get-buffer-create lsp-proxy-java-import-log-buffer-name)
        (lsp-proxy-java-import-log-mode)
        (current-buffer))))

(defun lsp-proxy-java--import-log-insert (text root)
  "Append TEXT to the import log, attributing it to workspace ROOT."
  (with-current-buffer (lsp-proxy-java--import-log-buffer)
    (let ((inhibit-read-only t)
          ;; Only follow the output in windows whose point is already at the end,
          ;; so a user reading back through a long import is not yanked away.
          (at-end (mapcar (lambda (win) (cons win (= (window-point win) (point-max))))
                          (get-buffer-window-list nil nil t))))
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        ;; A heading whenever the module changes: in a monorepo several imports run
        ;; and their lines would otherwise be indistinguishable.
        (when (and root (not (equal root lsp-proxy-java--import-log-root)))
          (insert (propertize (format "%s--- %s ---\n"
                                      (if (bobp) "" "\n")
                                      (abbreviate-file-name root))
                              'face 'font-lock-comment-face)))
        (insert text "\n")
        (setq lsp-proxy-java--import-log-root (or root lsp-proxy-java--import-log-root))
        (when (integerp lsp-proxy-java-import-log-max-lines)
          (let ((excess (- (line-number-at-pos (point-max))
                           lsp-proxy-java-import-log-max-lines)))
            (when (> excess 0)
              (goto-char (point-min))
              (forward-line excess)
              (delete-region (point-min) (point))))))
      (dolist (entry at-end)
        (when (cdr entry)
          (set-window-point (car entry) (point-max)))))))

(defun lsp-proxy-java--handle-import-log (msg)
  "Render an `intellij/importLog' notification MSG."
  (lsp-proxy--dbind (:type type :message message :tool tool
                     :failed failed :succeeded succeeded :started started
                     :rootPath root)
      msg
    (let ((failed (and failed (not (eq failed :json-false))))
          (succeeded (and succeeded (not (eq succeeded :json-false))))
          (started (and started (not (eq started :json-false)))))
      (cond
       (started
        ;; Carries no message worth showing; it only marks the beginning.
        (lsp-proxy-java--import-log-insert
         (propertize (format "%s import started" (or tool "Build"))
                     'face 'font-lock-comment-face)
         root)
        (when (eq lsp-proxy-java-import-log-auto-display 'on-start)
          (lsp-proxy-java-open-import-log)))
       (t
        (lsp-proxy-java--import-log-insert
         (if type (lsp-proxy--propertize message type) message)
         root)
        (cond
         (failed
          (when (memq lsp-proxy-java-import-log-auto-display '(on-start on-failure))
            (lsp-proxy-java-open-import-log))
          ;; The entry point VS Code puts in its status bar; here the echo area is
          ;; the only place guaranteed to be visible from any buffer.
          (lsp-proxy--warn "%s import failed. See `%s' (M-x lsp-proxy-java-open-import-log)."
                           (or tool "Build")
                           lsp-proxy-java-import-log-buffer-name))
         (succeeded
          (lsp-proxy--info "%s import finished." (or tool "Build")))))))))

;;;###autoload
(defun lsp-proxy-java-open-import-log ()
  "Display the build-tool import log."
  (interactive)
  (let ((buffer (lsp-proxy-java--import-log-buffer)))
    (with-current-buffer buffer
      (when (= (buffer-size) 0)
        (let ((inhibit-read-only t))
          (insert (propertize
                   "No import output yet.\n\nThe IntelliJ language server writes here while Maven, Gradle or Bazel\nresolves the project.\n"
                   'face 'font-lock-comment-face)))))
    (display-buffer buffer)))

(defun lsp-proxy-java-import-log-clear ()
  "Erase the import log."
  (interactive)
  (with-current-buffer (lsp-proxy-java--import-log-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (setq lsp-proxy-java--import-log-root nil)))

;;; Project build and configuration
;;
;; Two operations a Java backend exposes that LSP has no standard method for:
;; rebuilding the workspace, and re-reading the build file after it changed.  Both
;; travel over `emacs/forwardRequest', the proxy's generic "deliver this method to
;; that server" channel, so neither needs a request type of its own in the proxy.
;;
;; They are not symmetric on the wire, and the asymmetry is the server's, not ours:
;; `java/buildWorkspace' is a request answering with a build result, while
;; `java/projectConfigurationUpdate' is declared as a *notification*.  Sending the
;; latter as a request would sit out the full request timeout waiting for a reply the
;; server never sends, which is why the forwarding channel takes a `notify' flag.

(defconst lsp-proxy-java--build-result-messages
  '((0 . "Build cancelled.")
    (1 . "Build finished successfully.")
    (2 . "Build failed. See the diagnostics for details.")
    (3 . "Build finished with errors. See the diagnostics for details."))
  "Server `CompileWorkspaceStatus' codes and what to tell the user.
0 = cancelled, 1 = succeeded, 2 = failed, 3 = succeeded with errors.")

(defun lsp-proxy-java--forward (method params &optional notify server-name)
  "Send METHOD with PARAMS to the Java backend, returning its result.

Routed through a real project document, because the proxy dispatches by opened
document and a decompiled buffer is not one — the same reason
`lsp-proxy-java--routing-uri' exists for content fetches.

With NOTIFY non-nil the method is sent as a notification and nil is
returned; use it for methods the server declares as notifications.
SERVER-NAME defaults to `lsp-proxy-java-class-file-contents-server-name'."
  (unless lsp-proxy--connection
    (user-error "lsp-proxy is not running"))
  (let ((doc-uri (or (lsp-proxy-java--routing-uri)
                     (user-error "No active Java buffer to send %s from" method))))
    (jsonrpc-request
     lsp-proxy--connection 'emacs/forwardRequest
     (list :uri doc-uri
           :params (list :uri doc-uri
                         :serverName (or server-name
                                         lsp-proxy-java-class-file-contents-server-name)
                         :method method
                         :params params
                         :notify (if notify t :json-false))))))

;;;###autoload
(defun lsp-proxy-java-build-workspace (&optional full)
  "Rebuild the Java workspace.

With a prefix argument FULL, request a full rebuild instead of an
incremental one. A full rebuild of a large project takes minutes; the
incremental default is what you want after an ordinary edit."
  (interactive "P")
  (lsp-proxy--info "%s" (if full "Full rebuild requested..." "Rebuild requested..."))
  (let* ((result (lsp-proxy-java--forward "java/buildWorkspace" (if full t :json-false)))
         (text (alist-get result lsp-proxy-java--build-result-messages)))
    (cond
     ;; 2 and 3 both mean the user has something to look at; anything unrecognised is
     ;; reported verbatim rather than swallowed, so a status code we have not seen
     ;; before is visible instead of silently reading as success.
     ((null text) (lsp-proxy--warn "Build returned an unrecognised status: %S" result))
     ((memq result '(2 3)) (lsp-proxy--warn "%s" text))
     (t (lsp-proxy--info "%s" text)))
    result))

;;;###autoload
(defun lsp-proxy-java-update-project-configuration ()
  "Re-read this project's build file, picking up dependency changes.

Run it from the build file itself (`pom.xml', `build.gradle', ...) after editing
it: the server re-imports the project the file describes, and it identifies that
project by the document it is given."
  (interactive)
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (unless (string-match-p "\\`\\(?:pom\\.xml\\|build\\.gradle\\(?:\\.kts\\)?\\|settings\\.gradle\\(?:\\.kts\\)?\\|\\.classpath\\|\\.project\\)\'"
                          (file-name-nondirectory buffer-file-name))
    (user-error "Run this from a build file (pom.xml or a Gradle build script), not %s"
                (file-name-nondirectory buffer-file-name)))
  ;; A notification: the server declares it as one, so there is no reply to wait for
  ;; and no result to report. Progress shows up on the import channel instead.
  (lsp-proxy-java--forward "java/projectConfigurationUpdate"
                           (list :uri (lsp-proxy--path-to-uri buffer-file-name))
                           t)
  (lsp-proxy--info "Project configuration update requested."))

;;; Client-side code actions
;;
;; Some code actions are only half done by the server: it answers the code-action
;; request with a *command* that the client is expected to run, prompting the user
;; and sending a second request with what they picked.  Organising imports with
;; ambiguous names, generating accessors and overriding methods all work this way.
;;
;; The client has to opt in per action (see the `extendedClientCapabilities' block in
;; `languages.toml'), and opting in without implementing the handler is worse than
;; not opting in: the server then sends a command nothing can execute.  The table
;; below is therefore the authoritative list, and the TOML must match it exactly.

(defun lsp-proxy-java--first-argument (command)
  "Return COMMAND's first argument, or nil.
The server passes the action's context as a single argument; the vector is
JSON-decoded, so it may arrive as a vector or a list."
  (let ((args (plist-get command :arguments)))
    (cond ((and (vectorp args) (> (length args) 0)) (aref args 0))
          ((consp args) (car args))
          (t nil))))

(defun lsp-proxy-java--select-many (prompt items key)
  "Prompt with PROMPT to pick any number of ITEMS, returning the chosen ones.

KEY maps an item to its completion string. Returns a vector because the
server expects a JSON array, and an empty pick returns an empty vector
rather than nil — `nil' would serialise as null and the server would reject
the request."
  (let* ((table (mapcar (lambda (it) (cons (funcall key it) it)) items))
         (chosen (completing-read-multiple prompt (mapcar #'car table) nil t)))
    (apply #'vector
           (delq nil (mapcar (lambda (name) (alist-get name table nil nil #'equal))
                             chosen)))))

(defun lsp-proxy-java--handle-organize-imports (command)
  "Resolve ambiguous imports for COMMAND and apply the resulting edit.

The server answers with a `WorkspaceEdit' directly here — there is nothing
to ask the user, because the ambiguous cases come back through a separate
`chooseImports' round trip the server initiates itself."
  (when-let* ((context (lsp-proxy-java--first-argument command))
              (edit (lsp-proxy-java--forward "java/organizeImports" context)))
    (lsp-proxy--apply-workspace-edit edit 'lsp-proxy-java--handle-organize-imports)))

(defun lsp-proxy-java--handle-generate-accessors (command)
  "Ask which fields to generate accessors for, then apply the edit for COMMAND."
  (when-let* ((context (lsp-proxy-java--first-argument command))
              (fields (append (lsp-proxy-java--forward
                               "java/resolveUnimplementedAccessors" context)
                              nil)))
    (if (null fields)
        (lsp-proxy--info "No fields without accessors here.")
      (let ((chosen (lsp-proxy-java--select-many
                     "Generate accessors for: " fields
                     (lambda (f) (format "%s" (plist-get f :fieldName))))))
        (if (zerop (length chosen))
            (lsp-proxy--info "Nothing selected.")
          (lsp-proxy--apply-workspace-edit
           (lsp-proxy-java--forward "java/generateAccessors"
                                    (list :accessors chosen :context context))
           'lsp-proxy-java--handle-generate-accessors))))))

(defun lsp-proxy-java--handle-override-methods (command)
  "Ask which inherited methods to override, then apply the edit for COMMAND."
  (when-let* ((context (lsp-proxy-java--first-argument command))
              (result (lsp-proxy-java--forward "java/listOverridableMethods" context))
              (methods (append (plist-get result :methods) nil)))
    (if (null methods)
        (lsp-proxy--info "Nothing left to override here.")
      (let ((chosen (lsp-proxy-java--select-many
                     "Override methods: " methods
                     (lambda (m)
                       (format "%s(%s) — %s"
                               (plist-get m :name)
                               (string-join (append (plist-get m :parameters) nil) ", ")
                               (plist-get m :declaringClass))))))
        (if (zerop (length chosen))
            (lsp-proxy--info "Nothing selected.")
          (lsp-proxy--apply-workspace-edit
           (lsp-proxy-java--forward "java/addOverridableMethods"
                                    (list :overridableMethods chosen :context context))
           'lsp-proxy-java--handle-override-methods))))))

(defconst lsp-proxy-java--client-command-handlers
  '(("java.action.organizeImports" . lsp-proxy-java--handle-organize-imports)
    ("java.action.generateAccessorsPrompt" . lsp-proxy-java--handle-generate-accessors)
    ("java.action.overrideMethodsPrompt" . lsp-proxy-java--handle-override-methods))
  "Server commands this client executes itself instead of sending back.

Each entry is (COMMAND-ID . HANDLER); HANDLER receives the whole `Command'
plist.

Must stay in one-to-one correspondence with the
`extendedClientCapabilities' switches declared in `languages.toml': a
switch with no handler makes the server send a command nothing can run,
which is strictly worse than not declaring it.")

(defun lsp-proxy-java--execute-client-command (command)
  "Run COMMAND here if it is one of ours; return non-nil when it was handled.

Registered on `lsp-proxy-client-command-functions', which the generic
code-action path consults before forwarding a command to the server.
Returning nil leaves every other command on exactly the path it took before."
  (when-let* ((id (plist-get command :command))
              (handler (alist-get id lsp-proxy-java--client-command-handlers
                                  nil nil #'equal)))
    (funcall handler command)
    t))

(add-hook 'lsp-proxy-client-command-functions #'lsp-proxy-java--execute-client-command)

;;; Missing backend
;;
;; When the configured Java server is not installed, the proxy reports it as
;; `emacs/serverStartFailure' and the generic handler prints the fact.  All this adds
;; is where to get the thing — knowledge that belongs with the language, not in the
;; proxy or in the generic notification path.

(defcustom lsp-proxy-java-jdtls-install-hint
  "Run M-x lsp-proxy-java-install-server and choose `jdtls' to install it, \
or install Eclipse JDT Language Server yourself \
(https://github.com/eclipse-jdtls/eclipse.jdt.ls) and put its `bin/jdtls' \
launcher on PATH — e.g. `brew install jdtls' on macOS."
  "What to tell the user when the `java/classFileContents' backend is missing.
Both routes work: a managed install goes under
`lsp-proxy-java-jdtls-install-dir' and is put on the proxy's PATH at startup,
while a self-installed one is found on PATH like any other server."
  :type 'string
  :group 'lsp-proxy)

(defun lsp-proxy-java--install-hint (server-name)
  "Return an install hint for SERVER-NAME, or nil when we have none."
  (cond
   ((equal server-name lsp-proxy-java-class-file-contents-server-name)
    lsp-proxy-java-jdtls-install-hint)
   ((assoc server-name lsp-proxy-java--backends)
    (format "Run M-x lsp-proxy-java-install-server and choose `%s' to install it."
            server-name))))

(add-hook 'lsp-proxy-server-install-hint-functions #'lsp-proxy-java--install-hint)

;;; Debugging (dape)
;;
;; Wires `dape' (if installed — soft dependency, see the `when' below) to
;; the IntelliJ backend's own DAP endpoint.
;;
;; The backend hosts IntelliJ's XDebugger behind a DAP server: a plain
;; `workspace/executeCommand' with command `start_debug_server' (no
;; arguments) returns a fresh TCP port; connecting to it and sending a DAP
;; `initialize' with `adapterID = "intellij_jvm"' gets you a real
;; debug session (breakpoints, stepping, variables) with no VS Code
;; pieces involved.
;;
;; `launch' does NOT rely on the DAP endpoint to resolve
;; `:cwd'/`:classPaths'/`:javaExec' on its own — empirically it doesn't (or
;; doesn't reliably): omitting them produced a JVM that started but
;; couldn't find the main class at all (wrong/empty classpath). The VS
;; Code extension resolves these explicitly with `workspace/executeCommand'
;; before launching, and that DOES work (verified: matching a real
;; `cwd'/JDK/`@argfile' classpath in the launched process). This mirrors
;; that exactly instead of trusting the adapter to self-resolve.
;;
;; That resolution is one `intellij.java.resolveLaunch' call, which answers
;; the whole launch — `javaExec', classpath, module path, module content
;; roots, working directory — with the config's own values sent as
;; `overrides' so the server decides how they combine. It replaced three
;; per-fragment commands (`resolveClasspath', `resolveWorkingDirectory',
;; `resolveJavaExecutable') that no longer exist, so there is nothing to
;; fall back to: a backend that does not know it is too old for this file.
;;
;; The same rename swept the rest of this integration — the Run/Debug
;; CodeLens command `intellij_debugger.runMain' → `intellij.jvm.runMain',
;; and the adapter id `intellij_debugger' → `intellij_jvm'. Launching
;; *through* a build tool (`intellij_gradle',
;; `intellij.java.resolveBuildToolLaunch', the `intellij: build'
;; pre-launch task) is a separate feature and is not wired up here: it
;; needs a whole second launch shape, and a JVM launch behaves as it
;; always has — it runs whatever is already compiled.
;;
;; `attach' is unaffected — a plain JDWP `:hostName'/`:port' still needs
;; no resolution.
;;
;; `dape' config plists mix two key conventions: bare symbols (`host',
;; `port', `command', `modes', `ensure', `fn') are dape's own bookkeeping
;; and never reach the wire; keyword symbols (`:type', `:request',
;; `:mainClass', ...) are sent verbatim as the DAP `launch'/`attach'
;; request body (`:type' additionally becomes the `initialize' request's
;; `adapterID'). The bare `port' is dape's *own* connection to the
;; server's DAP endpoint; a keyword `:port' (attach only) is the JDWP
;; target port sent to the adapter — same-looking, different keys,
;; deliberately both present in the attach config. The scratch key
;; `lsp-proxy-uri' (also bare) smuggles an already-known source URI into
;; `lsp-proxy-java--dape-populate-launch-config' without leaking into the
;; DAP body, for the same reason.

;; A bare top-level `require' (not nested in the `when' below) so the
;; byte-compiler actually loads `dape' when compiling on a machine that has
;; it, instead of merely deciding at runtime whether to call it — a
;; `require' buried inside a conditional's test is not given that
;; compile-time treatment, and every reference below would otherwise warn
;; "not known to be defined" even when `dape' is genuinely installed.
(require 'dape nil t)

;; Same reasoning applies to `defun': one nested inside the `when' below
;; isn't given top-level treatment either, so a later function in the same
;; block calling an earlier one still warns "not known to be defined".
;; These four declarations are only about silencing that; the real
;; definitions are the ones below, conditionally, same as everything else
;; in this section.
(declare-function lsp-proxy-java--dape-execute-command "lsp-proxy-java")
(declare-function lsp-proxy-java--dape-compact "lsp-proxy-java")
(declare-function lsp-proxy-java--dape-start-debug-server "lsp-proxy-java")
(declare-function lsp-proxy-java--dape-resolve-uri "lsp-proxy-java")
(declare-function lsp-proxy-java--dape-resolve-launch "lsp-proxy-java")
(declare-function lsp-proxy-java--dape-populate-launch-config "lsp-proxy-java")
(declare-function lsp-proxy-java--project-java-executable "lsp-proxy-java")

(defcustom lsp-proxy-java-dape-adapter-id "intellij_jvm"
  "The `:type'/DAP `adapterID' the backend's debug endpoint is addressed by.
Anything the backend does not know gets \"No debugger adapter found for
given adapter id\".

`intellij_jvm' is the current name.  `intellij_debugger' is what it was
called before, kept working by the backend as an alias, so it is the one
to set if the server predates the rename.  `intellij_gradle' — a launch
that runs *through* the build tool — needs a differently shaped config
than this file builds and is not usable by setting this alone.

The named `dape-configs' templates capture this when `dape' loads, so
set it before then; `lsp-proxy-java-dape-run-main' (the Run/Debug
CodeLens) reads it afresh on every launch."
  :type '(choice (const :tag "intellij_jvm (current)" "intellij_jvm")
                 (const :tag "intellij_debugger (pre-rename alias)" "intellij_debugger")
                 (string :tag "Other adapter id"))
  :group 'lsp-proxy)

(when (featurep 'dape)


  (defun lsp-proxy-java--dape-execute-command (command &optional argument)
    "Send `workspace/executeCommand' COMMAND, synchronously, and return its result.
ARGUMENT, if given, is wrapped as the command's single positional
argument, matching the shape every `intellij.java.*'/
`start_debug_server' custom command expects. Signals a `user-error' if
the request fails."
    (condition-case err
        (lsp-proxy--request
         'workspace/executeCommand
         (lsp-proxy--build-params
          (list :command command
               :arguments (if argument (vector argument) (vector))))
         :timeout 10)
      (jsonrpc-error
       (user-error "lsp-proxy-java: `%s' failed: %s" command (error-message-string err)))))

  (defun lsp-proxy-java--dape-start-debug-server ()
    "Ask the language server for a fresh DAP endpoint port."
    (let ((port (lsp-proxy-java--dape-execute-command "start_debug_server")))
      (unless (numberp port)
        (user-error "lsp-proxy-java: `start_debug_server' returned %S, expected a port number" port))
      port))

  (defun lsp-proxy-java--dape-resolve-uri (main-class)
    "Resolve the source file URI for MAIN-CLASS via the language server."
    (or (plist-get
         (lsp-proxy-java--dape-execute-command
          "intellij.java.resolveClassDocument" (list :fqn main-class))
         :uri)
        (user-error "lsp-proxy-java: could not resolve a source file for `%s'" main-class)))

  (defun lsp-proxy-java--dape-compact (plist)
    "Return PLIST without the keys whose value is nil.
Absent and nil are the same thing to the backend's optional fields, but
they are not the same on the wire: a nil plist value would be sent as
JSON null, which is a value rather than an omission."
    (let (out)
      (while plist
        (when (cadr plist)
          (setq out (append out (list (car plist) (cadr plist)))))
        (setq plist (cddr plist)))
      out))

  (defun lsp-proxy-java--dape-resolve-launch (config uri)
    "Fill CONFIG with the runtime paths for URI's module, and return it.

One `intellij.java.resolveLaunch' call answers the whole launch in a
single read action — `javaExec', classpath, module path, module content
roots and working directory — with CONFIG's own values passed along as
`overrides' so the *server* decides how they combine.  This is what the
VS Code extension's `resolveJvmLaunchConfig' (`dap.ts') does, and the
reason it is one call rather than several: every fragment a client has
to remember to copy is a fragment it can silently drop."
    (let* ((java-exec (or (plist-get config :javaExec)
                          (lsp-proxy-java--project-java-executable uri)))
           (paths (lsp-proxy-java--dape-execute-command
                   "intellij.java.resolveLaunch"
                   (lsp-proxy-java--dape-compact
                    (list :uri uri
                          :cwd (plist-get config :cwd)
                          :overrides
                          (lsp-proxy-java--dape-compact
                           (list :classPaths (plist-get config :classPaths)
                                 :modulePaths (plist-get config :modulePaths)
                                 :moduleName (plist-get config :moduleName)
                                 :javaExec java-exec)))))))
      ;; Already merged with the overrides sent above, so this is copied
      ;; rather than combined with whatever CONFIG had.
      (setq config (plist-put config :classPaths (or (plist-get paths :classpath) [])))
      ;; For a JPMS launch the answer carries the module path and the owning
      ;; module name, so the main class runs from the module path
      ;; (`-m moduleName/mainClass') instead of the classpath.
      (setq config (plist-put config :modulePaths (or (plist-get paths :modulePath) [])))
      (when-let* ((mn (plist-get paths :moduleName)))
        (setq config (plist-put config :moduleName mn)))
      ;; A build tool may compile one module into several directories and only
      ;; the one holding `module-info.class' is the module; the adapter patches
      ;; the rest back in with `--patch-module' once they exist on disk.
      (setq config (plist-put config :moduleContentPaths
                              (or (plist-get paths :moduleContentPaths) [])))
      ;; The module's project directory, unless CONFIG named one. Without
      ;; either, the launched process inherits lsp-proxy's own directory.
      (when-let* ((wd (plist-get paths :workingDirectory)))
        (setq config (plist-put config :cwd wd)))
      ;; Never absent: a launch with neither a configured nor a project JDK is
      ;; refused by the server, which is the only side that can tell the two
      ;; apart.
      (when-let* ((java (plist-get paths :javaExec)))
        (setq config (plist-put config :javaExec java)))
      config))

  (defun lsp-proxy-java--dape-populate-launch-config (config)
    "`fn' for the IntelliJ launch/attach configs.

Always fetches a fresh debug-server `port'.  For `:request \"launch\"'
also resolves `:cwd', `:classPaths'/`:modulePaths'/`:moduleName' and
`:javaExec' for the module owning `:mainClass', mirroring the VS Code
extension's launch resolution (`dap.ts') — see this section's Commentary
for why that resolution can't be skipped.  `:request \"attach\"' needs
none of this (a plain JDWP host/port), so only the port gets added.

Uses the bare-symbol scratch key `lsp-proxy-uri' for an
already-known source URI (from a CodeLens click) to skip the extra
`resolveClassDocument' round-trip; harmless to omit."
    (setq config (plist-put config 'port (lsp-proxy-java--dape-start-debug-server)))
    (if (not (equal (plist-get config :request) "launch"))
        config
      (let* ((main-class (or (plist-get config :mainClass)
                             (user-error "lsp-proxy-java: no `:mainClass' to resolve a launch config for")))
             (uri (or (plist-get config 'lsp-proxy-uri)
                      (lsp-proxy-java--dape-resolve-uri main-class))))
        (lsp-proxy-java--dape-resolve-launch config uri))))

  ;; Named templates for `M-x dape'. `:mainClass''s form is only evaluated by
  ;; `dape' when reading a config by name (`dape--config-eval'); by the time
  ;; `fn' (`lsp-proxy-java--dape-populate-launch-config') runs, it's already a
  ;; concrete string.
  (with-eval-after-load 'dape
    (add-to-list 'dape-configs
                `(intellij-launch
                  modes (java-mode java-ts-mode kotlin-mode)
                  host "127.0.0.1"
                  fn lsp-proxy-java--dape-populate-launch-config
                  :type ,lsp-proxy-java-dape-adapter-id
                  :request "launch"
                  :mainClass (read-string "Main class (fully qualified): ")))
    (add-to-list 'dape-configs
                `(intellij-attach
                  modes (java-mode java-ts-mode kotlin-mode)
                  host "127.0.0.1"
                  fn lsp-proxy-java--dape-populate-launch-config
                  :type ,lsp-proxy-java-dape-adapter-id
                  :request "attach"
                  :hostName "127.0.0.1"
                  :port (read-number "JDWP port: " 5005))))

  ;;;###autoload
  (defun lsp-proxy-java-dape-run-main (arguments)
    "Handle the IntelliJ Run/Debug lens command's ARGUMENTS.
ARGUMENTS is the command's raw `:arguments' vector as delivered by the
proxy — its first element is `{mainClass, uri?, noDebug?}', mirroring
the VS Code extension's `RunMainArgs'. Builds a minimal config
(`:mainClass' plus the already-known `uri', stashed under the bare
`lsp-proxy-uri' key) and lets `lsp-proxy-java--dape-populate-launch-config'
\(via `fn') resolve the rest, same as the named `dape-configs'
templates. Meant to be called from `lsp-proxy-codelens.el' when a
CodeLens command matches \"intellij.jvm.runMain\"."
    (let* ((args (and (vectorp arguments) (> (length arguments) 0) (aref arguments 0)))
           (main-class (and args (plist-get args :mainClass)))
           (uri (and args (plist-get args :uri)))
           (no-debug (and args (eq (plist-get args :noDebug) t))))
      (unless main-class
        (user-error "lsp-proxy-java: the Run/Debug lens had no mainClass in its arguments"))
      (dape
       (append
        (list 'host "127.0.0.1"
              :type lsp-proxy-java-dape-adapter-id
              :request "launch"
              :mainClass main-class
              'fn #'lsp-proxy-java--dape-populate-launch-config)
        (when uri (list 'lsp-proxy-uri uri))
        (when no-debug (list :noDebug t)))))))

;;; Navigate to location (hover command links)

;; The IntelliJ backend embeds navigation links directly in hover markdown —
;; e.g. "Go to Super Method" on a method's hover — as a Markdown command URI
;; `[label](command:jetbrains.navigateToLocation?ARGS)', ARGS being a
;; percent-encoded JSON array `[uri, line, character]' (0-indexed). The VS
;; Code extension only makes these clickable inside the hover popup; this
;; command instead extracts and jumps to them directly from point, no popup
;; or click required.

(defconst lsp-proxy-java--navigate-link-regexp
  "\\[\\([^]]+\\)\\](command:jetbrains\\.navigateToLocation\\?\\([^)]+\\))"
  "Matches a `jetbrains.navigateToLocation' Markdown command link.
Group 1 is the link label, group 2 the percent-encoded JSON
`[uri, line, character]' argument array.")

(defun lsp-proxy-java--navigate-links-in-string (markdown)
  "Return each `jetbrains.navigateToLocation' link in MARKDOWN.
Each element is (LABEL URI LINE CHARACTER)."
  (let (links (start 0))
    (while (string-match lsp-proxy-java--navigate-link-regexp markdown start)
      (let* ((label (match-string 1 markdown))
             (args (json-parse-string (url-unhex-string (match-string 2 markdown))
                                      :array-type 'list)))
        (push (cons label args) links))
      (setq start (match-end 0)))
    (nreverse links)))

;;;###autoload
(defun lsp-proxy-java-navigate-at-point ()
  "Jump to the `jetbrains.navigateToLocation' hover link at point.
Requests hover at point and, if its markdown embeds exactly one such
link (e.g. \"Go to Super Method\"), jumps straight to it; with more
than one, prompts to choose. Meant to be bound to a key so a method
name can be navigated without opening the hover popup and clicking."
  (interactive)
  (unless lsp-proxy--support-hover
    (lsp-proxy--error "%s" "Server doesn't support hover"))
  (lsp-proxy--async-request
   'textDocument/hover
   (lsp-proxy--build-params (lsp-proxy--TextDocumentPositionParams))
   :success-fn
   (lambda (hover-help)
     (let ((links (and hover-help (not (string-empty-p hover-help))
                       (lsp-proxy-java--navigate-links-in-string hover-help))))
       (pcase links
         ('nil (lsp-proxy--message "%s" "No navigable link at point"))
         (`(,only) (pcase-let ((`(,_ ,uri ,line ,character) only))
                     (lsp-proxy--goto-uri-position uri line character)))
         (_ (let* ((choice (completing-read "Go to: " (mapcar #'car links) nil t))
                   (link (assoc choice links)))
              (pcase-let ((`(,_ ,uri ,line ,character) link))
                (lsp-proxy--goto-uri-position uri line character)))))))))

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
