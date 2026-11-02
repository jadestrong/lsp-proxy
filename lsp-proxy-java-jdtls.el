;;; lsp-proxy-java-jdtls.el --- jdtls integration for lsp-proxy Java -*- lexical-binding: t; -*-

;; Copyright (C) 2026-2026 JadeStrong

;;; Commentary:

;; Eclipse JDT LS specific integration: jdt:// content, managed jdtls install
;; metadata, Lombok/Java Debug bundles, jdtls run/test/debug commands and
;; jdtls-specific workspace/code-action helpers.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-handlers)
(require 'lsp-proxy-java)
(require 'lsp-proxy-utils)
(require 'lsp-proxy-core)

(declare-function dape "dape" (config &optional skip-compile))
(declare-function jsonrpc-request "jsonrpc")
(declare-function jsonrpc-async-request "jsonrpc")

(defcustom lsp-proxy-java-class-file-contents-server-name "jdtls"
  "Name of the language server that answers `java/classFileContents'.

Must match the `[language-server.<name>]' section in `languages.toml'.
A name is needed at all only because `emacs/forwardRequest' routes by
server name.  Which backend serves a given URI is decided from the URI
itself, never from a name."
  :type 'string
  :group 'lsp-proxy)

(defconst lsp-proxy-java--class-file-contents-scheme "jdt"
  "URI scheme whose contents are fetched with `java/classFileContents'.")

(defun lsp-proxy-java--fetch-via-class-file-contents (uri doc-uri)
  "Fetch URI with `java/classFileContents', routed through DOC-URI."
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

(defun lsp-proxy-java-jdtls--content-uri-p (uri)
  "Return non-nil when URI should be resolved through jdtls."
  (equal (lsp-proxy-java--uri-scheme uri)
         lsp-proxy-java--class-file-contents-scheme))

(lsp-proxy-java-register-content-provider
 #'lsp-proxy-java-jdtls--content-uri-p
 #'lsp-proxy-java--fetch-via-class-file-contents)

(defcustom lsp-proxy-java-jdtls-install-dir
  (expand-file-name "servers/jdtls/" lsp-proxy-install-dir)
  "Directory holding managed Eclipse JDT language server installs."
  :type 'directory
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-jdtls-version nil
  "jdtls milestone to install, e.g. \"1.61.0\", or nil for the latest."
  :type '(choice (const :tag "Latest" nil) string)
  :group 'lsp-proxy)

(defconst lsp-proxy-java--jdtls-min-java 21
  "Lowest Java major version `bin/jdtls' will start on.")

(defun lsp-proxy-java--java-executable ()
  "Return the `java' that `jdtls.py' would pick, or nil if there is none."
  (let ((home (getenv "JAVA_HOME")))
    (or (and home
             (let ((exe (expand-file-name
                         (if (eq system-type 'windows-nt) "bin/java.exe" "bin/java")
                         home)))
               (and (file-executable-p exe) exe)))
        (executable-find "java"))))

(defun lsp-proxy-java--java-major-version (exe)
  "Return the major version of the Java at EXE, or nil if it cannot be read."
  (with-temp-buffer
    (when (eq 0 (ignore-errors (call-process exe nil t nil "-version")))
      (goto-char (point-min))
      (when (re-search-forward "version \"\\([0-9]+\\)" nil t)
        (string-to-number (match-string 1))))))

(defun lsp-proxy-java--check-python ()
  "Return what is missing for `bin/jdtls' to run, or nil when nothing is."
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
     ((null major) (format "a JDK whose version can be read (`%s -version' gave none)" exe))
     ((< major min)
      (format "a JDK %d+, but %s is Java %d" min exe major)))))

(lsp-proxy-java-register-backend
 "jdtls"
 :install-dir 'lsp-proxy-java-jdtls-install-dir
 :version     'lsp-proxy-java-jdtls-version
 :exe         "jdtls"
 :exe-windows "jdtls.bat"
 :tag         "jdtls"
 :label       "Eclipse JDT language server"
 :wants-lombok t
 :runtime-requires '(lsp-proxy-java--check-python lsp-proxy-java--check-jdk))

(defun lsp-proxy-java--jdtls-execute (command arguments &optional timeout)
  "Ask jdtls to execute COMMAND with ARGUMENTS.
When TIMEOUT is non-nil, pass it to `jsonrpc-request' in seconds."
  (let ((params (lsp-proxy--build-params
                 (list :command command :arguments (vconcat arguments)))))
    (if timeout
        (lsp-proxy--request 'workspace/executeCommand params :timeout timeout)
      (lsp-proxy--request 'workspace/executeCommand params))))

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
  (let ((symbols (lsp-proxy--request
                  'textDocument/documentSymbol
                  (lsp-proxy--build-params
                   (list :textDocument (lsp-proxy--TextDocumentIdentifier))))))
    (unless symbols
      (user-error "No document symbols returned"))
    (append symbols nil)))

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

(defun lsp-proxy-java--first-type-position (entries)
  "Return the first Java type declaration position in imenu ENTRIES."
  (cl-loop for entry in entries
           for index from 0
           for name = (lsp-proxy-java--imenu-name entry)
           for children = (lsp-proxy-java--imenu-group entry)
           when children
           return (or (when-let* ((line (lsp-proxy-java--imenu-group-start-line
                                         entries index)))
                        (list :line line
                              :character
                              (save-excursion
                                (goto-char (point-min))
                                (forward-line line)
                                (if (and name
                                         (re-search-forward
                                          (regexp-quote name)
                                          (line-end-position) t))
                                    (- (match-beginning 0)
                                       (line-beginning-position))
                                  0))))
                      (lsp-proxy-java--first-type-position children))))

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

(defun lsp-proxy-java--debug-launch-options (file root project-name environment)
  "Return common Java Debug launch options for FILE, ROOT and ENVIRONMENT."
  (let ((env (lsp-proxy-java--environment-plist environment)))
    (append
     (list 'fn #'lsp-proxy-java--debug-prepare
           'lsp-proxy-java-source file
           :type "java"
           :request "launch"
           :cwd root
           :javaExec (lsp-proxy-java--java-program (lsp-proxy--path-to-uri file))
           :console lsp-proxy-java-debug-console)
     (when project-name (list :projectName project-name))
     (when env (list :env env)))))

(defun lsp-proxy-java--debug-settings-json ()
  "Return lsp-proxy's Java Debug settings as JSON."
  (json-serialize
   (list :hotCodeReplace (symbol-name lsp-proxy-java-hot-code-replace-mode)
         :jdwpRequestTimeout lsp-proxy-java-debug-request-timeout
         :logLevel "INFO")))

(defun lsp-proxy-java--debug-sync-settings ()
  "Send lsp-proxy's Java Debug settings to jdtls."
  (condition-case error-data
      (lsp-proxy-java--jdtls-execute
       "vscode.java.updateDebugSettings"
       (list (lsp-proxy-java--debug-settings-json)))
    (error
     (message "lsp-proxy-java could not update Java Debug settings: %s"
              (error-message-string error-data))
     nil)))

(defun lsp-proxy-java--debug-adapter-port ()
  "Ask jdtls to start Java Debug and return its adapter port."
  (lsp-proxy-java--debug-sync-settings)
  (let ((port
         (condition-case err
             (lsp-proxy-java--jdtls-execute
              "vscode.java.startDebugSession"
              nil
              (max 1.0 (/ lsp-proxy-java-debug-request-timeout 1000.0)))
           (jsonrpc-error
            (if (string-match-p "vscode\\.java\\.startDebugSession"
                                (error-message-string err))
                (user-error
                 "JDTLS Java Debug is not available; configure the Java Debug bundle")
              (signal (car err) (cdr err)))))))
    (unless (natnump port)
      (user-error
       "JDTLS did not start Java Debug; configure its debug bundle"))
    port))

(defun lsp-proxy-java--debug-prepare (config)
  "Prepare fresh JDTLS Java Debug resources for CONFIG."
  (let ((source (plist-get config 'lsp-proxy-java-source)))
    (with-current-buffer (if source (find-file-noselect source) (current-buffer))
      (plist-put config 'port (lsp-proxy-java--debug-adapter-port)))))

(defun lsp-proxy-java--debug-main-target-for-class (root class &optional file)
  "Resolve CLASS and its jdtls project under ROOT.
When FILE is non-nil, prefer a target declared in FILE."
  (let* ((items (lsp-proxy-java--jdtls-execute
                 "vscode.java.resolveMainClass"
                 (list (lsp-proxy--path-to-uri root))))
         (matches (seq-filter
                   (lambda (item)
                     (and (or (null class)
                              (equal class (plist-get item :mainClass)))
                          (or (null file)
                              (when-let* ((path (plist-get item :filePath)))
                                (equal (file-truename file)
                                       (file-truename path))))))
                   (append items nil))))
    (unless matches
      (user-error "JDTLS found no main class%s%s"
                  (if class (format " `%s'" class) "")
                  (if file (format " in %s" file) "")))
    (let* ((choices (mapcar (lambda (item)
                              (cons (format "%s (%s)"
                                            (plist-get item :mainClass)
                                            (plist-get item :projectName))
                                    item))
                            matches))
           (item (if (= (length choices) 1)
                     (cdar choices)
                   (cdr (assoc (completing-read "Main class: " choices nil t)
                               choices)))))
      (unless (and (stringp (plist-get item :mainClass))
                   (stringp (plist-get item :projectName)))
        (user-error "JDTLS returned an invalid main class"))
      item)))

(defun lsp-proxy-java--debug-main-target (file root)
  "Resolve FILE's main class and jdtls project under ROOT."
  (lsp-proxy-java--debug-main-target-for-class root nil file))

(defun lsp-proxy-java--debug-module-name (file)
  "Return FILE's nearest build module name."
  (file-name-nondirectory
   (directory-file-name (lsp-proxy-java--nearest-build-root file))))

(defun lsp-proxy-java--debug-project-name (file)
  "Resolve FILE's actual jdtls project from its type declaration.
Fall back to the nearest build module name when jdtls cannot resolve the
selection."
  (condition-case err
      (let* ((position (lsp-proxy-java--first-type-position
                        (lsp-proxy-java--document-symbols)))
             (element
              (and position
                   (lsp-proxy-java--jdtls-execute
                    "vscode.java.resolveElementAtSelection"
                    (list (lsp-proxy--path-to-uri file)
                          (plist-get position :line)
                          (plist-get position :character)))))
             (name (plist-get element :projectName)))
        (if (and (stringp name) (not (string-empty-p name)))
            name
          (lsp-proxy-java--debug-module-name file)))
    (jsonrpc-error
     (if (string-match-p "resolveElementAtSelection" (error-message-string err))
         (lsp-proxy-java--debug-module-name file)
       (signal (car err) (cdr err))))))

(defun lsp-proxy-java--debug-resolved-classpaths (class &optional project-name)
  "Resolve Java Debug module and class paths for CLASS.
PROJECT-NAME, when non-nil, selects the exact jdtls project."
  (let ((response
         (lsp-proxy-java--jdtls-execute
          "vscode.java.resolveClasspath"
          (if project-name (list class project-name) (list class)))))
    (unless (and (vectorp response) (= (length response) 2))
      (user-error "JDTLS Java Debug returned invalid classpaths"))
    (list (append (aref response 0) nil)
          (append (aref response 1) nil))))

(defun lsp-proxy-java--debug-build (class project-name root)
  "Ask jdtls to build CLASS in PROJECT-NAME under ROOT before debugging."
  (when lsp-proxy-java-debug-build-before-launch
    (save-some-buffers
     t (lambda ()
         (and buffer-file-name
              (file-in-directory-p buffer-file-name root))))
    (let ((status
           (condition-case err
               (lsp-proxy-java--jdtls-execute
                "vscode.java.buildWorkspace"
                (list (json-serialize
                       (append (list :mainClass class
                                     :isFullBuild :false)
                               (when project-name
                                 (list :projectName project-name)))))
                lsp-proxy-java-debug-build-timeout)
             (jsonrpc-error
              (if (string-match-p "vscode\\.java\\.buildWorkspace"
                                  (error-message-string err))
                  'unsupported
                (signal (car err) (cdr err)))))))
      (cond
       ((eq status 'unsupported)
        (message "JDTLS does not support vscode.java.buildWorkspace; launching without pre-build"))
       ((not (equal status "SUCCEED"))
        (user-error "Java launch cancelled: build returned %s" status))))))

(defun lsp-proxy-java--debug-main-config (file root project-name class
                                               module-paths class-paths)
  "Return a Java Debug launch config for CLASS in FILE."
  (append
   (lsp-proxy-java--debug-launch-options
    file root project-name lsp-proxy-java-main-environment)
   (list :mainClass class
         :modulePaths (vconcat module-paths)
         :classPaths (vconcat class-paths)
         :args (combine-and-quote-strings lsp-proxy-java-main-arguments)
         :vmArgs (combine-and-quote-strings
                  lsp-proxy-java-main-jvm-arguments))))

(defun lsp-proxy-java--debug-test-config (file root project-name classpaths
                                               class method)
  "Return a Java Debug launch config for a JUnit CLASS or METHOD."
  (let ((launch (lsp-proxy-java--junit-console-launch classpaths class method)))
    (append
     (lsp-proxy-java--debug-launch-options
      file root project-name lsp-proxy-java-test-environment)
     (list :mainClass "org.junit.platform.console.ConsoleLauncher"
           :classPaths (vector (car launch))
           :args (combine-and-quote-strings (cdr launch))
           :vmArgs (combine-and-quote-strings
                    lsp-proxy-java-test-jvm-arguments)))))

(defun lsp-proxy-java--debug-populate-launch-config (config)
  "Populate CONFIG using jdtls Java Debug.
This is a compatibility path for older Dape configs or CodeLens handlers that
were originally built for the IntelliJ backend."
  (setq config (plist-put config :type "java"))
  (if (not (equal (plist-get config :request) "launch"))
      (plist-put config 'port (lsp-proxy-java--debug-adapter-port))
    (let* ((uri (plist-get config 'lsp-proxy-uri))
           (file (or (and uri (lsp-proxy--uri-to-path uri))
                     buffer-file-name
                     (user-error "lsp-proxy-java: no source file for Java Debug launch")))
           (main-class (plist-get config :mainClass)))
      (with-current-buffer (find-file-noselect file)
        (let* ((root (lsp-proxy-java--nearest-build-root file))
               (target (lsp-proxy-java--debug-main-target-for-class
                        root main-class file))
               (class (plist-get target :mainClass))
               (project-name (plist-get target :projectName))
               (paths (lsp-proxy-java--debug-resolved-classpaths
                       class project-name)))
          (lsp-proxy-java--debug-build class project-name root)
          (setq config (plist-put config :mainClass class))
          (setq config (plist-put config :projectName project-name))
          (setq config (plist-put config :cwd root))
          (setq config (plist-put config :javaExec
                                  (lsp-proxy-java--java-program
                                   (lsp-proxy--path-to-uri file))))
          (unless (plist-member config :console)
            (setq config (plist-put config :console lsp-proxy-java-debug-console)))
          (setq config (plist-put config :modulePaths (vconcat (car paths))))
          (setq config (plist-put config :classPaths (vconcat (cadr paths))))
          (setq config (plist-put config 'port (lsp-proxy-java--debug-adapter-port)))
          config)))))

;;;###autoload
(defun lsp-proxy-java-run-main (&optional debug)
  "Run the current Java class.
With prefix argument DEBUG, launch it through Dape when available."
  (interactive "P")
  (if debug
      (lsp-proxy-java-debug-main)
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
  (if debug
      (lsp-proxy-java-debug-test)
    (let ((file (lsp-proxy-java--current-file)))
      (unless (lsp-proxy-java--jdtls-test-file-p file)
        (user-error "jdtls does not identify the current file as a test"))
      (let* ((root (lsp-proxy-java--nearest-build-root file))
             (target (lsp-proxy-java--current-target))
             (classpaths (lsp-proxy-java--jdtls-classpaths file "test"))
             (command
              (lsp-proxy-java--junit-command
               file classpaths
               (plist-get target :class)
               (plist-get target :method))))
        (lsp-proxy-java--start-compilation
         command root "test" lsp-proxy-java-test-environment
         (lsp-proxy--path-to-uri file))))))

;;;###autoload
(defun lsp-proxy-java-debug-main ()
  "Debug the current Java main class through jdtls Java Debug and Dape."
  (interactive)
  (lsp-proxy-java--ensure-dape)
  (let* ((file (lsp-proxy-java--current-file))
         (root (lsp-proxy-java--nearest-build-root file))
         (target (lsp-proxy-java--debug-main-target file root))
         (class (plist-get target :mainClass))
         (project-name (plist-get target :projectName)))
    (lsp-proxy-java--debug-build class project-name root)
    (let ((paths (lsp-proxy-java--debug-resolved-classpaths
                  class project-name)))
      (dape
       (lsp-proxy-java--debug-main-config
        file root project-name class (car paths) (cadr paths))))))

;;;###autoload
(defun lsp-proxy-java-debug-test ()
  "Debug the JUnit class or method at point through jdtls Java Debug and Dape."
  (interactive)
  (lsp-proxy-java--ensure-dape)
  (let ((file (lsp-proxy-java--current-file)))
    (unless (lsp-proxy-java--jdtls-test-file-p file)
      (user-error "jdtls does not identify the current file as a test"))
    (let* ((root (lsp-proxy-java--nearest-build-root file))
           (target (lsp-proxy-java--current-target))
           (class (plist-get target :class))
           (method (plist-get target :method))
           (project-name (lsp-proxy-java--debug-project-name file))
           (classpaths (lsp-proxy-java--jdtls-classpaths file "test")))
      (lsp-proxy-java--debug-build class project-name root)
      (dape
       (lsp-proxy-java--debug-test-config
        file root project-name classpaths class method)))))

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

(defcustom lsp-proxy-java-debug-install-dir
  (expand-file-name "java-debug/" lsp-proxy-install-dir)
  "Directory holding managed Microsoft Java Debug bundle jars.

jdtls loads these jars from its initialization options `bundles' array.  The
proxy injects the newest installed bundle automatically at startup, so users do
not need to edit `languages.toml' with machine-local jar paths."
  :type 'directory
  :group 'lsp-proxy)

(defun lsp-proxy-java-debug-bundles ()
  "Return the newest managed Java Debug bundle jar as a list."
  (when (file-directory-p lsp-proxy-java-debug-install-dir)
    (when-let* ((jar
                 (car
                  (sort
                   (directory-files-recursively
                    lsp-proxy-java-debug-install-dir
                    "com\\.microsoft\\.java\\.debug\\.plugin-[0-9].*\\.jar\\'")
                   (lambda (a b)
                     (lsp-proxy-java--version<
                      (lsp-proxy-java--jar-version-key b)
                      (lsp-proxy-java--jar-version-key a)))))))
      (list jar))))

(defun lsp-proxy-java--jar-version-key (jar)
  "Return a comparable list of integers for JAR's `lombok-<version>.jar' name."
  (let ((name (file-name-nondirectory jar)))
    (if (string-match "\\(?:\\`lombok-\\|java\\.debug\\.plugin-\\)\\([0-9.]+\\)" name)
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

(defun lsp-proxy-java--install-debug (force on-done)
  "Install the Java Debug jdtls bundle, then call ON-DONE with a report or nil."
  (let ((dir (file-name-as-directory
              (expand-file-name lsp-proxy-java-debug-install-dir))))
    (jsonrpc-async-request
     lsp-proxy--connection 'emacs/installJavaDebug
     (list :params (list :installDir dir :force (if force t :json-false)))
     :success-fn
     (lambda (result)
       (let ((version (plist-get result :version))
             (path (plist-get result :jarPath)))
         (funcall on-done
                  (if (eq (plist-get result :alreadyInstalled) t)
                      (format "Java Debug %s already installed" version)
                    (format "Installed Java Debug %s (jdtls bundle): %s" version path)))))
     :error-fn
     (lambda (err)
       (lsp-proxy--error "Java Debug install failed: %s" (or (plist-get err :message) err))
       (funcall on-done nil))
     :timeout-fn
     (lambda ()
       (lsp-proxy--error "Java Debug install timed out")
       (funcall on-done nil))
     :timeout 3600)))

;;;###autoload
(defun lsp-proxy-java-install-debug (&optional force)
  "Download the Microsoft Java Debug bundle used by jdtls debug commands.

With a prefix argument FORCE, re-download even if the latest resolved bundle is
already present.  The proxy injects installed bundles into jdtls initialization
options at startup, so a fresh install needs `lsp-proxy-restart'."
  (interactive "P")
  (lsp-proxy--ensure-connection)
  (lsp-proxy--set-global-status "Java Debug" "Installing Java Debug")
  (message "[lsp-proxy] Installing Java Debug ...")
  (lsp-proxy-java--install-debug
   force
   (lambda (report)
     (lsp-proxy--clear-global-status)
     (when report
       (message "[lsp-proxy] %s. Run M-x lsp-proxy-restart to use it" report)))))

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

(defcustom lsp-proxy-java-jdtls-install-hint
  "Run M-x lsp-proxy-java-install-server and choose `jdtls' to install it, \
or install Eclipse JDT Language Server yourself \
(https://github.com/eclipse-jdtls/eclipse.jdt.ls) and put its `bin/jdtls' \
launcher on PATH — e.g. `brew install jdtls' on macOS."
  "What to tell the user when the `java/classFileContents' backend is missing."
  :type 'string
  :group 'lsp-proxy)

(defun lsp-proxy-java-jdtls--install-hint (server-name)
  "Return the jdtls install hint for SERVER-NAME."
  (when (equal server-name lsp-proxy-java-class-file-contents-server-name)
    lsp-proxy-java-jdtls-install-hint))

(add-hook 'lsp-proxy-server-install-hint-functions
          #'lsp-proxy-java-jdtls--install-hint)

(defun lsp-proxy-java-jdtls-unload-function ()
  "Deregister jdtls-specific hooks.  See `unload-feature'."
  (remove-hook 'lsp-proxy-client-command-functions
               #'lsp-proxy-java--execute-client-command)
  (remove-hook 'lsp-proxy-server-install-hint-functions
               #'lsp-proxy-java-jdtls--install-hint)
  nil)

(provide 'lsp-proxy-java-jdtls)
;;; lsp-proxy-java-jdtls.el ends here
