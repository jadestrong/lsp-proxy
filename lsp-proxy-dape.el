;;; lsp-proxy-dape.el --- Debug integration for lsp-proxy's IntelliJ backend -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Wires `dape' to the IntelliJ backend's own DAP endpoint.
;;
;; The backend hosts IntelliJ's XDebugger behind a DAP server: a plain
;; `workspace/executeCommand' with command `start_debug_server' (no
;; arguments) returns a fresh TCP port; connecting to it and sending a DAP
;; `initialize' with `adapterID = "intellij_debugger"' gets you a real
;; debug session (breakpoints, stepping, variables) with no VS Code
;; pieces involved.
;;
;; Unlike an earlier version of this file, `launch' does NOT rely on the
;; DAP endpoint to resolve `:cwd'/`:classPaths'/`:javaExec' on its own —
;; empirically it doesn't (or doesn't reliably): omitting them produced a
;; JVM that started but couldn't find the main class at all (wrong/empty
;; classpath). The VS Code extension's `resolveLaunchConfig' (`dap.ts')
;; resolves these explicitly via three `workspace/executeCommand' calls
;; before launching, and that DOES work (verified: matching a real
;; `cwd'/JDK/`@argfile' classpath in the launched process). This file
;; mirrors that exactly instead of trusting the adapter to self-resolve.
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
;; `lsp-proxy-dape--populate-launch-config' without leaking into the DAP
;; body, for the same reason.
;;
;; Requires `dape'. Load this file only if you have it installed;
;; `lsp-proxy.el' requires it optionally (`require' with NOERROR) so
;; nothing else in lsp-proxy depends on it.

;;; Code:

(require 'dape)
(require 'lsp-proxy-core)
(require 'lsp-proxy-utils)

;;; External variable from lsp-proxy.el
(defvar lsp-proxy-mode)

(defconst lsp-proxy-dape--type "intellij_debugger"
  "The `:type'/DAP `adapterID' the backend's debug endpoint requires.
Anything else gets \"No debugger adapter found for given adapter id\".")

(defconst lsp-proxy-dape--run-main-command "intellij_debugger.runMain"
  "The CodeLens/code-action command emitted above `main' methods.
This is a client-side command in the VS Code extension (its DAP
integration registers a matching local command and never forwards it
to the server) — sending it through `workspace/executeCommand' just
gets \"Unknown command\" back. `lsp-proxy-codelens.el' recognises this
constant and routes to `lsp-proxy-dape-run-main' instead.")

(defun lsp-proxy-dape--execute-command (command &optional argument)
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
     (user-error "lsp-proxy-dape: `%s' failed: %s" command (error-message-string err)))))

(defun lsp-proxy-dape--start-debug-server ()
  "Ask the language server for a fresh DAP endpoint port."
  (let ((port (lsp-proxy-dape--execute-command "start_debug_server")))
    (unless (numberp port)
      (user-error "lsp-proxy-dape: `start_debug_server' returned %S, expected a port number" port))
    port))

(defun lsp-proxy-dape--resolve-uri (main-class)
  "Resolve the source file URI for MAIN-CLASS via the language server."
  (or (plist-get
       (lsp-proxy-dape--execute-command
        "intellij.java.resolveClassDocument" (list :fqn main-class))
       :uri)
      (user-error "lsp-proxy-dape: could not resolve a source file for `%s'" main-class)))

(defun lsp-proxy-dape--populate-launch-config (config)
  "`fn' for the `intellij_debugger' launch/attach configs.

Always fetches a fresh debug-server `port'. For `:request \"launch\"'
also resolves `:cwd', `:classPaths'/`:modulePaths'/`:moduleName' and
`:javaExec' for the module owning `:mainClass', mirroring the VS Code
extension's `resolveLaunchConfig' (`dap.ts') — see this file's
Commentary for why that resolution can't be skipped. `:request
\"attach\"' needs none of this (a plain JDWP host/port), so only the
port gets added.

Uses the bare-symbol scratch key `lsp-proxy-uri' for an
already-known source URI (from a CodeLens click) to skip the extra
`resolveClassDocument' round-trip; harmless to omit."
  (setq config (plist-put config 'port (lsp-proxy-dape--start-debug-server)))
  (if (not (equal (plist-get config :request) "launch"))
      config
    (let* ((main-class (or (plist-get config :mainClass)
                           (user-error "lsp-proxy-dape: no `:mainClass' to resolve a launch config for")))
           (uri (or (plist-get config 'lsp-proxy-uri)
                   (lsp-proxy-dape--resolve-uri main-class))))
      (unless (plist-get config :classPaths)
        (let ((cp (lsp-proxy-dape--execute-command
                   "intellij.java.resolveClasspath" (list :uri uri))))
          (setq config (plist-put config :classPaths (plist-get cp :classpath)))
          ;; For a JPMS launch the server also returns the module path and
          ;; owning module name, so the main class runs from the module path
          ;; (`-m moduleName/mainClass') instead of the classpath.
          (when-let* ((mp (plist-get cp :modulePath))
                      ((> (length mp) 0)))
            (setq config (plist-put config :modulePaths mp)))
          (when-let* ((mn (plist-get cp :moduleName)))
            (setq config (plist-put config :moduleName mn)))))
      (unless (plist-get config :cwd)
        ;; Optional: defaults to the module's project directory server-side.
        ;; Without it the launched process would inherit lsp-proxy's own
        ;; directory instead.
        (condition-case err
            (when-let* ((wd (lsp-proxy-dape--execute-command
                             "intellij.java.resolveWorkingDirectory" (list :uri uri)))
                        (cwd (plist-get wd :workingDirectory)))
              (setq config (plist-put config :cwd cwd)))
          (error
           (message "lsp-proxy-dape: working directory resolution failed, using default: %s"
                    (error-message-string err)))))
      (unless (plist-get config :javaExec)
        (let ((java (lsp-proxy-dape--execute-command
                     "intellij.java.resolveJavaExecutable" (list :uri uri))))
          (setq config (plist-put config :javaExec (plist-get java :javaExec)))))
      config)))

;; Named templates for `M-x dape'. `:mainClass''s form is only evaluated by
;; `dape' when reading a config by name (`dape--config-eval'); by the time
;; `fn' (`lsp-proxy-dape--populate-launch-config') runs, it's already a
;; concrete string.
(with-eval-after-load 'dape
  (add-to-list 'dape-configs
              `(intellij-launch
                modes (java-mode java-ts-mode kotlin-mode)
                host "127.0.0.1"
                fn lsp-proxy-dape--populate-launch-config
                :type ,lsp-proxy-dape--type
                :request "launch"
                :mainClass (read-string "Main class (fully qualified): ")))
  (add-to-list 'dape-configs
              `(intellij-attach
                modes (java-mode java-ts-mode kotlin-mode)
                host "127.0.0.1"
                fn lsp-proxy-dape--populate-launch-config
                :type ,lsp-proxy-dape--type
                :request "attach"
                :hostName "127.0.0.1"
                :port (read-number "JDWP port: " 5005))))

;;;###autoload
(defun lsp-proxy-dape-run-main (arguments)
  "Handle the `intellij_debugger.runMain' command's ARGUMENTS.
ARGUMENTS is the command's raw `:arguments' vector as delivered by the
proxy — its first element is `{mainClass, uri?, noDebug?}', mirroring
the VS Code extension's `RunMainArgs'. Builds a minimal config
(`:mainClass' plus the already-known `uri', stashed under the bare
`lsp-proxy-uri' key) and lets `lsp-proxy-dape--populate-launch-config'
\(via `fn') resolve the rest, same as the named `dape-configs'
templates. Meant to be called from `lsp-proxy-codelens.el' when a
CodeLens command matches `lsp-proxy-dape--run-main-command'."
  (let* ((args (and (vectorp arguments) (> (length arguments) 0) (aref arguments 0)))
         (main-class (and args (plist-get args :mainClass)))
         (uri (and args (plist-get args :uri)))
         (no-debug (and args (eq (plist-get args :noDebug) t))))
    (unless main-class
      (user-error "lsp-proxy-dape: `%s' had no mainClass in its arguments"
                 lsp-proxy-dape--run-main-command))
    (dape
     (append
      (list 'host "127.0.0.1"
            :type lsp-proxy-dape--type
            :request "launch"
            :mainClass main-class
            'fn #'lsp-proxy-dape--populate-launch-config)
      (when uri (list 'lsp-proxy-uri uri))
      (when no-debug (list :noDebug t))))))

(provide 'lsp-proxy-dape)
;;; lsp-proxy-dape.el ends here
