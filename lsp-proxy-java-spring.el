;;; lsp-proxy-java-spring.el --- STS4 installation -*- lexical-binding: t; -*-

;;; Commentary:
;; Spring Boot LS supplements JDT LS with workspace symbols and live endpoints.

;;; Code:
(require 'json)
(require 'seq)
(require 'lsp-proxy-java)

(defcustom lsp-proxy-java-spring-install-dir
  (expand-file-name "spring-boot/" lsp-proxy-install-dir)
  "Directory holding managed STS4 releases."
  :type 'directory :group 'lsp-proxy)

(defcustom lsp-proxy-java-spring-version "1.62.0"
  "Spring Boot Tools release to install and use."
  :type 'string :group 'lsp-proxy)

(defcustom lsp-proxy-java-spring-java-home nil
  "JDK home used to launch STS4, independently of the project JDK.
STS4 1.62.0 requires JDK 21.  Nil uses JAVA_HOME or java on PATH."
  :type '(choice (const nil) directory) :group 'lsp-proxy)

(defun lsp-proxy-java-spring--release-directory ()
  "Return the directory of the selected STS4 release."
  (expand-file-name lsp-proxy-java-spring-version lsp-proxy-java-spring-install-dir))

(defun lsp-proxy-java-spring-bin-directory ()
  "Return the selected release's bin directory when installed."
  (let* ((bin (expand-file-name "bin/" (lsp-proxy-java-spring--release-directory)))
         (launcher (expand-file-name
                    (if (eq system-type 'windows-nt) "spring-boot-ls.bat" "spring-boot-ls") bin)))
    (when (file-executable-p launcher) bin)))

(defun lsp-proxy-java-spring-bundles ()
  "Return JDT bundles recorded in the selected release's install manifest."
  (let* ((root (lsp-proxy-java-spring--release-directory))
         (manifest (expand-file-name "spring-install.json" root)))
    (when (file-readable-p manifest)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents manifest)
            (let* ((info (json-parse-buffer :object-type 'plist :array-type 'list))
                   (paths (plist-get info :bundles))
                   (bundles (mapcar (lambda (path) (expand-file-name path root)) paths)))
              (when (and (equal (plist-get info :version) lsp-proxy-java-spring-version)
                         bundles (seq-every-p #'file-readable-p bundles))
                bundles)))
        (error (lsp-proxy--warn "Cannot read Spring install manifest: %s"
                                (error-message-string err)))))))

(defun lsp-proxy-java-spring-environment ()
  "Return the selected STS4 runtime environment additions."
  (when lsp-proxy-java-spring-java-home
    (list (concat "LSP_PROXY_SPRING_JAVA="
                  (expand-file-name
                   (if (eq system-type 'windows-nt) "bin/java.exe" "bin/java")
                   lsp-proxy-java-spring-java-home)))))

;;;###autoload
(defun lsp-proxy-java-install-spring (&optional force)
  "Install STS4 and its JDT bundles.  With prefix FORCE, reinstall.
Restart lsp-proxy after installation to load the bundles into JDT LS."
  (interactive "P")
  (lsp-proxy--ensure-connection)
  (lsp-proxy--set-global-status "Spring Boot" "Installing Spring Boot Tools")
  (jsonrpc-async-request
   lsp-proxy--connection 'emacs/installSpring
   (list :params (list :installDir (expand-file-name lsp-proxy-java-spring-install-dir)
                       :version lsp-proxy-java-spring-version
                       :force (if force t :json-false)))
   :success-fn
   (lambda (result)
     (lsp-proxy--clear-global-status)
     (message "[lsp-proxy] Spring Boot Tools %s installed: %s. Run M-x lsp-proxy-restart"
              (plist-get result :version) (plist-get result :launcherPath)))
   :error-fn
   (lambda (err)
     (lsp-proxy--clear-global-status)
     (lsp-proxy--error "Spring install failed: %s" (or (plist-get err :message) err)))
   :timeout-fn
   (lambda ()
     (lsp-proxy--clear-global-status)
     (lsp-proxy--error "Spring install timed out"))
   :timeout 3600))

(defun lsp-proxy-java-spring--install-hint (name)
  "Return an installation hint for Spring server NAME."
  (when (equal name "spring-boot")
    "Run M-x lsp-proxy-java-install-spring, then M-x lsp-proxy-restart (STS4 requires JDK 21)."))

;;;###autoload
(defun lsp-proxy-java-spring-connect-running-app ()
  "Select a running Spring application and connect or refresh its LiveData.
STS4 discovers local JVMs and configured remote applications.  After connecting,
use `lsp-proxy-spring-running-endpoints' to select an endpoint."
  (interactive)
  (let* ((origin (current-buffer))
         (envelope (lsp-proxy--build-params nil))
         (processes (lsp-proxy--request
                     'workspace/executeCommand
                     (plist-put (copy-sequence envelope) :params
                                (list :command "sts/livedata/listProcesses" :arguments []))
                     :timeout 60))
         (index 0)
         (candidates
          (delq nil
                (mapcar
                 (lambda (process)
                   (when (member (plist-get process :action)
                                 '("sts/livedata/connect" "sts/livedata/refresh"))
                     (cons (format "[%d] %s (%s)" (cl-incf index)
                                   (plist-get process :label) (plist-get process :action)) process)))
                 (seq-into processes 'list)))))
    (unless candidates
      (user-error "STS found no running applications; start Spring Boot with local JMX enabled"))
    (let* ((choice (completing-read "Spring application: " candidates nil t))
           (process (cdr (assoc choice candidates))))
      (unless (buffer-live-p origin) (user-error "The source buffer was closed; try again"))
      (with-current-buffer origin
        (lsp-proxy--request
         'workspace/executeCommand
         (plist-put (copy-sequence envelope) :params
                    (list :command (plist-get process :action)
                          :arguments (vector (list :processKey (plist-get process :processKey)))))
         :timeout 60))
      (message "[lsp-proxy] Spring LiveData connected; use M-x lsp-proxy-spring-running-endpoints"))))

(add-hook 'lsp-proxy-server-install-hint-functions #'lsp-proxy-java-spring--install-hint)

(provide 'lsp-proxy-java-spring)
;;; lsp-proxy-java-spring.el ends here
