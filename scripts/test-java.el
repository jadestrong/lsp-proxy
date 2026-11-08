;;; test-java.el --- Shared Java integration regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'lsp-proxy-java)
(require 'lsp-proxy-java-jdtls)

(defvar lsp-proxy-mode nil)

(ert-deftest lsp-proxy-java-test-import-status-reaches-shared-log ()
  (let ((lsp-proxy-java-import-log-buffer-name " *java-import-test*")
        (lsp-proxy-java-import-log-max-lines nil)
        (lsp-proxy-java-import-log-auto-display nil))
    (unwind-protect
        (progn
          (lsp-proxy--handle-notification
           nil 'emacs/javaImportLog
           '(:type 1 :message "Cannot resolve dependency" :rootPath "/project/"))
          (with-current-buffer (lsp-proxy-java--import-log-buffer)
            (should (derived-mode-p 'lsp-proxy-java-import-log-mode))
            (should buffer-read-only)
            (should (string-match-p "Cannot resolve dependency" (buffer-string)))
            (should (string-match-p "/project/" (buffer-string)))))
      (when-let* ((buffer (get-buffer lsp-proxy-java-import-log-buffer-name)))
        (kill-buffer buffer)))))

(ert-deftest lsp-proxy-java-test-jdt-source-provider-keeps-routing ()
  (let ((uri "jdt://contents/library/Example.class")
        (doc-uri "file:///project/Example.java")
        (lsp-proxy-java--content-cache (make-hash-table :test 'equal))
        captured)
    (cl-letf (((symbol-function 'lsp-proxy-java--routing-uri) (lambda () doc-uri))
              ((symbol-function 'jsonrpc-request)
               (lambda (_connection method params &rest _)
                 (setq captured (list method params)) "class Example {}"))
              ((symbol-function 'lsp-proxy--ensure-connection) #'ignore))
      (should (equal (plist-get (lsp-proxy-java--content uri) :code) "class Example {}"))
      (should (eq (car captured) 'emacs/forwardRequest))
      (should (equal (plist-get (cadr captured) :uri) doc-uri))
      (should (equal (plist-get (plist-get (cadr captured) :params) :method)
                     "java/classFileContents")))))

(ert-deftest lsp-proxy-java-test-project-runtime-selects-explicit-uri ()
  (with-temp-buffer
    (setq buffer-file-name "/first/Example.java")
    (let ((lsp-proxy-mode t)
          captured)
      (cl-letf (((symbol-function 'lsp-proxy--ensure-connection) #'ignore)
                ((symbol-function 'lsp-proxy--make-virtual-doc-context) #'ignore)
                ((symbol-function 'jsonrpc-request)
                 (lambda (_connection method params &rest _)
                   (push (list method params) captured)
                   (if (equal (plist-get params :uri) "file:///second/Example.java")
                       '(:javaHome "/jdk/second" :javaExec "/jdk/second/bin/java")
                     '(:javaHome "/jdk/first" :javaExec "/jdk/first/bin/java")))))
        (should (equal (plist-get (lsp-proxy-java--project-runtime) :javaHome)
                       "/jdk/first"))
        (should (equal (lsp-proxy-java--project-java-executable
                        "file:///second/Example.java")
                       "/jdk/second/bin/java"))
        (should (= (length captured) 2))
        (should (eq (caar captured) 'emacs/getJavaProjectRuntime))
        (should-not (plist-member (cadar captured) :textDocument))))))

(ert-deftest lsp-proxy-java-test-project-configuration-build-files ()
  (with-temp-buffer
    (let (captured)
      (cl-letf (((symbol-function 'lsp-proxy-java--forward)
                 (lambda (method params &optional notify _server)
                   (setq captured (list method params notify))))
                ((symbol-function 'lsp-proxy--info) #'ignore))
        (dolist (name '("pom.xml" "build.gradle" "build.gradle.kts"
                        "settings.gradle" "settings.gradle.kts"
                        ".classpath" ".project"))
          (setq buffer-file-name (concat "/project/" name)
                captured nil)
          (lsp-proxy-java-update-project-configuration)
          (should (equal (car captured) "java/projectConfigurationUpdate"))
          (should (equal (plist-get (cadr captured) :uri)
                         (concat "file:///project/" name)))
          (should (caddr captured)))
        (dolist (name '("pom.xml.bak" "pom.xml'" "notpom.xml" "Example.java"))
          (setq buffer-file-name (concat "/project/" name)
                captured nil)
          (should-error (lsp-proxy-java-update-project-configuration)
                        :type 'user-error)
          (should-not captured))))))

;;; test-java.el ends here
