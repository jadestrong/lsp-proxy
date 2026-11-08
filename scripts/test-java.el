;;; test-java.el --- Shared Java integration regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'lsp-proxy-java)
(require 'lsp-proxy-java-jdtls)

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

;;; test-java.el ends here
