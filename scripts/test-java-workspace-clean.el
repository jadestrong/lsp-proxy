;;; test-java-workspace-clean.el --- Cache cleanup lifecycle -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'lsp-proxy-java-jdtls)

(defmacro lsp-proxy-clean-test--with-source (&rest body)
  (declare (indent 0))
  `(let* ((root (make-temp-file "java-clean-ert" t))
          (file (expand-file-name "App.java" root))
          (connection (list 'test-connection))
          (lsp-proxy--connection connection)
          success failure timeout request reopened warnings)
     (unwind-protect
         (progn
           (with-temp-file file (insert "saved"))
           (with-temp-buffer
             (setq buffer-file-name file
                   default-directory (file-name-as-directory root))
             (insert "unsaved changes")
             (setq-local lsp-proxy-mode t
                         lsp-proxy--language "java"
                         lsp-proxy--buffer-opened t
                         lsp-proxy--workspace-roots (list root))
             (cl-letf (((symbol-function 'lsp-proxy--ensure-connection) #'ignore)
                       ((symbol-function 'lsp-proxy--send-did-change) #'ignore)
                       ((symbol-function 'lsp-proxy-project-root) (lambda () root))
                       ((symbol-function 'lsp-proxy--remove-progress-in-workspace) #'ignore)
                       ((symbol-function 'lsp-proxy--on-doc-open)
                        (lambda () (push (cons buffer-file-name (buffer-string)) reopened)))
                       ((symbol-function 'lsp-proxy--build-params)
                        (lambda (params) (list :uri file :params params)))
                       ((symbol-function 'lsp-proxy--warn)
                        (lambda (&rest args) (push args warnings)))
                       ((symbol-function 'jsonrpc-request)
                        (lambda (&rest _) (ert-fail "Cleanup used synchronous RPC")))
                       ((symbol-function 'jsonrpc-async-request)
                        (lambda (conn method params &rest args)
                          (setq request (list conn method params)
                                success (plist-get args :success-fn)
                                failure (plist-get args :error-fn)
                                timeout (plist-get args :timeout-fn)))))
               ,@body)))
       (delete-directory root t))))

(ert-deftest lsp-proxy-clean-test-async-success-preserves-unsaved-content ()
  (lsp-proxy-clean-test--with-source
    (lsp-proxy-java-clean-workspace)
    (should (eq (car request) connection))
    (should (eq (cadr request) 'emacs/cleanJavaWorkspace))
    (should (equal (plist-get (plist-get (nth 2 request) :params) :serverName) "jdtls"))
    (should-not reopened)
    (funcall success (list :workspaceRoot root :closedDocuments (vector file)))
    (should (equal reopened (list (cons file "unsaved changes"))))
    (should (equal (buffer-string) "unsaved changes"))
    (should (buffer-modified-p))
    (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "saved"))))

(ert-deftest lsp-proxy-clean-test-worker-failure-reopens-stopped-documents ()
  (lsp-proxy-clean-test--with-source
    (lsp-proxy-java-clean-workspace)
    (funcall success (list :workspaceRoot root :closedDocuments (list file)
                           :error "Cache is still locked"))
    (should reopened)
    (should warnings)))

(ert-deftest lsp-proxy-clean-test-reopen-sends-widened-unsaved-text-and-resets-version ()
  (let ((open-document (symbol-function 'lsp-proxy--on-doc-open)))
    (lsp-proxy-clean-test--with-source
      (setq-local lsp-proxy--skip-auto-open t)
      (lsp-proxy--set-doc-version 17)
      (narrow-to-region 2 5)
      (let (sent)
        (cl-letf (((symbol-function 'lsp-proxy--on-doc-open) open-document)
                  ((symbol-function 'jsonrpc-notify)
                   (lambda (_connection method params)
                     (setq sent (list method (plist-get params :params)))))
                  ((symbol-function 'lsp-proxy--TextDocumentIdentifier)
                   (lambda () (list :uri file))))
          (lsp-proxy-java--clean-workspace-buffers root (list file) t))
        (should lsp-proxy--buffer-opened)
        (should-not lsp-proxy--skip-auto-open)
        (should (eq (car sent) 'textDocument/didOpen))
        (let ((document (plist-get (cadr sent) :textDocument)))
          (should (equal (plist-get document :text) "unsaved changes"))
          (should (= (plist-get document :version) 0)))
        (should (buffer-narrowed-p))
        (should (buffer-modified-p))))))

(ert-deftest lsp-proxy-clean-test-preflight-rejection-does-not-reset-buffers ()
  (lsp-proxy-clean-test--with-source
    (lsp-proxy-java-clean-workspace)
    (funcall failure '(:message "Shared cache"))
    (should lsp-proxy--buffer-opened)
    (should-not reopened)
    (should warnings)))

(ert-deftest lsp-proxy-clean-test-jsonrpc-rejection-allows-retry ()
  (let ((async-request (symbol-function 'jsonrpc-async-request)))
    (lsp-proxy-clean-test--with-source
      (setq connection (make-instance 'jsonrpc-connection :name "cleanup-test")
            lsp-proxy--connection connection)
      (cl-letf (((symbol-function 'jsonrpc-async-request) async-request)
                ((symbol-function 'jsonrpc-connection-send) #'ignore))
        (unwind-protect
            (progn
              (lsp-proxy-java-clean-workspace)
              (jsonrpc-connection-receive
               connection
               (list :jsonrpc "2.0" :id (jsonrpc--next-request-id connection)
                     :error '(:code -32603 :message "No jdtls server serves this document")))
              (should-not (jsonrpc--continuations connection))
              (should warnings)
              (should lsp-proxy--buffer-opened)
              (lsp-proxy-java-clean-workspace)
              (should (= (jsonrpc--next-request-id connection) 2))
              (jsonrpc-connection-receive
               connection
               (list :jsonrpc "2.0" :id (jsonrpc--next-request-id connection)
                     :result (list :workspaceRoot root :closedDocuments [])))
              (should-not (jsonrpc--continuations connection)))
          (dolist (cont (copy-sequence (jsonrpc--continuations connection)))
            (jsonrpc--remove connection (car cont)))
          (when-let* ((events (jsonrpc--events-buffer connection)))
            (kill-buffer events)))))))

(ert-deftest lsp-proxy-clean-test-old-connection-callback-is-ignored ()
  (lsp-proxy-clean-test--with-source
    (lsp-proxy-java-clean-workspace)
    (setq lsp-proxy--connection (list 'replacement-connection))
    (funcall success (list :workspaceRoot root :closedDocuments (vector file)))
    (should-not reopened)
    (should lsp-proxy--buffer-opened)))

(ert-deftest lsp-proxy-clean-test-repeated-command-is-sent-to-backend ()
  (lsp-proxy-clean-test--with-source
    (let (requests)
      (cl-letf (((symbol-function 'jsonrpc-async-request)
                 (lambda (_conn method params &rest _)
                   (push (list method params) requests))))
        (lsp-proxy-java-clean-workspace)
        (lsp-proxy-java-clean-workspace))
      (should (= (length requests) 2))
      (should (equal (car requests) (cadr requests))))))

(ert-deftest lsp-proxy-clean-test-timeout-allows-later-reconnection ()
  (lsp-proxy-clean-test--with-source
    (lsp-proxy-java-clean-workspace)
    (funcall timeout)
    (should-not lsp-proxy--buffer-opened)
    (should-not reopened)
    (should warnings)))

(ert-deftest lsp-proxy-clean-test-dispatch-error-is-propagated ()
  (lsp-proxy-clean-test--with-source
    (cl-letf (((symbol-function 'jsonrpc-async-request)
               (lambda (&rest _) (error "Connection failed"))))
      (should-error (lsp-proxy-java-clean-workspace)))
    (should lsp-proxy--buffer-opened)))

(ert-deftest lsp-proxy-clean-test-buffer-filter-preserves-other-workspaces ()
  (lsp-proxy-clean-test--with-source
    (with-temp-buffer
      (setq buffer-file-name "/other/Keep.java")
      (setq-local lsp-proxy-mode t lsp-proxy--buffer-opened t)
      (lsp-proxy-java--clean-workspace-buffers root (list file) t)
      (should lsp-proxy--buffer-opened))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "Keep.ts" root))
      (setq-local lsp-proxy-mode t lsp-proxy--buffer-opened t)
      (lsp-proxy-java--clean-workspace-buffers root (list file) t)
      (should lsp-proxy--buffer-opened))))

(ert-deftest lsp-proxy-clean-test-origin-buffer-may-be-killed ()
  (lsp-proxy-clean-test--with-source
    (lsp-proxy-java-clean-workspace)
    (let ((source (current-buffer)))
      (with-temp-buffer
        (setq buffer-file-name file)
        (setq-local lsp-proxy-mode t lsp-proxy--buffer-opened t)
        (insert "other live buffer")
        (kill-buffer source)
        (funcall success (list :workspaceRoot root :closedDocuments (vector file)))
        (should (equal reopened (list (cons file "other live buffer"))))))))

;;; test-java-workspace-clean.el ends here
