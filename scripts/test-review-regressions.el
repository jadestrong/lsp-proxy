;;; test-review-regressions.el --- PR review regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'lsp-proxy-core)
(require 'lsp-proxy-java)
(require 'lsp-proxy-diagnostics)

(defvar lsp-proxy-mode nil)
(defvar flycheck-checkers)
(defvar flycheck-check-syntax-automatically)
(defvar flycheck-last-status-change)
(defvar flycheck-current-errors)
(declare-function flycheck-mode "ext:flycheck" (&optional arg))
(declare-function flycheck-compute-working-directory "ext:flycheck" (checker))
(declare-function flycheck-start-current-syntax-check "ext:flycheck" (checker))
(declare-function flycheck-error-message "ext:flycheck" (error))

(ert-deftest lsp-proxy-review-test-request-dispatch-preserves-result ()
  (let ((action '(:title "Continue" :extra "server-data"))
        (edit '(:changes [])))
    (cl-letf (((symbol-function 'lsp-proxy--apply-workspace-edit)
               (lambda (value _command)
                 (should (eq value edit)) '(:applied t)))
              ((symbol-function 'browse-url)
               (lambda (url) (should (equal url "https://example.org")) 'opened))
              ((symbol-function 'lsp-proxy--handle-show-message-request)
               (lambda (msg) (should (equal msg '(:message "Continue?"))) action)))
      (should (equal (lsp-proxy--handle-request
                      nil 'workspace/applyEdit (list :edit edit)) '(:applied t)))
      (should (eq (lsp-proxy--handle-request
                   nil 'eslint/openDoc '(:url "https://example.org")) 'opened))
      (should (eq (lsp-proxy--handle-request
                   nil 'window/showMessageRequest '(:message "Continue?")) action))
      (should-not (lsp-proxy--handle-request nil 'unknown/method nil)))))

(ert-deftest lsp-proxy-review-test-decompiled-windows-uri-roundtrip ()
  (dolist (uri '("jar:file:///C:/repo/foo.jar!/A.java"
                 "jar:file:///C:/repo/a%3Ab.jar!/./pkg/../A.java"
                 "jdt://contents/lib/A.class?=C:/repo/%2F%25%3A"
                 "jar:file:///库/./a.jar!/../A.java"))
    (let* ((escaped (lsp-proxy--decompiled-escape uri))
           (name (lsp-proxy--decompiled-uri-to-file-name uri)))
      (should-not (string-match-p ":" escaped))
      (should (equal (lsp-proxy--decompiled-unescape escaped) uri))
      (should (file-name-absolute-p name))
      (should (equal (expand-file-name name) name))
      (should (equal (lsp-proxy--path-to-uri name) uri)))))

(ert-deftest lsp-proxy-review-test-old-decompiled-payload-remains-readable ()
  ;; Older names retained literal colons; percent sequences were already escaped.
  (should (equal (lsp-proxy--decompiled-file-name-to-uri
                  (concat lsp-proxy--decompiled-prefix
                          "jar:file:%2F%2F/C:/a%253Ab.jar!/A.java"))
                 "jar:file:///C:/a%3Ab.jar!/A.java")))

(defmacro lsp-proxy-review-test--with-decompiled-diagnostics (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (java-mode)
     (insert "class Example {}\n")
     (setq buffer-file-name
           (lsp-proxy--decompiled-uri-to-file-name
            "jdt://contents/library/Example.class")
           default-directory (file-name-directory buffer-file-name))
     (let ((lsp-proxy-mode t)
           (lsp-proxy--current-project-root "/project/")
           (lsp-proxy--diagnostics-map (make-hash-table :test 'equal)))
       (cl-letf (((symbol-function 'lsp-proxy-project-root) (lambda () "/project/")))
         ,@body))))

(defun lsp-proxy-review-test--store-diagnostic ()
  "Store one diagnostic for the current decompiled source buffer."
  (puthash buffer-file-name
           '((:range (:start (:line 0 :character 0)
                      :end (:line 0 :character 5))
              :severity 2 :message "Library diagnostic"))
           (lsp-proxy--ensure-project-map "/project/" lsp-proxy--diagnostics-map)))

(ert-deftest lsp-proxy-review-test-decompiled-diagnostics-default-suppression ()
  (lsp-proxy-review-test--with-decompiled-diagnostics
    (let ((lsp-proxy-diagnostics-in-decompiled-buffers nil))
      (should (lsp-proxy-diagnostics--suppressed-p))
      (cl-letf (((symbol-function 'lsp-proxy-diagnostics-flycheck-enable)
                 (lambda () (ert-fail "Flycheck must remain disabled")))
                ((symbol-function 'lsp-proxy-diagnostics-flymake-enable)
                 (lambda () (ert-fail "Flymake must remain disabled"))))
        (lsp-proxy--diagnostics-setup)))))

(ert-deftest lsp-proxy-review-test-decompiled-flycheck-opt-in ()
  (skip-unless (require 'flycheck nil t))
  (dolist (provider '(:flycheck :auto))
    (lsp-proxy-review-test--with-decompiled-diagnostics
      (let ((lsp-proxy-diagnostics-in-decompiled-buffers t)
            (lsp-proxy-diagnostics-provider provider)
            (flycheck-checkers (copy-sequence flycheck-checkers))
            (flycheck-check-syntax-automatically nil))
        (setq-local flycheck-checker 'lsp-proxy)
        (lsp-proxy-review-test--store-diagnostic)
        (unwind-protect
            (progn
              (should-not (lsp-proxy-diagnostics--suppressed-p))
              (lsp-proxy--diagnostics-setup)
              (should lsp-proxy-diagnostics--flycheck-enabled)
              (should (file-exists-p default-directory))
              (should (equal (flycheck-compute-working-directory 'lsp-proxy)
                             default-directory))
              ;; Run Flycheck's real start path, which validates the working dir.
              (flycheck-start-current-syntax-check 'lsp-proxy)
              (should (eq flycheck-last-status-change 'finished))
              (should (= (length flycheck-current-errors) 1))
              (should (equal (flycheck-error-message (car flycheck-current-errors))
                             "Library diagnostic")))
          (flycheck-mode -1))))))

(ert-deftest lsp-proxy-review-test-decompiled-flymake-opt-in ()
  (lsp-proxy-review-test--with-decompiled-diagnostics
    (let ((lsp-proxy-diagnostics-in-decompiled-buffers t)
          (lsp-proxy-diagnostics-provider :flymake)
          (flymake-start-on-flymake-mode nil)
          reported)
      (lsp-proxy-review-test--store-diagnostic)
      (unwind-protect
          (progn
            (lsp-proxy--diagnostics-setup)
            (should lsp-proxy-diagnostics--flymake-enabled)
            (lsp-proxy-diagnostics--flymake-backend
             (lambda (diagnostics &rest _)
               (setq reported (list diagnostics))))
            (should (= (length (car reported)) 1))
            (should (equal (flymake-diagnostic-text (caar reported))
                           "Library diagnostic")))
        (flymake-mode -1)))))

(ert-deftest lsp-proxy-review-test-decompiled-auto-flymake-opt-in ()
  (lsp-proxy-review-test--with-decompiled-diagnostics
    (let ((lsp-proxy-diagnostics-in-decompiled-buffers t)
          (lsp-proxy-diagnostics-provider :auto)
          (flymake-start-on-flymake-mode nil))
      ;; Simulate Flycheck being unavailable; run the real automatic selection.
      (cl-letf (((symbol-function 'flycheck-mode) nil))
        (unwind-protect
            (progn
              (lsp-proxy--diagnostics-setup)
              (should lsp-proxy-diagnostics--flymake-enabled)
              (should-not lsp-proxy-diagnostics--flycheck-enabled))
          (flymake-mode -1))))))

;;; test-review-regressions.el ends here
