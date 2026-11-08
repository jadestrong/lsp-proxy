;;; lsp-proxy-java-test.el --- Java test and subject navigation -*- lexical-binding: t; -*-

;;; Commentary:
;; Uses the same Java Test Runner delegate command as VS Code's Go to Test.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'xref)
(require 'lsp-proxy-java-jdtls)

(defcustom lsp-proxy-java-test-install-dir
  (expand-file-name "java-test/" lsp-proxy-install-dir)
  "Directory holding managed Java Test Runner bundles."
  :type 'directory :group 'lsp-proxy)

(defcustom lsp-proxy-java-test-version "0.47.0"
  "Java Test Runner release to install and load into JDTLS."
  :type 'string :group 'lsp-proxy)

(defun lsp-proxy-java-test-bundles ()
  "Return all JDT bundles from the selected Java Test Runner release."
  (let* ((root (expand-file-name lsp-proxy-java-test-version
                                 lsp-proxy-java-test-install-dir))
         (manifest (expand-file-name "test-install.json" root)))
    (when (file-readable-p manifest)
      (condition-case error
          (with-temp-buffer
            (insert-file-contents manifest)
            (let* ((info (json-parse-buffer :object-type 'plist :array-type 'list))
                   (names (plist-get info :bundles)))
              (when (and (equal (plist-get info :version) lsp-proxy-java-test-version)
                         names
                         (seq-some (lambda (name)
                                     (and (stringp name)
                                          (string-prefix-p "com.microsoft.java.test.plugin-" name)))
                                   names)
                         (seq-every-p
                          (lambda (name)
                            (and (stringp name)
                                 (string-match-p "\\`[[:alnum:]][[:alnum:]_.-]*\\.jar\\'" name)
                                 (file-readable-p (expand-file-name name root))))
                          names))
                (mapcar (lambda (name) (expand-file-name name root)) names))))
        (error (lsp-proxy--warn "Cannot read Java Test Runner manifest: %s"
                                (error-message-string error)))))))

;;;###autoload
(defun lsp-proxy-java-install-test (&optional force)
  "Install the Java Test Runner bundles needed for test navigation.
With prefix argument FORCE, reinstall the selected release."
  (interactive "P")
  (lsp-proxy--ensure-connection)
  (lsp-proxy--set-global-status "Java Test" "Installing Java Test Runner")
  (jsonrpc-async-request
   lsp-proxy--connection 'emacs/installJavaTest
   (list :params (list :installDir (expand-file-name lsp-proxy-java-test-install-dir)
                       :version lsp-proxy-java-test-version
                       :force (if force t :json-false)))
   :success-fn
   (lambda (result)
     (lsp-proxy--clear-global-status)
     (message "[lsp-proxy] Java Test Runner %s installed. Run M-x lsp-proxy-restart to load its bundles"
              (plist-get result :version)))
   :error-fn
   (lambda (error)
     (lsp-proxy--clear-global-status)
     (lsp-proxy--error "Java Test Runner install failed: %s"
                       (or (plist-get error :message) error)))
   :timeout-fn
   (lambda ()
     (lsp-proxy--clear-global-status)
     (lsp-proxy--error "Java Test Runner install timed out"))
   :timeout 3600))

(defvar-local lsp-proxy-java-test--generation 0
  "Generation of the latest test navigation request in this buffer.")

(defun lsp-proxy-java-test--sorted-items (items)
  "Sort navigation ITEMS by project, relevance, class name and URI."
  (sort (copy-sequence items)
        (lambda (a b)
          (let ((external-a (eq (plist-get a :outOfBelongingProject) t))
                (external-b (eq (plist-get b :outOfBelongingProject) t))
                (rank-a (or (plist-get a :relevance) most-positive-fixnum))
                (rank-b (or (plist-get b :relevance) most-positive-fixnum))
                (name-a (or (plist-get a :simpleName) ""))
                (name-b (or (plist-get b :simpleName) "")))
            (cond
             ((not (eq external-a external-b)) (not external-a))
             ((/= rank-a rank-b) (< rank-a rank-b))
             ((not (equal name-a name-b)) (string-lessp name-a name-b))
             (t (string-lessp (plist-get a :uri) (plist-get b :uri))))))))

(defun lsp-proxy-java-test--visit (result subject &optional origin-position)
  "Visit an item in navigation RESULT, looking for a test SUBJECT if non-nil."
  (let* ((items (lsp-proxy-java-test--sorted-items
                 (seq-filter (lambda (item) (stringp (plist-get item :uri)))
                             (append (plist-get result :items) nil))))
         (index 0)
         (candidates
          (mapcar (lambda (item)
                    (cons (format "[%d] %s — %s%s (%s)"
                                  (cl-incf index) (plist-get item :simpleName)
                                  (plist-get item :fullyQualifiedName)
                                  (if (eq (plist-get item :outOfBelongingProject) t)
                                      " [other project]" "")
                                  (lsp-proxy--uri-to-path (plist-get item :uri)))
                          item))
                  items)))
    (if (null items)
        (lsp-proxy--info "No %s found for this Java class"
                         (if subject "test subjects" "tests"))
      (when-let* ((item (if (= (length items) 1)
                            (car items)
                          (condition-case nil
                              (cdr (assoc (lsp-proxy--completing-read
                                           (if subject "Go to test subject: " "Go to test: ")
                                           (mapcar #'car candidates))
                                          candidates))
                            (quit nil)))))
        (save-excursion
          (save-restriction
            (widen)
            (when origin-position (goto-char origin-position))
            (xref-push-marker-stack)))
        ;; The server returns class URIs, not target ranges; VS Code opens the file.
        (lsp-proxy--goto-uri-position (plist-get item :uri) 0 0)))))

;;;###autoload
(defun lsp-proxy-java-go-to-test (&optional subject)
  "Find tests corresponding to the current Java class asynchronously.
With prefix argument SUBJECT, find the production class for a test instead.
Requires the bundles from `lsp-proxy-java-install-test' in JDTLS.
One match opens directly; multiple matches use completion."
  (interactive "P")
  (unless (and (bound-and-true-p lsp-proxy-mode) buffer-file-name
               (string-match-p "\\.java\\'" buffer-file-name)
               (not (lsp-proxy--decompiled-file-name-to-uri buffer-file-name)))
    (user-error "Run this from a Java source buffer with lsp-proxy-mode enabled"))
  (unless (file-exists-p buffer-file-name)
    (user-error "Save this Java file before looking for its tests"))
  (lsp-proxy--ensure-connection)
  (lsp-proxy--send-did-change)
  (unless lsp-proxy--buffer-opened (lsp-proxy--on-doc-open))
  (let* ((origin (current-buffer))
         (origin-position (point-marker))
         (file buffer-file-name)
         (connection lsp-proxy--connection)
         (generation (cl-incf lsp-proxy-java-test--generation))
         (uri (lsp-proxy--path-to-uri file)))
    (cl-labels ((active-p ()
                  (and (buffer-live-p origin) (eq connection lsp-proxy--connection)
                       (with-current-buffer origin
                         (and lsp-proxy-mode (equal file buffer-file-name)
                              (= generation lsp-proxy-java-test--generation))))))
      (jsonrpc-async-request
       connection 'emacs/forwardRequest
       (lsp-proxy--build-params
        (list :uri uri :serverName lsp-proxy-java-class-file-contents-server-name
              :method "workspace/executeCommand"
              :params (list :command "vscode.java.test.navigateToTestOrTarget"
                            :arguments (vector uri (if subject :json-false t)))))
       :success-fn
       (lambda (result)
         (when (active-p)
           (with-current-buffer origin
             (lsp-proxy-java-test--visit result subject origin-position))))
       :error-fn
       (lambda (error)
         (when (active-p)
           (lsp-proxy--error "Java test navigation failed: %s. Ensure Java Test Runner bundles are installed (M-x lsp-proxy-java-install-test) and restart lsp-proxy"
                             (or (plist-get error :message) error))))
       :timeout-fn
       (lambda ()
         (when (active-p) (lsp-proxy--error "Java test navigation timed out")))
       :timeout 60))))

;;;###autoload
(defun lsp-proxy-java-go-to-test-subject ()
  "Find the production class corresponding to the current Java test."
  (interactive)
  (lsp-proxy-java-go-to-test t))

(provide 'lsp-proxy-java-test)
;;; lsp-proxy-java-test.el ends here
