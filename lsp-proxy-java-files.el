;;; lsp-proxy-java-files.el --- Production file navigation -*- lexical-binding: t; -*-

;;; Commentary:
;; JDTLS supplies source-root classification; the proxy scans those roots in a
;; background task.  Completion uses standard Emacs interfaces.

;;; Code:
(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'xref)
(require 'lsp-proxy-java-jdtls)

(defvar lsp-proxy-mode)

(defcustom lsp-proxy-java-file-extensions '("java")
  "File extensions offered by production file navigation, without dots.
Nil includes all files beneath production source roots, including resources."
  :type '(choice (const :tag "All source files and resources" nil)
                 (repeat string))
  :group 'lsp-proxy)

(defcustom lsp-proxy-java-file-include-generated t
  "Whether production file navigation includes generated sources.
JDTLS attributes and common generated-source paths identify generated roots."
  :type 'boolean :group 'lsp-proxy)

(defcustom lsp-proxy-java-file-cache-duration 30
  "Seconds to cache production file candidates.
Zero disables caching.  A prefix argument refreshes the current query."
  :type 'natnum :group 'lsp-proxy)

(defvar lsp-proxy-java-files--cache (make-hash-table :test 'equal)
  "Production file results indexed by connection, project and options.")
(defvar lsp-proxy-java-files--epoch 0
  "Revision of the production file cache.")
(defvar lsp-proxy-java-files--lifecycle 0
  "Revision of the proxy and Java workspace lifecycle.")
(defvar-local lsp-proxy-java-files--generation 0
  "Generation of the latest production file query in this buffer.")

;;;###autoload
(defun lsp-proxy-java-clear-file-cache (&optional cancel-pending)
  "Discard cached production file candidates and source roots.
With CANCEL-PENDING, also ignore outstanding queries and completion timers."
  (interactive)
  (clrhash lsp-proxy-java-files--cache)
  (cl-incf lsp-proxy-java-files--epoch)
  (when cancel-pending (cl-incf lsp-proxy-java-files--lifecycle)))

(defun lsp-proxy-java-files--after-save ()
  "Invalidate file candidates after a source or build file is saved."
  (when (and buffer-file-name
             (or (null lsp-proxy-java-file-extensions)
                 (member (file-name-extension buffer-file-name)
                         lsp-proxy-java-file-extensions)
                 (member (file-name-nondirectory buffer-file-name)
                         '("pom.xml" "build.gradle" "build.gradle.kts"
                           "settings.gradle" "settings.gradle.kts"
                           ".classpath" ".project"))))
    (lsp-proxy-java-clear-file-cache)))

(add-hook 'after-save-hook #'lsp-proxy-java-files--after-save)

(defun lsp-proxy-java-files--cache-get (key)
  "Return a fresh cached result for KEY, pruning expired connections/results."
  (let ((now (float-time)) expired)
    (maphash
     (lambda (candidate entry)
       (when (or (not (eq (car candidate) lsp-proxy--connection))
                 (>= (- now (car entry)) lsp-proxy-java-file-cache-duration))
         (push candidate expired)))
     lsp-proxy-java-files--cache)
    (dolist (candidate expired) (remhash candidate lsp-proxy-java-files--cache))
    (cdr (gethash key lsp-proxy-java-files--cache))))

(defun lsp-proxy-java-files--choose (result root origin-position)
  "Select a production file from RESULT under ROOT, saving ORIGIN-POSITION."
  (when (seq-some #'identity (append (plist-get result :fallbackModules) nil))
    (lsp-proxy--info "Some Java modules have no available project model; using conventional source directories"))
  (dolist (warning (append (plist-get result :warnings) nil))
    (lsp-proxy--warn "%s" warning))
  (let* ((default-directory root)
         (files (seq-filter #'stringp (append (plist-get result :files) nil)))
         (candidates (mapcar (lambda (file) (file-relative-name file root)) files))
         ;; Let file-aware completion styles and the user's completion frontend work.
         (table (lambda (string predicate action)
                  (if (eq action 'metadata)
                      '(metadata (category . file))
                    (complete-with-action action candidates string predicate))))
         (choice (and candidates
                      (condition-case nil
                          (lsp-proxy--completing-read "Find production file: " table)
                        (quit nil)))))
    (cond
     ((null candidates) (lsp-proxy--info "No production files found"))
     (choice
      (let ((target (expand-file-name choice root)))
        (if (not (file-exists-p target))
            (progn
              (lsp-proxy-java-clear-file-cache)
              (lsp-proxy--info "File no longer exists; run the search again to refresh candidates"))
          (save-excursion
            (save-restriction
              (widen)
              (when (marker-position origin-position) (goto-char origin-position))
              (xref-push-marker-stack)))
          (find-file target)))))))

;;;###autoload
(defun lsp-proxy-java-find-production-file (&optional refresh)
  "Find a production Java file using the Java project model asynchronously.
With prefix argument REFRESH, refresh cached roots and file candidates.
Uses project.el's project boundary and standard completion, without Projectile.
Unimported modules fall back to conventional source directories.  Configure
`lsp-proxy-java-file-extensions' to include resources or other source types.
Currently requires a local Java source buffer with lsp-proxy-mode enabled."
  (interactive "P")
  (unless (and (bound-and-true-p lsp-proxy-mode) buffer-file-name
               (string-equal (file-name-extension buffer-file-name) "java")
               (not (lsp-proxy--decompiled-file-name-to-uri buffer-file-name)))
    (user-error "Run this from a Java source buffer with lsp-proxy-mode enabled"))
  (when (file-remote-p buffer-file-name)
    (user-error "Production file navigation currently requires a local Java project"))
  (lsp-proxy--ensure-connection)
  (lsp-proxy--send-did-change)
  (unless lsp-proxy--buffer-opened (lsp-proxy--on-doc-open))
  (let* ((origin (current-buffer))
         (origin-position (point-marker))
         (file buffer-file-name)
         (project (project-current nil))
         (root (file-name-as-directory
                (file-truename
                 (or (and project (project-root project))
                     (car lsp-proxy--workspace-roots)
                     default-directory))))
         (connection lsp-proxy--connection)
         (server lsp-proxy-java-class-file-contents-server-name)
         (key (list connection root server lsp-proxy-java-file-extensions
                    lsp-proxy-java-file-include-generated))
         (generation (cl-incf lsp-proxy-java-files--generation))
         (epoch lsp-proxy-java-files--epoch)
         (lifecycle lsp-proxy-java-files--lifecycle)
         (cached (unless refresh (lsp-proxy-java-files--cache-get key))))
    (cl-labels ((active-p ()
                  (and (buffer-live-p origin) (eq connection lsp-proxy--connection)
                       (= lifecycle lsp-proxy-java-files--lifecycle)
                       (with-current-buffer origin
                         (and lsp-proxy-mode (equal file buffer-file-name)
                              (= generation lsp-proxy-java-files--generation)))))
                (show-result (result)
                  ;; Do not start a minibuffer from inside a process filter, or
                  ;; while the user is already completing an unrelated command.
                  (run-at-time
                   0 nil
                   (lambda ()
                     (when (active-p)
                       (if (active-minibuffer-window)
                           (run-at-time 0.2 nil #'show-result result)
                         (with-current-buffer origin
                           (lsp-proxy-java-files--choose result root origin-position))))))))
      (if cached
          (show-result cached)
        (when refresh (remhash key lsp-proxy-java-files--cache))
        (message "[lsp-proxy] Finding production files...")
        (jsonrpc-async-request
         connection 'emacs/javaProductionFiles
         (lsp-proxy--build-params
          (list :projectUri (lsp-proxy--path-to-uri root) :serverName server
                :extensions (if lsp-proxy-java-file-extensions
                                (vconcat lsp-proxy-java-file-extensions) nil)
                :includeGenerated (if lsp-proxy-java-file-include-generated t :json-false)))
         :success-fn
         (lambda (result)
           (when (active-p)
             (when (and (= epoch lsp-proxy-java-files--epoch)
                        (> lsp-proxy-java-file-cache-duration 0))
               (when (>= (hash-table-count lsp-proxy-java-files--cache) 32)
                 (clrhash lsp-proxy-java-files--cache))
               (puthash key (cons (float-time) result) lsp-proxy-java-files--cache))
             (show-result result)))
         :error-fn
         (lambda (error)
           (when (active-p)
             (lsp-proxy--error "Production file search failed: %s"
                               (or (plist-get error :message) error))))
         :timeout-fn
         (lambda ()
           (when (active-p) (lsp-proxy--error "Production file search timed out")))
         :timeout 120)))))

(provide 'lsp-proxy-java-files)
;;; lsp-proxy-java-files.el ends here
