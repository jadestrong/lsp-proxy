;;; test-ivy.el --- Ivy workspace symbol regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'lsp-proxy-ivy)
(defvar lsp-proxy-mode)

;; Exercise jsonrpc's real continuations, cancellation and response dispatch.
(defclass lsp-proxy-ivy-test--connection (jsonrpc-connection)
  ((sent :initform nil :accessor lsp-proxy-ivy-test--sent)))

(cl-defmethod jsonrpc-running-p ((_connection lsp-proxy-ivy-test--connection)) t)
(cl-defmethod jsonrpc-connection-send
  ((connection lsp-proxy-ivy-test--connection) &rest message)
  (push message (lsp-proxy-ivy-test--sent connection)))

(defun lsp-proxy-ivy-test--item (name &optional uri kind)
  (list :language_server_id 17 :language_server_name "spring-boot"
        :lsp_item (list :name name :kind (or kind 5) :data '(:token 42)
                        :location (list :uri (or uri "file:///project/src/App.java")))))

(defmacro lsp-proxy-ivy-test--with-session (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (let* ((lsp-proxy-mode t)
            (lsp-proxy--buffer-opened t)
            (lsp-proxy--has-any-servers t)
            (lsp-proxy--connection
             (make-instance 'lsp-proxy-ivy-test--connection :name "ivy-test"))
            (lsp-proxy-ivy-debounce-delay 60)
            (session (lsp-proxy-ivy--make-session
                      :origin (current-buffer) :connection lsp-proxy--connection
                      :root "/project/" :context '(:uri "file:///project/Origin.java"
                                                  :virtual-doc (:language "java") :params nil))))
       (setf (lsp-proxy-ivy--session-collection session)
             (lambda (input &rest _) (lsp-proxy-ivy--collection session input)))
       (cl-letf (((symbol-function 'lsp-proxy--send-did-change) #'ignore))
         (unwind-protect (progn ,@body)
           (lsp-proxy-ivy--cleanup session))))))

(defun lsp-proxy-ivy-test--flush (session)
  (cancel-timer (lsp-proxy-ivy--session-timer session))
  (lsp-proxy-ivy--request session (lsp-proxy-ivy--session-generation session)
                          (lsp-proxy-ivy--session-query session)))

(defun lsp-proxy-ivy-test--reply (session items)
  (with-temp-buffer
    (jsonrpc-connection-receive
     (lsp-proxy-ivy--session-connection session)
     (list :jsonrpc "2.0" :id (lsp-proxy-ivy--session-request-id session) :result items))))

(ert-deftest lsp-proxy-ivy-test-candidates-retain-duplicates-data-and-literal-filters ()
  (lsp-proxy-ivy-test--with-session
    (let* ((one (lsp-proxy-ivy-test--item "@+ bean[1]"))
           (two (copy-tree one)))
      (setf (lsp-proxy-ivy--session-items session)
            (vector one two (lsp-proxy-ivy-test--item "bean1")
                    (lsp-proxy-ivy-test--item "method" nil 6)))
      (cl-letf (((symbol-function 'find-file-noselect) (lambda (&rest _) (ert-fail "eager file open"))))
        (let ((candidates (lsp-proxy-ivy--candidates session "@+ bean[1]")))
          (should (= (length candidates) 2))
          (should-not (equal (car candidates) (cadr candidates)))
          (should (eq (get-text-property 0 'lsp-proxy-ivy-symbol (car candidates)) one))
          (should (eq (get-text-property 0 'lsp-proxy-ivy-symbol (cadr candidates)) two))
          (should (string-match-p "src/App.java" (car candidates)))))
      (let ((lsp-proxy-ivy-filter-symbol-kind '(6))
            (lsp-proxy-ivy-show-symbol-kind nil)
            (lsp-proxy-ivy-show-symbol-filename nil))
        (let ((candidates (lsp-proxy-ivy--candidates session "bean")))
          (should (= (length candidates) 3))
          (should-not (string-match-p "src/App.java" (car candidates)))
          (should-not (string-match-p "Class" (car candidates))))))))

(ert-deftest lsp-proxy-ivy-test-debounce-and-empty-input ()
  (lsp-proxy-ivy-test--with-session
    (should-not (lsp-proxy-ivy--collection session ""))
    (should-not (lsp-proxy-ivy--session-timer session))
    (should-not (lsp-proxy-ivy--collection session "@"))
    (let ((timer (lsp-proxy-ivy--session-timer session)))
      (should-not (lsp-proxy-ivy--collection session "@/"))
      (should-not (memq timer timer-list))
      (should-not (eq timer (lsp-proxy-ivy--session-timer session))))
    (should-not (lsp-proxy-ivy-test--sent lsp-proxy--connection))
    (let ((lsp-proxy-ivy-minimum-query-length 0))
      (should-not (lsp-proxy-ivy--collection session ""))
      (should (lsp-proxy-ivy--session-timer session)))))

(ert-deftest lsp-proxy-ivy-test-rpc-keeps-envelope-and-reuses-query-for-local-filter ()
  (lsp-proxy-ivy-test--with-session
    (lsp-proxy-ivy--collection session "@/ api[1]")
    (lsp-proxy-ivy-test--flush session)
    (let* ((message (car (lsp-proxy-ivy-test--sent lsp-proxy--connection)))
           (params (plist-get message :params)))
      (should (eq (plist-get message :method) 'workspace/symbol))
      (should (equal (plist-get params :uri) "file:///project/Origin.java"))
      (should (equal (plist-get params :virtual-doc) '(:language "java")))
      (should (equal (plist-get params :params) '(:query "@/"))))
    (lsp-proxy-ivy-test--reply session
                             (vector (lsp-proxy-ivy-test--item "@/ api[1]")
                                     (lsp-proxy-ivy-test--item "@/ other")))
    (should (= (length (lsp-proxy-ivy--collection session "@/ api[1]")) 1))
    (should (= (length (lsp-proxy-ivy--collection session "@/ other")) 1))
    (should (= (length (lsp-proxy-ivy-test--sent lsp-proxy--connection)) 1))))

(ert-deftest lsp-proxy-ivy-test-cancel-and-discard-late-responses ()
  (lsp-proxy-ivy-test--with-session
    (lsp-proxy-ivy--collection session "old")
    (lsp-proxy-ivy-test--flush session)
    (let* ((old-id (lsp-proxy-ivy--session-request-id session))
           (callback (nth 2 (assq old-id (jsonrpc--continuations lsp-proxy--connection)))))
      (lsp-proxy-ivy--collection session "new")
      (should-not (assq old-id (jsonrpc--continuations lsp-proxy--connection)))
      (let ((cancel (car (lsp-proxy-ivy-test--sent lsp-proxy--connection))))
        (should (eq (plist-get cancel :method) '$/cancelRequest))
        (should (equal (plist-get cancel :params)
                       (list :id old-id :uri "file:///project/Origin.java"))))
      (funcall callback (vector (lsp-proxy-ivy-test--item "old")))
      (should-not (lsp-proxy-ivy--session-items session))
      (lsp-proxy-ivy-test--flush session)
      (lsp-proxy-ivy-test--reply session (vector (lsp-proxy-ivy-test--item "new")))
      (should (string-match-p "new" (car (lsp-proxy-ivy--collection session "new")))))))

(ert-deftest lsp-proxy-ivy-test-publish-isolates-nested-ivy-and-checks-raw-input ()
  (lsp-proxy-ivy-test--with-session
    (setf (lsp-proxy-ivy--session-query session) "@+"
          (lsp-proxy-ivy--session-items session)
          (vector (lsp-proxy-ivy-test--item "@+ alpha") (lsp-proxy-ivy-test--item "@+ beta")))
    (let ((ivy-last (make-ivy-state :collection #'ignore))
          (input "@+ alpha") updated update-buffer)
      (cl-letf (((symbol-function 'active-minibuffer-window) (lambda () (selected-window)))
                ((symbol-function 'ivy--input) (lambda () input))
                ((symbol-function 'ivy-update-candidates)
                 (lambda (candidates) (setq updated candidates update-buffer (current-buffer)))))
        (should-not (lsp-proxy-ivy--publish session))
        (should-not updated)
        (setf (ivy-state-collection ivy-last) (lsp-proxy-ivy--session-collection session))
        (setq input "@> alpha")
        (should-not (lsp-proxy-ivy--publish session))
        (setq input "@+ beta")
        (should (lsp-proxy-ivy--publish session))
        (should (= (length updated) 1))
        (should (eq update-buffer (window-buffer (selected-window))))))))

(ert-deftest lsp-proxy-ivy-test-cleanup-cancels-timers-and-callbacks ()
  (lsp-proxy-ivy-test--with-session
    (lsp-proxy-ivy--collection session "bean")
    (let ((timer (lsp-proxy-ivy--session-timer session)))
      (lsp-proxy-ivy--cleanup session)
      (should-not (memq timer timer-list)))
    (setf (lsp-proxy-ivy--session-active session) t)
    (lsp-proxy-ivy--collection session "other")
    (lsp-proxy-ivy-test--flush session)
    (let ((callback (nth 2 (car (jsonrpc--continuations lsp-proxy--connection)))))
      (lsp-proxy-ivy--cleanup session)
      (funcall callback (vector (lsp-proxy-ivy-test--item "other")))
      (should-not (lsp-proxy-ivy--session-items session))
      (should-not (jsonrpc--continuations lsp-proxy--connection))
      (should-not (lsp-proxy-ivy--session-request-id session)))))

(ert-deftest lsp-proxy-ivy-test-stale-debounce-callback-keeps-new-timer ()
  (lsp-proxy-ivy-test--with-session
    (lsp-proxy-ivy--collection session "old")
    (let ((generation (lsp-proxy-ivy--session-generation session)))
      (lsp-proxy-ivy--collection session "new")
      (let ((timer (lsp-proxy-ivy--session-timer session)))
        (lsp-proxy-ivy--request session generation "old")
        (should (eq timer (lsp-proxy-ivy--session-timer session)))
        (should-not (lsp-proxy-ivy-test--sent lsp-proxy--connection))))))

(ert-deftest lsp-proxy-ivy-test-resumed-session-refreshes-and-cleans-up ()
  (lsp-proxy-ivy-test--with-session
    (setf (lsp-proxy-ivy--session-query session) "bean")
    (lsp-proxy-ivy--cleanup session)
    (let ((ivy-last (make-ivy-state :collection (lsp-proxy-ivy--session-collection session))))
      (lsp-proxy-ivy--collection session "bean")
      (should (lsp-proxy-ivy--session-active session))
      (should (lsp-proxy-ivy--session-timer session))
      (lsp-proxy-ivy-test--flush session)
      (lsp-proxy-ivy-test--reply session (vector (lsp-proxy-ivy-test--item "bean")))
      (should (= (length (lsp-proxy-ivy--collection session "bean")) 1))
      (lsp-proxy-ivy--cleanup session)
      (should-not (jsonrpc--continuations lsp-proxy--connection)))))

(ert-deftest lsp-proxy-ivy-test-errors-and-jsonrpc-timeout-clear-results ()
  (lsp-proxy-ivy-test--with-session
    (lsp-proxy-ivy--collection session "bad")
    (lsp-proxy-ivy-test--flush session)
    (let (failure)
      (cl-letf (((symbol-function 'lsp-proxy-ivy--publish) (lambda (_) t))
                ((symbol-function 'message) (lambda (_format &rest args) (setq failure (car args)))))
        (jsonrpc-connection-receive lsp-proxy--connection
                                   (list :id (lsp-proxy-ivy--session-request-id session)
                                         :error '(:code -32001 :message "server failed")))
        (should (equal failure "server failed"))
        (should-not (lsp-proxy-ivy--session-pending session))
        (lsp-proxy-ivy--collection session "timeout")
        (lsp-proxy-ivy-test--flush session)
        (let ((timer (nth 4 (car (jsonrpc--continuations lsp-proxy--connection)))))
          (apply (timer--function timer) (timer--args timer)))
        (should (equal failure "Request timed out"))
        (should-not (lsp-proxy-ivy--session-pending session))
        (should-not (jsonrpc--continuations lsp-proxy--connection))))))

(ert-deftest lsp-proxy-ivy-test-action-keeps-origin-and-rejects-stale-selection ()
  (lsp-proxy-ivy-test--with-session
    (let ((item (lsp-proxy-ivy-test--item "bean")) visited)
      (setf (lsp-proxy-ivy--session-items session) (vector item))
      (let ((candidate (car (lsp-proxy-ivy--candidates session "bean"))))
        (cl-letf (((symbol-function 'lsp-proxy-workspace-symbol--jump)
                   (lambda (symbol &rest _)
                     (setq visited (list symbol (current-buffer) lsp-proxy-workspace-symbol--context)))))
          (with-temp-buffer (lsp-proxy-ivy--action session candidate))
          (should (eq (car visited) item))
          (should (eq (cadr visited) (lsp-proxy-ivy--session-origin session)))
          (should (equal (caddr visited) (lsp-proxy-ivy--session-context session)))
          (cl-incf (lsp-proxy-ivy--session-generation session))
          (should-error (lsp-proxy-ivy--action session candidate) :type 'user-error))))))

(ert-deftest lsp-proxy-ivy-test-actual-ivy-read-actions-run-after-cleanup ()
  (lsp-proxy-ivy-test--with-session
    (let ((constructor (symbol-function 'lsp-proxy-ivy--make-session)) opened active-session)
      (cl-letf (((symbol-function 'lsp-proxy-ivy--make-session)
                 (lambda (&rest args) (setq active-session (apply constructor args))))
                ((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'lsp-proxy--ensure-connection) #'ignore)
                ((symbol-function 'lsp-proxy--build-params)
                 (lambda (&rest _) (lsp-proxy-ivy--session-context session)))
                ((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _)
                   ;; Ivy's real reset-state, cleanup and action are exercised;
                   ;; replace only the interactive keyboard input in batch.
                   (progn
                     (lsp-proxy-ivy-test--flush active-session)
                     (lsp-proxy-ivy-test--reply
                      active-session (vector (lsp-proxy-ivy-test--item
                                              "http://127.0.0.1:8080/one"
                                              "http://127.0.0.1:8080/one")))
                     (setq ivy--all-candidates (lsp-proxy-ivy--candidates active-session "//")
                           ivy-text "//" ivy-exit 'done)
                     (setf (ivy-state-current ivy-last) (car ivy--all-candidates))
                     "//")))
                ((symbol-function 'browse-url) (lambda (uri &rest _) (setq opened uri)))
                ((symbol-function 'find-file-noselect) (lambda (&rest _) (ert-fail "URL opened as file"))))
        (lsp-proxy-ivy-spring-running-endpoints)
        (should (equal opened "http://127.0.0.1:8080/one"))
        (should-not (lsp-proxy-ivy--session-active active-session))
        (should-not (lsp-proxy-ivy--session-timer active-session))
        (should-not (memq (lsp-proxy-ivy--session-kill-hook active-session) kill-buffer-hook))
        (should-not (featurep 'lsp-mode))
        (should-not (featurep 'lsp-protocol))
        (should-not (featurep 'lsp-ivy))))))

(ert-deftest lsp-proxy-ivy-test-quit-and-source-buffer-kill-clean-up ()
  (lsp-proxy-ivy-test--with-session
    (let ((constructor (symbol-function 'lsp-proxy-ivy--make-session)) active-session)
      (cl-letf (((symbol-function 'lsp-proxy-ivy--make-session)
                 (lambda (&rest args) (setq active-session (apply constructor args))))
                ((symbol-function 'project-current) (lambda (&rest _) nil))
                ((symbol-function 'lsp-proxy--ensure-connection) #'ignore)
                ((symbol-function 'lsp-proxy--build-params)
                 (lambda (&rest _) (lsp-proxy-ivy--session-context session)))
                ((symbol-function 'ivy-read)
                 (lambda (_prompt collection &rest _)
                   (funcall collection "bean")
                   (lsp-proxy-ivy-test--flush active-session)
                   (signal 'quit nil))))
        (condition-case nil (lsp-proxy-ivy-workspace-symbol nil) (quit nil))
        (should-not (lsp-proxy-ivy--session-active active-session))
        (should-not (jsonrpc--continuations lsp-proxy--connection)))
      (let ((origin (generate-new-buffer " *ivy-dead-origin*")))
        (unwind-protect
            (progn
              (setf (lsp-proxy-ivy--session-origin session) origin
                    (lsp-proxy-ivy--session-kill-hook session)
                    (lambda () (lsp-proxy-ivy--cleanup session)))
              (with-current-buffer origin
                (add-hook 'kill-buffer-hook (lsp-proxy-ivy--session-kill-hook session) nil t))
              (lsp-proxy-ivy--collection session "waiting")
              (kill-buffer origin)
              (should-not (lsp-proxy-ivy--session-active session))
              (should-not (lsp-proxy-ivy--session-timer session))
              (should-error (lsp-proxy-ivy--action session "any") :type 'user-error))
          (when (buffer-live-p origin) (kill-buffer origin)))))))

;;; test-ivy.el ends here
