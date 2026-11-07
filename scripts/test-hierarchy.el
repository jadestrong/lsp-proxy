;;; test-hierarchy.el --- Asynchronous hierarchy regression tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'lsp-proxy-hierarchy)

(defvar lsp-proxy-mode nil)

(defclass lsp-proxy-hierarchy-test--connection (jsonrpc-connection)
  ((sent :initform nil :accessor lsp-proxy-hierarchy-test--sent)))

(cl-defmethod jsonrpc-running-p ((_connection lsp-proxy-hierarchy-test--connection)) t)
(cl-defmethod jsonrpc-connection-send
  ((connection lsp-proxy-hierarchy-test--connection) &rest message)
  (push message (lsp-proxy-hierarchy-test--sent connection)))

(defun lsp-proxy-hierarchy-test--item (&optional name uri)
  (list :name (or name "Bean") :kind 5 :uri (or uri "file:///w/Bean.java")
        :range '(:start (:line 0 :character 0) :end (:line 9 :character 0))
        :selectionRange '(:start (:line 1 :character 2) :end (:line 1 :character 6))
        :data '(:owner "jdtls" :opaque [1 (:x "y")])))

(defun lsp-proxy-hierarchy-test--node (&optional id name uri)
  (list :lsp_item (lsp-proxy-hierarchy-test--item name uri)
        :language_server_id (or id 2) :language_server_name "jdtls"))

(defmacro lsp-proxy-hierarchy-test--with-peer (&rest body)
  (declare (indent 0))
  `(let (requests removed cancelled)
     (cl-letf (((symbol-function 'jsonrpc-request)
                (lambda (&rest _) (ert-fail "Synchronous hierarchy request")))
               ((symbol-function 'jsonrpc-async-request)
                (lambda (connection method params &rest callbacks)
                  (let ((id (1+ (length requests))))
                    (setq requests (append requests
                                           (list (append (list :id id :connection connection
                                                               :method method :params params)
                                                         callbacks))))
                    (list id nil))))
               ((symbol-function 'jsonrpc-running-p) (lambda (_) t))
               ((symbol-function 'jsonrpc--remove)
                (lambda (connection id &rest _) (push (cons connection id) removed)))
               ((symbol-function 'jsonrpc-notify)
                (lambda (_connection method params) (push (cons method params) cancelled)))
               ((symbol-function 'lsp-proxy--ensure-connection) #'ignore)
               ((symbol-function 'lsp-proxy--send-did-change) #'ignore)
               ((symbol-function 'lsp-proxy--build-params)
                (lambda (params) (list :uri "file:///w/Source.java" :params params)))
               ((symbol-function 'lsp-proxy--TextDocumentPositionParams)
                (lambda () '(:textDocument (:uri "file:///w/Source.java")
                                           :position (:line 2 :character 1)))))
       ,@body)))

(defmacro lsp-proxy-hierarchy-test--with-tree (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (lsp-proxy-hierarchy-mode)
     (let ((lsp-proxy--connection 'connection))
       (setq-local lsp-proxy-hierarchy--connection 'connection
                   lsp-proxy-hierarchy--context
                   '(:uri "file:///w/Source.java" :params nil
                          :virtual-doc (:language "java" :line-bias 3))
                   lsp-proxy-hierarchy--specs lsp-proxy-hierarchy--call-specs
                   lsp-proxy-hierarchy--kind "call"
                   lsp-proxy-hierarchy--preparer 'textDocument/prepareCallHierarchy
                   lsp-proxy-hierarchy--roots (list (lsp-proxy-hierarchy-test--node)))
       ,@body)))

(defmacro lsp-proxy-hierarchy-test--with-source (&rest body)
  (declare (indent 0))
  `(let ((source (generate-new-buffer " *hierarchy-source*")))
     (unwind-protect
         (with-current-buffer source
           (setq-local buffer-file-name "/w/Source.java"
                       default-directory (concat (make-temp-name "/w/hierarchy-test-") "/")
                       lsp-proxy--support-call-hierarchy t lsp-proxy--support-type-hierarchy t)
           (let ((lsp-proxy-mode t) (lsp-proxy--connection 'connection)
                 (lsp-proxy--has-any-servers t) (lsp-proxy--buffer-opened t) views)
             (unwind-protect
                 (cl-letf (((symbol-function 'pop-to-buffer)
                            (lambda (buffer &rest _) (push buffer views))))
                   ,@body)
               (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b))) (delete-dups views)))))
       (when (buffer-live-p source) (kill-buffer source)))))

(defun lsp-proxy-hierarchy-test--reply (request value &optional kind)
  "Deliver a delayed reply, even if REQUEST was cancelled."
  (if (eq kind :timeout-fn)
      (funcall (plist-get request kind))
    (funcall (plist-get request (or kind :success-fn)) value)))

(defun lsp-proxy-hierarchy-test--edge (request &optional name)
  (let* ((method (plist-get request :method))
         (node (lsp-proxy-hierarchy-test--node 2 (or name "Child"))))
    (if (memq method '(callHierarchy/incomingCalls callHierarchy/outgoingCalls))
        (plist-put node :lsp_item
                   (list (if (eq method 'callHierarchy/incomingCalls) :from :to)
                         (plist-get node :lsp_item)
                         :fromRanges [(:start (:line 3 :character 5) :end (:line 3 :character 8))]))
      node)
    (vector node)))

(defun lsp-proxy-hierarchy-test--drain ()
  "Run only the view's scheduled widget updates, without waiting on I/O."
  (let ((limit 20))
    (while lsp-proxy-hierarchy--timers
      (should (> (cl-decf limit) 0))
      (let ((timer (car lsp-proxy-hierarchy--timers)))
        (cancel-timer timer)
        (apply (timer--function timer) (timer--args timer))))))

(ert-deftest lsp-proxy-hierarchy-test-real-jsonrpc-continuations-and-cancellation ()
  (lsp-proxy-hierarchy-test--with-tree
   (let* ((connection (make-instance 'lsp-proxy-hierarchy-test--connection :name "hierarchy-test"))
          (lsp-proxy--connection connection) received)
     (setq lsp-proxy-hierarchy--connection connection)
     (unwind-protect
         (progn
           (lsp-proxy-hierarchy--request
            'callHierarchy/incomingCalls (lsp-proxy-hierarchy-test--node 7)
            (lambda (result) (setq received (cons (current-buffer) result))) #'ignore)
           (let* ((message (car (lsp-proxy-hierarchy-test--sent connection)))
                  (id (plist-get message :id))
                  (params (plist-get message :params))
                  (buffer (current-buffer)))
             (should (= (length (jsonrpc--continuations connection)) 1))
             (should (equal (plist-get params :virtual-doc) '(:language "java" :line-bias 3)))
             (should (= (plist-get (plist-get params :context) :language-server-id) 7))
             (with-temp-buffer
               (jsonrpc-connection-receive connection
                                           (list :jsonrpc "2.0" :id id :result [])))
             (should (eq (car received) buffer))
             (should (= (length (jsonrpc--continuations connection)) 0)))
           (lsp-proxy-hierarchy--request
            'callHierarchy/outgoingCalls (lsp-proxy-hierarchy-test--node) #'ignore #'ignore)
           (lsp-proxy-hierarchy--invalidate)
           (should (= (length (jsonrpc--continuations connection)) 0))
           (should (eq (plist-get (car (lsp-proxy-hierarchy-test--sent connection)) :method)
                       '$/cancelRequest)))
       (lsp-proxy-hierarchy--stop)))))

(ert-deftest lsp-proxy-hierarchy-test-prepare-and-directions-are-asynchronous ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-source
    (dolist (command '(lsp-proxy-show-call-hierarchy lsp-proxy-show-type-hierarchy))
      (let ((offset (length requests)))
        (funcall command)
        (let ((view (car views)) (prepare (nth offset requests)))
          (with-current-buffer view
            (should (string-match-p "Loading hierarchy" (buffer-string)))
            (should-not lsp-proxy-hierarchy--roots))
          (should (= (length requests) (1+ offset)))
          (lsp-proxy-hierarchy-test--reply prepare (vector (lsp-proxy-hierarchy-test--node 7)))
          ;; Both directions are sent before either completes.
          (should (= (length requests) (+ offset 3)))
          (let ((first (nth (+ offset 1) requests)) (second (nth (+ offset 2) requests)))
            (dolist (request (list first second))
              (let ((params (plist-get request :params)))
                (should (equal (plist-get params :uri) "file:///w/Source.java"))
                (should (= (plist-get (plist-get params :context) :language-server-id) 7))
                (should (equal (plist-get (plist-get params :params) :item)
                               (lsp-proxy-hierarchy-test--item)))))
            (lsp-proxy-hierarchy-test--reply second (lsp-proxy-hierarchy-test--edge second "Second"))
            (with-current-buffer view
              (should (string-match-p "Bean" (buffer-string)))
              (should (string-match-p "Loading" (buffer-string))))
            (lsp-proxy-hierarchy-test--reply first (lsp-proxy-hierarchy-test--edge first "First"))
            (with-current-buffer view
              (lsp-proxy-hierarchy-test--drain)
              (should (string-match-p "First" (buffer-string)))
              (should (string-match-p "Second" (buffer-string)))
              (should (< (string-match "First" (buffer-string))
                         (string-match "Second" (buffer-string))))
              (should-not (string-match-p "Loading" (buffer-string)))))))))))

(ert-deftest lsp-proxy-hierarchy-test-cache-sharing-origin-and-empty-results ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (let* ((node (lsp-proxy-hierarchy-test--node)) (original (copy-tree node)) a b)
      (lsp-proxy-hierarchy--children node (lambda (result) (setq a result)))
      (lsp-proxy-hierarchy--children node (lambda (result) (setq b result)))
      (should (= (length requests) 2))
      (lsp-proxy-hierarchy-test--reply (car requests) (lsp-proxy-hierarchy-test--edge (car requests)))
      (lsp-proxy-hierarchy-test--reply (cadr requests) nil)
      (should (equal a b))
      (should (= (length a) 1))
      (should (equal (plist-get (plist-get (car a) :lsp_item) :data)
                     (plist-get (lsp-proxy-hierarchy-test--item) :data)))
      (should (equal (plist-get (car a) :call-sites)
                     [(:start (:line 3 :character 5) :end (:line 3 :character 8))]))
      (lsp-proxy-hierarchy--children node (lambda (result) (should (equal result a))))
      (should (= (length requests) 2))
      (lsp-proxy-hierarchy--children (lsp-proxy-hierarchy-test--node 3) #'ignore)
      (should (= (length requests) 4))
      (should (equal original node))))))

(ert-deftest lsp-proxy-hierarchy-test-empty-node-does-not-loop-or-refetch ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (lsp-proxy-hierarchy--render)
    (dolist (request requests) (lsp-proxy-hierarchy-test--reply request nil))
    (lsp-proxy-hierarchy-test--drain)
    (let ((root (car lsp-proxy-hierarchy--widgets)) (inhibit-read-only t))
      (widget-apply-action root)
      (widget-apply-action root)
      (should-not lsp-proxy-hierarchy--timers)
      (should (= (length requests) 2))
      (should (string-match-p "Bean" (buffer-string)))
      (should-not (string-match-p "Loading" (buffer-string)))))))

(ert-deftest lsp-proxy-hierarchy-test-refresh-cancels-and-ignores-old-replies ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (lsp-proxy-hierarchy--render)
    (let ((old (copy-sequence requests)))
      (lsp-proxy-hierarchy-refresh)
      (should (= (length removed) 2))
      (should (= (length cancelled) 2))
      (should (eq (caar cancelled) '$/cancelRequest))
      (should (equal (plist-get (cdar cancelled) :uri) "file:///w/Source.java"))
      (dolist (request old) (lsp-proxy-hierarchy-test--reply request (lsp-proxy-hierarchy-test--edge request "Old")))
      (should (= (hash-table-count lsp-proxy-hierarchy--cache) 0))
      (lsp-proxy-hierarchy-test--reply (nth 2 requests) (lsp-proxy-hierarchy-test--edge (nth 2 requests) "New"))
      (lsp-proxy-hierarchy-test--reply (nth 3 requests) nil :timeout-fn)
      (lsp-proxy-hierarchy-test--drain)
      (should (string-match-p "New" (buffer-string)))
      (should (string-match-p "timed out.*g to retry" (buffer-string)))
      (should-not (string-match-p "Old" (buffer-string)))
      (should (= (hash-table-count lsp-proxy-hierarchy--cache) 1))
      (lsp-proxy-hierarchy-refresh)
      (dolist (request (last requests 2)) (lsp-proxy-hierarchy-test--reply request nil))
      (lsp-proxy-hierarchy-test--drain)
      (should-not (string-match-p "timed out" (buffer-string)))))))

(ert-deftest lsp-proxy-hierarchy-test-refresh-invalidates-queued-widget-update ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (lsp-proxy-hierarchy--render)
    (dolist (request requests)
      (lsp-proxy-hierarchy-test--reply request (lsp-proxy-hierarchy-test--edge request "Old")))
    (let ((timer (car lsp-proxy-hierarchy--timers)))
      (should timer)
      (lsp-proxy-hierarchy-refresh)
      (should-not lsp-proxy-hierarchy--timers)
      (apply (timer--function timer) (timer--args timer))
      (should-not (string-match-p "Old" (buffer-string)))
      (should (= (hash-table-count lsp-proxy-hierarchy--cache) 0)))
    (dolist (request (last requests 2)) (lsp-proxy-hierarchy-test--reply request nil))
    (lsp-proxy-hierarchy-test--drain))))

(ert-deftest lsp-proxy-hierarchy-test-reopen-and-connection-change ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-source
    (lsp-proxy-show-call-hierarchy)
    (let ((old (car requests)))
      (setq lsp-proxy--connection 'replacement)
      (lsp-proxy-show-call-hierarchy)
      (should (= (length removed) 1))
      (lsp-proxy-hierarchy-test--reply old (vector (lsp-proxy-hierarchy-test--node 2 "Old")))
      (with-current-buffer (car views) (should-not lsp-proxy-hierarchy--roots))
      (lsp-proxy-hierarchy-test--reply (nth 1 requests) (vector (lsp-proxy-hierarchy-test--node 7 "New")))
      (with-current-buffer (car views)
        (should (eq lsp-proxy-hierarchy--connection 'replacement))
        (should (string-match-p "New" (buffer-string))))
      ;; A connection replacement without reopening also invalidates callbacks.
      (setq lsp-proxy--connection 'third)
      (dolist (request (last requests 2)) (lsp-proxy-hierarchy-test--reply request nil))
      (with-current-buffer (car views)
        (should (= (hash-table-count lsp-proxy-hierarchy--cache) 0))
        (let (children)
          (lsp-proxy-hierarchy--children
           (car lsp-proxy-hierarchy--roots) (lambda (result) (setq children result)))
          (should (= (length children) 2))
          (should (string-match-p "reopen" (plist-get (car children) :error)))))))))

(ert-deftest lsp-proxy-hierarchy-test-kill-source-view-and-quit-cancel ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-source
    (lsp-proxy-show-call-hierarchy)
    (let ((view (car views)) (request (car requests)))
      (with-current-buffer view
        (cl-letf (((symbol-function 'quit-window) #'ignore)) (lsp-proxy-hierarchy-quit)))
      (should (= (length removed) 1))
      (lsp-proxy-hierarchy-test--reply request (vector (lsp-proxy-hierarchy-test--node)))
      (with-current-buffer view (should-not lsp-proxy-hierarchy--roots))
      (lsp-proxy-show-call-hierarchy)
      (let ((request (car (last requests))))
        (kill-buffer view)
        (lsp-proxy-hierarchy-test--reply request nil))
      (lsp-proxy-show-call-hierarchy)
      (let ((request (car (last requests))) (view (car views)))
        (kill-buffer (current-buffer))
        (lsp-proxy-hierarchy-test--reply request nil)
        (with-current-buffer view
          (should-not lsp-proxy-hierarchy--active)
          (should (= (hash-table-count lsp-proxy-hierarchy--pending) 0))))))))

(ert-deftest lsp-proxy-hierarchy-test-parent-collapse-during-child-request ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (setq lsp-proxy-hierarchy--specs (list (car lsp-proxy-hierarchy--call-specs)))
    (lsp-proxy-hierarchy--render)
    (lsp-proxy-hierarchy-test--reply (car requests) (lsp-proxy-hierarchy-test--edge (car requests)))
    (lsp-proxy-hierarchy-test--drain)
    (let* ((root (car lsp-proxy-hierarchy--widgets))
           (child (cl-find-if #'tree-widget-p (widget-get root :children)))
           (inhibit-read-only t))
      (widget-apply-action child)
      (should (= (length requests) 2))
      (widget-apply-action root)
      (should-not (marker-buffer (widget-get child :from)))
      (lsp-proxy-hierarchy-test--reply (nth 1 requests) (lsp-proxy-hierarchy-test--edge (nth 1 requests) "Grandchild"))
      (lsp-proxy-hierarchy-test--drain)
      (should-not (widget-get root :open))
      (widget-apply-action root)
      (should (string-match-p "Grandchild" (buffer-string)))
      (should-not (string-match-p "Loading" (buffer-string)))
      (should (= (length requests) 2))))))

(ert-deftest lsp-proxy-hierarchy-test-center-cancels-child-work ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (setq lsp-proxy-hierarchy--specs (list (car lsp-proxy-hierarchy--call-specs)))
    (lsp-proxy-hierarchy--render)
    (lsp-proxy-hierarchy-test--reply (car requests) (lsp-proxy-hierarchy-test--edge (car requests)))
    (lsp-proxy-hierarchy-test--drain)
    (let ((child (cl-find-if #'tree-widget-p (widget-get (car lsp-proxy-hierarchy--widgets) :children)))
          (inhibit-read-only t))
      (widget-apply-action child))
    (goto-char (point-min)) (search-forward "Child") (backward-char)
    (lsp-proxy-hierarchy-center-on-node)
    (should (= (length cancelled) 1))
    (should-not (plist-get (car lsp-proxy-hierarchy--roots) :call-sites))
    (lsp-proxy-hierarchy-test--reply (nth 1 requests) nil)
    (should (= (hash-table-count lsp-proxy-hierarchy--pending) 1))
    (lsp-proxy-hierarchy-test--reply (nth 2 requests) nil)
    (lsp-proxy-hierarchy-test--drain)
    (should-not (string-match-p "Bean" (buffer-string))))))

(ert-deftest lsp-proxy-hierarchy-test-dead-widgets-and-failure-recovery ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (lsp-proxy-hierarchy--render)
    (lsp-proxy-hierarchy-test--reply (car requests) '(:message "Failed") :error-fn)
    (lsp-proxy-hierarchy-test--reply (cadr requests) (lsp-proxy-hierarchy-test--edge (cadr requests)))
    (lsp-proxy-hierarchy-test--drain)
    (should (string-match-p "Failed.*g to retry" (buffer-string)))
    (should (string-match-p "Child" (buffer-string)))
    (let ((inhibit-read-only t)) (widget-delete (car lsp-proxy-hierarchy--widgets)))
    (lsp-proxy-hierarchy-refresh)
    (dolist (request (last requests 2)) (lsp-proxy-hierarchy-test--reply request nil))
    (lsp-proxy-hierarchy-test--drain)
    (should (string-match-p "Bean" (buffer-string)))
    (should-not (string-match-p "Failed" (buffer-string))))))

(ert-deftest lsp-proxy-hierarchy-test-prepare-empty-error-timeout-and-refresh ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-source
    (lsp-proxy-show-type-hierarchy)
    (lsp-proxy-hierarchy-test--reply (car requests) [])
    (with-current-buffer (car views)
      (should (string-match-p "No type hierarchy" (buffer-string)))
      (lsp-proxy-hierarchy-refresh))
    (lsp-proxy-hierarchy-test--reply (nth 1 requests) '(:message "Prepare failed") :error-fn)
    (with-current-buffer (car views)
      (should (string-match-p "Prepare failed" (buffer-string)))
      (lsp-proxy-hierarchy-refresh))
    (lsp-proxy-hierarchy-test--reply (nth 2 requests) nil :timeout-fn)
    (with-current-buffer (car views)
      (should (string-match-p "timed out" (buffer-string)))
      (lsp-proxy-hierarchy-refresh))
    (lsp-proxy-hierarchy-test--reply (nth 3 requests) (vector (lsp-proxy-hierarchy-test--node)))
    (with-current-buffer (car views) (should (string-match-p "Bean" (buffer-string)))))))

(defun lsp-proxy-hierarchy-test--enable-user-evil (state)
  "Install explicit user bindings in a buffer-local copy and enter STATE."
  (let ((map (copy-keymap lsp-proxy-hierarchy-mode-map)))
    (evil-define-key* '(normal motion) map
      (kbd "RET") #'lsp-proxy-hierarchy-visit
      (kbd "<return>") #'lsp-proxy-hierarchy-visit
      (kbd "e") #'lsp-proxy-hierarchy-toggle
      (kbd "u") #'lsp-proxy-hierarchy-collapse-parent
      (kbd "d") #'lsp-proxy-hierarchy-set-direction
      (kbd "TAB") #'widget-forward
      (kbd "<backtab>") #'widget-backward
      (kbd "c") #'lsp-proxy-hierarchy-center-on-node
      (kbd "g") #'lsp-proxy-hierarchy-refresh
      (kbd "q") #'lsp-proxy-hierarchy-quit)
    (use-local-map map)
    (evil-local-mode 1)
    (evil-change-state state)
    (evil-normalize-keymaps)))

(ert-deftest lsp-proxy-hierarchy-test-default-map-has-no-evil-state-bindings ()
  (dolist (state '(normal-state motion-state))
    (should (eq (lookup-key lsp-proxy-hierarchy-mode-map (vector state))
                (lookup-key (keymap-parent lsp-proxy-hierarchy-mode-map) (vector state))))))

(ert-deftest lsp-proxy-hierarchy-test-user-evil-bindings-are-local ()
  (skip-unless (featurep 'evil))
  (with-temp-buffer
    (lsp-proxy-hierarchy-mode)
    (lsp-proxy-hierarchy-test--enable-user-evil 'normal)
    (should-not (eq (current-local-map) lsp-proxy-hierarchy-mode-map))
    (dolist (entry '(("RET" . lsp-proxy-hierarchy-visit)
                     ("d" . lsp-proxy-hierarchy-set-direction)
                     ("c" . lsp-proxy-hierarchy-center-on-node)
                     ("g" . lsp-proxy-hierarchy-refresh)
                     ("q" . lsp-proxy-hierarchy-quit)))
      (should (eq (key-binding (kbd (car entry))) (cdr entry)))))
  (dolist (state '(normal-state motion-state))
    (should (eq (lookup-key lsp-proxy-hierarchy-mode-map (vector state))
                (lookup-key (keymap-parent lsp-proxy-hierarchy-mode-map) (vector state))))))

(defun lsp-proxy-hierarchy-test--visit-every-column (&optional state)
  "Exercise real RET events on labels, indentation, icons and suffixes."
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (lsp-proxy-hierarchy--render)
    (dolist (request requests)
      (lsp-proxy-hierarchy-test--reply request (lsp-proxy-hierarchy-test--edge request)))
    (lsp-proxy-hierarchy-test--drain)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (when state (lsp-proxy-hierarchy-test--enable-user-evil state))
      (let (clicked)
        (cl-letf (((symbol-function 'lsp-proxy-hierarchy--jump)
                   (lambda (node) (setq clicked node))))
          (dolist (name '("Bean" "Child"))
            (goto-char (point-min))
            (search-forward name)
            (let* ((label (match-beginning 0))
                   (start (line-beginning-position))
                   (end (line-end-position))
                   (icon (save-excursion
                           (goto-char start) (search-forward "[" end)
                           (1- (point)))))
              (dolist (position (list start icon label (1- end)))
                (dolist (key '("RET" "<return>"))
                  (goto-char position)
                  (setq clicked nil)
                  (should (eq (key-binding (kbd key)) #'lsp-proxy-hierarchy-visit))
                  (execute-kbd-macro (kbd key))
                  (should (equal (plist-get (plist-get clicked :lsp_item) :name) name))))))))))))

(ert-deftest lsp-proxy-hierarchy-test-ret-visits-the-whole-node-line ()
  (lsp-proxy-hierarchy-test--visit-every-column))

(ert-deftest lsp-proxy-hierarchy-test-ret-in-evil-states ()
  (skip-unless (featurep 'evil))
  (dolist (state '(normal motion emacs))
    (lsp-proxy-hierarchy-test--visit-every-column state)))

(ert-deftest lsp-proxy-hierarchy-test-icon-mouse-action-still-toggles ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (lsp-proxy-hierarchy--render)
    (let ((root (car lsp-proxy-hierarchy--widgets)))
      (goto-char (point-min))
      (widget-button-press (point))
      (should-not (widget-get root :open))
      (widget-button-press (point))
      (should (widget-get root :open))))))

(defun lsp-proxy-hierarchy-test--toggle-with-keyboard (&optional state)
  "Toggle root/child rows with actual keys, including during async loading."
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (lsp-proxy-hierarchy--render)
    (dolist (request requests)
      (lsp-proxy-hierarchy-test--reply request (lsp-proxy-hierarchy-test--edge request)))
    (lsp-proxy-hierarchy-test--drain)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (when state (lsp-proxy-hierarchy-test--enable-user-evil state))
      (let ((root (car lsp-proxy-hierarchy--widgets)))
        (dolist (where '(indent icon name suffix))
          (goto-char (point-min))
          (pcase where
            ('icon (search-forward "[") (backward-char))
            ('name (search-forward "Bean") (backward-char))
            ('suffix (end-of-line) (backward-char)))
          (should (eq (key-binding (kbd "e")) #'lsp-proxy-hierarchy-toggle))
          (execute-kbd-macro (kbd "e"))
          (should-not (widget-get root :open))
          (execute-kbd-macro (kbd "e"))
          (should (widget-get root :open)))
        (should (= (length requests) 2))
        (goto-char (point-min)) (search-forward "Child") (beginning-of-line)
        (let ((child (cl-find-if #'tree-widget-p (widget-get root :children))))
          (execute-kbd-macro (kbd "e"))
          (should (widget-get child :open))
          (should (= (length requests) 4))
          (should (string-match-p "Loading" (buffer-string)))
          (execute-kbd-macro (kbd "e"))
          (should-not (widget-get child :open))
          (dolist (request (last requests 2))
            (lsp-proxy-hierarchy-test--reply request (lsp-proxy-hierarchy-test--edge request "Grandchild")))
          (lsp-proxy-hierarchy-test--drain)
          (should-not (widget-get child :open))
          (execute-kbd-macro (kbd "e"))
          (should (widget-get child :open))
          (should (string-match-p "Grandchild" (buffer-string)))
          (should (= (length requests) 4))
          (should (eq (key-binding (kbd "RET")) #'lsp-proxy-hierarchy-visit))))))))

(ert-deftest lsp-proxy-hierarchy-test-e-toggle-and-async-caching ()
  (lsp-proxy-hierarchy-test--toggle-with-keyboard))

(ert-deftest lsp-proxy-hierarchy-test-e-toggle-in-evil-states ()
  (skip-unless (featurep 'evil))
  (dolist (state '(normal motion emacs))
    (lsp-proxy-hierarchy-test--toggle-with-keyboard state)))

(defun lsp-proxy-hierarchy-test--collapse-parent-with-keyboard (&optional state)
  "Collapse ancestors from every column, preserving sibling trees and caches."
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (push (lsp-proxy-hierarchy-test--node 2 "OtherRoot") lsp-proxy-hierarchy--roots)
    (lsp-proxy-hierarchy--render)
    (dolist (request requests)
      (lsp-proxy-hierarchy-test--reply
       request (lsp-proxy-hierarchy-test--edge
                request (if (eq (plist-get request :method) 'callHierarchy/incomingCalls)
                            "Child" "Sibling"))))
    (lsp-proxy-hierarchy-test--drain)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (when state (lsp-proxy-hierarchy-test--enable-user-evil state))
      (goto-char (point-min)) (search-forward "Bean")
      (let* ((root (lsp-proxy-hierarchy--tree-on-line))
             (other (cl-find-if (lambda (tree) (not (eq tree root)))
                                lsp-proxy-hierarchy--widgets)))
        (dolist (where '(indent icon name suffix))
          (goto-char (widget-get root :from)) (search-forward "Child")
          (beginning-of-line)
          (let* ((child (lsp-proxy-hierarchy--tree-on-line))
                 (sibling (car (last (widget-get root :children)))))
            (let ((count (length requests)))
              (unless (widget-get child :open) (execute-kbd-macro (kbd "e")))
              (dolist (request (nthcdr count requests))
                (lsp-proxy-hierarchy-test--reply
                 request (lsp-proxy-hierarchy-test--edge request "Grandchild"))))
            (lsp-proxy-hierarchy-test--drain)
            (search-forward "Grandchild") (beginning-of-line)
            (pcase where
              ('icon (search-forward "[") (backward-char))
              ('name (search-forward "Grandchild") (backward-char))
              ('suffix (end-of-line) (backward-char)))
            (should (eq (key-binding (kbd "u")) #'lsp-proxy-hierarchy-collapse-parent))
            (let ((count (length requests)))
              (execute-kbd-macro (kbd "u"))
              (should-not (widget-get child :open))
              (should (eq (lsp-proxy-hierarchy--tree-on-line) child))
              (should (= (point) (line-beginning-position)))
              (should (eq sibling (car (last (widget-get root :children)))))
              (should (widget-get root :open))
              (should (widget-get other :open))
              (should-not (widget-get sibling :open))
              (execute-kbd-macro (kbd "e"))
              (should (widget-get child :open))
              (should (= (length requests) count))
              (should (string-match-p "Grandchild" (buffer-string))))
            ;; Repeating u climbs to this root, leaving the other root alone.
            (execute-kbd-macro (kbd "u"))
            (should-not (widget-get root :open))
            (should (eq (lsp-proxy-hierarchy--tree-on-line) root))
            (should (widget-get other :open))
            (execute-kbd-macro (kbd "e"))
            (should (widget-get root :open))))
        (should (eq (key-binding (kbd "RET")) #'lsp-proxy-hierarchy-visit))
        (should (eq (key-binding (kbd "TAB")) #'widget-forward)))))))

(ert-deftest lsp-proxy-hierarchy-test-u-collapses-parents-on-the-whole-line ()
  (lsp-proxy-hierarchy-test--collapse-parent-with-keyboard))

(ert-deftest lsp-proxy-hierarchy-test-u-collapse-parent-in-evil-states ()
  (skip-unless (featurep 'evil))
  (dolist (state '(normal motion emacs))
    (lsp-proxy-hierarchy-test--collapse-parent-with-keyboard state)))

(ert-deftest lsp-proxy-hierarchy-test-collapse-parent-during-loading-and-root-boundary ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (lsp-proxy-hierarchy--render)
    (goto-char (point-min))
    (let ((root (lsp-proxy-hierarchy--tree-on-line)))
      (should-error (lsp-proxy-hierarchy-collapse-parent) :type 'user-error)
      (should (widget-get root :open))
      (dolist (request requests)
        (lsp-proxy-hierarchy-test--reply request (lsp-proxy-hierarchy-test--edge request)))
      (lsp-proxy-hierarchy-test--drain)
      (goto-char (point-min)) (search-forward "Child")
      (lsp-proxy-hierarchy-toggle)
      (should (string-match-p "Loading" (buffer-string)))
      (lsp-proxy-hierarchy-collapse-parent)
      (let ((collapsed (buffer-string)))
        (dolist (request (last requests 2))
          (lsp-proxy-hierarchy-test--reply
           request (lsp-proxy-hierarchy-test--edge request "Grandchild")))
        (lsp-proxy-hierarchy-test--drain)
        (should (equal collapsed (buffer-string))))
      (lsp-proxy-hierarchy-toggle)
      (lsp-proxy-hierarchy-test--drain)
      (should (string-match-p "Grandchild" (buffer-string)))
      (should (= (length requests) 4))
      (should (widget-get root :open))))))

(ert-deftest lsp-proxy-hierarchy-test-command-directions-and-capabilities ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-source
    (dolist (entry '((lsp-proxy-show-call-hierarchy nil callHierarchy/incomingCalls 2)
                     (lsp-proxy-show-call-hierarchy base callHierarchy/outgoingCalls 1)
                     (lsp-proxy-show-type-hierarchy derived typeHierarchy/supertypes 1)
                     (lsp-proxy-show-type-hierarchy subtypes typeHierarchy/subtypes 1)))
      (funcall (nth 0 entry) (nth 1 entry))
      (with-current-buffer (car views)
        (should (eq (caar lsp-proxy-hierarchy--specs) (nth 2 entry)))
        (should (= (length lsp-proxy-hierarchy--specs) (nth 3 entry)))))
    (should-error (lsp-proxy-show-type-hierarchy 'invalid) :type 'user-error)
    (setq lsp-proxy--support-call-hierarchy nil)
    (should-error (lsp-proxy-show-call-hierarchy) :type 'user-error))))

(defun lsp-proxy-hierarchy-test--direction-with-keyboard (kind &optional state)
  "Select all KIND directions with real d events at each position on a row."
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (when (equal kind "type")
      (setq lsp-proxy-hierarchy--kind kind
            lsp-proxy-hierarchy--specs lsp-proxy-hierarchy--type-specs))
    (lsp-proxy-hierarchy--render)
    (dolist (request requests)
      (lsp-proxy-hierarchy-test--reply request (lsp-proxy-hierarchy-test--edge request)))
    (lsp-proxy-hierarchy-test--drain)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (when state (lsp-proxy-hierarchy-test--enable-user-evil state))
      (let ((roots lsp-proxy-hierarchy--roots)
            (context lsp-proxy-hierarchy--context)
            (choices (cons "both" (mapcar #'cl-fourth (lsp-proxy-hierarchy--all-specs)))))
        (dolist (where '(indent icon name suffix))
          (dolist (choice (append (cdr choices) '("both")))
            (goto-char (point-min))
            (pcase where
              ('icon (search-forward "[") (backward-char))
              ('name (search-forward "Bean") (backward-char))
              ('suffix (end-of-line) (backward-char)))
            (should (eq (key-binding (kbd "d")) #'lsp-proxy-hierarchy-set-direction))
            (let ((default (lsp-proxy-hierarchy--direction-label lsp-proxy-hierarchy--specs)))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_prompt collection &optional _predicate require-match
                                          _initial _history initial-default &rest _)
                           (should (equal collection choices))
                           (should require-match)
                           (should (equal initial-default default))
                           choice)))
                (execute-kbd-macro (kbd "d"))))
            (should (eq roots lsp-proxy-hierarchy--roots))
            (should (eq context lsp-proxy-hierarchy--context))
            (should (equal (lsp-proxy-hierarchy--direction-label lsp-proxy-hierarchy--specs) choice))
            (should (equal (eval (cadr header-line-format))
                           (concat "Direction: " choice " | d: change direction")))
            (should (= (length requests) 2))
            (let ((children (seq-filter #'tree-widget-p
                                        (widget-get (car lsp-proxy-hierarchy--widgets) :children))))
              (should (= (length children) (if (equal choice "both") 2 1)))
              (should (cl-every (lambda (child) (not (widget-get child :open))) children)))
            (should (eq (key-binding (kbd "RET")) #'lsp-proxy-hierarchy-visit))
            (should (eq (key-binding (kbd "e")) #'lsp-proxy-hierarchy-toggle))
            (should (eq (key-binding (kbd "u")) #'lsp-proxy-hierarchy-collapse-parent)))))))))

(ert-deftest lsp-proxy-hierarchy-test-d-direction-on-the-whole-line ()
  (dolist (kind '("call" "type"))
    (lsp-proxy-hierarchy-test--direction-with-keyboard kind)))

(ert-deftest lsp-proxy-hierarchy-test-d-direction-in-evil-states ()
  (skip-unless (featurep 'evil))
  (dolist (state '(normal motion emacs))
    (dolist (kind '("call" "type"))
      (lsp-proxy-hierarchy-test--direction-with-keyboard kind state))))

(ert-deftest lsp-proxy-hierarchy-test-direction-cancel-and-same-direction ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (lsp-proxy-hierarchy--render)
    (let ((before (buffer-string))
          (generation lsp-proxy-hierarchy--generation)
          (widgets lsp-proxy-hierarchy--widgets))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (signal 'quit nil))))
        (should (eq (condition-case nil (call-interactively #'lsp-proxy-hierarchy-set-direction)
                      (quit :quit)) :quit)))
      (lsp-proxy-hierarchy-set-direction t)
      (should (equal before (buffer-string)))
      (should (eq generation lsp-proxy-hierarchy--generation))
      (should (eq widgets lsp-proxy-hierarchy--widgets))
      (should (= (length requests) 2))
      (should-not removed)))))

(ert-deftest lsp-proxy-hierarchy-test-direction-during-prepare ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-source
    (lsp-proxy-show-call-hierarchy)
    (with-current-buffer (car views)
      (let ((old (car requests)) (context lsp-proxy-hierarchy--context))
        (lsp-proxy-hierarchy-set-direction 'outgoing)
        (should (eq context lsp-proxy-hierarchy--context))
        (should (= (length requests) 2))
        (should (= (length removed) 1))
        (should (eq (plist-get (cadr requests) :method) 'textDocument/prepareCallHierarchy))
        (lsp-proxy-hierarchy-test--reply old (vector (lsp-proxy-hierarchy-test--node 2 "Stale")))
        (should-not lsp-proxy-hierarchy--roots)
        (lsp-proxy-hierarchy-test--reply (cadr requests) (vector (lsp-proxy-hierarchy-test--node)))
        (should (= (length requests) 3))
        (should (eq (plist-get (nth 2 requests) :method) 'callHierarchy/outgoingCalls))
        (should-not (string-match-p "Stale" (buffer-string))))))))

(ert-deftest lsp-proxy-hierarchy-test-direction-cancels-old-child-work ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (lsp-proxy-hierarchy--render)
    (dolist (request requests)
      (lsp-proxy-hierarchy-test--reply request (lsp-proxy-hierarchy-test--edge request)))
    (lsp-proxy-hierarchy-test--drain)
    (search-forward "Child") (lsp-proxy-hierarchy-toggle)
    (let ((old (last requests 2)))
      (lsp-proxy-hierarchy-set-direction 'outgoing)
      (should (= (length removed) 2))
      (should (= (length cancelled) 2))
      (let ((before (buffer-string)))
        (dolist (request old)
          (lsp-proxy-hierarchy-test--reply request (lsp-proxy-hierarchy-test--edge request "Stale")))
        (lsp-proxy-hierarchy-test--drain)
        (should (equal before (buffer-string))))
      (lsp-proxy-hierarchy-set-direction 'incoming)
      (search-forward "Child") (lsp-proxy-hierarchy-toggle)
      (should (= (length requests) 5))
      (lsp-proxy-hierarchy-test--reply (car (last requests))
                                     (lsp-proxy-hierarchy-test--edge (car (last requests)) "New"))
      (lsp-proxy-hierarchy-test--drain)
      (should (string-match-p "New" (buffer-string)))
      (should-not (string-match-p "Stale" (buffer-string)))))))

(ert-deftest lsp-proxy-hierarchy-test-direction-invalidates-queued-update-and-keeps-centered-root ()
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (lsp-proxy-hierarchy--render)
    (dolist (request requests)
      (lsp-proxy-hierarchy-test--reply request (lsp-proxy-hierarchy-test--edge request)))
    (lsp-proxy-hierarchy-test--drain)
    (search-forward "Child") (backward-char)
    (lsp-proxy-hierarchy-center-on-node)
    (let ((roots lsp-proxy-hierarchy--roots))
      (dolist (request (last requests 2))
        (lsp-proxy-hierarchy-test--reply request (lsp-proxy-hierarchy-test--edge request "Grandchild")))
      (let ((timer (car lsp-proxy-hierarchy--timers)))
        (should timer)
        (lsp-proxy-hierarchy-set-direction 'outgoing)
        (should-not lsp-proxy-hierarchy--timers)
        (should (eq roots lsp-proxy-hierarchy--roots))
        (should (= (length requests) 4))
        (let ((before (buffer-string)))
          (apply (timer--function timer) (timer--args timer))
          (should (equal before (buffer-string)))))))))

(ert-deftest lsp-proxy-hierarchy-test-direction-rejects-invalid-state ()
  (with-temp-buffer
    (should-error (lsp-proxy-hierarchy-set-direction 'incoming) :type 'user-error))
  (lsp-proxy-hierarchy-test--with-peer
   (lsp-proxy-hierarchy-test--with-tree
    (let ((specs lsp-proxy-hierarchy--specs)
          (generation lsp-proxy-hierarchy--generation)
          (source (generate-new-buffer "hierarchy-closed-source")))
      (kill-buffer source)
      (should-error (lsp-proxy-hierarchy-set-direction 'subtypes) :type 'user-error)
      (let ((lsp-proxy-hierarchy--source source))
        (should-error (lsp-proxy-hierarchy-set-direction 'incoming) :type 'user-error))
      (let ((lsp-proxy--connection 'new-connection))
        (should-error (lsp-proxy-hierarchy-set-direction 'incoming) :type 'user-error))
      (should (eq specs lsp-proxy-hierarchy--specs))
      (should (eq generation lsp-proxy-hierarchy--generation))
      (should-not requests)))))

(ert-deftest lsp-proxy-hierarchy-test-call-site-file-and-selection-range ()
  (lsp-proxy-hierarchy-test--with-tree
   (let* ((node (lsp-proxy-hierarchy-test--node)) visited position
          (target (generate-new-buffer " *hierarchy-test-target*")))
     (unwind-protect
         (cl-letf (((symbol-function 'lsp-proxy--uri-to-path) #'identity)
                   ((symbol-function 'find-file-noselect)
                    (lambda (path &rest _) (setq visited path) target))
                   ((symbol-function 'pop-to-buffer) (lambda (&rest _) nil))
                   ((symbol-function 'xref-push-marker-stack) #'ignore)
                   ((symbol-function 'eglot--lsp-position-to-point)
                    (lambda (pos) (setq position pos) (point-min))))
           (setq node (append node '(:method callHierarchy/outgoingCalls
                                             :parent-uri "file:///w/Caller.java"
                                             :call-sites [(:start (:line 3 :character 5)
                                                                  :end (:line 3 :character 8))])))
           (lsp-proxy-hierarchy--jump node)
           (should (equal visited "file:///w/Caller.java"))
           (should (equal position '(:line 3 :character 5)))
           (plist-put node :method 'callHierarchy/incomingCalls)
           (lsp-proxy-hierarchy--jump node)
           (should (equal visited "file:///w/Bean.java"))
           (plist-put node :method 'callHierarchy/outgoingCalls)
           (plist-put node :call-sites [])
           (lsp-proxy-hierarchy--jump node)
           (should (equal visited "file:///w/Bean.java"))
           (should (equal position '(:line 1 :character 2))))
       (kill-buffer target)))))

(ert-deftest lsp-proxy-hierarchy-test-eldoc-and-button-navigation ()
  (lsp-proxy-hierarchy-test--with-tree
   (let* ((node (lsp-proxy-hierarchy-test--node)) (clicked nil)
          (inhibit-read-only t))
     (insert (lsp-proxy-hierarchy--label node))
     (goto-char (point-min))
     (search-forward "Bean") (backward-char)
     (should (string-match-p "Bean.java:2:3" (lsp-proxy-hierarchy--eldoc nil)))
     (cl-letf (((symbol-function 'lsp-proxy-hierarchy--jump)
                (lambda (value) (setq clicked value))))
       (button-activate (button-at (point)))
       (should (equal clicked node))))))

(ert-deftest lsp-proxy-hierarchy-test-capability-notifications ()
  (with-temp-buffer
    (let ((target (current-buffer)))
      (cl-letf (((symbol-function 'file-exists-p) (lambda (_) t))
                ((symbol-function 'find-file-noselect) (lambda (&rest _) target))
                ((symbol-function 'lsp-proxy--uri-to-path) #'identity)
                ((symbol-function 'lsp-proxy--confirm-project-activation) #'ignore)
                ((symbol-function 'lsp-proxy-activate-inlay-hints-mode) #'ignore)
                ((symbol-function 'lsp-proxy-activate-codelens-mode) #'ignore)
                ((symbol-function 'lsp-proxy-diagnostics--request-pull-diagnostics) #'ignore)
                ((symbol-function 'lsp-proxy-inline-completion-mode) #'ignore))
        (dolist (flags '((t :json-false) (:json-false t) (nil nil)))
          (lsp-proxy--handle-notification
           nil 'emacs/serverCapabilities
           (list :uri "file:///w/Bean.java" :hasAnyServers t
                 :supportCallHierarchy (car flags) :supportTypeHierarchy (cadr flags)))
          (should (eq lsp-proxy--support-call-hierarchy (eq (car flags) t)))
          (should (eq lsp-proxy--support-type-hierarchy (eq (cadr flags) t))))))))

(ert-deftest lsp-proxy-hierarchy-test-org-position-keeps-raw-item ()
  (lsp-proxy-hierarchy-test--with-tree
   (let* ((item (lsp-proxy-hierarchy-test--item))
          (position (plist-get (plist-get item :selectionRange) :start)))
     (should (equal (lsp-proxy-hierarchy--source-position "file:///w/Source.java" position)
                    '(:line 4 :character 2)))
     (should (equal (lsp-proxy-hierarchy--source-position "file:///w/Other.java" position)
                    '(:line 1 :character 2)))
     (should (equal position '(:line 1 :character 2))))))

;;; test-hierarchy.el ends here
