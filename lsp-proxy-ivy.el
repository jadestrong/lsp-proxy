;;; lsp-proxy-ivy.el --- Ivy workspace symbols for lsp-proxy -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1") (ivy "0.13.0"))
;; Keywords: languages, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Inspired by lsp-ivy's first-word query and local candidate filtering.
;; Uses lsp-proxy's transport and wrapped symbols, with no lsp-mode dependency.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'project)
(require 'jsonrpc)
(require 'ivy)
(require 'lsp-proxy-workspace-symbol)

(defun lsp-proxy-ivy--completing-read-action (prompt choices action caller)
  "Read CHOICES with PROMPT and retain ACTION for CALLER's Ivy session.
Return nil when Ivy is not the current completion frontend."
  (when (memq completing-read-function
              '(ivy-completing-read ivy-completing-read-with-empty-string-def))
    (ivy-read prompt choices :require-match t :sort nil
              :action action :caller caller)
    t))

(add-hook 'lsp-proxy--completion-read-functions
          #'lsp-proxy-ivy--completing-read-action)

(defgroup lsp-proxy-ivy nil
  "Ivy workspace symbol navigation through lsp-proxy."
  :group 'lsp-proxy)

(defcustom lsp-proxy-ivy-show-symbol-kind t
  "Whether candidates display the symbol kind."
  :type 'boolean :group 'lsp-proxy-ivy)

(defcustom lsp-proxy-ivy-show-symbol-filename t
  "Whether candidates display a project-relative source path or URI."
  :type 'boolean :group 'lsp-proxy-ivy)

(defcustom lsp-proxy-ivy-filter-symbol-kind nil
  "LSP SymbolKind numbers to exclude from the candidates."
  :type '(repeat integer) :group 'lsp-proxy-ivy)

(defcustom lsp-proxy-ivy-debounce-delay 0.2
  "Seconds to wait before sending a changed server query."
  :type 'number :group 'lsp-proxy-ivy)

(defcustom lsp-proxy-ivy-minimum-query-length 1
  "Minimum first-word length to query the server; zero allows empty queries."
  :type 'natnum :group 'lsp-proxy-ivy)

(defconst lsp-proxy-ivy--symbol-kinds
  ["Unknown" "File" "Module" "Namespace" "Package" "Class" "Method"
   "Property" "Field" "Constructor" "Enum" "Interface" "Function"
   "Variable" "Constant" "String" "Number" "Boolean" "Array" "Object"
   "Key" "Null" "EnumMember" "Struct" "Event" "Operator" "TypeParameter"])

(cl-defstruct (lsp-proxy-ivy--session (:constructor lsp-proxy-ivy--make-session))
  origin context root connection collection kill-hook
  query input items candidates timer request-id pending
  (generation 0) (active t))

(defun lsp-proxy-ivy--cancel (session)
  "Cancel SESSION's debounce timer and outstanding JSON-RPC request."
  (when-let* ((timer (lsp-proxy-ivy--session-timer session)))
    (cancel-timer timer)
    (setf (lsp-proxy-ivy--session-timer session) nil))
  (when-let* ((id (lsp-proxy-ivy--session-request-id session)))
    (setf (lsp-proxy-ivy--session-request-id session) nil)
    (let ((connection (lsp-proxy-ivy--session-connection session)))
      ;; Remove only this request's continuations, including its timeout timer.
      (jsonrpc--remove connection id)
      (when (jsonrpc-running-p connection)
        (ignore-errors
          (jsonrpc-notify connection '$/cancelRequest
                          (list :id id :uri
                                (plist-get (lsp-proxy-ivy--session-context session) :uri)))))))
  (setf (lsp-proxy-ivy--session-pending session) nil))

(defun lsp-proxy-ivy--cleanup (session)
  "Release SESSION's resources, including its source-buffer hook."
  (setf (lsp-proxy-ivy--session-active session) nil)
  (lsp-proxy-ivy--cancel session)
  (when (buffer-live-p (lsp-proxy-ivy--session-origin session))
    (with-current-buffer (lsp-proxy-ivy--session-origin session)
      (remove-hook 'kill-buffer-hook (lsp-proxy-ivy--session-kill-hook session) t))))

(defun lsp-proxy-ivy--current-p (session generation)
  "Whether GENERATION still belongs to a live SESSION."
  (and (lsp-proxy-ivy--session-active session)
       (buffer-live-p (lsp-proxy-ivy--session-origin session))
       (= generation (lsp-proxy-ivy--session-generation session))))

(defun lsp-proxy-ivy--label (item root)
  "Format wrapped symbol ITEM relative to ROOT without opening source files."
  (let* ((symbol (plist-get item :lsp_item))
         (kind (plist-get symbol :kind))
         (location (plist-get symbol :location))
         (uri (plist-get location :uri))
         (start (plist-get (plist-get location :range) :start))
         (container (plist-get symbol :containerName))
         (path (if (string-prefix-p "file:" uri)
                   (file-relative-name (lsp-proxy--uri-to-path uri) root)
                 uri)))
    (concat
     (when lsp-proxy-ivy-show-symbol-kind
       (propertize
        (format "[%s] " (if (and (integerp kind) (<= 0 kind)
                                 (< kind (length lsp-proxy-ivy--symbol-kinds)))
                            (aref lsp-proxy-ivy--symbol-kinds kind) "Unknown"))
        'face (if (memq kind '(6 9 12)) 'font-lock-function-name-face 'font-lock-type-face)))
     (plist-get symbol :name)
     (when (and container (not (equal container ""))) (format " (%s)" container))
     (when lsp-proxy-ivy-show-symbol-filename
       (propertize (concat " · " path (when start (format ":%d" (1+ (plist-get start :line)))))
                   'face 'font-lock-comment-face))
     (format " [%s]" (plist-get item :language_server_name)))))

(defun lsp-proxy-ivy--candidates (session input)
  "Format SESSION's cached items, filtering by words after the query in INPUT."
  (let ((filters (mapcar #'regexp-quote (cdr (split-string input))))
        (case-fold-search t)
        (index 0))
    (setf (lsp-proxy-ivy--session-candidates session)
          (delq nil
                (mapcar
                 (lambda (item)
                   (cl-incf index)
                   (unless (memq (plist-get (plist-get item :lsp_item) :kind)
                                 lsp-proxy-ivy-filter-symbol-kind)
                     (let ((label (lsp-proxy-ivy--label item (lsp-proxy-ivy--session-root session))))
                       (when (seq-every-p (lambda (filter) (string-match-p filter label)) filters)
                         (cons (propertize (format "[%d] %s" index label)
                                           'lsp-proxy-ivy-symbol item
                                           'lsp-proxy-ivy-generation
                                           (lsp-proxy-ivy--session-generation session)) item)))))
                 (seq-into (lsp-proxy-ivy--session-items session) 'list))))
    (mapcar #'car (lsp-proxy-ivy--session-candidates session))))

(defun lsp-proxy-ivy--publish (session)
  "Refresh only the active Ivy minibuffer belonging to SESSION."
  (when-let* ((window (active-minibuffer-window)))
    (when (eq (ivy-state-collection ivy-last) (lsp-proxy-ivy--session-collection session))
      (with-current-buffer (window-buffer window)
        (let ((input (ivy--input)))
          ;; Input can change before Ivy's post-command hook has run.
          (when (equal (or (car (split-string input)) "")
                       (lsp-proxy-ivy--session-query session))
            (ivy-update-candidates (lsp-proxy-ivy--candidates session input))
            t))))))

(defun lsp-proxy-ivy--failed (session generation message)
  "Clear current results and report MESSAGE if GENERATION is still current."
  (when (lsp-proxy-ivy--current-p session generation)
    (setf (lsp-proxy-ivy--session-pending session) nil
          (lsp-proxy-ivy--session-request-id session) nil
          (lsp-proxy-ivy--session-items session) nil
          (lsp-proxy-ivy--session-candidates session) nil)
    (when (lsp-proxy-ivy--publish session)
      (message "[lsp-proxy] Workspace symbol search: %s" message))))

(defun lsp-proxy-ivy--request (session generation query)
  "Send QUERY for SESSION after its debounce delay, retaining GENERATION."
  (when (lsp-proxy-ivy--current-p session generation)
    (setf (lsp-proxy-ivy--session-timer session) nil)
    (condition-case err
        (with-current-buffer (lsp-proxy-ivy--session-origin session)
          (unless (and (eq lsp-proxy--connection (lsp-proxy-ivy--session-connection session))
                       (jsonrpc-running-p lsp-proxy--connection)
                       lsp-proxy--has-any-servers)
            (user-error "Proxy connection changed or no server is available; search again"))
          (lsp-proxy--send-did-change)
          (setf (lsp-proxy-ivy--session-pending session) t)
          ;; Avoid the generic callback wrapper, which enters a dead source
          ;; buffer before user callbacks can check its lifetime.
          (let ((request
                 (jsonrpc-async-request
                  lsp-proxy--connection 'workspace/symbol
                  (plist-put (copy-sequence (lsp-proxy-ivy--session-context session))
                             :params (list :query query))
                  :success-fn
                  (lambda (items)
                    (when (lsp-proxy-ivy--current-p session generation)
                      (setf (lsp-proxy-ivy--session-pending session) nil
                            (lsp-proxy-ivy--session-request-id session) nil
                            (lsp-proxy-ivy--session-items session) items)
                      (lsp-proxy-ivy--publish session)))
                  :error-fn
                  (lambda (error)
                    (lsp-proxy-ivy--failed session generation
                                           (or (plist-get error :message) error)))
                  :timeout-fn
                  (lambda () (lsp-proxy-ivy--failed session generation "Request timed out")))))
            (when (lsp-proxy-ivy--session-pending session)
              (setf (lsp-proxy-ivy--session-request-id session) (car request)))))
      (error (lsp-proxy-ivy--failed session generation (error-message-string err))))))

(defun lsp-proxy-ivy--collection (session input)
  "Return dynamic candidates for INPUT, scheduling changed queries for SESSION."
  (unless (buffer-live-p (lsp-proxy-ivy--session-origin session))
    (user-error "The source buffer was closed; start a new search"))
  (unless (lsp-proxy-ivy--session-active session)
    ;; ivy-resume retains this collection and its unwind function.  Reactivate
    ;; only when Ivy is actually invoking this session, never from a callback.
    (unless (and ivy-last
                 (eq (ivy-state-collection ivy-last) (lsp-proxy-ivy--session-collection session)))
      (user-error "The workspace symbol session ended; start a new search"))
    (setf (lsp-proxy-ivy--session-active session) t
          (lsp-proxy-ivy--session-query session) nil)
    (when (lsp-proxy-ivy--session-kill-hook session)
      (with-current-buffer (lsp-proxy-ivy--session-origin session)
        (add-hook 'kill-buffer-hook (lsp-proxy-ivy--session-kill-hook session) nil t))))
  (let ((query (or (car (split-string input)) "")))
    (setf (lsp-proxy-ivy--session-input session) input)
    (unless (equal query (lsp-proxy-ivy--session-query session))
      (cl-incf (lsp-proxy-ivy--session-generation session))
      (lsp-proxy-ivy--cancel session)
      (setf (lsp-proxy-ivy--session-query session) query
            (lsp-proxy-ivy--session-items session) nil
            (lsp-proxy-ivy--session-candidates session) nil)
      (when (>= (length query) lsp-proxy-ivy-minimum-query-length)
        (setf (lsp-proxy-ivy--session-timer session)
              (run-with-timer (max 0 lsp-proxy-ivy-debounce-delay) nil
                              #'lsp-proxy-ivy--request session
                              (lsp-proxy-ivy--session-generation session) query))))
    ;; Ivy filters the initial collection before setting up the minibuffer;
    ;; an asynchronous sentinel (0) is not a sequence at that point.
    (unless (or (lsp-proxy-ivy--session-timer session) (lsp-proxy-ivy--session-pending session))
      (lsp-proxy-ivy--candidates session input))))

(defun lsp-proxy-ivy--action (session candidate)
  "Visit CANDIDATE using SESSION's original document and symbol origin."
  (unless (buffer-live-p (lsp-proxy-ivy--session-origin session))
    (user-error "The source buffer was closed; search again"))
  (let* ((generation (and (stringp candidate)
                          (get-text-property 0 'lsp-proxy-ivy-generation candidate)))
         (item (cdr (assoc candidate (lsp-proxy-ivy--session-candidates session)))))
    (unless (and item (or (null generation)
                         (= generation (lsp-proxy-ivy--session-generation session))))
      (user-error "The selected symbol is no longer current; search again"))
    (with-current-buffer (lsp-proxy-ivy--session-origin session)
      (let ((lsp-proxy-workspace-symbol--context (lsp-proxy-ivy--session-context session)))
        (lsp-proxy-workspace-symbol--jump item)))))

(defun lsp-proxy-ivy--read (initial-input)
  "Read workspace symbols with INITIAL-INPUT in an independent Ivy session."
  (unless (bound-and-true-p lsp-proxy-mode) (user-error "Enable lsp-proxy-mode first"))
  (lsp-proxy--ensure-connection)
  (unless lsp-proxy--buffer-opened (lsp-proxy--on-doc-open))
  (unless lsp-proxy--has-any-servers (user-error "No language servers are available yet"))
  (let* ((project (project-current nil))
         (session (lsp-proxy-ivy--make-session
                   :origin (current-buffer) :context (lsp-proxy--build-params nil)
                   :connection lsp-proxy--connection
                   :root (if project (project-root project) default-directory))))
    (setf (lsp-proxy-ivy--session-collection session)
          (lambda (input &rest arguments)
            ;; Completion metadata probes must not start or reset a query.
            (unless (eq (cadr arguments) 'metadata)
              (lsp-proxy-ivy--collection session input)))
          (lsp-proxy-ivy--session-kill-hook session)
          (lambda () (lsp-proxy-ivy--cleanup session)))
    (add-hook 'kill-buffer-hook (lsp-proxy-ivy--session-kill-hook session) nil t)
    (unwind-protect
        (ivy-read "Workspace symbol: " (lsp-proxy-ivy--session-collection session)
                  :dynamic-collection t :require-match t :sort nil
                  :initial-input initial-input
                  :action (lambda (candidate) (lsp-proxy-ivy--action session candidate))
                  :unwind (lambda () (lsp-proxy-ivy--cleanup session))
                  :caller 'lsp-proxy-ivy-workspace-symbol)
      (lsp-proxy-ivy--cleanup session))))

(defun lsp-proxy-ivy-workspace-symbol (arg)
  "Search current-project workspace symbols with Ivy.
With prefix ARG, start with the symbol at point.  The first input word is
sent to the server; further words filter its results locally.  Spring
prefixes @/, @+, @>, @ and // are supported."
  (interactive "P")
  (lsp-proxy-ivy--read (when arg (thing-at-point 'symbol t))))

(defun lsp-proxy-ivy-spring-running-endpoints ()
  "Search running Spring application endpoints with Ivy, starting with //."
  (interactive)
  (lsp-proxy-ivy--read "//"))

(provide 'lsp-proxy-ivy)
;;; lsp-proxy-ivy.el ends here
