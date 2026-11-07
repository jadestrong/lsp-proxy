;;; lsp-proxy-hierarchy.el --- Call and type hierarchies -*- lexical-binding: t; -*-

;;; Commentary:
;; Based on Eglot's hierarchy viewer: bidirectional lazy tree widgets, call-site
;; navigation and re-centering.  Proxy wrappers retain each item's server; UI
;; metadata stays outside the raw LSP item and its opaque data.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'button)
(require 'tree-widget)
(require 'eldoc)
(require 'xref)
(require 'eglot)
(require 'lsp-proxy-core)
(require 'lsp-proxy-utils)

(defvar lsp-proxy-mode)

(defvar-local lsp-proxy-hierarchy--roots nil)
(defvar-local lsp-proxy-hierarchy--specs nil)
(defvar-local lsp-proxy-hierarchy--context nil)
(defvar-local lsp-proxy-hierarchy--connection nil)
(defvar-local lsp-proxy-hierarchy--cache nil)
(defvar-local lsp-proxy-hierarchy--widgets nil)
(defvar-local lsp-proxy-hierarchy--pending nil)
(defvar-local lsp-proxy-hierarchy--generation nil)
(defvar-local lsp-proxy-hierarchy--active nil)
(defvar-local lsp-proxy-hierarchy--timers nil)
(defvar-local lsp-proxy-hierarchy--source nil)
(defvar-local lsp-proxy-hierarchy--source-kill-hook nil)
(defvar-local lsp-proxy-hierarchy--preparer nil)
(defvar-local lsp-proxy-hierarchy--kind nil)
(defvar-local lsp-proxy-hierarchy--status nil)

(cl-defstruct (lsp-proxy-hierarchy--pending-request
               (:constructor lsp-proxy-hierarchy--make-request))
  id callbacks)

(defconst lsp-proxy-hierarchy--call-specs
  '((callHierarchy/incomingCalls " ← " incoming "incoming calls" "called by" :from)
    (callHierarchy/outgoingCalls " → " outgoing "outgoing calls" "calls" :to)))
(defconst lsp-proxy-hierarchy--type-specs
  '((typeHierarchy/supertypes " ↑ " supertypes "supertypes" "derives from")
    (typeHierarchy/subtypes " ↓ " subtypes "subtypes" "base of")))

(defun lsp-proxy-hierarchy--read-direction (specs &optional default)
  "Read a direction from SPECS, initially selecting DEFAULT or both."
  (let ((answer (completing-read
                 "Hierarchy direction: "
                 (cons "both" (mapcar #'cl-fourth specs)) nil t nil nil
                 (or default "both"))))
    (if (equal answer "both") t
      (cl-third (cl-find answer specs :key #'cl-fourth :test #'equal)))))

(defun lsp-proxy-hierarchy--interactive (specs)
  "Read a direction from SPECS when invoked with a prefix argument."
  (list (if current-prefix-arg (lsp-proxy-hierarchy--read-direction specs) t)))

(defun lsp-proxy-hierarchy--direction-specs (specs direction)
  "Filter SPECS to DIRECTION, accepting nil or t for both."
  (if (memq direction '(nil t)) specs
    (list (or (cl-find direction specs :key #'cl-third)
              (user-error "Invalid hierarchy direction: %s" direction)))))

(defun lsp-proxy-hierarchy--direction-label (specs)
  "Return the display label for the selected SPECS."
  (if (cdr specs) "both" (or (cl-fourth (car specs)) "both")))

(defun lsp-proxy-hierarchy--all-specs ()
  "Return all directions for the current hierarchy view."
  (unless (derived-mode-p 'lsp-proxy-hierarchy-mode)
    (user-error "Open a hierarchy view first"))
  (pcase lsp-proxy-hierarchy--kind
    ("call" lsp-proxy-hierarchy--call-specs)
    ("type" lsp-proxy-hierarchy--type-specs)
    (_ (user-error "Reopen the hierarchy from a source buffer first"))))

;;;###autoload
(defun lsp-proxy-show-call-hierarchy (&optional direction)
  "Show call hierarchy for the symbol at point.
DIRECTION is `incoming', `outgoing' or t (both, the default).
The Eglot direction `base' is also accepted for outgoing calls.
With a prefix argument, prompt for DIRECTION."
  (interactive (lsp-proxy-hierarchy--interactive lsp-proxy-hierarchy--call-specs))
  (lsp-proxy-hierarchy--show
   "call" lsp-proxy--support-call-hierarchy 'textDocument/prepareCallHierarchy
   lsp-proxy-hierarchy--call-specs (if (eq direction 'base) 'outgoing direction)))

;;;###autoload
(defun lsp-proxy-show-type-hierarchy (&optional direction)
  "Show type hierarchy for the symbol at point.
DIRECTION is `supertypes', `subtypes' or t (both, the default).
The Eglot directions `derived' and `base' are accepted as aliases.
With a prefix argument, prompt for DIRECTION."
  (interactive (lsp-proxy-hierarchy--interactive lsp-proxy-hierarchy--type-specs))
  (lsp-proxy-hierarchy--show
   "type" lsp-proxy--support-type-hierarchy 'textDocument/prepareTypeHierarchy
   lsp-proxy-hierarchy--type-specs
   (pcase direction ('derived 'supertypes) ('base 'subtypes) (_ direction))))

(defun lsp-proxy-hierarchy--current-p (buffer generation connection)
  "Whether BUFFER still owns GENERATION and CONNECTION."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and lsp-proxy-hierarchy--active
              (eq generation lsp-proxy-hierarchy--generation)
              (eq connection lsp-proxy-hierarchy--connection)
              (eq connection lsp-proxy--connection)
              (or (null lsp-proxy-hierarchy--source)
                  (buffer-live-p lsp-proxy-hierarchy--source))))))

(defun lsp-proxy-hierarchy--invalidate ()
  "Invalidate callbacks and cancel only this view's requests and timers."
  (setq lsp-proxy-hierarchy--generation (make-symbol "hierarchy"))
  (when (hash-table-p lsp-proxy-hierarchy--pending)
    (maphash
     (lambda (_key request)
       (when-let* ((id (lsp-proxy-hierarchy--pending-request-id request)))
         ;; Match Ivy: remove continuations/timeouts before cancellation.
         (jsonrpc--remove lsp-proxy-hierarchy--connection id)
         (when (jsonrpc-running-p lsp-proxy-hierarchy--connection)
           (ignore-errors
             (jsonrpc-notify lsp-proxy-hierarchy--connection '$/cancelRequest
                             (list :id id :uri
                                   (plist-get lsp-proxy-hierarchy--context :uri)))))))
     lsp-proxy-hierarchy--pending)
    (clrhash lsp-proxy-hierarchy--pending))
  (mapc #'cancel-timer lsp-proxy-hierarchy--timers)
  (setq lsp-proxy-hierarchy--timers nil)
  (unless (hash-table-p lsp-proxy-hierarchy--pending)
    (setq lsp-proxy-hierarchy--pending (make-hash-table :test #'equal))))

(defun lsp-proxy-hierarchy--stop ()
  "Release the view's asynchronous work and source-buffer hook."
  (setq lsp-proxy-hierarchy--active nil)
  (lsp-proxy-hierarchy--invalidate)
  (let ((hook lsp-proxy-hierarchy--source-kill-hook))
    (when (buffer-live-p lsp-proxy-hierarchy--source)
      (with-current-buffer lsp-proxy-hierarchy--source
        (remove-hook 'kill-buffer-hook hook t)))))

(defun lsp-proxy-hierarchy--send (key method params success failure)
  "Send METHOD/PARAMS asynchronously, sharing KEY with other subscribers.
SUCCESS and FAILURE run only for the current live view."
  (if (not (and lsp-proxy-hierarchy--active
                (eq lsp-proxy-hierarchy--connection lsp-proxy--connection)
                (jsonrpc-running-p lsp-proxy-hierarchy--connection)
                (or (null lsp-proxy-hierarchy--source)
                    (buffer-live-p lsp-proxy-hierarchy--source))))
      (funcall failure "The proxy connection or source buffer changed; reopen the hierarchy")
    (if-let* ((pending (gethash key lsp-proxy-hierarchy--pending)))
        (push (cons success failure)
              (lsp-proxy-hierarchy--pending-request-callbacks pending))
      (let* ((buffer (current-buffer))
             (generation lsp-proxy-hierarchy--generation)
             (connection lsp-proxy-hierarchy--connection)
             (pending (lsp-proxy-hierarchy--make-request
                       :callbacks (list (cons success failure)))))
        (puthash key pending lsp-proxy-hierarchy--pending)
        (cl-labels
            ((finish (value failed)
               (when (lsp-proxy-hierarchy--current-p buffer generation connection)
                 (with-current-buffer buffer
                   (when (eq pending (gethash key lsp-proxy-hierarchy--pending))
                     (remhash key lsp-proxy-hierarchy--pending)
                     (dolist (callbacks (lsp-proxy-hierarchy--pending-request-callbacks pending))
                       (funcall (if failed (cdr callbacks) (car callbacks)) value)))))))
          (condition-case err
              (progn
                (unless (and (eq connection lsp-proxy--connection)
                             (jsonrpc-running-p connection))
                  (user-error "The proxy connection changed; reopen the hierarchy"))
                ;; Like Ivy, enter a captured buffer only after checking its lifetime.
                (let ((request (jsonrpc-async-request
                                connection method params
                                :success-fn (lambda (value) (finish value nil))
                                :error-fn (lambda (err)
                                            (finish (or (plist-get err :message)
                                                        (format "%s" err)) t))
                                :timeout-fn (lambda () (finish "Request timed out" t)))))
                  (when (eq pending (gethash key lsp-proxy-hierarchy--pending))
                    (setf (lsp-proxy-hierarchy--pending-request-id pending) (car request)))))
            (error
             (remhash key lsp-proxy-hierarchy--pending)
             (funcall failure (error-message-string err)))))))))

(defun lsp-proxy-hierarchy--prepare ()
  "Asynchronously prepare roots for the captured source position."
  (setq lsp-proxy-hierarchy--status "Loading hierarchy…")
  (lsp-proxy-hierarchy--render)
  (lsp-proxy-hierarchy--send
   :prepare lsp-proxy-hierarchy--preparer lsp-proxy-hierarchy--context
   (lambda (roots)
     (setq lsp-proxy-hierarchy--roots (seq-into roots 'list)
           lsp-proxy-hierarchy--status
           (unless lsp-proxy-hierarchy--roots
             (format "No %s hierarchy information at point" lsp-proxy-hierarchy--kind)))
     (lsp-proxy-hierarchy--render))
   (lambda (message)
     (setq lsp-proxy-hierarchy--status (concat message " (g to retry)"))
     (lsp-proxy-hierarchy--render))))

(defun lsp-proxy-hierarchy--show (kind supported preparer specs direction)
  "Show KIND immediately and asynchronously prepare its hierarchy roots."
  (unless (and (bound-and-true-p lsp-proxy-mode) buffer-file-name)
    (user-error "Enable lsp-proxy-mode in a source buffer first"))
  (unless supported
    (user-error "No language server supports %s hierarchy for this buffer" kind))
  (let ((specs (lsp-proxy-hierarchy--direction-specs specs direction)))
    (lsp-proxy--ensure-connection)
    (lsp-proxy--send-did-change)
    (unless lsp-proxy--buffer-opened (lsp-proxy--on-doc-open))
    (unless lsp-proxy--has-any-servers (user-error "No language server is available"))
    (let* ((source (current-buffer))
           (context (lsp-proxy--build-params (lsp-proxy--TextDocumentPositionParams)))
           (connection lsp-proxy--connection)
           (project-root lsp-proxy--current-project-root)
           (directory default-directory)
           (name (format "*lsp-proxy %s hierarchy: %s*" kind (or project-root directory))))
      (with-current-buffer (get-buffer-create name)
        (lsp-proxy-hierarchy--stop)
        (lsp-proxy-hierarchy--clear-widgets)
        (lsp-proxy-hierarchy-mode)
        (setq-local lsp-proxy-hierarchy--specs specs
                    lsp-proxy-hierarchy--context context
                    lsp-proxy-hierarchy--connection connection
                    lsp-proxy-hierarchy--source source
                    lsp-proxy-hierarchy--preparer preparer
                    lsp-proxy-hierarchy--kind kind
                    lsp-proxy--current-project-root project-root
                    default-directory directory)
        (let ((view (current-buffer)))
          (setq lsp-proxy-hierarchy--source-kill-hook
                (lambda ()
                  (when (buffer-live-p view)
                    (with-current-buffer view (lsp-proxy-hierarchy--stop)))))
          (let ((hook lsp-proxy-hierarchy--source-kill-hook))
            (with-current-buffer source
              (add-hook 'kill-buffer-hook hook nil t))))
        (lsp-proxy-hierarchy--prepare)
        (pop-to-buffer (current-buffer))))))

(defun lsp-proxy-hierarchy--request (method node success failure)
  "Asynchronously expand NODE with METHOD through its captured source/server."
  (let ((params (copy-sequence lsp-proxy-hierarchy--context)))
    (setq params (plist-put params :params (list :item (plist-get node :lsp_item))))
    (setq params (plist-put params :context
                            (list :language-server-id (plist-get node :language_server_id))))
    (lsp-proxy-hierarchy--send
     (list method (plist-get node :language_server_id) (plist-get node :lsp_item))
     method params success failure)))

(defun lsp-proxy-hierarchy--edge-nodes (node spec response)
  "Convert RESPONSE for NODE and SPEC to UI nodes without altering LSP data."
  (pcase-let ((`(,method ,bullet ,_ ,_ ,hint ,key) spec))
    (mapcar
     (lambda (wrapped)
       (let ((edge (plist-get wrapped :lsp_item)))
         (list :lsp_item (if key (plist-get edge key) edge)
               :language_server_id (plist-get wrapped :language_server_id)
               :language_server_name (plist-get wrapped :language_server_name)
               :method method :bullet bullet :hint hint
               :parent-uri (plist-get (plist-get node :lsp_item) :uri)
               :call-sites (when key (plist-get edge :fromRanges)))))
     (seq-into response 'list))))

(defun lsp-proxy-hierarchy--children (node callback)
  "Fetch NODE's directions concurrently, calling CALLBACK with ordered children.
Successful results, including empty results, are cached; failures are retriable."
  (let* ((specs lsp-proxy-hierarchy--specs)
         (remaining (length specs))
         (results (make-vector remaining nil)))
    (cl-loop
     for spec in specs for index from 0
     for method = (car spec)
     for key = (list method (plist-get node :language_server_id) (plist-get node :lsp_item))
     for cached = (gethash key lsp-proxy-hierarchy--cache :missing)
     do
     (let ((index index) (spec spec) (key key))
       (cl-labels ((finish (children)
                     (aset results index children)
                     (when (zerop (cl-decf remaining))
                       (funcall callback (apply #'append (append results nil))))))
         (if (not (eq cached :missing))
             (finish (lsp-proxy-hierarchy--edge-nodes node spec cached))
           (lsp-proxy-hierarchy--request
            (car spec) node
            (lambda (response)
              (puthash key response lsp-proxy-hierarchy--cache)
              (finish (lsp-proxy-hierarchy--edge-nodes node spec response)))
            (lambda (message)
              (finish (list (list :error (format "%s: %s (g to retry)"
                                                 (car spec) message))))))))))))

(defun lsp-proxy-hierarchy--source-position (uri position)
  "Translate POSITION to the source document when URI is an Org code block."
  (let* ((context lsp-proxy-hierarchy--context)
         (virtual-doc (plist-get context :virtual-doc))
         (bias (and virtual-doc (plist-get virtual-doc :line-bias))))
    (if (and position bias (equal uri (plist-get context :uri)))
        (plist-put (copy-sequence position) :line (+ bias (plist-get position :line)))
      position)))

(defun lsp-proxy-hierarchy--node-on-line ()
  "Return the hierarchy node on the current line, or nil."
  (let ((node (get-text-property (point) 'lsp-proxy-hierarchy-node))
        (position (line-beginning-position))
        (end (line-end-position)))
    (while (and (not node) (< position end))
      (setq node (get-text-property position 'lsp-proxy-hierarchy-node)
            position (next-single-property-change
                      position 'lsp-proxy-hierarchy-node nil end)))
    node))

(defun lsp-proxy-hierarchy-visit ()
  "Visit the hierarchy node on the current line, including its indentation."
  (interactive)
  (let ((node (lsp-proxy-hierarchy--node-on-line)))
    (unless node (user-error "No hierarchy node on this line"))
    (lsp-proxy-hierarchy--jump node)))

(defun lsp-proxy-hierarchy--tree-on-line ()
  "Return the tree widget for the hierarchy node on the current line."
  (unless (lsp-proxy-hierarchy--node-on-line)
    (user-error "No hierarchy node on this line"))
  (let ((tree (cl-loop for overlay in (overlays-in (line-beginning-position)
                                                   (line-end-position))
                       for button = (overlay-get overlay 'button)
                       for parent = (and button (widget-get button :parent))
                       when (tree-widget-p parent) return parent)))
    (or tree (user-error "No hierarchy tree on this line"))))

(defun lsp-proxy-hierarchy-toggle ()
  "Expand or collapse the hierarchy node on the current line."
  (interactive)
  (let ((tree (lsp-proxy-hierarchy--tree-on-line)))
    (let ((inhibit-read-only t))
      (save-excursion
        (widget-apply-action tree)
        (widget-setup)))))

(defun lsp-proxy-hierarchy-collapse-parent ()
  "Collapse the current node's direct parent and move to its line.
Repeated invocations collapse successive ancestors."
  (interactive)
  (let* ((tree (lsp-proxy-hierarchy--tree-on-line))
         (parent (widget-get tree :parent)))
    (unless (tree-widget-p parent)
      (user-error "Already at a root hierarchy node"))
    ;; Leave the child before its markers are invalidated by collapsing.
    (goto-char (widget-get parent :from))
    (when (widget-get parent :open)
      (let ((inhibit-read-only t))
        (widget-apply-action parent)
        (widget-setup)))
    (goto-char (widget-get parent :from))
    (beginning-of-line)))

(defun lsp-proxy-hierarchy--jump (node)
  "Visit NODE's first call site, or its selection range for a type/root."
  (let* ((item (plist-get node :lsp_item))
         (call-site (car (seq-into (plist-get node :call-sites) 'list)))
         ;; Outgoing call ranges belong to the caller (the parent), not the callee.
         (uri (if (and call-site
                       (eq (plist-get node :method) 'callHierarchy/outgoingCalls))
                  (plist-get node :parent-uri)
                (plist-get item :uri)))
         (range (or call-site (plist-get item :selectionRange) (plist-get item :range)))
         (start (lsp-proxy-hierarchy--source-position uri (plist-get range :start)))
         (path (and uri (lsp-proxy--uri-to-path uri))))
    (unless (and path start) (user-error "No source location for this hierarchy item"))
    (xref-push-marker-stack)
    (pop-to-buffer (find-file-noselect path))
    (widen)
    (goto-char (eglot--lsp-position-to-point start))))

(define-button-type 'lsp-proxy-hierarchy-item
  'follow-link t
  'face 'font-lock-function-name-face)

(defvar lsp-proxy-hierarchy-label-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map [mouse-3]
                (lambda (event)
                  (interactive "e")
                  (mouse-set-point event)
                  (lsp-proxy-hierarchy-center-on-node)))
    map)
  "Keymap on hierarchy node labels.")

(defvar lsp-proxy-hierarchy-icon-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    map)
  "Keymap on hierarchy icons; mouse actions still toggle expansion.")

(dolist (map (list lsp-proxy-hierarchy-label-map lsp-proxy-hierarchy-icon-map))
  (define-key map (kbd "RET") #'lsp-proxy-hierarchy-visit)
  (define-key map (kbd "<return>") #'lsp-proxy-hierarchy-visit)
  (define-key map (kbd "e") #'lsp-proxy-hierarchy-toggle)
  (define-key map (kbd "u") #'lsp-proxy-hierarchy-collapse-parent)
  (define-key map (kbd "d") #'lsp-proxy-hierarchy-set-direction))

(defun lsp-proxy-hierarchy--label (node)
  "Make the navigable label of NODE."
  (with-temp-buffer
    (insert (propertize (or (plist-get node :bullet) " ∘ ")
                        'face 'shadow 'help-echo (plist-get node :hint)))
    (insert-text-button
     (plist-get (plist-get node :lsp_item) :name)
     :type 'lsp-proxy-hierarchy-item
     'lsp-proxy-hierarchy-node node
     'keymap lsp-proxy-hierarchy-label-map
     'help-echo (format "RET: visit source, e: expand/collapse, u: collapse parent, d: direction, c/mouse-3: center [%s]"
                        (plist-get node :language_server_name))
     'action (lambda (_button) (lsp-proxy-hierarchy--jump node)))
    (insert (propertize (format " [%s]" (plist-get node :language_server_name))
                        'face 'shadow))
    (buffer-string)))

(defun lsp-proxy-hierarchy--clear-widgets ()
  "Discard old widgets, including incomplete widgets from a failed expansion."
  (let ((inhibit-read-only t))
    (dolist (widget lsp-proxy-hierarchy--widgets)
      (when (cl-every (lambda (key)
                        (let ((marker (widget-get widget key)))
                          (and (markerp marker)
                               (eq (marker-buffer marker) (current-buffer)))))
                      '(:from :to))
        ;; A partially recreated descendant may also have dead markers.
        (ignore-errors (widget-delete widget))))
    (setq lsp-proxy-hierarchy--widgets nil)
    (remove-overlays)))

(defun lsp-proxy-hierarchy--convert (node)
  "Convert NODE to a lazy asynchronous tree widget or status leaf."
  (if (plist-get node :error)
      (widget-convert 'item :tag (propertize (plist-get node :error) 'face 'error))
    (let ((widget
           (widget-convert
            'tree-widget :tag (lsp-proxy-hierarchy--label node)
            :expander-p (lambda (widget)
                          (let ((args (widget-get widget :args)))
                            (or (null args)
                                (cl-some (lambda (arg) (widget-get arg :hierarchy-loading)) args))))
            :expander (lambda (widget)
                        (lsp-proxy-hierarchy--expand-widget widget node)))))
      (widget-put widget :empty-icon (widget-get widget :leaf-icon))
      (dolist (key '(:open-icon :close-icon :empty-icon :leaf-icon))
        (widget-put widget key
                    (widget-convert (widget-get widget key)
                                    :keymap lsp-proxy-hierarchy-icon-map)))
      widget)))

(defun lsp-proxy-hierarchy--update-widget (widget children)
  "Schedule a local WIDGET update after creation and asynchronous callbacks finish."
  (let ((buffer (current-buffer))
        (generation lsp-proxy-hierarchy--generation)
        (connection lsp-proxy-hierarchy--connection)
        timer)
    (setq timer
          (run-at-time
           0 nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq lsp-proxy-hierarchy--timers (delq timer lsp-proxy-hierarchy--timers))
                 (when (and (lsp-proxy-hierarchy--current-p buffer generation connection)
                            (cl-every (lambda (key)
                                        (let ((marker (widget-get widget key)))
                                          (and (markerp marker) (eq (marker-buffer marker) buffer))))
                                      '(:from :to)))
                   (widget-put widget :args (mapcar #'lsp-proxy-hierarchy--convert children))
                   (when (widget-get widget :open)
                     (let ((inhibit-read-only t))
                       (save-excursion
                         (widget-value-set widget (widget-value widget))
                         (widget-setup))))))))))
    (push timer lsp-proxy-hierarchy--timers)))

(defun lsp-proxy-hierarchy--expand-widget (widget node)
  "Return cached children, or a loading leaf while fetching NODE asynchronously."
  (let ((creating t) ready children)
    (lsp-proxy-hierarchy--children
     node (lambda (result)
            (setq ready t children result)
            (unless creating
              (lsp-proxy-hierarchy--update-widget widget result))))
    (setq creating nil)
    (if ready
        (mapcar #'lsp-proxy-hierarchy--convert children)
      (list (widget-convert 'item :hierarchy-loading t
                            :tag (propertize "Loading…" 'face 'shadow))))))

(defun lsp-proxy-hierarchy--render ()
  "Render status or roots, initiating asynchronous first-level expansion."
  (let ((inhibit-read-only t))
    (lsp-proxy-hierarchy--clear-widgets)
    (erase-buffer)
    (if lsp-proxy-hierarchy--status
        (insert lsp-proxy-hierarchy--status "\n")
      (dolist (root lsp-proxy-hierarchy--roots)
        (push (widget-create (lsp-proxy-hierarchy--convert root) :open t)
              lsp-proxy-hierarchy--widgets)))
    (widget-setup)
    (goto-char (point-min))))

(defun lsp-proxy-hierarchy-set-direction (direction)
  "Select DIRECTION in the current view, preserving roots and cached results.
DIRECTION is t (both), `incoming' or `outgoing' for call hierarchies,
or `supertypes' or `subtypes' for type hierarchies.
Changing direction cancels pending work and resets expansion state."
  (interactive
   (list (lsp-proxy-hierarchy--read-direction
          (lsp-proxy-hierarchy--all-specs)
          (lsp-proxy-hierarchy--direction-label lsp-proxy-hierarchy--specs))))
  (let ((specs (lsp-proxy-hierarchy--direction-specs
                (lsp-proxy-hierarchy--all-specs) direction)))
    (when (and lsp-proxy-hierarchy--source
               (not (buffer-live-p lsp-proxy-hierarchy--source)))
      (user-error "The source buffer was closed; reopen the hierarchy"))
    (unless (and (eq lsp-proxy-hierarchy--connection lsp-proxy--connection)
                 (jsonrpc-running-p lsp-proxy-hierarchy--connection))
      (user-error "The proxy connection changed; reopen the hierarchy"))
    (unless (and (equal specs lsp-proxy-hierarchy--specs)
                 lsp-proxy-hierarchy--active)
      (lsp-proxy-hierarchy--invalidate)
      (setq lsp-proxy-hierarchy--specs specs
            lsp-proxy-hierarchy--active t)
      (let ((hook lsp-proxy-hierarchy--source-kill-hook))
        (when (and hook (buffer-live-p lsp-proxy-hierarchy--source))
          (with-current-buffer lsp-proxy-hierarchy--source
            (add-hook 'kill-buffer-hook hook nil t))))
      (if lsp-proxy-hierarchy--roots
          (progn (setq lsp-proxy-hierarchy--status nil)
                 (lsp-proxy-hierarchy--render))
        (lsp-proxy-hierarchy--prepare)))))

(defun lsp-proxy-hierarchy-refresh (&rest _ignored)
  "Clear all expansion caches and refresh the current hierarchy."
  (interactive)
  (when (and lsp-proxy-hierarchy--source
             (not (buffer-live-p lsp-proxy-hierarchy--source)))
    (user-error "The source buffer was closed; reopen the hierarchy"))
  (lsp-proxy-hierarchy--invalidate)
  (setq lsp-proxy-hierarchy--active t)
  (let ((hook lsp-proxy-hierarchy--source-kill-hook))
    (when (buffer-live-p lsp-proxy-hierarchy--source)
      (with-current-buffer lsp-proxy-hierarchy--source
        (add-hook 'kill-buffer-hook hook nil t))))
  (clrhash lsp-proxy-hierarchy--cache)
  (if lsp-proxy-hierarchy--roots
      (progn (setq lsp-proxy-hierarchy--status nil) (lsp-proxy-hierarchy--render))
    (lsp-proxy-hierarchy--prepare)))

(defun lsp-proxy-hierarchy-center-on-node ()
  "Center the hierarchy on the node at point."
  (interactive)
  (let ((node (get-text-property (point) 'lsp-proxy-hierarchy-node)))
    (unless node (user-error "No hierarchy node at point"))
    (lsp-proxy-hierarchy--invalidate)
    ;; A centered node is a root, with no call-site/parent edge metadata.
    (setq lsp-proxy-hierarchy--roots
          (list (list :lsp_item (plist-get node :lsp_item)
                      :language_server_id (plist-get node :language_server_id)
                      :language_server_name (plist-get node :language_server_name))))
    (lsp-proxy-hierarchy--render)))

(defun lsp-proxy-hierarchy--eldoc (_callback &rest _ignored)
  "Describe the hierarchy item at point."
  (when-let* ((node (get-text-property (point) 'lsp-proxy-hierarchy-node))
              (item (plist-get node :lsp_item))
              (uri (plist-get item :uri))
              (start (lsp-proxy-hierarchy--source-position
                      uri (plist-get (or (plist-get item :selectionRange)
                                         (plist-get item :range)) :start))))
    (format "%s%s:%d:%d [%s]"
            (if-let* ((detail (plist-get item :detail))) (concat detail " · ") "")
            (lsp-proxy--uri-to-path uri)
            (1+ (plist-get start :line)) (1+ (plist-get start :character))
            (plist-get node :language_server_name))))

(defun lsp-proxy-hierarchy-quit ()
  "Cancel pending hierarchy work and quit its window."
  (interactive)
  (lsp-proxy-hierarchy--stop)
  (quit-window))

(defvar lsp-proxy-hierarchy-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") #'widget-forward)
    (define-key map (kbd "<backtab>") #'widget-backward)
    (define-key map (kbd "c") #'lsp-proxy-hierarchy-center-on-node)
    (define-key map (kbd "g") #'lsp-proxy-hierarchy-refresh)
    map)
  "Keymap for hierarchy trees.")

(define-key lsp-proxy-hierarchy-mode-map (kbd "q") #'lsp-proxy-hierarchy-quit)
(define-key lsp-proxy-hierarchy-mode-map (kbd "RET") #'lsp-proxy-hierarchy-visit)
(define-key lsp-proxy-hierarchy-mode-map (kbd "<return>") #'lsp-proxy-hierarchy-visit)
(define-key lsp-proxy-hierarchy-mode-map (kbd "e") #'lsp-proxy-hierarchy-toggle)
(define-key lsp-proxy-hierarchy-mode-map (kbd "u") #'lsp-proxy-hierarchy-collapse-parent)
(define-key lsp-proxy-hierarchy-mode-map (kbd "d") #'lsp-proxy-hierarchy-set-direction)

(defun lsp-proxy-hierarchy--setup-navigation ()
  "Set the direction header and apply navigation bindings to existing icons."
  (setq-local header-line-format
              '(:eval (concat "Direction: "
                              (lsp-proxy-hierarchy--direction-label lsp-proxy-hierarchy--specs)
                              " | d: change direction")))
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when-let* ((widget (overlay-get overlay 'button)))
      (widget-put widget :keymap lsp-proxy-hierarchy-icon-map)
      (overlay-put overlay 'keymap lsp-proxy-hierarchy-icon-map))))

(define-derived-mode lsp-proxy-hierarchy-mode special-mode "LSP Hierarchy"
  "View call and type hierarchies.
Expand/collapse using e or the tree buttons; u collapses the parent and
moves to it.  d selects direction.  RET visits source, c centers,
g refreshes and q quits.
TAB and backtab move between buttons."
  (setq-local lsp-proxy-hierarchy--cache (make-hash-table :test #'equal)
              lsp-proxy-hierarchy--pending (make-hash-table :test #'equal)
              lsp-proxy-hierarchy--generation (make-symbol "hierarchy")
              lsp-proxy-hierarchy--active t)
  (add-hook 'kill-buffer-hook #'lsp-proxy-hierarchy--stop nil t)
  (add-hook 'change-major-mode-hook #'lsp-proxy-hierarchy--stop nil t)
  (setq-local revert-buffer-function #'lsp-proxy-hierarchy-refresh)
  (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose)
  (add-hook 'eldoc-documentation-functions #'lsp-proxy-hierarchy--eldoc nil t)
  (lsp-proxy-hierarchy--setup-navigation))

(provide 'lsp-proxy-hierarchy)
;;; lsp-proxy-hierarchy.el ends here
