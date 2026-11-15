;;; lsp-proxy-peek.el --- Inline LSP navigation with branching history -*- lexical-binding: t; -*-

;;; Commentary:
;; Read definitions, references and implementations without selecting another
;; buffer.  Each result keeps its own query branches and scroll position.
;; Symbol selection uses a transient map; neither selection nor LSP queries
;; enter a blocking input loop.  Only the selected result's source is loaded.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'xref)
(require 'eglot)
(require 'fringe)
(require 'lsp-proxy-core)
(require 'lsp-proxy-utils)

(declare-function lsp-proxy-mode "lsp-proxy" (&optional arg))
(declare-function lsp-proxy-java--setup-buffer "lsp-proxy-java")
(defvar lsp-proxy-java--content-cache)
(defvar lsp-proxy-java-class-file-contents-server-name)

(defcustom lsp-proxy-peek-height 12
  "Number of source lines displayed in a peek."
  :type 'natnum :group 'lsp-proxy)

(defcustom lsp-proxy-peek-results-height 3
  "Maximum number of results displayed below the source preview."
  :type 'natnum :group 'lsp-proxy)

(defcustom lsp-proxy-peek-fill-fringe t
  "Whether to fill the preview's left and right fringes with its border color.
Set this to nil if the borders look better without it when using line numbers."
  :type 'boolean :group 'lsp-proxy)

(defcustom lsp-proxy-peek-use-dashes-as-horizontal-border nil
  "Whether to use dashes for horizontal borders even on graphical displays.
This can look better with line numbers, which may give a thin border the
height of a full text line.  Terminals always use dashes."
  :type 'boolean :group 'lsp-proxy)

(defface lsp-proxy-peek-border-face
  '((t :inherit highlight :height 15 :extend t))
  "Face for peek borders; customize its background color and height.
Terminal borders use the background color as the color of their dashes."
  :group 'lsp-proxy)

(defface lsp-proxy-peek-header-face '((t :inherit header-line))
  "Face for the peek header and reading history." :group 'lsp-proxy)
(defface lsp-proxy-peek-selected-face '((t :inherit highlight))
  "Face for the selected navigation result." :group 'lsp-proxy)
(defface lsp-proxy-peek-match-face '((t :inherit lazy-highlight))
  "Face for the target range in the source preview." :group 'lsp-proxy)
(defface lsp-proxy-peek-key-face '((t :inherit font-lock-keyword-face :weight bold))
  "Face for symbol selection keys in the preview." :group 'lsp-proxy)

(cl-defstruct (lsp-proxy-peek--location (:constructor lsp-proxy-peek--location-create))
  uri range marker (offset -3) branches (branch-index 0))
(cl-defstruct (lsp-proxy-peek--query (:constructor lsp-proxy-peek--query-create))
  label locations (index 0) parent parent-location)
(cl-defstruct (lsp-proxy-peek--session (:constructor lsp-proxy-peek--session-create))
  source host connection context current overlay window token pending status
  buffers preview picks pick-prefix pick-method exit-map)

(defvar-local lsp-proxy-peek--session nil)
(defvar lsp-proxy-peek--last-session nil
  "Most recent reading session, retained when its UI is closed.")

(defvar lsp-proxy-peek-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'lsp-proxy-peek-next-line)
    (define-key map (kbd "M-p") #'lsp-proxy-peek-previous-line)
    (define-key map (kbd "M-N") #'lsp-proxy-peek-next-result)
    (define-key map (kbd "M-P") #'lsp-proxy-peek-previous-result)
    (define-key map (kbd "<left>") #'lsp-proxy-peek-backward)
    (define-key map (kbd "<right>") #'lsp-proxy-peek-forward)
    (define-key map (kbd "<up>") #'lsp-proxy-peek-previous-branch)
    (define-key map (kbd "<down>") #'lsp-proxy-peek-next-branch)
    (define-key map (kbd "M-l d") #'lsp-proxy-peek-definition)
    (define-key map (kbd "M-l r") #'lsp-proxy-peek-references)
    (define-key map (kbd "M-l i") #'lsp-proxy-peek-implementations)
    (define-key map (kbd "M-l t") #'lsp-proxy-peek-type-definition)
    (define-key map (kbd "M-l j") #'lsp-proxy-peek-jump)
    (define-key map [remap keyboard-quit] #'lsp-proxy-peek-abort)
    (define-key map (kbd "C-g") #'lsp-proxy-peek-abort)
    map)
  "Keys active while an inline peek is displayed.")

(define-minor-mode lsp-proxy-peek-mode
  "Display an inline LSP reading session in the current buffer.
Use `lsp-proxy-peek-definition', `lsp-proxy-peek-references' or
`lsp-proxy-peek-implementations' to start a session."
  :lighter " Peek" :keymap lsp-proxy-peek-mode-map
  (if (and lsp-proxy-peek-mode lsp-proxy-peek--session)
      (progn
        (setf (lsp-proxy-peek--session-host lsp-proxy-peek--session) (current-buffer)
              (lsp-proxy-peek--session-window lsp-proxy-peek--session) (selected-window)
              (lsp-proxy-peek--session-overlay lsp-proxy-peek--session)
              (make-overlay (line-end-position) (line-end-position)))
        (add-hook 'post-command-hook #'lsp-proxy-peek--post-command nil t)
        (add-hook 'post-command-hook #'lsp-proxy-peek--update-map)
        (add-hook 'kill-buffer-hook #'lsp-proxy-peek-abort nil t)
        (add-hook 'change-major-mode-hook #'lsp-proxy-peek-abort nil t)
        (lsp-proxy-peek--install-map lsp-proxy-peek--session))
    (setq lsp-proxy-peek-mode nil)
    (when lsp-proxy-peek--session
      (when-let* ((exit (lsp-proxy-peek--session-exit-map lsp-proxy-peek--session)))
        (funcall exit))
      (lsp-proxy-peek--invalidate lsp-proxy-peek--session)
      (when-let* ((overlay (lsp-proxy-peek--session-overlay lsp-proxy-peek--session)))
        (delete-overlay overlay))
      (setf (lsp-proxy-peek--session-overlay lsp-proxy-peek--session) nil)
      (lsp-proxy-peek--release-buffers lsp-proxy-peek--session))
    (remove-hook 'post-command-hook #'lsp-proxy-peek--post-command t)
    (remove-hook 'post-command-hook #'lsp-proxy-peek--update-map)
    (remove-hook 'kill-buffer-hook #'lsp-proxy-peek-abort t)
    (remove-hook 'change-major-mode-hook #'lsp-proxy-peek-abort t)))

(defun lsp-proxy-peek--active-p (session &optional token)
  "Whether SESSION still owns its live UI and, optionally, TOKEN."
  (let ((host (lsp-proxy-peek--session-host session)))
    (and (buffer-live-p host)
         (buffer-live-p (lsp-proxy-peek--session-source session))
         (eq (lsp-proxy-peek--session-connection session) lsp-proxy--connection)
         (with-current-buffer host
           (and lsp-proxy-peek-mode (eq session lsp-proxy-peek--session)))
         (or (null token) (eq token (lsp-proxy-peek--session-token session))))))

(defun lsp-proxy-peek--focused-p (session)
  "Whether SESSION owns the selected buffer and window."
  (and (lsp-proxy-peek--active-p session)
       (eq (current-buffer) (lsp-proxy-peek--session-host session))
       (eq (window-buffer (selected-window)) (current-buffer))))

(defun lsp-proxy-peek--install-map (session)
  "Give SESSION's navigation keys precedence only in its selected host."
  (when (and (lsp-proxy-peek--focused-p session)
             (null (lsp-proxy-peek--session-exit-map session)))
    (setf (lsp-proxy-peek--session-exit-map session)
          (set-transient-map
           lsp-proxy-peek-mode-map
           (lambda () (lsp-proxy-peek--focused-p session))
           (lambda () (setf (lsp-proxy-peek--session-exit-map session) nil))))))

(defun lsp-proxy-peek--update-map ()
  "Suspend or restore navigation keys immediately after a buffer switch."
  (when-let* ((session lsp-proxy-peek--last-session))
    (if (lsp-proxy-peek--focused-p session)
        (lsp-proxy-peek--install-map session)
      (when-let* ((exit (lsp-proxy-peek--session-exit-map session))) (funcall exit)))))

(defun lsp-proxy-peek--invalidate (session)
  "Cancel SESSION's request and invalidate outstanding callbacks and selection."
  (setf (lsp-proxy-peek--session-token session) (make-symbol "peek")
        (lsp-proxy-peek--session-picks session) nil
        (lsp-proxy-peek--session-preview session) nil
        (lsp-proxy-peek--session-status session) nil)
  (when-let* ((pending (lsp-proxy-peek--session-pending session))
              (id (plist-get pending :id)))
    (lsp-proxy--cancel-request (lsp-proxy-peek--session-connection session)
                              id (plist-get pending :context)))
  (setf (lsp-proxy-peek--session-pending session) nil))

(defun lsp-proxy-peek--release-buffers (session)
  "Release only unmodified, invisible file buffers created by SESSION."
  (dolist (buffer (lsp-proxy-peek--session-buffers session))
    (when (and (buffer-live-p buffer)
               (not (buffer-modified-p buffer))
               (not (get-buffer-window buffer t)))
      (kill-buffer buffer)))
  (setf (lsp-proxy-peek--session-buffers session) nil))

(defun lsp-proxy-peek--send (session method context success)
  "Send METHOD with CONTEXT asynchronously for SESSION, then call SUCCESS."
  (let* ((token (lsp-proxy-peek--session-token session))
         (connection (lsp-proxy-peek--session-connection session))
         (pending (list :context context)))
    (setf (lsp-proxy-peek--session-pending session) pending)
    (cl-labels
        ((finish (value failed)
           (when (and (lsp-proxy-peek--active-p session token)
                      (eq pending (lsp-proxy-peek--session-pending session)))
             (setf (lsp-proxy-peek--session-pending session) nil
                   (lsp-proxy-peek--session-status session) nil)
             (with-current-buffer (lsp-proxy-peek--session-host session)
               (condition-case err
                   (if failed
                       (setf (lsp-proxy-peek--session-status session) value)
                     (funcall success value))
                 (error (setf (lsp-proxy-peek--session-status session)
                              (error-message-string err))))
               (lsp-proxy-peek--render session)))))
      (condition-case err
          (progn
            (unless (and (lsp-proxy-peek--active-p session token)
                         (jsonrpc-running-p connection))
              (user-error "The proxy connection changed; start a new peek"))
            (let ((request
                   (jsonrpc-async-request
                    connection method context
                    :success-fn (lambda (value) (finish value nil))
                    :error-fn (lambda (err)
                                (finish (or (plist-get err :message) (format "%s" err)) t))
                    :timeout-fn (lambda () (finish "Peek request timed out" t)))))
              (when (eq pending (lsp-proxy-peek--session-pending session))
                (plist-put pending :id (car request)))))
        (error (finish (error-message-string err) t))))))

(defun lsp-proxy-peek--current-location (session)
  "Return the selected location in SESSION."
  (let ((query (lsp-proxy-peek--session-current session)))
    (aref (lsp-proxy-peek--query-locations query) (lsp-proxy-peek--query-index query))))

(defun lsp-proxy-peek--locations (response)
  "Normalize a navigation RESPONSE, preserving precise LSP ranges."
  (mapcar
   (lambda (item)
     (lsp-proxy-peek--location-create
      :uri (or (plist-get item :uri) (plist-get item :targetUri))
      :range (or (plist-get item :range) (plist-get item :targetSelectionRange))))
   (cond ((null response) nil)
         ((vectorp response) (append response nil))
         ((keywordp (car response)) (list response))
         (t response))))

(defun lsp-proxy-peek--visit (session location)
  "Get LOCATION's file buffer without selecting it, recording ownership."
  (let* ((path (with-current-buffer (lsp-proxy-peek--session-source session)
                 (lsp-proxy--uri-to-path (lsp-proxy-peek--location-uri location))))
         (existing (find-buffer-visiting path))
         (buffer (or existing
                     (progn
                       (unless (file-exists-p path) (user-error "Source file does not exist: %s" path))
                       (find-file-noselect path)))))
    (unless existing (cl-pushnew buffer (lsp-proxy-peek--session-buffers session)))
    (unless (marker-buffer (or (lsp-proxy-peek--location-marker location) (make-marker)))
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (setf (lsp-proxy-peek--location-marker location)
                (copy-marker (eglot--lsp-position-to-point
                              (plist-get (lsp-proxy-peek--location-range location) :start)))))))
    buffer))

(defun lsp-proxy-peek--load-preview (session)
  "Load only SESSION's selected source, fetching JDT sources asynchronously."
  (let* ((location (lsp-proxy-peek--current-location session))
         (uri (lsp-proxy-peek--location-uri location)))
    (if (and (not (let ((marker (lsp-proxy-peek--location-marker location)))
                    (and marker (marker-buffer marker))))
             (lsp-proxy--decompiled-scheme-p uri)
             (not (and (boundp 'lsp-proxy-java--content-cache)
                       (gethash uri lsp-proxy-java--content-cache))))
        (if (not (string-prefix-p "jdt:" uri))
            (setf (lsp-proxy-peek--session-status session)
                  "No asynchronous source provider for this URI")
          (require 'lsp-proxy-java-jdtls)
          (setf (lsp-proxy-peek--session-status session) "Loading source…")
          (let ((route (plist-get (lsp-proxy-peek--session-context session) :uri)))
            (lsp-proxy-peek--send
             session 'emacs/forwardRequest
             (list :uri route :params
                   (list :uri route :serverName lsp-proxy-java-class-file-contents-server-name
                         :method "java/classFileContents" :params (list :uri uri)))
             (lambda (code)
               (unless (stringp code) (error "No source content returned"))
               (puthash uri (list :code code :language "java") lsp-proxy-java--content-cache)
               (lsp-proxy-peek--visit session location)))))
      (condition-case err
          (lsp-proxy-peek--visit session location)
        (error (setf (lsp-proxy-peek--session-status session) (error-message-string err)))))))

(defun lsp-proxy-peek--refresh (session)
  "Refresh the selected preview after a navigation operation."
  (lsp-proxy-peek--invalidate session)
  (lsp-proxy-peek--load-preview session)
  (lsp-proxy-peek--render session))

(defun lsp-proxy-peek--query-at (session buffer position method)
  "Query METHOD at BUFFER/POSITION, adding a branch to SESSION's current result."
  (let* ((parent (lsp-proxy-peek--session-current session))
         (parent-location (lsp-proxy-peek--current-location session))
         (tick (buffer-chars-modified-tick buffer))
         context label)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char position)
          (setq label (format "%s: %s" (lsp-proxy-peek--method-label method)
                              (or (thing-at-point 'symbol t) "?")))
          (unless (lsp-proxy--decompiled-buffer-p)
            (unless (bound-and-true-p lsp-proxy-mode) (lsp-proxy-mode 1))
            (lsp-proxy--ensure-connection)
            (unless lsp-proxy--buffer-opened (lsp-proxy--on-doc-open))
            (lsp-proxy--send-did-change))
          (setq context (lsp-proxy--build-params
                         (append (lsp-proxy--TextDocumentPositionParams)
                                 (when (eq method 'textDocument/references)
                                   '(:context (:includeDeclaration t))))))
          ;; A decompiled target has no server-owned document of its own.
          ;; Route via this session's source, keeping the target in LSP params.
          (when (lsp-proxy--decompiled-buffer-p)
            (setq context (list :uri (plist-get (lsp-proxy-peek--session-context session) :uri)
                                :params (plist-get context :params)))))))
    (lsp-proxy-peek--invalidate session)
    (setf (lsp-proxy-peek--session-status session) (concat "Loading " label "…"))
    (lsp-proxy-peek--render session)
    (lsp-proxy-peek--send
     session method context
     (lambda (response)
       (unless (and (buffer-live-p buffer) (= tick (buffer-chars-modified-tick buffer)))
         (error "Query source changed; run the query again"))
       (let ((locations (lsp-proxy-peek--locations response)))
         (if (null locations)
             (let ((status (concat "No results for " label)))
               (if (or (lsp-proxy-peek--query-parent parent)
                       (lsp-proxy-peek--location-branches parent-location))
                   (setf (lsp-proxy-peek--session-status session) status)
                 (lsp-proxy-peek-abort)
                 (setq lsp-proxy-peek--session nil)
                 (when (eq lsp-proxy-peek--last-session session)
                   (setq lsp-proxy-peek--last-session nil))
                 (with-current-buffer (lsp-proxy-peek--session-source session)
                   (remove-hook 'kill-buffer-hook #'lsp-proxy-peek--source-closed t)
                   (remove-hook 'change-major-mode-hook #'lsp-proxy-peek--source-closed t))
                 (message "%s" status)))
           (let ((query (lsp-proxy-peek--query-create
                         :label label :locations (vconcat locations)
                         :parent parent :parent-location parent-location)))
             (setf (lsp-proxy-peek--location-branches parent-location)
                   (append (lsp-proxy-peek--location-branches parent-location) (list query))
                   (lsp-proxy-peek--location-branch-index parent-location)
                   (1- (length (lsp-proxy-peek--location-branches parent-location)))
                   (lsp-proxy-peek--session-current session) query)
             (lsp-proxy-peek--refresh session))))))))

(defun lsp-proxy-peek--method-label (method)
  "Return a short label for METHOD."
  (pcase method
    ('textDocument/references "References")
    ('textDocument/implementation "Implementations")
    ('textDocument/typeDefinition "Type definition")
    ('textDocument/declaration "Declaration")
    (_ "Definition")))

(defun lsp-proxy-peek--source-region (location)
  "Return the visible source region for LOCATION in its marker's buffer."
  (save-excursion
    (goto-char (lsp-proxy-peek--location-marker location))
    (beginning-of-line)
    (let ((overflow (forward-line (lsp-proxy-peek--location-offset location))))
      (cl-decf (lsp-proxy-peek--location-offset location) overflow))
    (let ((beg (line-beginning-position)))
      (forward-line (max 1 lsp-proxy-peek-height))
      (cons beg (point)))))

(defun lsp-proxy-peek--source-string (session width)
  "Build the visible source text for SESSION within WIDTH columns."
  (let* ((location (lsp-proxy-peek--current-location session))
         (marker (lsp-proxy-peek--location-marker location))
         (buffer (and marker (marker-buffer marker))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (pcase-let* ((`(,beg . ,end) (lsp-proxy-peek--source-region location))
                       (picks (lsp-proxy-peek--session-picks session))
                       (prefix (or (lsp-proxy-peek--session-pick-prefix session) "")))
            (font-lock-ensure beg end)
            (let ((text (buffer-substring beg end))
                  (start (max beg (marker-position marker)))
                  (finish (min end (eglot--lsp-position-to-point
                                    (plist-get (lsp-proxy-peek--location-range location) :end)))))
              (when (< start finish)
                (add-face-text-property (- start beg) (- finish beg)
                                        'lsp-proxy-peek-match-face t text))
              ;; Insert from right to left so source offsets stay valid.
              (dolist (pick (reverse picks))
                (when (string-prefix-p prefix (car pick))
                  (let ((offset (- (nth 1 pick) beg)))
                    (when (<= 0 offset (length text))
                      (setq text (concat (substring text 0 offset)
                                         (propertize (concat "[" (car pick) "]")
                                                     'face 'lsp-proxy-peek-key-face)
                                         (substring text offset)))))))
              (setf (lsp-proxy-peek--session-preview session)
                    (list buffer (buffer-chars-modified-tick) width))
              (concat (mapconcat (lambda (line) (truncate-string-to-width line width))
                                 (split-string (string-remove-suffix "\n" text) "\n") "\n")
                      "\n"))))))))

(defun lsp-proxy-peek--result-label (location)
  "Format LOCATION without reading its file."
  (format "%s:%d"
          (if (lsp-proxy--decompiled-scheme-p (lsp-proxy-peek--location-uri location))
              (lsp-proxy-peek--location-uri location)
            (abbreviate-file-name (lsp-proxy--uri-to-path (lsp-proxy-peek--location-uri location))))
          (1+ (plist-get (plist-get (lsp-proxy-peek--location-range location) :start) :line))))

(defun lsp-proxy-peek--tail (text width)
  "Fit TEXT within WIDTH, retaining its filename or latest history entry."
  (let ((length (string-width text)))
    (if (<= length width) text
      (concat "…" (truncate-string-to-width text length (max 0 (- length (1- width))))))))

(defun lsp-proxy-peek--history (query)
  "Format QUERY's reading path, including result and branch selections."
  (let (labels)
    (while query
      (push (format "%s [%d/%d]" (lsp-proxy-peek--query-label query)
                    (1+ (lsp-proxy-peek--query-index query))
                    (length (lsp-proxy-peek--query-locations query))) labels)
      (setq query (lsp-proxy-peek--query-parent query)))
    (mapconcat #'identity labels " → ")))

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'lsp-proxy-peek-fringe [0]))

(defun lsp-proxy-peek--decorate-fringes (text window)
  "Fill both fringes on every line of TEXT displayed in WINDOW."
  (if (not (and lsp-proxy-peek-fill-fringe
                (display-graphic-p (and (window-live-p window) (window-frame window)))
                (fringe-bitmap-p 'lsp-proxy-peek-fringe)))
      text
    (let ((start 0) pieces)
      (while (string-match "\n" text start)
        (let ((end (match-beginning 0)))
          (push (concat (substring text start end)
                        (propertize " " 'display
                                    '(left-fringe lsp-proxy-peek-fringe lsp-proxy-peek-border-face))
                        (propertize " " 'display
                                    '(right-fringe lsp-proxy-peek-fringe lsp-proxy-peek-border-face))
                        (substring text end (1+ end))) pieces)
          (setq start (1+ end))))
      (push (substring text start) pieces)
      (apply #'concat (nreverse pieces)))))

(defun lsp-proxy-peek--border (width window)
  "Return a horizontal border WIDTH columns wide for WINDOW."
  (if (and (display-graphic-p (and (window-live-p window) (window-frame window)))
           (not lsp-proxy-peek-use-dashes-as-horizontal-border))
      (propertize (lsp-proxy-peek--decorate-fringes "\n" window)
                  'face 'lsp-proxy-peek-border-face)
    (let ((color (face-attribute 'lsp-proxy-peek-border-face :background nil t)))
      (lsp-proxy-peek--decorate-fringes
       (propertize (concat (make-string width ?-) "\n")
                   'face (list :inherit 'default
                               :foreground (if (stringp color) color
                                             (face-attribute 'shadow :foreground nil t))))
       window))))

(defun lsp-proxy-peek--render (session)
  "Render SESSION in its host buffer, without selecting a window or buffer."
  (when (lsp-proxy-peek--active-p session)
    (with-current-buffer (lsp-proxy-peek--session-host session)
      (let* ((window (lsp-proxy-peek--session-window session))
             (width (max 20 (if (window-live-p window) (window-body-width window) 80)))
             (overlay (lsp-proxy-peek--session-overlay session))
             (border (lsp-proxy-peek--border width window))
             (query (lsp-proxy-peek--session-current session))
             (locations (lsp-proxy-peek--query-locations query))
             (index (lsp-proxy-peek--query-index query))
             (location (aref locations index))
             (status (lsp-proxy-peek--session-status session))
             (source (lsp-proxy-peek--source-string session width))
             (count (max 1 lsp-proxy-peek-results-height))
             (start (max 0 (min index (- (length locations) count))))
             rows)
        (cl-loop for i from start below (min (length locations) (+ start count)) do
                 (push (propertize
                        (concat (if (= i index) "› " "  ")
                                (lsp-proxy-peek--tail
                                 (lsp-proxy-peek--result-label (aref locations i)) (- width 2)) "\n")
                        'face (if (= i index) 'lsp-proxy-peek-selected-face 'shadow)) rows))
        (move-overlay overlay (line-end-position) (line-end-position))
        (overlay-put overlay 'window window)
        (overlay-put overlay 'after-string
                     (concat "\n" border
                             (lsp-proxy-peek--decorate-fringes
                              (concat
                               (propertize (concat (truncate-string-to-width
                                                    (lsp-proxy-peek--query-label query) width) "\n")
                                           'face 'lsp-proxy-peek-header-face)
                               (when status (concat (propertize (truncate-string-to-width status width)
								'face 'shadow) "\n"))
                               source (apply #'concat (nreverse rows))
                               (propertize
				(concat (lsp-proxy-peek--tail (lsp-proxy-peek--history query) width)
					(when (lsp-proxy-peek--location-branches location)
                                          (format "  branches %d/%d"
                                                  (1+ (lsp-proxy-peek--location-branch-index location))
                                                  (length (lsp-proxy-peek--location-branches location))))
					"\n") 'face 'lsp-proxy-peek-header-face))
                              window)
                             border))))))

(defun lsp-proxy-peek--post-command ()
  "Move the overlay with point; refresh code only when its source changes."
  (when-let* ((session lsp-proxy-peek--session)
              (overlay (lsp-proxy-peek--session-overlay session)))
    (if (not (lsp-proxy-peek--active-p session))
        (lsp-proxy-peek-abort)
      (move-overlay overlay (line-end-position) (line-end-position))
      (when (eq (window-buffer (selected-window)) (current-buffer))
        (setf (lsp-proxy-peek--session-window session) (selected-window))
        (overlay-put overlay 'window (selected-window))
        (lsp-proxy-peek--install-map session))
      (pcase-let ((`(,buffer ,tick ,width) (lsp-proxy-peek--session-preview session)))
        (when (and (buffer-live-p buffer)
                   (or (/= tick (buffer-chars-modified-tick buffer))
                       (/= width (window-body-width (selected-window)))))
          (setf (lsp-proxy-peek--session-picks session) nil)
          (lsp-proxy-peek--render session))))))

(defun lsp-proxy-peek--require-session ()
  "Return the active session or raise an actionable error."
  (unless (and lsp-proxy-peek--session
               (lsp-proxy-peek--active-p lsp-proxy-peek--session))
    (user-error "Start or restore a peek first"))
  lsp-proxy-peek--session)

(defun lsp-proxy-peek--selection-keys (count)
  "Return COUNT distinct equal-length key sequences."
  (let* ((alphabet "asdfghjkl") (base (length alphabet)) (digits 1) result)
    (while (< (expt base digits) count) (cl-incf digits))
    (dotimes (i count)
      (let ((n i) (key (make-string digits ?a)))
        (dotimes (d digits)
          (aset key (- digits d 1) (aref alphabet (% n base)))
          (setq n (/ n base)))
        (push key result)))
    (nreverse result)))

(defun lsp-proxy-peek--pick-symbol (session method)
  "Select a visible symbol asynchronously, then query METHOD for SESSION."
  (let* ((location (lsp-proxy-peek--current-location session))
         (marker (lsp-proxy-peek--location-marker location))
         (buffer (and marker (marker-buffer marker))) positions)
    (unless (buffer-live-p buffer) (user-error "Wait for the source preview to load"))
    (lsp-proxy-peek--invalidate session)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (pcase-let ((`(,beg . ,end) (lsp-proxy-peek--source-region location)))
            (goto-char beg)
            (while (re-search-forward "\\_<\\(?:\\sw\\|\\s_\\)+\\_>" end t)
              (let ((pos (match-beginning 0)))
                (unless (save-excursion (nth 8 (syntax-ppss pos)))
                  (push pos positions))))))))
    (unless positions (user-error "No symbols in the visible source"))
    (setf (lsp-proxy-peek--session-picks session)
          (cl-mapcar (lambda (key pos) (list key pos buffer (buffer-chars-modified-tick buffer)))
                     (lsp-proxy-peek--selection-keys (length positions)) (nreverse positions))
          (lsp-proxy-peek--session-pick-prefix session) ""
          (lsp-proxy-peek--session-pick-method session) method)
    (let ((map (make-sparse-keymap))
          (token (lsp-proxy-peek--session-token session)))
      (mapc (lambda (key) (define-key map (vector key) #'lsp-proxy-peek--select-key))
            (string-to-list "asdfghjkl"))
      (define-key map (kbd "C-g") #'lsp-proxy-peek--cancel-selection)
      (set-transient-map
       map (lambda () (and (lsp-proxy-peek--focused-p session)
                           (lsp-proxy-peek--active-p session token)
                           (memq this-command '(lsp-proxy-peek--select-key lsp-proxy-peek--cancel-selection))
                           (lsp-proxy-peek--session-picks session)))
       (lambda ()
         (when (and (lsp-proxy-peek--active-p session token)
                    (lsp-proxy-peek--session-picks session))
           (setf (lsp-proxy-peek--session-picks session) nil)
           (lsp-proxy-peek--render session)))))
    (lsp-proxy-peek--render session)
    (message "Peek %s: type the letters beside a symbol" (lsp-proxy-peek--method-label method))))

(defun lsp-proxy-peek--cancel-selection ()
  "Cancel symbol selection, keeping the preview."
  (interactive)
  (let ((session (lsp-proxy-peek--require-session)))
    (setf (lsp-proxy-peek--session-picks session) nil)
    (lsp-proxy-peek--render session)))

(defun lsp-proxy-peek--select-key ()
  "Consume a key in the nonblocking preview symbol selector."
  (interactive)
  (let* ((session (lsp-proxy-peek--require-session))
         (prefix (concat (lsp-proxy-peek--session-pick-prefix session)
                         (char-to-string last-command-event)))
         (picks (seq-filter (lambda (pick) (string-prefix-p prefix (car pick)))
                            (lsp-proxy-peek--session-picks session))))
    (cond
     ((null picks) (setf (lsp-proxy-peek--session-pick-prefix session) "")
      (message "No matching symbol; try again") (lsp-proxy-peek--render session))
     ((equal prefix (caar picks))
      (pcase-let ((`(,_ ,position ,buffer ,tick) (car picks)))
        (setf (lsp-proxy-peek--session-picks session) nil)
        (unless (and (buffer-live-p buffer) (= tick (buffer-chars-modified-tick buffer)))
          (lsp-proxy-peek--render session)
          (user-error "The source changed; select the symbol again"))
        (lsp-proxy-peek--query-at session buffer position
                                  (lsp-proxy-peek--session-pick-method session))))
     (t (setf (lsp-proxy-peek--session-pick-prefix session) prefix)
        (lsp-proxy-peek--render session)))))

(defun lsp-proxy-peek--start (method new-session)
  "Start METHOD at point, or continue the current peek unless NEW-SESSION."
  (if (and lsp-proxy-peek-mode (not new-session))
      (lsp-proxy-peek--pick-symbol (lsp-proxy-peek--require-session) method)
    (unless (and (bound-and-true-p lsp-proxy-mode) buffer-file-name)
      (user-error "Enable lsp-proxy-mode in a source buffer first"))
    (lsp-proxy--ensure-connection)
    (unless lsp-proxy--has-any-servers (user-error "No language server is available"))
    (when (and lsp-proxy-peek--last-session
               (buffer-live-p (lsp-proxy-peek--session-host lsp-proxy-peek--last-session)))
      (with-current-buffer (lsp-proxy-peek--session-host lsp-proxy-peek--last-session)
        (lsp-proxy-peek-abort)))
    (let* ((params (lsp-proxy--TextDocumentPositionParams))
           (position (plist-get params :position))
           (location (lsp-proxy-peek--location-create
                      :uri (plist-get (plist-get params :textDocument) :uri)
                      :range (list :start position :end position) :marker (point-marker)))
           (session (lsp-proxy-peek--session-create
                     :source (current-buffer) :connection lsp-proxy--connection
                     :context (lsp-proxy--build-params params)
                     :current (lsp-proxy-peek--query-create :label "Origin" :locations (vector location)))))
      (setq lsp-proxy-peek--session session lsp-proxy-peek--last-session session)
      (add-hook 'kill-buffer-hook #'lsp-proxy-peek--source-closed nil t)
      (add-hook 'change-major-mode-hook #'lsp-proxy-peek--source-closed nil t)
      (lsp-proxy-peek-mode 1)
      (lsp-proxy-peek--query-at session (current-buffer) (point) method))))

;;;###autoload
(defun lsp-proxy-peek-definition (&optional new-session)
  "Peek the definition at point, or a symbol selected inside the current peek.
With a prefix argument, start a new session at the editing cursor."
  (interactive "P") (lsp-proxy-peek--start 'textDocument/definition new-session))

;;;###autoload
(defun lsp-proxy-peek-references (&optional new-session)
  "Peek references at point, or a symbol selected inside the current peek.
With a prefix argument, start a new session at the editing cursor."
  (interactive "P") (lsp-proxy-peek--start 'textDocument/references new-session))

;;;###autoload
(defun lsp-proxy-peek-implementations (&optional new-session)
  "Peek implementations at point, or a symbol selected inside the current peek.
With a prefix argument, start a new session at the editing cursor."
  (interactive "P") (lsp-proxy-peek--start 'textDocument/implementation new-session))

;;;###autoload
(defun lsp-proxy-peek-type-definition (&optional new-session)
  "Peek a type definition at point or inside the current peek."
  (interactive "P") (lsp-proxy-peek--start 'textDocument/typeDefinition new-session))

;;;###autoload
(defun lsp-proxy-peek-declaration (&optional new-session)
  "Peek a declaration at point or inside the current peek."
  (interactive "P") (lsp-proxy-peek--start 'textDocument/declaration new-session))

(defun lsp-proxy-peek--move-result (delta)
  "Move DELTA results in the active query."
  (let* ((session (lsp-proxy-peek--require-session))
         (query (lsp-proxy-peek--session-current session)))
    (setf (lsp-proxy-peek--query-index query)
          (mod (+ (lsp-proxy-peek--query-index query) delta)
               (length (lsp-proxy-peek--query-locations query))))
    (lsp-proxy-peek--refresh session)))

(defun lsp-proxy-peek-next-result ()
  "Preview the next result." (interactive) (lsp-proxy-peek--move-result 1))
(defun lsp-proxy-peek-previous-result ()
  "Preview the previous result." (interactive) (lsp-proxy-peek--move-result -1))

(defun lsp-proxy-peek--scroll (delta)
  "Scroll the source preview DELTA lines."
  (let ((session (lsp-proxy-peek--require-session)))
    (cl-incf (lsp-proxy-peek--location-offset (lsp-proxy-peek--current-location session)) delta)
    (setf (lsp-proxy-peek--session-picks session) nil)
    (lsp-proxy-peek--render session)))
(defun lsp-proxy-peek-next-line ()
  "Scroll the preview down one line." (interactive) (lsp-proxy-peek--scroll 1))
(defun lsp-proxy-peek-previous-line ()
  "Scroll the preview up one line." (interactive) (lsp-proxy-peek--scroll -1))

(defun lsp-proxy-peek-backward ()
  "Return to the parent query, preserving its selected result and branches."
  (interactive)
  (let* ((session (lsp-proxy-peek--require-session))
         (query (lsp-proxy-peek--session-current session))
         (parent (lsp-proxy-peek--query-parent query)))
    (when parent
      (setf (lsp-proxy-peek--query-index parent)
            (cl-position (lsp-proxy-peek--query-parent-location query)
                         (lsp-proxy-peek--query-locations parent))
            (lsp-proxy-peek--session-current session) parent))
    (lsp-proxy-peek--refresh session)))

(defun lsp-proxy-peek-forward ()
  "Follow the selected branch of the currently previewed result."
  (interactive)
  (let* ((session (lsp-proxy-peek--require-session))
         (location (lsp-proxy-peek--current-location session))
         (branch (nth (lsp-proxy-peek--location-branch-index location)
                      (lsp-proxy-peek--location-branches location))))
    (when branch
      (setf (lsp-proxy-peek--session-current session) branch)
      (lsp-proxy-peek--refresh session))))

(defun lsp-proxy-peek--move-branch (delta)
  "Move DELTA sibling branches, or enter a branch from its parent result."
  (let* ((session (lsp-proxy-peek--require-session))
         (query (lsp-proxy-peek--session-current session))
         (location (or (lsp-proxy-peek--query-parent-location query)
                       (lsp-proxy-peek--current-location session)))
         (branches (lsp-proxy-peek--location-branches location)))
    (when branches
      (setf (lsp-proxy-peek--location-branch-index location)
            (mod (+ delta (lsp-proxy-peek--location-branch-index location)) (length branches))
            (lsp-proxy-peek--session-current session)
            (nth (lsp-proxy-peek--location-branch-index location) branches))
      (lsp-proxy-peek--refresh session))))
(defun lsp-proxy-peek-next-branch ()
  "Preview the next query branch." (interactive) (lsp-proxy-peek--move-branch 1))
(defun lsp-proxy-peek-previous-branch ()
  "Preview the previous query branch." (interactive) (lsp-proxy-peek--move-branch -1))

;;;###autoload
(defun lsp-proxy-peek-abort ()
  "Close the peek UI, cancel pending work and retain reading history."
  (interactive)
  (when lsp-proxy-peek-mode (lsp-proxy-peek-mode -1)))

(defun lsp-proxy-peek--source-closed ()
  "Close the active UI when its original source stops being managed."
  (when (and lsp-proxy-peek--last-session
             (eq (current-buffer) (lsp-proxy-peek--session-source lsp-proxy-peek--last-session))
             (buffer-live-p (lsp-proxy-peek--session-host lsp-proxy-peek--last-session)))
    (with-current-buffer (lsp-proxy-peek--session-host lsp-proxy-peek--last-session)
      (lsp-proxy-peek-abort))))

;;;###autoload
(defun lsp-proxy-peek-restore ()
  "Restore the last reading session below point in the current buffer."
  (interactive)
  (let ((session lsp-proxy-peek--last-session))
    (unless (and session (buffer-live-p (lsp-proxy-peek--session-source session))
                 (buffer-local-value 'lsp-proxy-mode (lsp-proxy-peek--session-source session)))
      (user-error "No live peek session to restore"))
    (unless (eq (lsp-proxy-peek--session-connection session) lsp-proxy--connection)
      (user-error "The proxy connection changed; start a new peek"))
    (when (buffer-live-p (lsp-proxy-peek--session-host session))
      (with-current-buffer (lsp-proxy-peek--session-host session) (lsp-proxy-peek-abort)))
    (setq lsp-proxy-peek--session session)
    (lsp-proxy-peek-mode 1)
    (lsp-proxy-peek--refresh session)))

(defun lsp-proxy-peek-jump (&optional other-window)
  "Open the previewed location, pushing the editing cursor onto the xref stack.
With a prefix argument, use another window."
  (interactive "P")
  (let* ((session (lsp-proxy-peek--require-session))
         (marker (lsp-proxy-peek--location-marker (lsp-proxy-peek--current-location session))))
    (unless (and marker (marker-buffer marker)) (user-error "Wait for the source preview to load"))
    (let ((buffer (marker-buffer marker)) (position (marker-position marker)))
      (xref-push-marker-stack)
      ;; Retain the destination buffer when closing the UI.
      (setf (lsp-proxy-peek--session-buffers session)
            (delq buffer (lsp-proxy-peek--session-buffers session)))
      (lsp-proxy-peek-abort)
      (if other-window (switch-to-buffer-other-window buffer) (switch-to-buffer buffer))
      (goto-char position))))

(provide 'lsp-proxy-peek)
;;; lsp-proxy-peek.el ends here
