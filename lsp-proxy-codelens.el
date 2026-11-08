;;; lsp-proxy-codelens.el --- CodeLens support for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; CodeLens functionality for lsp-proxy.  Displays CodeLens information as
;; overlays above code and supports interaction via mouse clicks and
;; keyboard.  Modeled after `lsp-lens.el' from lsp-mode
;; (https://github.com/emacs-lsp/lsp-mode): on every refresh all lens
;; overlays are deleted and rebuilt from scratch at positions computed
;; directly from the LSP coordinates, rather than trying to reuse and
;; reposition overlays across edits.  That trades a full rebuild per
;; refresh for never having to reason about overlay drift, which is a far
;; better trade here since lenses refresh at most once per debounce tick.
;;
;; Adapted for lsp-proxy's transport: every request goes through the single
;; proxy connection instead of talking to a language server directly, so
;; each CodeLens item round-trips through the proxy wrapped with the
;; originating server's id (`:lsp_item'/`:language_server_id'
;; /`:language_server_name'), the same envelope lsp-proxy already uses for
;; code actions.

;;; Code:

(require 'cl-lib)
(require 'pulse)
(require 'seq)
(require 'lsp-proxy-utils)
(require 'lsp-proxy-core)

(require 'nerd-icons nil t)

;;; External variables from lsp-proxy-core.el
(defvar lsp-proxy--support-code-lens)
(defvar lsp-proxy-mode)

;;; External functions
(declare-function lsp-proxy--execute-command "lsp-proxy")
(declare-function lsp-proxy--process-locations "lsp-proxy-xref")
(declare-function eglot--lsp-position-to-point "ext:eglot")

;;; Customization Options

(defcustom lsp-proxy-codelens-mode-config t
  "Configuration for enabling codelens mode in specific contexts.
The value can be:
- nil: Disable codelens completely
- t:   Enable codelens for all buffers
- (mode1 mode2 ...): Enable only for specified major modes
Example: `(`java-mode' `java-ts-mode')'"
  :type
  '(choice
    (const :tag "Disabled" nil)
    (const :tag "Enabled for all buffers" t)
    (repeat :tag "Enabled only for specific modes" symbol))
  :group 'lsp-proxy)

(defcustom lsp-proxy-codelens-debounce-interval 0.001
  "Debounce interval for refreshing CodeLens after document changes."
  :type 'float
  :group 'lsp-proxy)

(defcustom lsp-proxy-codelens-place-position 'avove-of-line
  "The position to place a CodeLens relative to its LSP range.
`end-of-line' appends the lens to the end of its code line (via an
`after-string'), the style VS Code/eglot use by default.  `above-line'
instead puts the lens on its own line directly above the code, aligned
to that code's indentation (via a `before-string'), the style jdtls/VS
Code's reference CodeLens UI traditionally used."
  :type '(choice (const above-line)
                 (const end-of-line))
  :group 'lsp-proxy)

(defcustom lsp-proxy-codelens-icon-height 0.8
  "Height (relative to the frame's default) for nerd-icons codicon glyphs.
Passed as `nerd-icons-codicon''s :height in
`lsp-proxy-codelens--codicons-to-nerd-icons'. Kept smaller than 1.0 by
default so the icon doesn't dominate the (already shrunk,
`lsp-proxy-codelens-face') label text next to it."
  :type 'float
  :group 'lsp-proxy)

(defcustom lsp-proxy-codelens-icon-v-adjust 0.2
  "Vertical shift for nerd-icons codicon glyphs, as a fraction of line height.
Passed as `nerd-icons-codicon''s :v-adjust (an Emacs `(raise N)' display
property: positive raises the glyph, negative lowers it) in
`lsp-proxy-codelens--codicons-to-nerd-icons'. Shrinking the glyph via
`lsp-proxy-codelens-icon-height' without also nudging this tends to
leave it looking off-baseline next to the label text; there's no
value that's exactly right for every font/height combination, so
adjust to taste if the default doesn't look centered for you."
  :type 'float
  :group 'lsp-proxy)

;;; Faces

(defface lsp-proxy-codelens-face
  '((t :inherit font-lock-comment-face :height 0.9))
  "Face for displaying CodeLens information."
  :group 'lsp-proxy)

(defface lsp-proxy-codelens-mouse-face
  '((t :inherit highlight :box (:line-width 1 :color "gray")))
  "Face for CodeLens when mouse is over them."
  :group 'lsp-proxy)

;;; Core Data Structures

;; Forward declaration so functions defined before `define-minor-mode' below
;; don't warn about referencing it.
(defvar-local lsp-proxy-codelens-mode nil)

(defvar-local lsp-proxy-codelens--overlays nil
  "Lens overlays currently displayed in the buffer.")

(defvar-local lsp-proxy-codelens--last-count nil
  "The number of lenses the last time they were rendered.
Used so a change in count (e.g. a lens disappearing) is always
re-rendered, even when every remaining lens is already resolved.")

(defvar-local lsp-proxy-codelens--refresh-timer nil
  "Debounce timer for `lsp-proxy-codelens-refresh'.")

(defvar-local lsp-proxy-codelens--backend-cache nil
  "Cache of the raw CodeLens items from the last `textDocument/codeLens'.
Each element is a plist with `:lsp_item'/`:language_server_id'
/`:language_server_name', mutated in place as resolve requests complete.")

(defsubst lsp-proxy-codelens--item-lsp (item)
  "Return the raw LSP CodeLens carried by ITEM."
  (plist-get item :lsp_item))

(defsubst lsp-proxy-codelens--item-ls-id (item)
  "Return the language-server id that produced ITEM."
  (plist-get item :language_server_id))

(defsubst lsp-proxy-codelens--item-command (item)
  "Return ITEM's resolved `:command', or nil if not yet resolved."
  (plist-get (lsp-proxy-codelens--item-lsp item) :command))

(defsubst lsp-proxy-codelens--item-range (item)
  "Return ITEM's LSP range."
  (plist-get (lsp-proxy-codelens--item-lsp item) :range))

(defun lsp-proxy-codelens--item-point (item)
  "Return the buffer position (point) where ITEM's range starts."
  (eglot--lsp-position-to-point (plist-get (lsp-proxy-codelens--item-range item) :start)))

(defun lsp-proxy-codelens--item-line (item)
  "Return ITEM's 0-based LSP start line."
  (plist-get (plist-get (lsp-proxy-codelens--item-range item) :start) :line))

;;; LSP Protocol Handlers

(cl-defgeneric lsp-proxy-codelens-provide-codelens (codelens uri)
  "Generic method for providing CodeLens data with middleware support.

CODELENS is the list/vector of wrapped CodeLens items from the proxy
\(each with `:lsp_item'/`:language_server_id'/`:language_server_name').
URI is the document URI string.

This generic method is called after fetching CodeLens from the proxy but
before building the cache.  Users can define methods to filter, transform,
or extend CodeLens data.

Default method returns CODELENS unchanged."
  (:method (codelens _uri) codelens))

(defun lsp-proxy-codelens--window ()
  "Return the window whose visible range should drive CodeLens rendering."
  (let ((selected (selected-window)))
    (if (eq (window-buffer selected) (current-buffer))
        selected
      (car (get-buffer-window-list (current-buffer) nil t)))))

(defun lsp-proxy-codelens--not-loaded-p (item)
  "Return non-nil when ITEM is visible but still needs `codeLens/resolve'."
  (let ((window (lsp-proxy-codelens--window)))
    (and window
         (not (lsp-proxy-codelens--item-command item))
         (not (plist-get item :_pending))
         (< (window-start window) (lsp-proxy-codelens--item-point item) (window-end window t)))))

(defun lsp-proxy-codelens--present-p (item)
  "Return non-nil when ITEM is resolved or currently outside the window."
  (let ((window (lsp-proxy-codelens--window)))
    (or (lsp-proxy-codelens--item-command item)
        (not window)
        (not (< (window-start window) (lsp-proxy-codelens--item-point item) (window-end window t))))))

(defun lsp-proxy-codelens--fetch-missing (items callback docver)
  "Resolve ITEMS still missing a `:command' that are inside the window.
CALLBACK is invoked with ITEMS and DOCVER once every item in ITEMS is
either resolved or out of the window again."
  (let ((buf (current-buffer)))
    (dolist (item (seq-filter #'lsp-proxy-codelens--not-loaded-p items))
      (plist-put item :_pending t)
      (lsp-proxy--async-request
       'codeLens/resolve
       (lsp-proxy--build-params
        (lsp-proxy-codelens--item-lsp item)
        `(:context (:language-server-id ,(lsp-proxy-codelens--item-ls-id item))))
       :success-fn (lambda (resolved)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (plist-put item :_pending nil)
                         (plist-put (lsp-proxy-codelens--item-lsp item) :command
                                    (lsp-proxy-codelens--item-command resolved))
                         (when (seq-every-p #'lsp-proxy-codelens--present-p items)
                           (funcall callback items docver)))))
       ;; Degrade quietly. The default handlers report to the user, which is wrong
       ;; twice over here: a resolve routinely fails while a server is still
       ;; starting up, and one lens failing is not something to interrupt anyone
       ;; about. An unresolved lens already renders as nothing, so failing is
       ;; simply the lens not appearing.
       :error-fn (lambda (err)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf (plist-put item :_pending nil)))
                   (lsp-proxy-log "codeLens/resolve failed: %s"
                                  (or (plist-get err :message) err)))
       :timeout-fn (lambda (method)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf (plist-put item :_pending nil)))
                     (lsp-proxy-log "%s timed out" method))))))

(defun lsp-proxy-codelens--backend (modified-p callback)
  "Fetch or resolve CodeLens, then call CALLBACK with items and docver.
MODIFIED-P is non-nil when the buffer changed since the last refresh, in
which case the CodeLens list itself is re-fetched; otherwise the existing
`lsp-proxy-codelens--backend-cache' is just re-checked for newly-visible
unresolved items (e.g. after scrolling)."
  (if modified-p
      (let ((docver (lsp-proxy--doc-version))
            (buf (current-buffer)))
        (setq lsp-proxy-codelens--backend-cache nil)
        (lsp-proxy--async-request
         'textDocument/codeLens
         (lsp-proxy--build-params (list :textDocument (lsp-proxy--TextDocumentIdentifier)))
         :success-fn
         (lambda (codelens-list)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (when (and lsp-proxy-codelens-mode
                          (eq docver (lsp-proxy--doc-version)))
                 (let ((uri (plist-get (lsp-proxy--TextDocumentIdentifier) :uri)))
                   (setq codelens-list (lsp-proxy-codelens-provide-codelens codelens-list uri)))
                 (setq lsp-proxy-codelens--backend-cache (append codelens-list nil))
                 (if (seq-every-p #'lsp-proxy-codelens--item-command lsp-proxy-codelens--backend-cache)
                     (funcall callback lsp-proxy-codelens--backend-cache docver)
                   (lsp-proxy-codelens--fetch-missing lsp-proxy-codelens--backend-cache callback docver))))))
         :error-fn #'ignore))
    (if (seq-every-p #'lsp-proxy-codelens--present-p lsp-proxy-codelens--backend-cache)
        (funcall callback lsp-proxy-codelens--backend-cache (lsp-proxy--doc-version))
      (lsp-proxy-codelens--fetch-missing lsp-proxy-codelens--backend-cache callback
                                          (lsp-proxy--doc-version)))))

;;; UI Display System

(declare-function nerd-icons-codicon "nerd-icons" (icon-name &rest _args))

(defun lsp-proxy-codelens--codicons-to-nerd-icons (title)
  "Convert VS Code icon placeholders in TITLE to nerd icons.
VS Code icon placeholder syntax: $(icon-name)
Converts to nerd icons using `nerd-icons-codicon'. Also swallows one
trailing space after the placeholder, so the icon glyph sits directly
against the label instead of VS Code's \"$(icon) Label\" spacing.
If the icon is not recognized, returns the original placeholder
\(including that trailing space, so the raw text still reads fine)."
  (if (featurep 'nerd-icons)
      (replace-regexp-in-string
       "\\$(\\([^)]+\\)) ?"
       (lambda (match)
         (let* ((icon-name (match-string 1 match))
                (nerd-icon-name (replace-regexp-in-string "-" "_" icon-name))
                (icon-code (format "nf-cod-%s" nerd-icon-name)))
           (condition-case _
               (nerd-icons-codicon icon-code
                                   :height lsp-proxy-codelens-icon-height
                                   :v-adjust lsp-proxy-codelens-icon-v-adjust)
             (error
              ;; If nerd-icons-codicon fails, return the original placeholder
              match))))
       title
       nil
       t)
    title))

(defun lsp-proxy-codelens--keymap (item)
  "Build the mouse keymap for ITEM's lens token."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
      (lambda () (interactive) (lsp-proxy-codelens-execute item)))
    map))

(defun lsp-proxy-codelens--token (item)
  "Return the propertized display token for resolved ITEM."
  (let* ((command (lsp-proxy-codelens--item-command item))
         (title (lsp-proxy-codelens--codicons-to-nerd-icons (plist-get command :title))))
    (propertize title
                'face 'lsp-proxy-codelens-face
                'mouse-face 'lsp-proxy-codelens-mouse-face
                'help-echo "Click to execute this CodeLens command"
                'keymap (lsp-proxy-codelens--keymap item))))

(defun lsp-proxy-codelens--text-width (from to)
  "Measure the width of the text between FROM and TO.
Results are meaningful only if FROM and TO are on the same line.
`current-column' is used instead of raw character count so
prettified/composed glyphs (e.g. ligatures) still measure correctly."
  (- (save-excursion (goto-char to) (current-column))
     (save-excursion (goto-char from) (current-column))))

(defun lsp-proxy-codelens--overlay-ensure-at (pos)
  "Find or create a lens overlay for the line at POS.
In `end-of-line' mode the overlay is a zero-width marker at the line's
end, carrying the lens as an `after-string'.  In `above-line' mode it
spans the whole line (plus its trailing newline) so a `before-string'
can insert a line above it without the lens being pulled along when
that line is edited."
  (save-excursion
    (goto-char pos)
    (let ((eol (line-end-position)))
      (if (eq lsp-proxy-codelens-place-position 'end-of-line)
          (make-overlay eol eol nil t t)
        (make-overlay (line-beginning-position) (1+ eol) nil t t)))))

(defun lsp-proxy-codelens--show (str pos items)
  "Display STR at POS as a lens overlay carrying ITEMS."
  (let ((ov (lsp-proxy-codelens--overlay-ensure-at pos)))
    (overlay-put ov 'lsp-proxy-codelens t)
    (overlay-put ov 'lsp-proxy-codelens-items items)
    (if (eq lsp-proxy-codelens-place-position 'end-of-line)
        (overlay-put ov 'after-string (propertize (concat " " str) 'cursor t))
      ;; Indent the lens line to match the code it sits above, the same
      ;; way the old above-line implementation reserved space for it.
      (let ((indent (make-string
                     (lsp-proxy-codelens--text-width
                      (save-excursion (goto-char pos) (beginning-of-visual-line) (point))
                      (save-excursion (goto-char pos) (beginning-of-line-text) (point)))
                     ?\s)))
        (overlay-put ov 'before-string (concat indent str "\n"))))
    ov))

(defun lsp-proxy-codelens--display (items &optional _docver)
  "Render ITEMS (a flat list of wrapped CodeLens) as lens overlays.
Called as the callback of `lsp-proxy-codelens--backend'/`--fetch-missing',
which also pass the docver the data was fetched for; that version match
is already enforced upstream (in the fetch's `:success-fn'), so _DOCVER
is accepted here only to match the callback signature, not used.

Mirrors `lsp-lens--display': rerenders when any ITEM hasn't been through
this function yet (marked via `:_processed' on first pass) or when the
lens count is unchanged from last time, and skips an otherwise-redundant
rebuild when the count differs but every item here has already been
processed once before."
  (let ((scroll-preserve-screen-position t))
    (when (or (seq-some (lambda (item) (not (plist-get item :_processed))) items)
              (eq (length items) lsp-proxy-codelens--last-count)
              (not items))
      (setq lsp-proxy-codelens--last-count (length items))
      (mapc #'delete-overlay lsp-proxy-codelens--overlays)
      (setq lsp-proxy-codelens--overlays
            (thread-last
              items
              (seq-filter #'lsp-proxy-codelens--item-command)
              (mapcar (lambda (item) (plist-put item :_processed t)))
              (seq-group-by #'lsp-proxy-codelens--item-line)
              (mapcar
               (lambda (line-items)
                 (let* ((sorted (seq-sort-by
                                 (lambda (item)
                                   (plist-get (plist-get (lsp-proxy-codelens--item-range item) :start)
                                             :character))
                                 #'<
                                 (cdr line-items)))
                        (tokens (mapcar #'lsp-proxy-codelens--token sorted)))
                   (lsp-proxy-codelens--show
                    (mapconcat #'identity tokens (propertize "|" 'face 'lsp-proxy-codelens-face))
                    (lsp-proxy-codelens--item-point (car sorted))
                    sorted)))))))))

;;; Resolve / refresh orchestration

(defun lsp-proxy-codelens-refresh (&optional modified-p)
  "Refresh CodeLens display for the current buffer.
MODIFIED-P is non-nil when a buffer edit (rather than a scroll or an
idle tick) is what triggered this refresh."
  (when (and lsp-proxy-codelens-mode (buffer-live-p (current-buffer)))
    (lsp-proxy-codelens--backend modified-p #'lsp-proxy-codelens--display)))

(defun lsp-proxy-codelens--schedule-refresh (&optional modified-p)
  "Debounce a call to `lsp-proxy-codelens-refresh' with MODIFIED-P."
  (when lsp-proxy-codelens--refresh-timer
    (cancel-timer lsp-proxy-codelens--refresh-timer))
  (setq lsp-proxy-codelens--refresh-timer
        (run-with-timer
         lsp-proxy-codelens-debounce-interval nil
         (lambda (buf modified-p)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (setq lsp-proxy-codelens--refresh-timer nil)
               (lsp-proxy-codelens-refresh modified-p))))
         (current-buffer) modified-p)))

(defun lsp-proxy-codelens--on-document-change (&rest _args)
  "Handle document modification by scheduling a debounced CodeLens refresh."
  (lsp-proxy-codelens--schedule-refresh t))

(defun lsp-proxy-codelens--on-scroll (&rest _args)
  "Handle window scroll/resize by scheduling a non-modifying CodeLens refresh."
  (lsp-proxy-codelens--schedule-refresh nil))

(defun lsp-proxy-codelens-refresh-all ()
  "Re-fetch CodeLens in every buffer currently showing them.

Called when a server sends `workspace/codeLens/refresh\'.  Nothing else
signals that lenses went stale, and the stale answer is typically the one a
server gave while it was still starting up -- jdtls answers
`textDocument/codeLens\' during project import, with lenses it cannot yet
resolve.  Without this the buffer keeps that first answer until it is
reverted by hand.

Every such buffer is refreshed rather than only those belonging to the server
that asked: the LSP request carries no parameters, so there is nothing to scope
by, and a refresh arrives about once per project import."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (bound-and-true-p lsp-proxy-codelens-mode)
                   (bound-and-true-p lsp-proxy-mode))
          (lsp-proxy-codelens-refresh t))))))

;;; Interaction

(defun lsp-proxy-codelens-execute (item)
  "Execute CodeLens command carried by ITEM.
If ITEM is not yet resolved, trigger the resolve process instead."
  (let* ((command (lsp-proxy-codelens--item-command item))
         (command-name (plist-get command :command))
         (arguments (plist-get command :arguments)))
    (cond
     ;; These are client-side commands in vscode-java.  The resolved CodeLens
     ;; already carries the target locations as its third argument; VS Code
     ;; forwards them to `editor.action.showReferences', and we do the same via
     ;; xref instead of sending an unknown command back to jdtls.
     ((and command
           (member command-name '("java.show.references" "java.show.implementations")))
      (if-let* ((locations (and (sequencep arguments)
                                (> (length arguments) 2)
                                (seq-elt arguments 2))))
          (lsp-proxy--process-locations locations)
        (message "No CodeLens locations available")))
     ;; The IntelliJ Run/Debug lens is a client-side-only command in the VS
     ;; Code extension (its DAP integration registers a matching local
     ;; command and never forwards it to the server) — sending it through
     ;; `workspace/executeCommand' just gets back "Unknown command". Route
     ;; it to `lsp-proxy-java''s dape integration instead, when available.
     ((and command (equal command-name "intellij.jvm.runMain"))
      (if (fboundp 'lsp-proxy-java-dape-run-main)
          (lsp-proxy-java-dape-run-main arguments)
        (lsp-proxy--error "%s" "Run/Debug from CodeLens needs `dape' installed; see the Debugging section in lsp-proxy-java.el")))
     ;; Execute resolved command
     (command
      (lsp-proxy--execute-command
       command-name
       arguments
       (lsp-proxy-codelens--item-ls-id item)))
     ;; No command yet: try to resolve. lsp-proxy's capability model only
     ;; tracks "some server supports codeLens" rather than a per-server
     ;; resolveProvider flag, so this just attempts the resolve request and
     ;; lets it fail loudly if the server doesn't actually support it.
     (lsp-proxy-codelens-mode
      (lsp-proxy-codelens--fetch-missing (list item) #'lsp-proxy-codelens--display
                                         (lsp-proxy--doc-version))
      (message "Resolving CodeLens command..."))
     (t
      (message "CodeLens command not available")))))

;;;###autoload
(defun lsp-proxy-codelens-execute-at-line (line)
  "Execute CodeLens at LINE with a visual flash effect on the line.

When called interactively, LINE is the current line.
When called from Elisp, LINE must be provided.

If there's only one CodeLens at the line, execute it directly.
If there are multiple, show a selection menu for user to choose."
  (interactive (list (line-number-at-pos (point) t)))
  (let ((items (seq-filter
                (lambda (item) (= (1+ (lsp-proxy-codelens--item-line item)) line))
                lsp-proxy-codelens--backend-cache)))
    (if items
        (progn
          ;; Flash the line to indicate execution
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- line))
            (pulse-momentary-highlight-one-line (point)))
          (if (= (length items) 1)
              (lsp-proxy-codelens-execute (car items))
            (let* ((choices (cl-loop for item in items
                                     for index from 0
                                     collect (cons
                                              (format "[%d] %s" index
                                                     (or (plist-get (lsp-proxy-codelens--item-command item) :title)
                                                         "(unresolved)"))
                                              item)))
                   (vertico-sort-function nil) ;; No sorting if using vertico
                   (selected (cdr (assoc
                                   (completing-read (format "CodeLens (L%d): " line) choices)
                                   choices))))
              (when selected
                (lsp-proxy-codelens-execute selected)))))
      (message (format "No CodeLens found at line %d." line)))))

;;;###autoload
(defun lsp-proxy-codelens-execute-dwim ()
  "Do-What-I-Mean: Execute CodeLens at or above the current line.

If the current line has CodeLens, execute it.
Otherwise, scan backwards to find the nearest line with CodeLens and execute it.
If no CodeLens is found, show a message."
  (interactive)
  (let ((current-line (line-number-at-pos (point) t))
        (lines (sort (delete-dups
                      (mapcar (lambda (item) (1+ (lsp-proxy-codelens--item-line item)))
                             lsp-proxy-codelens--backend-cache))
                     #'>)))
    (if-let* ((line (seq-find (lambda (l) (<= l current-line)) lines)))
        (lsp-proxy-codelens-execute-at-line line)
      (message "No CodeLens found before line %d." current-line))))

;;; Integration

(defun lsp-proxy-codelens--hide ()
  "Delete all lens overlays."
  (let ((scroll-preserve-screen-position t))
    (mapc #'delete-overlay lsp-proxy-codelens--overlays)
    (setq lsp-proxy-codelens--overlays nil)))

(defun lsp-proxy-codelens--setup-buffer ()
  "Setup CodeLens for current buffer."
  (when lsp-proxy-codelens-mode
    ;; `lsp-proxy-activate-codelens-mode' can be called more than once as
    ;; capabilities are refreshed.  Clear stale overlays/timers before
    ;; reinitializing buffer-local state, so re-setup never leaves orphan
    ;; after-strings behind.
    (lsp-proxy-codelens--cleanup-buffer)
    (setq lsp-proxy-codelens--backend-cache nil
          lsp-proxy-codelens--last-count nil)
    (add-hook 'after-change-functions #'lsp-proxy-codelens--on-document-change nil t)
    (add-hook 'after-revert-hook #'lsp-proxy-codelens--on-document-change nil t)
    (add-hook 'window-scroll-functions #'lsp-proxy-codelens--on-scroll nil t)
    (add-hook 'window-configuration-change-hook #'lsp-proxy-codelens--on-scroll nil t)))

(defun lsp-proxy-codelens--cleanup-buffer ()
  "Cleanup CodeLens for current buffer."
  (when lsp-proxy-codelens--refresh-timer
    (cancel-timer lsp-proxy-codelens--refresh-timer)
    (setq lsp-proxy-codelens--refresh-timer nil))

  (lsp-proxy-codelens--hide)

  (setq lsp-proxy-codelens--backend-cache nil
        lsp-proxy-codelens--last-count nil)

  (remove-hook 'after-change-functions #'lsp-proxy-codelens--on-document-change t)
  (remove-hook 'after-revert-hook #'lsp-proxy-codelens--on-document-change t)
  (remove-hook 'window-scroll-functions #'lsp-proxy-codelens--on-scroll t)
  (remove-hook 'window-configuration-change-hook #'lsp-proxy-codelens--on-scroll t))

;;; Minor Mode Definition

(define-minor-mode lsp-proxy-codelens-mode
  "Minor mode for displaying LSP CodeLens for lsp-proxy."
  :lighter nil
  :group 'lsp-proxy
  (cond
   (lsp-proxy-codelens-mode
    (if lsp-proxy--support-code-lens
        (progn
          (lsp-proxy-codelens--setup-buffer)
          (lsp-proxy-codelens-refresh t))
      (lsp-proxy-codelens-mode -1)))
   (t
    (lsp-proxy-codelens--cleanup-buffer))))

(defun lsp-proxy-activate-codelens-mode ()
  "Activate `lsp-proxy-codelens-mode' for the current buffer
if `lsp-proxy-codelens-mode-config' allows it."
  (when (and lsp-proxy--support-code-lens
             (boundp 'lsp-proxy-codelens-mode-config)
             (or (eq lsp-proxy-codelens-mode-config t)
                 (and (listp lsp-proxy-codelens-mode-config)
                      (member major-mode lsp-proxy-codelens-mode-config)))
             (not (bound-and-true-p lsp-proxy-codelens-mode)))
    (lsp-proxy-codelens-mode 1)))

(provide 'lsp-proxy-codelens)
;;; lsp-proxy-codelens.el ends here
