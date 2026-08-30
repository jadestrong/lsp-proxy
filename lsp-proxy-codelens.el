;;; lsp-proxy-codelens.el --- CodeLens support for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; CodeLens functionality for lsp-proxy.  Displays CodeLens information as
;; overlays above code and supports interaction via mouse clicks and
;; keyboard.  Adapted from the third-party `eglot-codelens' package
;; (https://github.com/zsxh/eglot-codelens) for lsp-proxy's transport: every
;; request goes through the single proxy connection instead of talking to a
;; language server directly, so each CodeLens item round-trips through the
;; proxy wrapped with the originating server's id
;; (`:lsp_item'/`:language_server_id'/`:language_server_name'), the same
;; envelope lsp-proxy already uses for code actions.

;;; Code:

(require 'cl-lib)
(require 'pulse)
(require 'lsp-proxy-utils)
(require 'lsp-proxy-core)

(require 'nerd-icons nil t)

;;; External variables from lsp-proxy-core.el
(defvar lsp-proxy--support-code-lens)
(defvar lsp-proxy--recent-changes)
(defvar lsp-proxy-mode)

;;; External functions
(declare-function lsp-proxy--execute-command "lsp-proxy")

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

(defcustom lsp-proxy-codelens-update-delay 0.5
  "Delay in seconds before updating CodeLens after document changes."
  :type 'float
  :group 'lsp-proxy)

(defcustom lsp-proxy-codelens-visible-refresh-delay 0.25
  "Delay in seconds before refreshing visible CodeLens after window changes.
This applies to scroll events and window configuration changes."
  :type 'float
  :group 'lsp-proxy)

(defcustom lsp-proxy-codelens-resolve-delay 0.25
  "Delay in seconds between processing each CodeLens resolve request.
This controls the rate at which pending resolve requests are processed
to avoid overwhelming the language server."
  :type 'float
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

;;; Core Data Structures and Cache Management

;; Forward declaration so functions defined before `define-minor-mode' below
;; don't warn about referencing it.
(defvar-local lsp-proxy-codelens-mode nil)

(defvar-local lsp-proxy-codelens--cache nil
  "Cache for CodeLens items grouped by line in current buffer.
A hash table with line numbers as keys.
Each value is a SORTED list of CODELENS-OVERLAY-CELL where
CODELENS-OVERLAY-CELL is (ITEM . OVERLAY).  ITEM is a plist with
`:lsp_item' (the raw LSP CodeLens), `:language_server_id' and
`:language_server_name'.")

(defvar-local lsp-proxy-codelens--prev-line-count nil
  "Previous document line count for calculating offset during line changes.
Used to compute delta when reusing overlays across document edits.")

(defvar-local lsp-proxy-codelens--version nil
  "Document version for cached CodeLens.")

(defvar-local lsp-proxy-codelens--recent-changes nil
  "Recent buffer changes as collected from `lsp-proxy--recent-changes'.")

(defvar-local lsp-proxy-codelens--update-timer nil
  "Timer for delayed CodeLens updates.")

(defvar-local lsp-proxy-codelens--refresh-timer nil
  "Timer for delayed CodeLens refresh on window visible changes.")

(defvar-local lsp-proxy-codelens--pending-lines nil
  "List of line numbers with pending CodeLens to be rendered.
This is used to track which lines need processing during partial updates.")

(defvar-local lsp-proxy-codelens--resolve-queue nil
  "Queue of pending CodeLens resolve requests.
Each element is (DOCVER . CODELENS-CELL) where CODELENS-CELL is
\(ITEM . OVERLAY).
This is used to batch resolve requests with debouncing.")

(defvar-local lsp-proxy-codelens--resolve-timer nil
  "Timer for processing CodeLens resolve queue.")

(defvar-local lsp-proxy-codelens--overlays nil
  "List of all CodeLens overlays in current buffer.
This is used to efficiently clean up stale overlays without traversing
the entire buffer's overlay list.")

(defsubst lsp-proxy-codelens--item-lsp (item)
  "Return the raw LSP CodeLens carried by ITEM."
  (plist-get item :lsp_item))

(defsubst lsp-proxy-codelens--item-ls-id (item)
  "Return the language-server id that produced ITEM."
  (plist-get item :language_server_id))

(defun lsp-proxy-codelens--build-cache (codelens-list)
  "Build cache from CODELENS-LIST.
CODELENS-LIST is a vector of items as returned by the proxy (each a plist
with `:lsp_item'/`:language_server_id'/`:language_server_name').
Returns a hash table with line numbers as keys.
Each value is a sorted list of CODELENS-OVERLAY-CELL where
CODELENS-OVERLAY-CELL is (ITEM . OVERLAY)."
  (when (and codelens-list (vectorp codelens-list) (length> codelens-list 0))
    (let ((line-groups (make-hash-table :test 'eq)))
      ;; Group CodeLens by line number
      (cl-loop for item across codelens-list
               when (and (listp item) (plist-get (lsp-proxy-codelens--item-lsp item) :range))
               for range = (plist-get (lsp-proxy-codelens--item-lsp item) :range)
               for start = (when range (plist-get range :start))
               for line-num = (when start (plist-get start :line))
               when (and (integerp line-num) (>= line-num 0))
               ;; Position in a text document expressed as zero-based line and zero-based character offset.
               do (push (cons item nil) (gethash (1+ line-num) line-groups)))

      ;; Reverse each list to maintain LSP order (sorted by index)
      (maphash (lambda (line codelens-on-line)
                 (puthash line (nreverse codelens-on-line) line-groups))
               line-groups)
      line-groups)))

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

(defun lsp-proxy-codelens--resolve-codelens (codelens-cell)
  "Resolve CODELENS-CELL and update its overlay.
Makes an async request to `codeLens/resolve'.
CODELENS-CELL is a cons cell (ITEM . OVERLAY)."
  (when-let* ((item (car codelens-cell))
              (ov (cdr codelens-cell))
              (buf (current-buffer)))
    (when (and (overlayp ov) (overlay-buffer ov))
      (lsp-proxy--async-request
       'codeLens/resolve
       (lsp-proxy--build-params
        (lsp-proxy-codelens--item-lsp item)
        `(:context (:language-server-id ,(lsp-proxy-codelens--item-ls-id item))))
       :success-fn (lambda (resolved)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (lsp-proxy-codelens--update-resolved-codelens codelens-cell resolved))))))))

(defun lsp-proxy-codelens--resolve-schedule ()
  "Schedule processing of the CodeLens resolve queue.
Starts processing the queue immediately and sets up a timer to pace
subsequent requests at `lsp-proxy-codelens-resolve-delay' intervals.
If a timer is already running, does nothing (prevents duplicate scheduling)."
  (unless (and lsp-proxy-codelens--resolve-timer
               (timerp lsp-proxy-codelens--resolve-timer))
    (lsp-proxy-codelens--resolve-process-queue)
    (setq lsp-proxy-codelens--resolve-timer
          (run-with-timer
           lsp-proxy-codelens-resolve-delay nil
           (lambda (buf)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when lsp-proxy-codelens--resolve-timer
                   (cancel-timer lsp-proxy-codelens--resolve-timer))
                 (setq lsp-proxy-codelens--resolve-timer nil)
                 (when lsp-proxy-codelens--resolve-queue
                   (lsp-proxy-codelens--resolve-schedule)))))
           (current-buffer)))))

(defun lsp-proxy-codelens--resolve-process-queue ()
  "Process one item from the resolve queue.
Each item is (DOCVER . CODELENS-CELL) where CODELENS-CELL is
\(ITEM . OVERLAY)."
  (when lsp-proxy-codelens--resolve-queue
    ;; Remove outdated items
    (setq lsp-proxy-codelens--resolve-queue
          (cl-delete-if (lambda (e)
                          (not (eq (car e) lsp-proxy-codelens--version)))
                        lsp-proxy-codelens--resolve-queue))
    ;; Process one item from the queue
    (let ((queue-item (pop lsp-proxy-codelens--resolve-queue)))
      (when queue-item
        (lsp-proxy-codelens--resolve-codelens (cdr queue-item))))))

(defun lsp-proxy-codelens--fetch-codelens ()
  "Fetch CodeLens from the proxy and update display in current buffer.
Makes an async request to `textDocument/codeLens'.
Renders only the visible area initially, with the full cache stored
for later visible-area refreshes."
  (when-let* ((docver (lsp-proxy--doc-version))
              (buf (current-buffer)))
    (lsp-proxy--async-request
     'textDocument/codeLens
     (lsp-proxy--build-params (list :textDocument (lsp-proxy--TextDocumentIdentifier)))
     :success-fn
     (lambda (codelens-list)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (and lsp-proxy-codelens-mode
                      (eq docver (lsp-proxy--doc-version))
                      (eq (window-buffer (selected-window)) buf))
             ;; Apply middleware hook for codelens transformation
             (let ((uri (plist-get (lsp-proxy--TextDocumentIdentifier) :uri)))
               (setq codelens-list (lsp-proxy-codelens-provide-codelens codelens-list uri)))

             ;; Save old cache before updating
             (let ((old-cache lsp-proxy-codelens--cache)
                   (new-cache (lsp-proxy-codelens--build-cache codelens-list))
                   (range (lsp-proxy-codelens--visible-range))
                   all-lines)

               (when new-cache
                 (maphash (lambda (line _) (push line all-lines)) new-cache))

               ;; Initialize pending-lines with all lines from new cache
               (setq lsp-proxy-codelens--cache new-cache
                     lsp-proxy-codelens--version docver
                     lsp-proxy-codelens--pending-lines all-lines)

               (lsp-proxy-codelens--render-codelens
                new-cache docver all-lines t old-cache range)))))))))

;;; UI Display System

(defun lsp-proxy-codelens--build-display-string (codelens-cell line-start index total-codelens)
  "Build display string for CODELENS-CELL.
Arguments are LINE-START, INDEX, and TOTAL-CODELENS.
CODELENS-CELL is a cons cell \(ITEM . OVERLAY)."
  (let* ((is-first (= index 0))
         (is-last (= index (1- total-codelens)))
         (indentation (if is-first
                          (save-excursion
                            (goto-char line-start)
                            (make-string (current-indentation) ? ))
                        ""))
         (separator (if is-last "\n" " | ")))
    (concat
     indentation
     (let ((text (propertize (lsp-proxy-codelens--format-text codelens-cell)
                             'mouse-face 'lsp-proxy-codelens-mouse-face
                             'help-echo "Click to execute this CodeLens command"
                             'keymap (let ((map (make-sparse-keymap)))
                                       (define-key map [mouse-1]
                                         (lambda () (interactive)
                                           (lsp-proxy-codelens-execute codelens-cell)))
                                       map))))
       ;; `append' so this only fills in what the icon glyph (if any) didn't
       ;; already set on its own characters — a plain `propertize' 'face
       ;; would instead clobber the icon's smaller :height from
       ;; `lsp-proxy-codelens--codicons-to-nerd-icons'.
       (add-face-text-property 0 (length text) 'lsp-proxy-codelens-face t text)
       text)
     separator)))

(defun lsp-proxy-codelens--make-overlay (line-start codelens-cell index total-codelens docver)
  "Create overlay for CODELENS-CELL at LINE-START.
INDEX and TOTAL-CODELENS specify the position and count.
CODELENS-CELL is a cons cell \(ITEM . OVERLAY).
DOCVER is the document version for tracking overlay validity.
Returns the created overlay."
  (let* ((ov (make-overlay line-start line-start)))

    ;; Priority increases: 0, 1, 2... matching LSP return order
    (overlay-put ov 'priority index)

    ;; Add identification for cleanup and store data
    (overlay-put ov 'lsp-proxy-codelens t)

    ;; Document version for which this overlay displays content.
    ;; Used to verify the overlay's display data is still current with the document.
    (overlay-put ov 'lsp-proxy-codelens-docver docver)

    ;; Cache version tracking whether this overlay is still referenced in the current cache.
    ;; Used for cleanup - overlays not referenced in the new cache iteration are deleted.
    (overlay-put ov 'lsp-proxy-codelens-usever docver)

    (when-let* ((item (car codelens-cell))
                (command (plist-get (lsp-proxy-codelens--item-lsp item) :command)))
      (overlay-put ov 'lsp-proxy-codelens-command command))

    ;; Set display string
    (overlay-put ov 'before-string
                 (lsp-proxy-codelens--build-display-string
                  codelens-cell line-start index total-codelens))

    ;; Register overlay in tracking list
    (push ov lsp-proxy-codelens--overlays)
    ov))

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

(defun lsp-proxy-codelens--format-text (codelens-cell)
  "Format display text for CODELENS-CELL.
CODELENS-CELL is a cons cell (ITEM . OVERLAY)."
  (let* ((item (car codelens-cell))
         (ov (cdr codelens-cell))
         (command (or (plist-get (lsp-proxy-codelens--item-lsp item) :command)
                      (and ov (overlayp ov) (overlay-buffer ov)
                           (overlay-get ov 'lsp-proxy-codelens-command))))
         (title (when (listp command) (plist-get command :title))))
    (if title
        (lsp-proxy-codelens--codicons-to-nerd-icons title)
      "Loading...")))

(defun lsp-proxy-codelens--cleanup-overlays ()
  "Clean up all CodeLens overlays in current buffer."
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'lsp-proxy-codelens)
      (delete-overlay ov))))

(defun lsp-proxy-codelens--update-resolved-codelens (codelens-cell resolved)
  "Update overlay in CODELENS-CELL with RESOLVED CodeLens item.
CODELENS-CELL is a cons cell (ITEM . OVERLAY).
RESOLVED is the resolved wrapped CodeLens item from the proxy."
  (let* ((ov (cdr codelens-cell))
         (command (plist-get (lsp-proxy-codelens--item-lsp resolved) :command)))
    (when (and lsp-proxy-codelens-mode
               ov (overlayp ov) (overlay-buffer ov)
               (eq (overlay-get ov 'lsp-proxy-codelens-docver) lsp-proxy-codelens--version))
      ;; Update cache with resolved codelens
      (setcar codelens-cell resolved)

      ;; Update the display string
      (let* ((line-start (overlay-start ov))
             (line (line-number-at-pos line-start t))
             ;; Find line group in cache (hashtable lookup)
             (sorted-codelens (gethash line lsp-proxy-codelens--cache))
             (total-on-line (if sorted-codelens
                                (length sorted-codelens)
                              1)))
        (overlay-put ov 'lsp-proxy-codelens-command command)
        (overlay-put ov 'before-string
                     (lsp-proxy-codelens--build-display-string
                      codelens-cell
                      line-start
                      (overlay-get ov 'priority)
                      total-on-line))))))

(defun lsp-proxy-codelens--line-delta ()
  "Calculate the line count delta and update the previous line count cache.

Returns the difference between the current buffer line count and the
previous line count (`lsp-proxy-codelens--prev-line-count').  A positive
value indicates lines were inserted, negative indicates lines were
deleted, and zero means no change.

As a side effect, updates `lsp-proxy-codelens--prev-line-count' to the
current line count for the next iteration.

This delta is used during overlay reuse to adjust line lookups when
the file has changed.  Overlays before the cursor position are assumed
to be unaffected by edits, while overlays at or after the cursor need
their line numbers adjusted by the delta."
  (let* ((line-count (line-number-at-pos (point-max) t))
         (prev-line-count lsp-proxy-codelens--prev-line-count)
         (line-delta (if (and prev-line-count (integerp prev-line-count))
                         (- line-count prev-line-count)
                       0)))
    ;; Update prev-line-count for next iteration
    (setq lsp-proxy-codelens--prev-line-count line-count)
    line-delta))

(defun lsp-proxy-codelens--render-codelens
    (new-cache docver pending-lines file-changed-p &optional old-cache range)
  "Render CodeLens overlays, reusing overlays from OLD-CACHE when possible.

Arguments:
  NEW-CACHE     - Hash table (line -> sorted list of CODELENS-OVERLAY-CELL)
                  containing the CodeLens data to render.
  DOCVER        - Document version for tracking overlay validity.
  PENDING-LINES - List of line numbers to process.
  FILE-CHANGED-P - Non-nil if the file content has changed, triggering
                  full cleanup and delta calculation for line adjustments.

Optional arguments:
  OLD-CACHE     - Previous cache for overlay reuse.  When provided, existing
                  overlays are reused where possible to minimize flicker.
  RANGE         - Cons cell (BEG-LINE . END-LINE) specifying the line number
                  range to render.  When provided, only CodeLens within this
                  range are updated.  Existing overlays in the range that
                  already have DOCVER are preserved unchanged.

Overlay Reuse Algorithm:
  1. For lines before the cursor: use direct line lookup in OLD-CACHE.
  2. For lines at/after the cursor: adjust lookup by LINE-DELTA
     (insertions/deletions).
  3. Overlays are matched by index position within each line.
  4. Outside RANGE: overlays are reused without updating display content.
  5. When FILE-CHANGED-P: clean up overlays not referenced in NEW-CACHE.

This function also schedules CodeLens resolve requests for CodeLens
without a :command property, adding them to `lsp-proxy-codelens--resolve-queue'."
  (with-silent-modifications
    (save-excursion
      (let* (lines-processed
             resolve-queue)
        ;; Step 1: Process pending lines
        (when (and new-cache pending-lines)
          (let* ((lines-to-process (sort pending-lines #'<))
                 (current-line 1)
                 (line-delta (if file-changed-p (lsp-proxy-codelens--line-delta) 0))
                 (beg-line (lsp-proxy-codelens--change-begin-line)))
            (goto-char (point-min))
            (dolist (line lines-to-process)
              (forward-line (- line current-line))
              (setq current-line line)
              (let* ((line-start (point))
                     (new-sorted (gethash line new-cache)))
                (when new-sorted
                  (let* ((in-range-p (or (not range)
                                         (and (>= line (car range))
                                              (<= line (cdr range)))))
                         (total-on-line (length new-sorted))
                         ;; Find corresponding line group in old cache
                         ;; Before cursor: assume no change, use line directly
                         ;; At/After cursor: adjust by delta (lines inserted/deleted)
                         (lookup-line (when old-cache
                                        (if (and (/= line-delta 0)
                                                 (> line beg-line))
                                            (- line line-delta)
                                          line)))
                         (old-sorted (when old-cache
                                       (gethash lookup-line old-cache))))

                    ;; Process each CodeLens by index
                    (cl-loop for new-cell in new-sorted
                             for item = (car new-cell)
                             for new-ov = (cdr new-cell)
                             for index from 0
                             for old-cell = (when old-sorted
                                              (nth index old-sorted))
                             for old-ov = (when old-cell
                                            (cdr old-cell))
                             do
                             ;; 1) Handle overlays
                             (cond
                              ;; Within range: update/create overlay with new codelens data
                              (in-range-p
                               (cond
                                ;; update existing overlay from new-cache
                                ((and new-ov (overlayp new-ov) (overlay-buffer new-ov))
                                 (unless (eq (overlay-get new-ov 'lsp-proxy-codelens-docver) docver)
                                   (overlay-put new-ov 'before-string
                                                (lsp-proxy-codelens--build-display-string
                                                 new-cell
                                                 line-start
                                                 index
                                                 total-on-line))
                                   (overlay-put new-ov 'lsp-proxy-codelens-docver docver)
                                   (move-overlay new-ov line-start line-start)))

                                ;; reuse and update existing overlay from old-cache
                                ((and old-ov (overlayp old-ov) (overlay-buffer old-ov))
                                 (setcdr new-cell old-ov)
                                 (overlay-put old-ov 'before-string
                                              (lsp-proxy-codelens--build-display-string
                                               new-cell
                                               line-start
                                               index
                                               total-on-line))
                                 (overlay-put old-ov 'lsp-proxy-codelens-docver docver)
                                 (overlay-put old-ov 'lsp-proxy-codelens-usever docver)
                                 (move-overlay old-ov line-start line-start))

                                ;; create new overlay
                                (t
                                 (let ((new-ov (lsp-proxy-codelens--make-overlay
                                                line-start new-cell index total-on-line docver)))
                                   (setcdr new-cell new-ov)))))

                              ;; Outside range: reuse overlay from old-cache, only update usever
                              ((and old-ov (overlayp old-ov) (overlay-buffer old-ov))
                               (setcdr new-cell old-ov)
                               (overlay-put old-ov 'lsp-proxy-codelens-usever docver)))

                             ;; 2) Check if codelens needs to be resolved
                             (when (and in-range-p
                                        (not (plist-get (lsp-proxy-codelens--item-lsp item) :command)))
                               (push (cons docver new-cell) resolve-queue)))

                    ;; Track this line as processed if in range
                    (when in-range-p
                      (push line lines-processed))))))))

        ;; Step 2: Delete overlays with old usever (entire buffer)
        ;; Use tracked overlay list for efficient iteration
        (when file-changed-p
          (let ((retained nil))
            (dolist (ov lsp-proxy-codelens--overlays)
              (if (and (overlayp ov) (overlay-buffer ov)
                       (eq (overlay-get ov 'lsp-proxy-codelens-usever) docver))
                  (push ov retained)
                (delete-overlay ov)))
            (setq lsp-proxy-codelens--overlays retained)))

        ;; Step 3: Update pending-lines cache (remove processed lines)
        (when lines-processed
          (setq lsp-proxy-codelens--pending-lines
                (cl-loop for line in lsp-proxy-codelens--pending-lines
                         unless (memq line lines-processed)
                         collect line)))

        ;; Step 4: Add resolve-queue items to global queue and schedule
        (when resolve-queue
          (setq lsp-proxy-codelens--resolve-queue
                (append lsp-proxy-codelens--resolve-queue (nreverse resolve-queue)))
          (lsp-proxy-codelens--resolve-schedule))))))

(defun lsp-proxy-codelens-execute (codelens-cell)
  "Execute CodeLens command from CODELENS-CELL.
If the command is already available (from the item or overlay), execute it
directly.
If the command needs resolving, trigger the resolve process.
CODELENS-CELL is a cons cell (ITEM . OVERLAY)."
  (let* ((item (car codelens-cell))
         (ov (cdr codelens-cell))
         (item-command (plist-get (lsp-proxy-codelens--item-lsp item) :command))
         (overlay-command (when (and ov (overlayp ov) (overlay-buffer ov))
                            (overlay-get ov 'lsp-proxy-codelens-command)))
         (command (or item-command overlay-command)))
    (cond
     ;; `intellij_debugger.runMain' is a client-side-only command in the VS
     ;; Code extension (its DAP integration registers a matching local
     ;; command and never forwards it to the server) — sending it through
     ;; `workspace/executeCommand' just gets back "Unknown command". Route
     ;; it to `lsp-proxy-java''s dape integration instead, when available.
     ((and command (equal (plist-get command :command) "intellij_debugger.runMain"))
      (if (fboundp 'lsp-proxy-java-dape-run-main)
          (lsp-proxy-java-dape-run-main (plist-get command :arguments))
        (lsp-proxy--error "%s" "Run/Debug from CodeLens needs `dape' installed; see the Debugging section in lsp-proxy-java.el")))
     ;; Execute resolved command
     (command
      (lsp-proxy--execute-command
       (plist-get command :command)
       (plist-get command :arguments)
       (lsp-proxy-codelens--item-ls-id item)))
     ;; No command yet: try to resolve. lsp-proxy's capability model only
     ;; tracks "some server supports codeLens" rather than a per-server
     ;; resolveProvider flag (unlike the eglot-codelens original), so this
     ;; just attempts the resolve request and lets it fail loudly if the
     ;; server doesn't actually support it.
     (lsp-proxy-codelens-mode
      (lsp-proxy-codelens--resolve-codelens codelens-cell)
      (message "Resolving CodeLens command..."))
     (t
      (message "CodeLens command not available")))))

;;; Integration

(defun lsp-proxy-codelens--collect-recent-changes (_beg _end _pre-change-length)
  "Collect the recent modifications tracked by lsp-proxy."
  (when lsp-proxy-codelens-mode
    (setq lsp-proxy-codelens--recent-changes lsp-proxy--recent-changes)))

(defun lsp-proxy-codelens--change-begin-line ()
  "Return the beginning line number of recent buffer edits.

Analyzes `lsp-proxy-codelens--recent-changes' (collected from lsp-proxy's
change tracking) to find the first line affected by edits.  Returns the
minimum line number from all change ranges, or 1 if there are no recent
changes or if the change tracking was interrupted by an Emacs messup.

This is used in overlay reuse to determine where line adjustments are needed:
overlays before this line use direct lookup, while overlays at or after this
line need delta adjustment."
  (if (and lsp-proxy-codelens--recent-changes
           (not (eq :emacs-messup lsp-proxy-codelens--recent-changes)))
      (let ((beg-line most-positive-fixnum))
        (cl-loop for (beg _end _len _text) in lsp-proxy-codelens--recent-changes
                 for line = (plist-get beg :line)
                 do (when (< line beg-line)
                      (setq beg-line line)))
        beg-line)
    1))

(defun lsp-proxy-codelens--schedule-visible-refresh (&rest _args)
  "Handle window scroll/resize to refresh visible CodeLens with debouncing."
  (when lsp-proxy-codelens-mode
    ;; If there's already a timer, just reset its time
    (if (timerp lsp-proxy-codelens--refresh-timer)
        ;; Reset existing timer's time
        (timer-set-idle-time lsp-proxy-codelens--refresh-timer lsp-proxy-codelens-visible-refresh-delay)
      ;; Create new timer if none exists
      (setq lsp-proxy-codelens--refresh-timer
            (run-with-idle-timer
             lsp-proxy-codelens-visible-refresh-delay nil
             (lambda (buf)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (timerp lsp-proxy-codelens--refresh-timer)
                     (cancel-timer lsp-proxy-codelens--refresh-timer))
                   (setq lsp-proxy-codelens--refresh-timer nil)
                   (when (eq (window-buffer (selected-window)) buf)
                     (lsp-proxy-codelens--refresh-visible-area)))))
             (current-buffer))))))

(defun lsp-proxy-codelens--on-document-change (&rest _args)
  "Handle document modification by scheduling a debounced CodeLens re-fetch."
  (when lsp-proxy-codelens-mode
    ;; If there's already a timer, just reset its time instead of canceling and recreating
    (if (timerp lsp-proxy-codelens--update-timer)
        ;; Reset existing timer's time
        (timer-set-idle-time lsp-proxy-codelens--update-timer lsp-proxy-codelens-update-delay)
      ;; Create new timer if none exists
      (setq lsp-proxy-codelens--update-timer
            (run-with-idle-timer
             lsp-proxy-codelens-update-delay nil
             (lambda (buf)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (timerp lsp-proxy-codelens--update-timer)
                     (cancel-timer lsp-proxy-codelens--update-timer))
                   (setq lsp-proxy-codelens--update-timer nil)
                   (message "[lsp-proxy DEBUG] codelens--update-timer FIRING (buffer=%s time=%s)"
                            (buffer-name buf) (current-time-string))
                   (lsp-proxy-codelens--fetch-codelens))))
             (current-buffer))))))

(defun lsp-proxy-codelens--visible-range (&optional extend-lines)
  "Calculate the visible line range for the current buffer's window.
Optional EXTEND-LINES extends the range by that many lines in both directions.

EXTEND-LINES, when a positive integer, extends the range by that many
lines in both directions (useful for pre-fetching).

Return a cons cell (BEG-LINE . END-LINE) where both are 1-based line numbers.
END-LINE may exceed the buffer's actual line count."
  (when-let* ((w (car (get-buffer-window-list)))
              (beg (window-start w))
              (end (window-end w t))
              (beg-line (line-number-at-pos beg t))
              (end-line (line-number-at-pos end t)))
    (if (and extend-lines (integerp extend-lines))
        (cons (max 1 (- beg-line extend-lines))
              ;; Overflow is safe here,
              ;; gethash handles non-existent keys gracefully.
              (+ end-line extend-lines))
      (cons beg-line end-line))))

(defun lsp-proxy-codelens--refresh-visible-area ()
  "Refresh CodeLens overlays in visible window area using existing cache.
This function efficiently updates only the visible portion of the buffer
without re-fetching CodeLens from the proxy."
  (when-let* ((_ (and lsp-proxy-codelens-mode
                      lsp-proxy-codelens--cache
                      (eq lsp-proxy-codelens--version (lsp-proxy--doc-version))))
              (range (lsp-proxy-codelens--visible-range))
              (docver lsp-proxy-codelens--version)
              (beg-line (car range))
              (end-line (cdr range))
              ;; Filter pending-lines to only those within visible range
              (pending-lines (when (and lsp-proxy-codelens--pending-lines
                                        (length> lsp-proxy-codelens--pending-lines 0))
                               (cl-loop for line in lsp-proxy-codelens--pending-lines
                                        when (and (>= line beg-line) (<= line end-line))
                                        collect line))))
    ;; Use existing cache - no new data, just refresh visible area
    (lsp-proxy-codelens--render-codelens
     lsp-proxy-codelens--cache docver pending-lines nil nil range)))

(defun lsp-proxy-codelens--setup-buffer ()
  "Setup CodeLens for current buffer."
  (when lsp-proxy-codelens-mode
    ;; Initialize buffer-local variables
    (setq lsp-proxy-codelens--cache nil
          lsp-proxy-codelens--prev-line-count nil
          lsp-proxy-codelens--version nil
          lsp-proxy-codelens--overlays nil)
    ;; Depth 10 so this runs after lsp-proxy's own `after-change-functions'
    ;; entry (`lsp-proxy--after-change') has already refreshed
    ;; `lsp-proxy--recent-changes' for this edit.
    (add-hook 'after-change-functions #'lsp-proxy-codelens--collect-recent-changes 10 t)
    (add-hook 'after-change-functions #'lsp-proxy-codelens--on-document-change 10 t)
    ;; Add window scroll hook for visible area refresh
    (add-hook 'window-scroll-functions #'lsp-proxy-codelens--schedule-visible-refresh nil t)
    ;; Add window configuration change hook
    (add-hook 'window-configuration-change-hook #'lsp-proxy-codelens--schedule-visible-refresh nil t)))

(defun lsp-proxy-codelens--cleanup-buffer ()
  "Cleanup CodeLens for current buffer."
  ;; Cancel any pending update timer
  (when lsp-proxy-codelens--update-timer
    (cancel-timer lsp-proxy-codelens--update-timer)
    (setq lsp-proxy-codelens--update-timer nil))
  ;; Cancel any pending refresh timer
  (when lsp-proxy-codelens--refresh-timer
    (cancel-timer lsp-proxy-codelens--refresh-timer)
    (setq lsp-proxy-codelens--refresh-timer nil))
  ;; Cancel any pending resolve timer
  (when lsp-proxy-codelens--resolve-timer
    (cancel-timer lsp-proxy-codelens--resolve-timer)
    (setq lsp-proxy-codelens--resolve-timer nil))

  ;; Remove all overlays
  (lsp-proxy-codelens--cleanup-overlays)

  ;; Clear cache, version, queues, and overlay tracking
  (when (hash-table-p lsp-proxy-codelens--cache)
    (clrhash lsp-proxy-codelens--cache))
  (setq lsp-proxy-codelens--cache nil
        lsp-proxy-codelens--prev-line-count nil
        lsp-proxy-codelens--version nil
        lsp-proxy-codelens--pending-lines nil
        lsp-proxy-codelens--resolve-queue nil
        lsp-proxy-codelens--overlays nil
        lsp-proxy-codelens--recent-changes nil)

  ;; Remove hooks
  (remove-hook 'after-change-functions #'lsp-proxy-codelens--collect-recent-changes t)
  (remove-hook 'after-change-functions #'lsp-proxy-codelens--on-document-change t)
  (remove-hook 'window-scroll-functions #'lsp-proxy-codelens--schedule-visible-refresh t)
  (remove-hook 'window-configuration-change-hook #'lsp-proxy-codelens--schedule-visible-refresh t))

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
          (lsp-proxy-codelens--fetch-codelens))
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
                      (member major-mode lsp-proxy-codelens-mode-config))))
    (lsp-proxy-codelens-mode 1)))

;;; Interaction

;;;###autoload
(defun lsp-proxy-codelens-execute-at-line (line)
  "Execute CodeLens at LINE with a visual flash effect on the line.

When called interactively, LINE is the current line.
When called from Elisp, LINE must be provided.

If there's only one CodeLens at the line, execute it directly.
If there are multiple, show a selection menu for user to choose."
  (interactive (list (line-number-at-pos (point) t)))
  (let* ((sorted-codelens (gethash line lsp-proxy-codelens--cache)))
    (if sorted-codelens
        (progn
          ;; Flash the line to indicate execution
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- line))
            (pulse-momentary-highlight-one-line (point)))
          (if (= (length sorted-codelens) 1)
              ;; Only one CodeLens, execute it directly from cache
              (lsp-proxy-codelens-execute (car sorted-codelens))
            ;; Multiple CodeLens, show selection menu using cached sorted list
            (let* ((choices (cl-loop for codelens-cell in sorted-codelens
                                     for index from 0
                                     collect (cons
                                              (format
                                               "[%d] %s" index
                                               (lsp-proxy-codelens--format-text
                                                codelens-cell))
                                              codelens-cell)))
                   (vertico-sort-function nil) ;; No sorting if using vertico
                   (selected-cell (cdr (assoc
                                        (completing-read
                                         (format "CodeLens (L%d): " line)
                                         choices)
                                        choices))))
              (when selected-cell
                (lsp-proxy-codelens-execute selected-cell)))))
      (message (format "No CodeLens found at line %d." line)))))

;;;###autoload
(defun lsp-proxy-codelens-execute-dwim ()
  "Do-What-I-Mean: Execute CodeLens at or above the current line.

If the current line has CodeLens, execute it.
Otherwise, scan backwards to find the nearest line with CodeLens and execute it.
If no CodeLens is found, show a message."
  (interactive)
  (let ((current-line (line-number-at-pos (point) t)))
    (cl-loop for line from current-line downto 1
             when (gethash line lsp-proxy-codelens--cache)
             return (lsp-proxy-codelens-execute-at-line line)
             finally (message "No CodeLens found before line %d." current-line))))

(provide 'lsp-proxy-codelens)
;;; lsp-proxy-codelens.el ends here
