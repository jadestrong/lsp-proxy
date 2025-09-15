;;; lsp-proxy-xref.el --- Xref integration for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Xref (cross-reference) integration for code navigation in lsp-proxy

;;; Code:

(require 'cl-lib)
(require 'xref)
(require 'dash)
(require 'subr-x)
(require 'eglot)
(require 'lsp-proxy-utils)
(require 'lsp-proxy-core)

;;; Configuration

(defcustom lsp-proxy-lazy-xref-threshold 10000
  "Threshold for using lazy xref evaluation.
When the target file is visiting in a buffer, files with more than
this many lines will use optimized/lazy evaluation to improve
performance.  For files that are not currently visited, the check
falls back to a quick byte-size based heuristic (via
`lsp-proxy--large-file-p') as an approximation."
  :type 'number
  :group 'lsp-proxy)

(defcustom lsp-proxy-xref-optimization-strategy 'optimized
  "Strategy for handling xref in large files.
- 'eager: Always use original method (may be slow for large files)
- 'lazy: Use lazy evaluation (no preview, fastest)
- 'optimized: Use optimized method with preview (balanced)"
  :type '(choice (const :tag "Eager (original)" eager)
                 (const :tag "Lazy (no preview)" lazy)
                 (const :tag "Optimized (fast with preview)" optimized))
  :group 'lsp-proxy)

;;; External variables
(defvar lsp-xref-force-references)
(defvar xref-show-definitions-function)
(defvar xref-show-xrefs-function)
(defvar xref-auto-jump-to-first-xref)
(defvar xref-auto-jump-to-first-definition)
(defvar lsp-proxy-hover-buffer)
(defvar lsp-proxy--highlights)

;;; External functions
(declare-function lsp-proxy--progressing-p "lsp-proxy")
(declare-function lsp-proxy--large-file-p "lsp-proxy-large-file")

;;; Xref backend

(defun lsp-proxy--xref-backend ()
  "Lsp proxy xref backend."
  'xref-lsp-proxy)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-lsp-proxy)))
  (propertize (or (thing-at-point 'symbol) "")
              'identifier-at-point t))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql xref-lsp-proxy)))
  (list (propertize (or (thing-at-point 'symbol) "")
                    'identifier-at-point t)))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-lsp-proxy)) _identifier)
  (save-excursion
    (lsp-proxy-find-definition)))

(cl-defmethod xref-backend-references ((_backend (eql xref-lsp-proxy)) _identifier)
  (save-excursion
    (lsp-proxy-find-references)))

(cl-defmethod xref-backend-implementations ((_backend (eql xref-lsp-proxy)) _identifier)
  (save-excursion
    (lsp-proxy-find-implementations)))

(cl-defmethod xref-backend-type-definitions ((_backend (eql xref-lsp-proxy)) _identifier)
  (save-excursion
    (lsp-proxy-find-type-definition)))

;;; Lazy xref location class

;; Custom xref location class for lazy evaluation
(cl-defstruct (lsp-proxy--lazy-location
               (:constructor lsp-proxy--lazy-location-create)
               (:conc-name lsp-proxy--lazy-location-))
  filepath range)

(cl-defmethod xref-location-marker ((location lsp-proxy--lazy-location))
  "Return the marker for lazy LOCATION, computing it on-demand."
  (let* ((filepath (lsp-proxy--lazy-location-filepath location))
         (range (lsp-proxy--lazy-location-range location))
         (start (plist-get range :start)))
    (with-current-buffer (find-file-noselect filepath)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (eglot--lsp-position-to-point start))
          (point-marker))))))

(cl-defmethod xref-location-group ((location lsp-proxy--lazy-location))
  "Return the group (file) for lazy LOCATION."
  (lsp-proxy--lazy-location-filepath location))

(cl-defmethod xref-location-line ((location lsp-proxy--lazy-location))
  "Return the line number for lazy LOCATION."
  (let* ((range (lsp-proxy--lazy-location-range location))
         (start (plist-get range :start)))
    (1+ (plist-get start :line))))

;;; Xref display

(defun lsp-proxy-show-xrefs (xrefs display-action references?)
  "Show XREFS with DISPLAY-ACTION, handling REFERENCES?."
  (unless (region-active-p) (push-mark nil t))
  (if (boundp 'xref-show-definitions-function)
      (with-no-warnings
        (xref-push-marker-stack)
        (funcall (if (and references? (not lsp-xref-force-references))
                     xref-show-xrefs-function
                   xref-show-definitions-function)
                 (-const xrefs)
                 `((window . ,(selected-window))
                   (display-action . ,display-action)
                   ,(if (and references? (not lsp-xref-force-references))
                        `(auto-jump . ,xref-auto-jump-to-first-xref)
                      `(auto-jump . ,xref-auto-jump-to-first-definition)))))
    (xref--show-xrefs xrefs display-action)))

;;; Location processing strategies

(defun lsp-proxy--should-use-lazy-xref-p (filepath)
  "Determine if FILEPATH should use lazy xref evaluation based on file size."
  (when (and lsp-proxy-lazy-xref-threshold
             (file-exists-p filepath))
    (let ((visiting (find-buffer-visiting filepath)))
      (if visiting
          ;; If buffer is already open, check line count directly
          (with-current-buffer visiting
            (> (line-number-at-pos (point-max)) lsp-proxy-lazy-xref-threshold))
        ;; For unopened files, fallback to byte-size based heuristic from
        ;; `lsp-proxy--large-file-p' as a quick approximation.
        (lsp-proxy--large-file-p filepath)))))

(defun lsp-proxy--process-location-eager (location-data)
  "Process a single location using eager evaluation (original method)."
  (let* ((uri (plist-get location-data :uri))
         (filepath (lsp-proxy--uri-to-path uri))
         (visiting (find-buffer-visiting filepath))
         (range (plist-get location-data :range))
         (start (plist-get range :start))
         (end (plist-get range :end))
         (start-line (plist-get start :line))
         (start-column (plist-get start :character))
         (collect (lambda ()
                    (save-excursion
                      (save-restriction
                        (widen)
                        (let* ((beg (eglot--lsp-position-to-point start))
                               (end-pos (eglot--lsp-position-to-point end))
                               (bol (progn (goto-char beg) (line-beginning-position)))
                               (summary (buffer-substring bol (line-end-position)))
                               (hi-beg (- beg bol))
                               (hi-end (- (min (line-end-position) end-pos) bol)))
                          (when summary
                            (add-face-text-property hi-beg hi-end 'xref-match t summary))
                          (xref-make summary
                                     (xref-make-file-location filepath (1+ start-line) start-column))))))))
    (cond
     (visiting (with-current-buffer visiting (funcall collect)))
     ((file-readable-p filepath)
      (with-temp-buffer
        (insert-file-contents-literally filepath)
        (funcall collect)))
     (t (lsp-proxy--warn "Failed to process xref entry for file %s" filepath)
        nil))))

(defun lsp-proxy--process-location-lazy (location-data)
  "Process a single location using lazy evaluation."
  (let* ((uri (plist-get location-data :uri))
         (filepath (lsp-proxy--uri-to-path uri))
         (range (plist-get location-data :range))
         (start (plist-get range :start))
         (start-line (plist-get start :line))
         (start-column (plist-get start :character))
         (location (lsp-proxy--lazy-location-create
                    :filepath filepath
                    :range range))
         (summary (format "%s:%d:%d" 
                         (file-name-nondirectory filepath)
                         (1+ start-line) 
                         (1+ start-column))))
    (when (file-exists-p filepath)
      (xref-make summary location))))

(defun lsp-proxy--read-file-lines (filepath locations)
  "Read only the required lines from FILEPATH for given LOCATIONS."
  (let ((line-numbers (mapcar (lambda (loc)
                               (plist-get (plist-get (plist-get loc :range) :start) :line))
                             locations)))
    (with-temp-buffer
      (let ((inhibit-read-only t)
            (large-file-warning-threshold nil)
            (lines (make-hash-table :test 'equal)))
        (insert-file-contents filepath nil)
        (goto-char (point-min))
        (let ((current-line 0))
          (while (not (eobp))
            (when (memq current-line line-numbers)
              (puthash current-line (buffer-substring (line-beginning-position) (line-end-position)) lines))
            (forward-line 1)
            (setq current-line (1+ current-line))))
        ;; Convert hash table to list indexed by line number
        (let* ((max-line (apply #'max line-numbers))
               (result-lines (make-vector (1+ max-line) nil)))
          (maphash (lambda (line-num content)
                    (aset result-lines line-num content))
                  lines)
          result-lines)))))

(defun lsp-proxy--batch-process-locations-optimized (locations-by-file)
  "Batch process LOCATIONS-BY-FILE using a single pass per file.

For each visited buffer we avoid repeatedly moving to `point-min' and
calling `forward-line' for every location.  Instead we sort the
locations by starting line and then advance linearly through the
buffer, producing summaries/highlighting in a single traversal.

For non‑visited but readable files we still fall back to reading
needed lines, but we keep the existing behaviour of extracting only
lines that are referenced.  The return value is a list of `xref-item'
objects preserving the original (stable) order per file as delivered
by the server (assuming `file-locations' was in that order)."
  (let (results)
    (maphash
     (lambda (filepath file-locations)
       (let ((visiting (find-buffer-visiting filepath)))
         (cond
          (visiting
           (with-current-buffer visiting
             (save-excursion
               (save-restriction
                 (widen)
                 ;; Sort locations by line to allow single linear scan.
                 (let* ((sorted (sort (copy-sequence file-locations)
                                      (lambda (a b)
                                        (< (plist-get (plist-get (plist-get a :range) :start) :line)
                                           (plist-get (plist-get (plist-get b :range) :start) :line)))))
                        (current-line 0)
                        ;; Alist ((line . column) . xref-item) for reconstruction
                        (file-results nil))
                   (goto-char (point-min))
                   (dolist (loc sorted)
                     (let* ((range (plist-get loc :range))
                            (start (plist-get range :start))
                            (end (plist-get range :end))
                            (start-line (plist-get start :line))
                            (start-column (plist-get start :character))
                            (end-line (plist-get end :line))
                            (end-column (plist-get end :character)))
                       ;; Advance to required line efficiently.
                       (forward-line (- start-line current-line))
                       (setq current-line start-line)
                       (let* ((bol (line-beginning-position))
                              (eol (line-end-position))
                              (summary (buffer-substring bol eol))
                              (hi-beg start-column)
                              (hi-end (if (= start-line end-line)
                                          (min end-column (length summary))
                                        (length summary))))
                         (when (and summary (> hi-end hi-beg) (>= hi-beg 0))
                           (add-face-text-property hi-beg hi-end 'xref-match t summary))
                         (let ((item (xref-make (if (and summary (not (string-blank-p summary)))
                                                    summary
                                                  (format "%s:%d:%d"
                                                          (file-name-nondirectory filepath)
                                                          (1+ start-line)
                                                          (1+ start-column)))
                                                (xref-make-file-location filepath (1+ start-line) start-column))))
                           (push (cons (cons start-line start-column) item) file-results)))))
                   ;; Reconstruct in original (server-provided) order
                   (dolist (orig-loc file-locations)
                     (let* ((range (plist-get orig-loc :range))
                            (start (plist-get range :start))
                            (line (plist-get start :line))
                            (col (plist-get start :character))
                            (cell (assoc (cons line col) file-results)))
                       (when cell (push (cdr cell) results)))))))))
          ((file-readable-p filepath)
           ;; Existing optimisation path for non‑visited files.
           (let ((file-lines (lsp-proxy--read-file-lines filepath file-locations)))
             (dolist (location-data file-locations)
               (let* ((range (plist-get location-data :range))
                      (start (plist-get range :start))
                      (start-line (plist-get start :line))
                      (start-column (plist-get start :character))
                      (summary (and (< start-line (length file-lines))
                                    (aref file-lines start-line))))
                 (push (xref-make (or summary
                                      (format "%s:%d:%d"
                                              (file-name-nondirectory filepath)
                                              (1+ start-line)
                                              (1+ start-column)))
                                   (xref-make-file-location filepath (1+ start-line) start-column))
                       results)))))
          (t
           (lsp-proxy--warn "Failed to process xref entry for file %s" filepath)))))
     locations-by-file)
    ;; results were pushed in per-file original order (reversed per file), so reverse once.
    (nreverse results)))

;;; Location processing

(defun lsp-proxy--process-locations (locations)
  "Process LOCATIONS and show xrefs, using optimized batch processing."
  (if (seq-empty-p locations)
      (lsp-proxy--error "Not found for: %s" (or (thing-at-point 'symbol t) ""))
    (let ((locations-vec (if (vectorp locations) locations (vector locations))))
      ;; Group locations by file and strategy
      (let ((file-strategy-map (make-hash-table :test 'equal))
            (eager-files (make-hash-table :test 'equal))
            (optimized-files (make-hash-table :test 'equal))
            (lazy-results nil))

        ;; First pass: group locations by file and determine strategy
        (cl-loop for location across locations-vec
                 for uri = (plist-get location :uri)
                 for filepath = (lsp-proxy--uri-to-path uri)
                 do (let ((strategy (or (gethash filepath file-strategy-map)
                                       (let ((is-large (lsp-proxy--should-use-lazy-xref-p filepath)))
                                         (puthash filepath
                                                  (cond
                                                   ((eq lsp-proxy-xref-optimization-strategy 'eager) 'eager)
                                                   ((eq lsp-proxy-xref-optimization-strategy 'lazy) (if is-large 'lazy 'eager))
                                                   ((eq lsp-proxy-xref-optimization-strategy 'optimized) (if is-large 'optimized 'eager))
                                                   (t 'eager))
                                                  file-strategy-map)))))
                      (pcase strategy
                        ('eager
                         (let ((current-list (gethash filepath eager-files)))
                           (puthash filepath (cons location current-list) eager-files)))
                        ('optimized
                         (let ((current-list (gethash filepath optimized-files)))
                           (puthash filepath (cons location current-list) optimized-files)))
                        ('lazy
                         (push (lsp-proxy--process-location-lazy location) lazy-results)))))

        ;; Process each strategy group in batch
        (let ((all-results lazy-results))
          ;; Process eager files (original method, but batched)
          (when (> (hash-table-count eager-files) 0)
            (maphash (lambda (_filepath file-locations)
                      (dolist (location (nreverse file-locations))
                        (when-let* ((result (lsp-proxy--process-location-eager location)))
                          (push result all-results))))
                    eager-files))

          ;; Process optimized files (batch optimized method)
          (when (> (hash-table-count optimized-files) 0)
            ;; Reverse the lists in optimized-files for correct order
            (let ((corrected-optimized-files (make-hash-table :test 'equal)))
              (maphash (lambda (filepath file-locations)
                        (puthash filepath (nreverse file-locations) corrected-optimized-files))
                      optimized-files)
              (setq all-results (append (lsp-proxy--batch-process-locations-optimized corrected-optimized-files) all-results))))

          (when all-results
            (lsp-proxy-show-xrefs (delq nil all-results) nil nil)))))))

;;; Navigation commands

(defun lsp-proxy-find-definition ()
  "Find definition."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/definition
   (lsp-proxy--request-or-notify-params (eglot--TextDocumentPositionParams))
   :success-fn #'lsp-proxy--process-locations))

(defun lsp-proxy-find-references ()
  "Find references."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/references
   (lsp-proxy--request-or-notify-params
    (append (eglot--TextDocumentPositionParams) `(:context (:includeDeclaration t))))
   :success-fn #'lsp-proxy--process-locations))

(defun lsp-proxy-find-declaration ()
  "Find declaration."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/declaration
   (lsp-proxy--request-or-notify-params (eglot--TextDocumentPositionParams))
   :success-fn #'lsp-proxy--process-locations))

(defun lsp-proxy-find-type-definition ()
  "Find type definition."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/typeDefinition
   (lsp-proxy--request-or-notify-params (eglot--TextDocumentPositionParams))
   :success-fn #'lsp-proxy--process-locations))

(defun lsp-proxy-find-implementations ()
  "Find implementations."
  (interactive)
  (lsp-proxy--async-request
   'textDocument/implementation
   (lsp-proxy--request-or-notify-params (eglot--TextDocumentPositionParams))
   :success-fn #'lsp-proxy--process-locations))

(provide 'lsp-proxy-xref)
;;; lsp-proxy-xref.el ends here
