;;; test-codelens.el --- CodeLens regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'lsp-proxy-utils)

;; Rendering tests do not need the proxy transport or its external dependencies.
(let ((features (cons 'lsp-proxy-core features)))
  (require 'lsp-proxy-codelens))

(defvar lsp-proxy-test--docver 1)
(defvar lsp-proxy--connection)
(defvar lsp-proxy--has-any-servers)
(defvar lsp-proxy--buffer-opened)
(defvar eglot--docver)
(defvar eglot--versioned-identifier)

(defmacro lsp-proxy-test--with-codelens (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (dotimes (line 200)
       (insert (format "    code line %d\n" (1+ line))))
     (let ((lsp-proxy-codelens-mode t)
           (lsp-proxy-codelens-place-position 'end-of-line)
           (lsp-proxy-codelens--last-count nil)
           (lsp-proxy-codelens--overlays nil)
           (eglot--docver 1)
           (eglot--versioned-identifier 1)
           (lsp-proxy-test--docver 1))
       (cl-letf (((symbol-function 'lsp-proxy--doc-version)
                  (lambda () lsp-proxy-test--docver))
                 ((symbol-function 'eglot--lsp-position-to-point)
                  (lambda (pos)
                    (save-excursion
                      (goto-char (point-min))
                      (forward-line (plist-get pos :line))
                      (+ (point) (plist-get pos :character))))))
         ,@body))))

(defun lsp-proxy-test--item (line &optional title kind)
  "Return a wrapped CodeLens at LINE with optional TITLE and KIND."
  (list :language_server_id 13 :language_server_name "jdtls"
        :lsp_item
        (append (list :range (list :start (list :line (1- line) :character 4)
                                  :end (list :line (1- line) :character 8))
                      :data (vector (or kind "references")))
                (when title
                  (list :command (list :title title :command (or kind "references")))))))

(defun lsp-proxy-test--item-command (item)
  (plist-get (plist-get item :lsp_item) :command))

(ert-deftest lsp-proxy-codelens-test-one-overlay-per-line ()
  "Both lenses on the same line are shown as a single joined token."
  (lsp-proxy-test--with-codelens
    (lsp-proxy-codelens--display
     (list (lsp-proxy-test--item 28 "0 references")
           (lsp-proxy-test--item 28 "0 implementations" "implementations")))
    (should (= (length lsp-proxy-codelens--overlays) 1))
    (let* ((ov (car lsp-proxy-codelens--overlays))
           (text (overlay-get ov 'after-string)))
      (should (string-match-p "0 references" text))
      (should (string-match-p "0 implementations" text))
      (should (string-match-p "|" text)))))

(ert-deftest lsp-proxy-codelens-test-display-accepts-docver-arg ()
  "`--display' is used as a callback for `--backend'/`--fetch-missing',
which invoke it with (ITEMS DOCVER); it must accept the extra arg."
  (lsp-proxy-test--with-codelens
    (funcall #'lsp-proxy-codelens--display
             (list (lsp-proxy-test--item 28 "0 references")) 7)
    (should (= (length lsp-proxy-codelens--overlays) 1))))

(ert-deftest lsp-proxy-codelens-test-above-line-uses-before-string ()
  "In `above-line' mode the lens renders as its own indented line above
the code, via `before-string', not appended via `after-string'."
  (lsp-proxy-test--with-codelens
    (let ((lsp-proxy-codelens-place-position 'above-line))
      (lsp-proxy-codelens--display (list (lsp-proxy-test--item 28 "0 references")))
      (should (= (length lsp-proxy-codelens--overlays) 1))
      (let* ((ov (car lsp-proxy-codelens--overlays))
             (before (overlay-get ov 'before-string)))
        (should-not (overlay-get ov 'after-string))
        (should (string-match-p "0 references" before))
        (should (string-suffix-p "\n" before))
        ;; Indented to match the code line's own indentation (4 spaces,
        ;; see `lsp-proxy-test--with-codelens').
        (should (string-prefix-p "    0 references" before))))))

(ert-deftest lsp-proxy-codelens-test-end-of-line-uses-after-string ()
  "In `end-of-line' mode the lens is appended to the code
line itself, via `after-string', not placed on its own line."
  (lsp-proxy-test--with-codelens
    (lsp-proxy-codelens--display (list (lsp-proxy-test--item 28 "0 references")))
    (let* ((ov (car lsp-proxy-codelens--overlays))
           (after (overlay-get ov 'after-string)))
      (should-not (overlay-get ov 'before-string))
      (should (string-match-p "0 references" after))
      (should (= (overlay-start ov) (overlay-end ov))))))

(ert-deftest lsp-proxy-codelens-test-unresolved-not-shown ()
  "An item with no :command yet renders nothing until resolved."
  (lsp-proxy-test--with-codelens
    (lsp-proxy-codelens--display (list (lsp-proxy-test--item 28)))
    (should (null lsp-proxy-codelens--overlays))))

(ert-deftest lsp-proxy-codelens-test-partial-resolve-then-redisplay ()
  "A still-unresolved item on the line doesn't block showing the ones
that are already resolved; once it resolves too, redisplay adds it in."
  (lsp-proxy-test--with-codelens
    (let* ((unresolved (lsp-proxy-test--item 28 nil "implementations"))
           (items (list (lsp-proxy-test--item 28 "0 references") unresolved)))
      (lsp-proxy-codelens--display items)
      (should (= (length lsp-proxy-codelens--overlays) 1))
      (let ((text (overlay-get (car lsp-proxy-codelens--overlays) 'after-string)))
        (should (string-match-p "0 references" text))
        (should-not (string-match-p "implementations" text)))
      (plist-put (plist-get unresolved :lsp_item) :command
                 (list :title "0 implementations" :command "implementations"))
      (lsp-proxy-codelens--display items)
      (should (= (length lsp-proxy-codelens--overlays) 1))
      (let ((text (overlay-get (car lsp-proxy-codelens--overlays) 'after-string)))
        (should (string-match-p "0 references" text))
        (should (string-match-p "0 implementations" text))))))

(ert-deftest lsp-proxy-codelens-test-overlay-at-correct-line ()
  "The overlay for a lens sits on its own line, not some other line."
  (lsp-proxy-test--with-codelens
    (lsp-proxy-codelens--display (list (lsp-proxy-test--item 174 "2 references")))
    (let ((ov (car lsp-proxy-codelens--overlays)))
      (should (= (line-number-at-pos (overlay-start ov) t) 174)))))

(ert-deftest lsp-proxy-codelens-test-redisplay-rebuilds-overlays ()
  "Every redisplay deletes the old overlays and builds fresh ones.
This is the core of the lsp-lens.el-derived design: never reuse or
reposition an overlay across refreshes, so overlay drift cannot happen."
  (lsp-proxy-test--with-codelens
    (lsp-proxy-codelens--display (list (lsp-proxy-test--item 28 "0 references")))
    (let ((first-ov (car lsp-proxy-codelens--overlays)))
      (lsp-proxy-codelens--display (list (lsp-proxy-test--item 28 "1 references")))
      (let ((second-ov (car lsp-proxy-codelens--overlays)))
        (should-not (eq first-ov second-ov))
        (should-not (overlay-buffer first-ov))
        (should (overlay-buffer second-ov))))))

(ert-deftest lsp-proxy-codelens-test-count-change-forces-redisplay ()
  "A lens disappearing (count changes) is redisplayed even though every
remaining lens is already resolved."
  (lsp-proxy-test--with-codelens
    (lsp-proxy-codelens--display
     (list (lsp-proxy-test--item 28 "0 references")
           (lsp-proxy-test--item 60 "0 references")))
    (should (= (length lsp-proxy-codelens--overlays) 2))
    (lsp-proxy-codelens--display (list (lsp-proxy-test--item 28 "0 references")))
    (should (= (length lsp-proxy-codelens--overlays) 1))))

(ert-deftest lsp-proxy-codelens-test-click-executes-command ()
  "Clicking a lens token's mouse-1 binding executes its command."
  (lsp-proxy-test--with-codelens
    (lsp-proxy-codelens--display (list (lsp-proxy-test--item 28 "0 references")))
    (let* ((text (overlay-get (car lsp-proxy-codelens--overlays) 'after-string))
           (map (get-text-property 1 'keymap text))
           executed)
      (cl-letf (((symbol-function 'lsp-proxy--execute-command)
                 (lambda (command _args _ls-id) (setq executed command))))
        (funcall (lookup-key map [mouse-1]))
        (should (equal executed "references"))))))

(ert-deftest lsp-proxy-codelens-test-not-loaded-p-respects-window ()
  "An item is only queued for resolve while its line is inside the window."
  (lsp-proxy-test--with-codelens
    (cl-letf (((symbol-function 'lsp-proxy-codelens--window)
               (lambda () (selected-window)))
              ((symbol-function 'window-start) (lambda (&rest _) (point-min)))
              ((symbol-function 'window-end) (lambda (&rest _) (point-max))))
      (should (lsp-proxy-codelens--not-loaded-p (lsp-proxy-test--item 28)))
      (should-not (lsp-proxy-codelens--not-loaded-p (lsp-proxy-test--item 28 "done"))))))

(ert-deftest lsp-proxy-codelens-test-default-is-above-line ()
  "The default is a supported choice and renders above the code."
  (should (eq (default-value 'lsp-proxy-codelens-place-position) 'above-line))
  (let ((default-position (default-value 'lsp-proxy-codelens-place-position)))
    (lsp-proxy-test--with-codelens
      (let ((lsp-proxy-codelens-place-position default-position))
        (lsp-proxy-codelens--display (list (lsp-proxy-test--item 28 "references")))
        (should (overlay-get (car lsp-proxy-codelens--overlays) 'before-string))))))

(ert-deftest lsp-proxy-codelens-test-scroll-preserves-modified-refresh ()
  "A scroll cannot downgrade a pending refetch to cache-only redisplay."
  (lsp-proxy-test--with-codelens
    (let (scheduled cancelled refreshed)
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (_seconds _repeat function &rest args)
                   (push (cons function args) scheduled)
                   (car scheduled)))
                ((symbol-function 'cancel-timer)
                 (lambda (timer) (push timer cancelled)))
                ((symbol-function 'lsp-proxy-codelens-refresh)
                 (lambda (modified) (push modified refreshed))))
        (lsp-proxy-codelens--on-document-change)
        (lsp-proxy-codelens--on-scroll)
        (should (= (length cancelled) 1))
        (apply (caar scheduled) (cdar scheduled))
        (should (equal refreshed '(t)))
        (should-not lsp-proxy-codelens--refresh-modified)
        (should-not lsp-proxy-codelens--refresh-timer)
        (lsp-proxy-codelens--on-scroll)
        (apply (caar scheduled) (cdar scheduled))
        (should (equal refreshed '(nil t)))
        (lsp-proxy-codelens--on-document-change)
        (lsp-proxy-codelens--cleanup-buffer)
        (should-not lsp-proxy-codelens--refresh-modified)
        (should-not lsp-proxy-codelens--refresh-timer)))))

(ert-deftest lsp-proxy-codelens-test-single-resolve-keeps-other-lenses ()
  "Resolving one lens redisplays the full cache in its original buffer."
  (lsp-proxy-test--with-codelens
    (let* ((unresolved (lsp-proxy-test--item 28))
           (other (lsp-proxy-test--item 60 "other references"))
           (lsp-proxy-codelens--backend-cache (list unresolved other))
           callback docver)
      (lsp-proxy-codelens--display lsp-proxy-codelens--backend-cache)
      (cl-letf (((symbol-function 'lsp-proxy-codelens--fetch-missing)
                 (lambda (_items cb version)
                   (setq callback cb docver version))))
        (lsp-proxy-codelens-execute unresolved))
      (plist-put (plist-get unresolved :lsp_item) :command
                 (list :title "resolved references" :command "references"))
      (with-temp-buffer
        (funcall callback (list unresolved) docver))
      (should (= (length lsp-proxy-codelens--overlays) 2))
      (should (equal (sort (mapcar (lambda (ov) (line-number-at-pos (overlay-start ov)))
                                  lsp-proxy-codelens--overlays) #'<)
                     '(28 60))))))

(ert-deftest lsp-proxy-codelens-test-inactive-commands-report-empty-cache ()
  "Commands without an initialized cache report absence without type errors."
  (with-temp-buffer
    (let (reported)
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args)
                   (push (apply #'format format args) reported))))
        (lsp-proxy-codelens-execute-at-line 1)
        (lsp-proxy-codelens-execute-dwim)
        (should (= (length reported) 2))
        (should (seq-every-p (lambda (text) (string-match-p "No CodeLens" text)) reported))))))

(ert-deftest lsp-proxy-codelens-test-resolve-rejects-stale-or-disabled-response ()
  "Only a resolve for the current document with the mode enabled is applied."
  (dolist (state '(current edited disabled))
    (lsp-proxy-test--with-codelens
      (let ((lsp-proxy--connection 'peer)
            (lsp-proxy--has-any-servers t)
            (lsp-proxy--buffer-opened t)
            (item (lsp-proxy-test--item 28))
            success displayed)
        ;; Cover both interpreted transport-free loading and compilation with
        ;; the real core request macro, which expands to jsonrpc-async-request.
        (cl-letf (((symbol-function 'lsp-proxy--async-request)
                   (lambda (_method _params &rest callbacks)
                     (setq success (plist-get callbacks :success-fn))))
                  ((symbol-function 'jsonrpc-async-request)
                   (lambda (_connection _method _params &rest callbacks)
                     (setq success (plist-get callbacks :success-fn))))
                  ((symbol-function 'lsp-proxy--ensure-connection) #'ignore)
                  ((symbol-function 'lsp-proxy--send-did-change) #'ignore)
                  ((symbol-function 'lsp-proxy--build-params)
                   (lambda (params &rest _) params))
                  ((symbol-function 'lsp-proxy-codelens--window)
                   (lambda () (selected-window)))
                  ((symbol-function 'window-start) (lambda (&rest _) (point-min)))
                  ((symbol-function 'window-end) (lambda (&rest _) (point-max))))
          (lsp-proxy-codelens--fetch-missing
           (list item) (lambda (items version) (setq displayed (list items version))) 1)
          (should success)
          (should (plist-get item :_pending))
          (pcase state
            ('edited (setq lsp-proxy-test--docver 2
                           eglot--docver 2 eglot--versioned-identifier 2))
            ('disabled (setq lsp-proxy-codelens-mode nil)))
          (funcall success (lsp-proxy-test--item 28 "resolved references"))
          (should-not (plist-get item :_pending))
          (if (eq state 'current)
              (progn
                (should (lsp-proxy-codelens--item-command item))
                (should (equal displayed (list (list item) 1))))
            (should-not (lsp-proxy-codelens--item-command item))
            (should-not displayed)))))))

;;; test-codelens.el ends here
