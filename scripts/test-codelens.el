;;; test-codelens.el --- CodeLens regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'lsp-proxy-utils)

;; Rendering tests do not need the proxy transport or its external dependencies.
(let ((features (cons 'lsp-proxy-core features)))
  (require 'lsp-proxy-codelens))

(defvar lsp-proxy-test--docver 1)

(defmacro lsp-proxy-test--with-codelens (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (dotimes (line 200)
       (insert (format "    code line %d\n" (1+ line))))
     (let ((lsp-proxy-codelens-mode t)
           (lsp-proxy-codelens--last-count nil)
           (lsp-proxy-codelens--overlays nil)
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
  "In `end-of-line' mode (the default) the lens is appended to the code
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

;;; test-codelens.el ends here
