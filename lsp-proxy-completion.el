;;; lsp-proxy-completion.el --- Completion support for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Code completion functionality for lsp-proxy

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 's)
(require 'dash)
(require 'lsp-proxy-utils)
(require 'lsp-proxy-core)

;;; Variables

(defcustom lsp-proxy-max-completion-item 20
  "Maximum numbers of completion sent and by server."
  :group 'lsp-proxy
  :type 'integer)

(defvar-local lsp-proxy--completion-trigger-characters nil
  "Completion trigger characters for this buffer.")

(defvar-local lsp-proxy--last-inserted-char nil
  "If non-nil, value of the last inserted character in buffer.")

;;; Completion styles

(defun lsp-proxy-passthrough-all-completions (_string table pred _point)
  "Like `completion-basic-all-completions' but have prefix ignored.
TABLE PRED"
  (completion-basic-all-completions "" table pred 0))

(defun lsp-proxy--dumb-tryc (pat table pred point)
  "Like `completion-basic-try-completion' but passthrough all completion.
Without common substring required. PAT TABLE PRED POINT."
  (let ((probe (funcall table pat pred nil)))
    (cond ((eq probe t) t)
          (probe (cons probe (length probe)))
          (t (cons pat point)))))

;;; Hooks for completion

(defun lsp-proxy--track-last-input ()
  "Track the last inserted character."
  (setq lsp-proxy--last-inserted-char last-input-event))

(defun lsp-proxy--reset-input-tracking ()
  "Reset input tracking variables.."
  (setq lsp-proxy--last-inserted-char nil))

;;; Completion boundaries

(defun lsp-proxy--get-english-dash-string-boundaries ()
  "Return the boundaries of the English and dash string before point.
Or nil if none."
  (save-excursion
    (let ((end (point))
          (start (re-search-backward "[^a-zA-Z0-9-]" nil t)))
      (if start
          (progn
            (forward-char)
            (setq start (point)))
        (setq start (point-min)))
      (goto-char end)
      (if (looking-back "[a-zA-Z0-9]+-[a-zA-Z0-9-]*" start)
          (cons start end)
        nil))))

;;; Trigger character support

(defun lsp-proxy--looking-back-trigger-characterp (trigger-characters)
  "Return character if text before point match any of the TRIGGER-CHARACTERS."
  (unless (= (point) (line-beginning-position))
    (cl-some
     (lambda (trigger-char)
       (and (equal (buffer-substring-no-properties (- (point) (length trigger-char)) (point))
                   trigger-char)
            trigger-char))
     trigger-characters)))

;;; Main completion at point function

(defun lsp-proxy-completion-at-point ()
  "Get lsp completions."
  (let* ((trigger-characters lsp-proxy--completion-trigger-characters)
         (bounds-start (if-let* ((bounds (lsp-proxy--get-english-dash-string-boundaries)))
                           (cl-first bounds)
                         (or (cl-first (bounds-of-thing-at-point 'symbol))
                             (point))))
         (candidates
          (lambda ()
            (let* ((prefix (buffer-substring-no-properties bounds-start (point)))
                   (resp (lsp-proxy--request
                          'textDocument/completion
                          (lsp-proxy--request-or-notify-params
                           (eglot--TextDocumentPositionParams)
                           `(:context
                             (:line ,(buffer-substring-no-properties (line-beginning-position) (line-end-position))
                              :prefix ,prefix
                              :boundsStart ,bounds-start
                              :startPoint ,(point)
                              ;; Used to distinguish trigger type: 1 for manual invocation (no character), 2 for automatic trigger (character typed)
                              :triggerKind ,(if (null lsp-proxy--last-inserted-char) 1 2))))
                          :cancel-on-input t))
                   (items (mapcar (lambda (candidate)
                                    (let* ((item (plist-get candidate :item))
                                           (label (plist-get item :label)))
                                      (propertize label 'lsp-proxy--item candidate)))
                                  resp)))
              items))))
    (list
     bounds-start
     (point)
     (lambda (probe pred action)
       (cond
        ((eq action 'metadata)
         '(metadata (category . lsp-proxy-capf)
           (display-sort-function . identity)
           (cycle-sort-function . identity)))
        ((eq (car-safe action) 'boundaries) nil)
        (t
         (complete-with-action action (funcall candidates) probe pred))))
     :annotation-function #'lsp-proxy--annotate
     :company-kind #'lsp-proxy--candidate-kind
     :company-require-match 'never
     :company-prefix-length
     (save-excursion
       (and (lsp-proxy--looking-back-trigger-characterp trigger-characters) t))
     :company-doc-buffer #'lsp-proxy--doc-buffer
     :exit-function #'lsp-proxy--company-post-completion)))

;;; Completion item handling

(defun lsp-proxy--candidate-kind (item)
  "Return ITEM's kind."
  (let* ((proxy-item (get-text-property 0 'lsp-proxy--item item))
         (completion-item (plist-get proxy-item :item))
         (kind (and completion-item (plist-get completion-item :kind))))
    (alist-get kind eglot--kind-names)))

(defun lsp-proxy--annotate (item)
  "Annotate ITEM detail."
  (let* ((proxy-item (get-text-property 0 'lsp-proxy--item item))
         (completion-item (plist-get proxy-item :item))
         (kind (and completion-item (plist-get completion-item :kind)))
         (detail (and completion-item (plist-get completion-item :detail)))
         (label-detail (and completion-item (plist-get completion-item :labelDetails))))
    (concat
     (when detail
       (concat " " (s-replace "\r" "" detail)))
     (when-let* ((label--detail (and label-detail (plist-get label-detail :detail))))
       (format " %s" label--detail))
     (when-let* ((description (and label-detail (plist-get label-detail :description))))
       (format " %s" description))
     (when-let* ((kind-name (alist-get kind eglot--kind-names)))
       (format " (%s)" kind-name)))))

(defun lsp-proxy--doc-buffer (item)
  "Get ITEM doc."
  (when-let* ((proxy-item (get-text-property 0 'lsp-proxy--item item))
              (langauge-sever-id (plist-get proxy-item :language_server_id))
              (completion-item (plist-get proxy-item :item)))
    (let ((documentation (plist-get completion-item :documentation)))
      (unless (or documentation (get-text-property 0 'resolved-item item))
        (let* ((resolved-item (lsp-proxy--sync-resolve proxy-item))) ;; (read item) 去掉了属性？
          (put-text-property 0 (length item) 'resolved-item resolved-item item)))))
  (when-let* ((resolved-item (or (get-text-property 0 'resolved-item item) (get-text-property 0 'lsp-proxy--item item)))
              (completion-item (plist-get resolved-item :item))
              (documentation (plist-get completion-item :documentation))
              (formatted (eglot--format-markup documentation)))
    (with-current-buffer (get-buffer-create "*lsp-proxy-doc*")
      (erase-buffer)
      (insert formatted)
      (current-buffer))))

(defun lsp-proxy--sync-resolve (proxy-item)
  "Request `completionItem/resolve' of PROXY-ITEM synchronously."
  (when-let* ((language-server-id (plist-get proxy-item :language_server_id))
              (start (plist-get proxy-item :start))
              (end (plist-get proxy-item :end))
              (item (plist-get proxy-item :item)))
    (lsp-proxy--request
     'completionItem/resolve
     (lsp-proxy--request-or-notify-params
      item
      `(:context (:language-server-id ,language-server-id :start ,start :end ,end)))
     :cancel-on-input t)))

(defun lsp-proxy--async-resolve (proxy-item callback &optional cleanup-fn)
  "Resolve completion PROXY-ITEM asynchronously with CALLBACK.
The CLEANUP-FN will be called to cleanup."
  (when-let* ((language-server-id (plist-get proxy-item :language_server_id))
              (start (plist-get proxy-item :start))
              (end (plist-get proxy-item :end))
              (item (plist-get proxy-item :item)))
    (lsp-proxy--async-request
     'completionItem/resolve
     (lsp-proxy--request-or-notify-params item `(:context (:language-server-id ,language-server-id :start ,start :end ,end)))
     :success-fn (lambda (resolved-item)
                   (if-let* ((complete-item (plist-get resolved-item :item))
                             (additionalTextEdits (plist-get complete-item :additionalTextEdits)))
                       (funcall callback additionalTextEdits))
                   (when cleanup-fn (funcall cleanup-fn)))
     :error-fn cleanup-fn
     :timeout-fn cleanup-fn)))

;;; Post-completion handling

(defun lsp-proxy--company-post-completion (candidate status)
  "Replace a CompletionItem's label with its insertText.
Apply text edits in CANDIDATE when STATUS is finished or exact."
  (when (memq status '(finished exact))
    (let* ((proxy-item (get-text-property 0 'lsp-proxy--item candidate))
           (resolved-item (get-text-property 0 'resolved-item candidate))
           (language-server-name (plist-get proxy-item :language_server_name))
           (marker (copy-marker (point) t)))
      (unless proxy-item
        (message "no lsp-proxy--item in post-completion %s" proxy-item))
      ;; typescript-language-server and vtsls does not provide the proper insertText without resolving.
      (if (or (equal language-server-name "typescript-language-server") (equal language-server-name "vtsls"))
          (if resolved-item
              (lsp-proxy--company-post-completion-item resolved-item candidate marker)
            (let ((resolved (lsp-proxy--sync-resolve proxy-item)))
              (put-text-property 0 (length candidate) 'resolved-item resolved candidate)
              (lsp-proxy--company-post-completion-item (or resolved proxy-item) candidate marker)))
        (lsp-proxy--company-post-completion-item (or resolved-item proxy-item) candidate marker)))))

(defun lsp-proxy--company-post-completion-item (proxy-item candidate marker)
  "Complete CANDIDATE of PROXY-ITEM from MARKER."
  (let* ((item (plist-get proxy-item :item))
         (label (plist-get item :label))
         (insertText (plist-get item :insertText))
         ;; 1 = plaintext, 2 = snippet
         (insertTextFormat (plist-get item :insertTextFormat))
         (textEdit (plist-get item :textEdit))
         (additionalTextEdits (plist-get item :additionalTextEdits))
         (startPoint (- marker (length candidate)))
         (insertTextMode (plist-get item :insertTextMode))
         (start (plist-get proxy-item :start))
         (end (plist-get proxy-item :end)))
    (cond (textEdit
           (let* ((range (plist-get textEdit :range))
                  (replaceStart (eglot--lsp-position-to-point (plist-get range :start)))
                  (replaceEnd (eglot--lsp-position-to-point (plist-get range :end)))
                  (newText (plist-get textEdit :newText))
                  (insertText (s-replace "\r" "" (or newText ""))))
             (delete-region start end)
             (delete-region replaceStart replaceEnd)
             (insert insertText)))
          ;; A snippet should be inserted, but using plain
          ;; `insertText'.  This requires us to delete the
          ;; whole completion, since `insertText' is the full
          ;; completion's text.
          (insertText
           (delete-region (- end (length candidate)) end)
           (insert (or insertText label))))
    (lsp-proxy--indent-lines startPoint (point) insertTextMode)
    (when (eq insertTextFormat 2)
      (lsp-proxy--expand-snippet (buffer-substring startPoint (point))
                                 startPoint
                                 (point)))
    (if (cl-plusp (length additionalTextEdits))
        (eglot--apply-text-edits additionalTextEdits)
      (if-let* ((resolved-item (get-text-property 0 'resolved-item candidate)))
          (if-let* ((additionalTextEdits (plist-get resolved-item :additionalTextEdits)))
              (eglot--apply-text-edits additionalTextEdits))
        (-let [(callback cleanup-fn) (lsp-proxy--create-apply-text-edits-handlers)]
          (lsp-proxy--async-resolve proxy-item callback cleanup-fn))))))

;;; Setup and teardown

(defun lsp-proxy--completion-setup ()
  "Setup completion."
  ;; Ensure that `lsp-proxy-completion-at-point' is the first CAPF to be tried
  (add-hook 'completion-at-point-functions #'lsp-proxy-completion-at-point -50 t)

  (make-local-variable 'completion-category-defaults)
  (setf (alist-get 'lsp-proxy-capf completion-category-defaults) '((styles . (lsp-proxy-passthrough))))

  (make-local-variable 'completion-styles-alist)
  (setf (alist-get 'lsp-proxy-passthrough completion-styles-alist)
        '(lsp-proxy--dumb-tryc
          lsp-proxy-passthrough-all-completions
          "Passthrough completion.")))

(defun lsp-proxy--completion-teardown ()
  "Teardown completion."
  (remove-hook 'completion-at-point-functions #'lsp-proxy-completion-at-point 'local)
  (setq-local completion-category-defaults
              (cl-remove 'lsp-proxy-capf completion-category-defaults :key #'cl-first))
  (setq-local completion-styles-alist
              (cl-remove 'lsp-proxy-passthrough completion-styles-alist :key #'cl-first)))

(provide 'lsp-proxy-completion)
;;; lsp-proxy-completion.el ends here
