;;; lsp-proxy-diagnostics.el --- Diagnostics integration for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Flycheck and Flymake integration for lsp-proxy diagnostics

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'lsp-proxy-utils)
(require 'lsp-proxy-core)

(defcustom lsp-proxy-diagnostics-provider :auto
  "The checker backend provider."
  :type
  '(choice
    (const :tag "Pick flycheck if present and fallback to flymake" :auto)
    (const :tag "Pick flycheck" :flycheck)
    (const :tag "Pick flymake" :flymake)
    (const :tag "Use neither flymake nor lsp" :none)
    (const :tag "Prefer flymake" t)
    (const :tag "Prefer flycheck" nil))
  :group 'lsp-proxy)

(defcustom lsp-proxy-diagnostics-buffer "*lsp-proxy-diagnostics*"
  "Buffer for display diagnostics."
  :type 'string
  :group 'lsp-proxy)


;;; External declarations

(declare-function flycheck-buffer "ext:flycheck")
(declare-function flycheck-mode "ext:flycheck")
(declare-function flycheck-define-generic-checker
                  "ext:flycheck" (symbol docstring &rest properties))
(declare-function flycheck-error-new "ext:flycheck" t t)
(declare-function flycheck-valid-checker-p "ext:flycheck")
(declare-function flycheck-checker-supports-major-mode-p "ext:flycheck")
(declare-function flycheck-add-mode "ext:flycheck")

(declare-function flymake-mode "ext:flymake")
(declare-function flymake-make-diagnostic "ext:flymake")
(declare-function flymake-diag-region "ext:flymake")

;;; Variables

(defvar flycheck-mode)
(defvar flycheck-checker)
(defvar flycheck-checkers)
(defvar flymake-diagnostic-functions)
(defvar flymake-mode)

(defvar lsp-proxy--diagnostics-map (make-hash-table :test 'equal)
  "Hash table mapping project roots to diagnostic maps.")

(defvar-local lsp-proxy-diagnostics--flycheck-enabled nil
  "Non-nil when flycheck integration has been enabled in this buffer.")

(defvar-local lsp-proxy-diagnostics--flymake-enabled nil
  "Non-nil when flymake integration has been enabled in this buffer.")

(defvar-local lsp-proxy-diagnostics--flycheck-checker nil
  "The value of flycheck-checker before lsp-proxy diagnostics was activated.")

(defvar-local lsp-proxy-diagnostics--flymake-report-fn nil
  "Report function for flymake backend.")

;;; External variables (defined in other modules)
(defvar lsp-proxy-mode)

(defun lsp-proxy-diagnostics--handle-publish-diagnostics (msg)
  "Handle publish diagnostics notification MSG."
  (lsp-proxy--dbind (:uri uri :diagnostics diagnostics) msg
    (let ((filepath (lsp-proxy--uri-to-path uri)))
      (if (file-exists-p filepath)
          (with-current-buffer (find-file-noselect filepath)
            (let ((workspace-diagnostics (lsp-proxy--ensure-project-map
                                          (lsp-proxy-project-root)
                                          lsp-proxy--diagnostics-map))
                  (file (lsp-proxy--fix-path-casing filepath)))
              (if (seq-empty-p diagnostics)
                  (remhash file workspace-diagnostics)
                (puthash file (append diagnostics nil) workspace-diagnostics)))
            (cond (lsp-proxy-diagnostics--flycheck-enabled
                   (add-hook 'lsp-proxy-on-idle-hook #'lsp-proxy-diagnostics--flycheck-buffer nil t)
                   (lsp-proxy--idle-reschedule (current-buffer)))
                  (lsp-proxy-diagnostics--flymake-enabled
                   (lsp-proxy-diagnostics--flymake-after-diagnostics))
                  (t (when lsp-proxy-mode
                       (lsp-proxy--warn "No diagnostics mode enabled for this buffer. Ensure Flycheck or Flymake is active.")))))
        (if (> lsp-proxy-log-level 1)
            (lsp-proxy--error "The file not found %s (uri=%s)" filepath uri))))))

;;; Flycheck integration

(defun lsp-proxy-diagnostics--flycheck-buffer ()
  "Trigger flycheck on buffer."
  (remove-hook 'lsp-proxy-on-idle-hook #'lsp-proxy-diagnostics--flycheck-buffer t)
  (when (bound-and-true-p flycheck-mode)
    (flycheck-buffer)))

(defun lsp-proxy-diagnostics--flycheck-start (checker callback)
  "Start an LSP syntax check with CHECKER.
CALLBACK is the status callback passed by Flycheck."
  (remove-hook 'lsp-proxy-on-idle-hook #'lsp-proxy-diagnostics--flycheck-buffer t)
  (let* ((workspace-diagnostics (lsp-proxy--ensure-project-map (lsp-proxy-project-root) lsp-proxy--diagnostics-map))
         (buffer-diagnostics (gethash (lsp-proxy--fix-path-casing buffer-file-name) workspace-diagnostics '()))
         (errors (mapcar
                  (lambda (diagnostic)
                    (let* ((range (plist-get diagnostic :range))
                           (start (plist-get range :start))
                           (end (plist-get range :end)))
                      (flycheck-error-new
                       :buffer (current-buffer)
                       :checker checker
                       :filename (buffer-file-name)
                       :message (plist-get diagnostic :message)
                       :level (pcase (plist-get diagnostic :severity)
                                (1 'error)
                                (2 'warning)
                                (3 'info)
                                (4 'info)
                                (_ 'error))
                       :id (plist-get diagnostic :code)
                       :group (plist-get diagnostic :source)
                       :line (1+ (plist-get start :line))
                       :column (1+ (plist-get start :character))
                       :end-line (1+ (plist-get end :line))
                       :end-column (1+ (plist-get end :character)))))
                  buffer-diagnostics)))
    (funcall callback 'finished errors)))

;;;###autoload
(defun lsp-proxy-diagnostics-lsp-proxy-checker-if-needed ()
  "Create a `lsp-proxy' checker of flycheck."
  (unless (flycheck-valid-checker-p 'lsp-proxy)
    (flycheck-define-generic-checker 'lsp-proxy
      "A syntax checker using the langauge server protocol provided by lsp-proxy."
      :start #'lsp-proxy-diagnostics--flycheck-start
      :modes '(lsp-proxy-placeholder-mode)
      :predicate (lambda () lsp-proxy-mode))))

(defun lsp-proxy-diagnostics-flycheck-enable (&rest _)
  "Enable flycheck integration for the current buffer."
  (require 'flycheck)
  (lsp-proxy-diagnostics-lsp-proxy-checker-if-needed)
  (unless lsp-proxy-diagnostics--flycheck-enabled
    (setq-local lsp-proxy-diagnostics--flycheck-enabled t)
    (add-to-list 'flycheck-checkers 'lsp-proxy)
    (unless (flycheck-checker-supports-major-mode-p 'lsp-proxy major-mode)
      (flycheck-add-mode 'lsp-proxy major-mode)))
  (flycheck-mode 1))

(defun lsp-proxy-diagnostics-flycheck-disable (&rest _)
  "Disable flycheck integartion for the current buffer."
  (when lsp-proxy-diagnostics--flycheck-enabled
    (setq-local lsp-proxy-diagnostics--flycheck-enabled nil)))

;;; Flymake integration

(defun lsp-proxy-diagnostics-flymake-enable ()
  "Setup flymake."
  (setq lsp-proxy-diagnostics--flymake-report-fn nil)
  (unless lsp-proxy-diagnostics--flymake-enabled
    (setq-local lsp-proxy-diagnostics--flymake-enabled t)
    (add-hook 'flymake-diagnostic-functions 'lsp-proxy-diagnostics--flymake-backend nil t))
  (flymake-mode 1))

(defun lsp-proxy-diagnostics-flymake-disable ()
  "Disable flymake integartion for the current buffer."
  (when lsp-proxy-diagnostics--flymake-enabled
    (setq-local lsp-proxy-diagnostics--flymake-enabled nil)))

(defun lsp-proxy-diagnostics--flymake-after-diagnostics ()
  "Handler for diagnostics update."
  (cond
   ((and lsp-proxy-diagnostics--flymake-report-fn flymake-mode)
    (lsp-proxy-diagnostics--flymake-update-diagnostics))
   ((not flymake-mode)
    (setq lsp-proxy-diagnostics--flymake-report-fn nil))))

(defun lsp-proxy-diagnostics--flymake-backend (report-fn &rest _args)
  "Flymake backend using REPORT-FN."
  (let ((first-run (null lsp-proxy-diagnostics--flymake-report-fn)))
    (setq lsp-proxy-diagnostics--flymake-report-fn report-fn)
    (when first-run
      (lsp-proxy-diagnostics--flymake-update-diagnostics))))

(defun lsp-proxy-diagnostics--flymake-update-diagnostics ()
  "Report new diagnostics to flymake."
  (let* ((workspace-diagnostics (lsp-proxy--ensure-project-map (lsp-proxy-project-root) lsp-proxy--diagnostics-map))
         (buffer-diagnostics (gethash (lsp-proxy--fix-path-casing buffer-file-name) workspace-diagnostics '()))
         (diags (mapcar
                 (lambda (diagnostic)
                   (let* ((message (plist-get diagnostic :message))
                          (severity (plist-get diagnostic :severity))
                          (range (plist-get diagnostic :range))
                          (start (plist-get range :start))
                          (end (plist-get range :end))
                          (start-line (plist-get start :line))
                          (character (plist-get start :character))
                          (end-line (plist-get end :line))
                          (start-point (eglot--lsp-position-to-point start))
                          (end-point (eglot--lsp-position-to-point end)))
                     (when (= start-point end-point)
                       (if-let* ((region (flymake-diag-region (current-buffer)
                                                              (1+ start-line)
                                                              character)))
                           (setq start-point (car region)
                                 end-point (cdr region))
                         (eglot--widening
                          (goto-char (point-min))
                          (setq start-point (line-beginning-position (1+ start-line))
                                end-point (line-end-position (1+ end-line))))))
                     (flymake-make-diagnostic (current-buffer)
                                              start-point
                                              end-point
                                              (cl-case severity
                                                (1 :error)
                                                (2 :warning)
                                                (t :note))
                                              message)))
                 buffer-diagnostics)))
    (funcall lsp-proxy-diagnostics--flymake-report-fn
             diags
             ;; This :region keyword forces flymake to delete old diagnostics in
             ;; case the buffer hasn't changed since the last call to the report
             ;; function. See https://github.com/joaotavora/eglot/issues/159
             :region (cons (point-min) (point-max)))))

;;; Pull diagnostics support

(defun lsp-proxy-diagnostics--request-pull-diagnostics ()
  "Request pull diagnostics if supported."
  (when (and (boundp 'lsp-proxy--support-pull-diagnostic) lsp-proxy--support-pull-diagnostic)
    (lsp-proxy--async-request
     'textDocument/diagnostic
     (lsp-proxy--request-or-notify-params
      (list :textDocument (eglot--TextDocumentIdentifier)))
     :timeout-fn #'ignore)))

;;; Project diagnostics buffer

(defvar lsp-proxy-diagnostics-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'lsp-proxy-show-diagnostic)
    (define-key map (kbd "o") 'lsp-proxy-goto-diagnostic)
    map)
  "Keymap for lsp-proxy diagnostics buffer mode.")

(defun lsp-proxy-show-diagnostic (pos &optional other-window)
  "Show location of diagnostic at POS.
If OTHER-WINDOW is non nil, show diagnosis in a new window."
  (interactive (list (point) t))
  (let* ((id (or (tabulated-list-get-id pos)
                 (user-error "Nothing at point")))
         (filepath (plist-get id :filepath))
         (diag (plist-get id :diag))
         (range (plist-get diag :range))
         (start (plist-get range :start))
         (end (plist-get range :end)))
    (with-current-buffer (find-file-noselect filepath)
      (with-selected-window
          (display-buffer (current-buffer) other-window)
        (save-excursion
          (save-restriction
            (widen)
            (let* ((b (eglot--lsp-position-to-point start))
                   (e (eglot--lsp-position-to-point end)))
              (goto-char b)
              (pulse-momentary-highlight-region (point)
                                                (or e (line-end-position)) 'highlight)))))
      (current-buffer))))

(defun lsp-proxy-goto-diagnostic (pos)
  "Show location of diagnostic at POS."
  (interactive "d")
  (pop-to-buffer
   (lsp-proxy-show-diagnostic pos)))

(defvar lsp-proxy--diagnostics-base-tabulated-list-format
  `[("Type" 8 nil)
    ("File" 40 nil)
    ("Backend" 50 t)
    ("Message" 0 t)]
  "Base format for diagnostics tabulated list.")

(define-derived-mode lsp-proxy-diagnostics-buffer-mode tabulated-list-mode
  "Lsp proxy diagnostics"
  "A mode for listing Lsp proxy diagnostics."
  :interactive nil
  (setq tabulated-list-format lsp-proxy--diagnostics-base-tabulated-list-format)
  (tabulated-list-init-header))

(defun lsp-proxy-show-project-diagnostics ()
  "Show a list of diagnostics for current project."
  (interactive)
  (unless lsp-proxy-mode
    (user-error "Lsp proxy mode is not enabled in the current buffer"))
  (let ((workspace-diagnostics (lsp-proxy--ensure-project-map
                                (lsp-proxy-project-root)
                                lsp-proxy--diagnostics-map))
        (target (or (get-buffer lsp-proxy-diagnostics-buffer)
                    (with-current-buffer (get-buffer-create lsp-proxy-diagnostics-buffer)
                      (lsp-proxy-diagnostics-buffer-mode)
                      (current-buffer))))
        rows)
    (maphash (lambda (filepath diags)
               (setq rows (append
                           rows
                           (mapcar (lambda (diag)
                                     (let* ((source (plist-get diag :source))
                                            (code (plist-get diag :code))
                                            (severity (plist-get diag :severity))
                                            (msg (plist-get diag :message))
                                            (range (plist-get diag :range))
                                            (start (plist-get range :start))
                                            (line (plist-get start :line))
                                            (character (plist-get start :character)))
                                       (list (list :diag diag :filepath filepath)
                                             (vector (number-to-string severity)
                                                     (format "/%s/%s" (file-name-nondirectory (directory-file-name (file-name-directory filepath))) (file-name-nondirectory filepath))
                                                     (format "%s(%s)" source code)
                                                     (format "%s [Ln%s,Col%s]" msg (1+ line) (1+ character)))))) diags))))
             workspace-diagnostics)
    (with-current-buffer target
      (display-buffer (current-buffer))
      (setq tabulated-list-entries rows)
      (tabulated-list-print t)
      (revert-buffer))))

;;; Setup and teardown

(defun lsp-proxy--diagnostics-setup ()
  "Setup diagnostics."
  (cond
   ((or (and (eq lsp-proxy-diagnostics-provider :auto)
             (functionp 'flycheck-mode))
        (and (eq lsp-proxy-diagnostics-provider :flycheck)
             (or (functionp 'flycheck-mode)
                 (user-error "The lsp-proxy-diagnostics-provider is set to :flycheck but flycheck is not installed?"))))
    (require 'flycheck nil t)
    (lsp-proxy-diagnostics-flycheck-enable))
   ((or (eq lsp-proxy-diagnostics-provider :auto)
        (eq lsp-proxy-diagnostics-provider :flymake)
        (eq lsp-proxy-diagnostics-provider t))
    (require 'flymake)
    (lsp-proxy-diagnostics-flymake-enable))
   ((not (eq lsp-proxy-diagnostics-provider :none))
    (lsp-proxy--warn "%s" "Unable to autoconfigure flycheck/flymake. The diagnostics won't be rendered."))
   (t (lsp-proxy--warn "%s" "Unable to configuration flycheck. The diagnostics won't be rendered."))))

(defun lsp-proxy--diagnostics-teardown ()
  "Teardown disagnostics."
  (lsp-proxy-diagnostics-flycheck-disable)
  (lsp-proxy-diagnostics-flymake-disable))

(provide 'lsp-proxy-diagnostics)
;;; lsp-proxy-diagnostics.el ends here
