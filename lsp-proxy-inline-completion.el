;;; lsp-proxy-inline-completion.el --- Inline completion support for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Inline completion functionality for lsp-proxy, similar to GitHub Copilot.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'dash)
(require 'lsp-proxy-core)
(require 'lsp-proxy-utils)

(defvar lsp-proxy-inline-completion-mode)

;;; Configuration

(defcustom lsp-proxy-inline-completion-enable-predicates '(evil-insert-state-p)
  "A list of predicate functions with no argument to enable inlineCompletion.
InlineCompletion will be triggered only if all predicates return t."
  :type '(repeat function)
  :group 'lsp-proxy)

(defcustom lsp-proxy-inline-completion-disable-predicates nil
  "A list of predicate functions with no argument to disable inlineCompletion.
InlineCompletion will not be triggered if any predicate returns t."
  :type '(repeat function)
  :group 'lsp-proxy)

;;; Configuration variables
(defcustom lsp-proxy-inline-completion-trigger-characters '()
  "List of characters that trigger inline completion."
  :type '(repeat character)
  :group 'lsp-proxy)

(defcustom lsp-proxy-inline-completion-idle-delay 0.3
  "Idle delay in seconds before showing inline completion."
  :type 'number
  :group 'lsp-proxy)
(defvar eglot--versioned-identifier)

;;; Faces

(defface lsp-proxy-inline-completion-overlay-face
  '((t :inherit shadow))
  "Face for displaying inline completion."
  :group 'lsp-proxy)

;;; Variables

(defvar-local lsp-proxy-inline-completion--items nil "The completions provided by the server.")
(defvar-local lsp-proxy-inline-completion--current nil "The current suggestion to be displayed.")
(defvar-local lsp-proxy-inline-completion--overlay nil "The overlay displaying code suggestions.")
(defvar-local lsp-proxy-inline-completion--start-point nil "The point where the completion started.")
(defvar-local lsp-proxy-inline-completion--keymap-overlay nil
  "Overlay used to surround point.
Make lsp-proxy-inline-completion-active-map activate.")
(defvar-local lsp-proxy-inline-completion--real-posn nil
  "Posn information without overlay.
To work around posn problems with after-string property.")

;;; Utility functions

(defmacro lsp-proxy-inline-completion--satisfy-predicates (enable disable)
  "Return t if satisfy all predicates in ENABLE and none in DISABLE."
  `(and (cl-every (lambda (pred)
                    (if (functionp pred) (funcall pred) t))
                  ,enable)
        (cl-notany (lambda (pred)
                     (if (functionp pred) (funcall pred) nil))
                   ,disable)))

(defun lsp-proxy-inline-completion--satisfy-trigger-predicates ()
  "Return t if all trigger predicates are satisfied."
  (lsp-proxy-inline-completion--satisfy-predicates
   lsp-proxy-inline-completion-enable-predicates
   lsp-proxy-inline-completion-disable-predicates))

(defsubst lsp-proxy-inline-completion--overlay-visible ()
  "Return whether the overlay is available."
  (and (overlayp lsp-proxy-inline-completion--overlay)
       (overlay-buffer lsp-proxy-inline-completion--overlay)))

;;; Keymap

(defvar lsp-proxy-inline-completion-active-map
  (let ((map (make-sparse-keymap)))
    ;; accept
    (define-key map (kbd "C-<return>") #'lsp-proxy-inline-completion-accept)
    (define-key map (kbd "<tab>") #'lsp-proxy-inline-completion-accept)
    (define-key map (kbd "TAB") #'lsp-proxy-inline-completion-accept)
    (define-key map (kbd "C-<tab>") #'lsp-proxy-inline-completion-accept-by-word)
    (define-key map (kbd "C-TAB") #'lsp-proxy-inline-completion-accept-by-word)
    (define-key map (kbd "<backtab>") #'lsp-proxy-inline-completion-accept-by-line)
    ;; navigate
    (define-key map (kbd "C-n") #'lsp-proxy-inline-completion-next)
    (define-key map (kbd "C-p") #'lsp-proxy-inline-completion-prev)
    ;; useful -- recenter without losing the completion
    (define-key map (kbd "C-l") #'recenter-top-bottom)
    ;; ignore
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [up-mouse-1] #'ignore)
    (define-key map [mouse-movement] #'ignore)
    map)
  "Keymap active when showing inline code suggestions.")

;;; Overlay management

(defun lsp-proxy-inline-completion--clear-overlay ()
  "Clear inline completion suggestion overlay."
  (when (lsp-proxy-inline-completion--overlay-visible)
    (delete-overlay lsp-proxy-inline-completion--keymap-overlay)
    (delete-overlay lsp-proxy-inline-completion--overlay))
  (setq-local lsp-proxy-inline-completion--real-posn nil))

(defun lsp-proxy-inline-completion--get-or-create-keymap-overlay ()
  "Make or return the local lsp-proxy-inline-completion--keymap-overlay."
  (unless (overlayp lsp-proxy-inline-completion--keymap-overlay)
    (setq lsp-proxy-inline-completion--keymap-overlay (make-overlay 1 1 nil nil t))
    (overlay-put lsp-proxy-inline-completion--keymap-overlay 'keymap lsp-proxy-inline-completion-active-map)
    (overlay-put lsp-proxy-inline-completion--keymap-overlay 'priority 101))
  lsp-proxy-inline-completion--keymap-overlay)

(defun lsp-proxy-inline-completion--get-overlay ()
  "Build the suggestion overlay."
  (unless (overlayp lsp-proxy-inline-completion--overlay)
    (setq lsp-proxy-inline-completion--overlay (make-overlay 1 1 nil nil t))
    (overlay-put lsp-proxy-inline-completion--overlay 'keymap-overlay (lsp-proxy-inline-completion--get-or-create-keymap-overlay)))
  lsp-proxy-inline-completion--overlay)

(defun lsp-proxy-inline-completion--overlay-end (ov)
  "Return the end position of overlay OV."
  (- (line-end-position) (overlay-get ov 'tail-length)))

(defun lsp-proxy-inline-completion--set-overlay-text (ov completion)
  "Set overlay OV with COMPLETION."
  (move-overlay ov (point) (line-end-position))

  ;; set overlay position for the keymap, to activate lsp-proxy-inline-completion-active-map
  (move-overlay (overlay-get ov 'keymap-overlay) (point) (min (point-max) (+ 1 (point))))
  (let* ((tail (buffer-substring (lsp-proxy-inline-completion--overlay-end ov) (line-end-position)))
         (p-completion (concat (propertize completion 'face 'lsp-proxy-inline-completion-overlay-face)
                               tail)))
    (if (eolp)
        (progn
          (overlay-put ov 'after-string "") ; make sure posn is correct
          (setq lsp-proxy-inline-completion--real-posn (cons (point) (posn-at-point)))
          (put-text-property 0 1 'cursor t p-completion)
          (overlay-put ov 'display "")
          (overlay-put ov 'after-string p-completion))
      (overlay-put ov 'display (substring p-completion 0 1))
      (overlay-put ov 'after-string (substring p-completion 1)))
    (overlay-put ov 'completion completion)
    (overlay-put ov 'start (point))))

;;; Completion display

(defun lsp-proxy-inline-completion-display (&optional implicit)
  "Display the inline completions overlay."
  (interactive)
  (condition-case err
      (lsp-proxy--async-request
       'textDocument/inlineCompletion
       (lsp-proxy--request-or-notify-params
        (append (eglot--TextDocumentPositionParams)
                `(:context (:triggerKind ,(if implicit 2 1))))
        `(:context
          (:triggerKind ,(if implicit 2 1)
           :selectedCompletionInfo nil
           :line ,(buffer-substring-no-properties (line-beginning-position) (line-end-position))
           :docVersion ,eglot--versioned-identifier)))
       :success-fn
       (lambda (resp)
         (when resp
           (let* ((doc-version (plist-get resp :docVersion))
                  (items (plist-get resp :items)))
             (if (= doc-version eglot--versioned-identifier)
                 (when (and items (> (length items) 0))
                   (setq lsp-proxy-inline-completion--items items)
                   (setq lsp-proxy-inline-completion--current 0)
                   (setq lsp-proxy-inline-completion--start-point (point))
                   (lsp-proxy-inline-completion-show-completion))
               (lsp-proxy--warn "%s" "Mismatch version.")))))
       :timeout-fn #'ignore)
    (t (lsp-proxy--error "Could not fetch completions: %s" err))))

(defun lsp-proxy-inline-completion--string-common-prefix (str1 str2)
  "Find the common prefix of STR1 and STR2 directly."
  (let ((min-len (min (length str1) (length str2)))
        (i 0))
    (while (and (< i min-len)
                (= (aref str1 i) (aref str2 i)))
      (setq i (1+ i)))
    (substring str1 0 i)))

(defun lsp-proxy-inline-completion-show-completion ()
  "Make the suggestion overlay visible."
  (unless (and lsp-proxy-inline-completion--items
               (> (length lsp-proxy-inline-completion--items) 0)
               (numberp lsp-proxy-inline-completion--current))
    (error "No completions to show"))
  (save-excursion
    (save-restriction
      (widen)
      (-let* ((p (point))
              (suggestion
               (elt lsp-proxy-inline-completion--items
                    lsp-proxy-inline-completion--current))
              (insert-text (plist-get suggestion :insertText))
              (line (map-nested-elt suggestion '(:range :start :line)))
              (start-char (map-nested-elt suggestion '(:range :start :character)))
              (end-char (map-nested-elt suggestion '(:range :end :character)))
              (goto-line! (lambda ()
                            (goto-char (point-min))
                            (forward-line line)))
              (start (progn
                       (funcall goto-line!)
                       (forward-char start-char)
                       (let* ((cur-line (buffer-substring-no-properties (point) (line-end-position)))
                              (common-prefix-len (length (lsp-proxy-inline-completion--string-common-prefix insert-text cur-line))))
                         (setq insert-text (substring insert-text common-prefix-len))
                         (forward-char common-prefix-len)
                         (point))))
              (end (progn
                     (funcall goto-line!)
                     (forward-char end-char)
                     (point))))
        (goto-char p)
        (lsp-proxy-inline-completion--display-overlay-completion insert-text start end)))))

(defun lsp-proxy-inline-completion--display-overlay-completion (completion start end)
  "Show COMPLETION between START and END."
  (lsp-proxy-inline-completion--clear-overlay)
  (when (and (not (string-blank-p completion))
             (or (<= start (point))))
    (let* ((ov (lsp-proxy-inline-completion--get-overlay)))
      (overlay-put ov 'tail-length (- (line-end-position) end))
      (overlay-put ov 'completion-start start)
      (overlay-put ov 'completion-end end)
      (lsp-proxy-inline-completion--set-overlay-text ov completion))))

;;; Interactive commands

(defun lsp-proxy-inline-completion-next ()
  "Display the next inline completion."
  (interactive)
  (unless (lsp-proxy-inline-completion--overlay-visible)
    (error "Not showing suggestions"))
  (setq lsp-proxy-inline-completion--current
        (mod (1+ lsp-proxy-inline-completion--current)
             (length lsp-proxy-inline-completion--items)))
  (lsp-proxy-inline-completion-show-completion))

(defun lsp-proxy-inline-completion-prev ()
  "Display the previous inline completion."
  (interactive)
  (unless (lsp-proxy-inline-completion--overlay-visible)
    (error "Not showing suggestions"))
  (setq lsp-proxy-inline-completion--current
        (mod (1- lsp-proxy-inline-completion--current)
             (length lsp-proxy-inline-completion--items)))
  (lsp-proxy-inline-completion-show-completion))

(defun lsp-proxy-inline-completion-cancel ()
  "Cancel current inline completion, if any."
  (interactive)
  (when (lsp-proxy-inline-completion--overlay-visible)
    (lsp-proxy-inline-completion--clear-overlay)
    (when lsp-proxy-inline-completion--start-point
      (goto-char lsp-proxy-inline-completion--start-point))))

(defun lsp-proxy-inline-completion-accept (&optional transform-fn)
  "Accept the current suggestion.
Return t if there is a completion. Use TRANSFORM-FN to transform completion if provided."
  (interactive)
  (unless (lsp-proxy-inline-completion--overlay-visible)
    (error "Not showing suggestions"))
  (-let* ((insert-text (overlay-get lsp-proxy-inline-completion--overlay 'completion))
          (t-insert-text (funcall (or transform-fn #'identity) insert-text))
          (completion-start (overlay-get lsp-proxy-inline-completion--overlay 'completion-start))
          (completion-end (overlay-get lsp-proxy-inline-completion--overlay 'completion-end)))
    (delete-region completion-start completion-end)
    (insert t-insert-text)
    (lsp-proxy-inline-completion--clear-overlay)
    ;; if it is a partial completion
    (when (and (string-prefix-p t-insert-text insert-text)
               (not (string-equal t-insert-text insert-text)))
      (lsp-proxy-inline-completion--set-overlay-text
       (lsp-proxy-inline-completion--get-overlay)
       (string-remove-prefix t-insert-text insert-text)))
    t))

(defmacro lsp-proxy-inline-completion--define-accept-completion-by-action (func-name action)
  "Define function FUNC-NAME to accept completion by ACTION."
  `(defun ,func-name (&optional n)
     (interactive "p")
     (setq n (or n 1))
     (lsp-proxy-inline-completion-accept (lambda (completion)
                                  (with-temp-buffer
                                    (insert completion)
                                    (goto-char (point-min))
                                    (funcall ,action n)
                                    (buffer-substring-no-properties (point-min) (point)))))))

(lsp-proxy-inline-completion--define-accept-completion-by-action
 lsp-proxy-inline-completion-accept-by-word #'forward-word)
(lsp-proxy-inline-completion--define-accept-completion-by-action
 lsp-proxy-inline-completion-accept-by-line #'forward-line)

;;; Self-insert handling

(defun lsp-proxy-inline-completion--self-insert (command)
  "Handle the case where the char just inserted is the start of the completion.
If so, update the overlays and continue. COMMAND is the command that triggered
in `post-command-hook'."
  (when (and (eq command 'self-insert-command)
             (lsp-proxy-inline-completion--overlay-visible))
    (let* ((ov lsp-proxy-inline-completion--overlay)
           (completion (overlay-get ov 'completion))
           (completion-end (overlay-get lsp-proxy-inline-completion--overlay 'completion-end))
           (completion-start (overlay-get lsp-proxy-inline-completion--overlay 'completion-start))
           ;; the last point before self-insert
           (prev-point (max (point-min) (1- (point)))))
      (when (and (eq last-command-event (elt completion 0))
                 (not (memq last-command-event lsp-proxy-inline-completion-trigger-characters)))
        (if (= (length completion) 1)
            ;; If there is only one char in the completion, accept it
            (progn
              (overlay-put ov 'completion (substring completion 1))
              (lsp-proxy-inline-completion-accept))
          (when (and (> completion-end prev-point) (>= completion-start prev-point))
            (overlay-put ov 'completion-start (+ completion-start 1))
            (overlay-put ov 'completion-end (+ completion-end 1)))
          (lsp-proxy-inline-completion--set-overlay-text ov (substring completion 1)))))))

(defun lsp-proxy-inline-completion--post-command-debounce (buffer)
  "Handle post-command debouncing for BUFFER."
  (when (and lsp-proxy-inline-completion-mode
             (buffer-live-p buffer)
             (equal buffer (current-buffer))
             (lsp-proxy-inline-completion--satisfy-trigger-predicates))
    (lsp-proxy-inline-completion-display 'implicit)))

(defvar lsp-proxy--inline-completion-debounce-timer nil)
(defun lsp-proxy-inline-completion-handle-command ()
  "Handle inline completion logic given THIS-COMMAND."
  ;; Clear overlay and cancel debounce timer if applicable
  (unless (or (and (symbolp this-command)
                   (or (string-prefix-p "lsp-proxy-inline-completion-" (symbol-name
                                                                        this-command))
                       (lsp-proxy-inline-completion--self-insert this-command))))
    (lsp-proxy-inline-completion--clear-overlay)
    (when lsp-proxy--inline-completion-debounce-timer
      (cancel-timer lsp-proxy--inline-completion-debounce-timer)))

  ;; Schedule inline completion debounce if idle delay is set
  (when (numberp lsp-proxy-inline-completion-idle-delay)
    (setq lsp-proxy--inline-completion-debounce-timer
          (run-with-idle-timer
           lsp-proxy-inline-completion-idle-delay
           nil
           #'lsp-proxy-inline-completion--post-command-debounce
           (current-buffer)))))

;;; Position advice

(defun lsp-proxy-inline-completion--posn-advice (&rest args)
  "Remap posn if in lsp-proxy-inline-completion-mode with ARGS."
  (when lsp-proxy-inline-completion-mode
    (let ((pos (or (car-safe args) (point))))
      (when (and lsp-proxy-inline-completion--real-posn
                 (eq pos (car lsp-proxy-inline-completion--real-posn)))
        (cdr lsp-proxy-inline-completion--real-posn)))))

;;; Minor mode

;;;###autoload
(define-minor-mode lsp-proxy-inline-completion-mode
  "Auto inline complete mode."
  :init-value nil
  :lighter " InlineComp"
  (lsp-proxy-inline-completion--clear-overlay)
  (advice-add 'posn-at-point :before-until #'lsp-proxy-inline-completion--posn-advice)
  (unless lsp-proxy-inline-completion-mode
    (lsp-proxy-inline-completion-cancel)))

(provide 'lsp-proxy-inline-completion)
;;; lsp-proxy-inline-completion.el ends here
