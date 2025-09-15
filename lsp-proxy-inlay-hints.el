;;; lsp-proxy-inlay-hints.el --- Inlay hints support for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Inlay hints functionality for lsp-proxy, based on backup file implementation.

;;; Code:

(require 'cl-lib)
(require 'jit-lock)
(require 'eglot)
(require 'lsp-proxy-utils)
(require 'lsp-proxy-core)

(defcustom lsp-proxy-inlay-hints-mode-config nil
  "Configuration for enabling inlay hints mode in specific contexts.
The value can be:
- nil: Disable inlay hints completely
- t:   Enable inlay hints for all buffers
- (mode1 mode2 ...): Enable only for specified major modes
Example: `(`emacs-lisp-mode' `python-mode')'"
  :type
  '(choice
    (const :tag "Disabled" nil)
    (const :tag "Enabled for all buffers" t)
    (repeat :tag "Enabled only for specific modes" symbol))
  :group 'lsp-proxy)

;;; External variables
(defvar lsp-proxy--support-inlay-hints)

;;; Faces
(defface lsp-proxy-inlay-hint-face '((t (:height 0.8 :inherit shadow)))
  "Face used for inlay hint overlays."
  :group 'lsp-proxy)

(defface lsp-proxy-type-hint-face '((t (:inherit lsp-proxy-inlay-hint-face)))
  "Face used for type inlay hint overlays."
  :group 'lsp-proxy)

(defface lsp-proxy-parameter-hint-face '((t (:inherit lsp-proxy-inlay-hint-face)))
  "Face used for parameter inlay hint overlays."
  :group 'lsp-proxy)

;;; Variables

(defvar-local lsp-proxy--outstanding-inlay-hints-region (cons nil nil)
  "Jit-lock-calculated (FROM . TO) region with potentially outdated hints.")

(defvar-local lsp-proxy--outstanding-inlay-hints-last-region nil)

(defvar-local lsp-proxy--outstanding-inlay-regions-timer nil
  "Helper timer for `lsp-proxy--update-hints'.")

;;; Functions

;; see eglot--update-hints
(cl-defun lsp-proxy--update-inlay-hints (from to)
  "Jit-lock function for lsp-proxy inlay hints.
Update the range of `(FROM TO)'."
  ;; XXX: We're relying on knowledge of jit-lock internals here.
  ;; Comparing `jit-lock-context-unfontify-pos' (if non-nil) to
  ;; `point-max' tells us whether this call to `jit-lock-functions'
  ;; happens after `jit-lock-context-timer' has just run.
  (when (and jit-lock-context-unfontify-pos
             (/= jit-lock-context-unfontify-pos (point-max)))
    (cl-return-from lsp-proxy--update-inlay-hints))
  (cl-symbol-macrolet ((region lsp-proxy--outstanding-inlay-hints-region)
                         (last-region lsp-proxy--outstanding-inlay-hints-last-region)
                         (timer lsp-proxy--outstanding-inlay-regions-timer))
      (setcar region (min (or (car region) (point-max)) from))
      (setcdr region (max (or (cdr region) (point-min)) to))
      ;; XXX: Then there is a smoothing timer.  I wish we didn't need it,
      ;; but sometimes a lot of calls come in all at once and do make it
      ;; past the check above.  Notice it is a 0 second timer though, so
      ;; we're not introducing any more delay over jit-lock's timers.
      (when timer (cancel-timer timer))
      (setq timer (run-at-time
                   0 nil
                   (lambda (buf)
                     (eglot--when-live-buffer buf
                       ;; HACK: In some pathological situations
                       ;; (Emacs's own coding.c, for example),
                       ;; jit-lock is calling `lsp-proxy--update-hints'
                       ;; repeatedly with same sequence of
                       ;; arguments, which leads to
                       ;; `lsp-proxy--update-hints-1' being called with
                       ;; the same region repeatedly.  This happens
                       ;; even if the hint-painting code does
                       ;; nothing else other than widen, narrow,
                       ;; move point then restore these things.
                       ;; Possible Emacs bug, but this fixes it.
                       (unless (equal last-region region)
                         (lsp-proxy--update-hints-1 (max (car region) (point-min))
                                                    (min (cdr region) (point-max)))
                         (setq last-region region))
                       (setq region (cons nil nil)
                             timer nil)))
                   (current-buffer)))))

;; see eglot--update-hints-1
(defun lsp-proxy--update-hints-1 (from to)
  "Do most work for `lsp-proxy--update-hints', including LSP request."
  (let* ((buf (current-buffer))
         (paint-hint
          (lambda (hint)
            (cl-block nil
              (let* ((position (plist-get hint :position))
                     (padding-left (plist-get hint :paddingLeft))
                     (padding-right (plist-get hint :paddingRight))
                     (kind (plist-get hint :kind))
                     (label (plist-get hint :label)))
                (goto-char (eglot--lsp-position-to-point position))
                (when (or (> (point) to) (< (point) from)) (cl-return))
                (let* ((left-pad (and padding-left
                                      (not (eq padding-left :json-false))
                                      (not (memq (char-before) '(32 9))) " "))
                       (right-pad (and padding-right
                                       (not (eq padding-right :json-false))
                                       (not (memq (char-after) '(32 9))) " "))
                       (peg-after-p (eql kind 1)))
                  (cl-labels
                      ((make-ov ()
                         (if peg-after-p
                             (make-overlay (point) (1+ (point)) nil t)
                           (make-overlay (1- (point)) (point) nil nil nil)))
                       (do-it (label lpad rpad i n)
                         (let* ((firstp (zerop i))
                                (tweak-cursor-p (and firstp peg-after-p))
                                (ov (make-ov))
                                (text (concat lpad label rpad)))
                           (when tweak-cursor-p (put-text-property 0 1 'cursor 1 text))
                           (overlay-put ov (if peg-after-p 'before-string 'after-string)
                                        (propertize
                                         text
                                         'face (pcase kind
                                                 (1 'lsp-proxy-type-hint-face)
                                                 (2 'lsp-proxy-parameter-hint-face)
                                                 (_ 'lsp-proxy-inlay-hint-face))))
                           (overlay-put ov 'priority (if peg-after-p i (- n i)))
                           (overlay-put ov 'lsp-proxy--inlay-hint t)
                           (overlay-put ov 'evaporate t)
                           (overlay-put ov 'lsp-proxy--overlay t))))
                    (if (stringp label) (do-it label left-pad right-pad 0 1)
                      (cl-loop
                       for i from 0 for ldetail across label
                       do (lsp-proxy--dbind (:value value) ldetail
                            (do-it value
                                   (and (zerop i) left-pad)
                                   (and (= i (1- (length label))) right-pad)
                                   i (length label))))))))))))
    (lsp-proxy--async-request
     'textDocument/inlayHint
     (lsp-proxy--request-or-notify-params
      (list :textDocument (eglot--TextDocumentIdentifier)
            :range (list :start (eglot--pos-to-lsp-position from)
                         :end (eglot--pos-to-lsp-position to))))
     :success-fn (lambda (hints)
                   (eglot--when-live-buffer buf
                     (eglot--widening
                      ;; Overlays ending right at FROM with an
                      ;; `after-string' property logically belong to
                      ;; the (FROM TO) region.  Likewise, such
                      ;; overlays ending at TO don't logically belong
                      ;; to it.
                      (dolist (o (overlays-in (1- from) to))
                        (when (and (overlay-get o 'lsp-proxy--inlay-hint)
                                   (cond ((eq (overlay-end o) from)
                                          (overlay-get o 'after-string))
                                         ((eq (overlay-end o) to)
                                          (overlay-get o 'before-string))
                                         (t)))
                          (delete-overlay o)))
                      (mapc paint-hint hints))))
     :deferred 'lsp-proxy--update-hints-1)))

(defun lsp-proxy-activate-inlay-hints-mode ()
  "Activate `lsp-proxy-inlay-hints-mode` for the current buffer
if `lsp-proxy-inlay-hints-mode-config` allows it."
  (when (and lsp-proxy--support-inlay-hints
             (boundp 'lsp-proxy-inlay-hints-mode-config)
             (or (eq lsp-proxy-inlay-hints-mode-config t)
                 (and (listp lsp-proxy-inlay-hints-mode-config)
                      (member major-mode lsp-proxy-inlay-hints-mode-config))))
    (lsp-proxy-inlay-hints-mode 1)))

(define-minor-mode lsp-proxy-inlay-hints-mode
  "Mode for displaying inlay hint."
  :lighter nil
  (cond
   (lsp-proxy-inlay-hints-mode
    (if lsp-proxy--support-inlay-hints
        (jit-lock-register #'lsp-proxy--update-inlay-hints 'contextual)
      (lsp-proxy-inlay-hints-mode -1)))
   (t
    (jit-lock-unregister #'lsp-proxy--update-inlay-hints)
    (remove-overlays nil nil 'lsp-proxy--inlay-hint t))))

(provide 'lsp-proxy-inlay-hints)
;;; lsp-proxy-inlay-hints.el ends here
