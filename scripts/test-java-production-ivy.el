;;; test-java-production-ivy.el --- Production file Ivy resume regressions -*- lexical-binding: t; -*-
(require 'ivy)
(require 'lsp-proxy-ivy)
(load (expand-file-name "test-java-production-files.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defun lsp-proxy-test-files-ivy--select (&rest _)
  "Simulate selection while exercising Ivy's real state, action and cleanup."
  (setq ivy-text "Widget" ivy-exit 'done)
  (setf (ivy-state-current ivy-last) (car ivy--all-candidates))
  "Widget")

(ert-deftest lsp-proxy-files-ivy-real-session-resumes-action-and-origin ()
  (lsp-proxy-test-files--with-source
    (let ((completing-read-function #'ivy-completing-read)
          (ivy-auto-select-single-candidate nil)
          (ivy-last (make-ivy-state)) (ivy--sessions nil) (ivy-text "")
          (position (point-marker)))
      (cl-letf (((symbol-function 'read-from-minibuffer) #'lsp-proxy-test-files-ivy--select)
                ((symbol-function 'lsp-proxy--completing-read)
                 (lambda (&rest _) (ert-fail "Ivy session fell back to identity action"))))
        (lsp-proxy-java-files--choose (list :files (vector file)) root position)
        (should (equal visits (list file)))
        (should-not (eq (ivy--get-action ivy-last) #'identity))
        (should (eq (ivy-state-caller ivy-last) 'lsp-proxy-java-find-production-file))
        (should (assq 'lsp-proxy-java-find-production-file ivy--sessions))
        (goto-char (point-min))
        (with-temp-buffer (ivy-resume))
        (should (equal visits (list file file)))
        (should (equal markers (list (marker-position position) (marker-position position))))
        (should-not requests)))))

(ert-deftest lsp-proxy-files-ivy-cancelled-session-can-resume ()
  (lsp-proxy-test-files--with-source
    (let ((completing-read-function #'ivy-completing-read)
          (ivy-auto-select-single-candidate nil)
          (ivy-last (make-ivy-state)) (ivy--sessions nil) (ivy-text ""))
      (cl-letf (((symbol-function 'read-from-minibuffer)
                 (lambda (&rest _) (setq ivy-text "Widget") (signal 'quit nil))))
        (lsp-proxy-java-files--choose (list :files (vector file)) root (point-marker)))
      (should-not visits)
      (cl-letf (((symbol-function 'read-from-minibuffer) #'lsp-proxy-test-files-ivy--select))
        (ivy-resume))
      (should (equal visits (list file)))
      (should (= (length markers) 1)))))

(ert-deftest lsp-proxy-files-ivy-resume-rejects-dead-origin ()
  (lsp-proxy-test-files--with-source
    (let ((completing-read-function #'ivy-completing-read)
          (ivy-auto-select-single-candidate nil)
          (ivy-last (make-ivy-state)) (ivy--sessions nil) (ivy-text "")
          (origin (generate-new-buffer " *production-ivy-origin*")))
      (unwind-protect
          (cl-letf (((symbol-function 'read-from-minibuffer) #'lsp-proxy-test-files-ivy--select))
            (with-current-buffer origin
              (insert "origin")
              (lsp-proxy-java-files--choose (list :files (vector file)) root (point-marker)))
            (let ((action (ivy--get-action ivy-last)))
              (kill-buffer origin)
              (should-error (funcall action "Widget.java") :type 'user-error))
            (should (= (length visits) 1)))
        (when (buffer-live-p origin) (kill-buffer origin))))))

(ert-deftest lsp-proxy-files-ivy-adapter-respects-other-frontends ()
  (lsp-proxy-test-files--with-source
    (let ((completing-read-function #'completing-read-default))
      (cl-letf (((symbol-function 'ivy-read)
                 (lambda (&rest _) (ert-fail "Forced Ivy on a different frontend"))))
        (lsp-proxy-java-files--choose (list :files (vector file)) root (point-marker)))
      (should visits)
      (should (eq (completion-metadata-get metadata 'category) 'file)))))
