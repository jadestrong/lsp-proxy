;;; test-mode-cleanup.el --- Proxy mode lifecycle regression -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'lsp-proxy)

(ert-deftest lsp-proxy-test-mode-exit-cleans-codelens ()
  "Disabling the parent mode clears CodeLens overlays, timers and hooks."
  (with-temp-buffer
    (insert "class Example {}\n")
    (setq-local lsp-proxy-mode t
                lsp-proxy-codelens-mode t
                lsp-proxy--support-code-lens t)
    (lsp-proxy-codelens--setup-buffer)
    (let ((overlay (make-overlay (point-min) (point-min)))
          (timer (run-with-timer 3600 nil #'ignore)))
      (unwind-protect
          (progn
            (setq lsp-proxy-codelens--overlays (list overlay)
                  lsp-proxy-codelens--refresh-timer timer
                  lsp-proxy-codelens--backend-cache '((:lsp_item (:data [1]))))
            (overlay-put overlay 'lsp-proxy-codelens t)
            (overlay-put overlay 'after-string "1 reference")
            (cl-letf (((symbol-function 'lsp-proxy--imenu-teardown) #'ignore)
                      ((symbol-function 'lsp-proxy--completion-teardown) #'ignore)
                      ((symbol-function 'lsp-proxy--diagnostics-teardown) #'ignore)
                      ((symbol-function 'lsp-proxy-cancel-large-file-loading) #'ignore)
                      ((symbol-function 'lsp-proxy--on-doc-close) #'ignore))
              (lsp-proxy-mode -1))
            (should-not lsp-proxy-mode)
            (should-not lsp-proxy-codelens-mode)
            (should-not lsp-proxy--support-code-lens)
            (should-not (overlay-buffer overlay))
            (should-not lsp-proxy-codelens--overlays)
            (should-not lsp-proxy-codelens--backend-cache)
            (should-not lsp-proxy-codelens--refresh-timer)
            (should-not (memq timer timer-list))
            (should-not (memq #'lsp-proxy-codelens--on-document-change
                              after-change-functions))
            (should-not (memq #'lsp-proxy-codelens--on-scroll window-scroll-functions)))
        (cancel-timer timer)
        (delete-overlay overlay)))))

;;; test-mode-cleanup.el ends here
