;;; lsp-proxy-large-file.el --- Large file handling for lsp-proxy -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 JadeStrong

;; Author: JadeStrong <jadestrong@163.com>
;; Keywords: tools, languages

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Asynchronous large file loading support for lsp-proxy

;;; Code:

(require 'cl-lib)
(require 'lsp-proxy-utils)

;;; Configuration

(defcustom lsp-proxy-large-file-threshold (* 10 1024 1024)  ; 10MB
  "File size threshold for large file handling."
  :type 'integer
  :group 'lsp-proxy)

(defcustom lsp-proxy-large-file-loading-timeout 30
  "Timeout in seconds for large file loading."
  :type 'integer
  :group 'lsp-proxy)

(defcustom lsp-proxy-large-file-chunk-size (* 1 1024 1024)  ; 1MB
  "Chunk size for loading large files."
  :type 'integer
  :group 'lsp-proxy)

;;; External functions
(declare-function lsp-proxy--notify "lsp-proxy-core")

;;; Variables

(defvar lsp-proxy--loading-files (make-hash-table :test 'equal)
  "Hash table tracking files currently being loaded.")

(defvar-local lsp-proxy--is-large-file nil
  "Whether this buffer contains a large file.")

(defvar-local lsp-proxy--large-file-loading-state nil
  "State of large file loading: pending, loading, completed, failed.")

(defvar-local lsp-proxy--loading-timeout-timer nil
  "Timer for loading timeout.")

(defvar-local lsp-proxy--large-file-start-time nil
  "Start time of large file loading.")

(defvar-local lsp-proxy--large-file-total-size nil
  "Total size of the large file being loaded.")

(defvar-local lsp-proxy--large-file-loaded-size nil
  "Size already loaded.")

(defvar-local lsp-proxy--large-file-loading-progress nil
  "Loading progress percentage.")

;;; Utility functions

(defun lsp-proxy--large-file-p (file-path)
  "Check if file is considered large."
  (and file-path
       (file-exists-p file-path)
       (let ((size (nth 7 (file-attributes file-path))))
         (and size (> size lsp-proxy-large-file-threshold)))))

;;; Large file loading cancellation

(defun lsp-proxy-cancel-large-file-loading ()
  "Cancel current large file loading."
  (interactive)
  (remhash buffer-file-name lsp-proxy--loading-files)
  (when (and lsp-proxy--large-file-loading-state
             (memq lsp-proxy--large-file-loading-state '(pending loading)))
    (setq lsp-proxy--large-file-loading-state 'failed)
    (when lsp-proxy--loading-timeout-timer
      (cancel-timer lsp-proxy--loading-timeout-timer)
      (setq lsp-proxy--loading-timeout-timer nil))
    (remhash buffer-file-name lsp-proxy--loading-files)
    (lsp-proxy--notify 'emacs/largeFileLoadCancel
                       (list :uri (concat "file://" buffer-file-name)))
    (lsp-proxy--warn "Large file loading cancelled")))

;;; Timeout handling

(defun lsp-proxy--setup-loading-timeout (buffer)
  "Setup timeout for large BUFFER file loading."
  (when lsp-proxy-large-file-loading-timeout
    (with-current-buffer buffer
      (when lsp-proxy--loading-timeout-timer
        (cancel-timer lsp-proxy--loading-timeout-timer))

      (setq lsp-proxy--loading-timeout-timer
            (run-with-timer
             lsp-proxy-large-file-loading-timeout nil
             (lambda (buf)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (memq lsp-proxy--large-file-loading-state '(pending loading))
                     (setq lsp-proxy--large-file-loading-state 'failed)
                     (lsp-proxy--error "Large file loading timeout after %ds"
                                      lsp-proxy-large-file-loading-timeout)
                     (lsp-proxy-cancel-large-file-loading)))))
             buffer)))))

;;; Main loading functions

(defun lsp-proxy--async-load-large-file (buffer)
  "Asynchronously load large file BUFFER content in chunks."
  (let ((file-path (buffer-file-name buffer)))
    (when (and file-path
               (not (gethash file-path lsp-proxy--loading-files))
               (lsp-proxy--large-file-p file-path))

      (message "[LSP-PROXY-CHUNK] 🚀 Starting large file loading: %s" (file-name-nondirectory file-path))
      
      (puthash file-path buffer lsp-proxy--loading-files)
      (with-current-buffer buffer
        (setq lsp-proxy--large-file-loading-state 'pending
              lsp-proxy--large-file-start-time (current-time)
              lsp-proxy--large-file-total-size (nth 7 (file-attributes file-path))
              lsp-proxy--large-file-loaded-size 0
              lsp-proxy--large-file-loading-progress 0))

      (lsp-proxy--setup-loading-timeout buffer)

      (lsp-proxy--notify 'emacs/largeFileLoadStart
                         (list :uri (concat "file://" file-path)
                               :totalSize (nth 7 (file-attributes file-path))
                               :chunkSize lsp-proxy-large-file-chunk-size))

      (lsp-proxy--start-chunked-loading buffer file-path))))

(defun lsp-proxy--start-chunked-loading (buffer file-path)
  "Start chunked loading process."
  (let ((chunk-size lsp-proxy-large-file-chunk-size)
        (total-size (nth 7 (file-attributes file-path)))
        (loaded-size 0)
        (chunk-index 0))

    (with-current-buffer buffer
      (setq lsp-proxy--large-file-loading-state 'loading))

    (lsp-proxy--load-next-chunk buffer file-path chunk-index
                               chunk-size total-size loaded-size)))

(defun lsp-proxy--load-next-chunk (buffer file-path chunk-index chunk-size total-size loaded-size)
  "Load next chunk of file content."
  (when (and (buffer-live-p buffer)
             (< loaded-size total-size)
             (eq (with-current-buffer buffer lsp-proxy--large-file-loading-state) 'loading))
    (let* ((start-pos loaded-size)
           (end-pos (min (+ loaded-size chunk-size) total-size))
           (chunk-data (with-temp-buffer
                        (insert-file-contents file-path nil start-pos end-pos)
                        (buffer-string)))
           (new-loaded-size end-pos)
           (progress (round (* 100.0 (/ (float new-loaded-size) (float total-size)))))
           (is-last-chunk (>= new-loaded-size total-size)))

      (message "[LSP-PROXY-CHUNK] Processing: start=%d end=%d new-loaded=%d is-last=%s"
               start-pos end-pos new-loaded-size is-last-chunk)

      (with-current-buffer buffer
        (setq lsp-proxy--large-file-loaded-size new-loaded-size
              lsp-proxy--large-file-loading-progress progress))

      (lsp-proxy--notify 'emacs/largeFileChunk
                         (list :uri (concat "file://" file-path)
                               :chunkIndex chunk-index
                               :chunkData chunk-data
                               :startPos start-pos
                               :endPos end-pos
                               :isLastChunk is-last-chunk
                               :progress progress))

      (if is-last-chunk
          (lsp-proxy--complete-large-file-loading buffer file-path)
        (run-with-idle-timer
         1 nil
         #'lsp-proxy--load-next-chunk buffer file-path (1+ chunk-index)
         chunk-size total-size new-loaded-size)))))

(defun lsp-proxy--complete-large-file-loading (buffer file-path)
  "Complete large file loading process."
  (message "[LSP-PROXY-CHUNK] 🎉 Completing large file loading for: %s" (file-name-nondirectory file-path))
  
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq lsp-proxy--large-file-loading-state 'completed
            lsp-proxy--large-file-loading-progress 100)

      (when lsp-proxy--loading-timeout-timer
        (cancel-timer lsp-proxy--loading-timeout-timer)
        (setq lsp-proxy--loading-timeout-timer nil)))

    (remhash file-path lsp-proxy--loading-files)

    (let ((elapsed (if (with-current-buffer buffer lsp-proxy--large-file-start-time)
                      (float-time (time-subtract (current-time)
                                                (with-current-buffer buffer lsp-proxy--large-file-start-time)))
                    0)))
      (lsp-proxy--info "Large file loaded: %s (%.1fs)"
                       (file-name-nondirectory file-path) elapsed))

    (run-with-timer 2.0 nil
                    (lambda (buf)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (setq lsp-proxy--large-file-loading-state nil))))
                    buffer)))

;;; Integration with file opening

(defun lsp-proxy--setup-large-file-handling (buffer)
  "Setup large file handling for BUFFER if needed."
  (with-current-buffer buffer
    (when buffer-file-name
      (setq-local lsp-proxy--is-large-file (lsp-proxy--large-file-p buffer-file-name)))))

(provide 'lsp-proxy-large-file)
;;; lsp-proxy-large-file.el ends here
