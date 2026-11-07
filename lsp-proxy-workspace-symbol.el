;;; lsp-proxy-workspace-symbol.el --- Workspace symbol navigation -*- lexical-binding: t; -*-

;;; Commentary:
;; Workspace symbols are server-generated names, including STS Spring prefixes.
;; Keep each raw symbol and its server origin until it is selected or resolved.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'xref)
(require 'eglot)
(require 'browse-url)
(require 'lsp-proxy-core)
(require 'lsp-proxy-utils)

(defvar lsp-proxy-workspace-symbol--context nil
  "Document request envelope captured before entering the minibuffer.")

(defun lsp-proxy-workspace-symbol--params (params)
  "Wrap PARAMS using the captured document, or the current document."
  (if lsp-proxy-workspace-symbol--context
      (plist-put (copy-sequence lsp-proxy-workspace-symbol--context) :params params)
    (lsp-proxy--build-params params)))

(defun lsp-proxy-workspace-symbol--search (query)
  "Request workspace symbols matching QUERY in the current document."
  (lsp-proxy--request 'workspace/symbol
                      (lsp-proxy-workspace-symbol--params (list :query query))))

(defun lsp-proxy-workspace-symbol--resolve (item)
  "Resolve ITEM through its original server if it has no range."
  (let* ((symbol (plist-get item :lsp_item))
         (location (plist-get symbol :location)))
    (if (plist-get location :range)
        item
      (lsp-proxy--request
       'workspaceSymbol/resolve
       (plist-put (lsp-proxy-workspace-symbol--params symbol)
                  :context (list :language-server-id
                                 (plist-get item :language_server_id)))))))

(defun lsp-proxy-workspace-symbol--label (item)
  "Return a readable label for ITEM without opening its source."
  (let* ((symbol (plist-get item :lsp_item))
         (location (plist-get symbol :location))
         (start (plist-get (plist-get location :range) :start))
         (container (plist-get symbol :containerName)))
    (format "%s%s · kind:%s · %s%s [%s]"
            (plist-get symbol :name)
            (if (and container (not (equal container "")))
                (format " (%s)" container) "")
            (plist-get symbol :kind)
            (plist-get location :uri)
            (if start (format ":%d" (1+ (plist-get start :line))) "")
            (plist-get item :language_server_name))))

(defun lsp-proxy-workspace-symbol--candidates (items)
  "Build unique completion candidates from ITEMS, preserving same-name entries."
  (let ((index 0))
    (mapcar
     (lambda (item)
       (cons (format "[%d] %s" (cl-incf index)
                     (lsp-proxy-workspace-symbol--label item)) item))
     (seq-into items 'list))))

(defun lsp-proxy-workspace-symbol--jump (item &optional marker-only)
  "Visit ITEM, or return a marker when MARKER-ONLY is non-nil."
  (let* ((initial-uri (plist-get (plist-get (plist-get item :lsp_item) :location) :uri))
         (item (if (member (url-type (url-generic-parse-url initial-uri)) '("http" "https"))
                   item
                 (lsp-proxy-workspace-symbol--resolve item)))
         (raw (plist-get item :lsp_item))
         (uri (plist-get (plist-get raw :location) :uri))
         (scheme (url-type (url-generic-parse-url uri))))
    (if (member scheme '("http" "https"))
        (if marker-only
            (user-error "Use M-x lsp-proxy-workspace-symbol to open endpoint URLs")
          (browse-url uri))
      (unless (member scheme '("file" "jdt" "jar" "jrt"))
        (user-error "Unsupported workspace symbol URI: %s" uri))
      (let* ((location (plist-get raw :location))
             (start (plist-get (plist-get location :range) :start))
             (path (lsp-proxy--uri-to-path (plist-get location :uri))))
        (unless start (user-error "The server did not resolve the symbol's location"))
        (let ((buffer (find-file-noselect path)))
          (if marker-only
              (with-current-buffer buffer
                (save-restriction
                  (widen)
                  (save-excursion
                    (goto-char (eglot--lsp-position-to-point start))
                    (point-marker))))
            (xref-push-marker-stack)
            (pop-to-buffer-same-window buffer)
            (widen)
            (goto-char (eglot--lsp-position-to-point start))))))))

;;;###autoload
(defun lsp-proxy-workspace-symbol (&optional query)
  "Search workspace symbols for QUERY and visit the selected result.
Spring queries include @/, @+, @>, @ and // for running endpoints."
  (interactive)
  (let* ((origin (current-buffer))
         (lsp-proxy-workspace-symbol--context (lsp-proxy--build-params nil))
         (query (or query (read-string "Workspace symbol: " (thing-at-point 'symbol))))
         (items (lsp-proxy-workspace-symbol--search query))
         (candidates (lsp-proxy-workspace-symbol--candidates items)))
    (unless candidates (user-error "No workspace symbols for %s" query))
    (let* ((choice (completing-read "Workspace symbol: " candidates nil t))
           (item (cdr (assoc choice candidates))))
      (unless (buffer-live-p origin) (user-error "The source buffer was closed; search again"))
      (with-current-buffer origin (lsp-proxy-workspace-symbol--jump item)))))

;;;###autoload
(defun lsp-proxy-spring-running-endpoints ()
  "Select an endpoint returned by STS for a running Spring application."
  (interactive)
  (lsp-proxy-workspace-symbol "//"))

(cl-defstruct (lsp-proxy--workspace-symbol-location
               (:constructor lsp-proxy--workspace-symbol-location-create))
  item origin path context)

(cl-defmethod xref-location-marker ((location lsp-proxy--workspace-symbol-location))
  (let ((origin (lsp-proxy--workspace-symbol-location-origin location)))
    (unless (buffer-live-p origin) (user-error "The source buffer was closed; search again"))
    (with-current-buffer origin
      (let ((lsp-proxy-workspace-symbol--context
             (lsp-proxy--workspace-symbol-location-context location)))
        (lsp-proxy-workspace-symbol--jump
         (lsp-proxy--workspace-symbol-location-item location) t)))))

(cl-defmethod xref-location-group ((location lsp-proxy--workspace-symbol-location))
  (lsp-proxy--workspace-symbol-location-path location))

(cl-defmethod xref-location-line ((location lsp-proxy--workspace-symbol-location))
  (let* ((symbol (plist-get (lsp-proxy--workspace-symbol-location-item location) :lsp_item))
         (start (plist-get (plist-get (plist-get symbol :location) :range) :start)))
    (when start (1+ (plist-get start :line)))))

(cl-defmethod xref-backend-apropos ((_backend (eql xref-lsp-proxy)) pattern)
  (let ((origin (current-buffer))
        (lsp-proxy-workspace-symbol--context (lsp-proxy--build-params nil)))
    (delq nil
          (mapcar
           (lambda (item)
             (let* ((symbol (plist-get item :lsp_item))
                    (uri (plist-get (plist-get symbol :location) :uri)))
               (when (member (url-type (url-generic-parse-url uri)) '("file" "jdt" "jar" "jrt"))
                 (xref-make
                  (lsp-proxy-workspace-symbol--label item)
                  (lsp-proxy--workspace-symbol-location-create
                   :item item :origin origin :path uri
                   :context lsp-proxy-workspace-symbol--context)))))
           (seq-into (lsp-proxy-workspace-symbol--search pattern) 'list)))))

(provide 'lsp-proxy-workspace-symbol)
;;; lsp-proxy-workspace-symbol.el ends here
