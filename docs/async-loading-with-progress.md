# LSP-Proxy 异步大文件加载与进度提示方案

## 方案概述

基于方案四（异步后台加载）的增强版本，添加：
1. Mode-line 进度提示
2. 加载期间的请求队列管理
3. Rust 服务端请求缓冲机制
4. 完整的状态管理系统

## 实现方案

### 1. Emacs 端实现

#### 1.1 大文件加载状态管理

```elisp
;; 大文件加载状态变量
(defvar-local lsp-proxy--large-file-loading-state nil
  "Loading state for large files.
Possible values:
- nil: Not loading
- 'pending: Waiting to start loading
- 'loading: Currently loading
- 'completed: Loading completed
- 'failed: Loading failed")

(defvar-local lsp-proxy--large-file-loading-progress 0
  "Loading progress percentage (0-100).")

(defvar-local lsp-proxy--large-file-start-time nil
  "Start time of large file loading.")

(defvar-local lsp-proxy--large-file-total-size 0
  "Total size of large file being loaded.")

(defvar-local lsp-proxy--large-file-loaded-size 0
  "Already loaded size of large file.")

;; 全局加载文件跟踪
(defvar lsp-proxy--loading-files (make-hash-table :test 'equal)
  "Hash table tracking files currently being loaded.
Key: file path, Value: buffer")

;; 请求队列管理
(defvar-local lsp-proxy--pending-requests nil
  "Queue of pending requests while large file is loading.")

(defvar lsp-proxy--request-queue-enabled t
  "Whether to queue requests during large file loading.")
```

#### 1.2 配置选项

```elisp
;; 用户配置选项
(defcustom lsp-proxy-large-file-threshold (* 10 1024 1024)  ; 10MB
  "File size threshold for large file optimization in bytes."
  :type 'integer
  :group 'lsp-proxy)

(defcustom lsp-proxy-large-file-async-loading t
  "Enable async loading for large files."
  :type 'boolean
  :group 'lsp-proxy)

(defcustom lsp-proxy-large-file-show-progress t
  "Show loading progress in mode-line for large files."
  :type 'boolean
  :group 'lsp-proxy)

(defcustom lsp-proxy-large-file-queue-requests t
  "Queue LSP requests while large file is loading."
  :type 'boolean
  :group 'lsp-proxy)

(defcustom lsp-proxy-large-file-loading-timeout 30
  "Timeout for large file loading in seconds."
  :type 'integer
  :group 'lsp-proxy)

(defcustom lsp-proxy-large-file-chunk-size (* 2 1024 1024)  ; 2MB
  "Chunk size for async loading in bytes."
  :type 'integer
  :group 'lsp-proxy)
```

#### 1.3 Mode-line 进度显示

```elisp
;; Mode-line 进度显示
(defun lsp-proxy--large-file-mode-line-status ()
  "Return mode-line status for large file loading."
  (when (and lsp-proxy-large-file-show-progress
             lsp-proxy--large-file-loading-state
             (not (eq lsp-proxy--large-file-loading-state 'completed)))
    (pcase lsp-proxy--large-file-loading-state
      ('pending 
       (propertize " [LSP: Preparing...]" 'face 'warning))
      ('loading 
       (let* ((progress lsp-proxy--large-file-loading-progress)
              (size-info (if (> lsp-proxy--large-file-total-size 0)
                            (format " %s/%s"
                                   (lsp-proxy--format-file-size lsp-proxy--large-file-loaded-size)
                                   (lsp-proxy--format-file-size lsp-proxy--large-file-total-size))
                          ""))
              (time-info (if lsp-proxy--large-file-start-time
                            (format " %.1fs"
                                   (float-time (time-subtract (current-time) 
                                                             lsp-proxy--large-file-start-time)))
                          "")))
         (propertize (format " [LSP: Loading %d%%%s%s]" 
                            progress size-info time-info)
                    'face 'compilation-info)))
      ('failed 
       (propertize " [LSP: Load Failed]" 'face 'error))
      (_ ""))))

(defun lsp-proxy--format-file-size (bytes)
  "Format file size in human readable format."
  (cond
   ((< bytes 1024) (format "%dB" bytes))
   ((< bytes (* 1024 1024)) (format "%.1fKB" (/ bytes 1024.0)))
   ((< bytes (* 1024 1024 1024)) (format "%.1fMB" (/ bytes 1024.0 1024.0)))
   (t (format "%.1fGB" (/ bytes 1024.0 1024.0 1024.0)))))

;; 添加到 mode-line
(defun lsp-proxy--setup-mode-line ()
  "Setup mode-line display for large file loading."
  (unless (memq '(:eval (lsp-proxy--large-file-mode-line-status)) mode-line-format)
    (setq mode-line-format 
          (append mode-line-format 
                  '((:eval (lsp-proxy--large-file-mode-line-status)))))))

;; 在 lsp-proxy-mode 启用时调用
(add-hook 'lsp-proxy-mode-hook #'lsp-proxy--setup-mode-line)
```

#### 1.4 异步加载实现

```elisp
;; 异步分块加载实现
(defun lsp-proxy--async-load-large-file (buffer)
  "Asynchronously load large file content in chunks."
  (let ((file-path (buffer-file-name buffer)))
    (when (and file-path 
               (not (gethash file-path lsp-proxy--loading-files))
               (> (nth 7 (file-attributes file-path)) lsp-proxy-large-file-threshold))
      
      ;; 设置加载状态
      (puthash file-path buffer lsp-proxy--loading-files)
      (with-current-buffer buffer
        (setq lsp-proxy--large-file-loading-state 'pending
              lsp-proxy--large-file-start-time (current-time)
              lsp-proxy--large-file-total-size (nth 7 (file-attributes file-path))
              lsp-proxy--large-file-loaded-size 0
              lsp-proxy--large-file-loading-progress 0
              lsp-proxy--pending-requests nil))
      
      ;; 通知服务端开始加载
      (lsp-proxy--notify 'emacs/largeFileLoadStart
                         (list :uri (concat "file://" file-path)
                               :totalSize (nth 7 (file-attributes file-path))
                               :chunkSize lsp-proxy-large-file-chunk-size))
      
      ;; 启动分块加载
      (lsp-proxy--start-chunked-loading buffer file-path))))

(defun lsp-proxy--start-chunked-loading (buffer file-path)
  "Start chunked loading process."
  (let ((chunk-size lsp-proxy-large-file-chunk-size)
        (total-size (nth 7 (file-attributes file-path)))
        (loaded-size 0)
        (chunk-index 0))
    
    (with-current-buffer buffer
      (setq lsp-proxy--large-file-loading-state 'loading))
    
    ;; 使用定时器模拟分块加载
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
           (new-loaded-size (+ loaded-size (length chunk-data)))
           (progress (round (* 100.0 (/ (float new-loaded-size) total-size))))
           (is-last-chunk (>= new-loaded-size total-size)))
      
      ;; 更新进度
      (with-current-buffer buffer
        (setq lsp-proxy--large-file-loaded-size new-loaded-size
              lsp-proxy--large-file-loading-progress progress))
      
      ;; 发送分块数据
      (lsp-proxy--notify 'emacs/largeFileChunk
                         (list :uri (concat "file://" file-path)
                               :chunkIndex chunk-index
                               :chunkData chunk-data
                               :startPos start-pos
                               :endPos end-pos
                               :isLastChunk is-last-chunk
                               :progress progress))
      
      (if is-last-chunk
          ;; 加载完成
          (lsp-proxy--complete-large-file-loading buffer file-path)
        ;; 继续加载下一块
        (run-with-idle-timer 
         0.01 nil
         #'lsp-proxy--load-next-chunk buffer file-path (1+ chunk-index)
         chunk-size total-size new-loaded-size)))))

(defun lsp-proxy--complete-large-file-loading (buffer file-path)
  "Complete large file loading process."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq lsp-proxy--large-file-loading-state 'completed
            lsp-proxy--large-file-loading-progress 100))
    
    ;; 清理加载状态
    (remhash file-path lsp-proxy--loading-files)
    
    ;; 通知服务端加载完成
    (lsp-proxy--notify 'emacs/largeFileLoadComplete
                       (list :uri (concat "file://" file-path)))
    
    ;; 处理积压的请求
    (lsp-proxy--process-pending-requests buffer)
    
    ;; 显示完成信息
    (let ((elapsed (if lsp-proxy--large-file-start-time
                      (float-time (time-subtract (current-time) 
                                                lsp-proxy--large-file-start-time))
                    0)))
      (lsp-proxy--info "Large file loaded: %s (%.1fs)" 
                       (file-name-nondirectory file-path) elapsed))
    
    ;; 延迟清理 mode-line 显示
    (run-with-timer 2.0 nil
                    (lambda (buf)
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (setq lsp-proxy--large-file-loading-state nil))))
                    buffer)))
```

#### 1.5 请求队列管理

```elisp
;; 请求拦截和队列管理
(defun lsp-proxy--should-queue-request (method)
  "Check if request should be queued during large file loading."
  (and lsp-proxy-large-file-queue-requests
       lsp-proxy--large-file-loading-state
       (memq lsp-proxy--large-file-loading-state '(pending loading))
       ;; 只队列特定类型的请求
       (memq method '(textDocument/completion
                      textDocument/hover
                      textDocument/signatureHelp
                      textDocument/definition
                      textDocument/references
                      textDocument/codeAction))))

(defun lsp-proxy--queue-request (method params success-fn error-fn timeout-fn)
  "Queue a request during large file loading."
  (let ((request-info (list :method method
                           :params params 
                           :success-fn success-fn
                           :error-fn error-fn
                           :timeout-fn timeout-fn
                           :timestamp (current-time))))
    (push request-info lsp-proxy--pending-requests)
    (lsp-proxy--info "Queued %s request (loading in progress)" method)))

(defun lsp-proxy--process-pending-requests (buffer)
  "Process all pending requests after loading completes."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when lsp-proxy--pending-requests
        (lsp-proxy--info "Processing %d pending requests" 
                         (length lsp-proxy--pending-requests))
        
        ;; 按时间顺序处理请求
        (dolist (request (nreverse lsp-proxy--pending-requests))
          (let ((method (plist-get request :method))
                (params (plist-get request :params))
                (success-fn (plist-get request :success-fn))
                (error-fn (plist-get request :error-fn))
                (timeout-fn (plist-get request :timeout-fn)))
            
            ;; 重新发送请求
            (condition-case err
                (lsp-proxy--async-request method params
                                         :success-fn success-fn
                                         :error-fn error-fn
                                         :timeout-fn timeout-fn)
              (error (lsp-proxy--warn "Failed to replay request %s: %s" method err)))))
        
        ;; 清空队列
        (setq lsp-proxy--pending-requests nil)))))

;; 修改原有的请求函数以支持队列
(defmacro lsp-proxy--async-request-with-queue (method params &rest args)
  "Send async request with queue support during large file loading."
  `(if (lsp-proxy--should-queue-request ,method)
       (lsp-proxy--queue-request ,method ,params 
                                ,@(plist-get args :success-fn)
                                ,@(plist-get args :error-fn)  
                                ,@(plist-get args :timeout-fn))
     (lsp-proxy--async-request ,method ,params ,@args)))
```

#### 1.6 修改的 didOpen 函数

```elisp
;; 修改后的 didOpen 实现
(defun lsp-proxy--on-doc-open-with-async-loading ()
  "Open document with async loading for large files."
  (setq lsp-proxy--recent-changes nil
        eglot--versioned-identifier 0)
  (when buffer-file-name
    (when (not (f-exists? buffer-file-name))
      (save-buffer))
    (add-to-list 'lsp-proxy--opened-buffers (current-buffer))
    (setq lsp-proxy--enable-symbol-highlighting (< (line-number-at-pos (point-max)) 10000))
    
    (let* ((file-size (nth 7 (file-attributes buffer-file-name)))
           (is-large-file (and file-size 
                              (> file-size lsp-proxy-large-file-threshold)
                              lsp-proxy-large-file-async-loading))
           (initial-content (if is-large-file 
                               (lsp-proxy--get-initial-content)
                             (eglot--widening
                              (buffer-substring-no-properties (point-min) (point-max))))))
      
      ;; 立即发送初始内容
      (lsp-proxy--notify 'textDocument/didOpen
                         (list :textDocument (append (eglot--TextDocumentIdentifier)
                                                     (list
                                                      :text initial-content
                                                      :languageId ""
                                                      :version eglot--versioned-identifier
                                                      :isLargeFile is-large-file))))
      
      ;; 对大文件启动异步加载
      (when is-large-file
        (lsp-proxy--async-load-large-file (current-buffer))))))

(defun lsp-proxy--get-initial-content ()
  "Get initial content to send for large files."
  (let ((file-name (file-name-nondirectory buffer-file-name))
        (file-size (nth 7 (file-attributes buffer-file-name))))
    (format ";; Large file: %s\n;; Size: %s\n;; Loading content asynchronously...\n\n"
            file-name
            (lsp-proxy--format-file-size file-size))))

;; 替换原有的 didOpen 函数
(defalias 'lsp-proxy--on-doc-open 'lsp-proxy--on-doc-open-with-async-loading)
```

### 2. Rust 端实现

#### 2.1 请求缓冲系统

```rust
// 在 src/handlers/notification.rs 中添加
use std::collections::{HashMap, VecDeque};
use std::sync::{Arc, Mutex};
use tokio::sync::mpsc;

// 大文件加载状态
#[derive(Debug, Clone)]
pub enum LargeFileLoadState {
    Starting,
    Loading { chunks_received: usize, total_chunks: usize },
    Completed,
    Failed(String),
}

// 缓冲的请求
#[derive(Debug, Clone)]
pub struct BufferedRequest {
    pub method: String,
    pub params: serde_json::Value,
    pub id: Option<lsp_types::NumberOrString>,
    pub timestamp: std::time::Instant,
}

// 大文件管理器
pub struct LargeFileManager {
    pub loading_files: HashMap<lsp_types::Url, LargeFileLoadState>,
    pub buffered_requests: HashMap<lsp_types::Url, VecDeque<BufferedRequest>>,
    pub file_contents: HashMap<lsp_types::Url, String>,
}

impl LargeFileManager {
    pub fn new() -> Self {
        Self {
            loading_files: HashMap::new(),
            buffered_requests: HashMap::new(),
            file_contents: HashMap::new(),
        }
    }
    
    pub fn start_loading(&mut self, uri: lsp_types::Url) {
        self.loading_files.insert(uri.clone(), LargeFileLoadState::Starting);
        self.buffered_requests.insert(uri, VecDeque::new());
        tracing::info!("Started large file loading: {}", uri);
    }
    
    pub fn add_chunk(&mut self, uri: &lsp_types::Url, chunk_data: String, is_last: bool) -> bool {
        if let Some(content) = self.file_contents.get_mut(uri) {
            content.push_str(&chunk_data);
        } else {
            self.file_contents.insert(uri.clone(), chunk_data);
        }
        
        if is_last {
            self.loading_files.insert(uri.clone(), LargeFileLoadState::Completed);
            tracing::info!("Large file loading completed: {}", uri);
            true
        } else {
            false
        }
    }
    
    pub fn is_loading(&self, uri: &lsp_types::Url) -> bool {
        matches!(
            self.loading_files.get(uri),
            Some(LargeFileLoadState::Starting) | Some(LargeFileLoadState::Loading { .. })
        )
    }
    
    pub fn buffer_request(&mut self, uri: &lsp_types::Url, request: BufferedRequest) {
        if let Some(queue) = self.buffered_requests.get_mut(uri) {
            queue.push_back(request);
            tracing::debug!("Buffered request for {}: {} (queue size: {})", 
                          uri, request.method, queue.len());
        }
    }
    
    pub fn get_buffered_requests(&mut self, uri: &lsp_types::Url) -> Option<VecDeque<BufferedRequest>> {
        self.buffered_requests.remove(uri)
    }
    
    pub fn get_content(&self, uri: &lsp_types::Url) -> Option<&String> {
        self.file_contents.get(uri)
    }
}

// 在 Application 结构中添加
pub struct Application {
    // 现有字段...
    pub large_file_manager: Arc<Mutex<LargeFileManager>>,
}
```

#### 2.2 大文件加载协议处理

```rust
// 处理大文件加载开始
pub fn handle_large_file_load_start(
    app: Arc<Mutex<Application>>,
    params: serde_json::Value,
) -> anyhow::Result<()> {
    let uri: lsp_types::Url = serde_json::from_value(params["uri"].clone())?;
    let total_size: usize = serde_json::from_value(params["totalSize"].clone())?;
    let chunk_size: usize = serde_json::from_value(params["chunkSize"].clone())?;
    
    {
        let mut app = app.lock().unwrap();
        app.large_file_manager.lock().unwrap().start_loading(uri.clone());
    }
    
    tracing::info!("Large file load started: {} ({} bytes, {} chunk size)", 
                   uri, total_size, chunk_size);
    Ok(())
}

// 处理大文件分块
pub fn handle_large_file_chunk(
    app: Arc<Mutex<Application>>,
    params: serde_json::Value,
) -> anyhow::Result<()> {
    let uri: lsp_types::Url = serde_json::from_value(params["uri"].clone())?;
    let chunk_index: usize = serde_json::from_value(params["chunkIndex"].clone())?;
    let chunk_data: String = serde_json::from_value(params["chunkData"].clone())?;
    let is_last_chunk: bool = serde_json::from_value(params["isLastChunk"].clone())?;
    let progress: u8 = serde_json::from_value(params["progress"].clone())?;
    
    let is_completed = {
        let mut app = app.lock().unwrap();
        app.large_file_manager.lock().unwrap()
            .add_chunk(&uri, chunk_data, is_last_chunk)
    };
    
    tracing::debug!("Received chunk {} for {}, progress: {}%", 
                   chunk_index, uri, progress);
    
    if is_completed {
        handle_large_file_load_complete(app, uri)?;
    }
    
    Ok(())
}

// 处理大文件加载完成
pub fn handle_large_file_load_complete(
    app: Arc<Mutex<Application>>,
    uri: lsp_types::Url,
) -> anyhow::Result<()> {
    let (content, buffered_requests) = {
        let mut app = app.lock().unwrap();
        let mut manager = app.large_file_manager.lock().unwrap();
        
        let content = manager.get_content(&uri).cloned()
            .ok_or_else(|| anyhow::anyhow!("Large file content not found"))?;
        
        let requests = manager.get_buffered_requests(&uri)
            .unwrap_or_default();
        
        (content, requests)
    };
    
    // 发送完整内容给语言服务器
    {
        let mut app = app.lock().unwrap();
        let document = app.editor.get_or_create_document(&uri, content.clone())?;
        
        // 创建 didChange 参数来更新内容
        let did_change_params = lsp_types::DidChangeTextDocumentParams {
            text_document: lsp_types::VersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: document.version,
            },
            content_changes: vec![lsp_types::TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: content,
            }],
        };
        
        // 发送给所有相关的语言服务器
        let language_servers = app.editor.get_language_servers(&uri)?;
        for ls in language_servers {
            ls.text_document_did_change(did_change_params.clone())
                .map_err(|e| tracing::error!("Failed to send didChange to {}: {}", ls.name(), e))
                .ok();
        }
    }
    
    // 处理缓冲的请求
    if !buffered_requests.is_empty() {
        tracing::info!("Processing {} buffered requests for {}", 
                      buffered_requests.len(), uri);
        
        // 在新的任务中处理缓冲的请求，避免阻塞
        let app_clone = app.clone();
        let uri_clone = uri.clone();
        tokio::spawn(async move {
            for request in buffered_requests {
                if let Err(e) = process_buffered_request(app_clone.clone(), &uri_clone, request).await {
                    tracing::error!("Failed to process buffered request: {}", e);
                }
                // 小延迟避免过载
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
            }
        });
    }
    
    tracing::info!("Large file loading completed and requests processed: {}", uri);
    Ok(())
}

// 处理缓冲的请求
async fn process_buffered_request(
    app: Arc<Mutex<Application>>,
    uri: &lsp_types::Url,
    request: BufferedRequest,
) -> anyhow::Result<()> {
    // 根据请求类型重新路由到相应的处理函数
    match request.method.as_str() {
        "textDocument/completion" => {
            // 处理补全请求
            let params: lsp_types::CompletionParams = serde_json::from_value(request.params)?;
            handle_completion_request(app, params).await
        }
        "textDocument/hover" => {
            // 处理悬停请求
            let params: lsp_types::HoverParams = serde_json::from_value(request.params)?;
            handle_hover_request(app, params).await
        }
        // 添加其他请求类型...
        _ => {
            tracing::warn!("Unknown buffered request type: {}", request.method);
            Ok(())
        }
    }
}
```

#### 2.3 请求拦截和缓冲

```rust
// 修改请求分发函数以支持缓冲
pub fn should_buffer_request(
    app: &Arc<Mutex<Application>>,
    uri: &lsp_types::Url,
    method: &str,
) -> bool {
    let manager = app.lock().unwrap().large_file_manager.clone();
    let manager = manager.lock().unwrap();
    
    manager.is_loading(uri) && matches!(method, 
        "textDocument/completion" |
        "textDocument/hover" |
        "textDocument/signatureHelp" |
        "textDocument/definition" |
        "textDocument/references" |
        "textDocument/codeAction"
    )
}

// 在主请求处理函数中添加缓冲逻辑
pub fn handle_lsp_request(
    app: Arc<Mutex<Application>>,
    method: String,
    params: serde_json::Value,
    id: Option<lsp_types::NumberOrString>,
) -> anyhow::Result<()> {
    // 尝试从参数中提取 URI
    if let Ok(text_doc_params) = extract_text_document_uri(&params) {
        if should_buffer_request(&app, &text_doc_params, &method) {
            // 缓冲请求
            let buffered_request = BufferedRequest {
                method: method.clone(),
                params,
                id,
                timestamp: std::time::Instant::now(),
            };
            
            app.lock().unwrap()
                .large_file_manager.lock().unwrap()
                .buffer_request(&text_doc_params, buffered_request);
            
            tracing::debug!("Buffered {} request for loading file: {}", method, text_doc_params);
            return Ok(());
        }
    }
    
    // 正常处理请求
    match method.as_str() {
        "textDocument/completion" => handle_completion_request(app, serde_json::from_value(params)?).await,
        "textDocument/hover" => handle_hover_request(app, serde_json::from_value(params)?).await,
        // 其他请求处理...
        _ => Ok(())
    }
}

fn extract_text_document_uri(params: &serde_json::Value) -> anyhow::Result<lsp_types::Url> {
    // 从不同类型的参数中提取 textDocument.uri
    if let Some(text_doc) = params.get("textDocument") {
        if let Some(uri) = text_doc.get("uri") {
            return Ok(serde_json::from_value(uri.clone())?);
        }
    }
    
    Err(anyhow::anyhow!("No textDocument.uri found in params"))
}
```

### 3. 用户交互和控制

```elisp
;; 用户控制命令
(defun lsp-proxy-cancel-large-file-loading ()
  "Cancel current large file loading."
  (interactive)
  (when (and lsp-proxy--large-file-loading-state
             (memq lsp-proxy--large-file-loading-state '(pending loading)))
    (setq lsp-proxy--large-file-loading-state 'failed)
    (remhash buffer-file-name lsp-proxy--loading-files)
    (lsp-proxy--notify 'emacs/largeFileLoadCancel
                       (list :uri (concat "file://" buffer-file-name)))
    (lsp-proxy--warn "Large file loading cancelled")))

(defun lsp-proxy-retry-large-file-loading ()
  "Retry loading large file."
  (interactive)
  (when (eq lsp-proxy--large-file-loading-state 'failed)
    (setq lsp-proxy--large-file-loading-state nil
          lsp-proxy--pending-requests nil)
    (remhash buffer-file-name lsp-proxy--loading-files)
    (lsp-proxy--async-load-large-file (current-buffer))))

(defun lsp-proxy-show-loading-status ()
  "Show detailed loading status for current buffer."
  (interactive)
  (if lsp-proxy--large-file-loading-state
      (let ((progress lsp-proxy--large-file-loading-progress)
            (loaded lsp-proxy--large-file-loaded-size)
            (total lsp-proxy--large-file-total-size)
            (pending (length lsp-proxy--pending-requests))
            (elapsed (if lsp-proxy--large-file-start-time
                        (float-time (time-subtract (current-time) 
                                                  lsp-proxy--large-file-start-time))
                      0)))
        (message "LSP Loading: %d%% (%s/%s) %d pending requests, %.1fs elapsed"
                progress
                (lsp-proxy--format-file-size loaded)
                (lsp-proxy--format-file-size total)
                pending
                elapsed))
    (message "No large file loading in progress")))

;; 键绑定
(define-key lsp-proxy-mode-map (kbd "C-c l c") #'lsp-proxy-cancel-large-file-loading)
(define-key lsp-proxy-mode-map (kbd "C-c l r") #'lsp-proxy-retry-large-file-loading)
(define-key lsp-proxy-mode-map (kbd "C-c l s") #'lsp-proxy-show-loading-status)
```

### 4. 错误处理和超时

```elisp
;; 超时处理
(defvar-local lsp-proxy--loading-timeout-timer nil
  "Timer for large file loading timeout.")

(defun lsp-proxy--setup-loading-timeout (buffer)
  "Setup timeout for large file loading."
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

;; 错误恢复
(defun lsp-proxy--handle-loading-error (buffer error-msg)
  "Handle large file loading error."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq lsp-proxy--large-file-loading-state 'failed)
      (remhash buffer-file-name lsp-proxy--loading-files)
      (lsp-proxy--error "Large file loading failed: %s" error-msg)
      
      ;; 提供恢复选项
      (when (y-or-n-p "Large file loading failed. Fallback to traditional method? ")
        (lsp-proxy--fallback-to-traditional-loading buffer)))))

(defun lsp-proxy--fallback-to-traditional-loading (buffer)
  "Fallback to traditional loading method."
  (with-current-buffer buffer
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (lsp-proxy--notify 'textDocument/didChange
                         (list :textDocument (eglot--VersionedTextDocumentIdentifier)
                               :contentChanges (vector (list :text content))))
      (setq lsp-proxy--large-file-loading-state 'completed)
      (lsp-proxy--process-pending-requests buffer)
      (lsp-proxy--info "Fallback loading completed"))))
```

## 使用说明

### 1. 启用异步加载

```elisp
;; 在配置中启用
(setq lsp-proxy-large-file-async-loading t
      lsp-proxy-large-file-show-progress t
      lsp-proxy-large-file-queue-requests t
      lsp-proxy-large-file-threshold (* 5 1024 1024))  ; 5MB 阈值
```

### 2. 用户体验

1. **打开大文件时**: Mode-line 显示 `[LSP: Preparing...]`
2. **加载过程中**: 显示进度 `[LSP: Loading 45% 2.1MB/4.7MB 3.2s]`
3. **请求被队列**: 自动缓存用户的操作请求
4. **加载完成**: 显示 `[LSP: Load Complete]` 并自动处理积压请求
5. **出错时**: 显示错误状态并提供重试选项

### 3. 性能特点

- **内存优化**: 分块加载，避免大量内存占用
- **响应优化**: 立即响应文件打开，后台加载内容
- **请求优化**: 智能队列管理，避免无效请求
- **进度反馈**: 实时显示加载进度和状态

这个方案在保证用户体验的同时，有效解决了大文件加载的性能问题。