# LSP-Proxy 大文件优化方案

## 问题分析

当打开大文件（如20MB）时，lsp-proxy.el 中的 `didOpen` 发送文件内容会导致严重的性能问题。

### 根本原因

1. **Emacs 层面 (lsp-proxy.el)**:
   - `lsp-proxy--on-doc-open` 函数在 827-828 行调用 `(buffer-substring-no-properties (point-min) (point-max))`
   - 这会一次性读取整个文件内容到内存中
   - 大文件（20MB+）的字符串操作在 Emacs 中非常耗时

2. **Rust 层面 (lsp-proxy)**:
   - `handle_did_open_text_document` 会为每个语言服务器克隆文件内容 (`text.clone()`)
   - 内存使用量为 O(文件大小 × 语言服务器数量)
   - 没有针对大文件的专门优化

3. **LSP 协议层面**:
   - 标准 LSP 协议要求在 `textDocument/didOpen` 中发送完整文件内容
   - JSON-RPC 传输层没有流式传输机制

## 优化方案

### 方案一：阈值式延迟加载 (推荐)

**实现思路**：
- 为大文件设置大小阈值（可配置，默认 10MB）
- 超过阈值的文件先发送空内容或截断内容
- 按需延迟发送完整内容

**实现要点**：

```elisp
;; 在 lsp-proxy.el 中添加配置选项
(defcustom lsp-proxy-large-file-threshold (* 10 1024 1024)  ; 10MB
  "File size threshold for large file optimization in bytes."
  :type 'integer
  :group 'lsp-proxy)

(defcustom lsp-proxy-large-file-strategy 'lazy
  "Strategy for handling large files.
Options:
- 'lazy: Send empty content first, load on demand
- 'truncate: Send only first N lines
- 'minimal: Send minimal metadata only"
  :type '(choice (const :tag "Lazy loading" lazy)
                 (const :tag "Truncate content" truncate) 
                 (const :tag "Minimal metadata" minimal))
  :group 'lsp-proxy)

;; 修改 lsp-proxy--on-doc-open 函数
(defun lsp-proxy--on-doc-open ()
  "On doc open with large file optimization."
  (setq lsp-proxy--recent-changes nil
        eglot--versioned-identifier 0)
  (when buffer-file-name
    (when (not (f-exists? buffer-file-name))
      (save-buffer))
    (add-to-list 'lsp-proxy--opened-buffers (current-buffer))
    (setq lsp-proxy--enable-symbol-highlighting (< (line-number-at-pos (point-max)) 10000))
    
    ;; 优化的大文件处理
    (let* ((file-size (nth 7 (file-attributes buffer-file-name)))
           (is-large-file (and file-size (> file-size lsp-proxy-large-file-threshold)))
           (text-content (lsp-proxy--get-optimized-content is-large-file)))
      
      (lsp-proxy--notify 'textDocument/didOpen
                         (list :textDocument (append (eglot--TextDocumentIdentifier)
                                                     (list
                                                      :text text-content
                                                      :languageId ""
                                                      :version eglot--versioned-identifier
                                                      :isLargeFile is-large-file)))))))

(defun lsp-proxy--get-optimized-content (is-large-file)
  "Get optimized content based on file size and strategy."
  (cond
   ((not is-large-file)
    ;; 小文件正常处理
    (eglot--widening (buffer-substring-no-properties (point-min) (point-max))))
   
   ((eq lsp-proxy-large-file-strategy 'lazy)
    ;; 延迟加载：先发送空内容
    "")
   
   ((eq lsp-proxy-large-file-strategy 'truncate)
    ;; 截断：只发送前1000行
    (eglot--widening 
     (save-excursion
       (goto-char (point-min))
       (forward-line 1000)
       (buffer-substring-no-properties (point-min) (point)))))
   
   ((eq lsp-proxy-large-file-strategy 'minimal)
    ;; 最小化：发送文件基本信息
    (format ";; Large file: %s\n;; Size: %d bytes\n" 
            (file-name-nondirectory buffer-file-name)
            (nth 7 (file-attributes buffer-file-name))))
   
   (t "")))

;; 按需加载完整内容的函数
(defun lsp-proxy-load-full-content ()
  "Load full content for large files on demand."
  (interactive)
  (when (and buffer-file-name 
             (> (nth 7 (file-attributes buffer-file-name)) lsp-proxy-large-file-threshold))
    (let ((full-content (eglot--widening 
                         (buffer-substring-no-properties (point-min) (point-max)))))
      (lsp-proxy--notify 'textDocument/didChange
                         (list :textDocument (eglot--VersionedTextDocumentIdentifier)
                               :contentChanges (vector (list :text full-content)))))))
```

**优点**：
- 显著提升大文件打开速度
- 保持与现有代码的兼容性
- 用户可配置策略和阈值

**缺点**：
- 某些 LSP 功能可能在完整内容加载前受限

### 方案二：分块流式传输

**实现思路**：
- 将大文件分成多个块（如 1MB 每块）
- 使用自定义 LSP 扩展协议分块发送
- 语言服务器端重新组装内容

**实现要点**：

```elisp
;; 分块传输实现
(defcustom lsp-proxy-chunk-size (* 1 1024 1024)  ; 1MB per chunk
  "Chunk size for streaming large files."
  :type 'integer
  :group 'lsp-proxy)

(defun lsp-proxy--send-file-in-chunks (file-content)
  "Send large file content in chunks."
  (let* ((content-length (length file-content))
         (chunks (ceiling (/ (float content-length) lsp-proxy-chunk-size)))
         (chunk-id (format "%s-%d" buffer-file-name (current-time))))
    
    ;; 发送分块开始通知
    (lsp-proxy--notify 'emacs/fileChunkStart
                       (list :chunkId chunk-id
                             :totalChunks chunks
                             :fileSize content-length
                             :textDocument (eglot--TextDocumentIdentifier)))
    
    ;; 发送各个分块
    (dotimes (i chunks)
      (let* ((start (* i lsp-proxy-chunk-size))
             (end (min (+ start lsp-proxy-chunk-size) content-length))
             (chunk (substring file-content start end)))
        
        (lsp-proxy--notify 'emacs/fileChunk
                           (list :chunkId chunk-id
                                 :chunkIndex i
                                 :chunkData chunk
                                 :isLastChunk (= i (1- chunks))))))
    
    ;; 发送分块结束通知
    (lsp-proxy--notify 'emacs/fileChunkEnd
                       (list :chunkId chunk-id))))
```

**优点**：
- 减少单次内存使用峰值
- 更好的进度控制
- 可以并行处理其他请求

**缺点**：
- 需要修改 Rust 端协议处理
- 增加了实现复杂度

### 方案三：缓存与增量更新

**实现思路**：
- 为大文件建立本地缓存
- 只发送文件变更增量
- 语言服务器端维护完整内容状态

**实现要点**：

```elisp
;; 文件缓存管理
(defvar lsp-proxy--file-cache (make-hash-table :test 'equal)
  "Cache for large file contents.")

(defun lsp-proxy--get-file-hash (file-path)
  "Get file content hash for change detection."
  (with-temp-buffer
    (insert-file-contents file-path)
    (md5 (current-buffer))))

(defun lsp-proxy--should-use-cache (file-path)
  "Check if file should use cache based on size and modification."
  (let* ((attrs (file-attributes file-path))
         (size (nth 7 attrs))
         (mtime (nth 5 attrs))
         (cache-entry (gethash file-path lsp-proxy--file-cache)))
    
    (and (> size lsp-proxy-large-file-threshold)
         cache-entry
         (equal mtime (plist-get cache-entry :mtime)))))

(defun lsp-proxy--cache-file-content (file-path content)
  "Cache file content with metadata."
  (let ((attrs (file-attributes file-path)))
    (puthash file-path
             (list :content content
                   :mtime (nth 5 attrs)
                   :size (nth 7 attrs)
                   :hash (md5 content))
             lsp-proxy--file-cache)))

;; 增量更新的 didOpen 实现
(defun lsp-proxy--on-doc-open-with-cache ()
  "Open document with caching for large files."
  (if (lsp-proxy--should-use-cache buffer-file-name)
      ;; 使用缓存，只发送增量
      (lsp-proxy--send-incremental-update)
    ;; 正常流程或建立缓存
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (when (> (length content) lsp-proxy-large-file-threshold)
        (lsp-proxy--cache-file-content buffer-file-name content))
      
      (lsp-proxy--notify 'textDocument/didOpen
                         (list :textDocument (append (eglot--TextDocumentIdentifier)
                                                     (list :text content
                                                           :languageId ""
                                                           :version eglot--versioned-identifier)))))))
```

**优点**：
- 避免重复传输相同内容
- 减少网络和内存开销
- 对小变更非常高效

**缺点**：
- 缓存管理复杂
- 需要处理缓存一致性问题

### 方案四：异步后台加载

**实现思路**：
- didOpen 时立即发送空内容或元数据
- 后台异步加载完整内容
- 加载完成后发送 didChange 更新

**实现要点**：

```elisp
;; 异步加载队列
(defvar lsp-proxy--async-load-queue nil
  "Queue for asynchronous large file loading.")

(defvar lsp-proxy--loading-files (make-hash-table :test 'equal)
  "Track files currently being loaded.")

(defun lsp-proxy--async-load-file-content (buffer)
  "Asynchronously load file content for large files."
  (let ((file-path (buffer-file-name buffer)))
    (when (and file-path 
               (not (gethash file-path lsp-proxy--loading-files))
               (> (nth 7 (file-attributes file-path)) lsp-proxy-large-file-threshold))
      
      (puthash file-path t lsp-proxy--loading-files)
      
      ;; 使用 run-with-idle-timer 进行异步加载
      (run-with-idle-timer
       0.1 nil
       (lambda (buf filepath)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let ((content (buffer-substring-no-properties (point-min) (point-max))))
               ;; 发送完整内容
               (lsp-proxy--notify 'textDocument/didChange
                                  (list :textDocument (eglot--VersionedTextDocumentIdentifier)
                                        :contentChanges (vector (list :text content))))
               
               ;; 清理加载状态
               (remhash filepath lsp-proxy--loading-files)
               
               (lsp-proxy--info "Large file loaded: %s" (file-name-nondirectory filepath))))))
       buffer file-path))))

;; 修改的 didOpen 函数
(defun lsp-proxy--on-doc-open-async ()
  "Open document with async loading for large files."
  (let* ((file-size (nth 7 (file-attributes buffer-file-name)))
         (is-large (and file-size (> file-size lsp-proxy-large-file-threshold)))
         (initial-content (if is-large 
                             (format ";; Loading large file: %s..." 
                                     (file-name-nondirectory buffer-file-name))
                           (buffer-substring-no-properties (point-min) (point-max)))))
    
    ;; 立即发送初始内容
    (lsp-proxy--notify 'textDocument/didOpen
                       (list :textDocument (append (eglot--TextDocumentIdentifier)
                                                   (list :text initial-content
                                                         :languageId ""
                                                         :version eglot--versioned-identifier))))
    
    ;; 对大文件启动异步加载
    (when is-large
      (lsp-proxy--async-load-file-content (current-buffer)))))
```

**优点**：
- 立即响应，用户体验好
- 不阻塞主线程
- 渐进式加载

**缺点**：
- 存在短暂的功能不完整期
- 需要处理加载状态管理

### 方案五：智能内容采样

**实现思路**：
- 对大文件进行智能采样
- 发送代表性的代码片段而非完整内容
- 基于语法结构优化采样策略

**实现要点**：

```elisp
;; 智能采样配置
(defcustom lsp-proxy-sampling-ratio 0.1
  "Sampling ratio for large files (0.1 = 10%)."
  :type 'float
  :group 'lsp-proxy)

(defcustom lsp-proxy-sampling-strategy 'mixed
  "Sampling strategy for large files.
Options:
- 'uniform: Evenly distributed samples
- 'beginning: Focus on file beginning  
- 'mixed: Combine beginning focus with uniform sampling"
  :type '(choice (const :tag "Uniform sampling" uniform)
                 (const :tag "Beginning focus" beginning)
                 (const :tag "Mixed strategy" mixed))
  :group 'lsp-proxy)

(defun lsp-proxy--sample-file-content (content)
  "Sample large file content intelligently."
  (let* ((lines (split-string content "\n"))
         (total-lines (length lines))
         (sample-lines (max 100 (round (* total-lines lsp-proxy-sampling-ratio)))))
    
    (cond
     ((eq lsp-proxy-sampling-strategy 'uniform)
      ;; 均匀采样
      (let ((step (/ (float total-lines) sample-lines))
            (sampled-lines '())
            (current 0.0))
        (dotimes (i sample-lines)
          (let ((line-idx (round current)))
            (when (< line-idx total-lines)
              (push (nth line-idx lines) sampled-lines))
            (setq current (+ current step))))
        (string-join (nreverse sampled-lines) "\n")))
     
     ((eq lsp-proxy-sampling-strategy 'beginning)
      ;; 前端重点采样
      (let ((beginning-lines (round (* sample-lines 0.7)))
            (remaining-lines (- sample-lines beginning-lines)))
        (concat
         ;; 前70%行数用于文件开头
         (string-join (seq-take lines beginning-lines) "\n")
         "\n\n// ... [truncated] ...\n\n"
         ;; 后30%行数从文件后部采样
         (when (> remaining-lines 0)
           (string-join 
            (seq-take (nthcdr (- total-lines remaining-lines) lines) 
                      remaining-lines) "\n")))))
     
     ((eq lsp-proxy-sampling-strategy 'mixed)
      ;; 混合策略：开头40% + 中间20% + 结尾20% + 均匀20%
      (let* ((begin-count (round (* sample-lines 0.4)))
             (middle-count (round (* sample-lines 0.2)))
             (end-count (round (* sample-lines 0.2)))
             (uniform-count (- sample-lines begin-count middle-count end-count))
             (middle-start (round (* total-lines 0.4)))
             (end-start (- total-lines end-count)))
        
        (concat
         ;; 开头部分
         (string-join (seq-take lines begin-count) "\n")
         "\n\n// ... [middle section] ...\n\n"
         ;; 中间部分  
         (string-join (seq-take (nthcdr middle-start lines) middle-count) "\n")
         "\n\n// ... [end section] ...\n\n"
         ;; 结尾部分
         (string-join (seq-take (nthcdr end-start lines) end-count) "\n")))))))

;; 使用采样的 didOpen 实现
(defun lsp-proxy--on-doc-open-sampled ()
  "Open document with content sampling for large files."
  (let* ((full-content (buffer-substring-no-properties (point-min) (point-max)))
         (file-size (length full-content))
         (is-large (> file-size lsp-proxy-large-file-threshold))
         (content (if is-large 
                     (lsp-proxy--sample-file-content full-content)
                   full-content)))
    
    (lsp-proxy--notify 'textDocument/didOpen
                       (list :textDocument (append (eglot--TextDocumentIdentifier)
                                                   (list :text content
                                                         :languageId ""
                                                         :version eglot--versioned-identifier
                                                         :isSampled is-large))))))
```

**优点**：
- 保留代码结构信息
- 减少传输数据量
- 语言服务器仍能提供部分功能

**缺点**：
- LSP 功能受限
- 采样策略需要针对不同语言优化

## Rust 端配套修改

为了完整支持上述优化方案，Rust 端也需要相应修改：

### 1. 支持大文件标识

```rust
// 在 notification.rs 中修改
pub fn handle_did_open_text_document(
    app: Arc<Mutex<Application>>,
    params: lsp_types::DidOpenTextDocumentParams,
) -> anyhow::Result<()> {
    let mut app = app.lock().unwrap();
    let uri = params.text_document.uri.clone();
    let text = params.text_document.text.clone();
    
    // 检查是否为大文件
    let is_large_file = params.text_document.version
        .and_then(|v| if v < 0 { Some(true) } else { None })
        .unwrap_or(false);
    
    if is_large_file {
        tracing::info!("Handling large file: {}", uri);
        // 大文件特殊处理逻辑
        handle_large_file_open(&mut app, &uri, &text)?;
    } else {
        // 正常文件处理
        handle_normal_file_open(&mut app, &uri, &text)?;
    }
    
    Ok(())
}

fn handle_large_file_open(
    app: &mut Application,
    uri: &lsp_types::Url,
    text: &str,
) -> anyhow::Result<()> {
    // 大文件优化处理逻辑
    // 例如：延迟发送到语言服务器、使用弱引用等
    Ok(())
}
```

### 2. 内存优化

```rust
use std::sync::Arc;

// 使用 Arc 共享内容，避免多次克隆
pub struct SharedContent {
    content: Arc<String>,
    is_large: bool,
}

impl SharedContent {
    fn new(content: String) -> Self {
        let is_large = content.len() > LARGE_FILE_THRESHOLD;
        Self {
            content: Arc::new(content),
            is_large,
        }
    }
    
    fn get_content(&self) -> Arc<String> {
        self.content.clone()
    }
}
```

### 3. 分块协议支持

```rust
// 新增分块协议处理
#[derive(Debug, serde::Deserialize)]
pub struct FileChunkStart {
    pub chunk_id: String,
    pub total_chunks: usize,
    pub file_size: usize,
    pub text_document: lsp_types::TextDocumentIdentifier,
}

#[derive(Debug, serde::Deserialize)]
pub struct FileChunk {
    pub chunk_id: String,
    pub chunk_index: usize,
    pub chunk_data: String,
    pub is_last_chunk: bool,
}

// 分块内容管理器
pub struct ChunkManager {
    chunks: HashMap<String, Vec<Option<String>>>,
}

impl ChunkManager {
    pub fn start_chunked_file(&mut self, params: FileChunkStart) {
        let chunks = vec![None; params.total_chunks];
        self.chunks.insert(params.chunk_id, chunks);
    }
    
    pub fn add_chunk(&mut self, params: FileChunk) -> Option<String> {
        if let Some(chunks) = self.chunks.get_mut(&params.chunk_id) {
            chunks[params.chunk_index] = Some(params.chunk_data);
            
            if params.is_last_chunk && chunks.iter().all(|c| c.is_some()) {
                // 所有分块都收到，重新组装
                let content = chunks.iter()
                    .map(|c| c.as_ref().unwrap().as_str())
                    .collect::<String>();
                self.chunks.remove(&params.chunk_id);
                return Some(content);
            }
        }
        None
    }
}
```

## 推荐实施策略

### 阶段一：快速改进（推荐先实施）
1. **实施方案一（阈值式延迟加载）**
   - 配置文件大小阈值（默认 10MB）
   - 提供多种策略选择
   - 添加手动加载完整内容的命令

### 阶段二：深度优化
2. **结合方案四（异步后台加载）**
   - 改善用户体验
   - 提供加载进度反馈

### 阶段三：高级特性  
3. **可选实施方案三（缓存）或方案五（采样）**
   - 根据用户反馈决定
   - 针对特定使用场景优化

## 配置示例

```elisp
;; 用户配置示例
(setq lsp-proxy-large-file-threshold (* 5 1024 1024))  ; 5MB 阈值
(setq lsp-proxy-large-file-strategy 'lazy)             ; 延迟加载策略

;; 自动加载钩子
(add-hook 'lsp-proxy-mode-hook
          (lambda ()
            (when (> (buffer-size) lsp-proxy-large-file-threshold)
              (message "Large file detected. Use M-x lsp-proxy-load-full-content to load complete content."))))
```

## 性能对比预期

| 文件大小 | 当前实现 | 方案一（lazy） | 方案四（async） | 改善比例 |
|----------|----------|----------------|-----------------|----------|
| 1MB      | ~100ms   | ~20ms          | ~15ms           | 80%+     |
| 10MB     | ~2000ms  | ~50ms          | ~30ms           | 95%+     |
| 50MB     | ~15000ms | ~100ms         | ~50ms           | 99%+     |

## 总结

通过实施这些优化方案，lsp-proxy 可以显著改善大文件处理性能，提升用户体验。建议从阈值式延迟加载开始实施，这个方案实现简单、效果明显，且保持了良好的向后兼容性。

<function_calls>
<invoke name="TodoWrite">
<parameter name="todos">[{"content": "Analyze current didOpen implementation in lsp-proxy.el", "id": "1", "priority": "high", "status": "completed"}, {"content": "Research large file handling patterns in LSP protocol", "id": "2", "priority": "high", "status": "completed"}, {"content": "Examine Rust side file content processing", "id": "3", "priority": "medium", "status": "completed"}, {"content": "Design multiple optimization strategies", "id": "4", "priority": "high", "status": "completed"}, {"content": "Create comprehensive optimization documentation", "id": "5", "priority": "high", "status": "completed"}]