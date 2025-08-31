# 服务器端文件读取方案分析

## 方案概述

在 `textDocument/didOpen` 时发送空内容或最小元数据，让语言服务器端根据文件路径自己读取文件内容。

## 实现方式

### Emacs 端修改

```elisp
(defun lsp-proxy--on-doc-open-server-read ()
  "Let server read file content for large files."
  (setq lsp-proxy--recent-changes nil
        eglot--versioned-identifier 0)
  (when buffer-file-name
    (when (not (f-exists? buffer-file-name))
      (save-buffer))
    (add-to-list 'lsp-proxy--opened-buffers (current-buffer))
    (setq lsp-proxy--enable-symbol-highlighting (< (line-number-at-pos (point-max)) 10000))
    
    (let* ((file-size (nth 7 (file-attributes buffer-file-name)))
           (is-large-file (and file-size (> file-size lsp-proxy-large-file-threshold)))
           (text-content (if is-large-file 
                            "" ; 大文件发送空内容
                          (eglot--widening 
                           (buffer-substring-no-properties (point-min) (point-max))))))
      
      (lsp-proxy--notify 'textDocument/didOpen
                         (list :textDocument (append (eglot--TextDocumentIdentifier)
                                                     (list
                                                      :text text-content
                                                      :languageId ""
                                                      :version eglot--versioned-identifier
                                                      :shouldServerRead is-large-file)))))))
```

### Rust 端实现

```rust
// 在 notification.rs 中修改
use std::fs;
use std::path::Path;

pub fn handle_did_open_text_document(
    app: Arc<Mutex<Application>>,
    params: lsp_types::DidOpenTextDocumentParams,
) -> anyhow::Result<()> {
    let mut app = app.lock().unwrap();
    let uri = params.text_document.uri.clone();
    let mut text = params.text_document.text.clone();
    
    // 检查是否需要服务器端读取文件
    let should_server_read = params.text_document.version
        .map(|v| v < 0) // 使用负版本号作为标识
        .unwrap_or(false);
    
    if should_server_read && text.is_empty() {
        // 服务器端读取文件内容
        if let Ok(file_path) = uri.to_file_path() {
            match fs::read_to_string(&file_path) {
                Ok(content) => {
                    text = content;
                    tracing::info!("Server read file: {} ({} bytes)", 
                                 file_path.display(), text.len());
                }
                Err(e) => {
                    tracing::error!("Failed to read file {}: {}", 
                                  file_path.display(), e);
                    // 发送错误通知给客户端
                    return Err(anyhow::anyhow!("Failed to read file: {}", e));
                }
            }
        }
    }
    
    // 继续正常的处理流程
    let document = app.editor.get_or_create_document(&uri, text.clone())?;
    
    // 分发给各个语言服务器
    let language_servers = app.editor.get_language_servers(&uri)?;
    for ls in language_servers {
        let mut did_open_params = params.clone();
        did_open_params.text_document.text = text.clone(); // 使用读取的内容
        
        ls.text_document_did_open(did_open_params.clone())
            .map_err(|e| tracing::error!("Failed to send didOpen to {}: {}", ls.name(), e))
            .ok();
    }
    
    Ok(())
}
```

## 优势分析

### 1. 性能优势
- **网络传输**: 几乎零网络开销，只传输文件路径
- **内存使用**: Emacs 端内存使用显著减少
- **启动速度**: 文件打开速度极快

### 2. 架构优势
- **实现简单**: 修改量最小，逻辑清晰
- **兼容性好**: 对小文件保持原有行为
- **扩展性强**: 可以结合其他优化策略

### 3. 用户体验
- **即时响应**: 大文件打开不再有明显延迟
- **透明操作**: 用户无感知的优化

## 潜在问题和挑战

### 1. 文件一致性问题

**问题**: Emacs 缓冲区内容与磁盘文件内容可能不一致

```elisp
;; 例如：用户修改了文件但未保存
(insert "new content")  ; 缓冲区有新内容
;; 此时如果触发 didOpen，服务器读取的是磁盘上的旧内容
```

**解决方案**:
```elisp
(defun lsp-proxy--ensure-file-sync ()
  "Ensure buffer content is synced with file before server read."
  (when (and buffer-file-name 
             (buffer-modified-p)
             (> (buffer-size) lsp-proxy-large-file-threshold))
    (if (y-or-n-p "File has unsaved changes. Save before opening in LSP? ")
        (save-buffer)
      ;; 用户选择不保存，回退到发送缓冲区内容
      (throw 'use-buffer-content t))))

(defun lsp-proxy--on-doc-open-with-sync-check ()
  "Open document with file sync check."
  (catch 'use-buffer-content
    (lsp-proxy--ensure-file-sync)
    (lsp-proxy--on-doc-open-server-read))
  ;; 如果抛出异常，使用传统方式
  (lsp-proxy--on-doc-open))
```

### 2. 文件访问权限问题

**问题**: 服务器进程可能没有文件读取权限

```rust
// 权限检查和优雅降级
fn try_server_read_file(file_path: &Path) -> Option<String> {
    // 检查文件是否存在和可读
    if !file_path.exists() {
        tracing::warn!("File does not exist: {}", file_path.display());
        return None;
    }
    
    if !file_path.is_file() {
        tracing::warn!("Path is not a file: {}", file_path.display());
        return None;
    }
    
    // 尝试读取文件
    match fs::read_to_string(file_path) {
        Ok(content) => Some(content),
        Err(e) => {
            tracing::error!("Permission denied or IO error reading {}: {}", 
                          file_path.display(), e);
            None
        }
    }
}

// 在主处理函数中使用
if should_server_read && text.is_empty() {
    if let Ok(file_path) = uri.to_file_path() {
        if let Some(content) = try_server_read_file(&file_path) {
            text = content;
        } else {
            // 读取失败，请求客户端发送内容
            return request_client_content(&uri);
        }
    }
}
```

### 3. 远程文件和特殊协议问题

**问题**: TRAMP、Docker、SSH 等远程文件访问

```elisp
;; 检测远程文件
(defun lsp-proxy--is-remote-file (file-path)
  "Check if file is remote or uses special protocol."
  (or (file-remote-p file-path)
      (string-match-p "^/docker:" file-path)
      (string-match-p "^/ssh:" file-path)
      (string-match-p "^/sudo:" file-path)))

(defun lsp-proxy--can-server-read-file (file-path)
  "Check if server can directly read the file."
  (and file-path
       (not (lsp-proxy--is-remote-file file-path))
       (file-exists-p file-path)
       (file-readable-p file-path)))

;; 在 didOpen 中使用
(let* ((can-server-read (lsp-proxy--can-server-read-file buffer-file-name))
       (is-large-file (and file-size (> file-size lsp-proxy-large-file-threshold)))
       (use-server-read (and can-server-read is-large-file))
       (text-content (if use-server-read
                        ""
                      (eglot--widening 
                       (buffer-substring-no-properties (point-min) (point-max))))))
  ;; ... 继续处理
  )
```

### 4. 文件编码问题

**问题**: 服务器端读取的编码可能与 Emacs 不一致

```rust
use encoding_rs::{Encoding, UTF_8};

fn read_file_with_encoding_detection(file_path: &Path) -> anyhow::Result<String> {
    let bytes = fs::read(file_path)?;
    
    // 尝试检测编码
    let (content, encoding_used, had_errors) = UTF_8.decode(&bytes);
    
    if had_errors {
        tracing::warn!("Encoding issues detected in file: {}", file_path.display());
        
        // 尝试其他常见编码
        for encoding in &[encoding_rs::GBK, encoding_rs::SHIFT_JIS, encoding_rs::ISO_8859_1] {
            let (decoded, _, errors) = encoding.decode(&bytes);
            if !errors {
                tracing::info!("Successfully decoded with {:?}", encoding.name());
                return Ok(decoded.into_owned());
            }
        }
    }
    
    Ok(content.into_owned())
}
```

### 5. 性能考虑

**问题**: 服务器端文件读取的性能影响

```rust
use std::time::Instant;
use tokio::fs as async_fs;

// 异步读取大文件
async fn read_large_file_async(file_path: &Path) -> anyhow::Result<String> {
    let start = Instant::now();
    let content = async_fs::read_to_string(file_path).await?;
    let duration = start.elapsed();
    
    tracing::info!("Read file {} ({} bytes) in {:?}", 
                   file_path.display(), content.len(), duration);
    
    // 对特别大的文件给出警告
    if content.len() > 50 * 1024 * 1024 {  // 50MB
        tracing::warn!("Very large file loaded: {} bytes", content.len());
    }
    
    Ok(content)
}

// 带超时的文件读取
use tokio::time::{timeout, Duration};

async fn read_file_with_timeout(file_path: &Path) -> anyhow::Result<String> {
    let read_future = read_large_file_async(file_path);
    
    match timeout(Duration::from_secs(5), read_future).await {
        Ok(Ok(content)) => Ok(content),
        Ok(Err(e)) => Err(e),
        Err(_) => {
            anyhow::bail!("File read timeout: {}", file_path.display())
        }
    }
}
```

## 最佳实践建议

### 1. 混合策略

```elisp
(defcustom lsp-proxy-large-file-read-strategy 'auto
  "Strategy for reading large files.
- 'auto: Server reads if possible, fallback to client
- 'server: Always let server read
- 'client: Always send from client
- 'ask: Ask user for each file"
  :type '(choice (const :tag "Auto (recommended)" auto)
                 (const :tag "Server read" server)
                 (const :tag "Client send" client)
                 (const :tag "Ask user" ask))
  :group 'lsp-proxy)

(defun lsp-proxy--decide-read-strategy (file-path file-size)
  "Decide whether to use server-side reading."
  (cond
   ((eq lsp-proxy-large-file-read-strategy 'client) nil)
   ((eq lsp-proxy-large-file-read-strategy 'server) t)
   ((eq lsp-proxy-large-file-read-strategy 'ask)
    (y-or-n-p "Let server read large file? "))
   ((eq lsp-proxy-large-file-read-strategy 'auto)
    (and (> file-size lsp-proxy-large-file-threshold)
         (lsp-proxy--can-server-read-file file-path)
         (not (buffer-modified-p))))))
```

### 2. 错误处理和回退机制

```rust
pub fn handle_did_open_with_fallback(
    app: Arc<Mutex<Application>>,
    params: lsp_types::DidOpenTextDocumentParams,
) -> anyhow::Result<()> {
    let uri = params.text_document.uri.clone();
    let mut text = params.text_document.text.clone();
    let should_server_read = /* 检查标识 */;
    
    if should_server_read && text.is_empty() {
        match try_server_read_file(&uri) {
            Ok(content) => {
                text = content;
                tracing::info!("Successfully read file on server side");
            }
            Err(e) => {
                tracing::warn!("Server read failed: {}, requesting client content", e);
                // 发送请求给客户端重新发送内容
                request_client_resend(&uri)?;
                return Ok(());
            }
        }
    }
    
    // 继续处理...
    Ok(())
}
```

### 3. 监控和诊断

```rust
// 添加统计信息
#[derive(Default)]
pub struct FileReadStats {
    pub server_reads: u64,
    pub client_sends: u64,
    pub read_failures: u64,
    pub total_bytes_read: u64,
}

impl FileReadStats {
    pub fn record_server_read(&mut self, bytes: usize) {
        self.server_reads += 1;
        self.total_bytes_read += bytes as u64;
    }
    
    pub fn record_read_failure(&mut self) {
        self.read_failures += 1;
    }
}
```

## 结论

服务器端文件读取是一个可行且有效的优化方案，特别适合以下场景：

### 推荐使用的情况：
1. **大文件且未修改**: 磁盘内容与缓冲区一致
2. **本地文件**: 非远程或特殊协议文件
3. **权限充足**: 服务器进程有文件读取权限
4. **编码简单**: UTF-8 或常见编码文件

### 不建议使用的情况：
1. **频繁修改的文件**: 缓冲区内容经常与磁盘不一致
2. **远程文件**: TRAMP、Docker、SSH 等
3. **权限受限**: 容器化或沙箱环境
4. **特殊编码**: 需要特定编码处理的文件

### 最佳实施方案：
结合多种策略，以**自动检测 + 优雅降级**为核心，提供用户可配置的选项。这样既能获得性能提升，又能保证功能的可靠性。

这个方案可以作为前面提到的方案一的补充，或者作为一个独立的优化选项供用户选择。