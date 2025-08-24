use std::{collections::HashMap, time::Instant};
use tokio::time::Duration;

#[derive(Debug, Clone, PartialEq)]
pub enum LargeFileLoadState {
    Starting,
    Loading {
        chunks_received: usize,
        total_chunks: usize,
        start_time: Instant,
    },
    Completed,
    Failed(String),
    Cancelled,
}

/// 缓冲的请求
// #[derive(Debug, Clone)]
// pub struct BufferedRequest {
//     pub method: String,
//     pub params: serde_json::Value,
//     pub id: Option<lsp_types::NumberOrString>,
//     pub timestamp: Instant,
// }

/// 大文件管理器
pub struct LargeFileManager {
    pub loading_files: HashMap<lsp_types::Url, LargeFileLoadState>,
    // pub buffered_requests: HashMap<lsp_types::Url, VecDeque<BufferedRequest>>,
    pub file_contents: HashMap<lsp_types::Url, String>,
    pub file_chunks: HashMap<lsp_types::Url, Vec<String>>,
}

impl LargeFileManager {
    pub fn new() -> Self {
        Self {
            loading_files: HashMap::new(),
            // buffered_requests: HashMap::new(),
            file_contents: HashMap::new(),
            file_chunks: HashMap::new(),
        }
    }

    pub fn start_loading(&mut self, uri: lsp_types::Url, total_size: usize) {
        self.loading_files
            .insert(uri.clone(), LargeFileLoadState::Starting);
        // self.buffered_requests.insert(uri.clone(), VecDeque::new());
        self.file_contents
            .insert(uri.clone(), String::with_capacity(total_size));
        self.file_chunks.insert(uri.clone(), Vec::new());
        tracing::info!("Started large file loading: {}", uri);
    }

    pub fn add_chunk(
        &mut self,
        uri: &lsp_types::Url,
        chunk_data: String,
        chunk_index: usize,
        is_last: bool,
    ) -> anyhow::Result<bool> {
        // 确保有存储空间
        if !self.file_chunks.contains_key(uri) {
            self.file_chunks.insert(uri.clone(), Vec::new());
        }

        let chunks = self.file_chunks.get_mut(uri).unwrap();

        // 扩展 vec 以容纳新的 chunk index
        if chunks.len() <= chunk_index {
            chunks.resize(chunk_index + 1, String::new());
        }

        chunks[chunk_index] = chunk_data;

        // 更新加载状态
        let chunks_received = chunks.iter().filter(|s| !s.is_empty()).count();
        self.loading_files.insert(
            uri.clone(),
            LargeFileLoadState::Loading {
                chunks_received,
                total_chunks: if is_last {
                    chunk_index + 1
                } else {
                    chunk_index + 2
                },
                start_time: Instant::now(),
            },
        );

        if is_last {
            // 重组完整内容
            let full_content = chunks.join("");
            self.file_contents.insert(uri.clone(), full_content);
            self.loading_files
                .insert(uri.clone(), LargeFileLoadState::Completed);

            // 清理临时数据
            self.file_chunks.remove(uri);

            tracing::info!(
                "Large file loading completed: {} ({} chunks)",
                uri,
                chunks_received
            );
            return Ok(true);
        }

        tracing::debug!(
            "Received chunk {} for {}, total chunks: {}",
            chunk_index,
            uri,
            chunks_received
        );
        Ok(false)
    }

    pub fn is_loading(&self, uri: &lsp_types::Url) -> bool {
        matches!(
            self.loading_files.get(uri),
            Some(LargeFileLoadState::Starting) | Some(LargeFileLoadState::Loading { .. })
        )
    }

    // pub fn buffer_request(&mut self, uri: &lsp_types::Url, request: BufferedRequest) {
    //     if let Some(queue) = self.buffered_requests.get_mut(uri) {
    //         queue.push_back(request);
    //         tracing::debug!("Buffered request for {}: {} (queue size: {})",
    //                       uri, request.method, queue.len());
    //     }
    // }

    // pub fn get_buffered_requests(&mut self, uri: &lsp_types::Url) -> Option<VecDeque<BufferedRequest>> {
    //     self.buffered_requests.remove(uri)
    // }

    pub fn get_content(&self, uri: &lsp_types::Url) -> Option<&String> {
        self.file_contents.get(uri)
    }

    pub fn cancel_loading(&mut self, uri: &lsp_types::Url) {
        self.loading_files
            .insert(uri.clone(), LargeFileLoadState::Cancelled);
        // self.buffered_requests.remove(uri);
        self.file_contents.remove(uri);
        self.file_chunks.remove(uri);
        tracing::info!("Cancelled large file loading: {}", uri);
    }

    pub fn get_loading_stats(
        &self,
        uri: &lsp_types::Url,
    ) -> Option<(usize, usize, std::time::Duration)> {
        if let Some(LargeFileLoadState::Loading {
            chunks_received,
            total_chunks,
            start_time,
        }) = self.loading_files.get(uri)
        {
            Some((*chunks_received, *total_chunks, start_time.elapsed()))
        } else {
            None
        }
    }
}

/// 大文件加载处理函数

/// 处理大文件加载开始
// pub fn handle_large_file_load_start(
//     app: Arc<Mutex<Application>>,
//     params: LargeFileLoadStartParams,
// ) -> anyhow::Result<()> {
//     {
//         let mut app = app.lock().unwrap();
//         app.large_file_manager.lock().unwrap()
//             .start_loading(params.uri.clone(), params.total_size);
//     }

//     tracing::info!("Large file load started: {} ({} bytes, {} chunk size)",
//                    params.uri, params.total_size, params.chunk_size);
//     Ok(())
// }

/// 处理大文件分块
// pub fn handle_large_file_chunk(
//     app: Arc<Mutex<Application>>,
//     params: LargeFileChunkParams,
// ) -> anyhow::Result<()> {
//     let is_completed = {
//         let mut app = app.lock().unwrap();
//         app.large_file_manager.lock().unwrap()
//             .add_chunk(&params.uri, params.chunk_data, params.chunk_index, params.is_last_chunk)?
//     };

//     tracing::debug!("Received chunk {} for {}, progress: {}%",
//                    params.chunk_index, params.uri, params.progress);

//     if is_completed {
//         handle_large_file_load_complete_internal(app, params.uri)?;
//     }

//     Ok(())
// }

/// 处理大文件加载完成
// pub fn handle_large_file_load_complete(
//     app: Arc<Mutex<Application>>,
//     params: LargeFileLoadCompleteParams,
// ) -> anyhow::Result<()> {
//     handle_large_file_load_complete_internal(app, params.uri)
// }

/// 处理大文件加载取消
// pub fn handle_large_file_load_cancel(
//     app: Arc<Mutex<Application>>,
//     params: LargeFileLoadCancelParams,
// ) -> anyhow::Result<()> {
//     {
//         let mut app = app.lock().unwrap();
//         app.large_file_manager.lock().unwrap()
//             .cancel_loading(&params.uri);
//     }

//     tracing::info!("Large file loading cancelled: {}", params.uri);
//     Ok(())
// }

/// 内部处理大文件加载完成
// fn handle_large_file_load_complete_internal(
//     app: Arc<Mutex<Application>>,
//     uri: lsp_types::Url,
// ) -> anyhow::Result<()> {
//     let (content, buffered_requests) = {
//         let mut app = app.lock().unwrap();
//         let mut manager = app.large_file_manager.lock().unwrap();

//         let content = manager.get_content(&uri).cloned()
//             .ok_or_else(|| anyhow::anyhow!("Large file content not found"))?;

//         let requests = manager.get_buffered_requests(&uri)
//             .unwrap_or_default();

//         (content, requests)
//     };

//     // 发送完整内容给语言服务器
//     {
//         let mut app = app.lock().unwrap();
//         let document = app.editor.get_or_create_document(&uri, content.clone())?;

//         // 创建 didChange 参数来更新内容
//         let did_change_params = lsp_types::DidChangeTextDocumentParams {
//             text_document: lsp_types::VersionedTextDocumentIdentifier {
//                 uri: uri.clone(),
//                 version: document.version,
//             },
//             content_changes: vec![lsp_types::TextDocumentContentChangeEvent {
//                 range: None,
//                 range_length: None,
//                 text: content,
//             }],
//         };

//         // 发送给所有相关的语言服务器
//         let language_servers = app.editor.get_language_servers(&uri)?;
//         for ls in language_servers {
//             if let Err(e) = ls.text_document_did_change(did_change_params.clone()) {
//                 tracing::error!("Failed to send didChange to {}: {}", ls.name(), e);
//             }
//         }
//     }

//     // 处理缓冲的请求
//     if !buffered_requests.is_empty() {
//         tracing::info!("Processing {} buffered requests for {}",
//                       buffered_requests.len(), uri);

//         // 在新的任务中处理缓冲的请求，避免阻塞
//         let app_clone = app.clone();
//         let uri_clone = uri.clone();
//         tokio::spawn(async move {
//             for request in buffered_requests {
//                 if let Err(e) = process_buffered_request(app_clone.clone(), &uri_clone, request).await {
//                     tracing::error!("Failed to process buffered request: {}", e);
//                 }
//                 // 小延迟避免过载
//                 tokio::time::sleep(Duration::from_millis(10)).await;
//             }
//         });
//     }

//     tracing::info!("Large file loading completed and requests processed: {}", uri);
//     Ok(())
// }

/// 处理缓冲的请求
// async fn process_buffered_request(
//     app: Arc<Mutex<Application>>,
//     uri: &lsp_types::Url,
//     request: BufferedRequest,
// ) -> anyhow::Result<()> {
//     // 根据请求类型重新路由到相应的处理函数
//     match request.method.as_str() {
//         "textDocument/completion" => {
//             let params: lsp_types::CompletionParams = serde_json::from_value(request.params)?;
//             handle_completion_request(app, params).await
//         }
//         "textDocument/hover" => {
//             let params: lsp_types::HoverParams = serde_json::from_value(request.params)?;
//             handle_hover_request(app, params).await
//         }
//         "textDocument/signatureHelp" => {
//             let params: lsp_types::SignatureHelpParams = serde_json::from_value(request.params)?;
//             handle_signature_help_request(app, params).await
//         }
//         "textDocument/definition" => {
//             let params: lsp_types::GotoDefinitionParams = serde_json::from_value(request.params)?;
//             handle_definition_request(app, params).await
//         }
//         "textDocument/references" => {
//             let params: lsp_types::ReferenceParams = serde_json::from_value(request.params)?;
//             handle_references_request(app, params).await
//         }
//         "textDocument/codeAction" => {
//             let params: lsp_types::CodeActionParams = serde_json::from_value(request.params)?;
//             handle_code_action_request(app, params).await
//         }
//         _ => {
//             tracing::warn!("Unknown buffered request type: {}", request.method);
//             Ok(())
//         }
//     }
// }

/// 请求拦截和缓冲逻辑

/// 检查是否应该缓冲请求
// pub fn should_buffer_request(
//     app: &Arc<Mutex<Application>>,
//     uri: &lsp_types::Url,
//     method: &str,
// ) -> bool {
//     let manager = app.lock().unwrap().large_file_manager.clone();
//     let manager = manager.lock().unwrap();

//     manager.is_loading(uri) && matches!(method,
//         "textDocument/completion" |
//         "textDocument/hover" |
//         "textDocument/signatureHelp" |
//         "textDocument/definition" |
//         "textDocument/references" |
//         "textDocument/codeAction" |
//         "textDocument/documentSymbol"
//     )
// }

/// 从不同类型的 LSP 参数中提取 URI
// pub fn extract_text_document_uri(params: &serde_json::Value) -> anyhow::Result<lsp_types::Url> {
//     // 尝试从 textDocument.uri 提取
//     if let Some(text_doc) = params.get("textDocument") {
//         if let Some(uri) = text_doc.get("uri") {
//             return Ok(serde_json::from_value(uri.clone())?);
//         }
//     }

//     // 尝试从其他可能的位置提取 URI
//     if let Some(uri) = params.get("uri") {
//         return Ok(serde_json::from_value(uri.clone())?);
//     }

//     Err(anyhow::anyhow!("No URI found in request parameters"))
// }

/// 修改的请求处理函数
// pub fn handle_lsp_request(
//     app: Arc<Mutex<Application>>,
//     method: String,
//     params: serde_json::Value,
//     id: Option<lsp_types::NumberOrString>,
// ) -> anyhow::Result<()> {
//     // 尝试从参数中提取 URI
//     if let Ok(uri) = extract_text_document_uri(&params) {
//         if should_buffer_request(&app, &uri, &method) {
//             // 缓冲请求
//             let buffered_request = BufferedRequest {
//                 method: method.clone(),
//                 params,
//                 id,
//                 timestamp: Instant::now(),
//             };

//             app.lock().unwrap()
//                 .large_file_manager.lock().unwrap()
//                 .buffer_request(&uri, buffered_request);

//             tracing::debug!("Buffered {} request for loading file: {}", method, uri);
//             return Ok(());
//         }
//     }

//     // 正常处理请求
//     match method.as_str() {
//         "textDocument/completion" => {
//             let params: lsp_types::CompletionParams = serde_json::from_value(params)?;
//             tokio::spawn(async move {
//                 if let Err(e) = handle_completion_request(app, params).await {
//                     tracing::error!("Failed to handle completion request: {}", e);
//                 }
//             });
//         }
//         "textDocument/hover" => {
//             let params: lsp_types::HoverParams = serde_json::from_value(params)?;
//             tokio::spawn(async move {
//                 if let Err(e) = handle_hover_request(app, params).await {
//                     tracing::error!("Failed to handle hover request: {}", e);
//                 }
//             });
//         }
//         // 其他请求处理...
//         _ => {
//             tracing::debug!("Unhandled request method: {}", method);
//         }
//     }

//     Ok(())
// }

/// 占位符处理函数（需要根据实际实现替换）
// async fn handle_completion_request(app: Arc<Mutex<Application>>, params: lsp_types::CompletionParams) -> anyhow::Result<()> {
//     // 实际的补全处理逻辑
//     tracing::debug!("Processing completion request for {}", params.text_document_position.text_document.uri);
//     Ok(())
// }

// async fn handle_hover_request(app: Arc<Mutex<Application>>, params: lsp_types::HoverParams) -> anyhow::Result<()> {
//     // 实际的悬停处理逻辑
//     tracing::debug!("Processing hover request for {}", params.text_document_position_params.text_document.uri);
//     Ok(())
// }

// async fn handle_signature_help_request(app: Arc<Mutex<Application>>, params: lsp_types::SignatureHelpParams) -> anyhow::Result<()> {
//     tracing::debug!("Processing signature help request for {}", params.text_document_position_params.text_document.uri);
//     Ok(())
// }

// async fn handle_definition_request(app: Arc<Mutex<Application>>, params: lsp_types::GotoDefinitionParams) -> anyhow::Result<()> {
//     tracing::debug!("Processing definition request for {}", params.text_document_position_params.text_document.uri);
//     Ok(())
// }

// async fn handle_references_request(app: Arc<Mutex<Application>>, params: lsp_types::ReferenceParams) -> anyhow::Result<()> {
//     tracing::debug!("Processing references request for {}", params.text_document_position.text_document.uri);
//     Ok(())
// }

// async fn handle_code_action_request(app: Arc<Mutex<Application>>, params: lsp_types::CodeActionParams) -> anyhow::Result<()> {
//     tracing::debug!("Processing code action request for {}", params.text_document.uri);
//     Ok(())
// }

/// 统计和监控

#[derive(Debug, Default)]
pub struct LargeFileStats {
    pub files_loaded: u64,
    pub total_chunks: u64,
    pub total_bytes: u64,
    pub requests_buffered: u64,
    pub average_load_time: f64,
}

impl LargeFileStats {
    pub fn record_file_loaded(&mut self, chunks: usize, bytes: usize, load_time: Duration) {
        self.files_loaded += 1;
        self.total_chunks += chunks as u64;
        self.total_bytes += bytes as u64;

        // 计算平均加载时间
        let load_time_secs = load_time.as_secs_f64();
        self.average_load_time = (self.average_load_time * (self.files_loaded - 1) as f64
            + load_time_secs)
            / self.files_loaded as f64;
    }

    pub fn record_request_buffered(&mut self) {
        self.requests_buffered += 1;
    }
}

/*
pub struct Application {
    // 现有字段...
    pub large_file_manager: Arc<Mutex<LargeFileManager>>,
    pub large_file_stats: Arc<Mutex<LargeFileStats>>,
}

impl Application {
    pub fn new() -> Self {
        Self {
            // 初始化现有字段...
            large_file_manager: Arc::new(Mutex::new(LargeFileManager::new())),
            large_file_stats: Arc::new(Mutex::new(LargeFileStats::default())),
        }
    }
}
*/

/*
// 在消息分发器中添加
match method.as_str() {
    "emacs/largeFileLoadStart" => {
        let params: LargeFileLoadStartParams = serde_json::from_value(msg.params)?;
        handle_large_file_load_start(app.clone(), params)?;
    }
    "emacs/largeFileChunk" => {
        let params: LargeFileChunkParams = serde_json::from_value(msg.params)?;
        handle_large_file_chunk(app.clone(), params)?;
    }
    "emacs/largeFileLoadComplete" => {
        let params: LargeFileLoadCompleteParams = serde_json::from_value(msg.params)?;
        handle_large_file_load_complete(app.clone(), params)?;
    }
    "emacs/largeFileLoadCancel" => {
        let params: LargeFileLoadCancelParams = serde_json::from_value(msg.params)?;
        handle_large_file_load_cancel(app.clone(), params)?;
    }
    // 其他现有处理...
}
*/
