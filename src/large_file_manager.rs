use std::{collections::HashMap, time::Instant};

#[derive(Debug, Clone, PartialEq)]
pub enum LargeFileLoadState {
    Starting,
    Loading {
        chunks_received: usize,
        total_chunks: usize,
        start_time: Instant,
    },
    Completed,
    Cancelled,
}

pub struct LargeFileManager {
    pub loading_files: HashMap<lsp_types::Url, LargeFileLoadState>,
    pub file_contents: HashMap<lsp_types::Url, String>,
    pub file_chunks: HashMap<lsp_types::Url, Vec<String>>,
}

impl LargeFileManager {
    pub fn new() -> Self {
        Self {
            loading_files: HashMap::new(),
            file_contents: HashMap::new(),
            file_chunks: HashMap::new(),
        }
    }

    pub fn start_loading(&mut self, uri: lsp_types::Url, total_size: usize) {
        self.loading_files
            .insert(uri.clone(), LargeFileLoadState::Starting);
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

    pub fn get_content(&self, uri: &lsp_types::Url) -> Option<&String> {
        self.file_contents.get(uri)
    }

    pub fn cancel_loading(&mut self, uri: &lsp_types::Url) {
        self.loading_files
            .insert(uri.clone(), LargeFileLoadState::Cancelled);
        self.file_contents.remove(uri);
        self.file_chunks.remove(uri);
        tracing::info!("Cancelled large file loading: {}", uri);
    }
}
