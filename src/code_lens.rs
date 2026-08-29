use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct CodeLensItem {
    pub lsp_item: lsp_types::CodeLens,
    pub language_server_id: usize,
    pub language_server_name: String,
}
