use crate::msg::RequestId;
use lsp_types::{
    notification::Notification, request::Request, DidCloseTextDocumentParams, ProgressParams,
};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CompletionItem {
    pub item: lsp_types::CompletionItem,
    pub language_server_id: Option<usize>,
    pub language_server_name: String,
    pub start: i32,
    pub end: i32,
}

// emacs/serverCapabilities
#[derive(Debug)]
pub enum CustomServerCapabilities {}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CustomServerCapabilitiesParams {
    pub uri: String,
    pub trigger_characters: Vec<String>,
    pub support_inlay_hints: bool,
    pub support_document_highlight: bool,
    pub support_document_symbols: bool,
    pub support_signature_help: bool,
    pub support_pull_diagnostic: bool,
    pub support_inline_completion: bool,
}

impl Notification for CustomServerCapabilities {
    type Params = CustomServerCapabilitiesParams;
    const METHOD: &'static str = "emacs/serverCapabilities";
}

// $/cancelRequest
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CustomizeCancelParams {
    pub uri: Option<String>,
    pub id: RequestId,
}

#[derive(Debug)]
pub enum CustomizeCancel {}

impl Notification for CustomizeCancel {
    type Params = CustomizeCancelParams;
    const METHOD: &'static str = "$/cancelRequest";
}

// emacs/workspaceRestart
#[derive(Debug)]
pub enum WorkspaceRestart {}

#[derive(Debug, Deserialize, Serialize)]
pub struct WorkspaceRestartResponse {
    pub paths: Vec<String>,
}

impl Request for WorkspaceRestart {
    type Params = ();
    type Result = Option<WorkspaceRestartResponse>;
    const METHOD: &'static str = "emacs/workspaceRestart";
}

// emacs/getFiles
#[derive(Debug)]
#[allow(dead_code)]
pub enum GetFiles {}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct FileConfig {
    pub path: PathBuf,
    pub language_id: String,
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GetFilesParams {
    pub paths: Vec<PathBuf>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct EmacsFile {
    pub path: String,
    pub content: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct GetFilesResponse {
    pub files: Vec<EmacsFile>,
}

impl Request for GetFiles {
    type Params = GetFilesParams;
    type Result = GetFilesResponse;
    const METHOD: &'static str = "emacs/getFiles";
}

// emacs/getCommands
#[derive(Debug)]
pub enum GetCommands {}

#[derive(Debug, Serialize, Deserialize)]
pub struct CommandItem {
    pub id: String,
    pub language_server_id: usize,
}

impl Request for GetCommands {
    type Params = ();
    type Result = Vec<CommandItem>;
    const METHOD: &'static str = "emacs/getCommands";
}

// $/progress
#[derive(Debug)]
pub enum CustomProgress {}

#[derive(Debug, PartialEq, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct CustomProgressParams {
    pub root_path: String,
    pub params: ProgressParams,
}

impl Notification for CustomProgress {
    type Params = CustomProgressParams;
    const METHOD: &'static str = "$/progress";
}

// didFocus
pub enum DidFocusTextDocument {}

impl Notification for DidFocusTextDocument {
    type Params = DidCloseTextDocumentParams;
    const METHOD: &'static str = "textDocument/didFocus";
}

// rust

#[derive(Debug)]
pub enum ViewFileText {}

#[derive(Debug, PartialEq, Deserialize, Serialize, Clone)]
pub struct ViewFileTextParams {
    pub uri: String,
}

impl Request for ViewFileText {
    type Params = ViewFileTextParams;
    type Result = Option<String>;
    const METHOD: &'static str = "rust-analyzer/viewFileText";
}

pub enum RustAnalyzerReloadWorkspace {}

impl Request for RustAnalyzerReloadWorkspace {
    type Params = ();
    type Result = ();
    const METHOD: &'static str = "rust-analyzer/reloadWorkspace";
}
/// Add version to inline completion
#[derive(Debug, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct VersionInlineCompletionResult {
    pub doc_version: i32,
    pub items: Vec<lsp_types::InlineCompletionItem>,
}
