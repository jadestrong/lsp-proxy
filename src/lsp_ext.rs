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
    pub text_document_sync_kind: String, // "full" or "incremental"
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

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct LargeFileLoadStartParams {
    pub uri: lsp_types::Url,
    #[serde(rename = "totalSize")]
    pub total_size: usize,
    #[serde(rename = "chunkSize")]
    pub chunk_size: usize,
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct LargeFileChunkParams {
    pub uri: lsp_types::Url,
    #[serde(rename = "chunkIndex")]
    pub chunk_index: usize,
    #[serde(rename = "chunkData")]
    pub chunk_data: String,
    #[serde(rename = "startPos")]
    pub start_pos: usize,
    #[serde(rename = "endPos")]
    pub end_pos: usize,
    #[serde(rename = "isLastChunk")]
    #[serde(default)]
    pub is_last_chunk: Option<bool>,
    pub progress: u8,
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct LargeFileLoadCompleteParams {
    pub uri: lsp_types::Url,
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct LargeFileLoadCancelParams {
    pub uri: lsp_types::Url,
}

#[derive(Debug)]
pub enum EmacsLargeFileLoadStart {}

impl Notification for EmacsLargeFileLoadStart {
    type Params = LargeFileLoadStartParams;
    const METHOD: &'static str = "emacs/largeFileLoadStart";
}

#[derive(Debug)]
pub enum EmacsLargeFileChunk {}

impl Notification for EmacsLargeFileChunk {
    type Params = LargeFileChunkParams;
    const METHOD: &'static str = "emacs/largeFileChunk";
}

#[derive(Debug)]
pub enum EmacsLargeFileLoadCancel {}

impl Notification for EmacsLargeFileLoadCancel {
    type Params = LargeFileLoadCancelParams;

    const METHOD: &'static str = "emacs/largeFileLoadCancel";
}

// tsserver/request for Vue
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct TsserverRequestParams(pub u32, pub String, pub serde_json::Value);

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct TsserverRequestResult {
    pub body: serde_json::Value,
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum TsserverRequest {}

impl Notification for TsserverRequest {
    type Params = Vec<TsserverRequestParams>;

    const METHOD: &'static str = "tsserver/request";
}

// tsserver/response for Vue
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct TsserverResponseParams(pub u32, pub serde_json::Value);

#[derive(Debug)]
pub enum TsserverResponse {}

impl Notification for TsserverResponse {
    type Params = Vec<TsserverResponseParams>;

    const METHOD: &'static str = "tsserver/response";
}

#[derive(Debug)]
pub enum RustAnalyzerExpandMacro {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExpandMacroResult {
    pub name: String,
    pub expansion: String,
}

impl Request for RustAnalyzerExpandMacro {
    type Params = lsp_types::TextDocumentPositionParams;
    type Result = ExpandMacroResult;
    const METHOD: &'static str = "rust-analyzer/expandMacro";
}

// Remote development LSP extensions

// emacs/remoteConnect
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RemoteConnectParams {
    pub server_name: String,
    pub host: Option<String>,
    pub user: Option<String>,
    pub port: Option<u16>,
}

#[derive(Debug)]
pub enum RemoteConnect {}

impl Request for RemoteConnect {
    type Params = RemoteConnectParams;
    type Result = serde_json::Value;
    const METHOD: &'static str = "emacs/remoteConnect";
}

// emacs/remoteDisconnect
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RemoteDisconnectParams {
    pub server_name: String,
}

#[derive(Debug)]
pub enum RemoteDisconnect {}

impl Request for RemoteDisconnect {
    type Params = RemoteDisconnectParams;
    type Result = serde_json::Value;
    const METHOD: &'static str = "emacs/remoteDisconnect";
}

// emacs/remoteList
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct RemoteListParams {}

#[derive(Debug)]
pub enum RemoteList {}

impl Request for RemoteList {
    type Params = RemoteListParams;
    type Result = serde_json::Value;
    const METHOD: &'static str = "emacs/remoteList";
}

// emacs/remoteStatus
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RemoteStatusParams {
    pub server_name: String,
}

#[derive(Debug)]
pub enum RemoteStatus {}

impl Request for RemoteStatus {
    type Params = RemoteStatusParams;
    type Result = serde_json::Value;
    const METHOD: &'static str = "emacs/remoteStatus";
}

// emacs/remoteFileRead
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RemoteFileReadParams {
    pub server_name: String,
    pub file_path: String,
}

#[derive(Debug)]
pub enum RemoteFileRead {}

impl Request for RemoteFileRead {
    type Params = RemoteFileReadParams;
    type Result = serde_json::Value;
    const METHOD: &'static str = "emacs/remoteFileRead";
}

// emacs/remoteFileWrite
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RemoteFileWriteParams {
    pub server_name: String,
    pub file_path: String,
    pub content: String,
}

#[derive(Debug)]
pub enum RemoteFileWrite {}

impl Request for RemoteFileWrite {
    type Params = RemoteFileWriteParams;
    type Result = serde_json::Value;
    const METHOD: &'static str = "emacs/remoteFileWrite";
}

// emacs/remoteLspRequest
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RemoteLspRequestParams {
    pub server_name: String,
    pub lsp_server_id: u32,
    pub method: String,
    pub params: serde_json::Value,
}

#[derive(Debug)]
pub enum RemoteLspRequest {}

impl Request for RemoteLspRequest {
    type Params = RemoteLspRequestParams;
    type Result = serde_json::Value;
    const METHOD: &'static str = "emacs/remoteLspRequest";
}

// emacs/remoteWorkspace
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
pub struct RemoteWorkspaceParams {}

#[derive(Debug)]
pub enum RemoteWorkspace {}

impl Request for RemoteWorkspace {
    type Params = RemoteWorkspaceParams;
    type Result = serde_json::Value;
    const METHOD: &'static str = "emacs/remoteWorkspace";
}

// Re-export jsonrpc_core for error handling
pub use serde_json as jsonrpc_core;
