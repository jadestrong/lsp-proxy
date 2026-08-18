use crate::{msg::RequestId, syntax};
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
    pub support_hover: bool,
    pub has_any_servers: bool,
    /// Workspace root of every language server serving this document.
    ///
    /// A list, not one value: a document can be served by several servers (vtsls +
    /// eslint + tailwind) whose roots differ, and unlike the booleans above these
    /// cannot be folded together.
    ///
    /// The editor needs them because `$/progress` is filed by server root, while the
    /// editor's own notion of "the project" comes from project.el. In a monorepo those
    /// disagree — the server root is the module (where `pom.xml` lives), project.el's
    /// is the repository (where `.git` lives) — so a lookup keyed on the latter never
    /// finds progress reported under the former.
    #[serde(default)]
    pub workspace_roots: Vec<String>,
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

// emacs/installJavaServer
//
// Downloads the JetBrains IntelliJ language server, which `lsp-proxy-java`
// requires. Lives here rather than in Emacs Lisp because the proxy already has
// tokio (so the ~370 MB download does not block the editor) and an established
// `$/progress` pipeline; the Emacs side is only a thin command.
//
// The *proxy's own* installer stays in Emacs Lisp on purpose — that one cannot
// run here, since the proxy does not exist yet when it runs.
#[derive(Debug)]
pub enum InstallJavaServer {}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InstallJavaServerParams {
    /// Where to place `<version>/bin/intellij-server`. Chosen by the client so the
    /// location stays next to everything else Emacs manages.
    pub install_dir: String,
    /// Pin a build (e.g. "263.2689.0"); `None` resolves the latest via Open VSX.
    #[serde(default)]
    pub version: Option<String>,
    /// Reinstall even when this version is already present.
    #[serde(default)]
    pub force: bool,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InstallJavaServerResult {
    pub launcher_path: String,
    pub version: String,
    /// True when the requested version was already installed and nothing was
    /// downloaded.
    pub already_installed: bool,
}

impl Request for InstallJavaServer {
    type Params = InstallJavaServerParams;
    type Result = InstallJavaServerResult;
    const METHOD: &'static str = "emacs/installJavaServer";
}

// emacs/installProgress
//
// Progress for a long install, reported on its own channel rather than as
// `$/progress`. The editor files `$/progress` by project root and only renders it
// for a buffer that is both in that project and has `lsp-proxy-mode' on, so a
// several-hundred-megabyte download started from anywhere else would be entirely
// silent. This goes to the echo area instead, which is visible from any buffer.
#[derive(Debug)]
pub enum InstallProgress {}

#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "camelCase")]
pub struct InstallProgressParams {
    pub message: String,
    /// Completion percentage when the phase has a measurable size.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub percentage: Option<u32>,
}

impl Notification for InstallProgress {
    type Params = InstallProgressParams;
    const METHOD: &'static str = "emacs/installProgress";
}

// intellij/importLog
//
// Build-tool import progress from the IntelliJ language server (Maven/Gradle/Bazel
// resolving a project). Mirrors the `ImportLogParams` interface in the JetBrains
// VS Code extension's `lspClient.ts`.
//
// Kept separate from `window/logMessage`: an import is a discrete, long operation
// whose outcome the user acts on (a failed import means no symbols resolve), so it
// gets its own buffer rather than being interleaved with general server chatter.
#[derive(Debug)]
pub enum ImportLog {}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ImportLogParams {
    /// `lsp_types::MessageType`: 1 = error, 2 = warning, 3 = info.
    ///
    /// Optional because losing a terminal `failed` event to a strict-parse error
    /// would be worse than rendering one line without a severity face.
    #[serde(rename = "type", default)]
    pub typ: Option<u8>,
    #[serde(default)]
    pub message: String,
    /// Build-tool display name, e.g. "Maven" / "Gradle" / "Bazel". Set on the
    /// `started` and `failed` events.
    #[serde(default)]
    pub tool: Option<String>,
    #[serde(default)]
    pub failed: bool,
    #[serde(default)]
    pub succeeded: bool,
    /// Marks the beginning of an import; carries no message worth showing.
    #[serde(default)]
    pub started: bool,
    /// Filled in by the proxy, not the server: which workspace root this import
    /// belongs to. A monorepo runs one import per module, and their lines would
    /// otherwise interleave in one buffer with no way to tell them apart.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub root_path: Option<String>,
}

impl Notification for ImportLog {
    type Params = ImportLogParams;
    const METHOD: &'static str = "intellij/importLog";
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

// emacs/getWorkspaceInfo
#[derive(Debug)]
pub enum GetWorkspaceInfo {}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WorkspaceInfo {
    pub file_path: String,
    pub workspace_root: String,
    pub language_servers: Vec<LanguageServerInfo>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LanguageServerInfo {
    pub name: String,
    pub root_path: String,
    pub support_workspace: syntax::SupportWorkspace,
}

impl Request for GetWorkspaceInfo {
    type Params = ();
    type Result = Option<WorkspaceInfo>;
    const METHOD: &'static str = "emacs/getWorkspaceInfo";
}

// emacs/getLanguagesConfig
#[derive(Debug)]
pub enum GetLanguagesConfig {}

impl Request for GetLanguagesConfig {
    type Params = ();
    type Result = String; // JSON string of merged languages config
    const METHOD: &'static str = "emacs/getLanguagesConfig";
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

// emacs/getRemoteInfo
#[derive(Debug)]
pub enum GetRemoteInfo {}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RemoteClientInfo {
    pub connection_key: String,
    pub remote_type: String,
    pub is_alive: bool,
    /// Absolute path where the remote binary is (or should be) installed.
    pub binary_path: String,
    /// Version of the locally-running lsp-proxy (compile-time).
    pub local_version: String,
    /// Version reported by the remote binary, if reachable.
    pub remote_version: Option<String>,
    /// One of: "deployed", "missing", "version_mismatch", "unknown".
    pub deploy_status: String,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RemoteConnectionStatus {
    pub enabled: bool,
    pub clients: Vec<RemoteClientInfo>,
}

impl Request for GetRemoteInfo {
    type Params = ();
    type Result = RemoteConnectionStatus;
    const METHOD: &'static str = "emacs/getRemoteInfo";
}

// emacs/checkRemoteBinary — probe both the global command and the deploy path
// without uploading anything.
#[derive(Debug)]
pub enum CheckRemoteBinary {}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RemoteBinaryLocationStatus {
    /// "match" | "version_mismatch" | "missing"
    pub status: String,
    pub version: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CheckRemoteBinaryResult {
    pub global: RemoteBinaryLocationStatus,
    pub path: RemoteBinaryLocationStatus,
    /// Set when either location has a matching binary; the value to pass to
    /// the server launch command.
    pub available_binary: Option<String>,
    pub local_version: String,
    pub deploy_path: String,
}

impl Request for CheckRemoteBinary {
    type Params = DeployRemoteBinaryParams;
    type Result = CheckRemoteBinaryResult;
    const METHOD: &'static str = "emacs/checkRemoteBinary";
}

// emacs/remoteDeployNeeded — sent by the server when the remote binary is
// missing or outdated, asking the user to trigger a manual deploy.
#[derive(Debug)]
pub enum RemoteDeployNeeded {}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RemoteDeployNeededParams {
    pub connection_key: String,
    /// "missing" | "version_mismatch"
    pub reason: String,
    pub remote_version: Option<String>,
    pub local_version: String,
    /// The fallback deploy path that would be used if the user deploys.
    pub deploy_path: String,
}

impl Notification for RemoteDeployNeeded {
    type Params = RemoteDeployNeededParams;
    const METHOD: &'static str = "emacs/remoteDeployNeeded";
}

// emacs/deployRemoteBinary — user-initiated deploy request.
#[derive(Debug)]
pub enum DeployRemoteBinary {}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DeployRemoteBinaryParams {
    pub connection_key: String,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DeployRemoteBinaryResult {
    pub success: bool,
    /// The binary command/path now available on the remote.
    pub binary_path: Option<String>,
    pub message: String,
}

impl Request for DeployRemoteBinary {
    type Params = DeployRemoteBinaryParams;
    type Result = DeployRemoteBinaryResult;
    const METHOD: &'static str = "emacs/deployRemoteBinary";
}

// emacs/remoteDeployProgress — progress notifications streamed during deploy.
#[derive(Debug)]
pub enum RemoteDeployProgress {}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RemoteDeployProgressParams {
    pub connection_key: String,
    pub message: String,
}

impl Notification for RemoteDeployProgress {
    type Params = RemoteDeployProgressParams;
    const METHOD: &'static str = "emacs/remoteDeployProgress";
}

// emacs/forwardRequest
#[derive(Debug)]
pub enum ForwardRequest {}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ForwardRequestParams {
    pub uri: String,
    pub server_name: String,
    pub method: String,
    pub params: serde_json::Value,
}

impl Request for ForwardRequest {
    type Params = ForwardRequestParams;
    type Result = serde_json::Value;
    const METHOD: &'static str = "emacs/forwardRequest";
}

#[cfg(test)]
mod import_log_tests {
    use super::{ImportLog, ImportLogParams};
    use lsp_types::notification::Notification;

    /// The method the JetBrains server actually sends. Getting this wrong means the
    /// notification keeps being dropped as unhandled, which is the bug being fixed.
    #[test]
    fn method_matches_the_server() {
        assert_eq!(ImportLog::METHOD, "intellij/importLog");
    }

    /// A `started` event as the server sends it: no `tool` guarantee, no message.
    #[test]
    fn parses_started_event() {
        let p: ImportLogParams =
            serde_json::from_str(r#"{"type":3,"message":"","started":true,"tool":"Maven"}"#)
                .unwrap();
        assert!(p.started);
        assert!(!p.failed);
        assert!(!p.succeeded);
        assert_eq!(p.tool.as_deref(), Some("Maven"));
    }

    /// A plain progress line: only `type` and `message`, every flag absent. Absent
    /// flags must read as false rather than failing the parse.
    #[test]
    fn parses_bare_progress_line() {
        let p: ImportLogParams =
            serde_json::from_str(r#"{"type":3,"message":"Resolving dependencies"}"#).unwrap();
        assert_eq!(p.message, "Resolving dependencies");
        assert!(!p.started && !p.failed && !p.succeeded);
        assert_eq!(p.root_path, None);
    }

    /// A terminal failure must survive even a payload missing `message`; losing it
    /// would leave the user with no indication that the import broke.
    #[test]
    fn failure_survives_a_missing_message() {
        let p: ImportLogParams =
            serde_json::from_str(r#"{"failed":true,"tool":"Gradle"}"#).unwrap();
        assert!(p.failed);
        assert_eq!(p.message, "");
        assert_eq!(p.typ, None);
    }

    /// `rootPath` is the proxy's addition, so it must go out camelCase and must be
    /// omitted rather than sent as null when unset.
    #[test]
    fn root_path_is_added_on_the_way_out() {
        let mut p: ImportLogParams =
            serde_json::from_str(r#"{"type":1,"message":"boom","failed":true}"#).unwrap();
        let json = serde_json::to_value(&p).unwrap();
        assert!(
            json.get("rootPath").is_none(),
            "unset rootPath must be omitted, not null"
        );

        p.root_path = Some("/repo/initial".to_string());
        let json = serde_json::to_value(&p).unwrap();
        assert_eq!(json["rootPath"], "/repo/initial");
        // `type` is a reserved word in Rust; make sure the rename survives.
        assert_eq!(json["type"], 1);
        assert!(json.get("typ").is_none());
    }

    /// The dispatch that was returning `Unhandled` before.
    #[test]
    fn registry_routes_the_notification() {
        let serde_json::Value::Object(map) =
            serde_json::json!({"type": 3, "message": "Importing", "tool": "Maven"})
        else {
            unreachable!()
        };
        let params = crate::lsp::jsonrpc::Params::Map(map);
        match crate::registry::NotificationFromServer::parse(ImportLog::METHOD, params) {
            Ok(crate::registry::NotificationFromServer::ImportLog(p)) => {
                assert_eq!(p.message, "Importing");
            }
            other => panic!("expected ImportLog, got {other:?}"),
        }
    }
}

#[cfg(test)]
mod server_capabilities_tests {
    use super::CustomServerCapabilitiesParams;

    fn params() -> CustomServerCapabilitiesParams {
        CustomServerCapabilitiesParams {
            uri: "file:///w/src/A.java".to_string(),
            trigger_characters: vec![],
            support_inlay_hints: false,
            support_document_highlight: false,
            support_document_symbols: false,
            support_signature_help: false,
            support_pull_diagnostic: false,
            support_inline_completion: false,
            text_document_sync_kind: "incremental".to_string(),
            support_hover: false,
            has_any_servers: true,
            workspace_roots: vec!["/w/initial".to_string(), "/w/complete".to_string()],
        }
    }

    /// The key on the wire must be `workspaceRoots`: the Emacs handler destructures
    /// by that name, so a snake_case key would silently bind nil and the mode-line
    /// would go back to showing nothing.
    #[test]
    fn serializes_roots_as_camel_case() {
        let json = serde_json::to_value(params()).unwrap();
        assert_eq!(
            json.get("workspaceRoots").and_then(|v| v.as_array()).map(|a| a.len()),
            Some(2),
            "actual keys: {:?}",
            json.as_object().unwrap().keys().collect::<Vec<_>>()
        );
        assert!(json.get("workspace_roots").is_none());
    }

    /// Order is preserved, so the editor tries the servers in activation order.
    #[test]
    fn round_trips() {
        let json = serde_json::to_string(&params()).unwrap();
        let back: CustomServerCapabilitiesParams = serde_json::from_str(&json).unwrap();
        assert_eq!(back.workspace_roots, vec!["/w/initial", "/w/complete"]);
    }
}

#[cfg(test)]
mod install_java_server_tests {
    use super::InstallJavaServerParams;

    /// The client omits `version` entirely rather than sending JSON null: its
    /// jsonrpc connection serializes with `:null-object nil`, so a `:null` keyword
    /// is not a valid value there. `#[serde(default)]` is what makes the absent key
    /// mean "latest".
    #[test]
    fn version_may_be_absent() {
        let json = r#"{"installDir":"/tmp/i/","force":false}"#;
        let params: InstallJavaServerParams = serde_json::from_str(json).unwrap();
        assert_eq!(params.version, None);
        assert!(!params.force);
    }

    #[test]
    fn version_may_be_present_or_explicitly_null() {
        let pinned = r#"{"installDir":"/tmp/i/","version":"263.2689.0","force":true}"#;
        let params: InstallJavaServerParams = serde_json::from_str(pinned).unwrap();
        assert_eq!(params.version.as_deref(), Some("263.2689.0"));
        assert!(params.force);

        // Tolerate an explicit null too, so a future client need not special-case it.
        let nulled = r#"{"installDir":"/tmp/i/","version":null,"force":false}"#;
        let params: InstallJavaServerParams = serde_json::from_str(nulled).unwrap();
        assert_eq!(params.version, None);
    }

    /// The full wire shape, end to end.
    ///
    /// The proxy wraps requests in a `{uri, context, params}` envelope and the
    /// payload lives in the nested `params`. Deserializing the payload struct
    /// alone hides that: a flat payload still parses in isolation while failing
    /// for real with "invalid type: null". So this parses the envelope exactly as
    /// it arrives on stdin, then the payload out of it.
    #[test]
    fn parses_the_real_request_envelope() {
        let wire = r#"{
          "id": 3,
          "method": "emacs/installJavaServer",
          "params": {
            "params": {
              "installDir": "/Users/u/.emacs.d/.local/cache/lsp-proxy/servers/intellij/",
              "force": false
            }
          }
        }"#;
        let req: crate::msg::Request = serde_json::from_str(wire).unwrap();
        let params: InstallJavaServerParams = serde_json::from_value(req.params.params).unwrap();
        assert_eq!(params.version, None);
        assert!(!params.force);
        assert!(params.install_dir.ends_with("/servers/intellij/"));
    }

    /// A payload flattened into the envelope (the bug) must be recognisable rather
    /// than silently arriving as defaults.
    #[test]
    fn flat_payload_is_rejected() {
        let wire = r#"{
          "id": 3,
          "method": "emacs/installJavaServer",
          "params": { "installDir": "/tmp/i/", "force": false }
        }"#;
        let req: crate::msg::Request = serde_json::from_str(wire).unwrap();
        assert!(
            req.params.params.is_null(),
            "a flat payload leaves the nested params null"
        );
        assert!(serde_json::from_value::<InstallJavaServerParams>(req.params.params).is_err());
    }

    /// Only `installDir` is genuinely required; the rest default.
    #[test]
    fn only_install_dir_is_required() {
        let params: InstallJavaServerParams =
            serde_json::from_str(r#"{"installDir":"/tmp/i/"}"#).unwrap();
        assert_eq!(params.install_dir, "/tmp/i/");
        assert_eq!(params.version, None);
        assert!(!params.force);
    }
}
