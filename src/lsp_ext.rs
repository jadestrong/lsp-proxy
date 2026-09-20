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
    pub support_code_lens: bool,
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
// Downloads a Java language server backend — the JetBrains IntelliJ server or
// Eclipse's jdtls — for `lsp-proxy-java`. Lives here rather than in Emacs Lisp
// because the proxy already has tokio (so a download of tens to hundreds of MB does
// not block the editor) and an established progress pipeline; the Emacs side is only
// a thin command.
//
// The method name is backend-neutral and predates the `backend` field, so that field
// defaults rather than being required: a client built before it still asks for, and
// still gets, the IntelliJ server.
//
// The *proxy's own* installer stays in Emacs Lisp on purpose — that one cannot
// run here, since the proxy does not exist yet when it runs.
#[derive(Debug)]
pub enum InstallJavaServer {}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InstallJavaServerParams {
    /// Where to place `<version>/bin/<launcher>`. Chosen by the client so the
    /// location stays next to everything else Emacs manages.
    pub install_dir: String,
    /// Which server to install. Absent means IntelliJ, for the reason above.
    #[serde(default)]
    pub backend: crate::java_install::Backend,
    /// Pin a build (e.g. "263.2689.0" or jdtls's "1.61.0"); `None` resolves the
    /// latest for the selected backend.
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

// emacs/installLombok
//
// Downloads the Lombok java agent that `jdtls` needs in order to see Lombok-generated
// members. Deliberately NOT a variant of `installJavaServer`'s `backend`: Lombok is
// not a language server, and widening that enum to hold one would make the abstraction
// lie about what a backend is.
//
// jdtls ships no Lombok and has no setting for it — `java.jdt.ls.lombokSupport.enabled`
// belongs to the VS Code Java extension, which consumes it client-side and appends
// `-javaagent:` to the server's command line. This is that half, moved here because
// the proxy is what launches the server.
#[derive(Debug)]
pub enum InstallLombok {}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InstallLombokParams {
    /// Where to place `lombok-<version>.jar`. Chosen by the client so the location
    /// stays next to everything else Emacs manages.
    pub install_dir: String,
    /// Re-download even when the pinned version is already present.
    #[serde(default)]
    pub force: bool,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct InstallLombokResult {
    pub jar_path: String,
    pub version: String,
    /// True when the pinned version was already present and nothing was downloaded.
    pub already_installed: bool,
}

impl Request for InstallLombok {
    type Params = InstallLombokParams;
    type Result = InstallLombokResult;
    const METHOD: &'static str = "emacs/installLombok";
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

// intellij/chooseAction  →  emacs/chooseAction  →  workspace/executeCommand
//
// The IntelliJ server asks the user to pick one of several ModCommand actions. It
// arrives as a *notification*, and the answer goes back as a fresh
// `workspace/executeCommand` request rather than as a reply — so the session id has
// to be held across the editor round trip.
//
// Note this is not `window/showMessageRequest`: that one is a request the proxy
// replies to. Here the server expects a command invocation, which is why the two
// cannot share a code path.
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ChooseActionEntry {
    /// Index the server identifies this entry by; not necessarily its position.
    pub index: i64,
    pub name: String,
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ChooseActionParams {
    pub session_id: i64,
    #[serde(default)]
    pub title: String,
    #[serde(default)]
    pub entries: Vec<ChooseActionEntry>,
}

#[derive(Debug)]
pub enum ChooseAction {}

impl Notification for ChooseAction {
    type Params = ChooseActionParams;
    const METHOD: &'static str = "intellij/chooseAction";
}

/// The editor-facing half. Result is the chosen `index`, or `None` when the user
/// dismissed the prompt — in which case nothing is sent back to the server.
#[derive(Debug)]
pub enum EmacsChooseAction {}

impl Request for EmacsChooseAction {
    type Params = ChooseActionParams;
    type Result = Option<i64>;
    const METHOD: &'static str = "emacs/chooseAction";
}

/// Server-side command the picked entry is delivered through.
pub const CHOOSE_MOD_COMMAND_ACTION: &str = "chooseModCommandAction";

// intellij/runEditorCommand
//
// The IntelliJ server asks the editor to run one of *its own* commands, as if the
// user had invoked it. Some ModCommands drive the editor UI rather than the
// document — starting an inline rename is the motivating case — and LSP cannot
// express that: a server may not send `workspace/executeCommand` to a client, and
// it cannot start such a session itself.
//
// The command ids are VS Code's (`editor.action.rename`), so the mapping onto
// Emacs commands is the editor's business; the proxy only relays. Unknown ids are
// dropped on the Emacs side rather than here, so that adding a mapping needs no
// new proxy build.
#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct RunEditorCommandParams {
    pub command: String,
    /// Positional arguments, verbatim. Left as raw JSON because their shape is
    /// per-command and the proxy never inspects them.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub arguments: Option<Vec<serde_json::Value>>,
}

#[derive(Debug)]
pub enum RunEditorCommand {}

impl Notification for RunEditorCommand {
    type Params = RunEditorCommandParams;
    const METHOD: &'static str = "intellij/runEditorCommand";
}

// language/status
//
// jdtls's own project-status channel. Cannot reuse `window/logMessage`: this is a
// *status* stream about one discrete operation (the project import) whose outcome
// the user has to act on — until it finishes nothing resolves, and the reason it
// failed is only ever in these lines. That is the same contract `intellij/importLog`
// already serves, which is why the proxy translates onto that channel rather than
// teaching the editor a second one.
//
// The method name is jdtls's own (`org.eclipse.jdt.ls` sends `language/status` with
// a `ServiceStatus` name in `type`); it is not a standard LSP method and has no
// standard equivalent.
#[derive(Debug)]
pub enum LanguageStatus {}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub struct LanguageStatusParams {
    /// A `ServiceStatus` *name*, not a severity: "Starting", "Started", "Error",
    /// "Message", "ServiceReady", "ProjectStatus". Only `Error`/`Warning` carry any
    /// severity information; the rest are lifecycle labels.
    ///
    /// Optional in practice — a payload missing `type` must still deliver its
    /// message rather than failing the parse, for the same reason `ImportLogParams`
    /// tolerates a missing one.
    #[serde(rename = "type", default)]
    pub typ: String,
    #[serde(default)]
    pub message: String,
}

impl Notification for LanguageStatus {
    type Params = LanguageStatusParams;
    const METHOD: &'static str = "language/status";
}

// language/progressReport
//
// jdtls's own progress channel, predating (and independent of) `$/progress`: it is
// gated on `extendedClientCapabilities.progressReportProvider` rather than on
// `window.workDoneProgress`, carries its own `id` instead of a
// `window/workDoneProgress/create` token, and reports `workDone`/`totalWork` as
// plain counters rather than a percentage. A standard `$/progress` payload cannot
// express it, which is why it needs its own type; the proxy translates it *onto*
// `$/progress` so the editor keeps one progress renderer.
#[derive(Debug)]
pub enum LanguageProgressReport {}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct LanguageProgressReportParams {
    /// jdtls's own token for this operation. Reused verbatim as the `$/progress`
    /// token so successive reports coalesce onto one mode-line entry.
    #[serde(default)]
    pub id: String,
    #[serde(default)]
    pub task: String,
    #[serde(default)]
    pub sub_task: String,
    /// The line jdtls means for display, e.g. "37% Starting Java Language Server".
    #[serde(default)]
    pub status: String,
    #[serde(default)]
    pub total_work: u32,
    #[serde(default)]
    pub work_done: u32,
    #[serde(default)]
    pub complete: bool,
}

impl Notification for LanguageProgressReport {
    type Params = LanguageProgressReportParams;
    const METHOD: &'static str = "language/progressReport";
}

/// Translate a jdtls `language/status` into the `intellij/importLog` payload.
///
/// Pure: `root_path` is left `None` here and stamped by the caller from the
/// language server's own root, exactly as the IntelliJ path does.
///
/// Every field `ImportLogParams` has that `language/status` does not is decided
/// here, deliberately and once (requirement FR4.3). The three booleans are the
/// interesting ones, and all three are hard-wired `false`:
///
///  * `started`/`succeeded`/`failed` are *terminal* markers in the IntelliJ
///    semantics — the editor renders `started` as a bare "<tool> import started"
///    line that discards the message, and `failed` both steals a window
///    (`lsp-proxy-java-import-log-auto-display`) and warns in the echo area.
///  * jdtls has no terminal import event. `Starting`/`Started`/`ServiceReady`
///    describe the *server's* lifecycle, not a build-tool import, and `Error` is a
///    severity that arrives repeatedly during one import — mapping it onto `failed`
///    would fire the warning and reveal the window several times per import.
///
/// What is given up by that choice: an import that goes wrong no longer reveals the
/// log buffer by itself. The compensation is `typ`, which carries `Error` through as
/// `MessageType::ERROR` so the line renders in the error face, and the buffer stays
/// one `M-x lsp-proxy-java-open-import-log` away.
///
/// `tool` stays `None` because jdtls never says which build tool is running. Filling
/// in "jdtls" would be a lie with visible consequences: the editor renders `tool` as
/// the *build tool* name ("Maven import finished"), and `None` already falls back to
/// "Build".
pub fn language_status_to_import_log(params: &LanguageStatusParams) -> ImportLogParams {
    ImportLogParams {
        typ: Some(message_type_for_status(&params.typ)),
        // An empty message would render as a blank line, indistinguishable from a
        // rendering bug; the status name is the only other thing the payload has.
        message: if params.message.is_empty() {
            params.typ.clone()
        } else {
            params.message.clone()
        },
        tool: None,
        failed: false,
        succeeded: false,
        started: false,
        root_path: None,
    }
}

/// `lsp_types::MessageType` for a jdtls `ServiceStatus` name: 1 = error,
/// 2 = warning, 3 = info.
///
/// Anything that is not explicitly an error or a warning is info — including the
/// lifecycle names and an absent `type`, neither of which says anything about
/// severity.
fn message_type_for_status(status: &str) -> u8 {
    match status {
        "Error" => 1,
        "Warning" => 2,
        _ => 3,
    }
}

/// Translate a jdtls `language/progressReport` into a standard `$/progress` payload.
///
/// Pure: the caller wraps the result in `CustomProgressParams` with the server root.
///
/// Deliberately stateless — `Report` while running, `End` on completion, and never a
/// synthesized `Begin`. Synthesizing one would mean holding a per-token set in the
/// proxy for the sole benefit of a `title` field the editor never reaches: its
/// renderer prefers `message` over `title`, and it treats `begin` and `report`
/// identically (`lsp-proxy-core.el`, the `$/progress` pcase). The state would buy
/// nothing and would have to be invalidated on server restart.
pub fn progress_report_to_progress(
    params: &LanguageProgressReportParams,
) -> lsp_types::ProgressParams {
    use lsp_types::{
        NumberOrString, ProgressParamsValue, WorkDoneProgress, WorkDoneProgressEnd,
        WorkDoneProgressReport,
    };

    let message = progress_message(params);
    let value = if params.complete {
        WorkDoneProgress::End(WorkDoneProgressEnd { message })
    } else {
        WorkDoneProgress::Report(WorkDoneProgressReport {
            cancellable: None,
            message,
            percentage: progress_percentage(params),
        })
    };

    lsp_types::ProgressParams {
        token: NumberOrString::String(params.id.clone()),
        value: ProgressParamsValue::WorkDone(value),
    }
}

/// The line to show for a progress report.
///
/// `status` is what jdtls composes for display and is preferred; `subTask` then
/// `task` are the fallbacks for builds that fill only those. `None` rather than an
/// empty string when all three are blank, so the renderer falls through to its own
/// default instead of printing nothing.
fn progress_message(params: &LanguageProgressReportParams) -> Option<String> {
    [&params.status, &params.sub_task, &params.task]
        .into_iter()
        .find(|s| !s.is_empty())
        .cloned()
}

/// Completion percentage, or `None` when the operation has no measurable size.
///
/// `total_work` of 0 means "unknown", which is the common case early in an import —
/// it must not divide, and it must not report 0% (which reads as "stuck at zero"
/// rather than "size unknown"). The result is clamped because jdtls has been seen
/// to report `workDone` past `totalWork` when a task grows mid-flight.
fn progress_percentage(params: &LanguageProgressReportParams) -> Option<u32> {
    if params.total_work == 0 {
        return None;
    }
    Some(((params.work_done as u64 * 100) / params.total_work as u64).min(100) as u32)
}

// emacs/serverStartFailure
//
// A language server that `languages.toml` configures for a document but that could
// not be started. Without this the failure is only an `error!` line in the proxy log
// — invisible at the default log level — and the editor, seeing zero attached
// servers, tells the user "No language server config found for this file", which is
// the opposite of what happened.
//
// Structured rather than a `window/showMessage` string so the editor can attach an
// install hint keyed on the server name; the proxy has no business knowing where any
// particular server is distributed from.
#[derive(Debug)]
pub enum ServerStartFailure {}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ServerStartFailureParams {
    /// The `languages.toml` server name, e.g. "jdtls".
    pub name: String,
    /// The configured `command`, which is what was looked up on PATH.
    pub command: String,
    /// True when the command simply is not on PATH, as opposed to any other
    /// start-up failure. The editor phrases the two differently: only the first one
    /// has "install it" as the next step.
    pub not_on_path: bool,
    /// The underlying error, for the cases `not_on_path` does not cover.
    pub message: String,
}

impl Notification for ServerStartFailure {
    type Params = ServerStartFailureParams;
    const METHOD: &'static str = "emacs/serverStartFailure";
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
    pub id: usize,
    pub name: String,
    pub root_path: String,
    pub support_workspace: syntax::SupportWorkspace,
}

impl Request for GetWorkspaceInfo {
    type Params = ();
    type Result = Option<WorkspaceInfo>;
    const METHOD: &'static str = "emacs/getWorkspaceInfo";
}

// emacs/listWorkspaces
#[derive(Debug)]
pub enum ListWorkspaces {}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WorkspaceListItem {
    pub workspace_root: String,
    pub document_count: usize,
    pub language_servers: Vec<WorkspaceLanguageServerInfo>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WorkspaceLanguageServerInfo {
    pub id: usize,
    pub name: String,
    pub root_path: String,
    pub shared: bool,
    pub workspace_count: usize,
}

impl Request for ListWorkspaces {
    type Params = ();
    type Result = Vec<WorkspaceListItem>;
    const METHOD: &'static str = "emacs/listWorkspaces";
}

// emacs/shutdownWorkspace
#[derive(Debug)]
pub enum ShutdownWorkspace {}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ShutdownWorkspaceParams {
    pub workspace_root: String,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ShutdownWorkspaceResult {
    pub workspace_root: String,
    pub closed_documents: Vec<String>,
    pub shutdown_servers: Vec<WorkspaceLanguageServerInfo>,
    pub preserved_servers: Vec<WorkspaceLanguageServerInfo>,
}

impl Request for ShutdownWorkspace {
    type Params = ShutdownWorkspaceParams;
    type Result = ShutdownWorkspaceResult;
    const METHOD: &'static str = "emacs/shutdownWorkspace";
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
    /// Send `method` as a *notification* rather than a request.
    ///
    /// Needed because a server's extension surface is not all requests: jdtls
    /// declares `java/projectConfigurationUpdate` as a notification, and sending it
    /// as a request means waiting out the full request timeout for a reply that is
    /// never coming.
    ///
    /// A flag on this channel rather than a second channel: the editor's problem is
    /// "deliver this method to that server", and whether the method happens to be
    /// request-shaped is a property of the method, not a different kind of routing.
    /// `#[serde(default)]` keeps every existing caller — which sends no such key —
    /// on exactly the behaviour it has today.
    #[serde(default)]
    pub notify: bool,
}

impl Request for ForwardRequest {
    type Params = ForwardRequestParams;
    type Result = serde_json::Value;
    const METHOD: &'static str = "emacs/forwardRequest";
}

#[cfg(test)]
mod choose_action_tests {
    use super::{
        ChooseAction, ChooseActionParams, EmacsChooseAction, RunEditorCommand,
        RunEditorCommandParams, CHOOSE_MOD_COMMAND_ACTION,
    };
    use lsp_types::notification::Notification;
    use lsp_types::request::Request;

    #[test]
    fn method_names_match_the_protocol() {
        assert_eq!(ChooseAction::METHOD, "intellij/chooseAction");
        assert_eq!(EmacsChooseAction::METHOD, "emacs/chooseAction");
        assert_eq!(CHOOSE_MOD_COMMAND_ACTION, "chooseModCommandAction");
        assert_eq!(RunEditorCommand::METHOD, "intellij/runEditorCommand");
    }

    /// `arguments` is optional, and absent is not the same as empty: a command
    /// invoked with no arguments at all must not grow a `[]` on the way through.
    #[test]
    fn parses_run_editor_command_with_and_without_arguments() {
        let bare: RunEditorCommandParams =
            serde_json::from_str(r#"{"command":"editor.action.rename"}"#).unwrap();
        assert_eq!(bare.command, "editor.action.rename");
        assert_eq!(bare.arguments, None);

        let with_args: RunEditorCommandParams =
            serde_json::from_str(r#"{"command":"some.command","arguments":[1,"x"]}"#).unwrap();
        assert_eq!(
            with_args.arguments,
            Some(vec![serde_json::json!(1), serde_json::json!("x")])
        );
    }

    /// The payload as the server sends it.
    #[test]
    fn parses_the_server_payload() {
        let json = r#"{
          "sessionId": 42,
          "title": "Choose an action",
          "entries": [{"index": 0, "name": "Introduce variable"},
                      {"index": 1, "name": "Introduce constant"}]
        }"#;
        let p: ChooseActionParams = serde_json::from_str(json).unwrap();
        assert_eq!(p.session_id, 42);
        assert_eq!(p.title, "Choose an action");
        assert_eq!(p.entries.len(), 2);
        // Index 0 is a legitimate answer, not "absent".
        assert_eq!(p.entries[0].index, 0);
        assert_eq!(p.entries[1].name, "Introduce constant");
    }

    /// Forwarded to the editor with camelCase keys, since the Emacs handler
    /// destructures `:entries` and each entry's `:index` / `:name`.
    #[test]
    fn forwards_as_camel_case() {
        let p: ChooseActionParams =
            serde_json::from_str(r#"{"sessionId":7,"entries":[{"index":3,"name":"x"}]}"#).unwrap();
        let json = serde_json::to_value(&p).unwrap();
        assert_eq!(json["sessionId"], 7);
        assert!(json.get("session_id").is_none());
        assert_eq!(json["entries"][0]["index"], 3);
    }

    /// Missing title/entries must not fail the parse and drop the notification.
    #[test]
    fn tolerates_a_minimal_payload() {
        let p: ChooseActionParams = serde_json::from_str(r#"{"sessionId":1}"#).unwrap();
        assert_eq!(p.title, "");
        assert!(p.entries.is_empty());
    }

    #[test]
    fn registry_routes_the_notification() {
        let serde_json::Value::Object(map) = serde_json::json!({
            "sessionId": 5,
            "entries": [{"index": 0, "name": "a"}]
        }) else {
            unreachable!()
        };
        let params = crate::lsp::jsonrpc::Params::Map(map);
        match crate::registry::NotificationFromServer::parse(ChooseAction::METHOD, params) {
            Ok(crate::registry::NotificationFromServer::ChooseAction(p)) => {
                assert_eq!(p.session_id, 5);
            }
            other => panic!("expected ChooseAction, got {other:?}"),
        }
    }

    /// The editor answers with the index or null; null means dismissed and must
    /// deserialize rather than error, because the proxy then sends nothing.
    #[test]
    fn editor_result_accepts_index_or_null() {
        let picked: Option<i64> = serde_json::from_str("3").unwrap();
        assert_eq!(picked, Some(3));
        let zero: Option<i64> = serde_json::from_str("0").unwrap();
        assert_eq!(zero, Some(0), "index 0 must survive as a real choice");
        let dismissed: Option<i64> = serde_json::from_str("null").unwrap();
        assert_eq!(dismissed, None);
    }
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
            support_code_lens: false,
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

    /// The `backend` field was added after the method shipped, so an older client
    /// that omits it must still get the IntelliJ server rather than an error or a
    /// silently different backend.
    #[test]
    fn backend_defaults_to_intellij_on_the_wire() {
        use crate::java_install::Backend;

        let without = r#"{"installDir":"/tmp/i/","force":false}"#;
        let params: InstallJavaServerParams = serde_json::from_str(without).unwrap();
        assert_eq!(params.backend, Backend::Intellij);

        let with = r#"{"installDir":"/tmp/j/","backend":"jdtls","version":"1.61.0","force":false}"#;
        let params: InstallJavaServerParams = serde_json::from_str(with).unwrap();
        assert_eq!(params.backend, Backend::Jdtls);
        assert_eq!(params.version.as_deref(), Some("1.61.0"));

        // An unknown backend must fail loudly rather than fall back to the default.
        let bogus = r#"{"installDir":"/tmp/j/","backend":"eclipse","force":false}"#;
        assert!(serde_json::from_str::<InstallJavaServerParams>(bogus).is_err());
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

#[cfg(test)]
mod jdtls_notification_tests {
    use super::{
        language_status_to_import_log, progress_report_to_progress, LanguageProgressReport,
        LanguageProgressReportParams, LanguageStatus, LanguageStatusParams, ServerStartFailure,
        ServerStartFailureParams,
    };
    use lsp_types::notification::Notification;

    fn report(params: serde_json::Value) -> LanguageProgressReportParams {
        serde_json::from_value(params).unwrap()
    }

    fn work_done(params: &lsp_types::ProgressParams) -> &lsp_types::WorkDoneProgress {
        match &params.value {
            lsp_types::ProgressParamsValue::WorkDone(v) => v,
        }
    }

    /// The methods jdtls actually sends. Getting either wrong means the notification
    /// goes back to being dropped as unhandled, which is the whole point of the
    /// whitelist entries.
    #[test]
    fn method_names_match_the_server() {
        assert_eq!(LanguageStatus::METHOD, "language/status");
        assert_eq!(LanguageProgressReport::METHOD, "language/progressReport");
        assert_eq!(ServerStartFailure::METHOD, "emacs/serverStartFailure");
    }

    /// A full payload as jdtls sends it.
    #[test]
    fn parses_a_complete_status() {
        let p: LanguageStatusParams =
            serde_json::from_str(r#"{"type":"Started","message":"Ready"}"#).unwrap();
        assert_eq!(p.typ, "Started");
        assert_eq!(p.message, "Ready");
    }

    /// The minimal payload: only the status name. An absent `message` must read as
    /// empty rather than failing the parse.
    #[test]
    fn parses_a_status_without_a_message() {
        let p: LanguageStatusParams = serde_json::from_str(r#"{"type":"ServiceReady"}"#).unwrap();
        assert_eq!(p.typ, "ServiceReady");
        assert_eq!(p.message, "");
    }

    /// Losing a message because `type` was missing would be the worst outcome here:
    /// the text is the only part the user reads.
    #[test]
    fn status_survives_a_missing_type() {
        let p: LanguageStatusParams = serde_json::from_str(r#"{"message":"boom"}"#).unwrap();
        assert_eq!(p.typ, "");
        assert_eq!(p.message, "boom");
    }

    /// `type` is a reserved word in Rust; make sure the rename survives a round trip
    /// in both directions.
    #[test]
    fn status_round_trips_through_the_wire_name() {
        let p = LanguageStatusParams {
            typ: "Error".to_string(),
            message: "x".to_string(),
        };
        let json = serde_json::to_value(&p).unwrap();
        assert_eq!(json["type"], "Error");
        assert!(json.get("typ").is_none());
        assert_eq!(serde_json::from_value::<LanguageStatusParams>(json).unwrap(), p);
    }

    /// Only `Error` and `Warning` say anything about severity; every lifecycle name
    /// is info. Covers each `type` value jdtls is known to send.
    #[test]
    fn status_type_maps_onto_a_message_severity() {
        let severity = |t: &str| {
            language_status_to_import_log(&LanguageStatusParams {
                typ: t.to_string(),
                message: "m".to_string(),
            })
            .typ
        };
        assert_eq!(severity("Error"), Some(1));
        assert_eq!(severity("Warning"), Some(2));
        assert_eq!(severity("Starting"), Some(3));
        assert_eq!(severity("Started"), Some(3));
        assert_eq!(severity("Message"), Some(3));
        assert_eq!(severity("ServiceReady"), Some(3));
        assert_eq!(severity("ProjectStatus"), Some(3));
        assert_eq!(severity(""), Some(3), "an absent type is not a severity");
    }

    /// Requirement FR4.3: every `ImportLogParams` field jdtls does not supply has a
    /// decided value, and the decision is that none of the terminal markers fire.
    /// Asserted one field at a time so a future change has to face each one.
    #[test]
    fn missing_import_log_fields_take_their_decided_values() {
        let out = language_status_to_import_log(&LanguageStatusParams {
            typ: "Error".to_string(),
            message: "Error occurred while building".to_string(),
        });
        assert_eq!(out.tool, None, "jdtls never names the build tool");
        assert!(
            !out.failed,
            "an Error status is a severity, not a terminal import failure"
        );
        assert!(!out.succeeded, "jdtls has no terminal import-succeeded event");
        assert!(
            !out.started,
            "a started line discards the message, which is the only content here"
        );
        assert_eq!(
            out.root_path, None,
            "root_path is stamped by the caller, not by the translation"
        );
        assert_eq!(out.message, "Error occurred while building");
    }

    /// A blank line in the import log is indistinguishable from a rendering bug, so
    /// an empty message falls back to the status name.
    #[test]
    fn an_empty_status_message_falls_back_to_the_status_name() {
        let out = language_status_to_import_log(&LanguageStatusParams {
            typ: "ServiceReady".to_string(),
            message: String::new(),
        });
        assert_eq!(out.message, "ServiceReady");
    }

    /// The token has to be jdtls's own id, or successive reports would each open
    /// their own mode-line entry instead of coalescing.
    #[test]
    fn progress_reuses_the_servers_own_id_as_the_token() {
        let out = progress_report_to_progress(&report(
            serde_json::json!({"id":"import-1","status":"Importing","totalWork":10,"workDone":3}),
        ));
        assert_eq!(
            out.token,
            lsp_types::NumberOrString::String("import-1".to_string())
        );
    }

    /// An in-flight report carries the message and a percentage.
    #[test]
    fn an_incomplete_report_becomes_a_progress_report() {
        let out = progress_report_to_progress(&report(
            serde_json::json!({"id":"t","status":"Importing projects","totalWork":8,"workDone":2}),
        ));
        match work_done(&out) {
            lsp_types::WorkDoneProgress::Report(r) => {
                assert_eq!(r.message.as_deref(), Some("Importing projects"));
                assert_eq!(r.percentage, Some(25));
            }
            other => panic!("expected Report, got {other:?}"),
        }
    }

    /// Completion must end the token, or the mode-line indicator stays up for the
    /// rest of the session.
    #[test]
    fn a_complete_report_ends_the_token() {
        let out = progress_report_to_progress(&report(
            serde_json::json!({"id":"t","status":"Done","totalWork":8,"workDone":8,"complete":true}),
        ));
        match work_done(&out) {
            lsp_types::WorkDoneProgress::End(e) => assert_eq!(e.message.as_deref(), Some("Done")),
            other => panic!("expected End, got {other:?}"),
        }
    }

    /// `totalWork` is 0 early in an import, when the size is not known yet. It must
    /// not divide, and it must not claim 0% — which reads as "stuck", not "unknown".
    #[test]
    fn unknown_total_work_yields_no_percentage() {
        let out = progress_report_to_progress(&report(
            serde_json::json!({"id":"t","status":"Starting","totalWork":0,"workDone":0}),
        ));
        match work_done(&out) {
            lsp_types::WorkDoneProgress::Report(r) => assert_eq!(r.percentage, None),
            other => panic!("expected Report, got {other:?}"),
        }
    }

    /// jdtls has been seen to report more work done than the total when a task grows
    /// mid-flight; "137%" in the mode line looks like a bug.
    #[test]
    fn percentage_is_clamped_at_one_hundred() {
        let out = progress_report_to_progress(&report(
            serde_json::json!({"id":"t","totalWork":10,"workDone":137}),
        ));
        match work_done(&out) {
            lsp_types::WorkDoneProgress::Report(r) => assert_eq!(r.percentage, Some(100)),
            other => panic!("expected Report, got {other:?}"),
        }
    }

    /// `status` is what jdtls composes for display; `subTask` and `task` are the
    /// fallbacks. All three blank yields None so the renderer uses its own default
    /// rather than printing an empty line.
    #[test]
    fn progress_message_prefers_status_then_sub_task_then_task() {
        let msg = |v: serde_json::Value| match work_done(&progress_report_to_progress(&report(v))) {
            lsp_types::WorkDoneProgress::Report(r) => r.message.clone(),
            other => panic!("expected Report, got {other:?}"),
        };
        assert_eq!(
            msg(serde_json::json!({"id":"t","status":"S","subTask":"U","task":"T"})),
            Some("S".to_string())
        );
        assert_eq!(
            msg(serde_json::json!({"id":"t","subTask":"U","task":"T"})),
            Some("U".to_string())
        );
        assert_eq!(
            msg(serde_json::json!({"id":"t","task":"T"})),
            Some("T".to_string())
        );
        assert_eq!(msg(serde_json::json!({"id":"t"})), None);
    }

    /// The bare payload: every field absent. jdtls omits the counters entirely for
    /// indeterminate work, and a strict parse here would drop the whole report.
    #[test]
    fn parses_a_bare_progress_report() {
        let p = report(serde_json::json!({}));
        assert_eq!(p.id, "");
        assert_eq!(p.total_work, 0);
        assert!(!p.complete);
    }

    /// The editor destructures `notOnPath` by that name, so a snake_case key would
    /// silently bind nil and the message would lose its "install it" branch.
    #[test]
    fn start_failure_serializes_as_camel_case() {
        let json = serde_json::to_value(ServerStartFailureParams {
            name: "jdtls".to_string(),
            command: "jdtls".to_string(),
            not_on_path: true,
            message: "cannot find binary path".to_string(),
        })
        .unwrap();
        assert_eq!(json["notOnPath"], true);
        assert_eq!(json["name"], "jdtls");
        assert!(json.get("not_on_path").is_none());
    }

    /// Every caller that predates the flag sends no `notify` key, and must keep
    /// getting a request. A default of `true` would silently turn every forwarded
    /// request into a notification and lose all their replies.
    #[test]
    fn forward_request_defaults_to_a_request_not_a_notification() {
        let p: super::ForwardRequestParams = serde_json::from_str(
            r#"{"uri":"file:///a/B.java","serverName":"copilot","method":"signIn","params":{}}"#,
        )
        .unwrap();
        assert!(!p.notify);
    }

    /// The editor spells the flag `notify`; a rename would silently fall back to the
    /// default and reintroduce the timeout this flag exists to avoid.
    #[test]
    fn forward_request_reads_the_notify_flag() {
        let p: super::ForwardRequestParams = serde_json::from_str(
            r#"{"uri":"file:///a/B.java","serverName":"jdtls","method":"java/projectConfigurationUpdate","params":{},"notify":true}"#,
        )
        .unwrap();
        assert!(p.notify);
        assert_eq!(p.method, "java/projectConfigurationUpdate");
    }

    /// The dispatch that would otherwise return `Unhandled`, for both methods.
    #[test]
    fn registry_routes_the_notifications() {
        let params = |v: serde_json::Value| {
            let serde_json::Value::Object(map) = v else {
                unreachable!()
            };
            crate::lsp::jsonrpc::Params::Map(map)
        };

        match crate::registry::NotificationFromServer::parse(
            LanguageStatus::METHOD,
            params(serde_json::json!({"type":"Started","message":"Ready"})),
        ) {
            Ok(crate::registry::NotificationFromServer::LanguageStatus(p)) => {
                assert_eq!(p.message, "Ready");
            }
            other => panic!("expected LanguageStatus, got {other:?}"),
        }

        match crate::registry::NotificationFromServer::parse(
            LanguageProgressReport::METHOD,
            params(serde_json::json!({"id":"t","status":"Importing","complete":false})),
        ) {
            Ok(crate::registry::NotificationFromServer::LanguageProgressReport(p)) => {
                assert_eq!(p.status, "Importing");
            }
            other => panic!("expected LanguageProgressReport, got {other:?}"),
        }
    }

    /// A malformed payload must NOT come back as `Unhandled`. The main loop
    /// branches on exactly that variant: `Unhandled` is an `info!` and everything
    /// else is an `error!`, so collapsing the two would make a renamed field
    /// indistinguishable from a method the proxy does not support — and silent at
    /// the default log level.
    ///
    /// Note the concrete variant is `Rpc`, not `Parse`: `Params::parse` wraps serde
    /// failures as `jsonrpc::Error::invalid_params` before they reach this enum. That
    /// is pre-existing behaviour shared with every other whitelisted notification;
    /// what the main loop depends on is the `Unhandled`/not-`Unhandled` split, so
    /// that is what is asserted.
    #[test]
    fn a_malformed_payload_is_not_reported_as_unhandled() {
        let serde_json::Value::Object(map) = serde_json::json!({"type": 3}) else {
            unreachable!()
        };
        let err = crate::registry::NotificationFromServer::parse(
            LanguageStatus::METHOD,
            crate::lsp::jsonrpc::Params::Map(map),
        )
        .expect_err("a numeric `type` is not a ServiceStatus name");
        assert!(
            !matches!(err, crate::registry::Error::Unhandled),
            "a malformed payload must not masquerade as an unsupported method, got {err:?}"
        );
        assert!(
            matches!(err, crate::registry::Error::Rpc(_)),
            "expected Rpc (Params::parse wraps serde errors as invalid_params), got {err:?}"
        );
    }
}

/// Probe for assumption A4: the official `bin/jdtls` launcher derives its `-data`
/// workspace directory from the project it is started in, rather than sharing one
/// workspace across every project.
///
/// **Run this after upgrading jdtls, and only then.** It shells out to the real
/// launcher, so it cannot run in a normal `cargo test`:
///
/// ```text
/// cargo test -- --ignored jdtls_data_dir
/// ```
///
/// Why it exists rather than being a one-off check in a debugging session: if the
/// assumption stops holding, two Java projects silently share one index and
/// cross-project symbol resolution goes wrong in a way that looks like a jdtls bug
/// rather than a configuration one. Pinning it costs one command per upgrade.
///
/// Why it is `#[ignore]` and not wired into CI: this repository's CI runs no tests at
/// all (`.github/workflows/main.yml` triggers on tags and only builds). The trigger
/// is a human following the note above — which is exactly why the note is here, and
/// not only in a commit message.
///
/// # Not yet executed against a real server
///
/// jdtls was not installed on the machine this was written on, so the *parsing*
/// below has never seen real launcher output. It is written to fail loudly rather
/// than to pass by accident: every path that cannot determine a workspace directory
/// panics with what it saw. If the first real run fails, fix the extraction here —
/// do not delete the assertion, which is the part that is known to be right.
#[cfg(test)]
mod jdtls_live_tests {
    use std::path::{Path, PathBuf};
    use std::process::Command;

    /// Absolute paths the launcher mentions when started in `dir`.
    ///
    /// The launcher echoes the command line it is about to run, `-data <dir>`
    /// included, which is the value this probe is after. Started with `--help` so it
    /// prints and exits instead of serving a session.
    fn launcher_paths(dir: &Path) -> Vec<PathBuf> {
        let out = Command::new("jdtls")
            .arg("--help")
            .current_dir(dir)
            .output()
            .expect("jdtls must be on PATH to run this probe");
        let text = format!(
            "{}{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
        let paths: Vec<PathBuf> = text
            .split_whitespace()
            .filter(|t| t.starts_with('/') && t.len() > 1)
            .map(PathBuf::from)
            .collect();
        assert!(
            !paths.is_empty(),
            "could not find any absolute path in the launcher's output for {}. \
             The extraction in this probe needs updating for this jdtls version; \
             output was:\n{text}",
            dir.display()
        );
        paths
    }

    #[test]
    #[ignore]
    fn jdtls_data_dir_varies_by_project() {
        let a = std::env::temp_dir().join("lsp-proxy-jdtls-probe-a");
        let b = std::env::temp_dir().join("lsp-proxy-jdtls-probe-b");
        std::fs::create_dir_all(&a).unwrap();
        std::fs::create_dir_all(&b).unwrap();

        let (pa, pb) = (launcher_paths(&a), launcher_paths(&b));
        eprintln!("  project A paths: {pa:#?}");
        eprintln!("  project B paths: {pb:#?}");

        // A4 holds only if *something* the launcher resolves differs between the two
        // projects. Identical output means one shared workspace, so the two projects'
        // indexes would contaminate each other.
        assert_ne!(
            pa, pb,
            "A4 no longer holds: the launcher resolves the same paths in two \
             different projects, so they would share one jdtls workspace. Pass an \
             explicit per-project `-data` in languages.toml, or drop the assumption."
        );
    }
}

#[cfg(test)]
mod install_lombok_tests {
    use super::{InstallLombok, InstallLombokParams, InstallLombokResult};
    use lsp_types::request::Request as _;

    /// The method name is the wire contract with `lsp-proxy-java.el`; a rename here
    /// silently stops the command working rather than failing to compile.
    #[test]
    fn the_method_name_is_stable() {
        assert_eq!(InstallLombok::METHOD, "emacs/installLombok");
    }

    /// Same envelope as every other request: the payload lives in the nested
    /// `params`, and `force` defaults so the client may omit it.
    #[test]
    fn params_and_result_round_trip_through_the_envelope() {
        let wire = r#"{
          "id": 7,
          "method": "emacs/installLombok",
          "params": { "params": { "installDir": "/u/.emacs.d/lsp-proxy/lombok/" } }
        }"#;
        let req: crate::msg::Request = serde_json::from_str(wire).unwrap();
        let params: InstallLombokParams = serde_json::from_value(req.params.params).unwrap();
        assert_eq!(params.install_dir, "/u/.emacs.d/lsp-proxy/lombok/");
        assert!(!params.force, "an absent force must mean false");

        // camelCase on the way out, because that is what the Emacs side reads.
        let json = serde_json::to_string(&InstallLombokResult {
            jar_path: "/u/lombok-1.18.48.jar".into(),
            version: "1.18.48".into(),
            already_installed: true,
        })
        .unwrap();
        assert!(json.contains("\"jarPath\""), "{json}");
        assert!(json.contains("\"alreadyInstalled\":true"), "{json}");
    }
}
