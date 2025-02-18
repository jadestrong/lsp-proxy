use futures_util::Future;
use log::debug;
use lsp::{
    notification::DidChangeWorkspaceFolders, request::Request, DidChangeWorkspaceFoldersParams,
    OneOf, WorkspaceFolder, WorkspaceFoldersChangeEvent,
};
use lsp_types as lsp;
use parking_lot::Mutex;
use serde_json::Value;
use std::{
    collections::HashMap,
    path::PathBuf,
    process::Stdio,
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc,
    },
};
use tokio::{
    io::{BufReader, BufWriter},
    process::{Child, Command},
    sync::{
        mpsc::{channel, UnboundedReceiver, UnboundedSender},
        Notify, OnceCell,
    },
};

use crate::{
    lsp::{
        jsonrpc::{self, Call, Version},
        transport::{Payload, Transport},
        Error, Result,
    },
    msg::RequestId,
    registry,
    syntax::{LanguageServerFeature, LanguageServerFeatures},
    utils::{find_lsp_workspace, find_workspace_for_file, get_activate_time, path},
};

fn workspace_for_uri(uri: lsp::Url) -> lsp::WorkspaceFolder {
    lsp::WorkspaceFolder {
        name: uri
            .path_segments()
            .and_then(|segments| segments.last())
            .map(|basename| basename.to_string())
            .unwrap_or_default(),
        uri,
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct RegisteredCapability {
    pub id: String,
    pub method: String,
    pub register_options: Option<lsp_types::TextDocumentRegistrationOptions>,
}

#[derive(Debug)]
pub struct Client {
    id: usize,
    name: String,
    _process: Child,
    server_tx: UnboundedSender<Payload>,
    request_counter: AtomicU64,
    pub(crate) capabilities: OnceCell<lsp::ServerCapabilities>,
    pub(crate) registered_capabilities: Mutex<Vec<RegisteredCapability>>,
    config: Option<Value>,
    experimental: Option<Value>,
    pub(crate) root_path: std::path::PathBuf,
    root_uri: Option<lsp::Url>,
    workspace_folders: Mutex<Vec<lsp::WorkspaceFolder>>,
    initialize_notify: Arc<Notify>,
    /// workspace folders added while the server is still initializing
    req_timeout: u64,
    features: Option<LanguageServerFeatures>,
    pub(crate) activate_time: Arc<Mutex<u128>>,
}

#[allow(dead_code)]
impl Client {
    pub fn try_add_doc(
        self: &Arc<Self>,
        root_markers: &[String],
        doc_path: Option<&std::path::PathBuf>,
        may_support_workspace: bool,
    ) -> bool {
        // 找到文件所属的项目目录
        let (workspace, workspace_is_cwd) = find_workspace_for_file(doc_path.unwrap());
        let workspace = path::normalize(&workspace);
        // FIXME 当非 git 项目时得到的 workspace 地址是错误的
        let root = find_lsp_workspace(
            doc_path
                .and_then(|x| x.parent().and_then(|x| x.to_str()))
                .unwrap_or("."),
            root_markers,
            &workspace,
            workspace_is_cwd,
        );

        let root_uri = root
            .as_ref()
            .and_then(|root| lsp::Url::from_file_path(root).ok());

        // 如果 lsp_workspace root 和当前 client 的对的上，就证明这个 client 属于该文件
        if self.root_path == root.unwrap_or(workspace)
            || root_uri.as_ref().map_or(false, |root_uri| {
                self.workspace_folders
                    .lock()
                    .iter()
                    .any(|workspace| &workspace.uri == root_uri)
            })
        {
            return true;
        }

        if !may_support_workspace {
            return false;
        }

        // 验证服务端返回的 workspace folder FIXME 弄明白什么意思
        let Some(capabilities) = self.capabilities.get() else {
            let client = Arc::clone(self);
            tokio::spawn(async move {
                client.initialize_notify.notified().await;
                if let Some(workspace_folders_caps) = client
                    .capabilities()
                    .workspace
                    .as_ref()
                    .and_then(|cap| cap.workspace_folders.as_ref())
                    .filter(|cap| cap.supported.unwrap_or(false))
                {
                    client.add_workspace_folder(
                        root_uri,
                        &workspace_folders_caps.change_notifications,
                    );
                }
            });

            return true;
        };

        if let Some(workspace_folders_caps) = capabilities
            .workspace
            .as_ref()
            .and_then(|cap| cap.workspace_folders.as_ref())
            .filter(|cap| cap.supported.unwrap_or(false))
        {
            self.add_workspace_folder(root_uri, &workspace_folders_caps.change_notifications);
            true
        } else {
            // the server doesn't support multip workspaces, we need a new client
            false
        }
    }

    fn add_workspace_folder(
        &self,
        root_uri: Option<lsp::Url>,
        change_notifications: &Option<OneOf<bool, String>>,
    ) {
        // root_uri is None just means that there isn't really any LSP workspace
        // associated with this file. For servers that support multiple workspaces
        // there is just one server so we can always just use that shared instance.
        // No need to add a new workspace root here as there is no logical root for this file
        // let the server deal with this
        let Some(root_uri) = root_uri else {
            return;
        };

        self.workspace_folders
            .lock()
            .push(workspace_for_uri(root_uri.clone()));
        if &Some(OneOf::Left(false)) == change_notifications {
            // server specifically opted out of DidWorkspaceChange notifications
            // let's assume the server will request the workspace folders itself
            // and that we can therefore reuse the client (but are done now)
            return;
        }
        // tokio::spawn();
        self.did_change_workspace(vec![workspace_for_uri(root_uri)], Vec::new())
            .unwrap();
    }

    pub fn start(
        cmd: &str,
        args: &[String],
        config: Option<Value>,
        experimental: Option<Value>,
        server_envirment: HashMap<String, String>,
        root_markers: &[String],
        id: usize,
        name: String,
        req_timeout: u64,
        doc_path: Option<&std::path::PathBuf>,
        features: Option<&LanguageServerFeatures>,
    ) -> registry::Result<(Self, UnboundedReceiver<(usize, Call)>, Arc<Notify>)> {
        // Resolve path to the binary
        let cmd = which::which(cmd).map_err(|err| anyhow::anyhow!(err))?;

        let process = Command::new(cmd)
            .envs(server_envirment)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            // make sure the process is reaped on drop
            .kill_on_drop(true)
            .spawn();
        let mut process = process?;

        // TODO: do we need bufreader/writer here? or do we use async wrappers on unblock?
        let writer = BufWriter::new(process.stdin.take().expect("Failed to open stdin"));
        let reader = BufReader::new(process.stdout.take().expect("Failed to open stdout"));
        let stderr = BufReader::new(process.stderr.take().expect("Failed to open stderr"));

        let (server_rx, server_tx, initialize_notify) =
            Transport::start(reader, writer, stderr, id, name.clone());

        // 找出 git 目录，如果不存在，则使用当前目录
        let (workspace, workspace_is_cwd) = find_workspace_for_file(doc_path.unwrap());
        let workspace = path::normalize(&workspace);
        let root = find_lsp_workspace(
            doc_path
                .and_then(|x| x.parent().and_then(|x| x.to_str()))
                .unwrap_or("."),
            root_markers,
            &workspace,
            workspace_is_cwd,
        );

        // `root_uri` and `workspace_folder` can be empty is case there is no workspace
        // `root_url` can not, use `workspace` as a fallback
        let root_path = root.clone().unwrap_or_else(|| workspace.clone());
        let root_uri = root.and_then(|root| lsp::Url::from_file_path(root).ok());

        if let Some(features) = features {
            if features.config_files.len() > 0
                && !features.config_files.iter().any(|config_file| {
                    // "Check if the root + config file exists."
                    let config_file_path = PathBuf::from(root_path.clone()).join(config_file);
                    config_file_path.exists()
                })
            {
                return Err(registry::Error::Other(anyhow::anyhow!(
                    "No config file found for language server"
                )));
            }
        }

        let workspace_folders = root_uri
            .clone()
            .map(|root| vec![workspace_for_uri(root)])
            .unwrap_or_default();

        let client = Self {
            id,
            name,
            _process: process,
            server_tx,
            request_counter: AtomicU64::new(0),
            capabilities: OnceCell::new(),
            registered_capabilities: Mutex::new(Vec::new()),
            config,
            experimental,
            root_path,
            root_uri,
            workspace_folders: Mutex::new(workspace_folders),
            initialize_notify: initialize_notify.clone(),
            req_timeout,
            features: features.map(|v| v.clone()),
            activate_time: Arc::new(Mutex::new(get_activate_time())),
        };

        Ok((client, server_rx, initialize_notify))
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn id(&self) -> usize {
        self.id
    }

    pub fn config(&self) -> Option<&Value> {
        self.config.as_ref()
    }

    pub async fn workspace_folders(
        &self,
    ) -> parking_lot::MutexGuard<'_, Vec<lsp_types::WorkspaceFolder>> {
        self.workspace_folders.lock()
    }

    fn next_request_id(&self) -> RequestId {
        let id = self.request_counter.fetch_add(1, Ordering::Relaxed);
        // avoid duplicate with emacs request
        RequestId::from(format!("req{:?}", id))
    }

    fn value_into_params(value: Value) -> jsonrpc::Params {
        use jsonrpc::Params;

        match value {
            Value::Null => Params::None,
            Value::Bool(_) | Value::Number(_) | Value::String(_) => Params::Array(vec![value]),
            Value::Array(vec) => Params::Array(vec),
            Value::Object(map) => Params::Map(map),
        }
    }

    pub fn is_initialized(&self) -> bool {
        self.capabilities.get().is_some()
    }

    pub fn capabilities(&self) -> &lsp::ServerCapabilities {
        self.capabilities
            .get()
            .expect("language server not initialized!")
    }

    pub fn with_feature(&self, feature: LanguageServerFeature) -> bool {
        self.is_initialized()
            && (self.supports_feature(feature) || self.supports_registered_feature(feature))
            && (self.features.is_none()
                || self
                    .features
                    .as_ref()
                    .map(|features| features.has_features(feature))
                    .unwrap_or(true))
        // .is_some_and(|features| features.has_features(feature)))
    }

    #[inline]
    pub fn supports_feature(&self, feature: LanguageServerFeature) -> bool {
        let capabilities = self.capabilities();

        use lsp::*;
        match feature {
            LanguageServerFeature::Format => matches!(
                capabilities.document_formatting_provider,
                Some(OneOf::Left(true) | OneOf::Right(_))
            ),
            LanguageServerFeature::GotoDeclaration => matches!(
                capabilities.declaration_provider,
                Some(
                    DeclarationCapability::Simple(true)
                        | DeclarationCapability::RegistrationOptions(_)
                        | DeclarationCapability::Options(_),
                )
            ),
            LanguageServerFeature::GotoDefinition => matches!(
                capabilities.definition_provider,
                Some(OneOf::Left(true) | OneOf::Right(_))
            ),
            LanguageServerFeature::GotoTypeDefinition => matches!(
                capabilities.type_definition_provider,
                Some(
                    TypeDefinitionProviderCapability::Simple(true)
                        | TypeDefinitionProviderCapability::Options(_),
                )
            ),
            LanguageServerFeature::GotoReference => matches!(
                capabilities.references_provider,
                Some(OneOf::Left(true) | OneOf::Right(_))
            ),
            LanguageServerFeature::GotoImplementation => matches!(
                capabilities.implementation_provider,
                Some(
                    ImplementationProviderCapability::Simple(true)
                        | ImplementationProviderCapability::Options(_),
                )
            ),
            LanguageServerFeature::SignatureHelp => capabilities.signature_help_provider.is_some(),
            LanguageServerFeature::Hover => matches!(
                capabilities.hover_provider,
                Some(HoverProviderCapability::Simple(true) | HoverProviderCapability::Options(_),)
            ),
            LanguageServerFeature::DocumentHighlight => matches!(
                capabilities.document_highlight_provider,
                Some(OneOf::Left(true) | OneOf::Right(_))
            ),
            LanguageServerFeature::Completion => capabilities.completion_provider.is_some(),
            LanguageServerFeature::CompletionResolve => matches!(
                capabilities.completion_provider,
                Some(lsp::CompletionOptions {
                    resolve_provider: Some(true),
                    ..
                })
            ),
            LanguageServerFeature::CodeAction => matches!(
                capabilities.code_action_provider,
                Some(
                    CodeActionProviderCapability::Simple(true)
                        | CodeActionProviderCapability::Options(_),
                )
            ),
            LanguageServerFeature::WorkspaceCommand => {
                capabilities.execute_command_provider.is_some()
            }
            LanguageServerFeature::DocumentSymbols => matches!(
                capabilities.document_symbol_provider,
                Some(OneOf::Left(true) | OneOf::Right(_))
            ),
            LanguageServerFeature::WorkspaceSymbols => matches!(
                capabilities.workspace_symbol_provider,
                Some(OneOf::Left(true) | OneOf::Right(_))
            ),
            LanguageServerFeature::Diagnostics => true, // there's no extra server capability
            LanguageServerFeature::RenameSymbol => matches!(
                capabilities.rename_provider,
                Some(OneOf::Left(true)) | Some(OneOf::Right(_))
            ),
            LanguageServerFeature::InlayHints => matches!(
                capabilities.inlay_hint_provider,
                Some(OneOf::Left(true) | OneOf::Right(InlayHintServerCapabilities::Options(_)))
            ),
        }
    }

    pub fn supports_registered_feature(&self, feature: LanguageServerFeature) -> bool {
        let registered_capabilities = self.registered_capabilities.lock();
        if feature == LanguageServerFeature::Format {
            return registered_capabilities
                .iter()
                .any(|cap| cap.method == lsp_types::request::Formatting::METHOD);
        }
        false
    }

    async fn request<R: lsp::request::Request>(&self, params: R::Params) -> Result<R::Result>
    where
        R::Params: serde::Serialize,
        R::Result: core::fmt::Debug, // TODO temporary
    {
        // a future that resolves into the response
        let json = self.call::<R>(self.next_request_id(), params).await?;
        let response = serde_json::from_value(json)?;
        Ok(response)
    }

    pub fn call<R: lsp::request::Request>(
        &self,
        req_id: RequestId,
        params: R::Params,
    ) -> impl Future<Output = Result<Value>>
    where
        R::Params: serde::Serialize,
    {
        self.call_with_timeout::<R>(req_id, params, self.req_timeout)
    }

    fn call_with_timeout<R: lsp::request::Request>(
        &self,
        req_id: RequestId,
        params: R::Params,
        timeout_secs: u64,
    ) -> impl Future<Output = Result<Value>>
    where
        R::Params: serde::Serialize,
    {
        let server_tx = self.server_tx.clone();
        async move {
            use std::time::Duration;
            use tokio::time::timeout;

            let params = serde_json::to_value(params)?;

            let request = jsonrpc::MethodCall {
                jsonrpc: Some(jsonrpc::Version::V2),
                id: req_id.clone(),
                method: R::METHOD.to_string(),
                params: Self::value_into_params(params),
            };

            let (tx, mut rx) = channel::<Result<Value>>(1);
            server_tx
                .send(Payload::Request {
                    chan: tx,
                    value: request,
                })
                .map_err(|e| Error::Other(e.into()))?;

            timeout(Duration::from_secs(timeout_secs), rx.recv())
                .await
                .map_err(|_| Error::Timeout(format!("{}({})", R::METHOD, req_id)))?
                .ok_or(Error::StreamClosed)?
        }
    }

    /// Send a RPC notification to the language server
    pub fn notify<R: lsp::notification::Notification>(&self, params: R::Params) -> Result<()>
    where
        R::Params: serde::Serialize,
    {
        let server_tx = self.server_tx.clone();
        let params = serde_json::to_value(params)?;

        let notification = jsonrpc::Notification {
            jsonrpc: Some(jsonrpc::Version::V2),
            method: R::METHOD.to_string(),
            params: Self::value_into_params(params),
        };
        server_tx
            .send(Payload::Notification(notification))
            .map_err(|e| Error::Other(e.into()))?;
        Ok(())
    }

    /// Reply to a language server RPC call.
    pub fn reply(
        &self,
        id: RequestId,
        result: core::result::Result<Value, jsonrpc::Error>,
    ) -> impl Future<Output = Result<()>> {
        use jsonrpc::{Failure, Output, Success};

        let server_tx = self.server_tx.clone();

        async move {
            let output = match result {
                Ok(result) => Output::Success(Success {
                    jsonrpc: Some(Version::V2),
                    result: serde_json::to_value(result)?,
                    id,
                }),
                Err(error) => Output::Failure(Failure {
                    jsonrpc: Some(Version::V2),
                    error,
                    id,
                }),
            };

            server_tx
                .send(Payload::Response(output))
                .map_err(|e| Error::Other(e.into()))?;

            Ok(())
        }
    }

    // -------------------------------------------------------------------------------------------
    // General messages
    // -------------------------------------------------------------------------------------------

    pub(crate) async fn initialize(&self, enable_snippets: bool) -> Result<lsp::InitializeResult> {
        if let Some(config) = &self.config {
            debug!("Using custom LSP config: {}", config);
        }

        #[allow(deprecated)]
        let params = lsp::InitializeParams {
            process_id: Some(std::process::id()),
            root_path: self.root_path.to_str().map(|path| path.to_owned()),
            root_uri: self.root_uri.clone(),
            initialization_options: self.config.clone(),
            capabilities: lsp::ClientCapabilities {
                workspace: Some(lsp::WorkspaceClientCapabilities {
                    configuration: Some(true),
                    did_change_configuration: Some(lsp::DynamicRegistrationClientCapabilities {
                        dynamic_registration: Some(false),
                    }),
                    workspace_folders: Some(true),
                    apply_edit: Some(true),
                    symbol: Some(lsp::WorkspaceSymbolClientCapabilities {
                        dynamic_registration: Some(false),
                        ..Default::default()
                    }),
                    execute_command: Some(lsp::DynamicRegistrationClientCapabilities {
                        dynamic_registration: Some(false),
                    }),
                    inlay_hint: Some(lsp::InlayHintWorkspaceClientCapabilities {
                        refresh_support: Some(false),
                    }),
                    workspace_edit: Some(lsp::WorkspaceEditClientCapabilities {
                        document_changes: Some(true),
                        resource_operations: Some(vec![
                            lsp::ResourceOperationKind::Create,
                            lsp::ResourceOperationKind::Rename,
                            lsp::ResourceOperationKind::Delete,
                        ]),
                        failure_handling: Some(lsp::FailureHandlingKind::Abort),
                        normalizes_line_endings: Some(false),
                        change_annotation_support: None,
                    }),
                    did_change_watched_files: Some(lsp::DidChangeWatchedFilesClientCapabilities {
                        dynamic_registration: Some(true),
                        relative_pattern_support: Some(false),
                    }),
                    file_operations: Some(lsp::WorkspaceFileOperationsClientCapabilities {
                        will_rename: Some(true),
                        did_rename: Some(true),
                        ..Default::default()
                    }),
                    ..Default::default()
                }),
                text_document: Some(lsp::TextDocumentClientCapabilities {
                    completion: Some(lsp::CompletionClientCapabilities {
                        completion_item: Some(lsp::CompletionItemCapability {
                            snippet_support: Some(enable_snippets),
                            resolve_support: Some(lsp::CompletionItemCapabilityResolveSupport {
                                properties: vec![
                                    String::from("documentation"),
                                    String::from("detail"),
                                    String::from("additionalTextEdits"),
                                ],
                            }),
                            insert_replace_support: Some(true),
                            deprecated_support: Some(true),
                            tag_support: Some(lsp::TagSupport {
                                value_set: vec![lsp::CompletionItemTag::DEPRECATED],
                            }),
                            ..Default::default()
                        }),
                        completion_item_kind: Some(lsp::CompletionItemKindCapability {
                            ..Default::default()
                        }),
                        context_support: None, // additional context information Some(true)
                        ..Default::default()
                    }),
                    hover: Some(lsp::HoverClientCapabilities {
                        // if not specified, rust-analyzer returns plaintext marked as markdown but
                        // badly formatted.
                        content_format: Some(vec![lsp::MarkupKind::Markdown]),
                        ..Default::default()
                    }),
                    signature_help: Some(lsp::SignatureHelpClientCapabilities {
                        signature_information: Some(lsp::SignatureInformationSettings {
                            documentation_format: Some(vec![lsp::MarkupKind::Markdown]),
                            parameter_information: Some(lsp::ParameterInformationSettings {
                                label_offset_support: Some(true),
                            }),
                            active_parameter_support: Some(true),
                        }),
                        ..Default::default()
                    }),
                    rename: Some(lsp::RenameClientCapabilities {
                        dynamic_registration: Some(false),
                        prepare_support: Some(true),
                        prepare_support_default_behavior: None,
                        honors_change_annotations: Some(false),
                    }),
                    code_action: Some(lsp::CodeActionClientCapabilities {
                        code_action_literal_support: Some(lsp::CodeActionLiteralSupport {
                            code_action_kind: lsp::CodeActionKindLiteralSupport {
                                value_set: [
                                    lsp::CodeActionKind::EMPTY,
                                    lsp::CodeActionKind::QUICKFIX,
                                    lsp::CodeActionKind::REFACTOR,
                                    lsp::CodeActionKind::REFACTOR_EXTRACT,
                                    lsp::CodeActionKind::REFACTOR_INLINE,
                                    lsp::CodeActionKind::REFACTOR_REWRITE,
                                    lsp::CodeActionKind::SOURCE,
                                    lsp::CodeActionKind::SOURCE_ORGANIZE_IMPORTS,
                                ]
                                .iter()
                                .map(|kind| kind.as_str().to_string())
                                .collect(),
                            },
                        }),
                        is_preferred_support: Some(true),
                        disabled_support: Some(true),
                        data_support: Some(true),
                        resolve_support: Some(lsp::CodeActionCapabilityResolveSupport {
                            properties: vec!["edit".to_owned(), "command".to_owned()],
                        }),
                        ..Default::default()
                    }),
                    publish_diagnostics: Some(lsp::PublishDiagnosticsClientCapabilities {
                        version_support: Some(true),
                        ..Default::default()
                    }),
                    inlay_hint: Some(lsp::InlayHintClientCapabilities {
                        dynamic_registration: Some(false),
                        resolve_support: None,
                    }),
                    definition: Some(lsp::GotoCapability {
                        dynamic_registration: Some(true),
                        link_support: Some(true),
                    }),
                    ..Default::default()
                }),
                window: Some(lsp::WindowClientCapabilities {
                    work_done_progress: Some(true),
                    ..Default::default()
                }),
                general: Some(lsp::GeneralClientCapabilities {
                    position_encodings: Some(vec![
                        lsp::PositionEncodingKind::UTF32,
                        lsp::PositionEncodingKind::UTF16,
                    ]),
                    ..Default::default()
                }),
                experimental: self.experimental.clone(),
                ..Default::default()
            },
            trace: None,
            workspace_folders: Some(self.workspace_folders.lock().clone()),
            client_info: Some(lsp::ClientInfo {
                name: String::from("Emacs"),
                version: Some("30.0.50".to_string()),
            }),
            locale: None,
            work_done_progress_params: lsp::WorkDoneProgressParams {
                work_done_token: None,
            },
        };

        self.request::<lsp::request::Initialize>(params).await
    }

    pub async fn shutdown(&self) -> Result<()> {
        self.request::<lsp::request::Shutdown>(()).await
    }

    pub fn exit(&self) -> Result<()> {
        self.notify::<lsp::notification::Exit>(())
    }

    /// Tries to shut down the language server but returns
    /// early if server responds with an error.
    pub async fn shutdown_and_exit(&self) -> Result<()> {
        self.shutdown().await?;
        self.exit()
    }

    /// Forcefully shuts down the language server ignoring any errors.
    pub async fn force_shutdown(&self) -> Result<()> {
        if let Err(e) = self.shutdown().await {
            log::warn!("language server failed to terminate gracefully - {}", e);
        }
        self.exit()
    }

    // -------------------------------------------------------------------------------------------
    // Workspace
    // -------------------------------------------------------------------------------------------

    pub fn did_change_configuration(&self, settings: Value) -> Result<()> {
        self.notify::<lsp::notification::DidChangeConfiguration>(
            lsp::DidChangeConfigurationParams { settings },
        )
    }

    pub fn did_change_workspace(
        &self,
        added: Vec<WorkspaceFolder>,
        removed: Vec<WorkspaceFolder>,
    ) -> Result<()> {
        self.notify::<DidChangeWorkspaceFolders>(DidChangeWorkspaceFoldersParams {
            event: WorkspaceFoldersChangeEvent { added, removed },
        })
    }

    // -------------------------------------------------------------------------------------------
    // Text document
    // -------------------------------------------------------------------------------------------

    pub fn text_document_did_open(
        &self,
        uri: lsp::Url,
        version: i32,
        doc: String,
        language_id: String,
    ) -> Result<()> {
        self.notify::<lsp::notification::DidOpenTextDocument>(lsp::DidOpenTextDocumentParams {
            text_document: lsp::TextDocumentItem {
                uri,
                language_id,
                version,
                text: doc,
            },
        })
    }

    pub fn text_document_did_close(&self, params: lsp::DidCloseTextDocumentParams) -> Result<()> {
        self.notify::<lsp::notification::DidCloseTextDocument>(params)
    }

    pub fn text_document_will_save(&self, params: lsp::WillSaveTextDocumentParams) -> Result<()> {
        let capabilities = self.capabilities.get().unwrap();
        let supported_will_save =
            if let Some(lsp_types::TextDocumentSyncCapability::Options(options)) =
                capabilities.text_document_sync.as_ref()
            {
                options.will_save.unwrap_or(false)
            } else {
                false
            };

        if supported_will_save {
            self.notify::<lsp::notification::WillSaveTextDocument>(params)
        } else {
            Ok(())
        }
    }

    pub fn text_document_did_save(&self, params: lsp::DidSaveTextDocumentParams) -> Result<()> {
        let capabilities = self.capabilities.get().unwrap();
        let supported_save = match &capabilities.text_document_sync.as_ref() {
            Some(lsp_types::TextDocumentSyncCapability::Kind(kind)) => {
                matches!(*kind, lsp_types::TextDocumentSyncKind::INCREMENTAL | lsp_types::TextDocumentSyncKind::FULL)
            }
            Some(lsp_types::TextDocumentSyncCapability::Options(options)) => {
                options
                    .save
                    .as_ref()
                    .map_or(false, |save_options| match save_options {
                        lsp_types::TextDocumentSyncSaveOptions::Supported(supported) => *supported,
                        lsp_types::TextDocumentSyncSaveOptions::SaveOptions(_) => true,
                    })
            }
            _ => false
        };
        if supported_save {
            self.notify::<lsp::notification::DidSaveTextDocument>(params)
        } else {
            Ok(())
        }
    }

    pub fn cancel(&self, params: lsp::CancelParams) -> Result<()> {
        self.notify::<lsp::notification::Cancel>(params)
    }

    pub fn did_change_watched_files(
        &self,
        changes: Vec<lsp::FileEvent>,
    ) -> std::result::Result<(), Error> {
        self.notify::<lsp_types::notification::DidChangeWatchedFiles>(
            lsp::DidChangeWatchedFilesParams { changes },
        )
    }

    pub fn completion(
        &self,
        req_id: RequestId,
        parmas: lsp::CompletionParams,
    ) -> Option<impl Future<Output = Result<Value>>> {
        let capabilities = self.capabilities.get().unwrap();

        capabilities.completion_provider.as_ref()?;
        Some(self.call::<lsp::request::Completion>(req_id, parmas))
    }

    // code action
    pub fn code_actions(
        &self,
        req_id: RequestId,
        params: lsp_types::CodeActionParams,
    ) -> Option<impl Future<Output = Result<Value>>> {
        let capbilities = self.capabilities.get().unwrap();

        match capbilities.code_action_provider {
            Some(
                lsp_types::CodeActionProviderCapability::Simple(true)
                | lsp_types::CodeActionProviderCapability::Options(_),
            ) => (),
            _ => return None,
        }

        Some(self.call::<lsp_types::request::CodeActionRequest>(req_id, params))
    }
}
