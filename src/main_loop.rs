use crate::{
    application::Application,
    client::{Client, RegisteredCapability},
    config::{self, DEFAULT_MAX_DIAGNOSTICS_PUSH},
    connection::Connection,
    controller::Controller,
    dispatch::{NotificationDispatcher, RequestDispatcher},
    document::{
        DiagnosticItem, DiagnosticProvider, Document, DocumentId,
        DEFAULT_VIRTUAL_DOC_SERVER_TTL_SECS,
    },
    handlers::{
        self,
        request::{
            create_error_response, create_error_response_with_uri, handle_code_action,
            handle_inline_completion, handle_pull_diagnostic_response,
            pull_diagnostics_for_document,
        },
    },
    lsp::{
        self,
        jsonrpc::{self, Call},
        MethodCall,
    },
    lsp_ext::{self, RustAnalyzerExpandMacro},
    msg::{self, Message, Response},
    registry::NotificationFromServer,
    syntax::{self},
    thread,
    utils::{
        find_workspace_folder_for_uri, from_json, is_diagnostic_vectors_equal,
        limit_diagnostics_for_push, log_diagnostics_mismatch,
    },
};
use anyhow::Result;
use crossbeam_channel::{bounded, Sender};
use futures_util::StreamExt;
use log::{debug, error, info, warn};
use lsp_types::{notification::Notification, request::Request};
use serde_json::{json, Value};
use std::time::Duration;
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    path::{Path, PathBuf},
    sync::Arc,
};
use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};

fn should_discard_org_virtual_diagnostics<V, R>(
    server_id: usize,
    virtual_document_server_ids: V,
    regular_document_server_ids: R,
) -> bool
where
    V: IntoIterator<Item = usize>,
    R: IntoIterator<Item = usize>,
{
    virtual_document_server_ids
        .into_iter()
        .any(|id| id == server_id)
        && !regular_document_server_ids
            .into_iter()
            .any(|id| id == server_id)
}

pub fn main_loop(connection: Connection, syn_loader_config: syntax::Configuration) -> Result<()> {
    let (sender_for_application, mut recevier_by_application) = unbounded_channel();
    let (sender_for_controller, receiver_by_controller) = bounded(0);
    let mut thread_handles = vec![];
    thread_handles.push(thread::spawn(move || {
        Controller::new(sender_for_application, connection.sender)
            .run(connection.receiver, receiver_by_controller)
            .unwrap();
    }));
    thread_handles.push(thread::spawn(move || {
        tokio::runtime::Runtime::new().unwrap().block_on(async {
            Application::new(sender_for_controller, syn_loader_config)
                .run(&mut recevier_by_application)
                .await
                .unwrap();
        });
    }));

    for handle in thread_handles {
        handle.join();
    }
    Ok(())
}

#[allow(dead_code, unused_variables)]
impl Application {
    async fn run(&mut self, emacs_receiver: &mut UnboundedReceiver<Message>) -> Result<()> {
        let (tx, mut rx) = unbounded_channel();

        // Create interval for cleaning up expired virtual document servers
        // Check every 60 seconds
        let mut cleanup_interval = tokio::time::interval(Duration::from_secs(60));
        // Don't run immediately on start
        cleanup_interval.tick().await;

        loop {
            tokio::select! {
                biased;
                Some((id, call)) = self.editor.language_servers.incoming.next() => {
                    self.handle_language_server_message(call, id, tx.clone()).await;
                }
                Some(msg) = emacs_receiver.recv() => {
                    self.handle_msg(msg).await.expect("handle msg exception");
                }
                Some(msg) = rx.recv() => {
                    self.send(msg);
                }
                Some(callback) = self.jobs.callbacks.recv() => {
                    self.jobs.handle_callback(&mut self.editor, Ok(Some(callback)));
                }
                Some((root, result)) = self.java_workspace_cleanup_rx.recv() => {
                    self.complete_java_workspace_cleanup(root, result);
                }
                _ = cleanup_interval.tick() => {
                    self.cleanup_expired_virtual_doc_servers().await;
                }
            }
        }
    }

    /// Cleanup expired virtual document servers.
    /// This is called periodically to reclaim resources from unused servers.
    async fn cleanup_expired_virtual_doc_servers(&mut self) {
        let removed = self
            .editor
            .cleanup_expired_virtual_doc_servers(DEFAULT_VIRTUAL_DOC_SERVER_TTL_SECS);

        for (uri, language, client) in removed {
            // Send shutdown request to the server
            if client.is_initialized() {
                debug!(
                    "Shutting down expired virtual doc server '{}' for '{}'",
                    client.name(),
                    uri
                );

                // First send didClose for the virtual document
                let _ = client.text_document_did_close(lsp_types::DidCloseTextDocumentParams {
                    text_document: lsp_types::TextDocumentIdentifier { uri: uri.clone() },
                });

                // Note: We don't shutdown the server itself because it might be shared
                // with other documents. The server will be cleaned up when no more
                // documents reference it.

                // Notify user about server cleanup
                self.send_notification::<lsp_types::notification::ShowMessage>(
                    lsp_types::ShowMessageParams {
                        typ: lsp_types::MessageType::INFO,
                        message: format!(
                            "Recycled {} server for {} (idle timeout).",
                            client.name(),
                            language
                        ),
                    },
                );
            }
        }
    }

    pub async fn handle_language_server_message(
        &mut self,
        call: Call,
        server_id: usize,
        debounce_sender: UnboundedSender<Message>,
    ) {
        macro_rules! language_server {
            () => {
                match self.editor.language_server_by_id(server_id) {
                    Some(language_server) => language_server,
                    None => {
                        warn!("can't find language server with id `{}`", server_id);
                        return;
                    }
                }
            };
        }

        match call {
            Call::MethodCall(jsonrpc::MethodCall {
                method, params, id, ..
            }) => {
                let reply = match MethodCall::parse(&method, params) {
                    Ok(MethodCall::AddClasspathListener(params)) => {
                        let spring = language_server!();
                        let clients = self.editor.language_servers.get_project_clients(&spring);
                        self.spring_bridge
                            .classpath_request(spring, clients, id, params, false);
                        return;
                    }
                    Ok(MethodCall::RemoveClasspathListener(params)) => {
                        let spring = language_server!();
                        let clients = self.editor.language_servers.get_project_clients(&spring);
                        self.spring_bridge
                            .classpath_request(spring, clients, id, params, true);
                        return;
                    }
                    Ok(MethodCall::ExecuteClientCommand(params)) => {
                        self.spring_bridge.callback(language_server!(), id, params);
                        return;
                    }
                    Err(lsp::Error::Unhandled) => {
                        error!("Language Server: Method {method} not found in request {id}");
                        Err(jsonrpc::Error {
                            code: jsonrpc::ErrorCode::MethodNotFound,
                            message: format!("Method not found: {method}"),
                            data: None,
                        })
                    }
                    Err(err) => {
                        error!(
                            "Language Server: Received malformed method call {method} in request {id}: {err}"
                        );
                        Err(jsonrpc::Error {
                            code: jsonrpc::ErrorCode::ParseError,
                            message: format!("Malformed method call: {method}"),
                            data: None,
                        })
                    }
                    Ok(MethodCall::WorkDoneProgressCreate(params)) => {
                        let language_server = language_server!();
                        self.work_done_progress.create(server_id, params.token);
                        Ok(serde_json::Value::Null)
                    }
                    Ok(MethodCall::ApplyWorkspaceEdit(params)) => {
                        let language_server = language_server!();
                        if language_server.is_initialized() {
                            self.send_request::<lsp_types::request::ApplyWorkspaceEdit>(
                                params,
                                |_, res| {
                                    debug!("res {res:?}");
                                },
                            );
                            Ok(json!(lsp_types::ApplyWorkspaceEditResponse {
                                applied: true,
                                failure_reason: None,
                                failed_change: None
                            }))
                        } else {
                            Err(jsonrpc::Error {
                                code: jsonrpc::ErrorCode::InvalidRequest,
                                message: "Server must be initialized to request workspace edits"
                                    .to_string(),
                                data: None,
                            })
                        }
                    }
                    Ok(MethodCall::WorkspaceFolders) => {
                        Ok(json!(&*language_server!().workspace_folders().await))
                    }
                    Ok(MethodCall::WorkspaceConfiguration(params)) => {
                        let language_server = language_server!();
                        let result: Vec<_> = params
                            .items
                            .iter()
                            .map(|item| {
                                let mut config = language_server.config()?;
                                if let Some(section) = item.section.as_ref() {
                                    // for some reason some lsps send an empty string (observed in 'vscode-eslint-language-server')
                                    if !section.is_empty() {
                                        for part in section.split('.') {
                                            config = config.get(part)?;
                                        }
                                    }
                                }

                                // eslint need scope_uri to set workspaceFolder
                                if language_server.name() == "eslint" {
                                    if let Some(uri) = item.scope_uri.as_ref() {
                                        let workspace_folder = find_workspace_folder_for_uri(uri);
                                        if let Some(workspace_folder) = workspace_folder {
                                            let v = json!({
                                                "uri": workspace_folder.0,
                                                "name": workspace_folder.1,
                                            });
                                            let config = match config {
                                                Value::Object(m) => {
                                                    let mut m = m.clone();
                                                    m.insert("workspaceFolder".to_string(), v);
                                                    Value::Object(m)
                                                }
                                                v => v.clone(),
                                            };
                                            return Some(config);
                                        }
                                    }
                                }

                                Some(config.to_owned())
                            })
                            .collect();
                        Ok(json!(result))
                    }
                    Ok(MethodCall::CodeLensRefresh) => {
                        self.send_notification::<lsp_ext::CodeLensRefresh>(());
                        Ok(serde_json::Value::Null)
                    }
                    Ok(MethodCall::RegisterCapability(params)) => {
                        // Set when a registration changes something
                        // `get_server_capabilities` reports, so the editor can be told.
                        let mut announced_capabilities_changed = false;
                        let mut registration_error = None;
                        if let Some(client) = self
                            .editor
                            .language_servers
                            .iter_clients()
                            .find(|client| client.id() == server_id)
                        {
                            for reg in params.registrations {
                                match reg.method.as_str() {
                                    lsp_types::request::GotoDefinition::METHOD => {
                                        let id = reg.id.clone();
                                        match client.register_definition(reg) {
                                            Ok(()) => debug!(
                                                "Registered definition {id} for server {server_id}"
                                            ),
                                            Err(err) => {
                                                registration_error = Some(jsonrpc::Error {
                                                    code: jsonrpc::ErrorCode::InvalidParams,
                                                    message: err.to_string(),
                                                    data: None,
                                                });
                                            }
                                        }
                                    }
                                    lsp_types::notification::DidChangeWatchedFiles::METHOD => {
                                        let Some(options) = reg.register_options else {
                                            continue;
                                        };
                                        let ops: lsp_types::DidChangeWatchedFilesRegistrationOptions = match serde_json::from_value(options) {
                                            Ok(ops) => ops,
                                            Err(err) => {
                                                warn!("Failed to deserialize DidChangeWatchedFilesRegistrationOptions: {err}");
                                                continue;
                                            },
                                        };
                                        self.editor.language_servers.file_event_handler.register(
                                            client.id(),
                                            Arc::downgrade(client),
                                            reg.id,
                                            ops,
                                        )
                                    }
                                    lsp_types::request::Formatting::METHOD => {
                                        let Some(options) = reg.register_options else {
                                            continue;
                                        };
                                        let ops: lsp_types::TextDocumentRegistrationOptions =
                                            match serde_json::from_value(options) {
                                                Ok(ops) => ops,
                                                Err(err) => {
                                                    warn!("Failed to deserialize TextDocumentRegistrationOptions: {err}");
                                                    continue;
                                                }
                                            };
                                        client.registered_capabilities.lock().push(
                                            RegisteredCapability {
                                                id: reg.id,
                                                method: reg.method,
                                                register_options: Some(ops),
                                            },
                                        );
                                    }
                                    lsp_types::request::CodeLensRequest::METHOD => {
                                        let Some(options) = reg.register_options else {
                                            continue;
                                        };
                                        // `CodeLensRegistrationOptions` flattens
                                        // `TextDocumentRegistrationOptions` at the top level and adds
                                        // an unused (here) `resolveProvider` field, so deserializing
                                        // into the smaller struct works the same way `Formatting`
                                        // does above.
                                        let ops: lsp_types::TextDocumentRegistrationOptions =
                                            match serde_json::from_value(options) {
                                                Ok(ops) => ops,
                                                Err(err) => {
                                                    warn!("Failed to deserialize TextDocumentRegistrationOptions for CodeLens: {err}");
                                                    continue;
                                                }
                                            };
                                        client.registered_capabilities.lock().push(
                                            RegisteredCapability {
                                                id: reg.id,
                                                method: reg.method,
                                                register_options: Some(ops),
                                            },
                                        );
                                        announced_capabilities_changed = true;
                                    }
                                    _ => {
                                        // Language Servers based on the `vscode-languageserver-node` library often send
                                        // client/registerCapability even though we do not enable dynamic registration
                                        // for most capabilities. We should send a MethodNotFound JSONRPC error in this
                                        // case but that rejects the registration promise in the server which causes an
                                        // exit. So we work around this by ignoring the request and sending back an OK
                                        // response.
                                        log::warn!("Ignoring unsupported dynamic capability registration for {}", reg.method);
                                    }
                                }
                            }
                        }

                        // A dynamic registration arrives *after* `initialized`, so the
                        // capability snapshot the editor was given at startup is now
                        // stale. Nothing else re-sends it, and that snapshot is what the
                        // editor consults before asking for lenses — so without this, a
                        // server that registers code lens dynamically is never asked for
                        // one in any buffer that was already open when it registered.
                        if announced_capabilities_changed {
                            self.editor
                                .documents()
                                .filter(|doc| doc.language_servers().any(|ls| ls.id() == server_id))
                                .for_each(|doc| {
                                    self.send_notification::<lsp_ext::CustomServerCapabilities>(
                                        doc.get_server_capabilities(),
                                    )
                                });
                        }

                        match registration_error {
                            Some(err) => Err(err),
                            None => Ok(serde_json::Value::Null),
                        }
                    }
                    Ok(MethodCall::UnregisterCapability(params)) => {
                        for unreg in params.unregisterations {
                            match unreg.method.as_str() {
                                lsp_types::request::GotoDefinition::METHOD => {
                                    language_server!().unregister_definition(&unreg.id);
                                    debug!(
                                        "Unregistered definition {} for server {server_id}",
                                        unreg.id
                                    );
                                }
                                lsp_types::notification::DidChangeWatchedFiles::METHOD => {
                                    self.editor
                                        .language_servers
                                        .file_event_handler
                                        .unregister(server_id, unreg.id);
                                }
                                _ => {
                                    log::warn!("Received unregistration request for unsupported method: {}", unreg.method);
                                }
                            }
                        }
                        Ok(serde_json::Value::Null)
                    }
                    Ok(MethodCall::ShowMessageRequest(params)) => {
                        // Answered asynchronously: the user has to choose first, so the
                        // reply cannot be produced here. `forward_show_message_request`
                        // takes ownership of `id` and replies once the editor responds.
                        self.forward_show_message_request(server_id, id, params);
                        return;
                    }
                };

                tokio::spawn(language_server!().reply(id, reply));
            }
            Call::Notification(jsonrpc::Notification { method, params, .. }) => {
                let notification = match NotificationFromServer::parse(&method, params) {
                    Ok(notification) => notification,
                    Err(crate::registry::Error::Unhandled) => {
                        info!("Ignoring unhandled notification from Language Server {method:?}");
                        return;
                    }
                    Err(err) => {
                        error!("Ignoring unknown notification from Language Server: {err}");
                        return;
                    }
                };

                match notification {
                    NotificationFromServer::Initialized => {
                        let client = language_server!();
                        let project_clients =
                            self.editor.language_servers.get_project_clients(&client);
                        self.spring_bridge.initialized(client, project_clients);
                        let language_server = language_server!();
                        // NOTE Trigger a workspace/didChangeConfiguration notification after initialization.
                        // This might not be required by the spec but Neovim does this as well, so it's
                        // probably a good idea for compatibility.
                        let config = match language_server.config() {
                            Some(value) => value.clone(),
                            None => json!({}),
                        };

                        language_server.did_change_configuration(config).unwrap();

                        // Send capabilities for regular documents
                        self.editor
                            .documents()
                            .filter(|doc| {
                                doc.language_servers()
                                    .any(|ls| ls.id() == language_server.id())
                            })
                            .for_each(|doc| {
                                self.send_notification::<lsp_ext::CustomServerCapabilities>(
                                    doc.get_server_capabilities(),
                                )
                            });

                        // Send capabilities for virtual documents (org babel blocks)
                        self.editor
                            .documents()
                            .filter(|doc| {
                                doc.language_servers_of_virtual_doc
                                    .values()
                                    .any(|entry| entry.client.id() == language_server.id())
                            })
                            .for_each(|doc| {
                                self.send_notification::<lsp_ext::CustomServerCapabilities>(
                                    doc.get_virtual_doc_server_capabilities(),
                                )
                            });
                    }
                    NotificationFromServer::InitializationFailed(message) => {
                        self.spring_bridge.remove_client(server_id);
                        self.work_done_progress.remove_server(server_id);
                        let failed_server_id = server_id;
                        let failed_server_name = self
                            .editor
                            .documents()
                            .flat_map(|doc| {
                                doc.language_servers.values().chain(
                                    doc.language_servers_of_virtual_doc
                                        .values()
                                        .map(|entry| &entry.client),
                                )
                            })
                            .find(|ls| ls.id() == failed_server_id)
                            .map(|ls| ls.name().to_string())
                            .unwrap_or_else(|| format!("#{failed_server_id}"));
                        let affected_documents = self
                            .editor
                            .documents()
                            .filter_map(|doc| {
                                let regular = doc
                                    .language_servers
                                    .values()
                                    .any(|ls| ls.id() == failed_server_id);
                                let virtual_doc = doc
                                    .language_servers_of_virtual_doc
                                    .values()
                                    .any(|entry| entry.client.id() == failed_server_id);
                                (regular || virtual_doc)
                                    .then(|| (doc.uri.clone(), regular, virtual_doc))
                            })
                            .collect::<Vec<_>>();

                        for doc in self.editor.documents_mut() {
                            doc.language_servers
                                .retain(|_, ls| ls.id() != failed_server_id);
                            doc.language_servers_of_virtual_doc
                                .retain(|_, entry| entry.client.id() != failed_server_id);
                        }
                        self.editor.language_servers.remove_by_id(failed_server_id);

                        for (uri, regular, virtual_doc) in affected_documents {
                            if let Some(doc) = self.editor.document_by_uri(&uri) {
                                if regular {
                                    let has_pending_server = doc
                                        .language_servers
                                        .values()
                                        .any(|ls| !ls.is_initialized());
                                    if !has_pending_server {
                                        self.send_notification::<lsp_ext::CustomServerCapabilities>(
                                            doc.get_server_capabilities(),
                                        );
                                    }
                                }
                                if virtual_doc {
                                    self.send_notification::<lsp_ext::CustomServerCapabilities>(
                                        doc.get_virtual_doc_server_capabilities(),
                                    );
                                }
                            }
                        }

                        self.send_notification::<lsp_types::notification::ShowMessage>(
                            lsp_types::ShowMessageParams {
                                typ: lsp_types::MessageType::ERROR,
                                message: format!(
                                    "Language server {failed_server_name} failed to initialize: {message}"
                                ),
                            },
                        );
                    }
                    NotificationFromServer::Exit => {
                        self.spring_bridge.remove_client(server_id);
                        self.work_done_progress.remove_server(server_id);
                        let language_server = language_server!();
                        for doc in self.editor.documents_mut() {
                            doc.clear_all_diagnostics(server_id);
                        }
                        // Remove the language server from the registry
                        self.editor.language_servers.remove_by_id(server_id);
                        self.send_notification::<lsp_types::notification::ShowMessage>(
                            lsp_types::ShowMessageParams {
                                typ: lsp_types::MessageType::ERROR,
                                message: format!(
                                    "Language server {} has exited",
                                    language_server.name()
                                ),
                            },
                        );
                    }
                    NotificationFromServer::PublishDianostics(params) => {
                        let language_server = language_server!();
                        if !language_server.is_initialized() {
                            log::error!("Discarding pushlishDiagnostic notification sent by uninitialized server: {}", language_server.name());
                            return;
                        }
                        let doc = self.editor.get(&params.uri);
                        if doc.is_org_file()
                            && should_discard_org_virtual_diagnostics(
                                server_id,
                                doc.language_servers_of_virtual_doc
                                    .values()
                                    .map(|entry| entry.client.id()),
                                doc.language_servers.values().map(|client| client.id()),
                            )
                        {
                            log::debug!(
                                "Discarding publishDiagnostics from a virtual-document server for an org file."
                            );
                            return;
                        }
                        // Deliberately no staleness gate, matching
                        // vscode-languageclient's `handleDiagnostics`, which never reads
                        // `params.version`. Superseding is by content instead: the
                        // comparison below skips the notification when the diagnostics
                        // are unchanged, and a newer publish simply replaces the older
                        // set for this provider.
                        //
                        // Dropping stale publishes sounds safer but is strictly worse,
                        // because nothing re-requests them. A server that stamps the
                        // version it analysed will always lag the client while the user
                        // types, so an equality check discarded every notification and
                        // then went silent, losing the final analysis permanently.
                        // Showing a slightly stale set that the next publish corrects is
                        // the trade VS Code makes.
                        if let Some(version) = params.version {
                            if version != doc.version {
                                log::debug!(
                                    "Accepting diagnostics for {:?} at server version ({version}) while the document is at ({}); ranges may lag until the next publish",
                                    params.uri,
                                    doc.version()
                                );
                            }
                        }
                        let provider = DiagnosticProvider {
                            server_id,
                            identifier: None,
                        };
                        let version = doc.version;
                        let old_diagnostics = doc.get_diagnostics_by_provider(&provider);
                        let diagnostics_changed = match &old_diagnostics {
                            None => true,
                            Some(old) => !is_diagnostic_vectors_equal(old, &params.diagnostics),
                        };
                        // Own a copy of the previous set now (only when there is a genuine
                        // mismatch to explain, not on the first-ever publish) before
                        // `doc.replace_diagnostics' below takes a `&mut doc' borrow that
                        // would otherwise outlive `old_diagnostics'.
                        let previous_diagnostics: Option<Vec<lsp_types::Diagnostic>> =
                            old_diagnostics
                                .filter(|_| diagnostics_changed)
                                .map(|old| old.into_iter().cloned().collect());
                        if diagnostics_changed {
                            let diagnostics: Vec<DiagnosticItem> = params
                                .diagnostics
                                .iter()
                                .map(|diagnostic| DiagnosticItem {
                                    item: diagnostic.to_owned(),
                                    provider: provider.clone(),
                                    file_path: doc
                                        .path()
                                        .map(|p| p.to_string_lossy().to_string())
                                        .unwrap_or("".to_string()),
                                })
                                .collect();
                            doc.replace_diagnostics(diagnostics, &provider);
                            let all_diagnostics: Vec<lsp_types::Diagnostic> = match doc
                                .diagnostics()
                                .as_ref()
                            {
                                Some(diags) => diags.iter().map(|diag| diag.item.clone()).collect(),
                                None => vec![],
                            };

                            // Apply diagnostic limiting for push notification
                            let total_count = all_diagnostics.len();
                            let max_diagnostics_push = config::MAX_DIAGNOSTICS_PUSH
                                .get()
                                .copied()
                                .unwrap_or(DEFAULT_MAX_DIAGNOSTICS_PUSH);
                            let limited_diagnostics =
                                limit_diagnostics_for_push(&all_diagnostics, max_diagnostics_push);

                            // Log diagnostic limiting info
                            if total_count > max_diagnostics_push {
                                debug!(
                                    "Limiting diagnostics for {}: {} -> {} diagnostics (max: {})",
                                    params.uri,
                                    total_count,
                                    limited_diagnostics.len(),
                                    max_diagnostics_push
                                );
                                if let Some(previous) = &previous_diagnostics {
                                    log_diagnostics_mismatch(
                                        &params.uri,
                                        &previous.iter().collect::<Vec<_>>(),
                                        &params.diagnostics,
                                        max_diagnostics_push,
                                    );
                                }
                            }

                            self.send_notification::<lsp_types::notification::PublishDiagnostics>(
                                lsp_types::PublishDiagnosticsParams {
                                    version: Some(version),
                                    uri: params.uri,
                                    diagnostics: limited_diagnostics,
                                },
                            )
                        } else {
                            debug!("old and new equal, ignore");
                        }
                    }
                    NotificationFromServer::ShowMessage(params) => {
                        self.send_notification::<lsp_types::notification::ShowMessage>(params)
                    }
                    NotificationFromServer::LogMessage(params) => {
                        self.send_notification::<lsp_types::notification::LogMessage>(params)
                    }
                    NotificationFromServer::ProgressMessage(params) => {
                        let language_server = language_server!();
                        if !self.work_done_progress.should_forward(server_id, &params) {
                            debug!(
                                "Ignoring late progress for completed token {:?} from server {}",
                                params.token, server_id
                            );
                            return;
                        }
                        self.send_notification::<lsp_ext::CustomProgress>(
                            lsp_ext::CustomProgressParams {
                                root_path: language_server.root_path.to_string_lossy().to_string(),
                                params,
                            },
                        )
                    }
                    NotificationFromServer::LanguageStatus(params) => {
                        if params.typ == "ServiceReady" {
                            language_server!()
                                .java_service_ready
                                .store(true, std::sync::atomic::Ordering::Release);
                        }
                        let language_server = language_server!();
                        let mut params = lsp_ext::language_status_to_import_log(&params);
                        params.root_path =
                            Some(language_server.root_path.to_string_lossy().to_string());
                        self.send_notification::<lsp_ext::ImportLog>(params)
                    }
                    NotificationFromServer::LanguageProgressReport(params) => {
                        let language_server = language_server!();
                        self.send_notification::<lsp_ext::CustomProgress>(
                            lsp_ext::CustomProgressParams {
                                root_path: language_server.root_path.to_string_lossy().to_string(),
                                params: lsp_ext::progress_report_to_progress(&params),
                            },
                        )
                    }
                    NotificationFromServer::ForwardRequest(params) => {
                        let language_server = language_server!();
                        let servers = self
                            .editor
                            .language_servers
                            .get_project_clients(&language_server);
                        let ts_server = servers.into_iter().find(|ls| {
                            if let Some(lsp_types::ExecuteCommandOptions { commands, .. }) =
                                &ls.capabilities().execute_command_provider
                            {
                                commands
                                    .iter()
                                    .any(|command| command == "typescript.tsserverRequest")
                            } else {
                                false
                            }
                        });
                        if let Some(ts_server) = ts_server {
                            tokio::spawn(async move {
                                let lsp_ext::TsserverRequestParams(seq, command, args) = params;
                                let command_request = ts_server
                                    .execute_command(lsp_types::Command {
                                        title: "vue_request_forward".to_string(),
                                        command: "typescript.tsserverRequest".to_string(),
                                        arguments: Some(vec![command.into(), args]),
                                    })
                                    .unwrap();
                                let resp: Option<lsp_ext::TsserverRequestResult> =
                                    match command_request.await {
                                        Ok(value) => {
                                            match serde_json::from_value(value) {
                                                Ok(response) => Some(response),
                                                Err(e) => {
                                                    tracing::error!("Failed to deserialize tsserver response: {}", e);
                                                    None
                                                }
                                            }
                                        }
                                        Err(e) => {
                                            tracing::error!("Command request failed: {}", e);
                                            None
                                        }
                                    };
                                if let Err(e) =
                                    language_server.notify::<lsp_ext::TsserverResponse>(vec![
                                        lsp_ext::TsserverResponseParams(
                                            seq,
                                            resp.map(|r| r.body).unwrap_or(serde_json::Value::Null),
                                        ),
                                    ])
                                {
                                    tracing::error!(
                                        "Failed to send tsserver/response notification: {}",
                                        e
                                    );
                                };
                            });
                        } else {
                            error!("No tsserver found.");
                            self.send_notification::<lsp_types::notification::ShowMessage>(
                                lsp_types::ShowMessageParams {
                                    typ: lsp_types::MessageType::ERROR,
                                    message: format!("[Vue] No tsserver to forward {params:?}"),
                                },
                            );
                        }
                    }
                }
            }
            Call::Invalid { id } => {
                log::error!("Invalid {id:?}");
                panic!("Invalid Call");
            }
        }
    }

    async fn handle_msg(&mut self, msg: Message) -> Result<()> {
        let uri = match &msg {
            Message::Request(request) => request.params.uri.as_deref(),
            Message::Notification(notification) => notification.params.uri.as_deref(),
            _ => None,
        };
        if uri.is_some_and(|uri| self.java_workspace_cleanup_blocks(uri)) {
            if let Message::Request(request) = msg {
                self.respond(Response::new_err(
                    request.id,
                    jsonrpc::ErrorCode::InvalidRequest,
                    "Java workspace cache cleanup is in progress; retry when it finishes".into(),
                ));
            }
            return Ok(());
        }
        match msg {
            Message::Request(req)
                if self.shutdown_requested
                    && req.method != lsp_types::request::Shutdown::METHOD =>
            {
                log::warn!("Rejecting request '{}' after shutdown", req.method);
                self.respond(Response::new_err(
                    req.id,
                    jsonrpc::ErrorCode::InvalidRequest,
                    "Server is shutting down".to_string(),
                ));
            }
            Message::Request(req)
                if req.method == lsp_types::request::CodeActionRequest::METHOD =>
            {
                match self.get_working_document(&req) {
                    Ok(doc) => {
                        tokio::spawn(handle_code_action(
                            req,
                            self.sender.clone(),
                            doc.get_all_language_servers(),
                            match doc.diagnostics().as_ref() {
                                Some(diags) => diags.to_vec(),
                                None => vec![],
                            },
                        ));
                    }
                    Err(response) => self.respond(response),
                }
            }
            Message::Request(req)
                if req.method == lsp_types::request::DocumentDiagnosticRequest::METHOD =>
            {
                match self.get_working_document(&req) {
                    Ok(doc) => {
                        let language_servers = doc.language_servers_with_feature(
                            syntax::LanguageServerFeature::PullDiagnostics,
                        );
                        let sender = self.sender.clone();
                        let doc_id = doc.id();
                        let previous_result_id = doc.previous_diagnostic_id.clone();

                        // Tag this batch so a response that is overtaken by a newer
                        // batch can be recognised and discarded on arrival.
                        let pull_generation = self
                            .editor
                            .document_mut(doc_id)
                            .map(|doc| doc.next_pull_generation())
                            .unwrap_or_default();

                        let limit_diagnostics = req
                            .params
                            .context
                            .as_ref()
                            .and_then(|ctx| match ctx {
                                msg::Context::Diagnostic(diag_ctx) => {
                                    Some(diag_ctx.limit_diagnostics)
                                }
                                _ => None,
                            })
                            .unwrap_or(true);

                        tokio::spawn(async move {
                            // If limit_diagnostics is false, push all existing diagnostics from the document directly
                            if !limit_diagnostics {
                                crate::job::dispatch(move |editor| {
                                    if let Some(doc) = editor.document_mut(doc_id) {
                                        // Get all diagnostics from the document
                                        let all_diagnostics: Vec<lsp_types::Diagnostic> = doc
                                            .diagnostics()
                                            .as_ref()
                                            .map(|diags| {
                                                diags.iter().map(|d| d.item.clone()).collect()
                                            })
                                            .unwrap_or_default();

                                        // Send PublishDiagnostics notification
                                        let notification = crate::msg::Notification::new(
                                            lsp_types::notification::PublishDiagnostics::METHOD
                                                .to_string(),
                                            lsp_types::PublishDiagnosticsParams {
                                                version: Some(doc.version),
                                                uri: doc.uri.clone(),
                                                diagnostics: all_diagnostics,
                                            },
                                        );

                                        let _ = sender.send(notification.into());
                                    }
                                })
                                .await;
                                return;
                            }

                            for language_server in language_servers {
                                let params = from_json(
                                    lsp_types::request::DocumentDiagnosticRequest::METHOD,
                                    &req.params.params,
                                )
                                .unwrap();

                                let identifier = language_server
                                    .capabilities()
                                    .diagnostic_provider
                                    .as_ref()
                                    .and_then(|diagnostic_provider| match diagnostic_provider {
                                        lsp_types::DiagnosticServerCapabilities::Options(options) => {
                                            options.identifier.clone()
                                        }
                                        lsp_types::DiagnosticServerCapabilities::RegistrationOptions(
                                            options,
                                        ) => options.diagnostic_options.identifier.clone(),
                                    });

                                let response = pull_diagnostics_for_document(
                                    req.clone(),
                                    identifier.clone(),
                                    previous_result_id.to_owned(),
                                    params,
                                    &language_server,
                                )
                                .await;

                                let provider = DiagnosticProvider {
                                    server_id: language_server.id(),
                                    identifier: Some(
                                        identifier.unwrap_or(language_server.name().to_string()),
                                    ),
                                };

                                if let Some(result) = response {
                                    let _ = handle_pull_diagnostic_response(
                                        sender.clone(),
                                        provider,
                                        result,
                                        doc_id,
                                        limit_diagnostics,
                                        pull_generation,
                                    )
                                    .await;
                                }
                            }
                        });
                    }
                    Err(response) => self.respond(response),
                }
            }
            Message::Request(req)
                if req.method == lsp_types::request::InlineCompletionRequest::METHOD =>
            {
                match self.get_working_document(&req) {
                    Ok(doc) => {
                        let language_servers = doc.get_all_language_servers();
                        let language_server = language_servers.into_iter().find(|ls| {
                            ls.with_feature(syntax::LanguageServerFeature::InlineCompletion)
                        });
                        if let Some(ls) = language_server {
                            let language_id = doc.language_id().unwrap().to_owned();
                            tokio::spawn(handle_inline_completion(
                                req,
                                ls,
                                language_id,
                                self.sender.clone(),
                            ));
                        }
                    }
                    Err(response) => self.respond(response),
                }
            }
            Message::Request(req) if req.method == lsp_types::request::Shutdown::METHOD => {
                log::info!("Received shutdown request");
                self.request_shutdown();
                self.respond(Response::new_ok(req.id, serde_json::Value::Null));
            }
            Message::Request(req) if req.method == lsp_ext::WorkspaceRestart::METHOD => {
                self.handle_workspace_restart(&req);
            }
            Message::Request(req) if req.method == lsp_ext::CleanJavaWorkspace::METHOD => {
                if let Err(error) = self.begin_java_workspace_cleanup(&req) {
                    self.respond(create_error_response(&req.id, error.to_string()));
                }
            }
            Message::Request(req) if req.method == lsp_ext::ListWorkspaces::METHOD => {
                self.handle_list_workspaces(&req);
            }
            Message::Request(req) if req.method == lsp_ext::ShutdownWorkspace::METHOD => {
                self.handle_shutdown_workspace(&req);
            }
            // Matched by method before the generic path below, which resolves a
            // document first: installing a server is not about any open file.
            Message::Request(req) if req.method == lsp_ext::InstallJavaServer::METHOD => {
                self.handle_install_java_server(req);
            }
            Message::Request(req) if req.method == lsp_ext::InstallLombok::METHOD => {
                self.handle_install_lombok(req);
            }
            Message::Request(req) if req.method == lsp_ext::InstallJavaDebug::METHOD => {
                self.handle_install_java_debug(req);
            }
            Message::Request(req) if req.method == lsp_ext::InstallSpring::METHOD => {
                self.handle_install_spring(req);
            }
            Message::Request(req) if req.method == lsp_ext::GetJavaProjectRuntime::METHOD => {
                self.handle_get_java_project_runtime(req);
            }
            Message::Request(req) if req.method == lsp_ext::SetJavaProjectRuntime::METHOD => {
                self.handle_set_java_project_runtime(req);
            }
            Message::Request(req) => {
                match self.get_working_document(&req) {
                    Ok(doc) => {
                        if doc.is_org_file() {
                            // Check for virtual document context
                            if let Some(ref vdoc_ctx) = req.params.virtual_doc {
                                // Get the client from the entry
                                let language_server = doc
                                    .language_servers_of_virtual_doc
                                    .get(&vdoc_ctx.language)
                                    .map(|entry| entry.client.clone());

                                if let Some(ls) = language_server {
                                    // Touch the entry to update last_used (need mutable access)
                                    let uri = doc.uri.clone();
                                    let language = vdoc_ctx.language.clone();
                                    if let Some(doc_mut) = self.editor.document_by_uri_mut(&uri) {
                                        if let Some(entry) = doc_mut
                                            .language_servers_of_virtual_doc
                                            .get_mut(&language)
                                        {
                                            entry.touch();
                                        }
                                    }

                                    let language_id = self
                                        .editor
                                        .syn_loader
                                        .language_config_for_language_id(&language)
                                        .map(|config| {
                                            config
                                                .language_server_language_id
                                                .as_deref()
                                                .unwrap_or(&config.language_id)
                                                .to_owned()
                                        })
                                        .unwrap_or(language);
                                    Self::on_request(
                                        req,
                                        self.sender.clone(),
                                        vec![ls],
                                        Some(language_id),
                                    );
                                } else {
                                    self.respond(create_error_response(
                                        &req.id,
                                        format!(
                                            "No available language server for {:?}.",
                                            req.method
                                        ),
                                    ));
                                }
                                return Ok(());
                            }
                        }
                        let language_servers = doc.get_all_language_servers();
                        if language_servers.is_empty() {
                            self.respond(create_error_response(
                                &req.id,
                                format!("No available language server for {:?}.", req.method),
                            ));
                            debug!("No available language server for {:?}.", req.method);
                            return Ok(());
                        }
                        let language_id = doc.language_id().map(str::to_owned);
                        Self::on_request(req, self.sender.clone(), language_servers, language_id);
                    }
                    Err(response) => self.respond(response),
                }
            }
            Message::Notification(not) => self.on_notification(not)?,
            Message::Response(resp) => self.complete_request(resp),
        }
        Ok(())
    }

    fn on_request(
        req: msg::Request,
        response_sender: Sender<Message>,
        language_servers: Vec<Arc<Client>>,
        language_id: Option<String>,
    ) {
        let mut dispatcher = RequestDispatcher {
            req: Some(req),
            sender: response_sender,
            language_servers,
        };

        dispatcher
            .on::<lsp_types::request::CallHierarchyPrepare, _, _>(
                crate::hierarchy::prepare::<lsp_types::request::CallHierarchyPrepare>,
            )
            .on::<lsp_types::request::CallHierarchyIncomingCalls, _, _>(
                crate::hierarchy::expand::<lsp_types::request::CallHierarchyIncomingCalls>,
            )
            .on::<lsp_types::request::CallHierarchyOutgoingCalls, _, _>(
                crate::hierarchy::expand::<lsp_types::request::CallHierarchyOutgoingCalls>,
            )
            .on::<crate::hierarchy::TypeHierarchyPrepare, _, _>(
                crate::hierarchy::prepare::<crate::hierarchy::TypeHierarchyPrepare>,
            )
            .on::<crate::hierarchy::TypeHierarchySupertypes, _, _>(
                crate::hierarchy::expand::<crate::hierarchy::TypeHierarchySupertypes>,
            )
            .on::<crate::hierarchy::TypeHierarchySubtypes, _, _>(
                crate::hierarchy::expand::<crate::hierarchy::TypeHierarchySubtypes>,
            )
            .on::<lsp_types::request::WorkspaceSymbolRequest, _, _>(
                crate::workspace_symbols::search,
            )
            .on::<lsp_types::request::WorkspaceSymbolResolve, _, _>(
                crate::workspace_symbols::resolve,
            )
            .on::<lsp_types::request::GotoDefinition, _, _>(move |req, params, servers| {
                handlers::request::handle_goto_definition::<lsp_types::request::GotoDefinition>(
                    req,
                    params,
                    servers,
                    language_id.clone(),
                )
            })
            .on::<lsp_types::request::GotoImplementation, _, _>(|req, params, servers| {
                handlers::request::handle_goto_definition::<lsp_types::request::GotoImplementation>(
                    req, params, servers, None,
                )
            })
            .on::<lsp_types::request::GotoDeclaration, _, _>(|req, params, servers| {
                handlers::request::handle_goto_definition::<lsp_types::request::GotoDeclaration>(
                    req, params, servers, None,
                )
            })
            .on::<lsp_types::request::GotoTypeDefinition, _, _>(|req, params, servers| {
                handlers::request::handle_goto_definition::<lsp_types::request::GotoTypeDefinition>(
                    req, params, servers, None,
                )
            })
            .on::<lsp_types::request::References, _, _>(handlers::request::handle_goto_references)
            .on::<lsp_types::request::Completion, _, _>(handlers::request::handle_completion)
            .on::<lsp_types::request::ResolveCompletionItem, _, _>(
                handlers::request::handle_completion_resolve,
            )
            .on::<lsp_types::request::CodeActionResolveRequest, _, _>(
                handlers::request::handle_code_action_resolve,
            )
            .on::<lsp_types::request::Formatting, _, _>(handlers::request::handle_formating)
            .on::<lsp_types::request::HoverRequest, _, _>(handlers::request::handle_hover)
            .on::<lsp_types::request::ExecuteCommand, _, _>(
                handlers::request::handle_execute_command,
            )
            .on::<lsp_types::request::SignatureHelpRequest, _, _>(
                handlers::request::handle_signature_help,
            )
            .on::<lsp_types::request::Rename, _, _>(handlers::request::handle_rename)
            .on::<lsp_ext::GetCommands, _, _>(handlers::request::handle_get_commands)
            .on::<lsp_ext::GetWorkspaceInfo, _, _>(handlers::request::handle_get_workspace_info)
            .on::<lsp_ext::GetLanguagesConfig, _, _>(handlers::request::handle_get_languages_config)
            .on::<lsp_ext::ViewFileText, _, _>(handlers::request::handle_view_file_text)
            .on::<lsp_types::request::InlayHintRequest, _, _>(handlers::request::handle_inlay_hints)
            .on::<lsp_types::request::DocumentHighlightRequest, _, _>(
                handlers::request::handle_document_highlight,
            )
            .on::<lsp_types::request::CodeLensRequest, _, _>(handlers::request::handle_code_lens)
            .on::<lsp_types::request::CodeLensResolve, _, _>(
                handlers::request::handle_code_lens_resolve,
            )
            .on::<lsp_types::request::DocumentSymbolRequest, _, _>(
                handlers::request::handle_document_symbols,
            )
            .on::<RustAnalyzerExpandMacro, _, _>(handlers::request::handle_ra_expand_macro)
            .on::<lsp_ext::ForwardRequest, _, _>(handlers::request::handle_forward_request)
            .finish();
    }

    fn on_notification(&mut self, not: msg::Notification) -> Result<()> {
        use lsp_types::notification as notfis;

        NotificationDispatcher {
            not: Some(not),
            app: self,
        }
        .on_sync_mut_with_virtual_doc::<notfis::DidOpenTextDocument>(
            handlers::notification::handle_did_open_text_document,
        )?
        .on_sync_mut_with_virtual_doc::<notfis::DidChangeTextDocument>(
            handlers::notification::handle_did_change_text_document,
        )?
        .on_sync_mut::<notfis::WillSaveTextDocument>(
            handlers::notification::handle_will_save_text_document,
        )?
        .on_sync_mut::<notfis::DidSaveTextDocument>(
            handlers::notification::handle_did_save_text_document,
        )?
        .on_sync_mut::<notfis::DidCloseTextDocument>(
            handlers::notification::handle_did_close_text_document,
        )?
        .on_sync_mut::<lsp_ext::CustomizeCancel>(handlers::notification::handle_cancel)?
        .on_sync_mut::<lsp_ext::EmacsLargeFileLoadStart>(
            handlers::notification::handle_large_file_load_start,
        )?
        .on_sync_mut::<lsp_ext::EmacsLargeFileChunk>(
            handlers::notification::handle_large_file_chunk,
        )?
        .on_sync_mut::<lsp_ext::EmacsLargeFileLoadCancel>(
            handlers::notification::handle_large_file_load_cancel,
        )?
        .on_sync_mut::<lsp_ext::DidFocusTextDocument>(
            handlers::notification::handle_did_focus_text_document,
        )?
        .on_sync_mut::<notfis::Exit>(handlers::notification::handle_exit)?
        .finish();

        Ok(())
    }

    fn forward_show_message_request(
        &mut self,
        server_id: usize,
        server_request_id: crate::msg::RequestId,
        params: lsp_types::ShowMessageRequestParams,
    ) {
        let editor_request_id = self
            .send_request_returning_id::<lsp_types::request::ShowMessageRequest>(
                params,
                Self::on_show_message_response,
            );
        self.pending_editor_choices
            .insert(editor_request_id, (server_id, server_request_id));
    }

    fn on_show_message_response(&mut self, response: msg::Response) {
        let Some((server_id, server_request_id)) = self.pending_editor_choices.remove(&response.id)
        else {
            warn!(
                "no pending showMessageRequest for response id={:?}",
                response.id
            );
            return;
        };
        let Some(language_server) = self.editor.language_server_by_id(server_id) else {
            warn!("language server {server_id} is gone; dropping showMessageRequest reply");
            return;
        };
        if let Some(error) = &response.error {
            warn!("editor failed to answer showMessageRequest: {error:?}");
        }
        let result = response.result.unwrap_or(Value::Null);
        tokio::spawn(language_server.reply(server_request_id, Ok(result)));
    }

    /// Install a Java language server and report progress to the editor.
    fn handle_install_java_server(&mut self, req: msg::Request) {
        let params: lsp_ext::InstallJavaServerParams =
            match serde_json::from_value(req.params.params.clone()) {
                Ok(params) => params,
                Err(err) => {
                    self.respond(Response::new_err(
                        req.id,
                        jsonrpc::ErrorCode::InvalidParams,
                        format!("invalid installJavaServer params: {err}"),
                    ));
                    return;
                }
            };

        let sender = self.sender.clone();
        let id = req.id.clone();
        let install_dir = std::path::PathBuf::from(&params.install_dir);
        let backend = params.backend;
        let version = params.version.clone();
        let force = params.force;

        let install_guard = match crate::java_install::ServerInstallGuard::acquire(&install_dir) {
            Ok(guard) => guard,
            Err(err) => {
                self.respond(Response::new_err(
                    req.id,
                    jsonrpc::ErrorCode::InvalidRequest,
                    err.to_string(),
                ));
                return;
            }
        };

        tokio::spawn(async move {
            let _install_guard = install_guard;
            let progress = Self::install_progress_reporter(sender.clone(), backend.tag());
            let outcome = crate::java_install::install(
                backend,
                &install_dir,
                version.as_deref(),
                force,
                progress,
            )
            .await;

            let response = match outcome {
                Ok((launcher, version, already_installed)) => Response::new_ok(
                    id,
                    lsp_ext::InstallJavaServerResult {
                        launcher_path: launcher.to_string_lossy().to_string(),
                        version,
                        already_installed,
                    },
                ),
                Err(err) => {
                    Response::new_err(id, jsonrpc::ErrorCode::InternalError, format!("{err:#}"))
                }
            };
            let _ = sender.send(response.into());
        });
    }

    fn handle_install_lombok(&mut self, req: msg::Request) {
        let params: lsp_ext::InstallLombokParams =
            match serde_json::from_value(req.params.params.clone()) {
                Ok(params) => params,
                Err(err) => {
                    self.respond(Response::new_err(
                        req.id,
                        jsonrpc::ErrorCode::InvalidParams,
                        format!("invalid installLombok params: {err}"),
                    ));
                    return;
                }
            };

        let sender = self.sender.clone();
        let id = req.id.clone();
        let install_dir = std::path::PathBuf::from(&params.install_dir);
        let force = params.force;

        tokio::spawn(async move {
            let progress = Self::install_progress_reporter(sender.clone(), "Lombok");
            let outcome = crate::java_install::install_lombok(&install_dir, force, progress).await;

            let response = match outcome {
                Ok((jar, version, already_installed)) => Response::new_ok(
                    id,
                    lsp_ext::InstallLombokResult {
                        jar_path: jar.to_string_lossy().to_string(),
                        version,
                        already_installed,
                    },
                ),
                Err(err) => {
                    Response::new_err(id, jsonrpc::ErrorCode::InternalError, format!("{err:#}"))
                }
            };
            let _ = sender.send(response.into());
        });
    }

    fn handle_install_java_debug(&mut self, req: msg::Request) {
        let params: lsp_ext::InstallJavaDebugParams =
            match serde_json::from_value(req.params.params.clone()) {
                Ok(params) => params,
                Err(err) => {
                    self.respond(Response::new_err(
                        req.id,
                        jsonrpc::ErrorCode::InvalidParams,
                        format!("invalid installJavaDebug params: {err}"),
                    ));
                    return;
                }
            };

        let sender = self.sender.clone();
        let id = req.id.clone();
        let install_dir = std::path::PathBuf::from(&params.install_dir);
        let force = params.force;

        tokio::spawn(async move {
            let progress = Self::install_progress_reporter(sender.clone(), "Java Debug");
            let outcome =
                crate::java_install::install_java_debug(&install_dir, force, progress).await;

            let response = match outcome {
                Ok((jar, version, already_installed)) => Response::new_ok(
                    id,
                    lsp_ext::InstallJavaDebugResult {
                        jar_path: jar.to_string_lossy().to_string(),
                        version,
                        already_installed,
                    },
                ),
                Err(err) => {
                    Response::new_err(id, jsonrpc::ErrorCode::InternalError, format!("{err:#}"))
                }
            };
            let _ = sender.send(response.into());
        });
    }

    fn handle_install_spring(&mut self, req: msg::Request) {
        let params: lsp_ext::InstallSpringParams = match serde_json::from_value(req.params.params) {
            Ok(params) => params,
            Err(err) => {
                self.respond(Response::new_err(
                    req.id,
                    jsonrpc::ErrorCode::InvalidParams,
                    err.to_string(),
                ));
                return;
            }
        };
        let sender = self.sender.clone();
        tokio::spawn(async move {
            let progress = Self::install_progress_reporter(sender.clone(), "Spring Boot");
            let result = crate::java_install::install_spring(
                std::path::Path::new(&params.install_dir),
                params.version.as_deref(),
                params.force,
                progress,
            )
            .await;
            let response = match result {
                Ok((launcher, info, already_installed)) => Response::new_ok(
                    req.id,
                    lsp_ext::InstallSpringResult {
                        launcher_path: launcher.to_string_lossy().into(),
                        version: info.version,
                        already_installed,
                    },
                ),
                Err(err) => Response::new_err(
                    req.id,
                    jsonrpc::ErrorCode::InternalError,
                    format!("{err:#}"),
                ),
            };
            let _ = sender.send(response.into());
        });
    }

    fn handle_set_java_project_runtime(&mut self, req: msg::Request) {
        let params: lsp_ext::SetJavaProjectRuntimeParams = if req.params.params.is_null() {
            lsp_ext::SetJavaProjectRuntimeParams { java_home: None }
        } else {
            match serde_json::from_value(req.params.params.clone()) {
                Ok(params) => params,
                Err(err) => {
                    self.respond(Response::new_err(
                        req.id,
                        jsonrpc::ErrorCode::InvalidParams,
                        format!("invalid setJavaProjectRuntime params: {err}"),
                    ));
                    return;
                }
            }
        };

        let doc = match self.get_working_document(&req) {
            Ok(doc) => doc,
            Err(response) => {
                self.respond(response);
                return;
            }
        };
        let Some(root_path) = Self::java_project_root(doc) else {
            self.respond(Response::new_err(
                req.id,
                jsonrpc::ErrorCode::InvalidRequest,
                "Cannot set Java project JDK before this document has a workspace".to_string(),
            ));
            return;
        };

        match crate::java_project::set_project_runtime_override(
            &root_path,
            params.java_home.as_deref(),
        ) {
            Ok((java_home, java_version)) => self.respond(Response::new_ok(
                req.id,
                lsp_ext::SetJavaProjectRuntimeResult {
                    workspace_root: root_path.to_string_lossy().to_string(),
                    java_home: java_home.map(|path| path.to_string_lossy().to_string()),
                    java_version,
                },
            )),
            Err(error) => self.respond(Response::new_err(
                req.id,
                jsonrpc::ErrorCode::InvalidParams,
                error,
            )),
        }
    }

    fn handle_get_java_project_runtime(&mut self, req: msg::Request) {
        let doc = match self.get_working_document(&req) {
            Ok(doc) => doc,
            Err(response) => {
                self.respond(response);
                return;
            }
        };
        let Some(root_path) = Self::java_project_root(doc) else {
            self.respond(Response::new_err(
                req.id,
                jsonrpc::ErrorCode::InvalidRequest,
                "Cannot get Java project JDK before this document has a workspace".to_string(),
            ));
            return;
        };

        let doc_path = doc.path();
        let runtime = crate::java_project::project_runtime_for(&root_path, doc_path.as_deref());
        self.respond(Response::new_ok(
            req.id,
            lsp_ext::GetJavaProjectRuntimeResult {
                workspace_root: root_path.to_string_lossy().to_string(),
                java_home: runtime
                    .as_ref()
                    .map(|runtime| runtime.java_home.to_string_lossy().to_string()),
                java_version: runtime.as_ref().map(|runtime| runtime.java_version.clone()),
                source: runtime.as_ref().map(|runtime| runtime.source.to_string()),
                java_exec: runtime.as_ref().map(|runtime| {
                    crate::java_project::java_executable(&runtime.java_home)
                        .to_string_lossy()
                        .to_string()
                }),
            },
        ));
    }

    fn install_progress_reporter(
        sender: Sender<Message>,
        label: &'static str,
    ) -> crate::java_install::ProgressFn {
        Box::new(move |percentage: Option<u32>, message: String| {
            let notification = msg::Notification::new(
                lsp_ext::InstallProgress::METHOD.to_string(),
                lsp_ext::InstallProgressParams {
                    label: label.to_string(),
                    message,
                    percentage,
                },
            );
            let _ = sender.send(notification.into());
        })
    }

    fn handle_list_workspaces(&mut self, req: &msg::Request) {
        self.respond(Response::new_ok(
            req.id.clone(),
            self.collect_workspace_items(),
        ));
    }

    fn java_workspace_cleanup_blocks(&self, uri: &str) -> bool {
        self.java_workspace_cleanups.iter().any(|(root, context)| {
            context.closed_documents.iter().any(|path| {
                path == uri
                    || lsp_types::Url::from_file_path(path)
                        .ok()
                        .as_ref()
                        .is_some_and(|url| url.as_str() == uri)
            }) || lsp_types::Url::parse(uri)
                .ok()
                .and_then(|url| url.to_file_path().ok())
                .is_some_and(|path| {
                    path.extension()
                        .is_some_and(|extension| extension == "java")
                        && crate::utils::path::canonicalize(path).starts_with(root)
                })
        })
    }

    fn begin_java_workspace_cleanup(&mut self, req: &msg::Request) -> Result<()> {
        let params: lsp_ext::CleanJavaWorkspaceParams =
            serde_json::from_value(req.params.params.clone())?;
        let server_name = params.server_name.as_deref().unwrap_or("jdtls");
        let doc = self.get_working_document(req).map_err(|_| {
            anyhow::anyhow!("Open a Java project buffer before cleaning its workspace")
        })?;
        let jdt = doc
            .get_all_language_servers()
            .into_iter()
            .find(|client| client.name() == server_name)
            .ok_or_else(|| anyhow::anyhow!("No {} server serves this document", server_name))?;
        let cache = jdt.jdtls_data_path.clone().ok_or_else(|| anyhow::anyhow!(
            "Cannot determine JDTLS cache directory; configure an explicit -data argument for this launcher"
        ))?;
        let root = crate::utils::path::canonicalize(&jdt.root_path);
        if Self::client_workspace_roots(&jdt).len() > 1 {
            anyhow::bail!("JDTLS serves several workspaces; cannot clean only this workspace");
        }
        for other in self.editor.language_servers.iter_clients() {
            if other.id() != jdt.id()
                && other.jdtls_data_path.as_ref().is_some_and(|path| {
                    crate::java_workspace::normalized(path)
                        == crate::java_workspace::normalized(&cache)
                })
            {
                anyhow::bail!("Another JDTLS server shares this cache; configure separate -data directories before cleaning");
            }
        }
        crate::java_workspace::validate_cache(&cache, &root)?;
        let reservation = crate::java_workspace::Reservation::acquire(&cache)?;
        let mut clients = vec![jdt.clone()];
        // STS must re-register its classpath listeners against the replacement JDT.
        for client in self.editor.language_servers.get_project_clients(&jdt) {
            if client.name() == "spring-boot" {
                if Self::client_workspace_roots(&client).len() > 1 {
                    anyhow::bail!(
                        "Spring server serves several workspaces; cannot clean only this workspace"
                    );
                }
                clients.push(client);
            }
        }
        let ids: HashSet<_> = clients.iter().map(|client| client.id()).collect();
        let document_ids: Vec<_> = self
            .editor
            .documents()
            .filter_map(|doc| {
                Self::document_clients(doc)
                    .iter()
                    .any(|client| ids.contains(&client.id()))
                    .then_some(doc.id())
            })
            .collect();
        let mut closed_documents = Vec::new();
        for id in document_ids {
            if let Some(doc) = self.editor.documents.get(&id) {
                closed_documents.push(
                    doc.path()
                        .map(|path| path.to_string_lossy().to_string())
                        .unwrap_or_else(|| doc.uri.to_string()),
                );
                // Other servers may also serve this Java document. Close/reopen
                // that document, while keeping those unrelated processes alive.
                for client in Self::document_clients(doc) {
                    let _ = client.text_document_did_close(lsp_types::DidCloseTextDocumentParams {
                        text_document: lsp_types::TextDocumentIdentifier {
                            uri: doc.uri.clone(),
                        },
                    });
                }
            }
            self.editor.remove_document_by_id(id);
        }
        for client in &clients {
            self.spring_bridge.remove_client(client.id());
            self.work_done_progress.remove_server(client.id());
            self.editor.language_servers.remove_by_id(client.id());
        }
        handlers::request::clear_completion_cache();
        self.java_workspace_cleanups.insert(
            root.clone(),
            crate::java_workspace::CleanupContext {
                request_id: req.id.clone(),
                cache: cache.clone(),
                closed_documents,
                _reservation: reservation,
            },
        );
        let sender = self.java_workspace_cleanup_tx.clone();
        tokio::spawn(async move {
            let result = async {
                for result in futures_util::future::join_all(
                    clients.iter().map(|client| client.stop_and_wait()),
                )
                .await
                {
                    result.map_err(|error| error.to_string())?;
                }
                let workspace = root.clone();
                tokio::task::spawn_blocking(move || {
                    crate::java_workspace::backup_cache(&cache, &workspace)
                })
                .await
                .map_err(|error| error.to_string())?
                .map_err(|error| format!("{error:#}"))
            }
            .await;
            let _ = sender.send((root, result));
        });
        Ok(())
    }

    fn complete_java_workspace_cleanup(
        &mut self,
        root: PathBuf,
        result: Result<Option<PathBuf>, String>,
    ) {
        let Some(context) = self.java_workspace_cleanups.remove(&root) else {
            return;
        };
        let (backup, error) = match result {
            Ok(backup) => (backup, None),
            Err(error) => (None, Some(error)),
        };
        // Release the reservation before Emacs reopens documents and starts JDT.
        drop(context._reservation);
        self.respond(Response::new_ok(
            context.request_id,
            lsp_ext::CleanJavaWorkspaceResult {
                workspace_root: root.to_string_lossy().to_string(),
                cache_directory: context.cache.to_string_lossy().to_string(),
                backup_directory: backup.map(|path| path.to_string_lossy().to_string()),
                closed_documents: context.closed_documents,
                error,
            },
        ));
    }

    fn handle_shutdown_workspace(&mut self, req: &msg::Request) {
        let params = match from_json::<lsp_ext::ShutdownWorkspaceParams>(
            lsp_ext::ShutdownWorkspace::METHOD,
            &req.params.params,
        ) {
            Ok(params) => params,
            Err(error) => {
                self.respond(Response::new_err(
                    req.id.clone(),
                    jsonrpc::ErrorCode::InvalidParams,
                    error.to_string(),
                ));
                return;
            }
        };

        let workspace_root = crate::utils::path::canonicalize(&params.workspace_root);
        let workspace_root_string = workspace_root.to_string_lossy().to_string();

        let document_ids_to_remove: Vec<DocumentId> = self
            .editor
            .documents()
            .filter_map(|doc| {
                if Self::document_workspace_roots(doc)
                    .into_iter()
                    .any(|root| crate::utils::path::canonicalize(root) == workspace_root)
                {
                    Some(doc.id())
                } else {
                    None
                }
            })
            .collect();

        if document_ids_to_remove.is_empty() {
            self.respond(Response::new_ok(
                req.id.clone(),
                lsp_ext::ShutdownWorkspaceResult {
                    workspace_root: workspace_root_string,
                    closed_documents: Vec::new(),
                    shutdown_servers: Vec::new(),
                    preserved_servers: Vec::new(),
                },
            ));
            return;
        }

        let mut affected_client_ids = BTreeSet::new();
        let mut closed_documents = Vec::new();

        for document_id in &document_ids_to_remove {
            if let Some(doc) = self.editor.document_mut(*document_id) {
                let close_params = lsp_types::DidCloseTextDocumentParams {
                    text_document: lsp_types::TextDocumentIdentifier {
                        uri: doc.uri.clone(),
                    },
                };
                closed_documents.push(
                    doc.path()
                        .map(|path| path.to_string_lossy().to_string())
                        .unwrap_or_else(|| doc.uri.to_string()),
                );

                let mut closed_for_client = HashSet::new();
                for client in Self::document_clients(doc) {
                    affected_client_ids.insert(client.id());
                    if closed_for_client.insert(client.id()) {
                        if let Err(error) = client.text_document_did_close(close_params.clone()) {
                            error!("Failed to send didClose to {}: {error}", client.name());
                        }
                    }
                }
            }
        }

        for document_id in &document_ids_to_remove {
            self.editor.remove_document_by_id(*document_id);
        }

        let remaining_client_ids: HashSet<usize> = self
            .editor
            .documents()
            .flat_map(|doc| {
                Self::document_clients(doc)
                    .into_iter()
                    .map(|client| client.id())
            })
            .collect();

        let mut shutdown_servers = Vec::new();
        let mut preserved_servers = Vec::new();

        for client_id in affected_client_ids {
            let Some(client) = self.editor.language_servers.get_by_id(client_id) else {
                continue;
            };

            let roots_before = Self::client_workspace_roots(&client);
            let remaining_workspace_count = roots_before
                .iter()
                .filter(|root| crate::utils::path::canonicalize(root) != workspace_root)
                .count();
            let shared = remaining_client_ids.contains(&client_id) || remaining_workspace_count > 0;

            if shared {
                if let Some(removed_workspace) = client.remove_workspace_folder(&workspace_root) {
                    if let Err(error) =
                        client.did_change_workspace(Vec::new(), vec![removed_workspace])
                    {
                        warn!(
                            "Failed to remove workspace folder from {}: {error}",
                            client.name()
                        );
                    }
                }
                preserved_servers.push(Self::workspace_language_server_info(
                    &client,
                    true,
                    remaining_workspace_count.max(1),
                ));
            } else {
                shutdown_servers.push(Self::workspace_language_server_info(&client, false, 0));
                self.spring_bridge.remove_client(client_id);
                self.work_done_progress.remove_server(client_id);
                self.editor.language_servers.remove_by_id(client_id);
                tokio::spawn(async move {
                    let _ = client.shutdown_and_exit().await;
                });
            }
        }

        handlers::request::clear_completion_cache();

        self.respond(Response::new_ok(
            req.id.clone(),
            lsp_ext::ShutdownWorkspaceResult {
                workspace_root: workspace_root_string,
                closed_documents,
                shutdown_servers,
                preserved_servers,
            },
        ));
    }

    fn collect_workspace_items(&self) -> Vec<lsp_ext::WorkspaceListItem> {
        struct WorkspaceAccumulator {
            document_paths: BTreeSet<String>,
            server_ids: BTreeSet<usize>,
        }

        let mut workspaces: BTreeMap<String, WorkspaceAccumulator> = BTreeMap::new();
        let mut clients: HashMap<usize, Arc<Client>> = HashMap::new();
        let mut client_workspace_roots: HashMap<usize, BTreeSet<String>> = HashMap::new();

        for doc in self.editor.documents() {
            let Some(doc_path) = doc.path() else {
                continue;
            };
            let doc_path_string = doc_path.to_string_lossy().to_string();

            let clients_for_doc = Self::document_clients(doc);

            for client in clients_for_doc {
                clients.entry(client.id()).or_insert_with(|| client.clone());
                let roots = Self::workspace_roots_for_doc(&client, &doc_path);
                for root in roots {
                    client_workspace_roots
                        .entry(client.id())
                        .or_default()
                        .insert(root.clone());
                    let workspace =
                        workspaces
                            .entry(root)
                            .or_insert_with(|| WorkspaceAccumulator {
                                document_paths: BTreeSet::new(),
                                server_ids: BTreeSet::new(),
                            });
                    workspace.document_paths.insert(doc_path_string.clone());
                    workspace.server_ids.insert(client.id());
                }
            }
        }

        workspaces
            .into_iter()
            .map(|(workspace_root, workspace)| {
                let language_servers = workspace
                    .server_ids
                    .into_iter()
                    .filter_map(|client_id| {
                        let client = clients.get(&client_id)?;
                        let workspace_count = client_workspace_roots
                            .get(&client_id)
                            .map(|roots| roots.len())
                            .unwrap_or(1);
                        Some(Self::workspace_language_server_info(
                            client,
                            workspace_count > 1,
                            workspace_count,
                        ))
                    })
                    .collect();

                lsp_ext::WorkspaceListItem {
                    workspace_root,
                    document_count: workspace.document_paths.len(),
                    language_servers,
                }
            })
            .collect()
    }

    fn document_clients(doc: &Document) -> Vec<Arc<Client>> {
        doc.language_servers
            .values()
            .cloned()
            .into_iter()
            .chain(
                doc.language_servers_of_virtual_doc
                    .values()
                    .map(|entry| entry.client.clone()),
            )
            .collect()
    }

    fn document_workspace_roots(doc: &Document) -> BTreeSet<PathBuf> {
        let Some(doc_path) = doc.path() else {
            return BTreeSet::new();
        };

        Self::document_clients(doc)
            .into_iter()
            .flat_map(|client| Self::workspace_roots_for_doc(&client, &doc_path))
            .map(PathBuf::from)
            .collect()
    }

    fn java_project_root(doc: &Document) -> Option<PathBuf> {
        let language_servers = doc.get_all_language_servers();
        language_servers
            .iter()
            .find(|client| client.name() == "jdtls")
            .or_else(|| language_servers.first())
            .map(|client| client.root_path.clone())
    }

    fn workspace_roots_for_doc(client: &Arc<Client>, doc_path: &Path) -> Vec<String> {
        let workspace_roots: Vec<PathBuf> = client
            .workspace_folders
            .lock()
            .iter()
            .filter_map(|workspace| workspace.uri.to_file_path().ok())
            .collect();
        let matching_roots: Vec<PathBuf> = workspace_roots
            .iter()
            .filter(|root| Self::path_is_in_workspace(doc_path, root))
            .map(crate::utils::path::canonicalize)
            .collect();

        if matching_roots.is_empty() && Self::path_is_in_workspace(doc_path, &client.root_path) {
            vec![crate::utils::path::canonicalize(&client.root_path)
                .to_string_lossy()
                .to_string()]
        } else {
            let max_component_count = matching_roots
                .iter()
                .map(|root| root.components().count())
                .max()
                .unwrap_or_default();
            matching_roots
                .into_iter()
                .filter(|root| root.components().count() == max_component_count)
                .map(|root| root.to_string_lossy().to_string())
                .collect()
        }
    }

    fn client_workspace_roots(client: &Arc<Client>) -> Vec<PathBuf> {
        let roots: Vec<PathBuf> = client
            .workspace_folders
            .lock()
            .iter()
            .filter_map(|workspace| workspace.uri.to_file_path().ok())
            .collect();
        if roots.is_empty() {
            vec![client.root_path.clone()]
        } else {
            roots
        }
    }

    fn workspace_language_server_info(
        client: &Arc<Client>,
        shared: bool,
        workspace_count: usize,
    ) -> lsp_ext::WorkspaceLanguageServerInfo {
        lsp_ext::WorkspaceLanguageServerInfo {
            id: client.id(),
            name: client.name().to_string(),
            root_path: client.root_path.to_string_lossy().to_string(),
            shared,
            workspace_count,
        }
    }

    fn path_is_in_workspace(path: &Path, workspace_root: &Path) -> bool {
        let path = crate::utils::path::canonicalize(path);
        let workspace_root = crate::utils::path::canonicalize(workspace_root);
        path == workspace_root || path.starts_with(workspace_root)
    }

    fn handle_workspace_restart(&mut self, req: &msg::Request) {
        match self.get_working_document(req) {
            Ok(doc) => {
                // A virtual document (a `jar:`/`jrt:` decompiled source) can lack a
                // language config, and always lacks a path; unwrapping either here
                // took the whole proxy down when restart was invoked from such a
                // buffer.
                let Some(config) = doc.language_config().cloned() else {
                    self.respond(Response::new_err(
                        req.id.clone(),
                        jsonrpc::ErrorCode::InvalidRequest,
                        "Cannot restart the workspace from a document with no language config"
                            .to_string(),
                    ));
                    return;
                };
                let doc_path = doc.path();
                let old_client_ids: Vec<usize> = doc
                    .get_all_language_servers()
                    .iter()
                    .map(|ls| ls.id())
                    .collect();
                let document_ids_to_refresh: Vec<DocumentId> = self
                    .editor
                    .documents()
                    .filter_map(|doc| {
                        if doc
                            .language_servers()
                            .any(|ls| old_client_ids.contains(&ls.id()))
                        {
                            Some(doc.id())
                        } else {
                            None
                        }
                    })
                    .collect();
                for id in &old_client_ids {
                    self.spring_bridge.remove_client(*id);
                    self.work_done_progress.remove_server(*id);
                }
                match self
                    .editor
                    .language_servers
                    .restart_v2(&config, old_client_ids, doc_path)
                {
                    Ok(_) => {
                        let mut doc_paths: Vec<String> = vec![];
                        for document_id in document_ids_to_refresh {
                            if let Some(doc) = self.editor.documents.remove(&document_id) {
                                // Virtual documents have no path to report back;
                                // skip them rather than panicking.
                                if let Some(path) = doc.path() {
                                    doc_paths.push(path.to_string_lossy().to_string());
                                }
                            }
                        }
                        self.respond(Response::new_ok(req.id.clone(), doc_paths));
                    }
                    Err(error) => {
                        self.respond(create_error_response(&req.id, error.to_string()));
                    }
                };
            }
            Err(response) => self.respond(response),
        }
    }

    fn get_working_document(&self, req: &msg::Request) -> Result<&Document, Response> {
        match &req.params.uri {
            Some(uri) => {
                if let Some(doc) = self
                    .editor
                    .document_by_uri(&lsp_types::Url::parse(uri).unwrap())
                {
                    Ok(doc)
                } else {
                    Err(create_error_response_with_uri(
                        &req.id,
                        "No document opened".to_string(),
                        uri.clone(),
                    ))
                }
            }
            None => Err(create_error_response(
                &req.id,
                "No uri provided".to_string(),
            )),
        }
    }
}

#[cfg(test)]
mod java_workspace_cleanup_tests {
    use super::*;
    use serde_json::json;
    use std::fs;

    fn app() -> (Application, crossbeam_channel::Receiver<Message>) {
        let (sender, receiver) = crossbeam_channel::unbounded();
        (
            Application::new(
                sender,
                syntax::Configuration {
                    language: vec![serde_json::from_value(json!({
                        "name":"java", "file-types":["java"],
                        "language-servers":["jdtls", "spring-boot", "other"]
                    }))
                    .unwrap()],
                    language_server: Default::default(),
                },
            ),
            receiver,
        )
    }

    fn cache(path: &std::path::Path) {
        fs::create_dir_all(path.join(".metadata/.plugins/org.eclipse.jdt.core")).unwrap();
        fs::write(path.join(".metadata/.lock"), "").unwrap();
        fs::write(
            path.join(".metadata/.plugins/org.eclipse.jdt.core/index"),
            "old",
        )
        .unwrap();
    }

    async fn attach(
        app: &mut Application,
        name: &str,
        id: usize,
        root: &std::path::Path,
        cache: Option<PathBuf>,
    ) -> Arc<Client> {
        let uri = lsp_types::Url::from_file_path(root.join("App.java")).unwrap();
        let (mut client, _) = crate::test_support::client(name, id, uri.as_str(), true).await;
        let mutable = Arc::get_mut(&mut client).unwrap();
        mutable.root_path = root.to_owned();
        mutable.jdtls_data_path = cache;
        app.editor
            .language_servers
            .insert_test_client(client.clone());
        let doc_id = app
            .editor
            .document_by_uri(&uri)
            .map(|doc| doc.id())
            .unwrap_or_else(|| app.editor.new_document(&uri, Some("java")).id());
        app.editor
            .documents
            .get_mut(&doc_id)
            .unwrap()
            .language_servers
            .insert(name.into(), client.clone());
        client
    }

    fn request(root: &std::path::Path) -> msg::Request {
        let mut request = crate::test_support::request(lsp_ext::CleanJavaWorkspace::METHOD, None);
        request.params.uri = Some(
            lsp_types::Url::from_file_path(root.join("App.java"))
                .unwrap()
                .into(),
        );
        request.params.params = json!({"serverName":"jdtls"});
        request
    }

    #[tokio::test]
    async fn test_cleanup_dispatch_is_async_and_releases_cache_before_reply() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().join("project");
        fs::create_dir(&root).unwrap();
        fs::write(root.join("App.java"), "saved").unwrap();
        let data = temp.path().join("cache");
        cache(&data);
        let (mut app, receiver) = app();
        let jdt = attach(&mut app, "jdtls", 1, &root, Some(data.clone())).await;
        let spring = attach(&mut app, "spring-boot", 2, &root, None).await;
        let other_root = temp.path().join("other");
        let other = attach(&mut app, "other", 3, &other_root, None).await;
        app.handle_msg(request(&root).into()).await.unwrap();
        assert!(
            receiver.try_recv().is_err(),
            "must reply only after the worker completes"
        );
        assert!(crate::java_workspace::is_cleaning(&data));
        assert!(data.exists());
        assert_eq!(app.editor.documents().count(), 1);
        assert!(app.editor.language_servers.get_by_id(jdt.id()).is_none());
        assert!(app.editor.language_servers.get_by_id(spring.id()).is_none());
        assert!(app.editor.language_servers.get_by_id(other.id()).is_some());

        // A second cleanup cannot replace the first one's pending context.
        app.handle_msg(request(&root).into()).await.unwrap();
        let Message::Response(rejected) = receiver.try_recv().unwrap() else {
            panic!()
        };
        assert!(rejected.error.is_some());
        assert_eq!(app.java_workspace_cleanups.len(), 1);
        // The proxy still services workspace queries while waiting for termination.
        app.handle_msg(crate::test_support::request(lsp_ext::ListWorkspaces::METHOD, None).into())
            .await
            .unwrap();
        let Message::Response(list) = receiver.try_recv().unwrap() else {
            panic!()
        };
        assert!(list.error.is_none());

        let (completed_root, result) = tokio::time::timeout(
            std::time::Duration::from_secs(3),
            app.java_workspace_cleanup_rx.recv(),
        )
        .await
        .unwrap()
        .unwrap();
        assert!(result.is_ok(), "{result:?}");
        app.complete_java_workspace_cleanup(completed_root, result);
        assert!(!crate::java_workspace::is_cleaning(&data));
        let Message::Response(response) = receiver.try_recv().unwrap() else {
            panic!()
        };
        let result: lsp_ext::CleanJavaWorkspaceResult =
            serde_json::from_value(response.result.unwrap()).unwrap();
        assert_eq!(
            result.closed_documents,
            vec![root.join("App.java").to_string_lossy()]
        );
        assert!(result.error.is_none());
        assert_eq!(
            fs::read_to_string(
                PathBuf::from(result.backup_directory.unwrap())
                    .join(".metadata/.plugins/org.eclipse.jdt.core/index")
            )
            .unwrap(),
            "old"
        );
        assert!(!data.exists());
        assert_eq!(fs::read_to_string(root.join("App.java")).unwrap(), "saved");
        assert!(app.java_workspace_cleanups.is_empty());
        other.stop_and_wait().await.unwrap();
    }

    #[tokio::test]
    async fn test_shared_cache_and_multiple_workspaces_reject_before_stopping() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().join("project");
        let other_root = temp.path().join("other");
        let data = temp.path().join("cache");
        cache(&data);
        let (mut app, _) = app();
        let jdt = attach(&mut app, "jdtls", 1, &root, Some(data.clone())).await;
        let other = attach(&mut app, "jdtls", 2, &other_root, Some(data.clone())).await;
        assert!(app
            .begin_java_workspace_cleanup(&request(&root))
            .unwrap_err()
            .to_string()
            .contains("shares this cache"));
        assert_eq!(app.editor.documents().count(), 2);
        assert!(app.java_workspace_cleanups.is_empty());
        assert!(data.exists());
        app.editor.language_servers.remove_by_id(other.id());
        other.stop_and_wait().await.unwrap();
        *jdt.workspace_folders.lock() = [&root, &other_root]
            .into_iter()
            .map(|root| lsp_types::WorkspaceFolder {
                uri: lsp_types::Url::from_directory_path(root).unwrap(),
                name: "project".into(),
            })
            .collect();
        assert!(app
            .begin_java_workspace_cleanup(&request(&root))
            .unwrap_err()
            .to_string()
            .contains("several workspaces"));
        assert!(app.editor.language_servers.get_by_id(jdt.id()).is_some());
        assert!(!crate::java_workspace::is_cleaning(&data));
        jdt.stop_and_wait().await.unwrap();
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn test_worker_failure_returns_documents_to_reopen_and_releases_reservation() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path().join("project");
        let data = temp.path().join("cache");
        cache(&data);
        // Valid workspace metadata, but an unsafe lock discovered by the worker.
        let lock = data.join(".metadata/.lock");
        fs::remove_file(&lock).unwrap();
        fs::write(temp.path().join("external-lock"), "keep").unwrap();
        std::os::unix::fs::symlink(temp.path().join("external-lock"), lock).unwrap();
        let (mut app, receiver) = app();
        attach(&mut app, "jdtls", 1, &root, Some(data.clone())).await;
        app.handle_msg(request(&root).into()).await.unwrap();
        let (root, result) = tokio::time::timeout(
            std::time::Duration::from_secs(3),
            app.java_workspace_cleanup_rx.recv(),
        )
        .await
        .unwrap()
        .unwrap();
        assert!(result.is_err());
        app.complete_java_workspace_cleanup(root, result);
        let Message::Response(response) = receiver.try_recv().unwrap() else {
            panic!()
        };
        let result: lsp_ext::CleanJavaWorkspaceResult =
            serde_json::from_value(response.result.unwrap()).unwrap();
        assert_eq!(result.closed_documents.len(), 1);
        assert!(result.error.unwrap().contains("symlinked workspace lock"));
        assert!(result.backup_directory.is_none());
        assert!(data.exists());
        assert!(!crate::java_workspace::is_cleaning(&data));
    }
}

#[cfg(test)]
mod org_diagnostic_tests {
    use super::should_discard_org_virtual_diagnostics;

    #[test]
    fn discards_diagnostics_from_virtual_document_only_server() {
        assert!(should_discard_org_virtual_diagnostics(2, [1, 2], [3]));
    }

    #[test]
    fn keeps_diagnostics_from_regular_document_server() {
        assert!(!should_discard_org_virtual_diagnostics(2, [1], [2, 3]));
    }

    #[test]
    fn keeps_diagnostics_from_server_registered_for_both_document_types() {
        assert!(!should_discard_org_virtual_diagnostics(2, [1, 2], [2, 3]));
    }
}

#[cfg(test)]
mod install_lifecycle_tests {
    use super::*;

    fn application() -> (Application, crossbeam_channel::Receiver<Message>) {
        let (sender, receiver) = crossbeam_channel::unbounded();
        let app = Application::new(
            sender,
            syntax::Configuration {
                language: vec![],
                language_server: Default::default(),
            },
        );
        (app, receiver)
    }

    #[tokio::test]
    async fn shutdown_rejects_install_and_other_special_requests_before_dispatch() {
        let (mut app, receiver) = application();
        app.shutdown_requested = true;
        for method in [
            lsp_ext::InstallJavaServer::METHOD,
            lsp_ext::InstallLombok::METHOD,
            lsp_ext::InstallJavaDebug::METHOD,
            lsp_ext::InstallSpring::METHOD,
            lsp_ext::ListWorkspaces::METHOD,
            lsp_types::request::CodeActionRequest::METHOD,
        ] {
            let request = crate::test_support::request(method, None);
            let id = request.id.clone();
            app.handle_msg(Message::Request(request)).await.unwrap();
            let Message::Response(response) = receiver.try_recv().unwrap() else {
                panic!("expected rejection")
            };
            assert_eq!(response.id, id);
            let error = response.error.unwrap();
            assert_eq!(error.code, jsonrpc::ErrorCode::InvalidRequest);
            assert_eq!(error.message, "Server is shutting down");
        }
        app.handle_msg(Message::Request(crate::test_support::request(
            lsp_types::request::Shutdown::METHOD,
            None,
        )))
        .await
        .unwrap();
        let Message::Response(response) = receiver.try_recv().unwrap() else {
            panic!("expected shutdown reply")
        };
        assert!(response.error.is_none());
        assert_eq!(response.result, Some(Value::Null));
    }

    #[tokio::test]
    async fn duplicate_install_is_rejected_without_progress_or_network_work() {
        let (mut app, receiver) = application();
        let directory = tempfile::tempdir().unwrap();
        let guard = crate::java_install::ServerInstallGuard::acquire(directory.path()).unwrap();
        let mut request = crate::test_support::request(lsp_ext::InstallJavaServer::METHOD, None);
        request.params.params = json!({
            "backend": "jdtls", "installDir": directory.path(), "version": "1.61.0", "force": false,
        });
        app.handle_msg(Message::Request(request)).await.unwrap();
        let Message::Response(response) = receiver.try_recv().unwrap() else {
            panic!("expected rejection")
        };
        let error = response.error.unwrap();
        assert_eq!(error.code, jsonrpc::ErrorCode::InvalidRequest);
        assert!(error.message.contains("already in progress"));
        assert!(receiver.try_recv().is_err());
        drop(guard);
        assert!(crate::java_install::ServerInstallGuard::acquire(directory.path()).is_ok());
    }
}

#[cfg(test)]
mod work_done_progress_tests {
    use super::*;

    #[tokio::test]
    async fn test_spring_late_begin_is_filtered_before_reaching_emacs() {
        let (sender, receiver) = crossbeam_channel::unbounded();
        let mut app = Application::new(
            sender,
            syntax::Configuration {
                language: vec![],
                language_server: Default::default(),
            },
        );
        let (spring, _) = crate::test_support::client("spring", 1, "file:///result", true).await;
        app.editor
            .language_servers
            .insert_test_client(spring.clone());
        let (debounce, _) = unbounded_channel();
        let token = "index-java-source-files-task-funding-client-5";
        // This is the wire order in the reported Spring Tools log: create,
        // report through 100%, end, then begin from the create response callback.
        for (id, value) in [
            (Some("create-1"), Value::Null),
            (None, json!({"kind":"report","percentage":100})),
            (None, json!({"kind":"end"})),
            (
                None,
                json!({"kind":"begin","title":"Indexing","percentage":0}),
            ),
            (None, json!({"kind":"report","percentage":1})),
            // A new create using the same token opens a new valid lifecycle.
            (Some("create-2"), Value::Null),
            (
                None,
                json!({"kind":"begin","title":"Indexing again","percentage":0}),
            ),
            (None, json!({"kind":"end"})),
        ] {
            let call = if let Some(id) = id {
                json!({"jsonrpc":"2.0","id":id,"method":"window/workDoneProgress/create",
                    "params":{"token":token}})
            } else {
                json!({"jsonrpc":"2.0","method":"$/progress",
                    "params":{"token":token,"value":value}})
            };
            app.handle_language_server_message(
                serde_json::from_value(call).unwrap(),
                spring.id(),
                debounce.clone(),
            )
            .await;
        }
        let notifications: Vec<_> = receiver.try_iter().collect();
        assert_eq!(notifications.len(), 4);
        for (message, kind) in notifications.iter().zip(["report", "end", "begin", "end"]) {
            let Message::Notification(notification) = message else {
                panic!("expected a progress notification")
            };
            assert_eq!(notification.method, "$/progress");
            assert_eq!(notification.params.params["params"]["token"], token);
            assert_eq!(notification.params.params["params"]["value"]["kind"], kind);
            assert_eq!(
                notification.params.params["rootPath"],
                spring.root_path.to_string_lossy().as_ref()
            );
        }
        drop(spring.shutdown_and_exit());
    }
}

#[cfg(test)]
mod dynamic_registration_tests {
    use lsp_types::request::Request as _;

    #[tokio::test]
    async fn unadvertised_single_server_definition_remains_usable() {
        let (client, _) =
            crate::test_support::client("legacy", 1, "file:///legacy-result", true).await;
        let params = serde_json::from_value(serde_json::json!({
            "textDocument":{"uri":"file:///one/App.java"}, "position":{"line":0,"character":0}
        }))
        .unwrap();
        let response =
            crate::handlers::request::handle_goto_definition::<lsp_types::request::GotoDefinition>(
                crate::test_support::request("textDocument/definition", None),
                params,
                vec![client],
                Some("java".into()),
            )
            .await
            .unwrap();
        assert_eq!(response.result.unwrap()[0]["uri"], "file:///legacy-result");
    }

    #[tokio::test]
    async fn definition_registration_and_unregistration_use_real_transport_and_routing() {
        use crate::{
            application::Application,
            handlers::request::handle_goto_definition,
            lsp::jsonrpc::Call,
            syntax::{Configuration, LanguageServerFeature},
            test_support,
        };
        use serde_json::{json, Value};
        use std::time::Duration;
        let (sender, receiver) = crossbeam_channel::unbounded();
        let mut app = Application::new(
            sender,
            Configuration {
                language: vec![],
                language_server: Default::default(),
            },
        );
        let (jdt, _) = test_support::client("jdtls", 1, "file:///jdt-result", true).await;
        let (spring, _) = test_support::client("spring", 2, "file:///spring-result", false).await;
        spring
            .capabilities
            .set(lsp_types::ServerCapabilities {
                definition_provider: Some(lsp_types::OneOf::Left(true)),
                ..Default::default()
            })
            .unwrap();
        app.editor.language_servers.insert_test_client(jdt.clone());
        let (debounce, _) = tokio::sync::mpsc::unbounded_channel();
        let registration: Call = serde_json::from_value(json!({
            "jsonrpc":"2.0", "id":"registration", "method":"client/registerCapability",
            "params":{"registrations":[{"id":"definition-id", "method":"textDocument/definition"}]}
        }))
        .unwrap();
        app.handle_language_server_message(registration, 1, debounce.clone())
            .await;
        assert!(jdt.with_feature(LanguageServerFeature::GotoDefinition));
        let params: lsp_types::GotoDefinitionParams = serde_json::from_value(json!({
            "textDocument":{"uri":"file:///one/App.java"}, "position":{"line":0,"character":0}
        }))
        .unwrap();
        // Exercise the dispatcher that supplies the document's language context.
        let mut request = test_support::request("textDocument/definition", None);
        request.params.params = serde_json::to_value(&params).unwrap();
        Application::on_request(
            request,
            app.sender.clone(),
            vec![jdt.clone(), spring],
            Some("java".into()),
        );
        let response = tokio::time::timeout(Duration::from_secs(3), async {
            loop {
                if let Ok(crate::msg::Message::Response(response)) = receiver.try_recv() {
                    break response;
                }
                tokio::time::sleep(Duration::from_millis(10)).await;
            }
        })
        .await
        .unwrap();
        assert!(response.error.is_none(), "{response:?}");
        assert_eq!(response.result.unwrap()[0]["uri"], "file:///jdt-result");

        let unregister: Call = serde_json::from_value(json!({
            "jsonrpc":"2.0", "id":"unregistration", "method":"client/unregisterCapability",
            "params":{"unregisterations":[{"id":"definition-id", "method":"textDocument/definition"}]}
        })).unwrap();
        app.handle_language_server_message(unregister, 1, debounce.clone())
            .await;
        assert!(!jdt.with_feature(LanguageServerFeature::GotoDefinition));
        // A single-server compatibility fallback must not undo unregistration.
        assert!(
            handle_goto_definition::<lsp_types::request::GotoDefinition>(
                test_support::request("textDocument/definition", None),
                params,
                vec![jdt.clone()],
                Some("java".into())
            )
            .await
            .is_err()
        );

        let malformed: Call = serde_json::from_value(json!({
            "jsonrpc":"2.0", "id":"malformed", "method":"client/registerCapability",
            "params":{"registrations":[{"id":"bad", "method":"textDocument/definition", "registerOptions":{"documentSelector":false}}]}
        })).unwrap();
        app.handle_language_server_message(malformed, 1, debounce)
            .await;
        let replies = tokio::time::timeout(Duration::from_secs(3), async {
            loop {
                let replies = jdt
                    .call_method(
                        "replies".to_string().into(),
                        "test/replies".into(),
                        Value::Null,
                    )
                    .await
                    .unwrap();
                if replies.as_array().unwrap().len() == 3 {
                    break replies;
                }
                tokio::time::sleep(Duration::from_millis(10)).await;
            }
        })
        .await
        .unwrap();
        for id in ["registration", "unregistration"] {
            let reply = replies
                .as_array()
                .unwrap()
                .iter()
                .find(|r| r["id"] == id)
                .unwrap();
            assert_eq!(reply.get("result"), Some(&Value::Null));
            assert!(reply.get("error").is_none());
        }
        let failure = replies
            .as_array()
            .unwrap()
            .iter()
            .find(|r| r["id"] == "malformed")
            .unwrap();
        assert_eq!(failure["error"]["code"], -32602);
    }

    /// The exact `registerOptions` jdtls sends for code lens.
    ///
    /// It carries only `resolveProvider` — no `documentSelector` — and the handler
    /// above deserializes it into `TextDocumentRegistrationOptions` before storing
    /// the registration. If that ever stopped accepting this shape the handler would
    /// `continue`, the registration would never be stored, and code lens would go
    /// quietly missing for every server that registers it this way: no error, just a
    /// capability the editor is told it does not have.
    #[test]
    fn jdtls_code_lens_register_options_deserialize() {
        let raw = serde_json::json!({ "resolveProvider": true });
        let ops: lsp_types::TextDocumentRegistrationOptions =
            serde_json::from_value(raw).expect("jdtls registerOptions must deserialize");
        assert!(
            ops.document_selector.is_none(),
            "jdtls registers for every document it serves"
        );
    }

    /// The method string the registration is matched on. jdtls sends this literal,
    /// and the match arm compares against the constant — pinning it keeps a rename
    /// in `lsp_types` from silently turning the arm into dead code.
    #[test]
    fn code_lens_registration_method_is_the_one_jdtls_sends() {
        assert_eq!(
            lsp_types::request::CodeLensRequest::METHOD,
            "textDocument/codeLens"
        );
    }

    /// A registration for something we do not announce must not be mistaken for one
    /// we do; only the code-lens arm sets the re-announce flag.
    #[test]
    fn watched_files_registration_is_a_different_method() {
        use lsp_types::notification::Notification as _;
        assert_ne!(
            lsp_types::notification::DidChangeWatchedFiles::METHOD,
            lsp_types::request::CodeLensRequest::METHOD
        );
    }

    #[tokio::test]
    async fn test_hierarchy_dispatches_all_six_methods() {
        use crate::application::Application;
        use crate::msg::{CommonContext, Context};
        use serde_json::{json, Value};
        let (client, _) =
            crate::test_support::client("hierarchy-tags-empty", 1, "file:///w/Bean.java", false)
                .await;
        let result = client.initialize(false).await.unwrap();
        client.capabilities.set(result.capabilities).unwrap();
        let mut root = Value::Null;
        for method in [
            "textDocument/prepareCallHierarchy",
            "callHierarchy/incomingCalls",
            "callHierarchy/outgoingCalls",
            "textDocument/prepareTypeHierarchy",
            "typeHierarchy/supertypes",
            "typeHierarchy/subtypes",
        ] {
            let preparing = method.starts_with("textDocument/");
            let context = (!preparing).then_some(Context::Common(CommonContext {
                language_server_id: 1,
            }));
            let mut request = crate::test_support::request(method, context);
            request.params.params = if preparing {
                json!({"textDocument":{"uri":"file:///w/Bean.java"},"position":{"line":0,"character":1}})
            } else {
                json!({"item":root})
            };
            let (sender, receiver) = crossbeam_channel::unbounded();
            Application::on_request(request, sender, vec![client.clone()], Some("java".into()));
            let response = tokio::time::timeout(std::time::Duration::from_secs(3), async {
                loop {
                    if let Ok(crate::msg::Message::Response(response)) = receiver.try_recv() {
                        break response;
                    }
                    tokio::time::sleep(std::time::Duration::from_millis(5)).await;
                }
            })
            .await
            .unwrap();
            assert!(response.error.is_none(), "{method}: {response:?}");
            let items = response.result.unwrap();
            assert_eq!(items[0]["language_server_id"], 1);
            if preparing {
                root = items[0]["lsp_item"].clone();
            }
        }
    }
}
