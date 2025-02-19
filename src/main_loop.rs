use crate::{
    application::Application,
    client::{Client, RegisteredCapability},
    connection::Connection,
    controller::Controller,
    dispatch::{NotificationDispatcher, RequestDispatcher},
    document::{DiagnosticItem, Document, DocumentId},
    handlers::{
        self,
        request::{create_error_response, handle_code_action},
    },
    lsp::{
        self,
        jsonrpc::{self, Call},
        MethodCall,
    },
    lsp_ext,
    msg::{self, Message, Response},
    registry::NotificationFromServer,
    syntax::{self},
    thread,
    utils::{find_workspace_folder_for_uri, is_diagnostic_vectors_equal},
};
use anyhow::{Error, Result};
use crossbeam_channel::{bounded, Sender};
use futures_util::StreamExt;
use log::{debug, error, info, warn};
use lsp_types::{notification::Notification, request::Request, LogMessageParams};
use serde_json::{json, Value};
use std::sync::Arc;
use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};

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
                    Err(lsp::Error::Unhandled) => {
                        error!(
                            "Language Server: Method {} not found in request {}",
                            method, id
                        );
                        Err(jsonrpc::Error {
                            code: jsonrpc::ErrorCode::MethodNotFound,
                            message: format!("Method not found: {}", method),
                            data: None,
                        })
                    }
                    Err(err) => {
                        error!(
                            "Language Server: Received malformed method call {} in request {}: {}",
                            method, id, err
                        );
                        Err(jsonrpc::Error {
                            code: jsonrpc::ErrorCode::ParseError,
                            message: format!("Malformed method call: {}", method),
                            data: None,
                        })
                    }
                    Ok(MethodCall::WorkDoneProgressCreate(_params)) => {
                        let language_server = language_server!();
                        Ok(serde_json::Value::Null)
                    }
                    Ok(MethodCall::ApplyWorkspaceEdit(params)) => {
                        let language_server = language_server!();
                        if language_server.is_initialized() {
                            self.send_request::<lsp_types::request::ApplyWorkspaceEdit>(
                                params,
                                |_, res| {
                                    debug!("res {:?}", res);
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

                                // eslint 需要根据 scope_uri 来设置 workspaceFolder
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
                    Ok(MethodCall::RegisterCapability(params)) => {
                        if let Some(client) = self
                            .editor
                            .language_servers
                            .iter_clients()
                            .find(|client| client.id() == server_id)
                        {
                            for reg in params.registrations {
                                match reg.method.as_str() {
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
                                    _ => {
                                        // Language Servers based on the `vscode-languageserver-node` library often send
                                        // client/registerCapability even though we do not enable dynamic registration
                                        // for most capabilities. We should send a MethodNotFound JSONRPC error in this
                                        // case but that rejects the registration promise in the server which causes an
                                        // exit. So we work around this by ignoring the request and sending back an OK
                                        // response.
                                        log::warn!("Ignoring a client/registerCapability request because dynamic capability registration is not enabled. Please report this upstream to the language server");
                                    }
                                }
                            }
                        }

                        Ok(serde_json::Value::Null)
                    }
                    Ok(MethodCall::UnregisterCapability(params)) => {
                        for unreg in params.unregisterations {
                            match unreg.method.as_str() {
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
                        log::warn!("unhandled window/showMessageRequest: {:?}", params);
                        let log_message = LogMessageParams {
                            typ: params.typ,
                            message: params.message,
                        };
                        self.send_notification::<lsp_types::notification::LogMessage>(log_message);
                        Ok(serde_json::Value::Null)
                    }
                };

                tokio::spawn(language_server!().reply(id, reply));
            }
            Call::Notification(jsonrpc::Notification { method, params, .. }) => {
                let notification = match NotificationFromServer::parse(&method, params) {
                    Ok(notification) => notification,
                    Err(crate::registry::Error::Unhandled) => {
                        info!("Ignoring unhandled notification from Language Server");
                        return;
                    }
                    Err(err) => {
                        error!(
                            "Ignoring unknown notification from Language Server: {}",
                            err
                        );
                        return;
                    }
                };

                match notification {
                    NotificationFromServer::Initialized => {
                        let language_server = language_server!();
                        // NOTE Trigger a workspace/didChangeConfiguration notification after initialization.
                        // This might not be required by the spec but Neovim does this as well, so it's
                        // probably a good idea for compatibility.
                        if let Some(config) = language_server.config() {
                            language_server
                                .did_change_configuration(config.clone())
                                .unwrap();
                        }
                        let docs = self
                            .editor
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
                    }
                    NotificationFromServer::Exit => {
                        for doc in self.editor.documents_mut() {
                            doc.clear_diagnostics(server_id);
                        }
                        // Remove the language server from the registry
                        self.editor.language_servers.remove_by_id(server_id);
                    }
                    NotificationFromServer::PublishDianostics(params) => {
                        let language_server = language_server!();
                        if !language_server.is_initialized() {
                            log::error!("Discarding pushlishDiagnostic notification sent by uninitialized server: {}", language_server.name());
                            return;
                        }
                        let doc = self.editor.document_by_uri_mut(&params.uri).filter(|doc| {
                            if let Some(version) = params.version {
                                if version != doc.version {
                                    error!("Version ({version}) is out of date for {:?} (expected ({}), dropping PublishDiagnostic notification", params.uri, doc.version());
                                    return false;
                                }
                            }

                            true
                        });
                        if let Some(doc) = doc {
                            let version = doc.version;
                            let old_diagnostics =
                                doc.get_diagnostics_by_language_server_id(server_id);
                            if old_diagnostics.is_none()
                                || !is_diagnostic_vectors_equal(
                                    &old_diagnostics.as_ref().unwrap(),
                                    &params.diagnostics,
                                )
                            {
                                let diagnostics: Vec<DiagnosticItem> = params
                                    .diagnostics
                                    .iter()
                                    .map(|diagnostic| DiagnosticItem {
                                        item: diagnostic.to_owned(),
                                        language_server_id: server_id,
                                        file_path: doc
                                            .path()
                                            .map(|p| p.to_string_lossy().to_string())
                                            .unwrap_or("".to_string()),
                                    })
                                    .collect();
                                doc.replace_diagnostics(diagnostics, server_id);
                                let diagnostics: Vec<lsp_types::Diagnostic> =
                                    match doc.diagnostics().as_ref() {
                                        Some(diags) => {
                                            diags.iter().map(|diag| diag.item.clone()).collect()
                                        }
                                        None => vec![],
                                    };
                                self.send_notification::<lsp_types::notification::PublishDiagnostics>(
                                    lsp_types::PublishDiagnosticsParams {
                                        version: Some(version),
                                        uri: params.uri,
                                        diagnostics,
                                    },
                                )
                            } else {
                                debug!("old and new equal, ignore");
                            }
                        } else {
                            debug!("document {:?} not found, maybe close/removed.", params.uri);
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
                        self.send_notification::<lsp_ext::CustomProgress>(
                            lsp_ext::CustomProgressParams {
                                root_path: language_server.root_path.to_string_lossy().to_string(),
                                params,
                            },
                        )
                    }
                }
            }
            Call::Invalid { id } => {
                log::error!("Invalid {:?}", id);
                panic!("Invalid Call");
            }
        }
    }

    async fn handle_msg(&mut self, msg: Message) -> Result<()> {
        match msg {
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
                    Err(e) => self.respond(create_error_response(&req.id, e.to_string())),
                }
            }
            Message::Request(req) if req.method == lsp_ext::WorkspaceRestart::METHOD => {
                self.handle_workspace_restart(&req);
            }
            Message::Request(req) => match self.get_working_document(&req) {
                Ok(doc) => {
                    Self::on_request(req, self.sender.clone(), doc.get_all_language_servers());
                }
                Err(e) => self.respond(create_error_response(&req.id, e.to_string())),
            },
            Message::Notification(not) => self.on_notification(not)?,
            Message::Response(resp) => self.complete_request(resp),
        }
        Ok(())
    }

    fn on_request(
        req: msg::Request,
        response_sender: Sender<Message>,
        language_servers: Vec<Arc<Client>>,
    ) {
        let mut dispatcher = RequestDispatcher {
            req: Some(req),
            sender: response_sender,
            language_servers,
        };

        dispatcher
            .on::<lsp_types::request::GotoDefinition, _, _>(
                handlers::request::handle_goto_definition::<lsp_types::request::GotoDefinition>,
            )
            .on::<lsp_types::request::GotoImplementation, _, _>(
                handlers::request::handle_goto_definition::<lsp_types::request::GotoImplementation>,
            )
            .on::<lsp_types::request::GotoDeclaration, _, _>(
                handlers::request::handle_goto_definition::<lsp_types::request::GotoDeclaration>,
            )
            .on::<lsp_types::request::GotoTypeDefinition, _, _>(
                handlers::request::handle_goto_definition::<lsp_types::request::GotoTypeDefinition>,
            )
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
            .on::<lsp_ext::ViewFileText, _, _>(handlers::request::handle_view_file_text)
            .on::<lsp_types::request::InlayHintRequest, _, _>(handlers::request::handle_inlay_hints)
            .on::<lsp_types::request::DocumentHighlightRequest, _, _>(
                handlers::request::handle_document_highlight,
            )
            .on::<lsp_types::request::DocumentSymbolRequest, _, _>(
                handlers::request::handle_document_symbols,
            )
            .finish();
    }

    fn on_notification(&mut self, not: msg::Notification) -> Result<()> {
        use lsp_types::notification as notfis;

        NotificationDispatcher {
            not: Some(not),
            app: self,
        }
        .on_sync_mut::<notfis::DidOpenTextDocument>(
            handlers::notification::handle_did_open_text_document,
        )?
        .on_sync_mut::<notfis::DidChangeTextDocument>(
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
        .on_sync_mut::<lsp_ext::DidFocusTextDocument>(
            handlers::notification::handle_did_focus_text_document,
        )?
        .finish();

        Ok(())
    }

    fn handle_workspace_restart(&mut self, req: &msg::Request) {
        match self.get_working_document(&req) {
            Ok(doc) => {
                let config = doc.language_config().unwrap().clone();
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
                match self
                    .editor
                    .language_servers
                    .restart_v2(&config, old_client_ids, doc_path)
                {
                    Ok(_) => {
                        let mut doc_paths: Vec<String> = vec![];
                        for document_id in document_ids_to_refresh {
                            if let Some(doc) = self.editor.documents.remove(&document_id) {
                                doc_paths.push(doc.path().unwrap().to_string_lossy().to_string());
                            }
                        }
                        self.respond(Response::new_ok(req.id.clone(), doc_paths));
                    }
                    Err(error) => {
                        self.respond(create_error_response(&req.id, error.to_string()));
                    }
                };
            }
            Err(_) => todo!(),
        }
    }

    fn get_working_document(&self, req: &msg::Request) -> Result<&Document> {
        match &req.params.uri {
            Some(uri) => {
                if let Some(doc) = self
                    .editor
                    .document_by_uri(&lsp_types::Url::parse(uri).unwrap())
                {
                    Ok(doc)
                } else {
                    Err(Error::msg("No document opened"))
                }
            }
            None => Err(Error::msg("No uri provided")),
        }
    }
}
