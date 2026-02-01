use crate::{
    application::Application, document::VirtualDocumentInfo, lsp_ext, msg::VirtualDocContext,
    utils::get_activate_time,
};
use anyhow::Result;
use itertools::Itertools;
use log::{debug, error};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidSaveTextDocumentParams,
    MessageType, WillSaveTextDocumentParams,
};

pub(crate) fn handle_did_open_text_document(
    app: &mut Application,
    params: lsp_types::DidOpenTextDocumentParams,
    virtual_doc_ctx: Option<VirtualDocContext>,
) -> Result<()> {
    debug!("did_open {params:?}");

    // 首先检查文档是否存在，如果不存在则创建
    let doc_exists = app
        .editor
        .document_by_uri(&params.text_document.uri)
        .is_some();
    if !doc_exists {
        debug!(
            "create a document with language {:?}",
            &params.text_document.language_id
        );
        // The language_id param is guessed by major-mode, only used as a fallback to get a language server when the file extension missed or no match server.
        app.editor.new_document(
            &params.text_document.uri,
            Some(&params.text_document.language_id),
        );
    }

    // 获取 URI 以便后续查找文档 ID
    let uri = params.text_document.uri.clone();

    // 检查是否是 org 文件并处理虚拟文档
    if let Some(doc) = app.editor.document_by_uri(&uri) {
        if doc.is_org_file() {
            if let Some(ref vdoc_ctx) = virtual_doc_ctx {
                let doc_id = doc.id();
                let version = doc.version();

                // Check current virtual doc state
                let has_server_for_language = doc.has_virtual_doc_server(&vdoc_ctx.language);

                let org_line_bias = vdoc_ctx.line_bias;
                let language = vdoc_ctx.language.clone();
                let syn_loader = app.editor.syn_loader.clone();

                // Update virtual doc info (always update line_bias)
                let virtual_doc =
                    VirtualDocumentInfo::new(org_line_bias, language.clone(), Some(syn_loader));
                let language_id = virtual_doc.language_id().to_owned();

                if let Some(doc) = app.editor.documents.get_mut(&doc_id) {
                    doc.virtual_doc = Some(virtual_doc);
                }

                if has_server_for_language {
                    // Reuse existing server - send didClose first, then didOpen for the new block
                    if let Some(doc) = app.editor.documents.get_mut(&doc_id) {
                        // Touch the entry to update last_used timestamp
                        if let Some(entry) = doc.language_servers_of_virtual_doc.get_mut(&language) {
                            entry.touch();
                            let ls = entry.client.clone();
                            
                            // Always send didClose before didOpen when reusing server
                            ls.text_document_did_close(lsp_types::DidCloseTextDocumentParams {
                                text_document: lsp_types::TextDocumentIdentifier {
                                    uri: uri.clone(),
                                },
                            }).unwrap();
                            debug!("Sent didClose for previous block to {:?}", ls.name());
                            
                            ls.text_document_did_open(
                                uri.clone(),
                                version,
                                params.text_document.text.to_owned(),
                                language_id.clone(),
                            ).unwrap();
                            debug!("Reusing {:?} server, sent didOpen for new block", ls.name());
                            
                            // Notify user about reusing existing server
                            app.send_notification::<lsp_types::notification::ShowMessage>(
                                lsp_types::ShowMessageParams {
                                    typ: MessageType::INFO,
                                    message: format!(
                                        "Reusing {} for {} code block.",
                                        ls.name(),
                                        language
                                    ),
                                },
                            );
                        }
                    }
                } else {
                    // Need to launch a new server for this language
                    let client = app
                        .editor
                        .launch_language_servers_for_virtual_document(doc_id, &language);

                    if let Some(ls) = client {
                        ls.text_document_did_open(
                            uri.clone(),
                            version,
                            params.text_document.text.to_owned(),
                            language_id.clone(),
                        )
                        .unwrap();
                        debug!("Success launch {:?} server for current block.", ls.name());

                        // Notify user about virtual document server startup
                        app.send_notification::<lsp_types::notification::ShowMessage>(
                            lsp_types::ShowMessageParams {
                                typ: MessageType::INFO,
                                message: format!(
                                    "Connected to {} for {} code block.",
                                    ls.name(),
                                    language
                                ),
                            },
                        );

                        // Send server capabilities for virtual document to enable trigger characters
                        if ls.is_initialized() {
                            if let Some(doc) = app.editor.document_by_uri(&uri) {
                                app.send_notification::<lsp_ext::CustomServerCapabilities>(
                                    doc.get_virtual_doc_server_capabilities(),
                                );
                            }
                        }
                    }
                }
            }
            return Ok(());
        }
    }

    let doc_id = app.editor.document_by_uri(&uri).map(|doc| doc.id());
    if let Some(doc_id) = doc_id {
        let text = params.text_document.text.to_owned();
        app.editor.launch_language_servers(doc_id);
        let doc = app.editor.document_by_uri(&uri);
        if let Some(doc) = doc {
            // send didOpen notification directly, notifies will pending until server initialized.
            let language_id = doc.language_id().to_owned().unwrap_or_default();
            doc.language_servers.values().for_each(|ls| {
                ls.text_document_did_open(
                    uri.clone(),
                    doc.version(),
                    text.clone(),
                    language_id.to_string(),
                )
                .unwrap();
            });
            if doc.language_servers.values().any(|ls| ls.is_initialized()) {
                app.send_notification::<lsp_ext::CustomServerCapabilities>(
                    doc.get_server_capabilities(),
                );
            }
            let configed_servers = doc.language_servers.keys().join("、");
            app.send_notification::<lsp_types::notification::ShowMessage>(
                lsp_types::ShowMessageParams {
                    typ: MessageType::INFO,
                    message: if configed_servers.is_empty() {
                        "No language server config found for this file, please check your custom config by M-x lsp-proxy-open-config-file.".to_string()
                    } else {
                        format!("Connected to {configed_servers}.")
                    }
                },
            );
        }
    } else {
        error!("No doc to send trigger characters");
    }

    Ok(())
}

pub(crate) fn handle_did_change_text_document(
    app: &mut Application,
    params: DidChangeTextDocumentParams,
    virtual_doc_ctx: Option<VirtualDocContext>,
) -> Result<()> {
    // 如果是 org file 且在 babel block 中，则需要基于 line_bias 来校正
    // server 也需要取 virtual_doc 的 server
    let doc = app.editor.document_by_uri_mut(&params.text_document.uri);
    if let Some(doc) = doc {
        doc.version = params.text_document.version;
        doc.language_servers().for_each(|ls| {
            ls.notify::<lsp_types::notification::DidChangeTextDocument>(params.clone())
                .unwrap()
        });

        debug!("virtual_doc_ctx: {virtual_doc_ctx:?}");
        if let Some(ref vdoc_ctx) = virtual_doc_ctx {
            debug!("is_org_file? {:?}", doc.is_org_file());
            // 如果是 org file 要单独给自己的 server 发送一份
            if doc.is_org_file() {
                // Get and touch the server entry
                let ls = doc.get_virtual_doc_server(&vdoc_ctx.language);
                debug!("get a ls {:?}", ls.is_some());
                if let Some(ls) = ls {
                    debug!("the ls is {:?}", ls.name());
                    ls.notify::<lsp_types::notification::DidChangeTextDocument>(
                        lsp_types::DidChangeTextDocumentParams {
                            text_document: params.text_document.clone(),
                            content_changes: params
                                .content_changes
                                .iter()
                                .map(|change| {
                                    let range = change.range.unwrap();
                                    // Use translation utilities from VirtualDocContext
                                    let translated_range = vdoc_ctx.translate_range_to_virtual(range);
                                    lsp_types::TextDocumentContentChangeEvent {
                                        text: change.text.clone(),
                                        range_length: change.range_length,
                                        range: Some(translated_range),
                                    }
                                })
                                .collect_vec(),
                        },
                    )
                    .unwrap();
                }
            }
        }
    } else {
        error!("no corresponding doc found for did change request");
    }
    Ok(())
}

pub(crate) fn handle_will_save_text_document(
    app: &mut Application,
    params: WillSaveTextDocumentParams,
) -> Result<()> {
    let doc = app.editor.document_by_uri(&params.text_document.uri);
    if let Some(doc) = doc {
        doc.language_servers().for_each(|ls| {
            ls.text_document_will_save(params.clone()).unwrap();
        });
    } else {
        error!("no corresponding doc found for will save notification");
    }

    Ok(())
}

pub(crate) fn handle_did_save_text_document(
    app: &mut Application,
    params: DidSaveTextDocumentParams,
) -> Result<()> {
    let handler = app.editor.language_servers.file_event_handler.clone();
    let doc = app.editor.document_by_uri_mut(&params.text_document.uri);
    if let Some(doc) = doc {
        doc.language_servers().for_each(|ls| {
            ls.text_document_did_save(params.clone()).unwrap();
            // tokio::spawn();
        });
        handler.file_changed(doc.path().unwrap());
    } else {
        error!("no corresponding doc found for did save notification");
    }
    Ok(())
}

pub(crate) fn handle_did_close_text_document(
    app: &mut Application,
    params: DidCloseTextDocumentParams,
) -> Result<()> {
    let editor = &mut app.editor;
    let uri = &params.text_document.uri;

    // Get document ID before removal for cleanup
    let doc_id = editor.document_by_uri(uri).map(|doc| doc.id);

    if let Some(doc_id) = doc_id {
        // Get a reference to the document to close language servers
        {
            let doc = editor.document_mut(doc_id).unwrap();

            // Close language servers
            for language_server in doc.language_servers() {
                language_server
                    .text_document_did_close(params.clone())
                    .unwrap();
            }

            doc.reset();
        }

        // Remove document from editor
        let removed = editor.remove_document(uri);
        if removed {
            log::info!("Document {uri} removed from editor");
        } else {
            log::warn!("Failed to remove document {uri} from editor");
        }
    }

    Ok(())
}

pub(crate) fn handle_cancel(
    app: &mut Application,
    params: lsp_ext::CustomizeCancelParams,
) -> Result<()> {
    let uri = &params.uri.as_ref().unwrap();
    let doc = app
        .editor
        .document_by_uri(&lsp_types::Url::parse(uri).unwrap());
    if let Some(doc) = doc {
        doc.language_servers().for_each(|ls| {
            ls.cancel(lsp_types::CancelParams {
                id: params.id.clone().into(),
            })
            .unwrap()
        })
    }
    Ok(())
}

pub fn handle_did_focus_text_document(
    app: &mut Application,
    params: DidCloseTextDocumentParams,
) -> Result<()> {
    let doc = app.editor.document_by_uri(&params.text_document.uri);
    if let Some(doc) = doc {
        doc.language_servers().for_each(|ls| {
            let mut activate_time = ls.activate_time.try_lock().unwrap();
            *activate_time = get_activate_time();
        });
    }
    Ok(())
}

pub fn handle_exit(app: &mut Application, _params: ()) -> Result<()> {
    log::info!("Received exit notification, preparing to shutdown");

    // Check if shutdown was called first (following LSP spec)
    let exit_code = if app.shutdown_requested {
        log::info!("Shutdown was called before exit, exiting with code 0");
        0
    } else {
        log::warn!("Exit called without shutdown, exiting with code 1");
        1
    };

    // Cleanup all resources
    app.cleanup_resources();

    // Exit the process
    std::process::exit(exit_code);
}

pub fn handle_large_file_load_start(
    app: &mut Application,
    params: lsp_ext::LargeFileLoadStartParams,
) -> anyhow::Result<()> {
    {
        // let mut app = app.lock().unwrap();
        app.large_file_manager
            .lock()
            .unwrap()
            .start_loading(params.uri.clone(), params.total_size);
    }

    tracing::info!(
        "Large file load started: {} ({} bytes, {} chunk size)",
        params.uri,
        params.total_size,
        params.chunk_size
    );
    Ok(())
}

pub fn handle_large_file_chunk(
    app: &mut Application,
    params: lsp_ext::LargeFileChunkParams,
) -> anyhow::Result<()> {
    let is_completed = {
        // let mut app = app.lock().unwrap();
        app.large_file_manager.lock().unwrap().add_chunk(
            &params.uri,
            params.chunk_data,
            params.chunk_index,
            params.is_last_chunk.unwrap_or(false),
        )?
    };

    tracing::debug!(
        "Received chunk {} for {}, progress: {}%",
        params.chunk_index,
        params.uri,
        params.progress
    );

    if is_completed {
        handle_large_file_load_complete_internal(app, params.uri)?;
    }

    Ok(())
}

pub fn handle_large_file_load_cancel(
    app: &mut Application,
    params: lsp_ext::LargeFileLoadCancelParams,
) -> anyhow::Result<()> {
    {
        // let mut app = app.lock().unwrap();
        app.large_file_manager
            .lock()
            .unwrap()
            .cancel_loading(&params.uri);
    }

    tracing::info!("Large file loading cancelled: {}", params.uri);
    Ok(())
}

fn handle_large_file_load_complete_internal(
    app: &mut Application,
    uri: lsp_types::Url,
) -> anyhow::Result<()> {
    let content = {
        // let mut app = app.lock().unwrap();
        let manager = app.large_file_manager.lock().unwrap();

        let content = manager
            .get_content(&uri)
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("Large file content not found"))?;

        // let requests = manager.get_buffered_requests(&uri).unwrap_or_default();

        content
    };

    // 发送完整内容给语言服务器
    {
        // let mut app = app.lock().unwrap();
        let document = app.editor.get(&uri);

        // 创建 didChange 参数来更新内容
        let did_change_params = lsp_types::DidChangeTextDocumentParams {
            text_document: lsp_types::VersionedTextDocumentIdentifier {
                uri: uri.clone(),
                version: 0,
            },
            content_changes: vec![lsp_types::TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: content,
            }],
        };

        let language_servers = document.language_servers();
        for ls in language_servers {
            if let Err(e) = ls.text_document_did_change(did_change_params.clone()) {
                tracing::error!("Failed to send didChange to {}: {}", ls.name(), e);
            }
        }
    }

    // 处理缓冲的请求
    // if !buffered_requests.is_empty() {
    //     tracing::info!("Processing {} buffered requests for {}",
    //                   buffered_requests.len(), uri);

    //     // 在新的任务中处理缓冲的请求，避免阻塞
    //     let app_clone = app.clone();
    //     let uri_clone = uri.clone();
    //     tokio::spawn(async move {
    //         for request in buffered_requests {
    //             if let Err(e) = process_buffered_request(app_clone.clone(), &uri_clone, request).await {
    //                 tracing::error!("Failed to process buffered request: {}", e);
    //             }
    //             // 小延迟避免过载
    //             tokio::time::sleep(Duration::from_millis(10)).await;
    //         }
    //     });
    // }

    tracing::info!(
        "Large file loading completed and requests processed: {}",
        uri
    );
    Ok(())
}
