use crate::{application::Application, lsp_ext, utils::get_activate_time};
use anyhow::Result;
use itertools::Itertools;
use log::error;
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidSaveTextDocumentParams,
    MessageType, WillSaveTextDocumentParams,
};

pub(crate) fn handle_did_open_text_document(
    app: &mut Application,
    params: lsp_types::DidOpenTextDocumentParams,
) -> Result<()> {
    let doc = app.editor.document_by_uri(&params.text_document.uri);
    let doc_id = match doc {
        Some(doc) => doc.id,
        None => {
            let doc = app.editor.new_document(&params.text_document.uri);
            doc.id()
        }
    };
    let uri = params.text_document.uri.to_owned();
    let text = params.text_document.text.to_owned();
    app.editor.launch_langauge_servers(doc_id);
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
                )
    } else {
        error!("No doc to send trigger characters");
    }

    Ok(())
}

pub(crate) fn handle_did_change_text_document(
    app: &mut Application,
    params: DidChangeTextDocumentParams,
) -> Result<()> {
    let doc = app.editor.document_by_uri_mut(&params.text_document.uri);
    if let Some(doc) = doc {
        doc.version = params.text_document.version;
        doc.language_servers().for_each(|ls| {
            ls.notify::<lsp_types::notification::DidChangeTextDocument>(params.clone())
                .unwrap()
        });
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
    {
        let doc = editor.document_by_uri_mut(&params.text_document.uri);
        if let Some(doc) = doc {
            for language_server in doc.language_servers() {
                language_server
                    .text_document_did_close(params.clone())
                    .unwrap();
            }
            doc.reset();
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
