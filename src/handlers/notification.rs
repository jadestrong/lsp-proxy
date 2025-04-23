use crate::{application::Application, document::DocumentId, lsp_ext, utils::get_activate_time};
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
    match doc {
        Some(doc) => {
            error!("duplicate DidOpenTextDocument: {:?}", doc.id);
            // send did change !!!
        }
        None => {
            let doc_id = app.editor.new_document(&params.text_document.uri);
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
                        doc.get_server_capabilities()
                    );
                }
                let configed_servers = doc.language_servers.keys().join("、");
                app.send_notification::<lsp_types::notification::ShowMessage>(
                    lsp_types::ShowMessageParams {
                        typ: MessageType::INFO,
                        message: if configed_servers.is_empty() {
                            format!("No language server config found for this file, please check your custom config by M-x lsp-proxy-open-config-file.")
                        } else {
                            format!("Connected to {:?}.", configed_servers)
                        }
                    },
                )
            } else {
                error!("No doc to send trigger characters");
            }
        }
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
    let id: Option<DocumentId>;
    {
        let doc = editor.document_by_uri_mut(&params.text_document.uri);
        id = match doc {
            Some(doc) => {
                for language_server in doc.language_servers() {
                    // tokio::spawn();
                    language_server
                        .text_document_did_close(params.clone())
                        .unwrap();
                }
                Some(doc.id)
            }
            None => {
                error!("no corresponding doc found for did close request.");
                None
            }
        }
    }
    if let Some(doc_id) = id {
        log::debug!("remove {} document successed.", doc_id);
        editor.documents.remove(&doc_id);
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
