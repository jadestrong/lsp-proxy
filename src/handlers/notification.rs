use anyhow::Result;
use itertools::Itertools;
use log::error;
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidSaveTextDocumentParams,
    MessageType, WillSaveTextDocumentParams,
};

use crate::{application::Application, document::DocumentId, lsp_ext, utils::get_activate_time};

pub(crate) fn handle_did_open_text_document(
    app: &mut Application,
    params: lsp_ext::CustomizeDidOpenTextDocumentParams,
) -> Result<()> {
    let doc = app.editor.document_by_uri(&params.text_document.uri);
    match doc {
        Some(doc) => {
            error!("duplicate DidOpenTextDocument: {:?}", doc.id);
            // send did change !!!
        }
        None => {
            let doc_id = app
                .editor
                .new_document(&params.text_document.uri, params.text_document.text.clone());
            let uri = params.text_document.uri.to_owned();
            app.editor.launch_langauge_servers(doc_id, params);
            let doc = app.editor.document_by_uri(&uri);
            if let Some(doc) = doc {
                app.send_notification::<lsp_ext::DidRecordTriggerCharacters>(
                    lsp_ext::DidRecordTriggerCharactersParams {
                        uri: uri.to_string(),
                        trigger_characters: doc.get_trigger_characters(),
                        signature_trigger_characters: doc.get_signature_trigger_characters(),
                        support_inlay_hints: doc.is_has_inlay_hints_support(),
                    },
                );
                let configed_servers = doc.language_servers.keys().join("、");
                let lang_servers = doc
                    .language_servers
                    .iter()
                    .filter_map(
                        |(key, ls)| {
                            if ls.is_initialized() {
                                Some(key)
                            } else {
                                None
                            }
                        },
                    )
                    .join("、");
                app.send_notification::<lsp_types::notification::ShowMessage>(
                    lsp_types::ShowMessageParams {
                        typ: MessageType::INFO,
                        message: if configed_servers.is_empty() {
                            format!("No language server config found for this file, please check your custom config by M-x lsp-copilot-open-config-file.")
                        } else if lang_servers.is_empty() {
                            format!("Detect {:?} configuration.", configed_servers)
                        } else {
                            format!("Connected to {:?}.", lang_servers)
                        },
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
            // tokio::spawn();
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
                id: lsp_types::NumberOrString::Number(params.id.clone().into()),
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
