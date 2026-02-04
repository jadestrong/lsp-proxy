use anyhow::Result;
use core::fmt;
use crossbeam_channel::Sender;
use futures_util::{stream::FuturesUnordered, FutureExt, TryStreamExt};
use itertools::Itertools;
use lazy_static::lazy_static;
use log::{debug, error, info};
use lsp_types::{notification::Notification, request::Request, CodeAction};
use parking_lot::Mutex;
use serde::de::DeserializeOwned;
use std::{cmp::Ordering, collections::HashSet, panic, sync::Arc, time::Instant};
use stringslice::StringSlice;

use crate::{
    client::Client,
    code_action::{
        action_category, action_fixes_diagnostics, action_preferred, CodeActionOrCommandItem,
    },
    completion_cache::CompletionCache,
    config::{self, DEFAULT_MAX_DIAGNOSTICS_PUSH, MAX_COMPLETION_ITEMS},
    document::{DiagnosticItem, DiagnosticProvider, DocumentId},
    fuzzy, job,
    lsp::jsonrpc,
    lsp_ext::{self, CommandItem, CompletionItem, RustAnalyzerExpandMacro},
    msg::{self, Context, Message, RequestId, Response},
    syntax::LanguageServerFeature,
    utils::{
        is_diagnostic_vectors_equal, limit_diagnostics_for_push, lsp_symbols_to_imenu,
        sort_imenu_entries_grouped, truncate_completion_item,
    },
};

pub fn create_error_response(id: &RequestId, message: String) -> Response {
    error!("result to response err {message}");
    Response::new_err(id.clone(), jsonrpc::ErrorCode::InternalError, message)
}

async fn call_single_language_server<R>(
    req: &msg::Request,
    params: R::Params,
    language_servers: &[Arc<Client>],
    feature: Option<LanguageServerFeature>,
    language_server_id: Option<usize>,
) -> Result<(R::Result, String)>
where
    R: lsp_types::request::Request + 'static,
    R::Params: DeserializeOwned + Send + fmt::Debug + panic::UnwindSafe,
{
    let ls_names = language_servers.iter().map(|ls| ls.name()).join("、");
    let language_server = match (language_server_id, feature) {
        (Some(ls_id), _) => language_servers.iter().find(|ls| ls.id() == ls_id),
        (None, Some(feature)) => language_servers.iter().find(|ls| ls.with_feature(feature)),
        _ => language_servers.iter().next(),
    };
    match language_server {
        Some(ls) => {
            let json = ls.call::<R>(req.id.clone(), params).await;
            match json {
                Ok(resp) => match serde_json::from_value::<R::Result>(resp) {
                    Ok(resp) => Ok((resp, ls.name().to_string())),
                    Err(err) => Err(err.into()),
                },
                Err(err) => Err(err.into()),
            }
        }
        None => Err(anyhow::Error::msg(format!(
            "Language server {:?} not support {:?}.",
            ls_names, req.method
        ))),
    }
}

async fn call_language_servers<R>(
    req: &msg::Request,
    params: R::Params,
    language_servers: &[Arc<Client>],
    feature: LanguageServerFeature,
) -> Vec<R::Result>
where
    R: lsp_types::request::Request + 'static,
    R::Params: DeserializeOwned + Send + fmt::Debug + panic::UnwindSafe + Clone,
{
    let language_servers = language_servers
        .iter()
        .filter(|ls| ls.with_feature(feature));

    let mut seen_language_servers = HashSet::new();
    let mut futures: FuturesUnordered<_> = language_servers
        .into_iter()
        .filter(|ls| seen_language_servers.insert(ls.id()))
        .map(|ls| {
            let request = ls.call::<R>(req.id.clone(), params.clone());
            async move {
                match request.await {
                    Ok(resp) => match serde_json::from_value::<R::Result>(resp) {
                        Ok(resp) => anyhow::Ok(Some(resp)),
                        Err(_) => anyhow::Ok(None),
                    },
                    Err(_) => anyhow::Ok(None),
                }
            }
        })
        .collect();

    let future = async move {
        let items_future = async move {
            let mut items = Vec::new();
            while let Some(resp) = futures.try_next().await.unwrap() {
                if let Some(contents) = resp {
                    items.push(contents);
                }
            }
            items
        };
        items_future.await
    };
    future.await
}

async fn call_first_language_server<R>(
    req: &msg::Request,
    params: R::Params,
    language_servers: &[Arc<Client>],
) -> Result<(R::Result, String)>
where
    R: lsp_types::request::Request + 'static,
    R::Params: DeserializeOwned + Send + fmt::Debug + panic::UnwindSafe,
{
    let language_server = language_servers.iter().next();
    match language_server {
        Some(ls) => {
            let json = ls.call::<R>(req.id.clone(), params).await;
            match json {
                Ok(resp) => match serde_json::from_value::<R::Result>(resp) {
                    Ok(resp) => Ok((resp, ls.name().to_string())),
                    Err(err) => Err(err.into()),
                },
                Err(err) => Err(err.into()),
            }
        }
        None => Err(anyhow::Error::msg("No language server available. Please check the log file by M-x lsp-proxy-open-log-file.".to_string())),
    }
}

pub(crate) async fn handle_goto_definition<R>(
    req: msg::Request,
    params: R::Params,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response>
where
    R: lsp_types::request::Request + 'static,
    R::Params: DeserializeOwned + Send + fmt::Debug + panic::UnwindSafe,
{
    let feature = match R::METHOD {
        lsp_types::request::GotoImplementation::METHOD => LanguageServerFeature::GotoImplementation,
        lsp_types::request::GotoDeclaration::METHOD => LanguageServerFeature::GotoDeclaration,
        lsp_types::request::GotoTypeDefinition::METHOD => LanguageServerFeature::GotoTypeDefinition,
        _ => LanguageServerFeature::GotoDefinition,
    };
    // Some language servers (e.g., Dart) did not declare `definitionProvider` but still support it.
    let language_server = if language_servers.len() == 1 {
        language_servers.first()
    } else {
        language_servers.iter().find(|ls| ls.with_feature(feature))
    };
    if let Some(ls) = language_server {
        match ls.call::<R>(req.id.clone(), params).await {
            Ok(json) => match serde_json::from_value::<lsp_types::GotoDefinitionResponse>(json) {
                Ok(mut resp) => {
                    if let lsp_types::GotoDefinitionResponse::Link(location_links) = resp {
                        let locations: Vec<lsp_types::Location> = location_links
                            .into_iter()
                            .map(|link| lsp_types::Location {
                                uri: link.target_uri,
                                range: link.target_selection_range,
                            })
                            .collect();
                        resp = lsp_types::GotoDefinitionResponse::Array(locations);
                    }
                    Ok(Response::new_ok(req.id, resp))
                }
                Err(e) => Err(e.into()),
            },
            Err(err) => Err(err.into()),
        }
    } else {
        let ls_names = language_servers.iter().map(|ls| ls.name()).join("、");
        let error_msg = if ls_names.is_empty() {
            "No language server available. Please check the log file by M-x lsp-proxy-open-log-file.".to_string()
        } else {
            format!(
                "Language server {:?} not support {:?}.",
                ls_names, req.method
            )
        };
        Err(anyhow::Error::msg(error_msg))
    }
}

pub(crate) async fn handle_goto_references(
    req: msg::Request,
    params: lsp_types::ReferenceParams,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    call_single_language_server::<lsp_types::request::References>(
        &req,
        params,
        &language_servers,
        Some(LanguageServerFeature::GotoReference),
        None,
    )
    .await
    .map(|(resp, _)| Response::new_ok(req.id.clone(), resp))
}

lazy_static! {
    static ref COMPLETION_CACHE: Mutex<CompletionCache> = Mutex::new(CompletionCache::new());
}

pub(crate) async fn handle_completion(
    req: msg::Request,
    params: lsp_types::CompletionParams,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    if let Some(Context::Completion(context)) = req.params.context {
        let prefix_len = &context.prefix.len();
        let bounds_start = &context.bounds_start;
        let trigger_kind = &context.trigger_kind;
        let pretext = context
            .line
            .slice(0..params.text_document_position.position.character as usize);
        debug!("prefix len {prefix_len} {pretext:?}");
        let uri = &req.params.uri.to_owned();
        let max_items = MAX_COMPLETION_ITEMS.get().unwrap_or(&20);
        if trigger_kind != &lsp_types::CompletionTriggerKind::INVOKED {
            if let Some(items) = COMPLETION_CACHE.try_lock().unwrap().get_cached_items(
                uri.as_ref().unwrap(),
                pretext,
                &context.prefix,
                bounds_start,
            ) {
                let now = Instant::now();
                let filtered_items = fuzzy::filter::filter(
                    pretext,
                    &items,
                    params.text_document_position.position,
                    &context.prefix,
                );
                debug!("filter elapsed {:0.2?}", now.elapsed());
                let slice_length = std::cmp::min(*max_items, filtered_items.len());
                let slice_items = &filtered_items[..slice_length];
                if !slice_items.is_empty() {
                    debug!("return cached items");
                    return Ok(Response::new_ok(req.id.clone(), slice_items));
                }
            } else {
                debug!("no cache {:?}", &bounds_start);
            }
        }

        COMPLETION_CACHE.try_lock().unwrap().clear_cache();

        let lang_servers = language_servers
            .iter()
            .filter(|ls| ls.with_feature(LanguageServerFeature::Completion));
        let mut seen_language_servers = HashSet::new();
        let mut futures: FuturesUnordered<_> = lang_servers
            .into_iter()
            .filter(|ls| {
                if trigger_kind == &lsp_types::CompletionTriggerKind::INVOKED || *prefix_len != 0 {
                    seen_language_servers.insert(ls.id())
                } else if ls.name() == "typescript-language-server" && pretext.ends_with(" ") {
                    // typescript-language-server 未拦截空格触发的情况，但 vscode 和 vtsls 支持拦截
                    false
                } else {
                    // 此时启动触发且 prefix_len 为空，如果不是 triggerCharacter 则不自动请求
                    let trigger_character = match &ls.capabilities().completion_provider {
                        Some(lsp_types::CompletionOptions {
                            trigger_characters: Some(triggers),
                            ..
                        }) => triggers.iter().find(|trigger| pretext.ends_with(*trigger)),
                        _ => None,
                    };
                    trigger_character.is_some()
                }
            })
            .map(|language_server| {
                let cmp_start = Instant::now();
                let language_server_id = language_server.id();
                let language_server_name = language_server.name().to_owned();
                let is_support_resolve =
                    language_server.with_feature(LanguageServerFeature::CompletionResolve);
                let trigger_character = match &language_server.capabilities().completion_provider {
                    Some(lsp_types::CompletionOptions {
                        trigger_characters: Some(triggers),
                        ..
                    }) => triggers.iter().find(|trigger| pretext.ends_with(*trigger)),
                    _ => None,
                };

                // Clone virtual_doc context for use in async closure
                let vdoc_ctx = req.params.virtual_doc.clone();

                let completion_request = language_server
                    .completion(
                        req.id.clone(),
                        lsp_types::CompletionParams {
                            context: Some(lsp_types::CompletionContext {
                                trigger_character: if trigger_kind
                                    == &lsp_types::CompletionTriggerKind::INVOKED
                                {
                                    None
                                } else {
                                    trigger_character.cloned()
                                },
                                trigger_kind: if trigger_kind
                                    == &lsp_types::CompletionTriggerKind::INVOKED
                                    || trigger_character.is_none()
                                {
                                    lsp_types::CompletionTriggerKind::INVOKED
                                } else {
                                    lsp_types::CompletionTriggerKind::TRIGGER_CHARACTER
                                },
                            }),
                            text_document_position: match &req.params.virtual_doc {
                                None => params.text_document_position.clone(),
                                Some(vdoc_ctx) => lsp_types::TextDocumentPositionParams {
                                    text_document: params
                                        .text_document_position
                                        .text_document
                                        .clone(),
                                    position: vdoc_ctx.translate_position_to_virtual(
                                        params.text_document_position.position,
                                    ),
                                },
                            },
                            ..params.clone()
                        },
                    )
                    .unwrap();

                let id = req.id.clone();
                async move {
                    let json = completion_request.await?;
                    info!(
                        "{:?} [{:?}] await ${:0.2?}",
                        language_server_name,
                        id.clone(),
                        cmp_start.elapsed()
                    );
                    let response: Option<lsp_types::CompletionResponse> =
                        serde_json::from_value(json)?;

                    let items = match response {
                        Some(lsp_types::CompletionResponse::Array(items)) => items,
                        Some(lsp_types::CompletionResponse::List(lsp_types::CompletionList {
                            is_incomplete: _is_incomplete,
                            items,
                        })) => items,
                        None => Vec::new(),
                    }
                    .into_iter()
                    .map(|item| {
                        let mut new_item = truncate_completion_item(item);
                        if let Some(text_edit) = &mut new_item.text_edit {
                            if let lsp_types::CompletionTextEdit::InsertAndReplace(
                                replace_text_edit,
                            ) = text_edit
                            {
                                new_item.text_edit = Some(lsp_types::CompletionTextEdit::Edit(
                                    lsp_types::TextEdit {
                                        range: replace_text_edit.replace,
                                        new_text: replace_text_edit.new_text.clone(),
                                    },
                                ));
                            }

                            // Translate textEdit range from virtual doc coordinates back to org file coordinates
                            if let Some(ref vdoc) = vdoc_ctx {
                                if let Some(lsp_types::CompletionTextEdit::Edit(edit)) =
                                    &mut new_item.text_edit
                                {
                                    edit.range = vdoc.translate_range_from_virtual(edit.range);
                                }
                                // Also translate additionalTextEdits for virtual documents
                                if let Some(ref mut additional_edits) = new_item.additional_text_edits {
                                    for edit in additional_edits.iter_mut() {
                                        edit.range = vdoc.translate_range_from_virtual(edit.range);
                                    }
                                }
                            }
                        }
                        let label_len = &new_item.label.chars().count();
                        CompletionItem {
                            item: new_item,
                            language_server_id: if is_support_resolve {
                                Some(language_server_id)
                            } else {
                                None
                            },
                            language_server_name: language_server_name.clone(),
                            start: context.start_point,
                            end: context.start_point + (*label_len as i32 - *prefix_len as i32),
                        }
                    })
                    .collect();

                    anyhow::Ok(items)
                }
            })
            .collect();

        let future = async move {
            let items_future = async move {
                let mut items = Vec::new();
                loop {
                    match futures.try_next().await {
                        Ok(Some(mut lsp_items)) => {
                            items.append(&mut lsp_items);
                        }
                        Ok(None) => break,
                        Err(e) => {
                            error!("Future Error: {e}");
                            continue;
                        }
                    }
                }

                // cache items with pretext\uri\bounds_start
                // NOTE 只有当 prefix 存在的情况下才缓存，空 prefix 的补全结果是不准确的
                if !items.is_empty() {
                    COMPLETION_CACHE.try_lock().unwrap().set_cache(
                        uri.clone(),
                        bounds_start.to_owned(),
                        pretext.to_owned(),
                        context.prefix.clone(),
                        items.clone(),
                    );
                }
                let now = Instant::now();
                let filtered_items = fuzzy::filter::filter(
                    pretext,
                    &items,
                    params.text_document_position.position,
                    &context.prefix,
                );
                info!("filter elapsed {:0.2?}", now.elapsed());
                if trigger_kind == &lsp_types::CompletionTriggerKind::INVOKED || *prefix_len == 0 {
                    anyhow::Ok(filtered_items.to_owned())
                } else {
                    let slice_length = std::cmp::min(*max_items, filtered_items.len());
                    let slice_items = &filtered_items[..slice_length];
                    anyhow::Ok(slice_items.to_owned())
                }
            };

            let res = items_future.await;
            match res {
                Ok(res) => Response::new_ok(req.id.clone(), res),
                Err(e) => create_error_response(&req.id, e.to_string()),
            }
        };
        let requst_start = Instant::now();
        let resp = future.await;
        info!("completion await end {:0.2?}", requst_start.elapsed());
        Ok(resp)
    } else {
        Err(anyhow::anyhow!("Not get a completion context: {req:?}."))
    }
}

pub(crate) async fn handle_completion_resolve(
    req: msg::Request,
    params: lsp_types::CompletionItem,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    if let Some(Context::Resolve(context)) = &req.params.context {
        let params_detail = params.detail.clone();
        let params_text_edit = params.text_edit.clone();
        call_single_language_server::<lsp_types::request::ResolveCompletionItem>(
            &req,
            params,
            &language_servers,
            Some(LanguageServerFeature::CompletionResolve),
            Some(context.language_server_id),
        )
        .await
        .map(|(mut resp, language_server_name)| {
            let documentation = &resp.documentation;
            let detail = &resp.detail;
            if detail.is_some()
                && detail.as_ref().unwrap() != params_detail.as_ref().unwrap_or(&"".to_string())
            {
                match documentation {
                    Some(documentation) => match documentation {
                        lsp_types::Documentation::String(str_doc) => {
                            resp.documentation = Some(lsp_types::Documentation::String(format!(
                                "```\n{}\n```\n\n{}",
                                detail.as_ref().unwrap().clone(),
                                str_doc
                            )))
                        }
                        lsp_types::Documentation::MarkupContent(markdown_doc) => {
                            resp.documentation = Some(lsp_types::Documentation::MarkupContent(
                                lsp_types::MarkupContent {
                                    kind: markdown_doc.kind.clone(),
                                    value: format!(
                                        "```\n{}\n```\n\n{}",
                                        detail.as_ref().unwrap().clone(),
                                        markdown_doc.value
                                    ),
                                },
                            ))
                        }
                    },
                    None => {
                        resp.documentation = Some(lsp_types::Documentation::MarkupContent(
                            lsp_types::MarkupContent {
                                kind: lsp_types::MarkupKind::Markdown,
                                value: format!("```\n{}\n```", detail.as_ref().unwrap().clone()),
                            },
                        ))
                    }
                }
            }

            // The textEdit of tsserver is different between textDocument/completion and completionItem/resolve.
            // The resolve response will have insertTextFormat and snippet information added.
            // So we cannot reuse completion's textEdit property directly.
            if params_text_edit.is_some()
                && language_server_name != "vtsls"
                && language_server_name != "typescript-language-server"
            {
                // Since we have cached completion results, the input position will not update immediately. The rust-analyzer server will always return the latest position for a resolve request.
                // Therefore, it is preferable to use the cached completionItem's textEdit property. Other servers like tsserver won't update the position, so it's fine.
                resp.text_edit = params_text_edit;
            } else if let Some(lsp_types::CompletionTextEdit::InsertAndReplace(replace_text_edit)) =
                &resp.text_edit
            {
                resp.text_edit = Some(lsp_types::CompletionTextEdit::Edit(lsp_types::TextEdit {
                    range: replace_text_edit.replace,
                    new_text: replace_text_edit.new_text.clone(),
                }));
            }

            // For virtual documents (org babel blocks), translate textEdit range back to org file coordinates
            if let Some(ref vdoc_ctx) = req.params.virtual_doc {
                if let Some(ref mut text_edit) = resp.text_edit {
                    match text_edit {
                        lsp_types::CompletionTextEdit::Edit(edit) => {
                            edit.range = vdoc_ctx.translate_range_from_virtual(edit.range);
                        }
                        lsp_types::CompletionTextEdit::InsertAndReplace(edit) => {
                            edit.insert = vdoc_ctx.translate_range_from_virtual(edit.insert);
                            edit.replace = vdoc_ctx.translate_range_from_virtual(edit.replace);
                        }
                    }
                }
                // Translate additionalTextEdits for virtual documents
                if let Some(ref mut additional_edits) = resp.additional_text_edits {
                    for edit in additional_edits.iter_mut() {
                        edit.range = vdoc_ctx.translate_range_from_virtual(edit.range);
                    }
                }
            }

            Response::new_ok(
                req.id.clone(),
                CompletionItem {
                    item: resp,
                    language_server_id: Some(context.language_server_id),
                    language_server_name,
                    start: context.start,
                    end: context.end,
                },
            )
        })
    } else {
        error!("No Resolve Context {:?}", req.params);
        Err(anyhow::Error::msg(format!(
            "No Resolve Context {:?}",
            req.params
        )))
    }
}

pub(crate) async fn handle_code_action_resolve(
    req: msg::Request,
    code_action: lsp_types::CodeAction,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    if let Some(Context::Common(context)) = &req.params.context {
        call_single_language_server::<lsp_types::request::CodeActionResolveRequest>(
            &req,
            code_action,
            &language_servers,
            None,
            Some(context.language_server_id),
        )
        .await
        .map(|(action, _)| {
            Response::new_ok(
                req.id,
                CodeActionOrCommandItem {
                    lsp_item: action.into(),
                    language_server_id: context.language_server_id,
                    language_server_name: format!("{:?}", context.language_server_id),
                },
            )
        })
    } else {
        Err(anyhow::Error::msg(format!(
            "No CodeAction Resolve Context {:?}",
            req.params
        )))
    }
}

pub(crate) async fn handle_formating(
    req: msg::Request,
    params: lsp_types::DocumentFormattingParams,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    call_single_language_server::<lsp_types::request::Formatting>(
        &req,
        params,
        &language_servers,
        Some(LanguageServerFeature::Format),
        None,
    )
    .await
    .map(|(resp, _)| {
        let edits = resp.unwrap_or_else(Default::default);
        Response::new_ok(req.id.clone(), edits)
    })
}

pub(crate) async fn handle_execute_command(
    req: msg::Request,
    params: lsp_types::ExecuteCommandParams,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    if let Some(Context::Common(context)) = &req.params.context {
        let language_server = language_servers
            .iter()
            .find(|ls| ls.id() == context.language_server_id);
        if let Some(ls) = language_server {
            if ls.name() == "rust-analyzer" && params.command == "reloadWorkspace" {
                return call_single_language_server::<lsp_ext::RustAnalyzerReloadWorkspace>(
                    &msg::Request::new(
                        req.id.clone(),
                        lsp_ext::RustAnalyzerReloadWorkspace::METHOD.to_string(),
                        (),
                    ),
                    (),
                    &language_servers,
                    None,
                    Some(context.language_server_id),
                )
                .await
                .map(|_| Response::new_ok(req.id, ""));
            }
        }
        call_single_language_server::<lsp_types::request::ExecuteCommand>(
            &req,
            params,
            &language_servers,
            None,
            Some(context.language_server_id),
        )
        .await
        .map(|_| Response::new_ok(req.id, ""))
    } else {
        Err(anyhow::Error::msg(format!(
            "No context params of {:?}",
            req.method
        )))
    }
}

pub(crate) async fn handle_get_commands(
    req: msg::Request,
    _params: (),
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    let mut execute_commands = Vec::new();
    language_servers.iter().for_each(|ls| {
        if let Some(lsp_types::ExecuteCommandOptions { commands, .. }) =
            &ls.capabilities().execute_command_provider
        {
            commands.iter().for_each(|command| {
                execute_commands.push(CommandItem {
                    id: command.clone(),
                    language_server_id: ls.id(),
                })
            });
        }
        if ls.name() == "rust-analyzer" {
            execute_commands.push(CommandItem {
                id: String::from("reloadWorkspace"),
                language_server_id: ls.id(),
            });
        }
        // 如果是 rust-analyzer 手动添加 command:
        // 1. rust-analyzer/reloadWorkspace 无参数
        // 2. rust-analyzer/viewFileText uri 参数， String 返回
    });
    Ok(Response::new_ok(req.id.clone(), execute_commands))
}

pub(crate) async fn handle_get_workspace_info(
    req: msg::Request,
    _params: (),
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    use crate::lsp_ext::{LanguageServerInfo, WorkspaceInfo};

    let file_path = req.params.uri.as_ref().map(|u| u.to_string());

    if file_path.is_none() || language_servers.is_empty() {
        return Ok(Response::new_ok(req.id.clone(), None::<WorkspaceInfo>));
    }

    let workspace_root = language_servers[0].root_path.to_string_lossy().to_string();
    let language_server_infos: Vec<LanguageServerInfo> = language_servers
        .iter()
        .map(|ls| LanguageServerInfo {
            name: ls.name().to_string(),
            root_path: ls.root_path.to_string_lossy().to_string(),
            support_workspace: ls.support_workspace(),
        })
        .collect();

    let info = WorkspaceInfo {
        file_path: file_path.unwrap(),
        workspace_root,
        language_servers: language_server_infos,
    };

    Ok(Response::new_ok(req.id.clone(), Some(info)))
}

pub(crate) async fn handle_get_languages_config(
    req: msg::Request,
    _params: (),
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    use crate::config;
    use serde_json::json;

    // Get the merged configuration
    let config = config::default_syntax_loader();

    // Get active client names
    let active_client_names: Vec<String> = language_servers
        .iter()
        .map(|ls| ls.name().to_string())
        .collect();

    // Find current buffer's language config
    let file_path = req.params.uri.as_ref().map(|u| u.to_string());
    let language_config = if let Some(file_path) = file_path {
        // Try to match by file extension or path
        let path = std::path::Path::new(&file_path);
        config.language.iter().find(|lang| {
            lang.file_types.iter().any(|ft| match ft {
                crate::syntax::FileType::Extension(ext) => {
                    path.extension().and_then(|e| e.to_str()) == Some(ext)
                        || path.file_name().and_then(|f| f.to_str()) == Some(ext)
                }
                crate::syntax::FileType::Glob(glob) => glob.compile_matcher().is_match(&file_path),
            })
        })
    } else {
        None
    };

    // Get all configured server names for this language
    let configured_server_names: Vec<String> = language_config
        .as_ref()
        .map(|lang| {
            lang.language_servers
                .iter()
                .map(|ls| ls.name.clone())
                .collect()
        })
        .unwrap_or_default();

    // Build language server configs for all configured servers
    let mut language_servers_json = serde_json::Map::new();
    for name in &configured_server_names {
        if let Some(ls_config) = config.language_server.get(name) {
            let mut ls_json = serde_json::Map::new();
            ls_json.insert("command".to_string(), json!(ls_config.command));
            ls_json.insert("args".to_string(), json!(ls_config.args));
            ls_json.insert("environment".to_string(), json!(ls_config.environment));
            ls_json.insert("timeout".to_string(), json!(ls_config.timeout));

            // Include config if present
            if let Some(ref config_value) = ls_config.config {
                ls_json.insert("config".to_string(), config_value.clone());
            }

            // Include experimental if present
            if let Some(ref experimental_value) = ls_config.experimental {
                ls_json.insert("experimental".to_string(), experimental_value.clone());
            }

            language_servers_json.insert(name.clone(), json!(ls_json));
        }
    }

    let full_config = json!({
        "active-clients": active_client_names,
        "configured-servers": configured_server_names,
        "language": language_config,
        "language-server": language_servers_json
    });

    // Serialize to JSON string
    let json_string = serde_json::to_string_pretty(&full_config)
        .unwrap_or_else(|e| format!("{{\"error\": \"Failed to serialize config: {e}\"}}"));

    Ok(Response::new_ok(req.id.clone(), json_string))
}

pub(crate) async fn handle_hover(
    req: msg::Request,
    params: lsp_types::HoverParams,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    fn marked_string_to_markdown(contents: lsp_types::MarkedString) -> String {
        match contents {
            lsp_types::MarkedString::String(contents) => contents,
            lsp_types::MarkedString::LanguageString(string) => {
                if string.language == "markdown" {
                    string.value
                } else {
                    format!("```{}\n{}\n```", string.language, string.value)
                }
            }
        }
    }

    // Translate position for virtual documents (org babel blocks)
    let params = if let Some(ref vdoc_ctx) = req.params.virtual_doc {
        lsp_types::HoverParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: params.text_document_position_params.text_document.clone(),
                position: vdoc_ctx
                    .translate_position_to_virtual(params.text_document_position_params.position),
            },
            work_done_progress_params: params.work_done_progress_params,
        }
    } else {
        params
    };

    let resps = call_language_servers::<lsp_types::request::HoverRequest>(
        &req,
        params,
        &language_servers,
        LanguageServerFeature::Hover,
    )
    .await;
    let results: Vec<String> = resps
        .into_iter()
        .filter_map(|resp| match resp {
            Some(hover) => {
                let contents = match hover.contents {
                    lsp_types::HoverContents::Scalar(contents) => {
                        marked_string_to_markdown(contents)
                    }
                    lsp_types::HoverContents::Array(contents) => contents
                        .into_iter()
                        .map(marked_string_to_markdown)
                        .collect::<Vec<_>>()
                        .join("\n\n"),
                    lsp_types::HoverContents::Markup(contents) => contents.value,
                };
                Some(contents)
            }
            None => None,
        })
        .collect();

    Ok(Response::new_ok(req.id, results.join("\n")))
}

pub(crate) async fn handle_signature_help(
    req: msg::Request,
    params: lsp_types::SignatureHelpParams,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    let resps = call_language_servers::<lsp_types::request::SignatureHelpRequest>(
        &req,
        params,
        &language_servers,
        LanguageServerFeature::SignatureHelp,
    )
    .await;

    let results: Vec<lsp_types::SignatureHelp> = resps.into_iter().flatten().collect();
    Ok(Response::new_ok(req.id, results.first()))
}

pub(crate) async fn handle_code_action(
    req: msg::Request,
    response_sender: Sender<Message>,
    language_servers: Vec<Arc<Client>>,
    diagnostics: Vec<DiagnosticItem>,
) {
    match serde_json::from_value(req.params.params) {
        Ok(lsp_types::CodeActionParams {
            text_document,
            range,
            ..
        }) => {
            let mut seen_language_servers = HashSet::new();
            let lang_servers = language_servers
                .iter()
                .filter(|ls| ls.with_feature(LanguageServerFeature::CodeAction));
            let mut futures: FuturesUnordered<_> = lang_servers
                .into_iter()
                .filter(|ls| seen_language_servers.insert(ls.id()))
                .filter_map(|language_server| {
                    let language_server_id = language_server.id();
                    let language_server_name = language_server.name().to_owned();
                    let code_action_context = lsp_types::CodeActionContext {
                        diagnostics: diagnostics
                            .iter()
                            .filter(|&diag| {
                                diag.provider.server_id == language_server_id
                                    && diag.item.range.start.line >= range.start.line
                                    && diag.item.range.start.line <= range.end.line
                            })
                            .map(|diag| diag.item.clone())
                            .collect(),
                        only: None,
                        trigger_kind: Some(lsp_types::CodeActionTriggerKind::INVOKED),
                    };

                    if code_action_context.diagnostics.is_empty() {
                        return None;
                    }

                    let code_action_request = language_server.code_actions(
                        req.id.clone(),
                        lsp_types::CodeActionParams {
                            text_document: text_document.clone(),
                            range,
                            context: code_action_context,
                            work_done_progress_params: lsp_types::WorkDoneProgressParams::default(),
                            partial_result_params: lsp_types::PartialResultParams::default(),
                        },
                    )?;

                    Some((
                        code_action_request,
                        language_server_id,
                        language_server_name,
                    ))
                })
                .map(|(request, ls_id, ls_name)| async move {
                    let json = request.await?;
                    let response: Option<lsp_types::CodeActionResponse> =
                        serde_json::from_value(json)?;
                    let mut actions = match response {
                        Some(a) => a,
                        None => return anyhow::Ok(Vec::new()),
                    };

                    // remove disabled code actions
                    actions.retain(|action| {
                        matches!(
                            action,
                            lsp_types::CodeActionOrCommand::Command(_)
                                | lsp_types::CodeActionOrCommand::CodeAction(CodeAction {
                                    disabled: None,
                                    ..
                                })
                        )
                    });

                    actions.sort_by(|action1, action2| {
                        let order = action_category(action1).cmp(&action_category(action2));
                        if order != Ordering::Equal {
                            return order;
                        }

                        // within the categories sort by relevancy.
                        // Modleed after the `codeActionsComparator` function in vscode:
                        // https://github.com/microsoft/vscode/blob/eaec601dd69aeb4abb63b9601a6f44308c8d8c6e/src/vs/editor/contrib/codeAction/browser/codeAction.ts

                        // if one code action fixes a diagnostic but the other one doesn't show it first.
                        let order = action_fixes_diagnostics(action1)
                            .cmp(&action_fixes_diagnostics(action2))
                            .reverse();
                        if order != Ordering::Equal {
                            return order;
                        }

                        // if one of the codeactions is marked as preferred show it first
                        // otherwise keep the original LSP sorting
                        action_preferred(action1)
                            .cmp(&action_preferred(action2))
                            .reverse()
                    });

                    Ok(actions
                        .into_iter()
                        .map(|lsp_item| CodeActionOrCommandItem {
                            lsp_item,
                            language_server_id: ls_id,
                            language_server_name: ls_name.to_owned(),
                        })
                        .collect())
                })
                .collect();

            if futures.is_empty() {
                response_sender
                    .send(
                        Response::new_ok::<Vec<CodeActionOrCommandItem>>(req.id.clone(), vec![])
                            .into(),
                    )
                    .unwrap();
                return;
            }

            let future = async move {
                let actions_future = async move {
                    let mut actions = Vec::new();

                    while let Some(mut lsp_items) = futures.try_next().await? {
                        actions.append(&mut lsp_items);
                    }

                    anyhow::Ok(actions)
                };

                let res = actions_future.await;
                let response = match res {
                    Ok(res) => Response::new_ok(req.id.clone(), res),
                    Err(e) => create_error_response(&req.id, e.to_string()),
                };

                response_sender.send(response.into()).unwrap();
            };
            future.await
        }
        Err(e) => {
            let resp = create_error_response(&req.id, format!("parse code action error {e}"));
            response_sender.send(resp.into()).unwrap();
        }
    }
}

pub(crate) async fn handle_view_file_text(
    req: msg::Request,
    params: lsp_ext::ViewFileTextParams,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    call_first_language_server::<lsp_ext::ViewFileText>(&req, params, &language_servers)
        .await
        .map(|(resp, _)| Response::new_ok(req.id.clone(), resp))
}

pub(crate) async fn handle_inlay_hints(
    req: msg::Request,
    params: lsp_types::InlayHintParams,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    call_single_language_server::<lsp_types::request::InlayHintRequest>(
        &req,
        params,
        &language_servers,
        Some(LanguageServerFeature::InlayHints),
        None,
    )
    .await
    .map(|(resp, _)| Response::new_ok(req.id.clone(), resp))
}

pub(crate) async fn handle_document_highlight(
    req: msg::Request,
    params: lsp_types::DocumentHighlightParams,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    call_single_language_server::<lsp_types::request::DocumentHighlightRequest>(
        &req,
        params,
        &language_servers,
        Some(LanguageServerFeature::DocumentHighlight),
        None,
    )
    .await
    .map(|(resp, _)| Response::new_ok(req.id.clone(), resp))
}

pub(crate) async fn handle_rename(
    req: msg::Request,
    params: lsp_types::RenameParams,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    call_single_language_server::<lsp_types::request::Rename>(
        &req,
        params,
        &language_servers,
        Some(LanguageServerFeature::RenameSymbol),
        None,
    )
    .await
    .map(|(resp, _)| Response::new_ok(req.id.clone(), resp))
}

pub(crate) async fn handle_document_symbols(
    req: msg::Request,
    params: lsp_types::DocumentSymbolParams,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    call_single_language_server::<lsp_types::request::DocumentSymbolRequest>(
        &req,
        params,
        &language_servers,
        Some(LanguageServerFeature::DocumentSymbols),
        None,
    )
    .await
    .map(|(resp, _)| {
        let mut result = lsp_symbols_to_imenu(resp);
        sort_imenu_entries_grouped(&mut result);
        Response::new_ok(req.id.clone(), result)
    })
}

pub(crate) async fn pull_diagnostics_for_document(
    req: msg::Request,
    identifier: Option<String>,
    previous_result_id: Option<String>,
    params: lsp_types::DocumentDiagnosticParams,
    language_server: &Arc<Client>,
) -> Option<lsp_types::DocumentDiagnosticReportResult> {
    let future = language_server.text_document_diagnostic(
        req.id.clone(),
        identifier,
        previous_result_id,
        params,
    )?;

    match future.await {
        Ok(result) => serde_json::from_value(result).ok(),
        Err(err) => {
            log::error!("Pull diagnostic request failed: {err}");
            None
        }
    }
}

pub(crate) async fn handle_pull_diagnostic_response(
    sender: Sender<Message>,
    provider: DiagnosticProvider,
    result: lsp_types::DocumentDiagnosticReportResult,
    document_id: DocumentId,
    limit_diagnostics: bool,
) {
    job::dispatch(move |editor| {
        let related_documents = match result {
            lsp_types::DocumentDiagnosticReportResult::Report(report) => {
                let (result_id, related_documents, diagnostics) = match report {
                    lsp_types::DocumentDiagnosticReport::Full(report) => {
                        let diagnostics = report.full_document_diagnostic_report.items;
                        (
                            report.full_document_diagnostic_report.result_id,
                            report.related_documents,
                            Some(diagnostics),
                        )
                    }
                    lsp_types::DocumentDiagnosticReport::Unchanged(report) => (
                        Some(report.unchanged_document_diagnostic_report.result_id),
                        report.related_documents,
                        None,
                    ),
                };

                if let Some(doc) = editor.document_mut(document_id) {
                    doc.previous_diagnostic_id = result_id;
                    if let Some(diags) = diagnostics {
                        let old_diags = doc.get_diagnostics_by_provider(&provider);
                        if old_diags.is_none()
                            || !is_diagnostic_vectors_equal(old_diags.as_ref().unwrap(), &diags)
                        {
                            let diagnostics: Vec<DiagnosticItem> = diags
                                .iter()
                                .map(|diag| DiagnosticItem {
                                    item: diag.to_owned(),
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

                            let diagnostics = if limit_diagnostics {
                                let max_diagnostics_push = config::MAX_DIAGNOSTICS_PUSH
                                    .get()
                                    .copied()
                                    .unwrap_or(DEFAULT_MAX_DIAGNOSTICS_PUSH);
                                limit_diagnostics_for_push(&all_diagnostics, max_diagnostics_push)
                            } else {
                                all_diagnostics
                            };

                            let not = msg::Notification::new(
                                lsp_types::notification::PublishDiagnostics::METHOD.to_string(),
                                lsp_types::PublishDiagnosticsParams {
                                    version: Some(doc.version),
                                    uri: doc.uri.clone(),
                                    diagnostics,
                                },
                            );

                            sender.send(not.into()).unwrap();
                        }
                    }
                };

                related_documents
            }
            lsp_types::DocumentDiagnosticReportResult::Partial(report) => report.related_documents,
        };

        for (url, report) in related_documents.into_iter().flatten() {
            let (result_id, diags) = match report {
                lsp_types::DocumentDiagnosticReportKind::Full(report) => {
                    (report.result_id, Some(report.items))
                }
                lsp_types::DocumentDiagnosticReportKind::Unchanged(report) => {
                    (Some(report.result_id), None)
                }
            };
            if let Some(doc) = editor.document_by_uri_mut(&url) {
                doc.previous_diagnostic_id = result_id;
                // handle lsp diagnostics
                if let Some(diags) = diags {
                    let old_diags = doc.get_diagnostics_by_provider(&provider);
                    if old_diags.is_none()
                        || !is_diagnostic_vectors_equal(old_diags.as_ref().unwrap(), &diags)
                    {
                        let diagnostics: Vec<DiagnosticItem> = diags
                            .iter()
                            .map(|diag| DiagnosticItem {
                                item: diag.to_owned(),
                                provider: provider.clone(),
                                file_path: doc
                                    .path()
                                    .map(|p| p.to_string_lossy().to_string())
                                    .unwrap_or("".to_string()),
                            })
                            .collect();
                        doc.replace_diagnostics(diagnostics, &provider);
                    }
                }
                // When open the corresponse doc, then publish the diags
            }
        }
    })
    .await;
}

pub(crate) async fn handle_inline_completion(
    req: msg::Request,
    language_server: Arc<Client>,
    _language_id: String,
    response_sender: Sender<Message>,
) {
    if let Some(Context::InlineCompletion(context)) = req.params.context {
        match serde_json::from_value::<lsp_types::InlineCompletionParams>(req.params.params) {
            Ok(params) => {
                if context.trigger_kind == lsp_types::InlineCompletionTriggerKind::Invoked
                    && params.context.trigger_kind != context.trigger_kind
                {
                    debug!("Skip automatic trigger when triggerKind is manual.");
                    response_sender
                        .send(
                            Response {
                                id: req.id,
                                result: None,
                                error: None,
                            }
                            .into(),
                        )
                        .unwrap();
                    return;
                }
                let doc_version = context.doc_version;
                let future = {
                    let request = language_server
                        .inline_completion(req.id.clone(), params.clone())
                        .unwrap();
                    async move {
                        let json = request.await?;
                        let resp: Option<lsp_types::InlineCompletionResponse> =
                            serde_json::from_value(json)?;
                        let items = match resp {
                            Some(lsp_types::InlineCompletionResponse::Array(items)) => items,
                            Some(lsp_types::InlineCompletionResponse::List(list)) => list.items,
                            None => Vec::new(),
                        };
                        anyhow::Ok(items)
                    }
                    .boxed()
                };
                let res = match future.await {
                    Ok(lsp_items) => Response::new_ok(
                        req.id.clone(),
                        lsp_ext::VersionInlineCompletionResult {
                            doc_version,
                            items: lsp_items,
                        },
                    ),
                    Err(e) => create_error_response(&req.id, e.to_string()),
                };
                response_sender.send(res.into()).unwrap();
            }
            Err(e) => {
                let resp =
                    create_error_response(&req.id, format!("parse inline completion error {e}"));
                response_sender.send(resp.into()).unwrap();
            }
        }
    } else {
        let resp = create_error_response(&req.id, "Not a inline completion context".to_string());
        response_sender.send(resp.into()).unwrap();
    }
}

pub(crate) async fn handle_ra_expand_macro(
    req: msg::Request,
    params: <RustAnalyzerExpandMacro as Request>::Params,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    debug!("requesting rust-analyzer/expandMacro");
    let ra = language_servers
        .iter()
        .find(|ls| ls.name() == "rust-analyzer");
    match ra {
        Some(ls) => {
            let json = ls
                .call::<lsp_ext::RustAnalyzerExpandMacro>(req.id.clone(), params)
                .await;
            match json {
                Ok(resp) => {
                    match serde_json::from_value::<Option<lsp_ext::ExpandMacroResult>>(resp) {
                        Ok(Some(result)) => Ok(Response::new_ok(req.id, result)),
                        Ok(None) => Err(anyhow::Error::msg(
                            "Macro expansion not available at this location",
                        )),
                        Err(err) => Err(err.into()),
                    }
                }
                Err(err) => Err(err.into()),
            }
        }
        None => Err(anyhow::Error::msg(format!(
            "No available language server for {:?}.",
            req.method
        ))),
    }
}
