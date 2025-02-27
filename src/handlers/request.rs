use anyhow::Result;
use core::fmt;
use crossbeam_channel::Sender;
use futures_util::{stream::FuturesUnordered, TryStreamExt};
use itertools::Itertools;
use lazy_static::lazy_static;
use log::{debug, error, info};
use lsp_types::{request::Request, CodeAction};
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
    document::DiagnosticItem,
    fuzzy,
    lsp::jsonrpc,
    lsp_ext::{self, CommandItem, CompletionItem},
    msg::{self, Context, Message, RequestId, Response},
    syntax::LanguageServerFeature,
    utils::truncate_completion_item,
};

pub fn create_error_response(id: &RequestId, message: String) -> Response {
    error!("result to response err {}", message);
    Response::new_err(id.clone(), jsonrpc::ErrorCode::InternalError, message)
}

async fn call_single_language_server<R>(
    req: &msg::Request,
    params: R::Params,
    language_servers: &Vec<Arc<Client>>,
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
    language_servers: &Vec<Arc<Client>>,
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
    language_servers: &Vec<Arc<Client>>,
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
        None => Err(anyhow::Error::msg(format!(
            "No language server available. Please check the log file by M-x lsp-proxy-open-log-file."))),
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
                    Ok(Response::new_ok(req.id, resp).into())
                }
                Err(e) => Err(e.into()),
            },
            Err(err) => Err(err.into()),
        }
    } else {
        let ls_names = language_servers.iter().map(|ls| ls.name()).join("、");
        let error_msg = if ls_names.is_empty() {
            format!("No language server available. Please check the log file by M-x lsp-proxy-open-log-file.")
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
    .and_then(|(resp, _)| Ok(Response::new_ok(req.id.clone(), resp)))
}

lazy_static! {
    static ref COMPLETION_CACHE: Mutex<CompletionCache> = Mutex::new(CompletionCache::new());
}

pub(crate) async fn handle_completion(
    req: msg::Request,
    params: lsp_types::CompletionParams,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    if let Some(Context::CompletionContext(context)) = req.params.context {
        let prefix_len = &context.prefix.len();
        let bounds_start = &context.bounds_start;
        let trigger_kind = &context.trigger_kind;
        let pretext = &context
            .line
            .slice(0..params.text_document_position.position.character as usize);
        debug!("prefix len {} {:?}", prefix_len, pretext);
        let uri = &req.params.uri.to_owned();
        if trigger_kind != &lsp_types::CompletionTriggerKind::INVOKED {
            if let Some(items) = COMPLETION_CACHE.try_lock().unwrap().get_cached_items(
                &uri.as_ref().unwrap(),
                pretext,
                &context.prefix,
                &bounds_start,
            ) {
                let now = Instant::now();
                let filtered_items = fuzzy::filter::filter(
                    pretext,
                    &items,
                    params.text_document_position.position,
                    &context.prefix,
                );
                debug!("filter elapsed {:0.2?}", now.elapsed());
                let slice_length = std::cmp::min(20, filtered_items.len());
                let slice_items = &filtered_items[..slice_length];
                if slice_items.len() > 0 {
                    debug!("return cached items");
                    return Ok(Response::new_ok(req.id.clone(), slice_items).into());
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
                    return seen_language_servers.insert(ls.id());
                } else if ls.name() == "typescript-language-server" && pretext.ends_with(" ") {
                    // typescript-language-server 未拦截空格触发的情况，但 vscode 和 vtsls 支持拦截
                    return false;
                } else {
                    // 此时启动触发且 prefix_len 为空，如果不是 triggerCharacter 则不自动请求
                    let trigger_character = match &ls.capabilities().completion_provider {
                        Some(lsp_types::CompletionOptions {
                            trigger_characters: Some(triggers),
                            ..
                        }) => triggers.iter().find(|trigger| pretext.ends_with(*trigger)),
                        _ => None,
                    };
                    return trigger_character.is_some();
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
                                    trigger_character.map(|c| c.clone())
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
                            end: context.start_point as i32
                                + (*label_len as i32 - *prefix_len as i32),
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
                            error!("Future Error: {}", e);
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
                        pretext.to_owned().to_owned(),
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
                    let slice_length = std::cmp::min(20, filtered_items.len());
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
        Err(anyhow::anyhow!("Not get a completion context: {:?}.", req))
    }
}

pub(crate) async fn handle_completion_resolve(
    req: msg::Request,
    params: lsp_types::CompletionItem,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    if let Some(Context::ResolveContext(context)) = &req.params.context {
        let params_detail = params.detail.clone();
        call_single_language_server::<lsp_types::request::ResolveCompletionItem>(
            &req,
            params,
            &language_servers,
            Some(LanguageServerFeature::CompletionResolve),
            Some(context.language_server_id),
        )
        .await
        .and_then(|(mut resp, language_server_name)| {
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

            if let Some(text_edit) = &resp.text_edit {
                if let lsp_types::CompletionTextEdit::InsertAndReplace(replace_text_edit) =
                    text_edit
                {
                    resp.text_edit =
                        Some(lsp_types::CompletionTextEdit::Edit(lsp_types::TextEdit {
                            range: replace_text_edit.replace,
                            new_text: replace_text_edit.new_text.clone(),
                        }));
                }
            }
            Ok(Response::new_ok(
                req.id.clone(),
                CompletionItem {
                    item: resp,
                    language_server_id: Some(context.language_server_id),
                    language_server_name,
                    start: context.start,
                    end: context.end,
                },
            ))
        })
    } else {
        error!("No Resolve Context {:?}", req.params);
        Err(anyhow::Error::msg(format!(
            "No Resolve Context {:?}",
            req.params
        )))
    }
}

pub(crate) async fn handle_inline_completion(
    req: msg::Request,
    params: lsp_types::InlineCompletionParams,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    if let Some(Context::InlineCompletionContext(context)) = req.params.context {
        log::trace!("handing inline completion {:?}", params);
        if context.trigger_kind == lsp_types::InlineCompletionTriggerKind::Invoked
            && params.context.trigger_kind != context.trigger_kind
        {
            debug!("Skip automatic trigger when triggerKind is manual.");
            return Ok(Response {
                id: req.id,
                result: None,
                error: None,
            });
        }
        let mut futures: FuturesUnordered<_> = language_servers
            .iter()
            .filter(|ls| ls.with_feature(LanguageServerFeature::InlineCompletion))
            .map(|ls| {
                let cmp_start = Instant::now();
                let language_server_name = ls.name().to_owned();
                let request = ls
                    .inline_completion(req.id.clone(), params.clone())
                    .unwrap();

                let id = req.id.clone();
                async move {
                    let json = request.await?;
                    info!(
                        "{:?} [{:?}] await ${:0.2?}",
                        language_server_name,
                        id.clone(),
                        cmp_start.elapsed()
                    );
                    let resp: Option<lsp_types::InlineCompletionResponse> =
                        serde_json::from_value(json)?;

                    let items = match resp {
                        Some(lsp_types::InlineCompletionResponse::Array(items)) => items,
                        Some(lsp_types::InlineCompletionResponse::List(list)) => list.items,
                        None => Vec::new(),
                    };
                    anyhow::Ok(items)
                }
            })
            .collect();
        let future = async move {
            let res = async move {
                let mut items = Vec::new();
                loop {
                    match futures.try_next().await {
                        Ok(Some(mut lsp_items)) => {
                            items.append(&mut lsp_items);
                        }
                        Ok(None) => break,
                        Err(e) => {
                            error!("Future Error: {}", e);
                            continue;
                        }
                    }
                }
                anyhow::Ok(items)
            }
            .await;
            match res {
                Ok(res) => Response::new_ok(req.id.clone(), res),
                Err(e) => create_error_response(&req.id, e.to_string()),
            }
        };
        let requst_start = Instant::now();
        let resp = future.await;
        info!("line completion await end {:0.2?}", requst_start.elapsed());

        Ok(resp)
    } else {
        Err(anyhow::Error::msg("Not a inline completion context."))
    }
}

pub(crate) async fn handle_code_action_resolve(
    req: msg::Request,
    code_action: lsp_types::CodeAction,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    if let Some(Context::CommonContext(context)) = &req.params.context {
        call_single_language_server::<lsp_types::request::CodeActionResolveRequest>(
            &req,
            code_action,
            &language_servers,
            None,
            Some(context.language_server_id),
        )
        .await
        .and_then(|(action, _)| {
            Ok(Response::new_ok(
                req.id,
                CodeActionOrCommandItem {
                    lsp_item: action.into(),
                    language_server_id: context.language_server_id,
                    language_server_name: format!("{:?}", context.language_server_id),
                },
            ))
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
    .and_then(|(resp, _)| {
        let edits = resp.unwrap_or_else(|| Default::default());
        Ok(Response::new_ok(req.id.clone(), edits).into())
    })
}

pub(crate) async fn handle_execute_command(
    req: msg::Request,
    params: lsp_types::ExecuteCommandParams,
    language_servers: Vec<Arc<Client>>,
) -> Result<Response> {
    if let Some(Context::CommonContext(context)) = &req.params.context {
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
                .and_then(|_| Ok(Response::new_ok(req.id, "")));
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
        .and_then(|_| Ok(Response::new_ok(req.id, "")))
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

    let results: Vec<lsp_types::SignatureHelp> = resps
        .into_iter()
        .filter_map(|resp| match resp {
            Some(signature_help) => Some(signature_help),
            None => None,
        })
        .collect();
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
                                diag.language_server_id == language_server_id
                                    && diag.item.range.start.line >= range.start.line
                                    && diag.item.range.start.line <= range.end.line
                            })
                            .map(|diag| diag.item.clone())
                            .collect(),
                        only: None,
                        trigger_kind: Some(lsp_types::CodeActionTriggerKind::INVOKED),
                    };

                    if code_action_context.diagnostics.len() == 0 {
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
            let resp = create_error_response(
                &req.id,
                format!("parse code action error {}", e.to_string()),
            );
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
        .and_then(|(resp, _)| Ok(Response::new_ok(req.id.clone(), resp)))
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
    .and_then(|(resp, _)| Ok(Response::new_ok(req.id.clone(), resp)))
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
    .and_then(|(resp, _)| Ok(Response::new_ok(req.id.clone(), resp)))
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
    .and_then(|(resp, _)| Ok(Response::new_ok(req.id.clone(), resp)))
}
