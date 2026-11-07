//! Workspace symbols retain their server origin for lazy resolution.
use std::{collections::HashSet, sync::Arc};

use anyhow::{bail, Context as _, Result};
use futures_util::{stream::FuturesUnordered, StreamExt};
use lsp_types::{request::WorkspaceSymbolRequest, WorkspaceSymbol, WorkspaceSymbolParams};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::{
    client::Client,
    msg::{Context, Request, Response},
    syntax::LanguageServerFeature,
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct WorkspaceSymbolItem {
    pub lsp_item: Value,
    pub language_server_id: usize,
    pub language_server_name: String,
}

fn wrap_response(value: Value, id: usize, name: &str) -> Result<Vec<WorkspaceSymbolItem>> {
    // Validate both protocol forms, but keep the original JSON: deserializing the
    // old form first would discard a new symbol's `data` and break resolve.
    serde_json::from_value::<Option<lsp_types::WorkspaceSymbolResponse>>(value.clone())?;
    Ok(value
        .as_array()
        .into_iter()
        .flatten()
        .map(|symbol| WorkspaceSymbolItem {
            lsp_item: symbol.clone(),
            language_server_id: id,
            language_server_name: name.into(),
        })
        .collect())
}

fn deduplicate(items: &mut Vec<WorkspaceSymbolItem>) {
    items.sort_by_key(|item| {
        (
            item.lsp_item["name"]
                .as_str()
                .unwrap_or_default()
                .to_string(),
            item.lsp_item["location"].to_string(),
            item.language_server_name.clone(),
            item.language_server_id,
        )
    });
    let mut seen = HashSet::new();
    items.retain(|item| {
        let symbol = &item.lsp_item;
        // URI-only locations cannot identify a symbol until it is resolved.
        symbol["location"]["range"].is_null()
            || seen.insert(
                serde_json::json!([symbol["name"], symbol["kind"], symbol["location"]]).to_string(),
            )
    });
}

pub(crate) async fn search(
    req: Request,
    params: WorkspaceSymbolParams,
    clients: Vec<Arc<Client>>,
) -> Result<Response> {
    let mut seen = HashSet::new();
    let mut calls: FuturesUnordered<_> = clients
        .into_iter()
        .filter(|ls| {
            ls.with_feature(LanguageServerFeature::WorkspaceSymbols) && seen.insert(ls.id())
        })
        .map(|ls| {
            let params = params.clone();
            let id = req.id.clone();
            async move {
                let result = ls.call::<WorkspaceSymbolRequest>(id, params).await;
                (ls.id(), ls.name().to_string(), result)
            }
        })
        .collect();
    if calls.is_empty() {
        bail!("No initialized language server supports workspace/symbol for this document");
    }
    let mut items = Vec::new();
    let mut succeeded = false;
    let mut failures = Vec::new();
    while let Some((id, name, result)) = calls.next().await {
        let result = result
            .map_err(anyhow::Error::from)
            .and_then(|value| wrap_response(value, id, &name));
        match result {
            Ok(mut response) => {
                succeeded = true;
                items.append(&mut response);
            }
            Err(err) => {
                log::warn!("workspace/symbol failed for {name}: {err}");
                failures.push(format!("{name}: {err}"));
            }
        }
    }
    if !succeeded {
        bail!("Workspace symbol search failed: {}", failures.join("; "));
    }
    deduplicate(&mut items);
    Ok(Response::new_ok(req.id, items))
}

pub(crate) async fn resolve(
    req: Request,
    symbol: WorkspaceSymbol,
    clients: Vec<Arc<Client>>,
) -> Result<Response> {
    let id = match req.params.context.as_ref() {
        Some(Context::Common(context)) => context.language_server_id,
        _ => bail!("workspaceSymbol/resolve requires the symbol's language-server-id"),
    };
    let ls = clients
        .iter()
        .find(|ls| ls.id() == id && ls.is_initialized())
        .context("The symbol's language server is no longer attached; search again")?;
    let value = ls
        .call::<lsp_types::request::WorkspaceSymbolResolve>(req.id.clone(), symbol)
        .await?;
    serde_json::from_value::<WorkspaceSymbol>(value.clone())?;
    Ok(Response::new_ok(
        req.id,
        WorkspaceSymbolItem {
            lsp_item: value,
            language_server_id: id,
            language_server_name: ls.name().into(),
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[tokio::test]
    async fn test_transport_aggregation_partial_failure_and_resolve_origin() {
        use crate::test_support::{client, request};
        let (old, _) = client("old", 1, "file:///one/Bean.java", true).await;
        let (new, _) = client("new", 2, "file:///two/Bean.java", true).await;
        let (bad, _) = client("bad", 3, "file:///three/Bean.java", true).await;
        let response = search(
            request("workspace/symbol", None),
            WorkspaceSymbolParams {
                query: "@+ bean".into(),
                ..Default::default()
            },
            vec![old.clone(), old.clone(), new.clone(), bad.clone()],
        )
        .await
        .unwrap();
        let items: Vec<WorkspaceSymbolItem> =
            serde_json::from_value(response.result.unwrap()).unwrap();
        assert_eq!(items.len(), 2);
        let item = items
            .iter()
            .find(|item| item.language_server_id == 2)
            .unwrap();
        let symbol = serde_json::from_value(item.lsp_item.clone()).unwrap();
        let context = Context::Common(crate::msg::CommonContext {
            language_server_id: 2,
        });
        let resolved = resolve(
            request("workspaceSymbol/resolve", Some(context)),
            symbol,
            vec![old, new],
        )
        .await
        .unwrap()
        .result
        .unwrap();
        assert_eq!(resolved["language_server_id"], 2);
        assert_eq!(resolved["lsp_item"]["data"]["owner"], "new");
        assert_eq!(
            resolved["lsp_item"]["location"]["range"]["start"]["line"],
            2
        );
        assert!(search(
            request("workspace/symbol", None),
            Default::default(),
            vec![bad]
        )
        .await
        .is_err());
        let (empty, _) = client("null", 4, "file:///empty.java", true).await;
        assert_eq!(
            search(
                request("workspace/symbol", None),
                Default::default(),
                vec![empty]
            )
            .await
            .unwrap()
            .result
            .unwrap(),
            json!([])
        );
        assert!(search(
            request("workspace/symbol", None),
            Default::default(),
            vec![]
        )
        .await
        .is_err());
    }

    #[test]
    fn test_wrap_keeps_new_symbol_data_and_old_locations() {
        let symbol = json!({"name":"@+ bean", "kind":5,
            "location":{"uri":"file:///a.java", "range":{"start":{"line":0,"character":0},"end":{"line":0,"character":4}}},
            "data":{"id":42}, "deprecated":true});
        let items = wrap_response(json!([symbol.clone()]), 7, "spring-boot").unwrap();
        assert_eq!(items[0].lsp_item, symbol);
        assert_eq!(items[0].language_server_id, 7);
        let unresolved =
            json!({"name":"bean", "kind":5, "location":{"uri":"file:///b.java"}, "data":17});
        assert_eq!(
            wrap_response(json!([unresolved.clone()]), 9, "jdtls").unwrap()[0].lsp_item,
            unresolved
        );
        assert!(wrap_response(Value::Null, 9, "jdtls").unwrap().is_empty());
        assert!(wrap_response(json!({"unexpected":true}), 9, "jdtls").is_err());
    }

    #[test]
    fn test_deduplicate_keeps_same_names_at_distinct_or_unresolved_locations() {
        let make = |line| {
            json!({"name":"bean","kind":5,"location":{"uri":"file:///a.java",
            "range":{"start":{"line":line,"character":0},"end":{"line":line,"character":4}}}})
        };
        let mut items = wrap_response(json!([make(1), make(2)]), 1, "jdtls").unwrap();
        items.extend(wrap_response(json!([make(1)]), 2, "spring-boot").unwrap());
        let partial = json!({"name":"bean","kind":5,"location":{"uri":"file:///a.java"},"data":1});
        items.extend(wrap_response(json!([partial.clone(), partial]), 3, "other").unwrap());
        deduplicate(&mut items);
        assert_eq!(items.len(), 4);
    }
}
