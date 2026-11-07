//! Call/type hierarchies keep server origin across every lazy expansion.
use std::{collections::HashSet, sync::Arc};

use anyhow::{bail, Context as _, Result};
use futures_util::{stream::FuturesUnordered, StreamExt};
use lsp_types::request::*;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::{
    client::Client,
    msg::{Context, Request as EditorRequest, Response},
    syntax::LanguageServerFeature,
};

// lsp-types 0.95.1 models TypeHierarchyItem.tags as a scalar. Both
// hierarchy item shapes are identical in LSP, and CallHierarchyItem has the
// correct array type. Keep the workaround local to the three type requests.
type TypeHierarchyItem = lsp_types::CallHierarchyItem;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct TypeHierarchyExpandParams {
    item: TypeHierarchyItem,
    #[serde(flatten)]
    work_done_progress_params: lsp_types::WorkDoneProgressParams,
    #[serde(flatten)]
    partial_result_params: lsp_types::PartialResultParams,
}

pub(crate) enum TypeHierarchyPrepare {}
impl Request for TypeHierarchyPrepare {
    type Params = lsp_types::TypeHierarchyPrepareParams;
    type Result = Option<Vec<TypeHierarchyItem>>;
    const METHOD: &'static str = "textDocument/prepareTypeHierarchy";
}

pub(crate) enum TypeHierarchySupertypes {}
impl Request for TypeHierarchySupertypes {
    type Params = TypeHierarchyExpandParams;
    type Result = Option<Vec<TypeHierarchyItem>>;
    const METHOD: &'static str = "typeHierarchy/supertypes";
}

pub(crate) enum TypeHierarchySubtypes {}
impl Request for TypeHierarchySubtypes {
    type Params = TypeHierarchyExpandParams;
    type Result = Option<Vec<TypeHierarchyItem>>;
    const METHOD: &'static str = "typeHierarchy/subtypes";
}

pub(crate) trait HierarchyRequest: Request {
    const FEATURE: LanguageServerFeature;

    fn prepare_position(_params: &mut Self::Params) -> Option<&mut lsp_types::Position> {
        None
    }
}

macro_rules! hierarchy_requests {
    ($feature:ident: $($request:ty),+ $(,)?) => {
        $(impl HierarchyRequest for $request {
            const FEATURE: LanguageServerFeature = LanguageServerFeature::$feature;
        })+
    };
}
hierarchy_requests!(CallHierarchy: CallHierarchyIncomingCalls, CallHierarchyOutgoingCalls);
hierarchy_requests!(TypeHierarchy: TypeHierarchySupertypes, TypeHierarchySubtypes);

impl HierarchyRequest for CallHierarchyPrepare {
    const FEATURE: LanguageServerFeature = LanguageServerFeature::CallHierarchy;

    fn prepare_position(params: &mut Self::Params) -> Option<&mut lsp_types::Position> {
        Some(&mut params.text_document_position_params.position)
    }
}

impl HierarchyRequest for TypeHierarchyPrepare {
    const FEATURE: LanguageServerFeature = LanguageServerFeature::TypeHierarchy;

    fn prepare_position(params: &mut Self::Params) -> Option<&mut lsp_types::Position> {
        Some(&mut params.text_document_position_params.position)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct HierarchyItem {
    pub lsp_item: Value,
    pub language_server_id: usize,
    pub language_server_name: String,
}

fn wrap<R: Request>(value: Value, ls: &Client) -> Result<Vec<HierarchyItem>> {
    // Validate the protocol shape, but preserve the JSON (including `data`).
    serde_json::from_value::<R::Result>(value.clone()).map_err(|err| {
        anyhow::anyhow!(
            "{} returned an invalid {} response: {err}",
            ls.name(),
            R::METHOD
        )
    })?;
    Ok(value
        .as_array()
        .into_iter()
        .flatten()
        .map(|item| HierarchyItem {
            lsp_item: item.clone(),
            language_server_id: ls.id(),
            language_server_name: ls.name().into(),
        })
        .collect())
}

pub(crate) async fn prepare<R: HierarchyRequest>(
    req: EditorRequest,
    mut params: R::Params,
    clients: Vec<Arc<Client>>,
) -> Result<Response>
where
    R::Params: Clone,
{
    if let (Some(context), Some(position)) = (
        req.params.virtual_doc.as_ref(),
        R::prepare_position(&mut params),
    ) {
        *position = context.translate_position_to_virtual(*position);
    }
    let mut seen = HashSet::new();
    let mut calls: FuturesUnordered<_> = clients
        .into_iter()
        .filter(|ls| ls.with_feature(R::FEATURE) && seen.insert(ls.id()))
        .map(|ls| {
            let params = params.clone();
            let id = req.id.clone();
            async move {
                let value = ls.hierarchy::<R>(id, params).unwrap().await;
                let result = value
                    .map_err(anyhow::Error::from)
                    .and_then(|v| wrap::<R>(v, &ls));
                (ls.name().to_string(), result)
            }
        })
        .collect();
    if calls.is_empty() {
        bail!(
            "No initialized language server supports {} for this document",
            R::METHOD
        );
    }
    let mut items = Vec::new();
    let mut failures = Vec::new();
    let mut succeeded = false;
    while let Some((name, result)) = calls.next().await {
        match result {
            Ok(mut response) => {
                succeeded = true;
                items.append(&mut response);
            }
            Err(err) => {
                log::warn!("{} failed for {name}: {err:#}", R::METHOD);
                failures.push(format!("{name}: {err:#}"));
            }
        }
    }
    if !succeeded {
        bail!("{} failed: {}", R::METHOD, failures.join("; "));
    }
    // Keep same-name roots from different servers distinct, with stable ordering.
    items.sort_by_key(|item| (item.language_server_id, item.lsp_item["name"].to_string()));
    Ok(Response::new_ok(req.id, items))
}

pub(crate) async fn expand<R: HierarchyRequest>(
    req: EditorRequest,
    params: R::Params,
    clients: Vec<Arc<Client>>,
) -> Result<Response> {
    let id = match req.params.context.as_ref() {
        Some(Context::Common(context)) => context.language_server_id,
        _ => bail!("{} requires the item's language-server-id", R::METHOD),
    };
    let ls = clients
        .iter()
        .find(|ls| ls.id() == id && ls.with_feature(R::FEATURE))
        .context(
            "The hierarchy item's language server is no longer available; reopen the hierarchy",
        )?;
    let value = ls
        .hierarchy::<R>(req.id.clone(), params)
        .context("The language server no longer supports this hierarchy")?
        .await
        .with_context(|| format!("{} failed for {}", R::METHOD, ls.name()))?;
    Ok(Response::new_ok(req.id, wrap::<R>(value, ls)?))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::msg::CommonContext;
    use serde_json::json;

    pub(crate) async fn initialized_client(name: &str, id: usize) -> Arc<Client> {
        let (client, _) = crate::test_support::client(name, id, "file:///w/Bean.java", false).await;
        let result = client.initialize(false).await.unwrap();
        client.capabilities.set(result.capabilities).unwrap();
        client
    }

    fn position() -> Value {
        json!({"textDocument":{"uri":"file:///w/Bean.java"},"position":{"line":0,"character":1}})
    }

    #[tokio::test]
    async fn test_hierarchy_initialize_capabilities_and_guards() {
        for name in ["hierarchy", "hierarchy-options", "hierarchy-unsupported"] {
            let client = initialized_client(name, 1).await;
            let params = client
                .call_method(100.into(), "test/initializeParams".into(), Value::Null)
                .await
                .unwrap();
            for capability in ["callHierarchy", "typeHierarchy"] {
                assert_eq!(
                    params["capabilities"]["textDocument"][capability]["dynamicRegistration"],
                    false
                );
            }
            for feature in [
                LanguageServerFeature::CallHierarchy,
                LanguageServerFeature::TypeHierarchy,
            ] {
                assert_eq!(
                    client.with_feature(feature),
                    name != "hierarchy-unsupported"
                );
            }
        }
        let (uninitialized, _) =
            crate::test_support::client("hierarchy", 5, "file:///w/Bean.java", false).await;
        assert!(!uninitialized.with_feature(LanguageServerFeature::CallHierarchy));
        assert!(uninitialized
            .hierarchy::<TypeHierarchyPrepare>(
                1.into(),
                serde_json::from_value(position()).unwrap()
            )
            .is_none());
    }

    #[tokio::test]
    async fn test_hierarchy_prepare_translates_org_position_and_keeps_items_virtual() {
        let client = initialized_client("hierarchy-virtual", 1).await;
        for method in [CallHierarchyPrepare::METHOD, TypeHierarchyPrepare::METHOD] {
            let mut request = crate::test_support::request(method, None);
            request.params.virtual_doc = Some(crate::msg::VirtualDocContext {
                line_bias: 10,
                language: "java".into(),
                source_type: crate::msg::VirtualDocSourceType::OrgBabel,
            });
            let mut params = position();
            params["position"]["line"] = json!(11);
            let response = if method == CallHierarchyPrepare::METHOD {
                prepare::<CallHierarchyPrepare>(
                    request,
                    serde_json::from_value(params).unwrap(),
                    vec![client.clone()],
                )
                .await
            } else {
                prepare::<TypeHierarchyPrepare>(
                    request,
                    serde_json::from_value(params).unwrap(),
                    vec![client.clone()],
                )
                .await
            }
            .unwrap()
            .result
            .unwrap();
            assert_eq!(
                response[0]["lsp_item"]["selectionRange"]["start"]["line"],
                0
            );
        }
    }

    #[tokio::test]
    async fn test_hierarchy_prepare_aggregation_partial_failure_and_null() {
        let first = initialized_client("hierarchy", 1).await;
        let second = initialized_client("hierarchy-options", 2).await;
        let bad = initialized_client("hierarchy-error", 3).await;
        let null = initialized_client("hierarchy-null", 4).await;
        let unsupported = initialized_client("hierarchy-unsupported", 5).await;
        let params: lsp_types::CallHierarchyPrepareParams =
            serde_json::from_value(position()).unwrap();
        let response = prepare::<CallHierarchyPrepare>(
            crate::test_support::request(CallHierarchyPrepare::METHOD, None),
            params.clone(),
            vec![
                first.clone(),
                first.clone(),
                second.clone(),
                bad.clone(),
                null.clone(),
                unsupported.clone(),
            ],
        )
        .await
        .unwrap()
        .result
        .unwrap();
        assert_eq!(response.as_array().unwrap().len(), 2);
        assert_eq!(response[1]["language_server_id"], 2);
        assert_eq!(
            response[1]["lsp_item"]["data"]["owner"],
            "hierarchy-options"
        );
        assert_eq!(
            prepare::<CallHierarchyPrepare>(
                crate::test_support::request(CallHierarchyPrepare::METHOD, None),
                params.clone(),
                vec![null]
            )
            .await
            .unwrap()
            .result
            .unwrap(),
            json!([])
        );
        assert!(prepare::<CallHierarchyPrepare>(
            crate::test_support::request(CallHierarchyPrepare::METHOD, None),
            params.clone(),
            vec![bad]
        )
        .await
        .unwrap_err()
        .to_string()
        .contains("hierarchy RPC failure"));
        assert!(prepare::<CallHierarchyPrepare>(
            crate::test_support::request(CallHierarchyPrepare::METHOD, None),
            params,
            vec![unsupported]
        )
        .await
        .is_err());
        let result = prepare::<TypeHierarchyPrepare>(
            crate::test_support::request(TypeHierarchyPrepare::METHOD, None),
            serde_json::from_value(position()).unwrap(),
            vec![first, second],
        )
        .await
        .unwrap()
        .result
        .unwrap();
        assert_eq!(result.as_array().unwrap().len(), 2);
    }

    async fn check_expansion<R: HierarchyRequest>(item: Value, clients: Vec<Arc<Client>>) -> Value {
        expand::<R>(
            crate::test_support::request(
                R::METHOD,
                Some(Context::Common(CommonContext {
                    language_server_id: 2,
                })),
            ),
            serde_json::from_value(json!({"item":item})).unwrap(),
            clients,
        )
        .await
        .unwrap()
        .result
        .unwrap()
    }

    #[tokio::test]
    async fn test_type_hierarchy_tags_arrays_round_trip() {
        for (name, tags) in [
            ("hierarchy-tags-empty", json!([])),
            ("hierarchy-tags-deprecated", json!([1])),
        ] {
            let client = initialized_client(name, 2).await;
            let roots = prepare::<TypeHierarchyPrepare>(
                crate::test_support::request(TypeHierarchyPrepare::METHOD, None),
                serde_json::from_value(position()).unwrap(),
                vec![client.clone()],
            )
            .await
            .unwrap()
            .result
            .unwrap();
            let item = roots[0]["lsp_item"].clone();
            assert_eq!(item["tags"], tags);
            for result in [
                check_expansion::<TypeHierarchySupertypes>(item.clone(), vec![client.clone()])
                    .await,
                check_expansion::<TypeHierarchySubtypes>(item.clone(), vec![client.clone()]).await,
            ] {
                assert_eq!(result[0]["lsp_item"]["tags"], tags);
                assert_eq!(result[0]["lsp_item"]["data"], item["data"]);
            }
            let params: TypeHierarchyExpandParams = serde_json::from_value(json!({
                "item": item, "workDoneToken": "work", "partialResultToken": 3
            }))
            .unwrap();
            let params = serde_json::to_value(params).unwrap();
            assert_eq!(params["workDoneToken"], "work");
            assert_eq!(params["partialResultToken"], 3);
        }
    }

    #[tokio::test]
    async fn test_hierarchy_expansion_origin_data_and_call_ranges() {
        let first = initialized_client("hierarchy", 1).await;
        let second = initialized_client("hierarchy-options", 2).await;
        let clients = vec![first, second];
        let roots = prepare::<CallHierarchyPrepare>(
            crate::test_support::request(CallHierarchyPrepare::METHOD, None),
            serde_json::from_value(position()).unwrap(),
            clients.clone(),
        )
        .await
        .unwrap()
        .result
        .unwrap();
        let item = roots[1]["lsp_item"].clone();
        for result in [
            check_expansion::<CallHierarchyIncomingCalls>(item.clone(), clients.clone()).await,
            check_expansion::<CallHierarchyOutgoingCalls>(item.clone(), clients.clone()).await,
            check_expansion::<TypeHierarchySupertypes>(item.clone(), clients.clone()).await,
            check_expansion::<TypeHierarchySubtypes>(item.clone(), clients.clone()).await,
        ] {
            assert_eq!(result[0]["language_server_id"], 2);
            let edge = &result[0]["lsp_item"];
            let node = edge.get("from").or_else(|| edge.get("to")).unwrap_or(edge);
            assert_eq!(node["data"], item["data"]);
            if edge.get("fromRanges").is_some() {
                assert_eq!(edge["fromRanges"][0]["start"]["character"], 1);
            }
        }
        let params: lsp_types::CallHierarchyIncomingCallsParams =
            serde_json::from_value(json!({"item":item})).unwrap();
        assert!(expand::<CallHierarchyIncomingCalls>(
            crate::test_support::request(CallHierarchyIncomingCalls::METHOD, None),
            params.clone(),
            clients.clone()
        )
        .await
        .is_err());
        let missing = Some(Context::Common(CommonContext {
            language_server_id: 99,
        }));
        assert!(expand::<CallHierarchyIncomingCalls>(
            crate::test_support::request(CallHierarchyIncomingCalls::METHOD, missing),
            params,
            clients
        )
        .await
        .unwrap_err()
        .to_string()
        .contains("no longer available"));
    }

    #[tokio::test]
    async fn test_hierarchy_invalid_response_and_expansion_failures() {
        for name in ["hierarchy-invalid", "hierarchy-error", "hierarchy-null"] {
            let client = initialized_client(name, 2).await;
            let item = json!({"name":"Bean","kind":5,"uri":"file:///w/Bean.java","range":{"start":{"line":0,"character":1},"end":{"line":0,"character":4}},"selectionRange":{"start":{"line":0,"character":1},"end":{"line":0,"character":4}},"data":{"owner":name,"opaque":[1,{"x":"y"}]}});
            let result = expand::<TypeHierarchySubtypes>(
                crate::test_support::request(
                    TypeHierarchySubtypes::METHOD,
                    Some(Context::Common(CommonContext {
                        language_server_id: 2,
                    })),
                ),
                serde_json::from_value(json!({"item":item})).unwrap(),
                vec![client.clone()],
            )
            .await;
            if name == "hierarchy-null" {
                assert_eq!(result.unwrap().result.unwrap(), json!([]));
            } else {
                let error = result.unwrap_err().to_string();
                assert!(error.contains(name));
                assert!(error.contains(TypeHierarchySubtypes::METHOD));
                assert!(prepare::<TypeHierarchyPrepare>(
                    crate::test_support::request(TypeHierarchyPrepare::METHOD, None),
                    serde_json::from_value(position()).unwrap(),
                    vec![client]
                )
                .await
                .is_err());
            }
        }
    }
}
