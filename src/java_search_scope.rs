//! JDTLS search scope changes use workspace configuration, as VS Code does.
use anyhow::{anyhow, bail, Result};
use serde_json::{json, Value};
use std::sync::Arc;

use crate::{
    client::Client,
    lsp_ext::{JavaSearchScopeParams, JavaSearchScopeResult, JavaSearchScopeValue},
    msg::{Request, Response},
};

pub(crate) fn configured_scope(config: Option<&Value>) -> String {
    config
        .and_then(|config| config.pointer("/settings/java/search/scope"))
        .and_then(Value::as_str)
        .unwrap_or("all")
        .to_owned()
}

pub(crate) fn with_scope(config: Option<&Value>, scope: JavaSearchScopeValue) -> Result<Value> {
    let mut updated = config.cloned().unwrap_or_else(|| json!({}));
    let mut settings = &mut updated;
    for key in ["settings", "java", "search"] {
        settings = settings
            .as_object_mut()
            .ok_or_else(|| anyhow!("Invalid JDTLS configuration: {key} parent is not an object"))?
            .entry(key)
            .or_insert_with(|| json!({}));
    }
    settings
        .as_object_mut()
        .ok_or_else(|| anyhow!("Invalid JDTLS configuration: java.search is not an object"))?
        .insert("scope".into(), serde_json::to_value(scope)?);
    Ok(updated)
}

pub(crate) async fn handle(
    req: Request,
    params: JavaSearchScopeParams,
    servers: Vec<Arc<Client>>,
) -> Result<Response> {
    let server = servers
        .iter()
        .find(|server| {
            server.name() == params.server_name
                && params.server_id.map_or(true, |id| id == server.id())
        })
        .ok_or_else(|| anyhow!("The selected JDTLS instance is no longer available"))?;
    if !server.is_initialized() {
        bail!("JDTLS is still initializing; try changing search scope again later");
    }
    let scope = server.java_search_scope(params.scope)?;
    Ok(Response::new_ok(
        req.id,
        JavaSearchScopeResult {
            scope,
            server_id: server.id(),
            workspace_root: server.root_path.to_string_lossy().into_owned(),
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support;

    enum GetConfigurations {}
    impl lsp_types::request::Request for GetConfigurations {
        type Params = ();
        type Result = Value;
        const METHOD: &'static str = "test/configurations";
    }

    fn params(
        scope: Option<JavaSearchScopeValue>,
        server_id: Option<usize>,
    ) -> JavaSearchScopeParams {
        JavaSearchScopeParams {
            scope,
            server_id,
            server_name: "jdtls".into(),
        }
    }

    #[test]
    fn test_scope_update_preserves_initialization_and_other_java_settings() {
        let config = json!({
            "bundles": ["/test.jar"],
            "extendedClientCapabilities": {"classFileContentsSupport": true},
            "settings": {"java": {
                "search": {"scope": "main", "other": 7},
                "referencesCodeLens": {"enabled": true},
                "configuration": {"runtimes": [{"name": "JavaSE-17", "path": "/jdk"}]}
            }, "other": {"enabled": false}}
        });
        let updated = with_scope(Some(&config), JavaSearchScopeValue::All).unwrap();
        let mut expected = config.clone();
        expected["settings"]["java"]["search"]["scope"] = json!("all");
        assert_eq!(updated, expected);
        assert_eq!(configured_scope(Some(&config)), "main");
    }

    #[test]
    fn test_scope_defaults_and_sparse_configuration() {
        assert_eq!(configured_scope(None), "all");
        assert_eq!(configured_scope(Some(&json!({}))), "all");
        let config = with_scope(None, JavaSearchScopeValue::Main).unwrap();
        assert_eq!(
            config,
            json!({"settings":{"java":{"search":{"scope":"main"}}}})
        );
        assert_eq!(configured_scope(Some(&config)), "main");
        assert_eq!(
            configured_scope(Some(
                &json!({"settings":{"java":{"search":{"scope":"projectOnly"}}}})
            )),
            "projectOnly"
        );
    }

    #[test]
    fn test_invalid_configuration_is_not_overwritten() {
        for config in [
            json!(false),
            json!({"settings":null}),
            json!({"settings":{"java":[]}}),
            json!({"settings":{"java":{"search":"main"}}}),
        ] {
            assert!(with_scope(Some(&config), JavaSearchScopeValue::All).is_err());
        }
    }

    #[test]
    fn test_scope_wire_validation() {
        let read: JavaSearchScopeParams = serde_json::from_value(json!({})).unwrap();
        assert!(read.scope.is_none());
        assert_eq!(read.server_name, "jdtls");
        for scope in [
            JavaSearchScopeValue::Main,
            JavaSearchScopeValue::All,
            JavaSearchScopeValue::ProjectOnly,
        ] {
            let value = serde_json::to_value(scope).unwrap();
            let params: JavaSearchScopeParams =
                serde_json::from_value(json!({"scope": value})).unwrap();
            assert_eq!(serde_json::to_value(params.scope.unwrap()).unwrap(), value);
        }
        assert_eq!(
            serde_json::to_value(JavaSearchScopeValue::ProjectOnly).unwrap(),
            json!("projectOnly")
        );
        for scope in [json!("projectonly"), json!("bad"), json!(1), json!(false)] {
            assert!(
                serde_json::from_value::<JavaSearchScopeParams>(json!({"scope":scope})).is_err()
            );
        }
    }

    #[tokio::test]
    async fn test_scope_changes_are_targeted_and_transport_settings_match_config() {
        let (jdt, _rx) = test_support::client("jdtls", 7, "file:///Scope.java", true).await;
        let (other, _other_rx) = test_support::client("other", 8, "file:///Scope.java", true).await;
        let servers = vec![other.clone(), jdt.clone()];
        let req = test_support::request("emacs/javaSearchScope", None);
        let response = handle(req.clone(), params(None, None), servers.clone())
            .await
            .unwrap();
        assert_eq!(response.result.unwrap()["scope"], "all");
        assert!(jdt.config().is_none());
        for scope in [
            JavaSearchScopeValue::Main,
            JavaSearchScopeValue::All,
            JavaSearchScopeValue::ProjectOnly,
        ] {
            let response = handle(req.clone(), params(Some(scope), Some(7)), servers.clone())
                .await
                .unwrap();
            let result: JavaSearchScopeResult =
                serde_json::from_value(response.result.unwrap()).unwrap();
            assert_eq!(result.server_id, 7);
            assert_eq!(result.scope, configured_scope(jdt.config().as_ref()));
            assert!(other.config().is_none());
        }
        let notifications = jdt.request::<GetConfigurations>(()).await.unwrap();
        assert_eq!(
            notifications,
            json!([
                {"java":{"search":{"scope":"main"}}},
                {"java":{"search":{"scope":"all"}}},
                {"java":{"search":{"scope":"projectOnly"}}}
            ])
        );
        jdt.force_shutdown().await.unwrap();
        other.force_shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn test_stale_instance_or_missing_server_does_not_change_scope() {
        let (jdt, _rx) = test_support::client("jdtls", 10, "file:///Scope.java", true).await;
        let req = test_support::request("emacs/javaSearchScope", None);
        assert!(handle(
            req.clone(),
            params(Some(JavaSearchScopeValue::Main), Some(9)),
            vec![jdt.clone()]
        )
        .await
        .is_err());
        assert!(handle(req, params(None, None), vec![]).await.is_err());
        assert!(jdt.config().is_none());
        jdt.force_shutdown().await.unwrap();
    }

    #[tokio::test]
    async fn test_uninitialized_server_is_rejected() {
        let (jdt, _rx) = test_support::client("jdtls", 10, "file:///Scope.java", false).await;
        assert!(handle(
            test_support::request("emacs/javaSearchScope", None),
            params(Some(JavaSearchScopeValue::Main), None),
            vec![jdt.clone()]
        )
        .await
        .is_err());
        assert!(jdt.config().is_none());
        jdt.force_shutdown().await.unwrap();
    }
}
