//! STS classpath callbacks are scoped to a specific JDT/STS pair.
use crate::{client::Client, lsp::jsonrpc, msg::RequestId};
use lsp_types::ExecuteCommandParams;
use parking_lot::Mutex;
use serde::Deserialize;
use serde_json::Value;
use std::{
    collections::HashMap,
    sync::{Arc, Weak},
    time::Duration,
};

#[derive(Debug, Clone, PartialEq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct ClasspathListenerParams {
    pub callback_command_id: String,
    #[serde(default)]
    pub batched: bool,
}

#[derive(Clone)]
struct Pair {
    spring_id: usize,
    spring: Weak<Client>,
    jdt: Weak<Client>,
}

#[derive(Clone, Default)]
pub(crate) struct Bridge {
    callbacks: Arc<Mutex<HashMap<(usize, String), Pair>>>,
    listeners: Arc<Mutex<HashMap<(usize, String), (Weak<Client>, bool)>>>,
}

fn rpc_error(message: impl Into<String>) -> jsonrpc::Error {
    jsonrpc::Error {
        code: jsonrpc::ErrorCode::InternalError,
        message: message.into(),
        data: None,
    }
}

fn forward_error(err: crate::lsp::Error) -> jsonrpc::Error {
    match err {
        crate::lsp::Error::Rpc(err) => err,
        crate::lsp::Error::Other(ref other) => {
            for cause in other.chain() {
                if let Some(crate::lsp::Error::Rpc(rpc)) = cause.downcast_ref::<crate::lsp::Error>()
                {
                    return rpc.clone();
                }
            }
            rpc_error(err.to_string())
        }
        _ => rpc_error(err.to_string()),
    }
}

fn reply(spring: &Arc<Client>, id: Option<RequestId>, result: Result<Value, jsonrpc::Error>) {
    if let Some(id) = id {
        tokio::spawn(spring.reply(id, result));
    } else if let Err(err) = result {
        log::warn!("Spring classpath re-registration: {}", err.message);
    }
}

async fn wait_initialized(client: &Client) -> Result<(), jsonrpc::Error> {
    tokio::time::timeout(Duration::from_secs(120), async {
        while !client.is_initialized() {
            tokio::time::sleep(Duration::from_millis(50)).await;
        }
    })
    .await
    .map_err(|_| rpc_error("JDT LS did not initialize; check its startup log"))
}

fn command(name: &str, callback: &str) -> ExecuteCommandParams {
    ExecuteCommandParams {
        command: name.into(),
        arguments: vec![Value::String(callback.into())],
        work_done_progress_params: Default::default(),
    }
}

impl Bridge {
    pub(crate) fn classpath_request(
        &self,
        spring: Arc<Client>,
        candidates: Vec<Arc<Client>>,
        id: RequestId,
        params: ClasspathListenerParams,
        remove: bool,
    ) {
        self.register(spring, candidates, Some(id), params, remove);
    }

    fn register(
        &self,
        spring: Arc<Client>,
        candidates: Vec<Arc<Client>>,
        id: Option<RequestId>,
        params: ClasspathListenerParams,
        remove: bool,
    ) {
        let jdts: Vec<_> = candidates
            .into_iter()
            .filter(|ls| ls.name() == "jdtls")
            .collect();
        if jdts.len() != 1 || params.callback_command_id.is_empty() {
            reply(&spring, id, Err(rpc_error(
                "STS requires exactly one JDT LS in the same workspace and a nonempty callbackCommandId")));
            return;
        }
        let jdt = jdts.into_iter().next().unwrap();
        let key = (jdt.id(), params.callback_command_id.clone());
        {
            let mut callbacks = self.callbacks.lock();
            if let Some(pair) = callbacks.get(&key) {
                if pair.spring_id != spring.id() && pair.spring.upgrade().is_some() {
                    reply(
                        &spring,
                        id,
                        Err(rpc_error(
                            "Classpath callback is already owned by another STS instance",
                        )),
                    );
                    return;
                }
            }
            if !remove {
                // JDT may call back before it replies to registration.
                callbacks.insert(
                    key.clone(),
                    Pair {
                        spring_id: spring.id(),
                        spring: Arc::downgrade(&spring),
                        jdt: Arc::downgrade(&jdt),
                    },
                );
            }
        }
        let listener = (spring.id(), params.callback_command_id.clone());
        if remove {
            self.listeners.lock().remove(&listener);
        } else {
            self.listeners
                .lock()
                .insert(listener, (Arc::downgrade(&spring), params.batched));
        }
        let bridge = self.clone();
        tokio::spawn(async move {
            let action = if remove {
                "sts.java.removeClasspathListener"
            } else {
                "sts.java.addClasspathListener"
            };
            let result = match wait_initialized(&jdt).await {
                Ok(()) => {
                    if !remove
                        && !bridge
                            .callbacks
                            .lock()
                            .get(&key)
                            .is_some_and(|pair| pair.spring_id == spring.id())
                    {
                        reply(
                            &spring,
                            id,
                            Err(rpc_error("Classpath registration cancelled: server exited")),
                        );
                        return;
                    }
                    let mut command = command(action, &params.callback_command_id);
                    if !remove {
                        command.arguments.push(Value::Bool(params.batched));
                    }
                    jdt.request::<lsp_types::request::ExecuteCommand>(command)
                        .await
                        .map(|value| value.unwrap_or(Value::Null))
                        .map_err(forward_error)
                }
                Err(err) => Err(err),
            };
            if remove || result.is_err() {
                let mut callbacks = bridge.callbacks.lock();
                if callbacks
                    .get(&key)
                    .is_some_and(|pair| pair.spring_id == spring.id())
                {
                    callbacks.remove(&key);
                }
            }
            // A server may exit while the executeCommand response is in flight.
            // Undo a late registration after the lifecycle cleanup ran.
            let active = bridge
                .callbacks
                .lock()
                .get(&key)
                .is_some_and(|pair| pair.spring_id == spring.id());
            if !remove && result.is_ok() && !active {
                let _ = jdt
                    .request::<lsp_types::request::ExecuteCommand>(command(
                        "sts.java.removeClasspathListener",
                        &params.callback_command_id,
                    ))
                    .await;
            }
            if let Err(err) = &result {
                log::warn!("Spring classpath registration: {}", err.message);
            } else {
                log::info!(
                    "Spring classpath {action}: STS {} / JDT {}",
                    spring.id(),
                    jdt.id()
                );
            }
            reply(&spring, id, result);
        });
    }

    pub(crate) fn callback(&self, jdt: Arc<Client>, id: RequestId, params: ExecuteCommandParams) {
        if jdt.name() == "jdtls"
            && matches!(
                params.command.as_str(),
                "vscode-spring-boot.ls.start" | "vscode-spring-boot.ls.stop"
            )
        {
            // The JDT extension sends these as Boot projects appear/disappear.
            // The proxy owns STS lifecycle through its server configuration.
            // Acknowledge immediately: waiting for STS (and thus ServiceReady)
            // here would block the JDT initialization job that sent the request.
            tokio::spawn(jdt.reply(id, Ok(Value::Null)));
            return;
        }
        let key = (jdt.id(), params.command.clone());
        let spring = self
            .callbacks
            .lock()
            .get(&key)
            .and_then(|pair| pair.spring.upgrade());
        let Some(spring) = spring else {
            tokio::spawn(jdt.reply(
                id,
                Err(jsonrpc::Error {
                    code: jsonrpc::ErrorCode::MethodNotFound,
                    message: format!("No STS callback registered for {}", params.command),
                    data: None,
                }),
            ));
            return;
        };
        tokio::spawn(async move {
            // This callback may be part of STS initialize, so do not wait for the
            // STS capabilities cell here (that would deadlock initialization).
            let result = spring
                .request::<lsp_types::request::ExecuteCommand>(params)
                .await
                .map(|value| value.unwrap_or(Value::Null))
                .map_err(forward_error);
            if let Err(err) = jdt.reply(id, result).await {
                log::warn!("JDT classpath callback reply: {err}");
            }
        });
    }

    pub(crate) fn remove_client(&self, id: usize) {
        self.listeners
            .lock()
            .retain(|(spring, _), (client, _)| *spring != id && client.upgrade().is_some());
        let removed: Vec<_> = {
            let mut callbacks = self.callbacks.lock();
            let keys: Vec<_> = callbacks
                .iter()
                .filter(|((jdt, _), pair)| {
                    *jdt == id || pair.spring_id == id || pair.spring.upgrade().is_none()
                })
                .map(|(key, _)| key.clone())
                .collect();
            keys.into_iter()
                .filter_map(|key| callbacks.remove(&key).map(|pair| (key, pair)))
                .collect()
        };
        for ((jdt_id, callback), pair) in removed {
            if jdt_id != id {
                if let Some(jdt) = pair.jdt.upgrade() {
                    tokio::spawn(async move {
                        let _ = jdt
                            .request::<lsp_types::request::ExecuteCommand>(command(
                                "sts.java.removeClasspathListener",
                                &callback,
                            ))
                            .await;
                    });
                }
            }
        }
    }

    pub(crate) fn initialized(&self, client: Arc<Client>, project_clients: Vec<Arc<Client>>) {
        if client.name() != "jdtls" {
            return;
        }
        let listeners: Vec<_> = self
            .listeners
            .lock()
            .iter()
            .filter_map(|((_, callback), (weak, batched))| {
                weak.upgrade()
                    .map(|spring| (callback.clone(), spring, *batched))
            })
            .collect();
        for (callback, spring, batched) in listeners {
            if project_clients
                .iter()
                .any(|candidate| candidate.id() == spring.id())
                && !self
                    .callbacks
                    .lock()
                    .contains_key(&(client.id(), callback.clone()))
            {
                self.register(
                    spring,
                    vec![client.clone()],
                    None,
                    ClasspathListenerParams {
                        callback_command_id: callback,
                        batched,
                    },
                    false,
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    async fn next_callback(
        rx: &mut tokio::sync::mpsc::UnboundedReceiver<(usize, jsonrpc::Call)>,
    ) -> (RequestId, ExecuteCommandParams) {
        loop {
            let (_, call) = tokio::time::timeout(Duration::from_secs(3), rx.recv())
                .await
                .unwrap()
                .unwrap();
            if let jsonrpc::Call::MethodCall(call) = call {
                return (call.id, call.params.parse().unwrap());
            }
        }
    }

    async fn responses(client: &Client) -> Value {
        for _ in 0..50 {
            let response = client
                .call_method(
                    "replies".to_string().into(),
                    "test/replies".into(),
                    Value::Null,
                )
                .await
                .unwrap();
            if !response.as_array().unwrap().is_empty() {
                return response;
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
        panic!("bridge did not reply");
    }

    #[tokio::test]
    async fn test_transport_registration_callback_isolation_cleanup_and_restart() {
        use crate::test_support::client;
        let bridge = Bridge::default();
        let (jdt, mut rx) = client("jdtls", 1, "file:///one", false).await;
        let (spring, _) = client("spring-one", 2, "file:///one", true).await;
        let params = ClasspathListenerParams {
            callback_command_id: "same-callback".into(),
            batched: true,
        };
        bridge.classpath_request(
            spring.clone(),
            vec![jdt.clone()],
            11.into(),
            params.clone(),
            false,
        );
        // Registration arrives while JDT is still initializing.
        tokio::time::sleep(Duration::from_millis(60)).await;
        crate::test_support::initialize(&jdt);
        let (id, cmd) = next_callback(&mut rx).await;
        let arguments = cmd.arguments.clone();
        bridge.callback(jdt.clone(), id, cmd);
        let reply = responses(&spring).await;
        assert_eq!(reply[0]["result"]["server"], "spring-one");
        assert_eq!(
            reply[0]["result"]["arguments"],
            serde_json::json!(arguments)
        );
        let (other_jdt, mut other_rx) = client("jdtls", 3, "file:///two", true).await;
        let (other_spring, _) = client("spring-two", 4, "file:///two", true).await;
        bridge.classpath_request(
            other_spring.clone(),
            vec![other_jdt.clone()],
            12.into(),
            params.clone(),
            false,
        );
        let (id, cmd) = next_callback(&mut other_rx).await;
        bridge.callback(other_jdt, id, cmd);
        assert_eq!(
            responses(&other_spring).await[0]["result"]["server"],
            "spring-two"
        );
        bridge.remove_client(1);
        assert!(!bridge
            .callbacks
            .lock()
            .contains_key(&(1, "same-callback".into())));
        let (new_jdt, mut new_rx) = client("jdtls", 5, "file:///one", true).await;
        bridge.initialized(new_jdt.clone(), vec![spring.clone(), new_jdt.clone()]);
        let (id, cmd) = next_callback(&mut new_rx).await;
        bridge.callback(new_jdt.clone(), id, cmd);
        bridge.classpath_request(spring, vec![new_jdt], 13.into(), params, true);
        tokio::time::sleep(Duration::from_millis(100)).await;
        assert!(!bridge
            .callbacks
            .lock()
            .contains_key(&(5, "same-callback".into())));
        bridge.remove_client(4);
        assert!(bridge.callbacks.lock().is_empty());
        assert!(bridge.listeners.lock().is_empty());
    }

    #[tokio::test]
    async fn test_transport_lifecycle_commands_before_initialization_without_sts() {
        let bridge = Bridge::default();
        let (jdt, _) = crate::test_support::client("jdtls", 1, "file:///one", false).await;
        for (id, name) in [
            (11, "vscode-spring-boot.ls.start"),
            (12, "vscode-spring-boot.ls.stop"),
        ] {
            bridge.callback(jdt.clone(), id.into(), command(name, "ignored"));
        }
        let mut replies = Value::Null;
        for _ in 0..50 {
            replies = responses(&jdt).await;
            if replies.as_array().unwrap().len() == 2 {
                break;
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
        let replies = replies.as_array().unwrap();
        assert_eq!(replies.len(), 2);
        for reply in replies {
            assert!(reply.get("error").is_none(), "{reply}");
            assert_eq!(reply.get("result"), Some(&Value::Null));
        }
        assert!(!jdt.is_initialized());
        assert!(bridge.callbacks.lock().is_empty());
        assert!(bridge.listeners.lock().is_empty());
    }

    #[tokio::test]
    async fn test_transport_unknown_callback_still_returns_method_not_found() {
        let bridge = Bridge::default();
        let (jdt, _) = crate::test_support::client("jdtls", 1, "file:///one", true).await;
        bridge.callback(
            jdt.clone(),
            13.into(),
            command("unknown-callback", "ignored"),
        );
        let reply = responses(&jdt).await;
        assert_eq!(reply[0]["error"]["code"], -32601);
        assert_eq!(
            reply[0]["error"]["message"],
            "No STS callback registered for unknown-callback"
        );
    }

    #[tokio::test]
    async fn test_transport_callback_error_preserves_code_message_and_data() {
        let (spring, _) = crate::test_support::client("spring", 1, "file:///one", true).await;
        let err = spring
            .request::<lsp_types::request::ExecuteCommand>(command("error", "ignored"))
            .await
            .unwrap_err();
        let err = forward_error(err);
        assert_eq!(
            serde_json::to_value(err).unwrap(),
            serde_json::json!({
                "code":-32042,"message":"callback failed","data":{"owner":"spring"}
            })
        );
    }
    #[test]
    fn test_classpath_arguments_are_an_array_and_preserve_callback_id() {
        let p: ClasspathListenerParams =
            serde_json::from_value(serde_json::json!({"callbackCommandId":"sts.callback.42"}))
                .unwrap();
        let cmd = command("sts.java.addClasspathListener", &p.callback_command_id);
        assert_eq!(cmd.arguments, vec![Value::String("sts.callback.42".into())]);
        assert!(serde_json::from_value::<ClasspathListenerParams>(
            serde_json::json!({"callbackCommandId":false})
        )
        .is_err());
    }
}
