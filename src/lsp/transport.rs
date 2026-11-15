use super::{jsonrpc, Error, Result};
use crate::{logging::LspLogger, msg::RequestId};
use anyhow::Context;
use log::error;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::{
    collections::HashMap,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};
use tokio::{
    io::{AsyncBufRead, AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader, BufWriter},
    process::{Child, ChildStderr, ChildStdin, ChildStdout},
    sync::{
        mpsc::{unbounded_channel, Sender, UnboundedReceiver, UnboundedSender},
        Mutex, Notify,
    },
};

#[derive(Debug)]
pub enum Payload {
    Request {
        chan: Sender<Result<Value>>,
        value: jsonrpc::MethodCall,
    },
    Notification(jsonrpc::Notification),
    Response(jsonrpc::Output),
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
#[serde(untagged)]
enum ServerMessage {
    Output(jsonrpc::Output),
    Call(jsonrpc::Call),
}

pub struct Transport {
    id: usize,
    name: String,
    logger: LspLogger,
    pending_requests: Mutex<HashMap<RequestId, Sender<core::result::Result<Value, Error>>>>,
    process: Arc<Mutex<Child>>,
    closed: AtomicBool,
    close_notify: Notify,
}

impl Transport {
    pub fn start(
        server_stdout: BufReader<ChildStdout>,
        server_stdin: BufWriter<ChildStdin>,
        server_stderr: BufReader<ChildStderr>,
        id: usize,
        name: String,
        process: Arc<Mutex<Child>>,
    ) -> (
        UnboundedReceiver<(usize, jsonrpc::Call)>,
        UnboundedSender<Payload>,
        Arc<Notify>,
        UnboundedSender<(usize, jsonrpc::Call)>,
    ) {
        let (client_tx, rx) = unbounded_channel();
        // 发送给 server ，接收要发生给 server 的消息
        let (tx, client_rx) = unbounded_channel();
        let notify = Arc::new(Notify::new());

        let logger = LspLogger::new(name.clone(), Some(id as u32));

        let transport = Self {
            id,
            name,
            logger,
            pending_requests: Mutex::new(HashMap::default()),
            process,
            closed: AtomicBool::new(false),
            close_notify: Notify::new(),
        };

        let transport = Arc::new(transport);

        tokio::spawn(Self::recv(
            transport.clone(),
            server_stdout,
            client_tx.clone(),
        ));

        tokio::spawn(Self::err(transport.clone(), server_stderr));
        tokio::spawn(Self::send(
            transport,
            server_stdin,
            client_tx.clone(),
            client_rx,
            notify.clone(),
        ));

        (rx, tx, notify, client_tx)
    }

    async fn recv(
        transport: Arc<Self>,
        mut server_stdout: BufReader<ChildStdout>,
        client_tx: UnboundedSender<(usize, jsonrpc::Call)>,
    ) {
        let mut recv_buffer = String::new();
        loop {
            match Self::recv_server_message(&mut server_stdout, &mut recv_buffer, &transport.logger)
                .await
            {
                Ok(Some(msg)) => {
                    match transport
                        .process_server_message(&client_tx, msg, &transport.name)
                        .await
                    {
                        Ok(_) => {}
                        Err(err) => {
                            error!("ok {} err: <- {err:?}", transport.name);
                            transport.close(&client_tx).await;
                            break;
                        }
                    };
                }
                Ok(None) => continue,
                Err(err) => {
                    if !matches!(err, Error::StreamClosed) {
                        error!(
                            "Exiting {} after unexpected error: {err:?}",
                            &transport.name
                        );
                    }
                    transport.close(&client_tx).await;
                    break;
                }
            }
        }
    }

    /// A replacement may use the same workspace cache. Only release the registry
    /// entry (via Exit) after its previous owner has actually exited and been reaped.
    async fn close(&self, client_tx: &UnboundedSender<(usize, jsonrpc::Call)>) {
        if self.closed.swap(true, Ordering::AcqRel) {
            return;
        }
        self.close_notify.notify_one();
        // Dropping the senders wakes all outstanding requests with StreamClosed.
        self.pending_requests.lock().await.clear();
        let mut process = self.process.lock().await;
        let stopped = match process.try_wait() {
            Ok(Some(_)) => Ok(()),
            Ok(None) => process.kill().await,
            Err(err) => Err(err),
        };
        if let Err(err) = stopped {
            error!(
                "Failed to reap {} ({}); retaining registry entry: {err}",
                self.name, self.id
            );
            return;
        }
        use lsp_types::notification::Notification as _;
        let _ = client_tx.send((
            self.id,
            jsonrpc::Call::Notification(jsonrpc::Notification {
                jsonrpc: None,
                method: lsp_types::notification::Exit::METHOD.to_string(),
                params: jsonrpc::Params::None,
            }),
        ));
    }

    async fn send(
        transport: Arc<Self>,
        mut server_stdin: BufWriter<ChildStdin>,
        client_tx: UnboundedSender<(usize, jsonrpc::Call)>,
        mut client_rx: UnboundedReceiver<Payload>,
        initialize_notify: Arc<Notify>,
    ) {
        let mut pending_messages: Vec<Payload> = Vec::new();
        let mut is_pending = true;

        // Determine if a message is allowed to be send early
        fn is_initialize(payload: &Payload) -> bool {
            use lsp_types::{
                notification::{Initialized, Notification},
                request::{Initialize, Request},
            };
            match payload {
                Payload::Request {
                    value: jsonrpc::MethodCall { method, .. },
                    ..
                } if method == Initialize::METHOD => true,
                Payload::Notification(jsonrpc::Notification { method, .. })
                    if method == Initialized::METHOD =>
                {
                    true
                }
                _ => false,
            }
        }

        fn is_shutdown(payload: &Payload) -> bool {
            use lsp_types::request::{Request, Shutdown};
            matches!(payload, Payload::Request { value: jsonrpc::MethodCall { method, .. }, .. } if method == Shutdown::METHOD)
        }

        let send = async {
            loop {
                tokio::select! {
                    biased;
                    _ = initialize_notify.notified() => {
                        let initialized = jsonrpc::Notification {
                            jsonrpc: Some(jsonrpc::Version::V2),
                            method: lsp_types::notification::Initialized::METHOD.to_string(),
                            params: jsonrpc::Params::None,
                        };
                        transport.send_payload_to_server(&mut server_stdin, Payload::Notification(initialized)).await?;
                        is_pending = false;

                        // Hack: inject an initialized notification so we trigger code that needs to happen after init
                        // callback: main_loop.rs handle_language_server_message
                        use lsp_types::notification::Notification;
                        let notification = ServerMessage::Call(jsonrpc::Call::Notification(jsonrpc::Notification {
                            jsonrpc: None,
                            method: lsp_types::notification::Initialized::METHOD.to_string(),
                            params: jsonrpc::Params::None,
                        }));
                        let language_server_name = &transport.name;
                        transport.process_server_message(&client_tx, notification, language_server_name).await?;

                        // drain the pending queue and send payloads to server
                        for msg in pending_messages.drain(..) {
                            transport.send_payload_to_server(&mut server_stdin, msg).await?;
                        }
                    }
                    msg = client_rx.recv() => {
                        if let Some(msg) = msg {
                            if is_pending && is_shutdown(&msg) {
                                break;
                            } else if is_pending && !is_initialize(&msg) {
                                // ignore notifications
                                // if let Payload::Notification(_) = msg {
                                //     continue;
                                // }
                                pending_messages.push(msg);
                            } else {
                                transport.send_payload_to_server(&mut server_stdin, msg).await?;
                            }
                        } else {
                            error!("recv none msg, cause channel closed");
                            // channel closed
                            break;
                        }
                    }
                }
            }
            Ok::<(), Error>(())
        };
        tokio::select! {
            biased;
            _ = transport.close_notify.notified() => {},
            result = send => {
                if let Err(err) = result {
                    error!("Exiting {} after send error: {err:?}", transport.name);
                }
                transport.close(&client_tx).await;
            }
        }
    }

    async fn err(transport: Arc<Self>, mut server_stderr: BufReader<ChildStderr>) {
        let mut recv_buffer = String::new();
        loop {
            match Self::recv_server_error(&mut server_stderr, &mut recv_buffer, &transport.logger)
                .await
            {
                Ok(_) => {}
                Err(err) => {
                    error!("err~ {} err: <- {err:?}", transport.name);
                    break;
                }
            }
        }
    }

    async fn recv_server_message(
        reader: &mut (impl AsyncBufRead + Unpin + Send),
        buffer: &mut String,
        logger: &LspLogger,
    ) -> core::result::Result<Option<ServerMessage>, Error> {
        let mut content_length = None;
        loop {
            buffer.truncate(0);
            if reader.read_line(buffer).await? == 0 {
                return Err(Error::StreamClosed);
            };

            if buffer == "\r\n" {
                break;
            }

            let header = buffer.trim();

            let parts = header.split_once(": ");
            match parts {
                Some(("Content-Length", value)) => {
                    content_length = Some(value.parse().context("invald content length")?);
                }
                Some((_, _)) => {}
                None => {
                    // Workaround: Some non-conformant language servers will output logging and other garbage
                    // into the same stream as JSON-RPC messages. This can also happen from shell scripts that spawn
                    // the server. Skip such lines and log a warning.

                    // warn!("Failed to parse header: {:?}", header);
                }
            }
        }

        let content_length = content_length.context("missing content length")?;

        let mut content = vec![0; content_length];
        reader.read_exact(&mut content).await?;
        logger.log_response(&String::from_utf8_lossy(&content));
        // The complete frame has been consumed, so invalid JSON/UTF-8 does not
        // lose framing synchronization and must not kill a healthy server.
        Ok(Self::parse_server_message(&content))
    }

    fn parse_server_message(content: &[u8]) -> Option<ServerMessage> {
        let mut value: Value = match serde_json::from_slice(content) {
            Ok(value) => value,
            Err(err) => {
                log::warn!("Discarding malformed language server message: {err}");
                return None;
            }
        };
        let object = value.as_object()?;
        let is_call = object.contains_key("method");
        let response_id = (!is_call)
            .then(|| {
                object
                    .get("id")
                    .cloned()
                    .and_then(|id| serde_json::from_value::<RequestId>(id).ok())
            })
            .flatten();
        let both_results = object.contains_key("result") && object.contains_key("error");
        if response_id.is_some() && object.keys().all(|key| key == "id" || key == "jsonrpc") {
            // JDTLS occasionally omits result for an empty navigation response.
            // Keep this narrow: an error must never become a successful null.
            log::warn!(
                "Language server response {response_id:?} omitted result; treating it as null"
            );
            value
                .as_object_mut()
                .unwrap()
                .insert("result".into(), Value::Null);
        }
        let decoded = if is_call {
            serde_json::from_value::<jsonrpc::Call>(value)
                .map(ServerMessage::Call)
                .map_err(|err| err.to_string())
        } else if both_results {
            Err("response contains both result and error".into())
        } else {
            serde_json::from_value::<jsonrpc::Output>(value)
                .map(ServerMessage::Output)
                .map_err(|err| err.to_string())
        };
        match decoded {
            Ok(ServerMessage::Call(jsonrpc::Call::Invalid { .. })) => {
                log::warn!("Discarding invalid language server call");
                None
            }
            Ok(message) => Some(message),
            Err(err) => {
                log::warn!("Malformed language server message: {err}");
                response_id.map(|id| {
                    ServerMessage::Output(jsonrpc::Output::Failure(jsonrpc::Failure {
                        jsonrpc: Some(jsonrpc::Version::V2),
                        id,
                        error: jsonrpc::Error {
                            code: jsonrpc::ErrorCode::InternalError,
                            message: format!("Malformed language server response: {err}"),
                            data: None,
                        },
                    }))
                })
            }
        }
    }

    async fn recv_server_error(
        err: &mut (impl AsyncBufRead + Unpin + Send),
        buffer: &mut String,
        logger: &LspLogger,
    ) -> Result<()> {
        buffer.truncate(0);
        if err.read_line(buffer).await? == 0 {
            return Err(Error::StreamClosed);
        };
        logger.log_stderr(buffer.trim());
        Ok(())
    }

    async fn process_server_message(
        &self,
        client_tx: &UnboundedSender<(usize, jsonrpc::Call)>,
        msg: ServerMessage,
        language_server_name: &str,
    ) -> Result<()> {
        match msg {
            ServerMessage::Output(output) => {
                self.process_request_response(output, language_server_name)
                    .await?
            }
            ServerMessage::Call(call) => {
                use lsp_types::notification::Notification as _;
                if matches!(&call, jsonrpc::Call::Notification(notification)
                    if notification.method == lsp_types::notification::Exit::METHOD)
                {
                    self.close(client_tx).await;
                    return Ok(());
                }
                client_tx
                    .send((self.id, call))
                    .context("failed to send a message to server")?;
            }
        };

        Ok(())
    }

    async fn process_request_response(
        &self,
        output: jsonrpc::Output,
        language_server_name: &str,
    ) -> Result<()> {
        let (id, result) = match output {
            jsonrpc::Output::Success(jsonrpc::Success { id, result, .. }) => (id, Ok(result)),
            jsonrpc::Output::Failure(jsonrpc::Failure { id, error, .. }) => {
                error!("{language_server_name} <- {error}");
                (id, Err(error.into()))
            }
        };

        if let Some(tx) = self.pending_requests.lock().await.remove(&id) {
            match tx.send(result).await {
                Ok(_) => (),
                Err(_) => error!(
                    "Tried sending response into a closed channel (id={id:?}), original request likely timed out",
                ),
            };
        } else {
            error!("Discarding Language Server response without a request (id={id:?})");
        }

        Ok(())
    }

    async fn send_payload_to_server(
        &self,
        server_stdin: &mut BufWriter<ChildStdin>,
        payload: Payload,
    ) -> Result<()> {
        let json = match payload {
            Payload::Request { chan, value } => {
                let mut requests = self.pending_requests.lock().await;
                if self.closed.load(Ordering::Acquire) {
                    return Err(Error::StreamClosed);
                }
                requests.insert(value.id.clone(), chan);
                serde_json::to_string(&value)?
            }
            Payload::Notification(value) => serde_json::to_string(&value)?,
            Payload::Response(error) => serde_json::to_string(&error)?,
        };
        self.send_string_to_server(server_stdin, json, &self.name)
            .await
    }

    async fn send_string_to_server(
        &self,
        server_stdin: &mut BufWriter<ChildStdin>,
        request: String,
        _language_server_name: &str,
    ) -> Result<()> {
        if self.closed.load(Ordering::Acquire) {
            return Err(Error::StreamClosed);
        }
        // Use structured logging for outgoing messages
        if let Some(method) = LspLogger::extract_method(&request) {
            self.logger.log_request(&method, &request);
        } else {
            self.logger
                .log_debug(&format!("Sending message: {}", &request));
        }

        // send the headers
        server_stdin
            .write_all(format!("Content-Length: {}\r\n\r\n", request.len()).as_bytes())
            .await?;

        // send the body
        server_stdin.write_all(request.as_bytes()).await?;

        server_stdin.flush().await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use std::time::Duration;

    async fn query(client: &crate::client::Client, method: &str) -> Result<Value> {
        tokio::time::timeout(
            Duration::from_secs(1),
            client.call_method(method.to_owned().into(), method.into(), Value::Null),
        )
        .await
        .expect("request must finish without waiting for its timeout")
    }

    #[tokio::test]
    async fn malformed_messages_and_missing_result_preserve_live_transport() {
        let (client, _rx) =
            crate::test_support::client("transport", 1, "file:///w/App.java", true).await;
        let pid = client.process.lock().await.id();
        assert_eq!(
            query(&client, "test/missingResult").await.unwrap(),
            Value::Null
        );
        assert_eq!(
            query(&client, "test/malformedMessages").await.unwrap(),
            json!("still alive")
        );
        for method in ["test/malformedResponse", "test/bothResults"] {
            match query(&client, method).await.unwrap_err() {
                Error::Rpc(err) => {
                    assert_eq!(err.code, jsonrpc::ErrorCode::InternalError);
                    assert!(err.message.contains("Malformed language server response"));
                }
                err => panic!("bad response must fail only its request: {err:?}"),
            }
        }
        match query(&client, "test/rpcError").await.unwrap_err() {
            Error::Rpc(err) => {
                assert_eq!(err.code, jsonrpc::ErrorCode::ServerError(-32042));
                assert_eq!(err.message, "real RPC failure");
                assert_eq!(err.data, Some(json!({"detail":42})));
            }
            err => panic!("RPC error lost: {err:?}"),
        }
        assert_eq!(
            query(&client, "test/missingResult").await.unwrap(),
            Value::Null
        );
        let mut process = client.process.lock().await;
        assert_eq!(process.id(), pid);
        assert!(process.try_wait().unwrap().is_none());
        drop(process);
        client.stop_and_wait().await.unwrap();
    }

    #[test]
    fn missing_result_compatibility_is_limited_to_valid_minimal_response() {
        for value in [
            json!({"id":3,"jsonrpc":"1.0"}),
            json!({"id":3,"unexpected":true}),
            json!({"id":3,"error":null}),
        ] {
            assert!(matches!(
                Transport::parse_server_message(&serde_json::to_vec(&value).unwrap()),
                Some(ServerMessage::Output(jsonrpc::Output::Failure(_)))
            ));
        }
        for value in [json!({"id":null}), json!({"method":false}), json!([])] {
            assert!(
                Transport::parse_server_message(&serde_json::to_vec(&value).unwrap()).is_none()
            );
        }
        for id in [json!(348), json!("navigation")] {
            let response = Transport::parse_server_message(
                &serde_json::to_vec(&json!({"id":id,"jsonrpc":"2.0"})).unwrap(),
            );
            assert!(matches!(
                response,
                Some(ServerMessage::Output(jsonrpc::Output::Success(
                    jsonrpc::Success {
                        result: Value::Null,
                        ..
                    }
                )))
            ));
        }
    }

    async fn exit_event(
        rx: &mut UnboundedReceiver<(usize, jsonrpc::Call)>,
    ) -> (usize, jsonrpc::Call) {
        tokio::time::timeout(Duration::from_secs(2), async {
            loop {
                let message = rx.recv().await.expect("exit notification");
                if matches!(&message.1, jsonrpc::Call::Notification(n) if n.method == "exit") {
                    break message;
                }
            }
        })
        .await
        .expect("failed transport must terminate promptly")
    }

    #[tokio::test]
    async fn fatal_stream_failure_reaps_child_before_one_exit_notification() {
        for method in [
            "test/closeOutput",
            "test/truncatedFrame",
            "test/closeInput",
            "test/exitNotification",
        ] {
            let (client, mut rx) =
                crate::test_support::client("transport", 7, "file:///w/App.java", true).await;
            if method == "test/closeInput" {
                assert_eq!(query(&client, method).await.unwrap(), json!("input closed"));
                assert!(query(&client, "test/missingResult").await.is_err());
            } else {
                assert!(matches!(
                    query(&client, method).await,
                    Err(Error::StreamClosed)
                ));
            }
            assert_eq!(exit_event(&mut rx).await.0, 7);
            // The peer deliberately stays alive for 60s after disconnecting.
            // Exit must be emitted only after kill + wait, even while Client is retained.
            assert!(client.process.lock().await.try_wait().unwrap().is_some());
            assert!(query(&client, "test/missingResult").await.is_err());
            assert!(tokio::time::timeout(Duration::from_secs(1), rx.recv())
                .await
                .unwrap()
                .is_none());
            // Existing graceful cleanup remains safe after transport cleanup.
            client.stop_and_wait().await.unwrap();
        }
    }

    #[tokio::test]
    async fn closing_transport_wakes_other_outstanding_requests() {
        let (client, mut rx) =
            crate::test_support::client("transport", 8, "file:///w/App.java", true).await;
        let held = tokio::spawn({
            let client = client.clone();
            async move { query(&client, "test/hold").await }
        });
        tokio::time::timeout(Duration::from_secs(1), async {
            while query(&client, "test/heldRequests").await.unwrap() != json!(1) {
                tokio::task::yield_now().await;
            }
        })
        .await
        .unwrap();
        assert!(matches!(
            query(&client, "test/closeOutput").await,
            Err(Error::StreamClosed)
        ));
        assert!(matches!(held.await.unwrap(), Err(Error::StreamClosed)));
        exit_event(&mut rx).await;
        assert!(client.process.lock().await.try_wait().unwrap().is_some());
    }

    #[tokio::test]
    async fn initialization_write_failure_reaps_process_and_drops_buffered_requests() {
        use std::process::Stdio;
        // Close input before signaling ready; keep stdout open to ensure this
        // exercises the write-failure path rather than EOF from the reader.
        let peer = "import os,sys,time; os.close(0); body=b'{\"jsonrpc\":\"2.0\",\"method\":\"test/ready\"}'; sys.stdout.buffer.write(('Content-Length: %d\\r\\n\\r\\n'%len(body)).encode()+body); sys.stdout.buffer.flush(); time.sleep(60)";
        let mut process = tokio::process::Command::new("python3")
            .args(["-u", "-c", peer])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .kill_on_drop(true)
            .spawn()
            .unwrap();
        let reader = BufReader::new(process.stdout.take().unwrap());
        let writer = BufWriter::new(process.stdin.take().unwrap());
        let stderr = BufReader::new(process.stderr.take().unwrap());
        let process = Arc::new(Mutex::new(process));
        let (mut rx, tx, notify, _) = Transport::start(
            reader,
            writer,
            stderr,
            9,
            "initialization-failure".into(),
            process.clone(),
        );
        let (_, ready) = tokio::time::timeout(Duration::from_secs(2), rx.recv())
            .await
            .unwrap()
            .unwrap();
        assert!(matches!(ready, jsonrpc::Call::Notification(n) if n.method == "test/ready"));
        let (chan, mut result) = tokio::sync::mpsc::channel(1);
        tx.send(Payload::Request {
            chan,
            value: jsonrpc::MethodCall {
                jsonrpc: Some(jsonrpc::Version::V2),
                id: 1.into(),
                method: "test/buffered".into(),
                params: jsonrpc::Params::None,
            },
        })
        .unwrap();
        notify.notify_one();
        exit_event(&mut rx).await;
        assert!(process.lock().await.try_wait().unwrap().is_some());
        assert!(tokio::time::timeout(Duration::from_secs(1), result.recv())
            .await
            .unwrap()
            .is_none());
        assert!(tx.is_closed());
    }
}
