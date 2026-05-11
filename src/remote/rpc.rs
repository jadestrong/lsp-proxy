use anyhow::{anyhow, Result};
use futures::channel::mpsc::{UnboundedReceiver, UnboundedSender};
use futures::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt, StreamExt};
use log::{debug, error, info, warn};
use prost::Message as ProstMessage;
use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::Arc;
use tokio::sync::mpsc::UnboundedSender as TokioUnboundedSender;
use tokio::sync::oneshot;
use tokio::time::{timeout, Duration};

use crate::lsp::jsonrpc;
use crate::msg::{Message, Notification, Params, Request, RequestId, Response};

/// Method name for application-level heartbeat pings. The remote side
/// short-circuits this in its Controller and replies with an empty result
/// without routing the request through Application.
pub const PING_METHOD: &str = "$/lspProxy/ping";

/// How often the heartbeat task emits a ping.
const HEARTBEAT_INTERVAL: Duration = Duration::from_secs(5);
/// How long a single ping is allowed to take before it counts as missed.
const PING_TIMEOUT: Duration = Duration::from_secs(5);
/// Consecutive misses that mark the client dead. 5 × 5s ≈ 25s detection.
const MAX_MISSED_HEARTBEATS: u32 = 5;

pub mod proto {
    include!(concat!(env!("OUT_DIR"), "/lsp_proxy.rpc.rs"));
}

use proto::{envelope, Envelope, RpcError};

/// Wire format length type. Each Envelope is framed by a 4-byte LE length prefix.
pub type MessageLen = u32;
pub const MESSAGE_LEN_SIZE: usize = std::mem::size_of::<MessageLen>();
/// 10 MiB hard cap on a single envelope to avoid runaway allocations.
const MAX_MESSAGE_LEN: u32 = 10 * 1024 * 1024;

/// Raw-envelope reader for the server side.
pub async fn read_envelope<S: AsyncRead + Unpin>(
    stream: &mut S,
    buffer: &mut Vec<u8>,
) -> Result<Envelope> {
    buffer.resize(MESSAGE_LEN_SIZE, 0);
    stream.read_exact(buffer).await?;
    let message_len = MessageLen::from_le_bytes(buffer.as_slice().try_into().unwrap());
    if message_len == 0 || message_len > MAX_MESSAGE_LEN {
        return Err(anyhow!("Invalid envelope length: {message_len}"));
    }
    buffer.resize(message_len as usize, 0);
    stream.read_exact(buffer).await?;
    Envelope::decode(buffer.as_slice()).map_err(Into::into)
}

/// Raw-envelope writer for the server side.
pub async fn write_envelope<S: AsyncWrite + Unpin>(
    stream: &mut S,
    buffer: &mut Vec<u8>,
    envelope: Envelope,
) -> Result<()> {
    let message_len = envelope.encoded_len() as u32;
    stream.write_all(&message_len.to_le_bytes()).await?;
    buffer.clear();
    buffer.reserve(message_len as usize);
    envelope.encode(buffer)?;
    stream.write_all(buffer).await?;
    stream.flush().await?;
    Ok(())
}

/// Outgoing message destined for the wire.
#[derive(Debug)]
enum OutgoingCommand {
    Request {
        request: Request,
        response_tx: oneshot::Sender<Result<Response>>,
    },
    Notification(Notification),
}

/// RPC client that speaks Protobuf-framed Envelopes over a duplex byte stream.
pub struct RpcClient {
    sender: UnboundedSender<OutgoingCommand>,
    _next_id: Arc<AtomicU32>,
    /// Set to true by the heartbeat task when it decides the far end is
    /// unresponsive (N consecutive ping misses). Checked by `is_dead()`.
    dead: Arc<AtomicBool>,
}

impl RpcClient {
    /// Create an RpcClient over `stream`. When `unsolicited` is `Some(_)`,
    /// server-originated Notifications (and Requests) that arrive on the
    /// stream are decoded and pushed to that channel, so the caller can
    /// forward them to Emacs (e.g. for `textDocument/publishDiagnostics`).
    /// When `None`, they are silently dropped — useful for tests and for
    /// code paths that only care about request/response round-trips.
    pub fn new<S>(
        stream: S,
        unsolicited: Option<TokioUnboundedSender<Message>>,
    ) -> Result<Self>
    where
        S: AsyncRead + AsyncWrite + Send + Unpin + 'static,
    {
        let (sender, receiver) = futures::channel::mpsc::unbounded();
        tokio::spawn(Self::run_communication_loop(stream, receiver, unsolicited));
        let dead = Arc::new(AtomicBool::new(false));
        tokio::spawn(Self::heartbeat_loop(sender.clone(), dead.clone()));
        Ok(Self {
            sender,
            _next_id: Arc::new(AtomicU32::new(1)),
            dead,
        })
    }

    /// Application-level heartbeat: send a tiny ping every few seconds and
    /// count consecutive misses. When the miss count hits the threshold, flip
    /// the `dead` flag so the next `get_or_create_rpc_client` call evicts
    /// this zombie and rebuilds the tunnel. This catches "tunnel is alive,
    /// but remote stopped responding" cases that pure stream-EOF detection
    /// misses.
    async fn heartbeat_loop(
        sender: UnboundedSender<OutgoingCommand>,
        dead: Arc<AtomicBool>,
    ) {
        let mut consecutive_misses: u32 = 0;
        loop {
            tokio::time::sleep(HEARTBEAT_INTERVAL).await;

            if dead.load(Ordering::Relaxed) || sender.is_closed() {
                debug!("heartbeat: client already dead, stopping");
                return;
            }

            let (resp_tx, resp_rx) = oneshot::channel();
            let ping_req = Request {
                id: RequestId::from(-1),
                method: PING_METHOD.to_string(),
                params: Params {
                    uri: None,
                    context: None,
                    virtual_doc: None,
                    params: serde_json::Value::Null,
                },
            };
            if sender
                .unbounded_send(OutgoingCommand::Request {
                    request: ping_req,
                    response_tx: resp_tx,
                })
                .is_err()
            {
                debug!("heartbeat: outgoing channel closed");
                dead.store(true, Ordering::Relaxed);
                return;
            }

            let res = timeout(PING_TIMEOUT, resp_rx).await;
            match res {
                Ok(Ok(Ok(_))) => {
                    if consecutive_misses > 0 {
                        debug!("heartbeat: recovered after {consecutive_misses} misses");
                    }
                    consecutive_misses = 0;
                }
                _ => {
                    consecutive_misses += 1;
                    warn!(
                        "heartbeat: ping miss {consecutive_misses}/{MAX_MISSED_HEARTBEATS}"
                    );
                    if consecutive_misses >= MAX_MISSED_HEARTBEATS {
                        warn!(
                            "heartbeat: {MAX_MISSED_HEARTBEATS} consecutive misses, marking RPC client dead"
                        );
                        dead.store(true, Ordering::Relaxed);
                        return;
                    }
                }
            }
        }
    }

    /// Returns true if either:
    /// - the background communication loop has exited (`sender.is_closed()`
    ///   — underlying transport died), or
    /// - the heartbeat task declared the far end unresponsive.
    ///   Cached clients that report `is_dead()` must be evicted and recreated
    ///   before the next request — otherwise every subsequent `send_request`
    ///   either fails instantly ("channel closed") or hangs until its 30s
    ///   timeout, repeatedly.
    pub fn is_dead(&self) -> bool {
        self.sender.is_closed() || self.dead.load(Ordering::Relaxed)
    }

    pub async fn send_request(&self, request: Request) -> Result<Response> {
        let (response_tx, response_rx) = oneshot::channel();
        let method = request.method.clone();
        let orig_id = request.id.clone();
        self.sender
            .unbounded_send(OutgoingCommand::Request {
                request,
                response_tx,
            })
            .map_err(|_| anyhow!("RPC client channel closed"))?;

        match timeout(Duration::from_secs(30), response_rx).await {
            Ok(inner) => {
                let response = inner.map_err(|_| anyhow!("Response channel closed"))??;
                Ok(response)
            }
            Err(_) => {
                warn!(
                    "RPC request timeout after 30s: method={method} orig_id={orig_id:?}"
                );
                Err(anyhow!("RPC request timeout"))
            }
        }
    }

    pub fn send_notification(&self, notification: Notification) -> Result<()> {
        self.sender
            .unbounded_send(OutgoingCommand::Notification(notification))
            .map_err(|_| anyhow!("RPC client channel closed"))?;
        Ok(())
    }

    async fn run_communication_loop<S>(
        mut stream: S,
        mut receiver: UnboundedReceiver<OutgoingCommand>,
        unsolicited: Option<TokioUnboundedSender<Message>>,
    ) where
        S: AsyncRead + AsyncWrite + Send + Unpin + 'static,
    {
        // envelope_id -> (original LSP request id, response sender)
        let mut pending: HashMap<u32, (RequestId, oneshot::Sender<Result<Response>>)> =
            HashMap::new();
        let mut next_envelope_id: u32 = 1;
        let mut buffer = Vec::new();

        loop {
            tokio::select! {
                outgoing = receiver.next() => {
                    match outgoing {
                        Some(OutgoingCommand::Request { request, response_tx }) => {
                            let envelope_id = next_envelope_id;
                            next_envelope_id = next_envelope_id.wrapping_add(1);
                            let original_id = request.id.clone();

                            let params_bytes = match serde_json::to_vec(&request.params) {
                                Ok(b) => b,
                                Err(e) => {
                                    let _ = response_tx.send(Err(anyhow!("encode params: {e}")));
                                    continue;
                                }
                            };
                            let method_for_log = request.method.clone();
                            debug!(
                                "RPC send request env_id={envelope_id} method={method_for_log} orig_id={original_id:?}"
                            );
                            let envelope = Envelope {
                                id: Some(envelope_id),
                                responding_to: None,
                                payload: Some(envelope::Payload::Request(proto::Request {
                                    method: request.method,
                                    params: params_bytes,
                                })),
                            };
                            if let Err(e) = write_envelope(&mut stream, &mut buffer, envelope).await {
                                error!("Failed to write request envelope: {e}");
                                let _ = response_tx.send(Err(e));
                                break;
                            }
                            pending.insert(envelope_id, (original_id, response_tx));
                        }
                        Some(OutgoingCommand::Notification(notif)) => {
                            let params_bytes = match serde_json::to_vec(&notif.params) {
                                Ok(b) => b,
                                Err(e) => {
                                    warn!("Failed to encode notification params: {e}");
                                    continue;
                                }
                            };
                            let envelope = Envelope {
                                id: None,
                                responding_to: None,
                                payload: Some(envelope::Payload::Notification(proto::Notification {
                                    method: notif.method,
                                    params: params_bytes,
                                })),
                            };
                            if let Err(e) = write_envelope(&mut stream, &mut buffer, envelope).await {
                                error!("Failed to write notification envelope: {e}");
                                break;
                            }
                        }
                        None => {
                            debug!("RPC outgoing channel closed");
                            break;
                        }
                    }
                }
                incoming = read_envelope(&mut stream, &mut buffer) => {
                    match incoming {
                        Ok(envelope) => match envelope.payload {
                            Some(envelope::Payload::Response(resp)) => {
                                if let Some(env_id) = envelope.responding_to {
                                    if let Some((orig_id, tx)) = pending.remove(&env_id) {
                                        debug!(
                                            "RPC recv response env_id={} orig_id={:?} result_bytes={} error={:?}",
                                            env_id,
                                            orig_id,
                                            resp.result.as_ref().map(|b| b.len()).unwrap_or(0),
                                            resp.error.as_ref().map(|e| &e.message)
                                        );
                                        if let Some(bytes) = resp.result.as_ref() {
                                            let preview: String = String::from_utf8_lossy(
                                                &bytes[..bytes.len().min(512)],
                                            )
                                            .into_owned();
                                            debug!("RPC recv result preview: {preview}");
                                        }
                                        let response = decode_response(orig_id, resp);
                                        let _ = tx.send(response);
                                    } else {
                                        debug!("Unmatched response envelope id={env_id}");
                                    }
                                } else {
                                    debug!("Response envelope missing responding_to");
                                }
                            }
                            Some(envelope::Payload::Notification(notif)) => {
                                match decode_notification(notif) {
                                    Ok(msg) => forward_unsolicited(&unsolicited, msg),
                                    Err(e) => warn!("bad server notification: {e}"),
                                }
                            }
                            Some(envelope::Payload::Request(req)) => {
                                match decode_server_request(envelope.id, req) {
                                    Ok(msg) => forward_unsolicited(&unsolicited, msg),
                                    Err(e) => warn!("bad server request: {e}"),
                                }
                            }
                            None => {
                                debug!("envelope with no payload");
                            }
                        },
                        Err(e) => {
                            error!("RPC read error: {e}");
                            break;
                        }
                    }
                }
            }
        }

        for (_, (_, tx)) in pending {
            let _ = tx.send(Err(anyhow!("RPC connection closed")));
        }
    }

    pub fn _next_request_id(&self) -> u32 {
        self._next_id.fetch_add(1, Ordering::Relaxed)
    }
}

fn decode_notification(notif: proto::Notification) -> Result<Message> {
    let params: crate::msg::Params = serde_json::from_slice(&notif.params)?;
    Ok(Message::Notification(Notification {
        method: notif.method,
        params,
    }))
}

fn decode_server_request(envelope_id: Option<u32>, req: proto::Request) -> Result<Message> {
    let params: crate::msg::Params = serde_json::from_slice(&req.params)?;
    // Server-initiated requests still need an id so the client can respond.
    let id = envelope_id
        .map(|n| RequestId::from(n as i32))
        .ok_or_else(|| anyhow!("server request envelope missing id"))?;
    Ok(Message::Request(Request {
        id,
        method: req.method,
        params,
    }))
}

fn forward_unsolicited(sink: &Option<TokioUnboundedSender<Message>>, msg: Message) {
    match sink {
        Some(tx) => {
            if let Err(e) = tx.send(msg) {
                debug!("unsolicited channel closed: {e}");
            }
        }
        None => {
            debug!("no unsolicited sink configured, dropping server-initiated message");
        }
    }
}

fn decode_response(original_id: RequestId, resp: proto::Response) -> Result<Response> {
    let result = match resp.result {
        Some(bytes) => Some(serde_json::from_slice::<serde_json::Value>(&bytes)?),
        None => None,
    };
    let error = resp.error.map(|e| jsonrpc::Error {
        code: jsonrpc::ErrorCode::from(e.code as i64),
        message: e.message,
        data: e
            .data
            .and_then(|d| serde_json::from_slice::<serde_json::Value>(&d).ok()),
    });
    Ok(Response {
        id: original_id,
        result,
        error,
    })
}

/// Server-side handler for decoded envelopes.
#[async_trait::async_trait]
#[allow(dead_code)]
pub trait RpcHandler {
    async fn handle_message(&self, message: Message) -> Result<Option<Message>>;
}

/// RPC server that reads Envelopes from a byte stream and dispatches to a handler.
#[allow(dead_code)]
pub struct RpcServer {
    handler: Box<dyn RpcHandler + Send + Sync>,
}

impl RpcServer {
    pub fn new<H: RpcHandler + Send + Sync + 'static>(handler: H) -> Self {
        Self {
            handler: Box::new(handler),
        }
    }

    pub async fn serve<S>(&self, mut stream: S) -> Result<()>
    where
        S: AsyncRead + AsyncWrite + Send + Unpin,
    {
        info!("RPC server started");
        let mut buffer = Vec::new();

        loop {
            match read_envelope(&mut stream, &mut buffer).await {
                Ok(envelope) => {
                    let envelope_id = envelope.id;
                    let incoming_msg = match envelope_to_message(envelope) {
                        Ok(msg) => msg,
                        Err(e) => {
                            warn!("Failed to decode envelope: {e}");
                            continue;
                        }
                    };

                    match self.handler.handle_message(incoming_msg).await {
                        Ok(Some(response_msg)) => {
                            let response_envelope =
                                match message_to_response_envelope(response_msg, envelope_id) {
                                    Ok(env) => env,
                                    Err(e) => {
                                        warn!("Failed to encode response: {e}");
                                        continue;
                                    }
                                };
                            if let Err(e) =
                                write_envelope(&mut stream, &mut buffer, response_envelope).await
                            {
                                error!("Failed to write response envelope: {e}");
                                break;
                            }
                        }
                        Ok(None) => {}
                        Err(e) => {
                            warn!("Handler error: {e}");
                            if let Some(env_id) = envelope_id {
                                let error_envelope = Envelope {
                                    id: None,
                                    responding_to: Some(env_id),
                                    payload: Some(envelope::Payload::Response(proto::Response {
                                        result: None,
                                        error: Some(RpcError {
                                            code: -32000,
                                            message: e.to_string(),
                                            data: None,
                                        }),
                                    })),
                                };
                                if let Err(we) =
                                    write_envelope(&mut stream, &mut buffer, error_envelope).await
                                {
                                    error!("Failed to write error envelope: {we}");
                                    break;
                                }
                            }
                        }
                    }
                }
                Err(e) => {
                    error!("RPC server read error: {e}");
                    break;
                }
            }
        }
        info!("RPC server stopped");
        Ok(())
    }
}

/// Translate an incoming Envelope into an internal Message. The caller is
/// responsible for using `envelope.id` when producing a response.
#[allow(dead_code)]
fn envelope_to_message(envelope: Envelope) -> Result<Message> {
    let payload = envelope
        .payload
        .ok_or_else(|| anyhow!("envelope missing payload"))?;
    match payload {
        envelope::Payload::Request(req) => {
            let params: crate::msg::Params = serde_json::from_slice(&req.params)?;
            let id_numeric = envelope
                .id
                .ok_or_else(|| anyhow!("request envelope missing id"))?;
            Ok(Message::Request(Request {
                id: RequestId::from(id_numeric as i32),
                method: req.method,
                params,
            }))
        }
        envelope::Payload::Notification(notif) => {
            let params: crate::msg::Params = serde_json::from_slice(&notif.params)?;
            Ok(Message::Notification(Notification {
                method: notif.method,
                params,
            }))
        }
        envelope::Payload::Response(_) => {
            Err(anyhow!("Unexpected response envelope on server side"))
        }
    }
}

/// Encode a server-produced Response Message into a correlated Envelope.
#[allow(dead_code)]
fn message_to_response_envelope(message: Message, responding_to: Option<u32>) -> Result<Envelope> {
    let response = match message {
        Message::Response(r) => r,
        _ => return Err(anyhow!("handler returned non-response message")),
    };
    let result = match response.result {
        Some(v) => Some(serde_json::to_vec(&v)?),
        None => None,
    };
    let error = response.error.map(|e| RpcError {
        code: e.code.code() as i32,
        message: e.message,
        data: e
            .data
            .as_ref()
            .and_then(|d| serde_json::to_vec(d).ok()),
    });
    Ok(Envelope {
        id: None,
        responding_to,
        payload: Some(envelope::Payload::Response(proto::Response {
            result,
            error,
        })),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::msg::{Notification as MsgNotification, Params};
    use std::sync::{Arc, Mutex};
    use tokio_util::compat::TokioAsyncReadCompatExt;

    #[test]
    fn envelope_round_trip() {
        let env = Envelope {
            id: Some(42),
            responding_to: None,
            payload: Some(envelope::Payload::Request(proto::Request {
                method: "textDocument/hover".into(),
                params: b"{}".to_vec(),
            })),
        };
        let mut buf = Vec::new();
        env.clone().encode(&mut buf).unwrap();
        let decoded = Envelope::decode(buf.as_slice()).unwrap();
        assert_eq!(env.id, decoded.id);
        match decoded.payload.unwrap() {
            envelope::Payload::Request(r) => {
                assert_eq!(r.method, "textDocument/hover");
                assert_eq!(r.params, b"{}");
            }
            _ => panic!("expected request"),
        }
    }

    /// Test handler that echoes back the request id inside the result, and
    /// captures any notifications it sees for later assertion.
    struct EchoHandler {
        notifications: Arc<Mutex<Vec<MsgNotification>>>,
    }

    #[async_trait::async_trait]
    impl RpcHandler for EchoHandler {
        async fn handle_message(&self, message: Message) -> Result<Option<Message>> {
            match message {
                Message::Request(req) => {
                    // Fail loudly on a specific method so we can exercise the
                    // error path as well.
                    if req.method == "force_error" {
                        return Err(anyhow!("boom"));
                    }
                    Ok(Some(Message::Response(Response {
                        id: req.id.clone(),
                        result: Some(serde_json::json!({
                            "method": req.method,
                            "echoed_uri": req.params.uri,
                        })),
                        error: None,
                    })))
                }
                Message::Notification(notif) => {
                    self.notifications.lock().unwrap().push(notif);
                    Ok(None)
                }
                Message::Response(_) => Ok(None),
            }
        }
    }

    /// Build a `Params` that carries the same shape Emacs sends, so the
    /// request/response path is exercised on real serde paths.
    fn params_with_uri(uri: &str) -> Params {
        Params {
            uri: Some(uri.to_string()),
            context: None,
            virtual_doc: None,
            params: serde_json::json!({ "textDocument": { "uri": uri } }),
        }
    }

    /// Stand up a client↔server pair connected over an in-memory duplex
    /// stream. Returns the client so the test can drive it.
    async fn spawn_pair(
        notifications: Arc<Mutex<Vec<MsgNotification>>>,
    ) -> RpcClient {
        let (client_side, server_side) = tokio::io::duplex(1 << 16);

        // RpcClient wants futures::{AsyncRead, AsyncWrite}. Tokio's DuplexStream
        // only implements the tokio traits, so wrap both halves with compat.
        // `Compat<T>` already implements both futures traits when `T` implements
        // the tokio ones, so a single `.compat()` is enough.
        let client_stream = client_side.compat();
        let server_stream = server_side.compat();

        tokio::spawn(async move {
            let server = RpcServer::new(EchoHandler { notifications });
            let _ = server.serve(server_stream).await;
        });

        // Tests don't care about server-initiated messages, so pass None.
        RpcClient::new(client_stream, None).expect("build RpcClient")
    }

    #[tokio::test]
    async fn request_response_round_trip() {
        let notifs = Arc::new(Mutex::new(Vec::new()));
        let client = spawn_pair(notifs.clone()).await;

        let req = Request {
            id: crate::msg::RequestId::from(7),
            method: "textDocument/hover".into(),
            params: params_with_uri("file:///tmp/foo.rs"),
        };

        let resp = client.send_request(req).await.expect("request ok");
        assert_eq!(resp.id, crate::msg::RequestId::from(7));
        assert!(resp.error.is_none(), "expected no error, got {:?}", resp.error);
        let result = resp.result.expect("result should be present");
        assert_eq!(result["method"], "textDocument/hover");
        assert_eq!(result["echoed_uri"], "file:///tmp/foo.rs");
    }

    #[tokio::test]
    async fn notification_delivered_to_handler() {
        let notifs = Arc::new(Mutex::new(Vec::new()));
        let client = spawn_pair(notifs.clone()).await;

        let notif = MsgNotification {
            method: "textDocument/didChange".into(),
            params: params_with_uri("file:///tmp/bar.rs"),
        };
        client.send_notification(notif).expect("send notification");

        // Give the server task a chance to pick the notification up.
        for _ in 0..100 {
            if !notifs.lock().unwrap().is_empty() {
                break;
            }
            tokio::time::sleep(std::time::Duration::from_millis(10)).await;
        }

        let collected = notifs.lock().unwrap();
        assert_eq!(collected.len(), 1);
        assert_eq!(collected[0].method, "textDocument/didChange");
        assert_eq!(
            collected[0].params.uri.as_deref(),
            Some("file:///tmp/bar.rs")
        );
    }

    #[tokio::test]
    async fn handler_error_surfaces_as_rpc_error() {
        let notifs = Arc::new(Mutex::new(Vec::new()));
        let client = spawn_pair(notifs).await;

        let req = Request {
            id: crate::msg::RequestId::from(99),
            method: "force_error".into(),
            params: params_with_uri("file:///tmp/x.rs"),
        };
        let resp = client.send_request(req).await.expect("transport ok");
        assert_eq!(resp.id, crate::msg::RequestId::from(99));
        let err = resp.error.expect("expected RpcError");
        assert_eq!(err.message, "boom");
    }
}
