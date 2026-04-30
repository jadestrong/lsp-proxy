use anyhow::Result;
use crossbeam_channel::{bounded, select, Receiver, Sender};
use log::{debug, error, info, warn};
use lsp_types::notification::Notification;
use std::collections::HashMap;
use std::sync::Arc;
use std::thread;
use std::time::Instant;
use tokio::sync::mpsc;
use tokio::sync::mpsc::UnboundedSender;

use crate::{
    lsp_ext,
    msg::{self, Context, Message, Request, Response},
    remote::RemoteConnectionManager,
    req_queue,
};

pub(crate) type ReqHandler = fn(Response);
type ReqQueue = req_queue::ReqQueue<(String, Instant, Request), ReqHandler>;

const REQUEST_WHITELIST: &[&str] = &[
    "textDocument/signatureHelp",
    "emacs/workspaceRestart",
    "textDocument/diagnostic",
];

pub struct Controller {
    sender_for_server: UnboundedSender<Message>,
    sender_to_emacs: Sender<Message>,
    req_queue: ReqQueue,
    processing_requests: HashMap<String, Request>,
    remote_manager: Option<Arc<RemoteConnectionManager>>,
    /// Outbound channel into the remote worker thread. If `None`, remote
    /// routing is disabled and every message goes through the local path.
    remote_task_tx: Option<mpsc::UnboundedSender<RemoteTask>>,
    /// Responses/notifications produced by the remote worker. Consumed by the
    /// main `select!` loop and forwarded to Emacs.
    remote_result_rx: Option<Receiver<Message>>,
}

/// Unit of work queued to the remote worker thread.
#[derive(Debug)]
struct RemoteTask {
    message: Message,
}

impl Controller {
    pub fn new(
        sender_for_server: UnboundedSender<Message>,
        sender_for_emacs: Sender<Message>,
    ) -> Self {
        let remote_manager = match RemoteConnectionManager::new() {
            Ok(manager) => {
                info!("Remote connection manager initialized successfully");
                Some(Arc::new(manager))
            }
            Err(e) => {
                warn!(
                    "Failed to initialize remote connection manager: {}. Running in local mode only.",
                    e
                );
                None
            }
        };

        let (remote_task_tx, remote_result_rx) = match remote_manager.clone() {
            Some(manager) => {
                let (task_tx, task_rx) = mpsc::unbounded_channel::<RemoteTask>();
                let (result_tx, result_rx) = bounded::<Message>(64);
                spawn_remote_worker(manager, task_rx, result_tx);
                (Some(task_tx), Some(result_rx))
            }
            None => (None, None),
        };

        Controller {
            sender_for_server,
            sender_to_emacs: sender_for_emacs,
            req_queue: ReqQueue::default(),
            processing_requests: HashMap::new(),
            remote_manager,
            remote_task_tx,
            remote_result_rx,
        }
    }

    fn handle_message(&mut self, msg: Message, now: Instant) -> Result<()> {
        // If the remote worker is up and the message targets a remote path,
        // register it and hand it off to the async worker instead of the local
        // LSP pipeline.
        if self.remote_task_tx.is_some() && self.is_remote_message(&msg) {
            if let Message::Request(ref req) = msg {
                self.register_request(req, now);
            }
            let tx = self.remote_task_tx.as_ref().unwrap();
            if let Err(e) = tx.send(RemoteTask { message: msg }) {
                error!("remote worker channel closed: {}", e);
            }
            return Ok(());
        }

        // Local path (unchanged).
        match msg {
            Message::Request(req) => {
                if REQUEST_WHITELIST.contains(&req.method.as_str()) {
                    self.register_request(&req, now);
                    self.sender_for_server.send(req.into()).unwrap();
                } else if !self.processing_requests.contains_key(&req.method) {
                    debug!(
                        "No pending request of type {}, register and process {:?}",
                        req.method, req.id
                    );
                    self.register_request(&req, now);
                    self.processing_requests.insert(req.method.clone(), req.clone());
                    self.sender_for_server.send(req.into()).unwrap();
                } else {
                    debug!(
                        "Has pending request of type {}, register {:?}",
                        req.method, req.id
                    );
                    self.register_request(&req, now);
                }
            }
            Message::Response(resp) => {
                self.sender_for_server.send(resp.into()).unwrap();
            }
            Message::Notification(not) => {
                self.sender_for_server.send(not.into()).unwrap();
            }
        }

        Ok(())
    }

    fn is_remote_message(&self, message: &Message) -> bool {
        if let Some(path) = self.extract_path_from_message(message) {
            return path.starts_with("/ssh:") || path.starts_with("/rpc:");
        }
        false
    }

    fn extract_path_from_message(&self, message: &Message) -> Option<String> {
        match message {
            Message::Request(req) => {
                if let Some(uri) = &req.params.uri {
                    Some(uri.clone())
                } else {
                    self.extract_uri_from_params(&req.params.params)
                }
            }
            Message::Notification(notif) => self.extract_uri_from_params(&notif.params.params),
            _ => None,
        }
    }

    fn extract_uri_from_params(&self, params: &serde_json::Value) -> Option<String> {
        if let Some(text_doc) = params.get("textDocument") {
            if let Some(uri) = text_doc.get("uri") {
                return uri.as_str().map(|s| s.to_string());
            }
        }
        if let Some(uri) = params.get("uri") {
            return uri.as_str().map(|s| s.to_string());
        }
        None
    }

    pub fn run(
        &mut self,
        inbox: Receiver<Message>,
        receiver_of_server: Receiver<Message>,
    ) -> Result<()> {
        // `crossbeam::select!` needs a real Receiver. When the remote worker
        // isn't running, point at a dummy channel that never fires.
        let (_dummy_tx, dummy_rx) = bounded::<Message>(0);
        loop {
            let remote_rx = self.remote_result_rx.clone().unwrap_or_else(|| dummy_rx.clone());
            select! {
                recv(inbox) -> msg => {
                    let now = Instant::now();
                    if let Ok(msg) = msg {
                        if let Err(e) = self.handle_message(msg, now) {
                            error!("Error handling message: {}", e);
                        }
                    } else {
                        error!("inbox error");
                        panic!("inbox error");
                    }
                }
                recv(receiver_of_server) -> msg => {
                    match msg {
                        Ok(msg) => {
                            match msg {
                                Message::Request(req) => {
                                    self.sender_to_emacs.send(req.into()).unwrap();
                                },
                                Message::Response(resp) => {
                                    self.respond(resp);
                                },
                                Message::Notification(not) => {
                                    self.sender_to_emacs.send(not.into()).unwrap();
                                },
                            }
                        },
                        Err(err) => {
                            error!("server error {err:?}");
                            std::panic::panic_any(msg)
                        },
                    }
                }
                recv(remote_rx) -> msg => {
                    match msg {
                        Ok(msg) => {
                            match msg {
                                Message::Response(resp) => {
                                    self.respond(resp);
                                }
                                Message::Notification(not) => {
                                    self.sender_to_emacs.send(not.into()).unwrap();
                                }
                                Message::Request(req) => {
                                    // Server-initiated requests are rare for now;
                                    // forward verbatim.
                                    self.sender_to_emacs.send(req.into()).unwrap();
                                }
                            }
                        }
                        Err(err) => {
                            warn!("remote worker result channel closed: {}", err);
                        }
                    }
                }
            }
        }
    }

    pub(crate) fn register_request(&mut self, request: &Request, request_received: Instant) {
        let should_cancel_old = match &request.params.context {
            Some(Context::Completion(new_context)) => {
                if let Some(old_request) = self.processing_requests.get(&request.method) {
                    matches!(&old_request.params.context,
                        Some(Context::Completion(old_context))
                        if old_context.prefix.is_empty() && !new_context.prefix.is_empty())
                } else {
                    false
                }
            }
            _ => false,
        };

        if should_cancel_old {
            if let Some(old_request) = self.processing_requests.get(&request.method) {
                debug!(
                    "{:?} completion request canceled due to new request {:?} with non-empty prefix",
                    old_request.id, request.id,
                );

                let cancel_notification = msg::Notification::new(
                    lsp_ext::CustomizeCancel::METHOD.to_string(),
                    lsp_ext::CustomizeCancelParams {
                        uri: old_request.params.uri.clone(),
                        id: old_request.id.clone(),
                    },
                );
                self.sender_for_server.send(cancel_notification.into()).unwrap();
            }
        }

        let duplicate_id = {
            let entries = self.req_queue.incoming.entries();
            entries
                .iter()
                .find_map(|(id, (method, _, _))| {
                    (&request.method == method).then(|| (*id).to_owned())
                })
        };

        if let Some(id) = duplicate_id {
            debug!(
                "{:?}({:?}){} duplicated, canceled",
                id, request.id, request.method,
            );
            self.req_queue.incoming.cancel(&id);
        }

        self.req_queue.incoming.register(
            request.id.clone(),
            (request.method.clone(), request_received, request.clone()),
        );
    }

    pub(crate) fn respond(&mut self, response: Response) {
        let id = response.id.clone();

        let (method, duration) = if let Some((method, start, _)) =
            self.req_queue.incoming.complete(&id)
        {
            if let Some(err) = &response.error {
                if err.message.starts_with("server panicked") {
                    error!("{}, check the log", err.message)
                }
            }

            let duration = start.elapsed();
            info!("handled {} - ({}) in {:0.2?}", method, response.id, duration);

            self.sender_to_emacs.send(response.into()).unwrap();

            (method, Some(duration))
        } else {
            debug!(
                "received response({:?}), but request had been canceled",
                response.id
            );

            let method = self
                .processing_requests
                .iter()
                .find(|(_, req)| req.id == id)
                .map(|(method, _)| method.clone());

            (method.unwrap_or_default(), None)
        };

        if let Some(processing_request) = self.processing_requests.get_mut(&method) {
            if processing_request.id == id {
                self.processing_requests.remove(&method);

                if duration.is_some() {
                    debug!(
                        "request {} completed in {:?}, processing next request of type {}",
                        id,
                        duration.unwrap(),
                        method
                    );
                } else {
                    debug!("request {id} was canceled, processing next request of type {method}");
                }

                self.process_next_request_of_type(&method);
            }
        }
    }

    pub(crate) fn process_next_request_of_type(&mut self, method: &str) {
        let next_request = {
            let entries = self.req_queue.incoming.entries();
            entries
                .iter()
                .filter(|&(_, (req_method, _, _))| req_method == method)
                .min_by_key(|&(_, &(_, instant, _))| instant)
                .map(|(_, (_, _, req))| req.clone())
        };

        if let Some(req) = next_request {
            debug!(
                "processing next request of type {} - id: {:?}",
                method, req.id
            );
            self.sender_for_server.send(req.clone().into()).unwrap();
            self.processing_requests.insert(method.to_string(), req);
        } else {
            debug!("no more pending requests of type {method}");
        }
    }
}

/// Boot a dedicated tokio current-thread runtime that consumes RemoteTasks,
/// calls `RemoteConnectionManager::route_message`, and pipes results back into
/// `result_tx` for the controller to forward to Emacs.
fn spawn_remote_worker(
    manager: Arc<RemoteConnectionManager>,
    mut task_rx: mpsc::UnboundedReceiver<RemoteTask>,
    result_tx: Sender<Message>,
) {
    thread::Builder::new()
        .name("lsp-proxy-remote".into())
        .spawn(move || {
            let rt = match tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
            {
                Ok(rt) => rt,
                Err(e) => {
                    error!("failed to build tokio runtime for remote worker: {}", e);
                    return;
                }
            };
            rt.block_on(async move {
                while let Some(task) = task_rx.recv().await {
                    let manager = manager.clone();
                    let result_tx = result_tx.clone();
                    tokio::spawn(async move {
                        match manager.route_message(task.message).await {
                            Ok(Some(response)) => {
                                if let Err(e) = result_tx.send(response) {
                                    warn!("remote result channel closed: {}", e);
                                }
                            }
                            Ok(None) => {
                                debug!("remote worker: notification accepted, no response");
                            }
                            Err(e) => {
                                error!("remote routing failed: {}", e);
                            }
                        }
                    });
                }
            });
        })
        .expect("failed to spawn remote worker thread");
}
