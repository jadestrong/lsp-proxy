use anyhow::Result;
use crossbeam_channel::{select, Receiver, Sender};
use log::{debug, error, info};
use lsp_types::notification::Notification;
use std::time::Instant;
use tokio::sync::mpsc::UnboundedSender;

use crate::{
    lsp_ext,
    msg::{self, Context, Message, Request, Response},
    req_queue,
};

pub(crate) type ReqHandler = fn(Response);
type ReqQueue = req_queue::ReqQueue<(String, Instant, Request), ReqHandler>;

const REQUEST_WHITELIST: &[&str] = &["textDocument/signatureHelp", "emacs/workspaceRestart"];

pub struct Controller {
    sender_for_server: UnboundedSender<Message>,
    sender_to_emacs: Sender<Message>,
    req_queue: ReqQueue,
    processing_request: Option<Request>,
}

impl Controller {
    pub fn new(
        sender_for_server: UnboundedSender<Message>,
        sender_for_emacs: Sender<Message>,
    ) -> Self {
        Controller {
            sender_for_server,
            sender_to_emacs: sender_for_emacs,
            req_queue: ReqQueue::default(),
            processing_request: None,
        }
    }

    pub fn run(
        &mut self,
        inbox: Receiver<Message>,
        receiver_of_server: Receiver<Message>,
    ) -> Result<()> {
        loop {
            select! {
                // biased;
                recv(inbox) -> msg => {
                    let now = Instant::now();
                    if let Ok(msg) = msg {
                        match msg {
                            Message::Request(req) => {
                                if REQUEST_WHITELIST.contains(&req.method.as_str()) {
                                    self.register_request(&req, now);
                                    self.sender_for_server.send(req.into()).unwrap();
                                } else {
                                    if self.req_queue.incoming.is_empty() || self.req_queue.incoming.values().iter().all(|req| REQUEST_WHITELIST.contains(&req.0.as_str())) {
                                        debug!("No pending request, register and process {:?}", req.id);
                                        self.register_request(&req, now);
                                        self.processing_request = Some(req.clone());
                                        self.sender_for_server.send(req.into()).unwrap();
                                    } else {
                                        debug!("Has pending request, register {:?}", req.id);
                                        self.register_request(&req, now);
                                    }
                                }
                            },
                            Message::Response(resp) => {
                                self.sender_for_server.send(resp.into()).unwrap();
                            },
                            Message::Notification(not) => {
                                self.sender_for_server.send(not.into()).unwrap();
                            },
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
                            error!("server error {:?}", err);
                            std::panic::panic_any(msg)
                        },
                    }
                }
            }
        }
    }

    pub(crate) fn register_request(&mut self, request: &Request, request_received: Instant) {
        // 如果存在相同的请求，则取消它，只处理最新的
        // 1. 如果是 textDocument/completion 请求，如果当前在执行的请求的 prefix 为空，则不可复用；
        // 2. prefix 不为空
        if let Some(id) =
            self.req_queue
                .incoming
                .entries()
                .iter()
                .find_map(|(id, (method, _, __))| {
                    if &request.method == method {
                        Some(id.to_owned().clone())
                    } else {
                        None
                    }
                })
        {
            debug!(
                "{:?}({:?}){} duplicated, canceled",
                id, request.id, request.method,
            );
            self.req_queue.incoming.cancel(id);
            self.req_queue.incoming.register(
                request.id.clone(),
                (request.method.clone(), request_received, request.clone()),
            );
            if let Some(old_request) = &self.processing_request {
                match (&old_request.params.context, &request.params.context) {
                    (
                        Some(Context::CompletionContext(old_context)),
                        Some(Context::CompletionContext(context)),
                    ) => {
                        // 如果旧的请求是prefix空的，且新请求的prefix不是空，则不可复用
                        if old_context.prefix.is_empty() && !context.prefix.is_empty() {
                            debug!(
                            "{:?} request cannot be reused, it need to be canceled. Handle the latest request ({:?}) first.",
                            old_request.id,
                            request.id,
                            );

                            let not = msg::Notification::new(
                                lsp_ext::CustomizeCancel::METHOD.to_string(),
                                lsp_ext::CustomizeCancelParams {
                                    uri: old_request.params.uri.clone(),
                                    id: old_request.id.clone(),
                                },
                            );
                            // Cancel current request
                            self.sender_for_server.send(not.into()).unwrap();
                        }
                    }
                    _ => {}
                }
            }
        } else {
            self.req_queue.incoming.register(
                request.id.clone(),
                (request.method.clone(), request_received, request.clone()),
            );
        }
    }

    pub(crate) fn respond(&mut self, response: Response) {
        let id = response.id.clone();
        if let Some((method, start, _)) = self.req_queue.incoming.complete(id.clone()) {
            if let Some(err) = &response.error {
                if err.message.starts_with("server panicked") {
                    error!("{}, check the log", err.message)
                }
            }

            let duration = start.elapsed();
            info!(
                "handled {} - ({}) in {:0.2?}",
                method, response.id, duration
            );
            self.sender_to_emacs.send(response.into()).unwrap();
        } else {
            debug!(
                "received response({:?}), but request had been canceled",
                response.id
            );
        }
        // 只有当完成的请求和当前正在执行的请求一致时才处理下一个请求，否则表明已经有新的请求在执行了
        if let Some(processing_request) = &self.processing_request {
            if &processing_request.id == &id {
                self.process_next_request();
            }
        }
    }

    pub(crate) fn process_next_request(&mut self) {
        let requests = self.req_queue.incoming.entries();
        if let Some((_, (_, _, req))) = requests
            .iter()
            .filter(|&(_, value)| !REQUEST_WHITELIST.contains(&value.0.as_str()))
            .min_by_key(|&(_, &(_, instant, _))| instant)
        {
            if let Some(processing_request) = &self.processing_request {
                if processing_request.id == req.id {
                    return;
                }
            }
            let cloned_req = req.clone();
            debug!("process next req {:?}", cloned_req.id);
            self.sender_for_server.send(cloned_req.into()).unwrap();
            self.processing_request = Some(req.clone());
        }
    }
}
