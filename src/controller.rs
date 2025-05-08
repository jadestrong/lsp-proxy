use anyhow::Result;
use crossbeam_channel::{select, Receiver, Sender};
use log::{debug, error, info};
use lsp_types::notification::Notification;
use std::time::Instant;
use tokio::sync::mpsc::UnboundedSender;
use std::collections::HashMap;

use crate::{
    lsp_ext,
    msg::{self, Context, Message, Request, Response},
    req_queue,
};

pub(crate) type ReqHandler = fn(Response);
type ReqQueue = req_queue::ReqQueue<(String, Instant, Request), ReqHandler>;

const REQUEST_WHITELIST: &[&str] = &["textDocument/signatureHelp", "emacs/workspaceRestart", "textDocument/diagnostic"];

pub struct Controller {
    sender_for_server: UnboundedSender<Message>,
    sender_to_emacs: Sender<Message>,
    req_queue: ReqQueue,
    processing_requests: HashMap<String, Request>,
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
            processing_requests: HashMap::new(),
        }
    }

    pub fn run(
        &mut self,
        inbox: Receiver<Message>,
        receiver_of_server: Receiver<Message>,
    ) -> Result<()> {
        loop {
            select! {
                recv(inbox) -> msg => {
                    let now = Instant::now();
                    if let Ok(msg) = msg {
                        match msg {
                            Message::Request(req) => {
                                if REQUEST_WHITELIST.contains(&req.method.as_str()) {
                                    self.register_request(&req, now);
                                    self.sender_for_server.send(req.into()).unwrap();
                                } else {
                                    if !self.processing_requests.contains_key(&req.method) {
                                        debug!("No pending request of type {}, register and process {:?}", req.method, req.id);
                                        self.register_request(&req, now);
                                        self.processing_requests.insert(req.method.clone(), req.clone());
                                        self.sender_for_server.send(req.into()).unwrap();
                                    } else {
                                        debug!("Has pending request of type {}, register {:?}", req.method, req.id);
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
        let should_cancel_old = match &request.params.context {
            Some(Context::CompletionContext(new_context)) => {
                if let Some(old_request) = self.processing_requests.get(&request.method) {
                    matches!(&old_request.params.context, 
                        Some(Context::CompletionContext(old_context)) 
                        if old_context.prefix.is_empty() && !new_context.prefix.is_empty())
                } else {
                    false
                }
            }
            _ => false
        };

        if should_cancel_old {
            if let Some(old_request) = self.processing_requests.get(&request.method) {
                debug!(
                    "{:?} completion request canceled due to new request {:?} with non-empty prefix",
                    old_request.id,
                    request.id,
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
        
        let (method, duration) = if let Some((method, start, _)) = self.req_queue.incoming.complete(&id) {
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
            
            (method, Some(duration))
        } else {
            debug!(
                "received response({:?}), but request had been canceled",
                response.id
            );
            
            let method = self.processing_requests
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
                    debug!(
                        "request {} was canceled, processing next request of type {}",
                        id,
                        method
                    );
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
            debug!("processing next request of type {} - id: {:?}", method, req.id);
            self.sender_for_server.send(req.clone().into()).unwrap();
            self.processing_requests.insert(method.to_string(), req);
        } else {
            debug!("no more pending requests of type {}", method);
        }
    }
}
