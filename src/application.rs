use crate::{
    editor::Editor,
    job::Jobs,
    msg::{Message, Notification, Response},
    req_queue, syntax,
};
use crossbeam_channel::Sender;
use std::time::Instant;

pub(crate) type ReqHandler = fn(&mut Application, Response);
type ReqQueue = req_queue::ReqQueue<(String, Instant), ReqHandler>;

pub(crate) struct Application {
    pub sender: Sender<Message>,
    req_queue: ReqQueue,
    pub editor: Editor,
    pub jobs: Jobs,
}

impl Application {
    pub(crate) fn new(
        sender: Sender<Message>,
        syn_loader_config: syntax::Configuration,
    ) -> Application {
        let loader =
            syntax::Loader::new(syn_loader_config).expect("Cound not compile loader for config");
        let syn_loader = std::sync::Arc::new(loader);
        let editor = Editor::new(syn_loader.clone());

        Application {
            sender,
            req_queue: ReqQueue::default(),
            editor,
            jobs: Jobs::new(),
        }
    }

    // 发送响应给 emacs
    pub(crate) fn respond(&self, response: Response) {
        self.send(response.into());
    }

    pub(crate) fn complete_request(&mut self, response: Response) {
        let handler = self
            .req_queue
            .outgoing
            .complete(response.id.clone())
            .expect("received response for unknown request");
        handler(self, response)
    }

    // FIXME 发送请求给 emacs ？
    pub(crate) fn send_request<R: lsp_types::request::Request>(
        &mut self,
        params: R::Params,
        handler: ReqHandler,
    ) {
        let request = self
            .req_queue
            .outgoing
            .register(R::METHOD.to_string(), params, handler);
        self.send(request.into());
    }

    pub fn send(&self, message: Message) {
        self.sender.send(message).unwrap()
    }

    pub(crate) fn send_notification<N: lsp_types::notification::Notification>(
        &self,
        params: N::Params,
    ) {
        let not = Notification::new(N::METHOD.to_string(), params);
        self.send(not.into());
    }
}
