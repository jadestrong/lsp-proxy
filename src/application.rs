use crate::{
    editor::Editor,
    job::Jobs,
    large_file_manager::LargeFileManager,
    msg::{Message, Notification, Response},
    req_queue, syntax,
};
use crossbeam_channel::Sender;
use std::{sync::Arc, sync::Mutex, time::Instant};

pub(crate) type ReqHandler = fn(&mut Application, Response);
type ReqQueue = req_queue::ReqQueue<(String, Instant), ReqHandler>;

pub(crate) struct Application {
    pub sender: Sender<Message>,
    req_queue: ReqQueue,
    pub editor: Editor,
    pub jobs: Jobs,
    pub shutdown_requested: bool,
    pub large_file_manager: Arc<Mutex<LargeFileManager>>,
}

impl Application {
    pub(crate) fn new(
        sender: Sender<Message>,
        syn_loader_config: syntax::Configuration,
    ) -> Application {
        // Set the max completion items
        let loader =
            syntax::Loader::new(syn_loader_config).expect("Cound not compile loader for config");
        let syn_loader = std::sync::Arc::new(loader);
        let editor = Editor::new(syn_loader.clone());

        Application {
            sender,
            req_queue: ReqQueue::default(),
            editor,
            jobs: Jobs::new(),
            shutdown_requested: false,
            large_file_manager: Arc::new(Mutex::new(LargeFileManager::new())),
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

    pub(crate) fn request_shutdown(&mut self) {
        log::info!("Shutdown requested");
        self.shutdown_requested = true;
    }

    pub(crate) fn cleanup_resources(&mut self) {
        log::info!("Cleaning up resources before exit");

        // Shutdown all language servers
        for client in self.editor.language_servers.iter_clients() {
            log::debug!("Shutting down language server: {}", client.name());
            // Send shutdown to language servers if they're running
            if client.is_initialized() {
                let _ = client.shutdown_and_exit();
            }
        }

        // Clear documents
        self.editor.documents.clear();

        // Clear request queue
        self.req_queue = req_queue::ReqQueue::default();

        log::info!("Resource cleanup complete");
    }
}
