use crate::{
    editor::Editor,
    job::Jobs,
    large_file_manager::LargeFileManager,
    msg::{Message, Notification, Response},
    req_queue, syntax,
};
use crossbeam_channel::Sender;
use std::{sync::Arc, sync::Mutex, time::Instant};

/// A hook registered by the Controller to kill remote processes synchronously
/// before `process::exit()`.  Stored as a global so it is reachable from the
/// Application thread without requiring a shared Arc between the two threads.
static EXIT_HOOK: std::sync::OnceLock<Box<dyn Fn() + Send + Sync>> =
    std::sync::OnceLock::new();

/// Register the exit hook.  Called once during Controller initialisation.
pub fn register_exit_hook(f: impl Fn() + Send + Sync + 'static) {
    EXIT_HOOK.set(Box::new(f)).ok();
}

/// Run the exit hook if one was registered.
pub fn run_exit_hook() {
    if let Some(hook) = EXIT_HOOK.get() {
        hook();
    }
}

pub(crate) type ReqHandler = fn(&mut Application, Response);
type ReqQueue = req_queue::ReqQueue<(String, Instant), ReqHandler>;

pub(crate) struct Application {
    pub sender: Sender<Message>,
    req_queue: ReqQueue,
    /// Server requests that are waiting on an answer from the editor.
    ///
    /// Maps the id of the request we sent to the editor back to the language server
    /// and request id that must be answered once the user decides. `ReqHandler` is a
    /// bare `fn` and cannot capture, so the correlation has to live here.
    pub(crate) pending_editor_choices:
        std::collections::HashMap<crate::msg::RequestId, (usize, crate::msg::RequestId)>,
    /// Editor request id → (language server, `sessionId`) for a pending
    /// `intellij/chooseAction`. Separate from `pending_editor_choices` because the
    /// answer is a new request to the server, not a reply to one of its requests.
    pub(crate) pending_choose_actions:
        std::collections::HashMap<crate::msg::RequestId, (usize, i64)>,
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
            pending_editor_choices: std::collections::HashMap::new(),
            pending_choose_actions: std::collections::HashMap::new(),
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
        let Some(handler) = self.req_queue.outgoing.complete(response.id.clone()) else {
            log::warn!(
                "received response for unknown request id={:?}, ignoring",
                response.id
            );
            return;
        };
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

    /// Like [`Self::send_request`] but returns the id of the request that was sent,
    /// so a deferred reply can be correlated back to it.
    pub(crate) fn send_request_returning_id<R: lsp_types::request::Request>(
        &mut self,
        params: R::Params,
        handler: ReqHandler,
    ) -> crate::msg::RequestId {
        let request = self
            .req_queue
            .outgoing
            .register(R::METHOD.to_string(), params, handler);
        let id = request.id.clone();
        self.send(request.into());
        id
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
                drop(client.shutdown_and_exit());
            }
        }

        // Clear documents
        self.editor.documents.clear();

        // Clear request queue
        self.req_queue = req_queue::ReqQueue::default();

        log::info!("Resource cleanup complete");
    }
}
