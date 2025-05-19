use once_cell::sync::OnceCell;
use tokio::sync::mpsc::{channel, Receiver, Sender};

use crate::editor::Editor;

pub type Callback = Box<dyn FnOnce(&mut Editor) + Send>;

static JOB_QUEUE: OnceCell<Sender<Callback>> = OnceCell::new();

pub struct Jobs {
    pub callbacks: Receiver<Callback>,
}

impl Jobs {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let (tx, rx) = channel(1024);
        let _ = JOB_QUEUE.set(tx);
        Self { callbacks: rx }
    }

    pub fn handle_callback(&self, editor: &mut Editor, call: anyhow::Result<Option<Callback>>) {
        match call {
            Ok(None) => {}
            Ok(Some(call)) => call(editor),
            Err(e) => {
                log::error!("Async job failed: {}", e);
            }
        }
    }
}

pub async fn dispatch(job: impl FnOnce(&mut Editor) + Send + 'static) {
    let _ = JOB_QUEUE.wait().send(Box::new(job)).await;
}
