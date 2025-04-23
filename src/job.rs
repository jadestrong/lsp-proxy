use futures_util::{
    future::{BoxFuture, Future},
    stream::FuturesUnordered,
    FutureExt,
};
use once_cell::sync::OnceCell;
use tokio::sync::mpsc::{channel, Receiver, Sender};

use crate::editor::Editor;

pub type Callback = Box<dyn FnOnce(&mut Editor) + Send>;

static JOB_QUEUE: OnceCell<Sender<Callback>> = OnceCell::new();

pub type JobFuture = BoxFuture<'static, anyhow::Result<Option<Callback>>>;

pub struct Job {
    pub future: BoxFuture<'static, anyhow::Result<Option<Callback>>>,
    pub wait: bool,
}

pub struct Jobs {
    pub wait_futures: FuturesUnordered<JobFuture>,
    pub callbacks: Receiver<Callback>,
}

impl Job {
    pub fn new<F: Future<Output = anyhow::Result<()>> + Send + 'static>(f: F) -> Self {
        Self {
            future: f.map(|r| r.map(|()| None)).boxed(),
            wait: false,
        }
    }

    pub fn with_callback<F: Future<Output = anyhow::Result<Callback>> + Send + 'static>(
        f: F,
    ) -> Self {
        Self {
            future: f.map(|r| r.map(Some)).boxed(),
            wait: false,
        }
    }
}

impl Jobs {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        let (tx, rx) = channel(1024);
        let _ = JOB_QUEUE.set(tx);
        Self {
            wait_futures: FuturesUnordered::new(),
            callbacks: rx,
        }
    }

    pub fn callback<F: Future<Output = anyhow::Result<Callback>> + Send + 'static>(
        &mut self,
        f: F,
    ) {
        self.add(Job::with_callback(f));
    }

    pub fn add(&self, j: Job) {
        if j.wait {
            self.wait_futures.push(j.future);
        } else {
            tokio::spawn(async move {
                match j.future.await {
                    Ok(Some(cb)) => dispatch_callback(cb).await,
                    Ok(None) => (),
                    Err(err) => {
                        log::error!("Job error {err}");
                    }
                }
            });
        }
    }

    pub fn handle_callback(
        &self,
        editor: &mut Editor,
        call: anyhow::Result<Option<Callback>>,
    ) {
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

pub async fn dispatch_callback(job: Callback) {
    let _ = JOB_QUEUE.wait().send(job).await;
}
