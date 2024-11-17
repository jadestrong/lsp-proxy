use std::sync::{
    atomic::{AtomicUsize, Ordering},
    Arc,
};

use crossbeam_channel::{Receiver, Sender};

use crate::thread::Builder;

use super::JoinHandle;

pub struct Pool {
    job_sender: Sender<Job>,
    _handles: Vec<JoinHandle>,
    extant_tasks: Arc<AtomicUsize>,
}

struct Job {
    f: Box<dyn FnOnce() + Send + 'static>,
}

impl Pool {
    pub fn new(threads: usize) -> Pool {
        const STACK_SIZE: usize = 8 * 1024 * 1024;

        let (job_sender, job_receiver) = crossbeam_channel::unbounded();
        let extant_tasks = Arc::new(AtomicUsize::new(0));

        let mut handles = Vec::with_capacity(threads);
        for _ in 0..threads {
            let handle = Builder::new()
                .stack_size(STACK_SIZE)
                .name("Worker".into())
                .spawn({
                    let extant_tasks = Arc::clone(&extant_tasks);
                    let job_receiver: Receiver<Job> = job_receiver.clone();
                    move || {
                        for job in job_receiver {
                            extant_tasks.fetch_add(1, Ordering::SeqCst);
                            (job.f)();
                            extant_tasks.fetch_sub(1, Ordering::SeqCst);
                        }
                    }
                })
                .expect("failed to spawn thread");

            handles.push(handle);
        }

        Pool {
            job_sender,
            _handles: handles,
            extant_tasks,
        }
    }

    pub fn spawn<F>(&self, f: F)
    where
        F: FnOnce() + Send + 'static,
    {
        let f = Box::new(move || f());

        let job = Job { f };
        self.job_sender.send(job).unwrap();
    }

    pub fn len(&self) -> usize {
        self.extant_tasks.load(Ordering::SeqCst)
    }
}
