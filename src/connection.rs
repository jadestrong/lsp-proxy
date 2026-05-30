use crate::msg::Message;
use crossbeam_channel::{bounded, Receiver, Sender};
use log::{debug, error, warn};
use std::{
    io::{self, stdin, stdout},
    thread,
};

/// Force fd 0 (stdin) back to blocking mode.
///
/// `tokio::process::Command`'s pre-exec fd plumbing can leave fd 0 with
/// `O_NONBLOCK` set on Unix, which makes subsequent blocking `read`s return
/// `EAGAIN` (errno 35). Our stdio reader thread uses blocking reads, so clear
/// the flag before the first read and every time we recover from an error.
#[cfg(unix)]
fn force_stdin_blocking() {
    use std::os::unix::io::AsRawFd;
    let fd = io::stdin().as_raw_fd();
    unsafe {
        let flags = libc::fcntl(fd, libc::F_GETFL);
        if flags < 0 {
            warn!("stdio reader: F_GETFL failed on stdin: {}", io::Error::last_os_error());
            return;
        }
        if (flags & libc::O_NONBLOCK) != 0 {
            if libc::fcntl(fd, libc::F_SETFL, flags & !libc::O_NONBLOCK) < 0 {
                warn!(
                    "stdio reader: failed to clear O_NONBLOCK on stdin: {}",
                    io::Error::last_os_error()
                );
            } else {
                debug!("stdio reader: cleared O_NONBLOCK on stdin");
            }
        }
    }
}

#[cfg(not(unix))]
fn force_stdin_blocking() {}

pub struct Connection {
    pub sender: Sender<Message>,
    pub receiver: Receiver<Message>,
}

impl Connection {
    /// Create connection over standard in/standard out.
    ///
    /// Use this to create a real language server.
    pub fn stdio() -> (Connection, IoThreads) {
        let (writer_sender, writer_receiver) = bounded::<Message>(0);
        let writer = thread::spawn(move || {
            let stdout = stdout();
            let mut stdout = stdout.lock();
            writer_receiver
                .into_iter()
                .try_for_each(|message| message.write(&mut stdout))?;
            Ok(())
        });
        let (reader_sender, reader_receiver) = bounded::<Message>(0);
        let reader = thread::spawn(move || {
            force_stdin_blocking();
            let stdin = stdin();
            let mut stdin = stdin.lock();
            loop {
                match Message::read(&mut stdin) {
                    Ok(Some(msg)) => {
                        if let Err(e) = reader_sender.send(msg) {
                            error!("stdio reader: inbox receiver dropped: {e}");
                            return Ok(());
                        }
                    }
                    Ok(None) => {
                        debug!("stdio reader: EOF on stdin, exiting");
                        return Ok(());
                    }
                    Err(e) if e.kind() == io::ErrorKind::WouldBlock => {
                        // Something flipped fd 0 into non-blocking after we
                        // cleared it — most commonly a tokio::process::Command
                        // spawn racing us. Restore blocking mode and retry.
                        warn!("stdio reader: stdin went non-blocking mid-read; restoring");
                        force_stdin_blocking();
                    }
                    Err(e) => {
                        error!("stdio reader: read error on stdin: {e}");
                        return Err(e);
                    }
                }
            }
        });
        let threads = IoThreads { reader, writer };
        (
            Connection {
                sender: writer_sender,
                receiver: reader_receiver,
            },
            threads,
        )
    }
}

pub struct IoThreads {
    reader: thread::JoinHandle<io::Result<()>>,
    writer: thread::JoinHandle<io::Result<()>>,
}

impl IoThreads {
    pub fn new(
        reader: thread::JoinHandle<io::Result<()>>,
        writer: thread::JoinHandle<io::Result<()>>,
    ) -> Self {
        Self { reader, writer }
    }

    pub fn join(self) -> io::Result<()> {
        match self.reader.join() {
            Ok(r) => r?,
            Err(err) => {
                println!("reader panicked!");
                std::panic::panic_any(err)
            }
        }

        match self.writer.join() {
            Ok(r) => r,
            Err(err) => {
                println!("writer panicked!");
                std::panic::panic_any(err);
            }
        }
    }
}
