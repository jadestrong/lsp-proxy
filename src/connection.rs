use crate::msg::Message;
use crossbeam_channel::{bounded, Receiver, Sender};
use log::{debug, error};
use std::{
    io::{self, stdin, stdout},
    thread,
};

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
            let stdin = stdin();
            let mut stdin = stdin.lock();
            loop {
                match Message::read(&mut stdin) {
                    Ok(Some(msg)) => {
                        if let Err(e) = reader_sender.send(msg) {
                            error!("stdio reader: inbox receiver dropped: {}", e);
                            return Ok(());
                        }
                    }
                    Ok(None) => {
                        debug!("stdio reader: EOF on stdin, exiting");
                        return Ok(());
                    }
                    Err(e) => {
                        error!("stdio reader: read error on stdin: {}", e);
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
