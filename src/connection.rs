use crate::msg::Message;
use crossbeam_channel::{bounded, Receiver, Sender};
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
            // 将接收到的数据都直接传递给 stdout
            writer_receiver
                .into_iter()
                .try_for_each(|it| it.write(&mut stdout))?;
            Ok(())
        });
        let (reader_sender, reader_receiver) = bounded::<Message>(0);
        let reader = thread::spawn(move || {
            let stdin = stdin();
            let mut stdin = stdin.lock();
            while let Some(msg) = Message::read(&mut stdin)? {
                let is_exit = matches!(&msg, Message::Notification(n) if n.is_exit());
                reader_sender
                    .send(msg)
                    .expect("receiver was dropped, failed to send a message.");

                if is_exit {
                    break;
                }
            }
            Ok(())
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
