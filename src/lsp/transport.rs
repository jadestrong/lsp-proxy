use super::{jsonrpc, Error, Result};
use crate::{msg::RequestId, logging::LspLogger};
use anyhow::Context;
use log::error;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::{collections::HashMap, sync::Arc};
use tokio::{
    io::{AsyncBufRead, AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader, BufWriter},
    process::{ChildStderr, ChildStdin, ChildStdout},
    sync::{
        mpsc::{unbounded_channel, Sender, UnboundedReceiver, UnboundedSender},
        Mutex, Notify,
    },
    time::{timeout, Duration},
};

#[derive(Debug)]
pub enum Payload {
    Request {
        chan: Sender<Result<Value>>,
        value: jsonrpc::MethodCall,
    },
    Notification(jsonrpc::Notification),
    Response(jsonrpc::Output),
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
#[serde(untagged)]
enum ServerMessage {
    Output(jsonrpc::Output),
    Call(jsonrpc::Call),
}

pub struct Transport {
    id: usize,
    name: String,
    logger: LspLogger,
    pending_requests: Mutex<HashMap<RequestId, Sender<core::result::Result<Value, Error>>>>,
}

impl Transport {
    pub fn start(
        server_stdout: BufReader<ChildStdout>,
        server_stdin: BufWriter<ChildStdin>,
        server_stderr: BufReader<ChildStderr>,
        id: usize,
        name: String,
    ) -> (
        UnboundedReceiver<(usize, jsonrpc::Call)>,
        UnboundedSender<Payload>,
        Arc<Notify>,
    ) {
        let (client_tx, rx) = unbounded_channel();
        // 发送给 server ，接收要发生给 server 的消息
        let (tx, client_rx) = unbounded_channel();
        let notify = Arc::new(Notify::new());

        let logger = LspLogger::new(name.clone(), Some(id as u32));
        
        let transport = Self {
            id,
            name,
            logger,
            pending_requests: Mutex::new(HashMap::default()),
        };

        let transport = Arc::new(transport);

        tokio::spawn(Self::recv(
            transport.clone(),
            server_stdout,
            client_tx.clone(),
        ));

        tokio::spawn(Self::err(transport.clone(), server_stderr));
        tokio::spawn(Self::send(
            transport,
            server_stdin,
            client_tx,
            client_rx,
            notify.clone(),
        ));

        (rx, tx, notify)
    }

    async fn recv(
        transport: Arc<Self>,
        mut server_stdout: BufReader<ChildStdout>,
        client_tx: UnboundedSender<(usize, jsonrpc::Call)>,
    ) {
        let mut recv_buffer = String::new();
        loop {
            match Self::recv_server_message(&mut server_stdout, &mut recv_buffer, &transport.logger)
                .await
            {
                Ok(msg) => {
                    match transport
                        .process_server_message(&client_tx, msg, &transport.name)
                        .await
                    {
                        Ok(_) => {}
                        Err(err) => {
                            error!("ok {} err: <- {err:?}", transport.name);
                            break;
                        }
                    };
                }
                Err(err) => {
                    if !matches!(err, Error::StreamClosed) {
                        error!(
                            "Exiting {} after unexpected error: {err:?}",
                            &transport.name
                        );
                    }
                    // Close any outstanding requests.
                    for (id, tx) in transport.pending_requests.lock().await.drain() {
                        match tx.send(Err(Error::StreamClosed)).await {
                            Ok(_) => (),
                            Err(_) => {
                                error!("Could not close request on a closed channel (id={:?})", id)
                            }
                        }
                    }

                    use lsp_types::notification::Notification as _;
                    let notification =
                        ServerMessage::Call(jsonrpc::Call::Notification(jsonrpc::Notification {
                            jsonrpc: None,
                            method: lsp_types::notification::Exit::METHOD.to_string(),
                            params: jsonrpc::Params::None,
                        }));
                    match transport
                        .process_server_message(&client_tx, notification, &transport.name)
                        .await
                    {
                        Ok(_) => {}
                        Err(err) => {
                            error!("stream closed err: <- {:?}", err);
                        }
                    }
                    break;
                }
            }
        }
    }

    async fn send(
        transport: Arc<Self>,
        mut server_stdin: BufWriter<ChildStdin>,
        client_tx: UnboundedSender<(usize, jsonrpc::Call)>,
        mut client_rx: UnboundedReceiver<Payload>,
        initialize_notify: Arc<Notify>,
    ) {
        let mut pending_messages: Vec<Payload> = Vec::new();
        let mut is_pending = true;

        // Determine if a message is allowed to be send early
        fn is_initialize(payload: &Payload) -> bool {
            use lsp_types::{
                notification::{Initialized, Notification},
                request::{Initialize, Request},
            };
            match payload {
                Payload::Request {
                    value: jsonrpc::MethodCall { method, .. },
                    ..
                } if method == Initialize::METHOD => true,
                Payload::Notification(jsonrpc::Notification { method, .. })
                    if method == Initialized::METHOD =>
                {
                    true
                }
                _ => false,
            }
        }

        fn is_shutdown(payload: &Payload) -> bool {
            use lsp_types::request::{Request, Shutdown};
            matches!(payload, Payload::Request { value: jsonrpc::MethodCall { method, .. }, .. } if method == Shutdown::METHOD)
        }

        loop {
            tokio::select! {
                biased;
                _ = initialize_notify.notified() => {
                    let initialized = jsonrpc::Notification {
                        jsonrpc: Some(jsonrpc::Version::V2),
                        method: lsp_types::notification::Initialized::METHOD.to_string(),
                        params: jsonrpc::Params::None,
                    };
                    transport.send_payload_to_server(&mut server_stdin, Payload::Notification(initialized)).await.unwrap();
                    is_pending = false;

                    // Hack: inject an initialized notification so we trigger code that needs to happen after init
                    // callback: main_loop.rs handle_language_server_message
                    use lsp_types::notification::Notification;
                    let notification = ServerMessage::Call(jsonrpc::Call::Notification(jsonrpc::Notification {
                        jsonrpc: None,
                        method: lsp_types::notification::Initialized::METHOD.to_string(),
                        params: jsonrpc::Params::None,
                    }));
                    let language_server_name = &transport.name;
                    match transport.process_server_message(&client_tx, notification, language_server_name).await {
                        Ok(_) => {},
                        Err(err) => {
                            error!("{language_server_name} err: <- {err:?}");
                        },
                    }

                    // drain the pending queue and send payloads to server
                    for msg in pending_messages.drain(..) {
                        match transport.send_payload_to_server(&mut server_stdin, msg).await {
                            Ok(_) => {},
                            Err(err) => {
                                error!("{language_server_name} err: <- {err:?}");
                            },
                        }
                    }
                }
                msg = client_rx.recv() => {
                    if let Some(msg) = msg {
                        if is_pending && is_shutdown(&msg) {
                            break;
                        } else if is_pending && !is_initialize(&msg) {
                            // ignore notifications
                            // if let Payload::Notification(_) = msg {
                            //     continue;
                            // }
                            pending_messages.push(msg);
                        } else {
                            match transport.send_payload_to_server(&mut server_stdin, msg).await {
                                Ok(_) => {},
                                Err(err) => {
                                    error!("{}({}) err: <- {err:?}", transport.name, transport.id);
                                },
                            }
                        }
                    } else {
                        error!("recv none msg, cause channel closed");
                        // channel closed
                        break;
                    }
                }
            }
        }
    }

    async fn err(transport: Arc<Self>, mut server_stderr: BufReader<ChildStderr>) {
        let mut error_buffer = Vec::new();
        let mut line_buffer = String::new();
        
        loop {
            line_buffer.clear();
            
            // Try to read a line with a timeout
            let read_result = timeout(
                Duration::from_millis(100), // 100ms timeout for detecting end of error block
                server_stderr.read_line(&mut line_buffer)
            ).await;
            
            match read_result {
                Ok(Ok(0)) => {
                    // EOF reached
                    if !error_buffer.is_empty() {
                        Self::flush_error_buffer(&mut error_buffer, &transport.logger);
                    }
                    error!("err~ {} err: <- StreamClosed", transport.name);
                    break;
                }
                Ok(Ok(_)) => {
                    // Successfully read a line
                    let line = line_buffer.trim();
                    if !line.is_empty() {
                        error_buffer.push(line.to_string());
                    }
                }
                Ok(Err(err)) => {
                    // IO error
                    if !error_buffer.is_empty() {
                        Self::flush_error_buffer(&mut error_buffer, &transport.logger);
                    }
                    error!("err~ {} err: <- {err:?}", transport.name);
                    break;
                }
                Err(_) => {
                    // Timeout occurred - likely end of error block
                    if !error_buffer.is_empty() {
                        Self::flush_error_buffer(&mut error_buffer, &transport.logger);
                    }
                    // Continue reading for more errors
                }
            }
        }
    }
    
    fn flush_error_buffer(buffer: &mut Vec<String>, logger: &LspLogger) {
        if buffer.is_empty() {
            return;
        }
        
        if buffer.len() == 1 {
            // Single line error - log as before
            logger.log_error(&buffer[0]);
        } else {
            // Multi-line error - combine into single log entry
            let combined_error = buffer.join("\n");
            logger.log_multiline_error(&combined_error);
        }
        
        buffer.clear();
    }

    async fn recv_server_message(
        reader: &mut (impl AsyncBufRead + Unpin + Send),
        buffer: &mut String,
        logger: &LspLogger,
    ) -> core::result::Result<ServerMessage, Error> {
        let mut content_length = None;
        loop {
            buffer.truncate(0);
            if reader.read_line(buffer).await? == 0 {
                return Err(Error::StreamClosed);
            };

            if buffer == "\r\n" {
                break;
            }

            let header = buffer.trim();

            let parts = header.split_once(": ");
            match parts {
                Some(("Content-Length", value)) => {
                    content_length = Some(value.parse().context("invald content length")?);
                }
                Some((_, _)) => {}
                None => {
                    // Workaround: Some non-conformant language servers will output logging and other garbage
                    // into the same stream as JSON-RPC messages. This can also happen from shell scripts that spawn
                    // the server. Skip such lines and log a warning.

                    // warn!("Failed to parse header: {:?}", header);
                }
            }
        }

        let content_length = content_length.context("missing content length")?;

        let mut content = vec![0; content_length];
        reader.read_exact(&mut content).await?;
        let msg = std::str::from_utf8(&content).context("invalid utf8 from server")?;

        // Log the received message with structured logging
        logger.log_response(msg);
        
        let output: serde_json::Result<ServerMessage> = serde_json::from_str(msg);

        Ok(output?)
    }

    async fn process_server_message(
        &self,
        client_tx: &UnboundedSender<(usize, jsonrpc::Call)>,
        msg: ServerMessage,
        language_server_name: &str,
    ) -> Result<()> {
        match msg {
            ServerMessage::Output(output) => {
                self.process_request_response(output, language_server_name)
                    .await?
            }
            ServerMessage::Call(call) => {
                client_tx
                    .send((self.id, call))
                    .context("failed to send a message to server")?;
            }
        };

        Ok(())
    }

    async fn process_request_response(
        &self,
        output: jsonrpc::Output,
        language_server_name: &str,
    ) -> Result<()> {
        let (id, result) = match output {
            jsonrpc::Output::Success(jsonrpc::Success { id, result, .. }) => (id, Ok(result)),
            jsonrpc::Output::Failure(jsonrpc::Failure { id, error, .. }) => {
                error!("{language_server_name} <- {error}");
                (id, Err(error.into()))
            }
        };

        if let Some(tx) = self.pending_requests.lock().await.remove(&id) {
            match tx.send(result).await {
                Ok(_) => (),
                Err(_) => error!(
                    "Tried sending response into a closed channel (id={:?}), original request likely timed out",
                    id,
                ),
            };
        } else {
            error!(
                "Discarding Language Server response without a request (id={:?})",
                id
            );
        }

        Ok(())
    }

    async fn send_payload_to_server(
        &self,
        server_stdin: &mut BufWriter<ChildStdin>,
        payload: Payload,
    ) -> Result<()> {
        let json = match payload {
            Payload::Request { chan, value } => {
                self.pending_requests
                    .lock()
                    .await
                    .insert(value.id.clone(), chan);
                serde_json::to_string(&value)?
            }
            Payload::Notification(value) => serde_json::to_string(&value)?,
            Payload::Response(error) => serde_json::to_string(&error)?,
        };
        self.send_string_to_server(server_stdin, json, &self.name)
            .await
    }

    async fn send_string_to_server(
        &self,
        server_stdin: &mut BufWriter<ChildStdin>,
        request: String,
        _language_server_name: &str,
    ) -> Result<()> {
        // Use structured logging for outgoing messages
        if let Some(method) = LspLogger::extract_method(&request) {
            self.logger.log_request(&method, &request);
        } else {
            self.logger.log_debug(&format!("Sending message: {}", &request));
        }

        // send the headers
        server_stdin
            .write_all(format!("Content-Length: {}\r\n\r\n", request.len()).as_bytes())
            .await?;

        // send the body
        server_stdin.write_all(request.as_bytes()).await?;

        server_stdin.flush().await?;

        Ok(())
    }
}
