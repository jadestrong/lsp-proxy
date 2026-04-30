//! Remote-server entry point.
//!
//! When lsp-proxy is launched with `--remote-server`, it speaks the Protobuf
//! Envelope protocol on stdin/stdout instead of LSP JSON-RPC. Incoming
//! Envelopes are translated to the internal `msg::Message` type so the normal
//! `main_loop` + `Controller` can process them, and outgoing messages are
//! re-encoded back into Envelopes for the SSH tunnel.

use anyhow::Result;
use crossbeam_channel::{bounded, Receiver, Sender};
use log::{debug, error};
use lsp_types::NumberOrString;
use prost::Message as _;
use std::io::{stdin, stdout, Read, Write};
use std::thread;

use crate::connection::{Connection, IoThreads};
use crate::lsp::jsonrpc;
use crate::msg::{Message, Notification, Params, Request, RequestId, Response};
use crate::remote::rpc::{
    proto::{self, envelope, Envelope, RpcError},
    MessageLen, MESSAGE_LEN_SIZE,
};

const MAX_MESSAGE_LEN: u32 = 10 * 1024 * 1024;

/// Build a [`Connection`] that speaks Envelope frames on stdio.
///
/// The returned `IoThreads` can be joined just like `Connection::stdio()`.
pub fn envelope_stdio() -> (Connection, IoThreads) {
    let (writer_sender, writer_receiver) = bounded::<Message>(0);
    let (reader_sender, reader_receiver) = bounded::<Message>(0);

    let writer = thread::spawn(move || write_loop(writer_receiver));
    let reader = thread::spawn(move || read_loop(reader_sender));

    (
        Connection {
            sender: writer_sender,
            receiver: reader_receiver,
        },
        IoThreads::new(reader, writer),
    )
}

fn read_loop(sender: Sender<Message>) -> std::io::Result<()> {
    let mut stdin = stdin().lock();
    let mut len_buf = [0u8; MESSAGE_LEN_SIZE];
    let mut body = Vec::<u8>::new();

    loop {
        if let Err(e) = stdin.read_exact(&mut len_buf) {
            if e.kind() == std::io::ErrorKind::UnexpectedEof {
                return Ok(());
            }
            return Err(e);
        }
        let len = MessageLen::from_le_bytes(len_buf);
        if len == 0 || len > MAX_MESSAGE_LEN {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("invalid envelope length: {}", len),
            ));
        }
        body.resize(len as usize, 0);
        stdin.read_exact(&mut body)?;

        let envelope = match Envelope::decode(body.as_slice()) {
            Ok(e) => e,
            Err(e) => {
                error!("failed to decode envelope: {}", e);
                continue;
            }
        };

        match envelope_to_message(envelope) {
            Ok(msg) => {
                if sender.send(msg).is_err() {
                    return Ok(());
                }
            }
            Err(e) => {
                debug!("ignoring undecodable envelope: {}", e);
            }
        }
    }
}

fn write_loop(receiver: Receiver<Message>) -> std::io::Result<()> {
    let mut stdout = stdout().lock();
    let mut buf = Vec::<u8>::new();

    for msg in receiver {
        let envelope = match message_to_envelope(msg) {
            Ok(e) => e,
            Err(e) => {
                error!("failed to encode outgoing message as envelope: {}", e);
                continue;
            }
        };
        let message_len = envelope.encoded_len() as u32;
        stdout.write_all(&message_len.to_le_bytes())?;
        buf.clear();
        buf.reserve(message_len as usize);
        envelope
            .encode(&mut buf)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        stdout.write_all(&buf)?;
        stdout.flush()?;
    }
    Ok(())
}

fn envelope_to_message(envelope: Envelope) -> Result<Message> {
    let payload = envelope
        .payload
        .ok_or_else(|| anyhow::anyhow!("envelope missing payload"))?;
    match payload {
        envelope::Payload::Request(req) => {
            let params: Params = serde_json::from_slice(&req.params)?;
            let id_numeric = envelope
                .id
                .ok_or_else(|| anyhow::anyhow!("request envelope missing id"))?;
            Ok(Message::Request(Request {
                id: RequestId::from(id_numeric as i32),
                method: req.method,
                params,
            }))
        }
        envelope::Payload::Notification(notif) => {
            let params: Params = serde_json::from_slice(&notif.params)?;
            Ok(Message::Notification(Notification {
                method: notif.method,
                params,
            }))
        }
        envelope::Payload::Response(_) => {
            Err(anyhow::anyhow!("server should not receive response payloads"))
        }
    }
}

fn message_to_envelope(msg: Message) -> Result<Envelope> {
    match msg {
        Message::Response(Response { id, result, error }) => {
            let responding_to = request_id_to_u32(&id);
            let result_bytes = match result {
                Some(v) => Some(serde_json::to_vec(&v)?),
                None => None,
            };
            let error = error.map(|e| RpcError {
                code: e.code.code() as i32,
                message: e.message,
                data: e
                    .data
                    .as_ref()
                    .and_then(|d| serde_json::to_vec(d).ok()),
            });
            Ok(Envelope {
                id: None,
                responding_to,
                payload: Some(envelope::Payload::Response(proto::Response {
                    result: result_bytes,
                    error,
                })),
            })
        }
        Message::Notification(Notification { method, params }) => {
            let params_bytes = serde_json::to_vec(&params)?;
            Ok(Envelope {
                id: None,
                responding_to: None,
                payload: Some(envelope::Payload::Notification(proto::Notification {
                    method,
                    params: params_bytes,
                })),
            })
        }
        Message::Request(Request { id, method, params }) => {
            // Server-initiated requests. Use the numeric id if available,
            // otherwise fall back to hashing/ignoring (strings aren't
            // representable in the wire `id` field).
            let params_bytes = serde_json::to_vec(&params)?;
            Ok(Envelope {
                id: request_id_to_u32(&id),
                responding_to: None,
                payload: Some(envelope::Payload::Request(proto::Request {
                    method,
                    params: params_bytes,
                })),
            })
        }
    }
}

fn request_id_to_u32(id: &RequestId) -> Option<u32> {
    match NumberOrString::from(id.clone()) {
        NumberOrString::Number(n) => Some(n as u32),
        NumberOrString::String(_) => None,
    }
}

// Silence unused-import lint on jsonrpc when feature flags change.
#[allow(dead_code)]
fn _use_jsonrpc(_e: &jsonrpc::Error) {}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_params(uri: &str) -> Params {
        Params {
            uri: Some(uri.to_string()),
            context: None,
            virtual_doc: None,
            params: serde_json::json!({ "textDocument": { "uri": uri } }),
        }
    }

    #[test]
    fn envelope_request_round_trip() {
        let envelope = Envelope {
            id: Some(17),
            responding_to: None,
            payload: Some(envelope::Payload::Request(proto::Request {
                method: "textDocument/definition".into(),
                params: serde_json::to_vec(&make_params("file:///x.rs")).unwrap(),
            })),
        };

        let msg = envelope_to_message(envelope).expect("decode");
        match msg {
            Message::Request(req) => {
                assert_eq!(req.method, "textDocument/definition");
                assert_eq!(req.id, RequestId::from(17));
                assert_eq!(req.params.uri.as_deref(), Some("file:///x.rs"));
            }
            other => panic!("expected Request, got {:?}", other),
        }
    }

    #[test]
    fn response_message_encodes_responding_to() {
        let msg = Message::Response(Response {
            id: RequestId::from(21),
            result: Some(serde_json::json!({"ok": true})),
            error: None,
        });
        let env = message_to_envelope(msg).expect("encode");
        assert_eq!(env.responding_to, Some(21));
        match env.payload {
            Some(envelope::Payload::Response(resp)) => {
                let decoded: serde_json::Value =
                    serde_json::from_slice(&resp.result.unwrap()).unwrap();
                assert_eq!(decoded["ok"], true);
                assert!(resp.error.is_none());
            }
            _ => panic!("expected Response payload"),
        }
    }

    #[test]
    fn notification_round_trip() {
        let notif = Notification {
            method: "window/logMessage".into(),
            params: make_params("file:///foo.rs"),
        };
        let env = message_to_envelope(Message::Notification(notif.clone())).unwrap();
        assert!(env.id.is_none() && env.responding_to.is_none());

        let back = envelope_to_message(env).unwrap();
        match back {
            Message::Notification(n) => {
                assert_eq!(n.method, notif.method);
                assert_eq!(n.params.uri, notif.params.uri);
            }
            _ => panic!("expected Notification"),
        }
    }

    #[test]
    fn server_rejects_response_envelope() {
        let env = Envelope {
            id: None,
            responding_to: Some(5),
            payload: Some(envelope::Payload::Response(proto::Response {
                result: None,
                error: None,
            })),
        };
        assert!(envelope_to_message(env).is_err());
    }

    #[test]
    fn request_id_to_u32_handles_numbers_and_strings() {
        assert_eq!(request_id_to_u32(&RequestId::from(0)), Some(0));
        assert_eq!(request_id_to_u32(&RequestId::from(42)), Some(42));
        assert_eq!(
            request_id_to_u32(&RequestId::from("abc".to_string())),
            None
        );
    }
}
