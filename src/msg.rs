use crate::{bytecode, error::ExtractError, lsp::jsonrpc};
use core::fmt;
use log::{debug, warn};
use lsp_types::NumberOrString;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::io::{self, BufRead, Write};

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum Message {
    Request(Request),
    Response(Response),
    Notification(Notification),
}

impl From<Request> for Message {
    fn from(value: Request) -> Self {
        Message::Request(value)
    }
}

impl From<Response> for Message {
    fn from(value: Response) -> Self {
        Message::Response(value)
    }
}

impl From<Notification> for Message {
    fn from(value: Notification) -> Self {
        Message::Notification(value)
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(untagged)]
enum IdRepr {
    I32(i32),
    String(String),
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(transparent)]
pub struct RequestId(IdRepr);

impl From<i32> for RequestId {
    fn from(value: i32) -> Self {
        RequestId(IdRepr::I32(value))
    }
}

impl From<String> for RequestId {
    fn from(value: String) -> Self {
        RequestId(IdRepr::String(value))
    }
}

impl From<RequestId> for NumberOrString {
    fn from(value: RequestId) -> Self {
        match value.0 {
            IdRepr::I32(num) => NumberOrString::Number(num),
            IdRepr::String(s) => NumberOrString::String(s),
        }
    }
}

impl fmt::Display for RequestId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            IdRepr::I32(it) => fmt::Display::fmt(it, f),
            // Use debug here, to make it clear that `92` and `"92"` are
            // different, and to reduce WTF factor if the server users `" "` as an
            // ID.
            IdRepr::String(it) => fmt::Debug::fmt(it, f),
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct CompletionContext {
    pub line: String,
    pub prefix: String,
    #[serde(rename = "startPoint")]
    pub start_point: i32,
    #[serde(rename = "boundsStart")]
    pub bounds_start: i32,
    #[serde(rename = "triggerKind")]
    pub trigger_kind: lsp_types::CompletionTriggerKind,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct InlineCompletionContext {
    #[serde(rename = "triggerKind")]
    pub trigger_kind: lsp_types::InlineCompletionTriggerKind,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ResolveContext {
    #[serde(rename = "language-server-id")]
    pub language_server_id: usize,
    pub start: i32,
    pub end: i32,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct WorkspaceContext {
    #[serde(rename = "workspace-root")]
    pub workspace_root: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct CommonContext {
    #[serde(rename = "language-server-id")]
    pub language_server_id: usize,
}

// #[derive(Debug, Serialize, Deserialize, Clone)]
// pub struct SignatureHelpContext {
//     #[serde(rename = "signature-trigger-character")]
//     pub signature_trigger_character: String,
// }

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum Context {
    CompletionContext(CompletionContext),
    ResolveContext(ResolveContext),
    CommonContext(CommonContext),
    WorkspaceContext(WorkspaceContext),
    InlineCompletionContext(InlineCompletionContext),
    // SignatureHelpContext(SignatureHelpContext),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Params {
    pub uri: Option<String>,
    pub context: Option<Context>,
    #[serde(default = "serde_json::Value::default")]
    #[serde(skip_serializing_if = "serde_json::Value::is_null")]
    pub params: serde_json::Value,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Request {
    pub id: RequestId,
    pub method: String,
    pub params: Params,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Response {
    // JSON RPC allows this to be null if it was impossible
    // to decode the request's id. Ignore this special case and
    // just die horribly
    pub id: RequestId,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<jsonrpc::Error>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Notification {
    pub method: String,
    pub params: Params,
}

impl Message {
    pub fn read(r: &mut impl BufRead) -> io::Result<Option<Message>> {
        Message::_read(r)
    }

    fn _read(r: &mut dyn BufRead) -> io::Result<Option<Message>> {
        let text = match read_msg_text(r)? {
            None => return Ok(None),
            Some(text) => text,
        };
        let msg = serde_json::from_str(&text)?;
        Ok(Some(msg))
    }

    pub fn write(self, w: &mut impl Write) -> io::Result<()> {
        self._write(w)
    }
    pub fn _write(self, w: &mut impl Write) -> io::Result<()> {
        #[derive(Serialize)]
        struct JsonRpc {
            jsonrpc: &'static str,
            #[serde(flatten)]
            msg: Message,
        }
        let json_val = serde_json::to_value(&JsonRpc {
            jsonrpc: "2.0",
            msg: self.clone(),
        })?;

        match bytecode::generate_bytecode_repl(
            &json_val,
            bytecode::BytecodeOptions {
                object_type: bytecode::ObjectType::Plist,
                null_value: bytecode::LispObject::Nil,
                false_value: bytecode::LispObject::Keyword("json-false".into()),
            },
        ) {
            Ok(bytecode_str) => write_msg_text(w, &bytecode_str),
            Err(err) => {
                warn!("Failed to convert json to bytecode: {}", err);
                let text = serde_json::to_string(&JsonRpc {
                    jsonrpc: "2.0",
                    msg: self,
                })?;
                write_msg_text(w, &text)
            }
        }
    }
}

impl Response {
    pub fn new_ok<R: Serialize>(id: RequestId, result: R) -> Response {
        Response {
            id,
            result: Some(serde_json::to_value(result).unwrap()),
            error: None,
        }
    }
    pub fn new_err(id: RequestId, code: jsonrpc::ErrorCode, message: String) -> Response {
        let error = jsonrpc::Error {
            code,
            message,
            data: None,
        };
        Response {
            id,
            result: None,
            error: Some(error),
        }
    }
}

impl Request {
    pub fn new<P: Serialize>(id: RequestId, method: String, params: P) -> Request {
        Request {
            id,
            method,
            params: Params {
                uri: None,
                context: None,
                params: serde_json::to_value(params).unwrap(),
            },
        }
    }
}

impl Notification {
    pub fn new(method: String, params: impl Serialize) -> Notification {
        Notification {
            method,
            params: Params {
                uri: None,
                context: None,
                params: serde_json::to_value(params).unwrap(),
            },
        }
    }

    pub fn extract<P: DeserializeOwned>(
        self,
        method: &str,
    ) -> Result<P, ExtractError<Notification>> {
        if self.method != method {
            return Err(ExtractError::MethodMismatch(self));
        }
        match serde_json::from_value(self.params.params) {
            Ok(params) => Ok(params),
            Err(error) => Err(ExtractError::JsonError {
                method: self.method,
                error,
            }),
        }
    }

    pub(crate) fn is_exit(&self) -> bool {
        self.method == "exit"
    }
}

fn read_msg_text(inp: &mut dyn BufRead) -> io::Result<Option<String>> {
    fn invalid_data(error: impl Into<Box<dyn std::error::Error + Send + Sync>>) -> io::Error {
        io::Error::new(io::ErrorKind::InvalidData, error)
    }
    macro_rules! invalid_data {
        ($($tt:tt)*) => (invalid_data(format!($($tt)*)))
    }

    let mut size = None;
    let mut buf = String::new();

    loop {
        buf.clear();
        if inp.read_line(&mut buf)? == 0 {
            return Ok(None);
        }
        if !buf.ends_with("\r\n") {
            return Err(invalid_data!("malformed header: {:?}", buf));
        }
        let buf = &buf[..buf.len() - 2];
        if buf.is_empty() {
            break;
        }
        let mut parts = buf.splitn(2, ": ");
        let header_name = parts.next().unwrap();
        let header_value = parts
            .next()
            .ok_or_else(|| invalid_data!("malformed header: {:?}", buf))?;
        if header_name.eq_ignore_ascii_case("Content-Length") {
            size = Some(header_value.parse::<usize>().map_err(invalid_data)?);
        }
    }

    let size: usize = size.ok_or_else(|| invalid_data!("no Content-Length"))?;
    let mut buf = buf.into_bytes();
    buf.resize(size, 0);
    inp.read_exact(&mut buf)?;
    let buf = String::from_utf8(buf).map_err(invalid_data)?;
    debug!("< {}", buf);
    Ok(Some(buf))
}

fn write_msg_text(out: &mut dyn Write, msg: &str) -> io::Result<()> {
    write!(out, "Content-Length: {}\r\n\r\n", msg.len())?;
    out.write_all(msg.as_bytes())?;
    out.flush()?;
    Ok(())
}
