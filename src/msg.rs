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

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct OutRequest {
    pub id: RequestId,
    pub method: String,
    #[serde(default = "serde_json::Value::default")]
    #[serde(skip_serializing_if = "serde_json::Value::is_null")]
    pub params: serde_json::Value,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct OutNotification {
    pub method: String,
    #[serde(default = "serde_json::Value::default")]
    #[serde(skip_serializing_if = "serde_json::Value::is_null")]
    pub params: serde_json::Value,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum OutMessage {
    Request(OutRequest),
    Response(Response),
    Notification(OutNotification),
}

impl From<Message> for OutMessage {
    fn from(msg: Message) -> Self {
        match msg {
            Message::Request(req) => OutMessage::Request(OutRequest {
                id: req.id,
                method: req.method,
                params: req.params.params,
            }),
            Message::Response(res) => OutMessage::Response(res),
            Message::Notification(notif) => OutMessage::Notification(OutNotification {
                method: notif.method,
                params: notif.params.params,
            }),
        }
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

/// Context specific to completion requests.
///
/// Virtual document info is now in `Params.virtual_doc`, not embedded here.
/// This struct only contains completion-specific fields.
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
    #[serde(rename = "docVersion")]
    pub doc_version: i32,
    pub line: String,
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

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct DiagnosticContext {
    #[serde(rename = "limitDiagnostics")]
    pub limit_diagnostics: bool,
}

/// Types of sources that can contain virtual documents.
///
/// Virtual documents are internal representations of code block content
/// (e.g., from org-mode babel blocks) as if they were standalone files.
/// This enum identifies the source type to enable appropriate handling
/// and future extensibility for other embedded code formats.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum VirtualDocSourceType {
    /// Org-mode babel source blocks.
    ///
    /// Represents code blocks in org-mode files that are delimited by
    /// `#+BEGIN_SRC` and `#+END_SRC` markers.
    OrgBabel,
    // Future variants:
    // Markdown,
    // Jupyter,
}

/// Context for virtual documents embedded in other files (e.g., org-mode babel blocks).
///
/// This struct is orthogonal to request-specific context like `CompletionContext` or
/// `DiagnosticContext`. While request context carries information specific to a particular
/// LSP operation (e.g., completion trigger kind), virtual document context carries
/// information about the embedding relationship between the source file and the virtual
/// document, which is relevant across all LSP operations.
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct VirtualDocContext {
    /// Line offset between source file and virtual document.
    ///
    /// Virtual doc line = source file line - line_bias.
    /// This is used to translate positions between the source file (e.g., org file)
    /// and the virtual document (the extracted code block content).
    #[serde(rename = "line-bias")]
    pub line_bias: u32,

    /// Programming language of the virtual document content.
    ///
    /// This identifies the language of the code block (e.g., "python", "rust")
    /// and is used to select the appropriate language server.
    pub language: String,

    /// Type of source containing the virtual document.
    ///
    /// Identifies what kind of document contains the embedded code block,
    /// enabling source-type-specific handling if needed.
    #[serde(rename = "source-type")]
    pub source_type: VirtualDocSourceType,
}

impl VirtualDocContext {
    /// Translate a position from source file coordinates to virtual document coordinates.
    ///
    /// The `line_bias` represents the line offset between the source file (e.g., an org file)
    /// and the virtual document (the extracted code block). When a code block starts at line N
    /// in the source file, `line_bias` is N-1 (0-indexed), so line N in the source becomes
    /// line 0 in the virtual document.
    ///
    /// This function subtracts `line_bias` from the line number while preserving the column
    /// (character) position. Uses `saturating_sub` to prevent underflow when the position's
    /// line is less than `line_bias` (which would indicate an invalid position outside the
    /// code block).
    ///
    /// # Example
    /// If a code block starts at line 10 in an org file (`line_bias = 9`), and the cursor
    /// is at line 15, column 5 in the org file, this translates to line 6, column 5 in the
    /// virtual document.
    pub fn translate_position_to_virtual(&self, pos: lsp_types::Position) -> lsp_types::Position {
        lsp_types::Position {
            line: pos.line.saturating_sub(self.line_bias),
            character: pos.character,
        }
    }

    /// Translate a position from virtual document coordinates to source file coordinates.
    ///
    /// This is the inverse of `translate_position_to_virtual`. It adds `line_bias` to the
    /// line number to convert from virtual document coordinates back to source file coordinates,
    /// while preserving the column (character) position.
    ///
    /// This is used when receiving responses from the language server (which operates on the
    /// virtual document) and translating positions back to the source file for display in
    /// the editor.
    ///
    /// # Example
    /// If a code block starts at line 10 in an org file (`line_bias = 10`), and the language
    /// server reports an error at line 5, column 3 in the virtual document, this translates
    /// to line 15, column 3 in the org file.
    pub fn translate_position_from_virtual(&self, pos: lsp_types::Position) -> lsp_types::Position {
        lsp_types::Position {
            line: pos.line + self.line_bias,
            character: pos.character,
        }
    }

    /// Translate a range from source file coordinates to virtual document coordinates.
    ///
    /// Applies `translate_position_to_virtual` to both the start and end positions of the range.
    /// This is useful for translating text ranges (e.g., for text edits or selections) from
    /// source file coordinates to virtual document coordinates.
    pub fn translate_range_to_virtual(&self, range: lsp_types::Range) -> lsp_types::Range {
        lsp_types::Range {
            start: self.translate_position_to_virtual(range.start),
            end: self.translate_position_to_virtual(range.end),
        }
    }

    /// Translate a range from virtual document coordinates to source file coordinates.
    ///
    /// Applies `translate_position_from_virtual` to both the start and end positions of the range.
    /// This is used when receiving ranges from the language server (e.g., diagnostic ranges,
    /// code action ranges) and translating them back to source file coordinates.
    pub fn translate_range_from_virtual(&self, range: lsp_types::Range) -> lsp_types::Range {
        lsp_types::Range {
            start: self.translate_position_from_virtual(range.start),
            end: self.translate_position_from_virtual(range.end),
        }
    }
}

/// Request-specific context for LSP operations.
///
/// This enum represents different types of context that are specific to particular
/// LSP operations. Virtual document context (line bias, language, source type) is
/// now separate in `Params.virtual_doc` as it is orthogonal to request-specific context.
#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum Context {
    Completion(CompletionContext),
    Resolve(ResolveContext),
    Common(CommonContext),
    Workspace(WorkspaceContext),
    InlineCompletion(InlineCompletionContext),
    Diagnostic(DiagnosticContext),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Params {
    pub uri: Option<String>,
    /// Request-specific context (completion, resolve, diagnostic, etc.).
    ///
    /// This field carries context that is specific to a particular LSP operation,
    /// such as completion trigger kind or diagnostic limits. It is orthogonal to
    /// virtual document context, which describes the embedding relationship.
    pub context: Option<Context>,
    /// Virtual document context (orthogonal to request context).
    ///
    /// This field carries metadata about virtual documents embedded in other files
    /// (e.g., org-mode babel blocks). It is separate from `context` because virtual
    /// document information (line bias, language, source type) is relevant across
    /// all LSP operations, while `context` carries operation-specific data.
    ///
    /// When both fields are present, they provide complementary information:
    /// - `context`: How to handle this specific request (e.g., completion trigger)
    /// - `virtual_doc`: How to translate positions for the embedded code block
    #[serde(rename = "virtual-doc")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub virtual_doc: Option<VirtualDocContext>,
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
        let enable_bytecode = crate::config::ENABLE_BYTECODE.get().unwrap_or(&false);
        self._write_with_bytecode(w, *enable_bytecode)
    }

    pub fn _write_with_bytecode(self, w: &mut impl Write, enable_bytecode: bool) -> io::Result<()> {
        #[derive(Serialize)]
        struct JsonRpc {
            jsonrpc: &'static str,
            #[serde(flatten)]
            msg: OutMessage,
        }
        let out_msg: OutMessage = self.into();
        let json_val = serde_json::to_value(&JsonRpc {
            jsonrpc: "2.0",
            msg: out_msg.clone(),
        })?;
        debug!("> {json_val}");

        if enable_bytecode {
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
                    warn!("Failed to convert json to bytecode: {err}");
                    let text = serde_json::to_string(&JsonRpc {
                        jsonrpc: "2.0",
                        msg: out_msg,
                    })?;
                    write_msg_text(w, &text)
                }
            }
        } else {
            let text = serde_json::to_string(&JsonRpc {
                jsonrpc: "2.0",
                msg: out_msg,
            })?;
            write_msg_text(w, &text)
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
                virtual_doc: None,
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
                virtual_doc: None,
                params: serde_json::to_value(params).unwrap(),
            },
        }
    }

    /// Extract typed parameters, request context, and virtual document context from a notification.
    ///
    /// Returns a 3-tuple containing:
    /// - The deserialized notification parameters
    /// - Optional request-specific context (e.g., completion trigger kind)
    /// - Optional virtual document context (line bias, language, source type)
    ///
    /// The virtual document context is orthogonal to request context and is used
    /// for position translation when working with embedded code blocks (e.g., org-mode babel).
    pub fn extract<P: DeserializeOwned>(
        self,
        method: &str,
    ) -> Result<(P, Option<Context>, Option<VirtualDocContext>), ExtractError<Notification>> {
        if self.method != method {
            return Err(ExtractError::MethodMismatch(self));
        }
        match serde_json::from_value(self.params.params) {
            Ok(params) => Ok((params, self.params.context, self.params.virtual_doc)),
            Err(error) => Err(ExtractError::JsonError {
                method: self.method,
                error,
            }),
        }
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
    debug!("< {buf}");
    Ok(Some(buf))
}

fn write_msg_text(out: &mut dyn Write, msg: &str) -> io::Result<()> {
    write!(out, "Content-Length: {}\r\n\r\n", msg.len())?;
    out.write_all(msg.as_bytes())?;
    out.flush()?;
    Ok(())
}
