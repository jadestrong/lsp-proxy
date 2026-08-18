use thiserror::Error;
pub mod file_event;
pub mod jsonrpc;
pub mod transport;

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("protocol error: {0}")]
    Rpc(#[from] jsonrpc::Error),
    #[error("failed to parse: {0}")]
    Parse(#[from] serde_json::Error),
    #[error("IO Error: {0}")]
    IO(#[from] std::io::Error),
    #[error("request {0} timed out")]
    Timeout(String),
    #[error("server closed the stream")]
    StreamClosed,
    #[error("Unhandled")]
    Unhandled,
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

#[derive(Debug, PartialEq, Clone)]
pub enum MethodCall {
    WorkDoneProgressCreate(lsp_types::WorkDoneProgressCreateParams),
    ApplyWorkspaceEdit(lsp_types::ApplyWorkspaceEditParams),
    WorkspaceFolders,
    WorkspaceConfiguration(lsp_types::ConfigurationParams),
    RegisterCapability(lsp_types::RegistrationParams),
    UnregisterCapability(lsp_types::UnregistrationParams),
    ShowMessageRequest(lsp_types::ShowMessageRequestParams),
}

impl MethodCall {
    pub fn parse(method: &str, params: jsonrpc::Params) -> Result<MethodCall> {
        use lsp_types::request::Request;
        let request = match method {
            lsp_types::request::WorkspaceConfiguration::METHOD => {
                let params: lsp_types::ConfigurationParams = params.parse()?;
                Self::WorkspaceConfiguration(params)
            }
            lsp_types::request::RegisterCapability::METHOD => {
                let params: lsp_types::RegistrationParams = params.parse()?;
                Self::RegisterCapability(params)
            }
            lsp_types::request::UnregisterCapability::METHOD => {
                let params: lsp_types::UnregistrationParams = params.parse()?;
                Self::UnregisterCapability(params)
            }
            lsp_types::request::WorkDoneProgressCreate::METHOD => {
                let params: lsp_types::WorkDoneProgressCreateParams = params.parse()?;
                Self::WorkDoneProgressCreate(params)
            }
            lsp_types::request::ApplyWorkspaceEdit::METHOD => {
                let params: lsp_types::ApplyWorkspaceEditParams = params.parse()?;
                Self::ApplyWorkspaceEdit(params)
            }
            lsp_types::request::WorkspaceFoldersRequest::METHOD => Self::WorkspaceFolders,
            lsp_types::request::ShowMessageRequest::METHOD => {
                let params: lsp_types::ShowMessageRequestParams = params.parse()?;
                Self::ShowMessageRequest(params)
            }
            _ => {
                return Err(Error::Unhandled);
            }
        };

        Ok(request)
    }
}

#[cfg(test)]
mod show_message_request_tests {
    use super::MethodCall;
    use crate::lsp::jsonrpc;

    fn params(v: serde_json::Value) -> jsonrpc::Params {
        serde_json::from_value(v).unwrap()
    }

    /// `window/showMessageRequest` must parse with its actions intact: dropping them
    /// is what reduced the prompt to a log line with no way to answer.
    #[test]
    fn parses_actions() {
        let call = MethodCall::parse(
            "window/showMessageRequest",
            params(serde_json::json!({
                "type": 2,
                "message": "Build tool conflicts are detected in workspace. Which one would you like to use?",
                "actions": [{"title": "Maven"}, {"title": "Gradle"}, {"title": "Bazel"}]
            })),
        )
        .expect("must parse");
        match call {
            MethodCall::ShowMessageRequest(p) => {
                assert_eq!(p.typ, lsp_types::MessageType::WARNING);
                let actions = p.actions.expect("actions must survive parsing");
                assert_eq!(actions.len(), 3);
                assert_eq!(actions[2].title, "Bazel");
            }
            other => panic!("wrong variant: {other:?}"),
        }
    }

    /// Actions are optional in the spec.
    #[test]
    fn parses_without_actions() {
        let call = MethodCall::parse(
            "window/showMessageRequest",
            params(serde_json::json!({"type": 3, "message": "hi"})),
        )
        .expect("must parse");
        match call {
            MethodCall::ShowMessageRequest(p) => assert!(p.actions.is_none()),
            other => panic!("wrong variant: {other:?}"),
        }
    }
}
