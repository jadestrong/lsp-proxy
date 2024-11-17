use thiserror::Error;

use crate::msg::RequestId;

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
    Timeout(RequestId),
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
