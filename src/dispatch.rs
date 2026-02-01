use anyhow::Result;
use crossbeam_channel::Sender;
use futures_util::Future;
use log::error;
use serde::de::DeserializeOwned;
use std::{fmt, panic, sync::Arc};

use crate::{
    application::Application,
    client::Client,
    error::ExtractError,
    handlers::request::create_error_response,
    lsp::jsonrpc,
    msg::{Message, Notification, Request, Response},
    utils::from_json,
};

pub(crate) struct RequestDispatcher {
    pub(crate) req: Option<Request>,
    pub(crate) sender: Sender<Message>,
    pub(crate) language_servers: Vec<Arc<Client>>,
}

impl RequestDispatcher {
    pub(crate) fn on<R, F, Fut>(&mut self, f: F) -> &mut Self
    where
        R: lsp_types::request::Request + 'static,
        R::Params: DeserializeOwned + Send + fmt::Debug + panic::UnwindSafe,
        F: Fn(Request, R::Params, Vec<Arc<Client>>) -> Fut + Send + 'static,
        Fut: Future<Output = Result<Response>> + Send + 'static,
    {
        let (req, params) = match self.parse::<R>() {
            Some(it) => it,
            None => return self,
        };

        let language_servers = self.language_servers.clone();
        let sender = self.sender.clone();
        tokio::spawn(async move {
            let id = req.id.clone();
            let result = f(req, params, language_servers).await;
            match result {
                Ok(response) => sender.send(response.into()).unwrap(),
                Err(err) => sender
                    .send(create_error_response(&id, err.to_string()).into())
                    .unwrap(),
            }
        });
        self
    }

    // 解析 self 的属性，整理成函数需要的格式，比如 context、 params 和 language_servers?
    fn parse<R>(&mut self) -> Option<(Request, R::Params)>
    where
        R: lsp_types::request::Request,
        R::Params: DeserializeOwned + fmt::Debug,
    {
        let req = match &self.req {
            Some(req) if req.method == R::METHOD => self.req.take()?,
            _ => return None,
        };

        let res = from_json(R::METHOD, &req.params.params);
        match res {
            Ok(params) => Some((req, params)),
            Err(err) => {
                let response =
                    Response::new_err(req.id, jsonrpc::ErrorCode::InvalidParams, err.to_string());
                self.sender.send(response.into()).unwrap();
                None
            }
        }
    }

    pub(crate) fn finish(&mut self) {
        if let Some(req) = self.req.take() {
            error!("unknown request: {req:?}");
            let response = Response::new_err(
                req.id,
                jsonrpc::ErrorCode::MethodNotFound,
                "unknown request".to_string(),
            );
            self.sender.send(response.into()).unwrap();
        }
    }
}

pub(crate) struct NotificationDispatcher<'a> {
    pub(crate) not: Option<Notification>,
    pub(crate) app: &'a mut Application,
}

impl NotificationDispatcher<'_> {
    pub(crate) fn on_sync_mut<N>(
        &mut self,
        f: fn(&mut Application, N::Params) -> anyhow::Result<()>,
    ) -> anyhow::Result<&mut Self>
    where
        N: lsp_types::notification::Notification,
        N::Params: DeserializeOwned + Send,
    {
        let not = match self.not.take() {
            Some(it) => it,
            None => return Ok(self),
        };

        let params = match not.extract::<N::Params>(N::METHOD) {
            Ok(it) => it,
            Err(ExtractError::JsonError { method, error }) => {
                panic!("Invald request\nMethod: {method}\n error: {error}");
            }
            Err(ExtractError::MethodMismatch(not)) => {
                self.not = Some(not);
                return Ok(self);
            }
        };

        f(self.app, params)?;
        Ok(self)
    }

    pub(crate) fn on_sync_mut_with_language<N>(
        &mut self,
        f: fn(&mut Application, N::Params, Option<&str>) -> anyhow::Result<()>,
    ) -> anyhow::Result<&mut Self>
    where
        N: lsp_types::notification::Notification,
        N::Params: DeserializeOwned + Send,
    {
        let not = match self.not.take() {
            Some(it) => it,
            None => return Ok(self),
        };

        // Extract language before extracting the main params
        let language = not.params.language.clone();

        let params = match not.extract::<N::Params>(N::METHOD) {
            Ok(it) => it,
            Err(ExtractError::JsonError { method, error }) => {
                panic!("Invald request\nMethod: {method}\n error: {error}");
            }
            Err(ExtractError::MethodMismatch(not)) => {
                self.not = Some(not);
                return Ok(self);
            }
        };

        f(self.app, params, language.as_deref())?;
        Ok(self)
    }

    pub(crate) fn finish(&mut self) {
        if let Some(not) = &self.not {
            if !not.method.starts_with("$/") {
                error!("unhandled notification: {not:?}");
            }
        }
    }
}
