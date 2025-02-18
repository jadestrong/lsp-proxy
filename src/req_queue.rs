use std::collections::HashMap;

use serde::Serialize;

use crate::{
    lsp::jsonrpc,
    msg::{Request, RequestId, Response},
};

#[derive(Debug)]
pub struct ReqQueue<I, O> {
    pub incoming: Incoming<I>,
    pub outgoing: Outgoing<O>,
}

impl<I, O> Default for ReqQueue<I, O> {
    fn default() -> Self {
        Self {
            incoming: Incoming {
                pending: HashMap::default(),
            },
            outgoing: Outgoing {
                next_id: 0,
                pending: HashMap::default(),
            },
        }
    }
}

#[derive(Debug)]
pub struct Incoming<I> {
    pending: HashMap<RequestId, I>,
}

#[derive(Debug)]
pub struct Outgoing<O> {
    next_id: i32,
    pending: HashMap<RequestId, O>,
}

#[allow(dead_code)]
impl<I> Incoming<I> {
    pub fn register(&mut self, id: RequestId, data: I) {
        self.pending.insert(id, data);
    }

    pub fn cancel(&mut self, id: RequestId) -> Option<Response> {
        let _data = self.complete(id.clone())?;
        let error = jsonrpc::Error {
            code: jsonrpc::ErrorCode::RequestCanceled,
            message: "canceled by client".to_string(),
            data: None,
        };
        Some(Response {
            id,
            result: None,
            error: Some(error),
        })
    }

    pub fn complete(&mut self, id: RequestId) -> Option<I> {
        self.pending.remove(&id)
    }

    pub fn is_completed(&self, id: &RequestId) -> bool {
        !self.pending.contains_key(id)
    }

    pub fn entries(&self) -> Vec<(&RequestId, &I)> {
        self.pending.iter().map(|(k, v)| (k, v)).collect()
    }

    pub fn is_empty(&self) -> bool {
        self.pending.is_empty()
    }

    pub fn values(&self) -> Vec<&I> {
        self.pending.values().collect()
    }
}

impl<O> Outgoing<O> {
    pub fn register<P: Serialize>(&mut self, method: String, params: P, data: O) -> Request {
        let id = RequestId::from(self.next_id);
        self.pending.insert(id.clone(), data);
        self.next_id += 1;
        Request::new(id, method, params)
    }

    pub fn complete(&mut self, id: RequestId) -> Option<O> {
        self.pending.remove(&id)
    }
}
