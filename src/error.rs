use crate::msg::{Notification, Request};
use core::fmt;

#[derive(Debug)]
pub enum ExtractError<T> {
    /// The extracted message was of a different method that expected.
    MethodMismatch(T),
    /// Failed to deserialize the message.
    JsonError {
        method: String,
        error: serde_json::Error,
    },
}

impl std::error::Error for ExtractError<Request> {}
impl fmt::Display for ExtractError<Request> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtractError::MethodMismatch(req) => {
                write!(f, "Method mismatch for request '{}'", req.method)
            }
            ExtractError::JsonError { method, error } => {
                write!(f, "Invalid request\nMethod: {method}\n error: {error}",)
            }
        }
    }
}

impl std::error::Error for ExtractError<Notification> {}
impl fmt::Display for ExtractError<Notification> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtractError::MethodMismatch(req) => {
                write!(f, "Method mismatch for notification '{}'", req.method)
            }
            ExtractError::JsonError { method, error } => {
                write!(f, "Invalid notification\nMethod: {method}\n error:{error}")
            }
        }
    }
}
