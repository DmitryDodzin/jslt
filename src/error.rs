use thiserror::Error;

use crate::parser::Span;

pub type Result<T, E = JsltError> = std::result::Result<T, E>;

#[derive(Debug, Error)]
pub enum JsltError {
  #[error("There isn't a vaild start delimiter, expecting '{{' or '[' or '\"'")]
  InvalidStart,

  #[error("InvalidToken: there is an invalid token ({0:?}) at {1:?}")]
  InvalidToken(String, Span),

  #[error("UnexpectedToken: expected {1:?} but got {0:?}")]
  UnexpectedToken(String, &'static str),

  #[error(transparent)]
  SerdeJson(#[from] serde_json::Error),
}
