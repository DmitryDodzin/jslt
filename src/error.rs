use serde_json::Value;
use thiserror::Error;

use crate::parser::Rule;

pub type Result<T, E = JsltError> = std::result::Result<T, E>;

#[derive(Debug, Error)]
pub enum JsltError {
  #[error("UnexpectedInput: {0:?} but got {1:?} ({2})")]
  UnexpectedInput(Rule, Rule, String),
  #[error("UnexpectedContent: for {0:?}")]
  UnexpectedContent(Rule),
  #[error("UnexpectedEnd")]
  UnexpectedEnd,
  #[error("InvalidInput: {0}")]
  InvalidInput(String),
  #[error(transparent)]
  Pest(#[from] Box<pest::error::Error<Rule>>),
  #[error("Expecting Number for ranged access but got {0:?}")]
  RangeNotNumber(Value),
  #[error("Could not fit index into u64")]
  IndexOutOfRange,
  #[error(transparent)]
  Unescape(unescaper::Error),
  #[error(transparent)]
  SerdeJson(#[from] serde_json::Error),
  #[error("{0}")]
  RuntimeError(String),
}
