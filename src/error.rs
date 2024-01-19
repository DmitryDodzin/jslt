use alloc::{boxed::Box, fmt, string::String};

use serde_json::Value;

// use thiserror::Error;
use crate::parser::Rule;

pub type Result<T, E = JsltError> = core::result::Result<T, E>;

// #[derive(Debug, Error)]
#[derive(Debug)]
pub enum JsltError {
  // #[error("UnexpectedInput: {0:?} ({1})")]
  UnexpectedInput(Rule, String),
  // #[error("UnexpectedContent: for {0:?}")]
  UnexpectedContent(Rule),
  // #[error("UnexpectedEnd")]
  UnexpectedEnd,
  // #[error("InvalidInput: {0}")]
  InvalidInput(String),
  // #[error(transparent)]
  Pest(Box<pest::error::Error<Rule>>),
  // #[error("Expecting Number for ranged access but got {0:?}")]
  RangeNotNumber(Value),
  // #[error("Could not fit index into u64")]
  IndexOutOfRange,
  // #[error(transparent)]
  SerdeJson(serde_json::Error),
  // #[error("{0}")]
  Unknown(String),
}

impl From<Box<pest::error::Error<Rule>>> for JsltError {
  fn from(err: Box<pest::error::Error<Rule>>) -> Self {
    JsltError::Pest(err)
  }
}

impl From<serde_json::Error> for JsltError {
  fn from(err: serde_json::Error) -> Self {
    JsltError::SerdeJson(err)
  }
}

impl fmt::Display for JsltError {
  fn fmt(&self, _: &mut fmt::Formatter<'_>) -> fmt::Result {
    todo!()
  }
}
