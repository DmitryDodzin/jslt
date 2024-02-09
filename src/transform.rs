use serde_json::Value;

use crate::{context::Context, error::Result};

pub mod expr;
pub mod value;

pub trait Transform {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value>;
}
