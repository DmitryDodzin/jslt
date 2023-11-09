#![cfg_attr(test, feature(lazy_cell))]

use std::str::FromStr;

use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::error::{JsltError, Result};

pub mod error;
pub mod parser;

pub struct Jslt;

impl Jslt {
  pub fn transform<S, T>(&self, input: &T) -> Result<S>
  where
    S: for<'de> Deserialize<'de>,
    T: Serialize,
  {
    let value = self.transform_value(&serde_json::to_value(input)?)?;
    serde_json::from_value(value).map_err(Into::into)
  }

  /// Apply the jslt transformation
  pub fn transform_value(&self, input: &Value) -> Result<Value> {
    Ok(input.clone())
  }
}

impl FromStr for Jslt {
  type Err = JsltError;

  fn from_str(_: &str) -> Result<Self> {
    Ok(Jslt)
  }
}

#[cfg(test)]
mod tests {
  use std::sync::LazyLock;

  use serde_json::json;

  static BASIC_INPUT: LazyLock<Value> = LazyLock::new(|| {
    json!({
      "menu": {
        "popup": {
          "menuitem": [
           {
              "value": "Open",
              "onclick": "OpenDoc()"
            },
            {
              "value": "Close",
              "onclick": "CloseDoc()"
            }
          ]
        }
      }
    })
  });

  static BASIC_OUTPUT: LazyLock<Value> = LazyLock::new(|| {
    json!({
      "result" : {
        "Open" : "OpenDoc()",
        "Close" : "CloseDoc()"
      }
    })
  });

  use super::*;

  #[test]
  #[ignore]
  fn basic() -> Result<()> {
    let jslt: Jslt = r#"
    {
      "result" : {
        "Open" : .menu.popup.menuitem[0].onclick,
        "Close" : .menu.popup.menuitem[1].onclick
      }
    }
    "#
    .parse()?;

    let output = jslt.transform_value(&BASIC_INPUT)?;

    assert_eq!(&output, &*BASIC_OUTPUT);

    Ok(())
  }
}
