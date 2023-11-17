#![cfg_attr(test, feature(lazy_cell))]
#![doc = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md"))]

use std::str::FromStr;

use pest::Parser;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::{
  error::{JsltError, Result},
  parser::{FromParis, JsltBuilder, JsltParser, Rule, Transform},
};

pub mod error;
pub mod parser;

#[derive(Debug)]
pub struct Jslt {
  builder: JsltBuilder,
}

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
    self.builder.transform_value(input)
  }
}

impl FromStr for Jslt {
  type Err = JsltError;

  fn from_str(value: &str) -> Result<Self> {
    let mut pairs = JsltParser::parse(Rule::Jslb, value).map_err(Box::new)?;
    let builder = JsltBuilder::from_pairs(&mut pairs)?;

    Ok(Jslt { builder })
  }
}

#[cfg(test)]
mod tests {
  use std::{ops::Deref, sync::LazyLock};

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
      },
      "data": [
        [1, 2, 3, 4, 5],
        [6, 7, 8, 9, 10]
      ]
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

    assert_eq!(&output, BASIC_OUTPUT.deref());

    Ok(())
  }

  #[test]
  fn object_for() -> Result<()> {
    let jslt: Jslt = r#"
    {
      "result" : {for (.menu.popup.menuitem)
        .value : .onclick
      }
    }
    "#
    .parse()?;

    let output = jslt.transform_value(&BASIC_INPUT)?;

    assert_eq!(&output, BASIC_OUTPUT.deref());

    Ok(())
  }

  #[test]
  fn nested_data() -> Result<()> {
    let jslt: Jslt = r#"
    {
      "result" : [
        .data[0][0],
        .data[1][0]
      ]
    }
    "#
    .parse()?;

    let output = jslt.transform_value(&BASIC_INPUT)?;

    assert_eq!(output, json!({ "result": [1, 6] }));

    Ok(())
  }

  #[test]
  fn array_range() -> Result<()> {
    let jslt: Jslt = r#"
    {
      "result" : .data[0][0:3]
    }
    "#
    .parse()?;

    let output = jslt.transform_value(&BASIC_INPUT)?;

    assert_eq!(output, json!({ "result": [1, 2, 3] }));

    Ok(())
  }

  #[test]
  fn array_for_range() -> Result<()> {
    let jslt: Jslt = r#"
    {
      "result" : [ 
        for ( .data ) 
          { "values": .[2:4], "active": true, "deletedAt": null }
      ]
    }
    "#
    .parse()?;

    let output = jslt.transform_value(&BASIC_INPUT)?;

    assert_eq!(
      output,
      json!({
        "result": [{
          "active": true,
          "values": [3, 4],
          "deletedAt": Value::Null
        }, {
          "active": true,
          "values": [8, 9],
          "deletedAt": Value::Null
        }]
      })
    );

    Ok(())
  }
}
