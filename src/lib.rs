#![cfg_attr(test, feature(lazy_cell))]
#![doc = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md"))]

use std::{borrow::Cow, str::FromStr};

use pest::Parser;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::{
  context::{Context, JsltContext},
  error::{JsltError, Result},
  parser::*,
};

pub mod context;
pub mod error;
mod macros;
pub mod parser;

pub trait Transform {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value>;
}

#[derive(Debug)]
pub struct Jslt {
  builder: JsltBuilder,
  context: JsltContext,
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
    self
      .builder
      .transform_value(Cow::Borrowed(&self.context), input)
  }
}

impl FromStr for Jslt {
  type Err = JsltError;

  fn from_str(value: &str) -> Result<Self> {
    let mut pairs = JsltParser::parse(Rule::Jslb, value).map_err(Box::new)?;
    let builder = JsltBuilder::from_pairs(&mut pairs)?;
    let context = JsltContext::default();

    Ok(Jslt { builder, context })
  }
}

#[cfg(test)]
mod tests {
  use std::{ops::Deref, sync::LazyLock};

  use rstest::rstest;
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

  #[rstest]
  #[case::contains("contains(., [1, 2, 3])", "null", "false")]
  #[case::contains("contains(., [1, 2, 3])", "1", "true")]
  #[case::contains("contains(., [1, 2, 3])", "0", "false")]
  #[case::contains("contains(., {\"no\" : false})", "\"no\"", "true")]
  #[case::contains("contains(., {\"1\" : false})", "1", "true")]
  #[case::contains("contains(., \"abc\")", "\"ab\"", "true")]
  #[case::size("size(.)", "[1, 2, 3]", "3")]
  #[case::size("size(.)", "{\"1\" : 3}", "1")]
  #[case::size("size(.)", "\"abcdef\"", "6")]
  #[case::size("size(.)", "null", "null")]
  fn function_call(
    #[case] template: &str,
    #[case] input: Value,
    #[case] expected: Value,
  ) -> Result<()> {
    let jslt: Jslt = template.parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }
}
