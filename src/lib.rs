#![feature(iter_intersperse)]
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
  #[case("null", "[1, 2, 3]", "false")]
  #[case("1", "[1, 2, 3]", "true")]
  #[case("0", "[1, 2, 3]", "false")]
  #[case("\"no\"", "{\"no\" : false}", "true")]
  #[case("1", "{\"1\" : false}", "true")]
  #[case("\"ab\"", "\"abc\"", "true")]
  fn function_contains(
    #[case] left: Value,
    #[case] right: Value,
    #[case] expected: Value,
  ) -> Result<()> {
    let jslt: Jslt = "contains(.left, .right)".parse()?;

    let output = jslt.transform_value(&json!({ "left": left, "right": right }))?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("[1, 2, 3]", "3")]
  #[case("{\"1\" : 3}", "1")]
  #[case("\"abcdef\"", "6")]
  #[case("null", "null")]
  fn function_size(#[case] input: Value, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = "size(.)".parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[test]
  fn function_error() -> Result<()> {
    let jslt: Jslt = "error(.)".parse()?;

    let output = jslt.transform_value(&json!("foobar")).err();

    assert!(
      matches!(output, Some(JsltError::Unknown(ref err)) if err == "foobar"),
      "Bad Err: {output:?}"
    );

    Ok(())
  }

  #[rstest]
  #[case(".not_existing_key, .another_not_existing, 1", "1")]
  #[case("null, [], {}, \"value\"", "\"value\"")]
  fn function_fallback(#[case] arguments: &str, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = format!("fallback({arguments})").parse()?;

    let output = jslt.transform_value(&json!({}))?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("10", "1", "1")]
  #[case("\"a\"", "\"b\"", "\"a\"")]
  #[case("10", "null", "null")]
  #[case("null", "10", "null")]
  #[case("9.1232", "2003", "9.1232")]
  fn function_min(
    #[case] left: Value,
    #[case] right: Value,
    #[case] expected: Value,
  ) -> Result<()> {
    let jslt: Jslt = "min(.left, .right)".parse()?;

    let output = jslt.transform_value(&json!({ "left": left, "right": right }))?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("10", "1", "10")]
  #[case("\"a\"", "\"b\"", "\"b\"")]
  #[case("10", "null", "null")]
  #[case("null", "10", "null")]
  #[case("9.1232", "2003", "2003")]
  fn function_max(
    #[case] left: Value,
    #[case] right: Value,
    #[case] expected: Value,
  ) -> Result<()> {
    let jslt: Jslt = "max(.left, .right)".parse()?;

    let output = jslt.transform_value(&json!({ "left": left, "right": right }))?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case::number("number", "null", false)]
  #[case::number("number", "1", true)]
  #[case::number("number", "1.0", true)]
  #[case::number("number", "\"1\"", false)]
  #[case::integer("integer", "null", false)]
  #[case::integer("integer", "1", true)]
  #[case::integer("integer", "1.0", false)]
  #[case::integer("integer", "\"1\"", false)]
  #[case::decimal("decimal", "null", false)]
  #[case::decimal("decimal", "1", false)]
  #[case::decimal("decimal", "1.0", true)]
  #[case::decimal("decimal", "\"1\"", false)]
  #[case::string("string", "null", false)]
  #[case::string("string", "\"123\"", true)]
  #[case::string("string", "123", false)]
  #[case::boolean("boolean", "null", false)]
  #[case::boolean("boolean", "true", true)]
  #[case::boolean("boolean", "false", true)]
  #[case::boolean("boolean", "\"\"", false)]
  #[case::boolean("boolean", "\" \"", false)]
  #[case::object("object", "null", false)]
  #[case::object("object", "{}", true)]
  #[case::object("object", "[]", false)]
  #[case::object("object", "\"\"", false)]
  #[case::array("array", "null", false)]
  #[case::array("array", "[1, 2]", true)]
  #[case::array("array", "\"123\"", false)]
  fn function_is(#[case] kind: &str, #[case] input: Value, #[case] expected: bool) -> Result<()> {
    let jslt: Jslt = format!("is-{kind}(.)").parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("23", "23")]
  #[case("\"23\"", "23")]
  #[case("\"023\"", "23")]
  #[case("23.0", "23.0")]
  #[case("null", "null")]
  fn function_number(#[case] input: Value, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = "number(.)".parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case::round("round", "1", "1")]
  #[case::round("round", "1.0", "1")]
  #[case::round("round", "1.51", "2")]
  #[case::round("round", "null", "null")]
  #[case::floor("floor", "1", "1")]
  #[case::floor("floor", "1.0", "1")]
  #[case::floor("floor", "1.51", "1")]
  #[case::floor("floor", "null", "null")]
  #[case::ceiling("ceiling", "1", "1")]
  #[case::ceiling("ceiling", "1.0", "1")]
  #[case::ceiling("ceiling", "1.51", "2")]
  #[case::ceiling("ceiling", "null", "null")]
  fn function_roundings(
    #[case] kind: &str,
    #[case] input: Value,
    #[case] expected: Value,
  ) -> Result<()> {
    let jslt: Jslt = format!("{kind}(.)").parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("[1,2,3]", "6")]
  #[case("[1]", "1")]
  #[case("[1.0, 2.0]", "3.0")]
  #[case("[]", "0")]
  #[case("null", "null")]
  fn function_sum(#[case] input: Value, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = "sum(.)".parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("null", "\"null\"")]
  #[case("123", "\"123\"")]
  #[case("\"123\"", "\"123\"")]
  fn function_string(#[case] input: Value, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = "string(.)".parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case(r#"["a", "b", "c"]"#, "\" \"", "\"a b c\"")]
  #[case(r#"["a"]"#, "\" \"", "\"a\"")]
  #[case("null", "\"-\"", "null")]
  #[case("[1]", "\"-\"", "\"1\"")]
  fn function_join(
    #[case] left: Value,
    #[case] right: Value,
    #[case] expected: Value,
  ) -> Result<()> {
    let jslt: Jslt = "join(.left, .right)".parse()?;

    let output = jslt.transform_value(&json!({ "left": left, "right": right }))?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case::lowercase("lowercase", "\"ABCÆØÅ\"", "\"abcæøå\"")]
  #[case::lowercase("lowercase", "null", "null")]
  #[case::uppercase("uppercase", "\"abcæøå\"", "\"ABCÆØÅ\"")]
  #[case::uppercase("uppercase", "null", "null")]
  fn function_case(
    #[case] case: &str,
    #[case] input: Value,
    #[case] expected: Value,
  ) -> Result<()> {
    let jslt: Jslt = format!("{case}(.)").parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("\"[1,2]\"", "[1, 2]", None)]
  #[case("\"[1,2\"", "\"BAD\"", Some("BAD".into()))]
  #[case("null", "null", None)]
  fn function_from_json(
    #[case] input: Value,
    #[case] expected: Value,
    #[case] fallback: Option<Value>,
  ) -> Result<()> {
    let output = if let Some(fallback) = fallback {
      let jslt: Jslt = "from-json(.value, .fallback)".parse()?;
      jslt.transform_value(&json!({ "value": input, "fallback": fallback }))?
    } else {
      let jslt: Jslt = "from-json(.)".parse()?;
      jslt.transform_value(&input)?
    };

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("[1, 2]", "\"[1,2]\"")]
  #[case("1", "\"1\"")]
  #[case("\"foo\"", "\"foo\"".into())]
  #[case("null", "\"null\"")]
  fn function_to_json(#[case] input: Value, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = "to-json(.)".parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("\"  abc  \"", "\"abc\"")]
  #[case("\"abc\"", "\"abc\"")]
  #[case("\"abc \\t\\r\\n\"", "\"abc\"")]
  #[case("false", "\"false\"")]
  #[case("null", "null")]
  fn function_trim(#[case] input: Value, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = "trim(.)".parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("null", "false")]
  #[case("\"\"", "false")]
  #[case("\" \"", "true")]
  #[case("0", "false")]
  #[case("1", "true")]
  #[case("true", "true")]
  #[case("false", "false")]
  #[case("[]", "false")]
  #[case("[1]", "true")]
  fn function_boolean(#[case] input: Value, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = "boolean(.)".parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("null", "true")]
  #[case("\"\"", "true")]
  #[case("\" \"", "false")]
  #[case("0", "true")]
  #[case("1", "false")]
  #[case("true", "false")]
  #[case("false", "true")]
  #[case("[]", "true")]
  #[case("[1]", "false")]
  fn function_not(#[case] input: Value, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = "not(.)".parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }
}
