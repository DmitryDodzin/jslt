#![feature(iter_intersperse)]
#![cfg_attr(test, feature(lazy_cell))]
#![cfg_attr(not(feature = "std"), no_std)]
#![doc = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md"))]

extern crate alloc;

use alloc::{borrow::Cow, boxed::Box};
use core::str::FromStr;

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
  builder: ExprParser,
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
    let mut pairs = JsltGrammar::parse(Rule::Jslt, value).map_err(Box::new)?;
    let builder = ExprParser::from_pairs(&mut pairs)?;
    let context = JsltContext::default();

    Ok(Jslt { builder, context })
  }
}

#[cfg(test)]
mod tests {
  extern crate std;
  use std::{ops::Deref, sync::LazyLock, *};

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
  fn example_flatten() -> Result<()> {
    let jslt: Jslt = include_str!("../examples/flatten.jslt").parse()?;

    let output = jslt.transform_value(&BASIC_INPUT)?;

    assert_eq!(
      output,
      json!({
        "menu_popup_menuitem": [
          {
            "value": "Open",
            "onclick": "OpenDoc()"
          },
          {
            "value": "Close",
            "onclick": "CloseDoc()"
          }
        ],
        "data": [
          [1, 2, 3, 4, 5],
          [6, 7, 8, 9, 10]
        ]
      })
    );

    Ok(())
  }

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
  #[case("\"1E+1\"", "1E+1")]
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

  #[test]
  fn function_random() -> Result<()> {
    let jslt: Jslt = "random()".parse()?;

    let value = jslt
      .transform_value(&().into())?
      .as_f64()
      .expect("Should be f64");

    assert!(
      value > 0.0 && value < 1.0,
      "random() should return value between 0.0 and 1.0, got {value}"
    );

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
  #[case("10", "2", "0")]
  #[case("10", "3", "1")]
  #[case("10", "4", "2")]
  #[case("-10", "3", "2")]
  #[case("-10", "-3", "2")]
  #[case("10", "-3", "1")]
  #[case("10", "null", "null")]
  #[case("null", "10", "null")]
  fn function_mod(
    #[case] left: Value,
    #[case] right: Value,
    #[case] expected: Value,
  ) -> Result<()> {
    let jslt: Jslt = "mod(.left, .right)".parse()?;

    let output = jslt.transform_value(&json!({ "left": left, "right": right }))?;

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
  #[case("123".into(), "\\d+".into(), "true")]
  #[case("abc123".into(), "\\d+".into(), "true")]
  #[case("abc123".into(), "^\\d+$".into(), "false")]
  fn function_test(
    #[case] left: Value,
    #[case] right: Value,
    #[case] expected: Value,
  ) -> Result<()> {
    let jslt: Jslt = "test(.left, .right)".parse()?;

    let output = jslt.transform_value(&json!({ "left": left, "right": right }))?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("1,2,3,4,5".into(), ",".into(), r#"["1", "2", "3", "4", "5"]"#)]
  #[case("1,2,3,4,5".into(), ";".into(), r#"["1,2,3,4,5"]"#)]
  #[case("null", ";".into(), "null")]
  #[case(",2".into(), ",".into(), r#"["", "2"]"#)]
  // #[case("2,".into(), ",".into(), r#"["2"]"#)] TODO: maybe add the logic
  fn function_split(
    #[case] left: Value,
    #[case] right: Value,
    #[case] expected: Value,
  ) -> Result<()> {
    let jslt: Jslt = "split(.left, .right)".parse()?;

    let output = jslt.transform_value(&json!({ "left": left, "right": right }))?;

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
  #[case(
    "\"foo\"",
    "\"2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae\""
  )]
  #[case(
    "\"42\"",
    "\"73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049\""
  )]
  #[case(
    "42",
    "\"73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049\""
  )]
  #[case("null", "null")]
  fn function_sha256_hex(#[case] input: Value, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = "sha256-hex(.)".parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case(r#"starts-with("prohibition", "pro")"#, "true")]
  #[case(r#"starts-with("prohibition", "pre")"#, "false")]
  #[case(r#"starts-with(null, "pre")"#, "false")]
  fn function_starts_with(#[case] jslt: &str, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = jslt.parse()?;

    let output = jslt.transform_value(&Value::Null)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case(r#"ends-with("prohibition", "pro")"#, "false")]
  #[case(r#"ends-with("prohibition", "ion")"#, "true")]
  #[case(r#"ends-with(null, "pre")"#, "false")]
  fn function_ends_with(#[case] jslt: &str, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = jslt.parse()?;

    let output = jslt.transform_value(&Value::Null)?;

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
  #[case("null", "\"null\"")]
  fn function_to_json(#[case] input: Value, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = "to-json(.)".parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case(r#"replace("abc def ghi", " ", "-")"#, "\"abc-def-ghi\"")]
  #[case(r#"replace("abc def ghi", "\\s+", "-")"#, "\"abc-def-ghi\"")]
  #[case(r#"replace(null, "\\s+", "-")"#, "null")]
  #[case(r#"replace("   whoah", "^\\s+", "")"#, "\"whoah\"")]
  #[case(r#"replace("abc def ghi", "[a-z]", "x")"#, "\"xxx xxx xxx\"")]
  #[case(r#"replace("abc def ghi", "[a-z]+", "x")"#, "\"x x x\"")]
  fn function_replace(#[case] jslt: &str, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = jslt.parse()?;

    let output = jslt.transform_value(&Value::Null)?;

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
  #[case("null", "null", "\"00000000-0000-0000-0000-000000000000\"")]
  fn function_uuid(
    #[case] left: Value,
    #[case] right: Value,
    #[case] expected: Value,
  ) -> Result<()> {
    let jslt: Jslt = "uuid(.left, .right)".parse()?;

    let output = jslt.transform_value(&json!({ "left": left, "right": right }))?;

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

  #[rstest]
  #[case("no", Some("<unknown>"), "Norway".into())]
  #[case("se", Some("<unknown>"), "Sweden".into())]
  #[case("dk", Some("<unknown>"), "<unknown>".into())]
  #[case("dk", None, "null")]
  fn function_get_key(
    #[case] key: &str,
    #[case] fallback: Option<&str>,
    #[case] expected: Value,
  ) -> Result<()> {
    let lut = json!({
      "no": "Norway",
      "se": "Sweden"
    });

    let jslt: Jslt = "get-key(.lut, .key, .fallback)".parse()?;

    let output = jslt.transform_value(&json!({ "lut": lut, "key": key, "fallback": fallback }))?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("null", "null")]
  #[case("[1, 2]", "[1, 2]")]
  #[case(
    r#"{"a": 1, "b": 2}"#,
    r#"
    [
      {"key" : "a", "value" : 1},
      {"key" : "b", "value" : 2}
    ]
    "#
  )]
  fn function_array(#[case] input: Value, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = "array(.)".parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("[[1,2], [3,4]]", "[1, 2, 3, 4]")]
  #[case("[1, 2, 3, 4]", "[1, 2, 3, 4]")]
  #[case("[1, [2, [3, [4, []]]]]", "[1, 2, 3, 4]")]
  #[case("null", "null")]
  fn function_flatten(#[case] input: Value, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = "flatten(.)".parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("[true, true, true]", "true")]
  #[case("[true, true, false]", "false")]
  #[case("null", "null")]
  #[case("[]", "true")]
  fn function_all(#[case] input: Value, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = "all(.)".parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("[false, false, false]", "false")]
  #[case("[false, false, true]", "true")]
  #[case("null", "null")]
  #[case("[]", "false")]
  fn function_any(#[case] input: Value, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = "any(.)".parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case(r#"["a", "b", "c"]"#, "[1, 2, 3]", r#"[["a", 1], ["b", 2], ["c", 3]]"#)]
  #[case(r#"["a", "b", "c"]"#, "null", "null")]
  #[case("null", "[1, 2, 3]", "null")]
  fn function_zip(
    #[case] left: Value,
    #[case] right: Value,
    #[case] expected: Value,
  ) -> Result<()> {
    let jslt: Jslt = "zip(.left, .right)".parse()?;

    let output = jslt.transform_value(&json!({ "left": left, "right": right }))?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("[]", "1", "-1")]
  #[case("[0, 1, 2]", "1", "1")]
  #[case("[0, 1, 2, null]", "null", "3")]
  #[case("[0, 1, 2]", "null", "-1")]
  #[case("null", "1", "null")]
  fn function_index_of(
    #[case] left: Value,
    #[case] right: Value,
    #[case] expected: Value,
  ) -> Result<()> {
    let jslt: Jslt = "index-of(.left, .right)".parse()?;

    let output = jslt.transform_value(&json!({ "left": left, "right": right }))?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[test]
  fn function_parse_url() -> Result<()> {
    let jslt: Jslt = "parse-url(\"https://www.example.com/?aa=1&aa=2&bb=&cc\")".parse()?;

    let output = jslt.transform_value(&Value::Null)?;

    assert_eq!(
      output,
      json!({
        "scheme": "https",
        "userinfo": "",
        "host": "www.example.com",
        "port": null,
        "path": "/",
        "query": "aa=1&aa=2&bb=&cc",
        "parameters": {
          "aa": ["1", "2"],
          "bb": [""],
          "cc": [""]
        },
        "fragment": null,
      })
    );

    Ok(())
  }

  #[test]
  #[cfg(feature = "std")]
  fn function_now() -> Result<()> {
    let jslt: Jslt = "now()".parse()?;

    let output = jslt.transform_value(&Value::Null)?;

    assert!(matches!(output, Value::Number(_)));

    Ok(())
  }

  #[rstest]
  #[case("1 + 2", "3")]
  #[case("1 + 2 / 2", "2")]
  #[case("2 / 2 + 1", "2")]
  #[case("1 + 2 / 2 + 1", "3")]
  #[case("3.12 * 2", "6.24")]
  #[case("\"3.12\" + \".2\"", "\"3.12.2\"")]
  #[case("true and true", "true")]
  #[case("true or false", "true")]
  #[case("false or false", "false")]
  #[case("false == false", "true")]
  #[case("1 < 3 and 4 * 2 > 5", "true")]
  #[case("1 <= 3 and 4 * 2 <= 8", "true")]
  #[case("[for ([1, 2, 3]) . if ( . > 2 )]", "[3]")]
  fn operators(#[case] jslt: &str, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = jslt.parse()?;

    let output = jslt.transform_value(&Value::Null)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case("def my_add(a, b)\n $a + $b \n my_add(1, 2)", "3")]
  #[case("def my_add(a, b)\n let foo = $a \n $foo + $b \n my_add(1, 2)", "3")]
  #[case(
    r"
      def zero_odd(num)
        if (mod($num, 2) == 0)
          $num
        else
          0

      [zero_odd(2), zero_odd(3)]
    ",
    "[2, 0]"
  )]
  fn function_def(#[case] jslt: &str, #[case] expected: Value) -> Result<()> {
    let jslt: Jslt = jslt.parse()?;

    let output = jslt.transform_value(&Value::Null)?;

    assert_eq!(output, expected);

    Ok(())
  }

  #[rstest]
  #[case(
    r#"{"foo": number(.foo), *: . / 1000}"#,
    r#"{"foo": "bar", "baz": 2000}"#,
    r#"{"foo": null, "baz": 2}"#
  )]
  fn object_spread(
    #[case] jslt: &str,
    #[case] input: Value,
    #[case] expected: Value,
  ) -> Result<()> {
    let jslt: Jslt = jslt.parse()?;

    let output = jslt.transform_value(&input)?;

    assert_eq!(output, expected);

    Ok(())
  }
}
