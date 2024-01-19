use alloc::{borrow::ToOwned, boxed::Box, string::String};

use pest::iterators::Pairs;
use serde_json::Value;

use crate::{
  context::Context,
  error::{JsltError, Result},
  expect_inner,
  parser::{
    builder::ExprParser,
    value::{accessor::AccessorParser, array::ArrayParser, object::ObjectParser},
    FromPairs, Rule,
  },
  Transform,
};

pub mod accessor;
pub mod array;
pub mod object;

#[derive(Debug)]
pub struct BooleanParser(bool);

impl FromPairs for BooleanParser {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let pair = pairs.next().ok_or(JsltError::UnexpectedInput(
      Rule::Boolean,
      pairs.as_str().to_owned(),
    ))?;

    let rule = pair.as_rule();

    if matches!(rule, Rule::Boolean) {
      Ok(BooleanParser(
        pair
          .as_str()
          .parse()
          .map_err(|_| JsltError::UnexpectedContent(Rule::Boolean))?,
      ))
    } else {
      Err(JsltError::UnexpectedInput(rule, pair.as_str().to_owned()))
    }
  }
}

impl Transform for BooleanParser {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::Bool(self.0))
  }
}

#[derive(Debug)]
pub struct NullParser;

impl Transform for NullParser {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::Null)
  }
}

#[derive(Debug)]
pub struct NumberParser(serde_json::Number);

impl FromPairs for NumberParser {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let pair = pairs.next().ok_or(JsltError::UnexpectedInput(
      Rule::Number,
      pairs.as_str().to_owned(),
    ))?;

    let rule = pair.as_rule();

    if matches!(rule, Rule::Number) {
      Ok(NumberParser(
        pair
          .as_str()
          .parse()
          .map_err(|_| JsltError::UnexpectedContent(Rule::Number))?,
      ))
    } else {
      Err(JsltError::UnexpectedInput(rule, pair.as_str().to_owned()))
    }
  }
}

impl Transform for NumberParser {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::Number(self.0.clone()))
  }
}

#[derive(Debug)]
pub struct ScopeParser(Box<ExprParser>);

impl FromPairs for ScopeParser {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::Scope)?;

    ExprParser::from_pairs(&mut pairs)
      .map(Box::new)
      .map(ScopeParser)
  }
}

impl Transform for ScopeParser {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    self.0.transform_value(context, input)
  }
}

#[derive(Debug)]
pub struct StringParser(String);

impl FromPairs for StringParser {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::String)?;

    let inner = pairs.next().ok_or(JsltError::UnexpectedInput(
      Rule::String,
      pairs.as_str().to_owned(),
    ))?;

    let rule = inner.as_rule();

    if matches!(rule, Rule::Inner) {
      Ok(StringParser(inner.as_str().to_owned()))
    } else {
      Err(JsltError::UnexpectedInput(rule, inner.as_str().to_owned()))
    }
  }
}

impl Transform for StringParser {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::String(self.0.clone()))
  }
}

#[derive(Debug)]
pub enum ValueParser {
  Accessor(AccessorParser),
  Array(ArrayParser),
  Boolean(BooleanParser),
  Null(NullParser),
  Number(NumberParser),
  Object(ObjectParser),
  Scope(ScopeParser),
  String(StringParser),
  Variable(VariableParser),
}

impl FromPairs for ValueParser {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    for pair in pairs {
      return match pair.as_rule() {
        Rule::Accessor => {
          AccessorParser::from_pairs(&mut Pairs::single(pair)).map(ValueParser::Accessor)
        }
        Rule::Array => ArrayParser::from_pairs(&mut Pairs::single(pair)).map(ValueParser::Array),
        Rule::Boolean => {
          BooleanParser::from_pairs(&mut Pairs::single(pair)).map(ValueParser::Boolean)
        }
        Rule::COMMENT => continue,
        Rule::Null => Ok(ValueParser::Null(NullParser)),
        Rule::Number => NumberParser::from_pairs(&mut Pairs::single(pair)).map(ValueParser::Number),
        Rule::Object => ObjectParser::from_pairs(&mut Pairs::single(pair)).map(ValueParser::Object),
        Rule::Scope => ScopeParser::from_pairs(&mut Pairs::single(pair)).map(ValueParser::Scope),
        Rule::String => StringParser::from_pairs(&mut Pairs::single(pair)).map(ValueParser::String),
        Rule::Variable => {
          VariableParser::from_pairs(&mut Pairs::single(pair)).map(ValueParser::Variable)
        }
        rule => Err(JsltError::UnexpectedInput(rule, pair.as_str().to_owned())),
      };
    }

    Err(JsltError::UnexpectedInput(Rule::EOI, "EOI".to_owned()))
  }
}

impl Transform for ValueParser {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    match self {
      ValueParser::Accessor(accessor) => accessor.transform_value(context, input),
      ValueParser::Array(array) => array.transform_value(context, input),
      ValueParser::Boolean(boolean) => boolean.transform_value(context, input),
      ValueParser::Null(null) => null.transform_value(context, input),
      ValueParser::Number(number) => number.transform_value(context, input),
      ValueParser::Object(object) => object.transform_value(context, input),
      ValueParser::Scope(scope) => scope.transform_value(context, input),
      ValueParser::String(string) => string.transform_value(context, input),
      ValueParser::Variable(variable) => variable.transform_value(context, input),
    }
  }
}

#[derive(Debug)]
pub struct VariableParser(String);

impl FromPairs for VariableParser {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let pairs = expect_inner!(pairs, Rule::Variable)?;

    Ok(VariableParser(pairs.as_str().to_owned()))
  }
}

impl Transform for VariableParser {
  fn transform_value(&self, context: Context<'_>, _: &Value) -> Result<Value> {
    Ok(context.variables[&self.0].clone())
  }
}
