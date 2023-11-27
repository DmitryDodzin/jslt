use pest::iterators::Pairs;
use serde_json::Value;

use crate::{
  context::Context,
  error::{JsltError, Result},
  expect_inner,
  parser::{
    builder::ExprBuilder,
    value::{accessor::AccessorBuilder, array::ArrayBuilder, object::ObjectBuilder},
    FromParis, Rule,
  },
  Transform,
};

pub mod accessor;
pub mod array;
pub mod object;

#[derive(Debug)]
pub struct BooleanBuilder(bool);

impl FromParis for BooleanBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let pair = pairs.next().ok_or(JsltError::UnexpectedInput(
      Rule::Boolean,
      pairs.as_str().to_owned(),
    ))?;

    let rule = pair.as_rule();

    if matches!(rule, Rule::Boolean) {
      Ok(BooleanBuilder(
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

impl Transform for BooleanBuilder {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::Bool(self.0))
  }
}

#[derive(Debug)]
pub struct NullBuilder;

impl Transform for NullBuilder {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::Null)
  }
}

#[derive(Debug)]
pub struct NumberBuilder(serde_json::Number);

impl FromParis for NumberBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let pair = pairs.next().ok_or(JsltError::UnexpectedInput(
      Rule::Number,
      pairs.as_str().to_owned(),
    ))?;

    let rule = pair.as_rule();

    if matches!(rule, Rule::Number) {
      Ok(NumberBuilder(
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

impl Transform for NumberBuilder {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::Number(self.0.clone()))
  }
}

#[derive(Debug)]
pub struct ScopeBuilder(Box<ExprBuilder>);

impl FromParis for ScopeBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::Scope)?;

    ExprBuilder::from_pairs(&mut pairs)
      .map(Box::new)
      .map(ScopeBuilder)
  }
}

impl Transform for ScopeBuilder {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    self.0.transform_value(context, input)
  }
}

#[derive(Debug)]
pub struct StringBuilder(String);

impl FromParis for StringBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::String)?;

    let inner = pairs.next().ok_or(JsltError::UnexpectedInput(
      Rule::String,
      pairs.as_str().to_owned(),
    ))?;

    let rule = inner.as_rule();

    if matches!(rule, Rule::Inner) {
      Ok(StringBuilder(inner.as_str().to_owned()))
    } else {
      Err(JsltError::UnexpectedInput(rule, inner.as_str().to_owned()))
    }
  }
}

impl Transform for StringBuilder {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::String(self.0.clone()))
  }
}

#[derive(Debug)]
pub enum ValueBuilder {
  Accessor(AccessorBuilder),
  Array(ArrayBuilder),
  Boolean(BooleanBuilder),
  Null(NullBuilder),
  Number(NumberBuilder),
  Object(ObjectBuilder),
  Scope(ScopeBuilder),
  String(StringBuilder),
}

impl Transform for ValueBuilder {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    match self {
      ValueBuilder::Accessor(accessor) => accessor.transform_value(context, input),
      ValueBuilder::Array(array) => array.transform_value(context, input),
      ValueBuilder::Boolean(boolean) => boolean.transform_value(context, input),
      ValueBuilder::Null(null) => null.transform_value(context, input),
      ValueBuilder::Number(number) => number.transform_value(context, input),
      ValueBuilder::Object(object) => object.transform_value(context, input),
      ValueBuilder::Scope(scope) => scope.transform_value(context, input),
      ValueBuilder::String(string) => string.transform_value(context, input),
    }
  }
}

impl FromParis for ValueBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    for pair in pairs {
      return match pair.as_rule() {
        Rule::Accessor => {
          AccessorBuilder::from_pairs(&mut Pairs::single(pair)).map(ValueBuilder::Accessor)
        }
        Rule::Array => ArrayBuilder::from_pairs(&mut Pairs::single(pair)).map(ValueBuilder::Array),
        Rule::Boolean => {
          BooleanBuilder::from_pairs(&mut Pairs::single(pair)).map(ValueBuilder::Boolean)
        }
        Rule::COMMENT => continue,
        Rule::Null => Ok(ValueBuilder::Null(NullBuilder)),
        Rule::Number => {
          NumberBuilder::from_pairs(&mut Pairs::single(pair)).map(ValueBuilder::Number)
        }
        Rule::Object => {
          ObjectBuilder::from_pairs(&mut Pairs::single(pair)).map(ValueBuilder::Object)
        }
        Rule::Scope => ScopeBuilder::from_pairs(&mut Pairs::single(pair)).map(ValueBuilder::Scope),
        Rule::String => {
          StringBuilder::from_pairs(&mut Pairs::single(pair)).map(ValueBuilder::String)
        }
        rule => Err(JsltError::UnexpectedInput(rule, pair.as_str().to_owned())),
      };
    }

    Err(JsltError::UnexpectedInput(Rule::EOI, "EOI".to_owned()))
  }
}
