use std::{fmt, fmt::Write};

use jslt_macro::expect_inner;
use pest::iterators::Pairs;
use serde_json::Value;

use crate::{
  context::Context,
  error::{JsltError, Result},
  format,
  parser::{FromPairs, Rule},
  transform::{
    Transform,
    expr::ExprTransformer,
    value::{accessor::AccessorTransformer, array::ArrayTransformer, object::ObjectTransformer},
  },
};

pub mod accessor;
pub mod array;
pub mod object;

#[derive(Debug)]
pub struct BooleanTransformer(bool);

impl FromPairs for BooleanTransformer {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let pair = pairs.next().ok_or(JsltError::UnexpectedEnd)?;

    let rule = pair.as_rule();

    if matches!(rule, Rule::Boolean) {
      Ok(BooleanTransformer(
        pair
          .as_str()
          .parse()
          .map_err(|_| JsltError::UnexpectedContent(Rule::Boolean))?,
      ))
    } else {
      Err(JsltError::UnexpectedInput(
        Rule::Boolean,
        rule,
        pair.as_str().to_owned(),
      ))
    }
  }
}

impl Transform for BooleanTransformer {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::Bool(self.0))
  }
}

impl format::Display for BooleanTransformer {
  fn fmt(&self, f: &mut format::Formatter<'_>) -> fmt::Result {
    let BooleanTransformer(value) = self;
    write!(f, "{value}")
  }
}

#[derive(Debug)]
pub struct NullTransformer;

impl Transform for NullTransformer {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::Null)
  }
}

impl format::Display for NullTransformer {
  fn fmt(&self, f: &mut format::Formatter<'_>) -> fmt::Result {
    write!(f, "null")
  }
}

#[derive(Debug)]
pub struct NumberTransformer(serde_json::Number);

impl FromPairs for NumberTransformer {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let pair = pairs.next().ok_or(JsltError::UnexpectedEnd)?;

    let rule = pair.as_rule();

    if matches!(rule, Rule::Number) {
      Ok(NumberTransformer(
        pair
          .as_str()
          .parse()
          .map_err(|_| JsltError::UnexpectedContent(Rule::Number))?,
      ))
    } else {
      Err(JsltError::UnexpectedInput(
        Rule::Number,
        rule,
        pair.as_str().to_owned(),
      ))
    }
  }
}

impl Transform for NumberTransformer {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::Number(self.0.clone()))
  }
}

impl format::Display for NumberTransformer {
  fn fmt(&self, f: &mut format::Formatter<'_>) -> fmt::Result {
    let NumberTransformer(value) = self;
    write!(f, "{value}")
  }
}

#[derive(Debug)]
pub struct ScopeTransformer(Box<ExprTransformer>);

impl FromPairs for ScopeTransformer {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::Scope)?;

    ExprTransformer::from_pairs(&mut pairs)
      .map(Box::new)
      .map(ScopeTransformer)
  }
}

impl Transform for ScopeTransformer {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    self.0.transform_value(context, input)
  }
}

impl format::Display for ScopeTransformer {
  fn fmt(&self, f: &mut format::Formatter<'_>) -> fmt::Result {
    let ScopeTransformer(expr) = self;
    f.write_char('(')?;
    format::Display::fmt(expr.as_ref(), f)?;
    f.write_char(')')
  }
}

#[derive(Debug)]
pub struct StringTransformer(String);

impl FromPairs for StringTransformer {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::String)?;

    let inner = pairs.next().ok_or(JsltError::UnexpectedEnd)?;

    let rule = inner.as_rule();

    if matches!(rule, Rule::Inner) {
      unescaper::unescape(inner.as_str())
        .map(StringTransformer)
        .map_err(JsltError::Unescape)
    } else {
      Err(JsltError::UnexpectedInput(
        Rule::String,
        rule,
        inner.as_str().to_owned(),
      ))
    }
  }
}

impl Transform for StringTransformer {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::String(self.0.clone()))
  }
}

impl format::Display for StringTransformer {
  fn fmt(&self, f: &mut format::Formatter<'_>) -> fmt::Result {
    let StringTransformer(value) = self;
    write!(f, "{value:?}")
  }
}

#[derive(Debug)]
pub enum ValueTransformer {
  Accessor(AccessorTransformer),
  Array(ArrayTransformer),
  Boolean(BooleanTransformer),
  Null(NullTransformer),
  Number(NumberTransformer),
  Object(ObjectTransformer),
  Scope(ScopeTransformer),
  String(StringTransformer),
  Variable(VariableTransformer),
}

impl FromPairs for ValueTransformer {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    if let Some(pair) = pairs.peek() {
      return match pair.as_rule() {
        Rule::Accessor => AccessorTransformer::from_pairs(pairs).map(ValueTransformer::Accessor),
        Rule::Array => ArrayTransformer::from_pairs(pairs).map(ValueTransformer::Array),
        Rule::Boolean => BooleanTransformer::from_pairs(pairs).map(ValueTransformer::Boolean),
        Rule::Number => NumberTransformer::from_pairs(pairs).map(ValueTransformer::Number),
        Rule::Object => ObjectTransformer::from_pairs(pairs).map(ValueTransformer::Object),
        Rule::Scope => ScopeTransformer::from_pairs(pairs).map(ValueTransformer::Scope),
        Rule::String => StringTransformer::from_pairs(pairs).map(ValueTransformer::String),
        Rule::Variable => VariableTransformer::from_pairs(pairs).map(ValueTransformer::Variable),
        Rule::Null => {
          let _ = pairs.next();
          Ok(ValueTransformer::Null(NullTransformer))
        }
        rule => Err(JsltError::UnexpectedInput(
          Rule::Value,
          rule,
          pair.as_str().to_owned(),
        )),
      };
    }

    Err(JsltError::UnexpectedEnd)
  }
}

impl Transform for ValueTransformer {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    match self {
      ValueTransformer::Accessor(accessor) => accessor.transform_value(context, input),
      ValueTransformer::Array(array) => array.transform_value(context, input),
      ValueTransformer::Boolean(boolean) => boolean.transform_value(context, input),
      ValueTransformer::Null(null) => null.transform_value(context, input),
      ValueTransformer::Number(number) => number.transform_value(context, input),
      ValueTransformer::Object(object) => object.transform_value(context, input),
      ValueTransformer::Scope(scope) => scope.transform_value(context, input),
      ValueTransformer::String(string) => string.transform_value(context, input),
      ValueTransformer::Variable(variable) => variable.transform_value(context, input),
    }
  }
}

impl format::Display for ValueTransformer {
  fn fmt(&self, f: &mut format::Formatter<'_>) -> fmt::Result {
    match self {
      ValueTransformer::Accessor(accessor) => {
        format::Display::fmt(&accessor::RootAccessorDisplay(accessor), f)
      }
      ValueTransformer::Array(array) => format::Display::fmt(array, f),
      ValueTransformer::Boolean(boolean) => format::Display::fmt(boolean, f),
      ValueTransformer::Null(null) => format::Display::fmt(null, f),
      ValueTransformer::Number(number) => format::Display::fmt(number, f),
      ValueTransformer::Object(object) => format::Display::fmt(object, f),
      ValueTransformer::Scope(scope) => format::Display::fmt(scope, f),
      ValueTransformer::String(string) => format::Display::fmt(string, f),
      ValueTransformer::Variable(variable) => format::Display::fmt(variable, f),
    }
  }
}

#[derive(Debug)]
pub struct VariableTransformer(String, Option<AccessorTransformer>);

impl FromPairs for VariableTransformer {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::Variable)?;

    let ident = pairs.next().ok_or(JsltError::UnexpectedEnd)?;

    Ok(VariableTransformer(
      ident.as_str().to_owned(),
      pairs
        .next()
        .map(|pair| AccessorTransformer::from_pairs(&mut Pairs::single(pair)))
        .transpose()?,
    ))
  }
}

impl Transform for VariableTransformer {
  fn transform_value(&self, context: Context<'_>, _: &Value) -> Result<Value> {
    let value = context.variables[&self.0].clone();

    match &self.1 {
      Some(accessor) => accessor.transform_value(context, &value),
      None => Ok(value),
    }
  }
}

impl format::Display for VariableTransformer {
  fn fmt(&self, f: &mut format::Formatter<'_>) -> fmt::Result {
    let VariableTransformer(name, accessor) = self;

    write!(f, "${name}")?;

    if let Some(accessor) = accessor {
      format::Display::fmt(accessor, f)?;
    }

    Ok(())
  }
}
