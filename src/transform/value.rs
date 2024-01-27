use pest::iterators::Pairs;
use serde_json::Value;

use crate::{
  context::Context,
  error::{JsltError, Result},
  expect_inner,
  parser::{FromPairs, Rule},
  transform::{
    expr::ExprTransformer,
    value::{accessor::AccessorTransformer, array::ArrayTransformer, object::ObjectTransformer},
    Transform,
  },
};

pub mod accessor;
pub mod array;
pub mod object;

#[derive(Debug)]
pub struct BooleanTransformer(bool);

impl FromPairs for BooleanTransformer {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let pair = pairs.next().ok_or(JsltError::UnexpectedInput(
      Rule::Boolean,
      pairs.as_str().to_owned(),
    ))?;

    let rule = pair.as_rule();

    if matches!(rule, Rule::Boolean) {
      Ok(BooleanTransformer(
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

impl Transform for BooleanTransformer {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::Bool(self.0))
  }
}

#[derive(Debug)]
pub struct NullTransformer;

impl Transform for NullTransformer {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::Null)
  }
}

#[derive(Debug)]
pub struct NumberTransformer(serde_json::Number);

impl FromPairs for NumberTransformer {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let pair = pairs.next().ok_or(JsltError::UnexpectedInput(
      Rule::Number,
      pairs.as_str().to_owned(),
    ))?;

    let rule = pair.as_rule();

    if matches!(rule, Rule::Number) {
      Ok(NumberTransformer(
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

impl Transform for NumberTransformer {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::Number(self.0.clone()))
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

#[derive(Debug)]
pub struct StringTransformer(String);

impl FromPairs for StringTransformer {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::String)?;

    let inner = pairs.next().ok_or(JsltError::UnexpectedInput(
      Rule::String,
      pairs.as_str().to_owned(),
    ))?;

    let rule = inner.as_rule();

    if matches!(rule, Rule::Inner) {
      Ok(StringTransformer(inner.as_str().to_owned()))
    } else {
      Err(JsltError::UnexpectedInput(rule, inner.as_str().to_owned()))
    }
  }
}

impl Transform for StringTransformer {
  fn transform_value(&self, _: Context<'_>, _: &Value) -> Result<Value> {
    Ok(Value::String(self.0.clone()))
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
    for pair in pairs {
      return match pair.as_rule() {
        Rule::Accessor => {
          AccessorTransformer::from_pairs(&mut Pairs::single(pair)).map(ValueTransformer::Accessor)
        }
        Rule::Array => {
          ArrayTransformer::from_pairs(&mut Pairs::single(pair)).map(ValueTransformer::Array)
        }
        Rule::Boolean => {
          BooleanTransformer::from_pairs(&mut Pairs::single(pair)).map(ValueTransformer::Boolean)
        }
        Rule::COMMENT => continue,
        Rule::Null => Ok(ValueTransformer::Null(NullTransformer)),
        Rule::Number => {
          NumberTransformer::from_pairs(&mut Pairs::single(pair)).map(ValueTransformer::Number)
        }
        Rule::Object => {
          ObjectTransformer::from_pairs(&mut Pairs::single(pair)).map(ValueTransformer::Object)
        }
        Rule::Scope => {
          ScopeTransformer::from_pairs(&mut Pairs::single(pair)).map(ValueTransformer::Scope)
        }
        Rule::String => {
          StringTransformer::from_pairs(&mut Pairs::single(pair)).map(ValueTransformer::String)
        }
        Rule::Variable => {
          VariableTransformer::from_pairs(&mut Pairs::single(pair)).map(ValueTransformer::Variable)
        }
        rule => Err(JsltError::UnexpectedInput(rule, pair.as_str().to_owned())),
      };
    }

    Err(JsltError::UnexpectedInput(Rule::EOI, "EOI".to_owned()))
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

#[derive(Debug)]
pub struct VariableTransformer(String);

impl FromPairs for VariableTransformer {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let pairs = expect_inner!(pairs, Rule::Variable)?;

    Ok(VariableTransformer(pairs.as_str().to_owned()))
  }
}

impl Transform for VariableTransformer {
  fn transform_value(&self, context: Context<'_>, _: &Value) -> Result<Value> {
    Ok(context.variables[&self.0].clone())
  }
}
