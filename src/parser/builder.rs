use pest::iterators::Pairs;
use serde_json::Value;

use crate::{
  context::Context,
  error::{JsltError, Result},
  expect_inner,
  parser::{value::ValueBuilder, FromParis, Rule},
  Transform,
};

#[derive(Debug)]
pub enum ExprBuilder {
  Value(ValueBuilder),
  FunctionCall(FunctionCallBuilder),
}

impl Transform for ExprBuilder {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    match self {
      ExprBuilder::Value(value) => value.transform_value(context, input),
      ExprBuilder::FunctionCall(fcall) => fcall.transform_value(context, input),
    }
  }
}

impl FromParis for ExprBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    while let Some(pair) = pairs.peek() {
      return match pair.as_rule() {
        Rule::COMMENT => {
          let _ = pairs.next();
          continue;
        }
        Rule::FunctionCall => FunctionCallBuilder::from_pairs(pairs).map(ExprBuilder::FunctionCall),
        Rule::OperatorExpr => {
          println!("{pairs:#?}");

          todo!()
        }
        _ => ValueBuilder::from_pairs(pairs).map(ExprBuilder::Value),
      };
    }

    Err(JsltError::UnexpectedInput(Rule::EOI, "EOI".to_owned()))
  }
}

#[derive(Debug)]
pub struct ForBuilder<B> {
  pub(super) source: Box<ExprBuilder>,

  pub(super) output: Box<B>,
}

impl<B> FromParis for ForBuilder<B>
where
  B: FromParis,
{
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let source = ExprBuilder::from_pairs(pairs)?;

    let output = B::from_pairs(pairs)?;

    Ok(ForBuilder {
      source: Box::new(source),
      output: Box::new(output),
    })
  }
}

#[derive(Debug)]
pub struct FunctionCallBuilder {
  name: String,
  arguments: Vec<ExprBuilder>,
}

impl FromParis for FunctionCallBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::FunctionCall)?;

    let name = pairs.next().ok_or(JsltError::UnexpectedEnd)?;

    let arguments = pairs
      .map(|pair| ExprBuilder::from_pairs(&mut Pairs::single(pair)))
      .collect::<Result<_>>()?;

    Ok(FunctionCallBuilder {
      name: name.as_str().to_owned(),
      arguments,
    })
  }
}

impl Transform for FunctionCallBuilder {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    let function = context
      .functions
      .get(&self.name)
      .ok_or_else(|| JsltError::Unknown(format!("Unknown Functuion: {}", self.name)))?;

    function.call(
      &self
        .arguments
        .iter()
        .map(|arg| arg.transform_value(context.clone(), input))
        .collect::<Result<Vec<_>>>()?,
    )
  }
}
