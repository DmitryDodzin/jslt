use pest::iterators::{Pair, Pairs};
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
  OperatorExpr(OperatorExprBuilder),
  FunctionCall(FunctionCallBuilder),
}

impl Transform for ExprBuilder {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    match self {
      ExprBuilder::Value(value) => value.transform_value(context, input),
      ExprBuilder::FunctionCall(fcall) => fcall.transform_value(context, input),
      ExprBuilder::OperatorExpr(oper_expr) => oper_expr.transform_value(context, input),
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
        Rule::OperatorExpr => OperatorExprBuilder::from_pairs(pairs).map(ExprBuilder::OperatorExpr),
        _ => ValueBuilder::from_pairs(pairs).map(ExprBuilder::Value),
      };
    }

    Err(JsltError::UnexpectedInput(Rule::EOI, "EOI".to_owned()))
  }
}

#[derive(Debug)]
pub enum OperatorBuilder {
  Add,
  Sub,
  Div,
  Mul,
}

#[derive(Debug)]
pub struct OperatorExprBuilder {
  lhs: Box<ExprBuilder>,
  operator: OperatorBuilder,
  rhs: Box<ExprBuilder>,
}

macro_rules! impl_operator_parse {
  ($ident:ident, $op:ident) => {
    if let Some((index, _)) = $ident
      .iter()
      .enumerate()
      .find(|(_, pair)| matches!(pair.as_rule(), Rule::$op))
    {
      let mut right = $ident.split_off(index).split_off(1);

      let lhs = if $ident.len() == 1 {
        Box::new(ExprBuilder::from_pairs(&mut Pairs::single(
          $ident.pop().expect("Should have at least one value"),
        ))?)
      } else {
        Box::new(ExprBuilder::OperatorExpr(
          OperatorExprBuilder::from_inner_vec($ident)?,
        ))
      };

      let rhs = if right.len() == 1 {
        Box::new(ExprBuilder::from_pairs(&mut Pairs::single(
          right.pop().expect("Should have at least one value"),
        ))?)
      } else {
        Box::new(ExprBuilder::OperatorExpr(
          OperatorExprBuilder::from_inner_vec(right)?,
        ))
      };

      return Ok(OperatorExprBuilder {
        lhs,
        rhs,
        operator: OperatorBuilder::$op,
      });
    }
  };
}

impl OperatorExprBuilder {
  pub fn from_inner_vec(mut pairs: Vec<Pair<Rule>>) -> Result<Self> {
    impl_operator_parse!(pairs, Add);
    impl_operator_parse!(pairs, Sub);
    impl_operator_parse!(pairs, Mul);
    impl_operator_parse!(pairs, Div);

    unreachable!()
  }
}

impl FromParis for OperatorExprBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let pairs = expect_inner!(pairs, Rule::OperatorExpr)?;

    Self::from_inner_vec(pairs.collect())
  }
}

impl Transform for OperatorExprBuilder {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    match self.operator {
      OperatorBuilder::Add => {
        let left = self.lhs.transform_value(context.clone(), input)?;
        let right = self.rhs.transform_value(context, input)?;

        match (left, right) {
          (Value::Number(left), Value::Number(right)) if left.is_u64() && right.is_u64() => {
            Ok(Value::Number(
              (left.as_u64().expect("Should be u64") + right.as_u64().expect("Should be u64"))
                .into(),
            ))
          }
          (Value::Number(left), Value::Number(right)) if left.is_i64() && right.is_i64() => {
            Ok(Value::Number(
              (left.as_i64().expect("Should be i64") + right.as_i64().expect("Should be i64"))
                .into(),
            ))
          }
          (Value::Number(left), Value::Number(right)) => Ok(
            (left.as_f64().expect("Should be f64") + right.as_f64().expect("Should be f64")).into(),
          ),
          _ => Err(JsltError::InvalidInput(
            "Add (\"+\") operator must have an input of 2 numbers".to_string(),
          )),
        }
      }
      OperatorBuilder::Sub => {
        let left = self.lhs.transform_value(context.clone(), input)?;
        let right = self.rhs.transform_value(context, input)?;

        match (left, right) {
          (Value::Number(left), Value::Number(right)) if left.is_u64() && right.is_u64() => {
            Ok(Value::Number(
              (left.as_u64().expect("Should be u64") - right.as_u64().expect("Should be u64"))
                .into(),
            ))
          }
          (Value::Number(left), Value::Number(right)) if left.is_i64() && right.is_i64() => {
            Ok(Value::Number(
              (left.as_i64().expect("Should be i64") - right.as_i64().expect("Should be i64"))
                .into(),
            ))
          }
          (Value::Number(left), Value::Number(right)) => Ok(
            (left.as_f64().expect("Should be f64") - right.as_f64().expect("Should be f64")).into(),
          ),
          _ => Err(JsltError::InvalidInput(
            "Sub (\"-\") operator must have an input of 2 numbers".to_string(),
          )),
        }
      }
      OperatorBuilder::Mul => {
        let left = self.lhs.transform_value(context.clone(), input)?;
        let right = self.rhs.transform_value(context, input)?;

        match (left, right) {
          (Value::Number(left), Value::Number(right)) if left.is_u64() && right.is_u64() => {
            Ok(Value::Number(
              (left.as_u64().expect("Should be u64") * right.as_u64().expect("Should be u64"))
                .into(),
            ))
          }
          (Value::Number(left), Value::Number(right)) if left.is_i64() && right.is_i64() => {
            Ok(Value::Number(
              (left.as_i64().expect("Should be i64") * right.as_i64().expect("Should be i64"))
                .into(),
            ))
          }
          (Value::Number(left), Value::Number(right)) => Ok(
            (left.as_f64().expect("Should be f64") * right.as_f64().expect("Should be f64")).into(),
          ),
          _ => Err(JsltError::InvalidInput(
            "Mul (\"*\") operator must have an input of 2 numbers".to_string(),
          )),
        }
      }
      OperatorBuilder::Div => {
        let left = self.lhs.transform_value(context.clone(), input)?;
        let right = self.rhs.transform_value(context, input)?;

        match (left, right) {
          (Value::Number(left), Value::Number(right)) if left.is_u64() && right.is_u64() => {
            Ok(Value::Number(
              (left.as_u64().expect("Should be u64") / right.as_u64().expect("Should be u64"))
                .into(),
            ))
          }
          (Value::Number(left), Value::Number(right)) if left.is_i64() && right.is_i64() => {
            Ok(Value::Number(
              (left.as_i64().expect("Should be i64") / right.as_i64().expect("Should be i64"))
                .into(),
            ))
          }
          (Value::Number(left), Value::Number(right)) => Ok(
            (left.as_f64().expect("Should be f64") / right.as_f64().expect("Should be f64")).into(),
          ),
          _ => Err(JsltError::InvalidInput(
            "Div (\"/\") operator must have an input of 2 numbers".to_string(),
          )),
        }
      }
    }
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
