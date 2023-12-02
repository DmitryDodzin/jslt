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
  And,
  Or,
  Gt,
  Gte,
  Lt,
  Lte,
  Equal,
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
    impl_operator_parse!(pairs, And);
    impl_operator_parse!(pairs, Or);
    impl_operator_parse!(pairs, Gt);
    impl_operator_parse!(pairs, Gte);
    impl_operator_parse!(pairs, Lt);
    impl_operator_parse!(pairs, Lte);
    impl_operator_parse!(pairs, Equal);
    impl_operator_parse!(pairs, Add);
    impl_operator_parse!(pairs, Sub);
    impl_operator_parse!(pairs, Mul);
    impl_operator_parse!(pairs, Div);

    Err(JsltError::InvalidInput(format!(
      "Could not evaluate the expession {pairs:#?}",
    )))
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
    let left = self.lhs.transform_value(context.clone(), input)?;
    let right = self.rhs.transform_value(context, input)?;

    match self.operator {
      OperatorBuilder::Add => match (&left, &right) {
        (Value::Number(left), Value::Number(right)) if left.is_u64() && right.is_u64() => {
          Ok(Value::Number(
            (left.as_u64().expect("Should be u64") + right.as_u64().expect("Should be u64")).into(),
          ))
        }
        (Value::Number(left), Value::Number(right)) if left.is_i64() && right.is_i64() => {
          Ok(Value::Number(
            (left.as_i64().expect("Should be i64") + right.as_i64().expect("Should be i64")).into(),
          ))
        }
        (Value::Number(left), Value::Number(right)) => Ok(
          (left.as_f64().expect("Should be f64") + right.as_f64().expect("Should be f64")).into(),
        ),
        (Value::String(left), Value::String(right)) => Ok(Value::String(format!("{left}{right}"))),
        _ => Err(JsltError::InvalidInput(format!(
          "Add (\"+\") operator must be 2 numbers or strings (got \"{left} + {right}\")"
        ))),
      },
      OperatorBuilder::Sub => match (&left, &right) {
        (Value::Number(left), Value::Number(right)) if left.is_u64() && right.is_u64() => {
          Ok(Value::Number(
            (left.as_u64().expect("Should be u64") - right.as_u64().expect("Should be u64")).into(),
          ))
        }
        (Value::Number(left), Value::Number(right)) if left.is_i64() && right.is_i64() => {
          Ok(Value::Number(
            (left.as_i64().expect("Should be i64") - right.as_i64().expect("Should be i64")).into(),
          ))
        }
        (Value::Number(left), Value::Number(right)) => Ok(
          (left.as_f64().expect("Should be f64") - right.as_f64().expect("Should be f64")).into(),
        ),
        _ => Err(JsltError::InvalidInput(format!(
          "Sub (\"-\") operator must be 2 numbers (got \"{left} - {right}\")"
        ))),
      },
      OperatorBuilder::Mul => match (&left, &right) {
        (Value::Number(left), Value::Number(right)) if left.is_u64() && right.is_u64() => {
          Ok(Value::Number(
            (left.as_u64().expect("Should be u64") * right.as_u64().expect("Should be u64")).into(),
          ))
        }
        (Value::Number(left), Value::Number(right)) if left.is_i64() && right.is_i64() => {
          Ok(Value::Number(
            (left.as_i64().expect("Should be i64") * right.as_i64().expect("Should be i64")).into(),
          ))
        }
        (Value::Number(left), Value::Number(right)) => Ok(
          (left.as_f64().expect("Should be f64") * right.as_f64().expect("Should be f64")).into(),
        ),
        _ => Err(JsltError::InvalidInput(format!(
          "Mul (\"*\") operator must be 2 numbers (got \"{left} * {right}\")"
        ))),
      },
      OperatorBuilder::Div => match (&left, &right) {
        (Value::Number(left), Value::Number(right)) if left.is_u64() && right.is_u64() => {
          Ok(Value::Number(
            (left.as_u64().expect("Should be u64") / right.as_u64().expect("Should be u64")).into(),
          ))
        }
        (Value::Number(left), Value::Number(right)) if left.is_i64() && right.is_i64() => {
          Ok(Value::Number(
            (left.as_i64().expect("Should be i64") / right.as_i64().expect("Should be i64")).into(),
          ))
        }
        (Value::Number(left), Value::Number(right)) => Ok(
          (left.as_f64().expect("Should be f64") / right.as_f64().expect("Should be f64")).into(),
        ),
        _ => Err(JsltError::InvalidInput(format!(
          "Div (\"/\") operator must be 2 numbers (got \"{left} / {right}\")"
        ))),
      },
      OperatorBuilder::And => match (&left, &right) {
        (Value::Bool(true), Value::Bool(true)) => Ok(Value::Bool(true)),
        (Value::Bool(_) | Value::Null, Value::Bool(_) | Value::Null) => Ok(Value::Bool(false)),
        _ => Err(JsltError::InvalidInput(format!(
          "And (\"and\") operator must be 2 booleans (got \"{left} and {right}\")"
        ))),
      },
      OperatorBuilder::Or => match (&left, &right) {
        (Value::Bool(_) | Value::Null, Value::Bool(true))
        | (Value::Bool(true), Value::Bool(_) | Value::Null) => Ok(Value::Bool(true)),
        (Value::Bool(_) | Value::Null, Value::Bool(_) | Value::Null) => Ok(Value::Bool(false)),
        _ => Err(JsltError::InvalidInput(format!(
          "Or (\"/\") operator must be 2 booleans (got \"{left} or {right}\")"
        ))),
      },
      OperatorBuilder::Gt => match (&left, &right) {
        (Value::Number(left), Value::Number(right)) if left.is_u64() && right.is_u64() => {
          Ok(Value::Bool(
            left.as_u64().expect("Should be u64") > right.as_u64().expect("Should be u64"),
          ))
        }
        (Value::Number(left), Value::Number(right)) if left.is_i64() && right.is_i64() => {
          Ok(Value::Bool(
            left.as_i64().expect("Should be i64") > right.as_i64().expect("Should be i64"),
          ))
        }
        (Value::Number(left), Value::Number(right)) => Ok(Value::Bool(
          left.as_f64().expect("Should be f64") > right.as_f64().expect("Should be f64"),
        )),
        (Value::String(left), Value::String(right)) => Ok(Value::Bool(left > right)),
        _ => Err(JsltError::InvalidInput(format!(
          "GreaterThan (\">\") operator must be 2 numbers or strings (got \"{left} > {right}\")"
        ))),
      },
      OperatorBuilder::Gte => match (&left, &right) {
        (Value::Number(left), Value::Number(right)) if left.is_u64() && right.is_u64() => {
          Ok(Value::Bool(
            left.as_u64().expect("Should be u64") >= right.as_u64().expect("Should be u64"),
          ))
        }
        (Value::Number(left), Value::Number(right)) if left.is_i64() && right.is_i64() => {
          Ok(Value::Bool(
            left.as_i64().expect("Should be i64") >= right.as_i64().expect("Should be i64"),
          ))
        }
        (Value::Number(left), Value::Number(right)) => Ok(Value::Bool(
          left.as_f64().expect("Should be f64") >= right.as_f64().expect("Should be f64"),
        )),
        (Value::String(left), Value::String(right)) => Ok(Value::Bool(left >= right)),
        _ => Err(JsltError::InvalidInput(format!(
          "GreaterThan (\">\") operator must be 2 numbers or strings (got \"{left} > {right}\")"
        ))),
      },
      OperatorBuilder::Lt => match (&left, &right) {
        (Value::Number(left), Value::Number(right)) if left.is_u64() && right.is_u64() => {
          Ok(Value::Bool(
            left.as_u64().expect("Should be u64") < right.as_u64().expect("Should be u64"),
          ))
        }
        (Value::Number(left), Value::Number(right)) if left.is_i64() && right.is_i64() => {
          Ok(Value::Bool(
            left.as_i64().expect("Should be i64") < right.as_i64().expect("Should be i64"),
          ))
        }
        (Value::Number(left), Value::Number(right)) => Ok(Value::Bool(
          left.as_f64().expect("Should be f64") < right.as_f64().expect("Should be f64"),
        )),
        (Value::String(left), Value::String(right)) => Ok(Value::Bool(left < right)),
        _ => Err(JsltError::InvalidInput(format!(
          "LessThanEquals (\"<=\") operator must be 2 numbers or strings (got \"{left} < {right}\")"
        ))),
      },
      OperatorBuilder::Lte => match (&left, &right) {
        (Value::Number(left), Value::Number(right)) if left.is_u64() && right.is_u64() => {
          Ok(Value::Bool(
            left.as_u64().expect("Should be u64") <= right.as_u64().expect("Should be u64"),
          ))
        }
        (Value::Number(left), Value::Number(right)) if left.is_i64() && right.is_i64() => {
          Ok(Value::Bool(
            left.as_i64().expect("Should be i64") <= right.as_i64().expect("Should be i64"),
          ))
        }
        (Value::Number(left), Value::Number(right)) => Ok(Value::Bool(
          left.as_f64().expect("Should be f64") <= right.as_f64().expect("Should be f64"),
        )),
        (Value::String(left), Value::String(right)) => Ok(Value::Bool(left <= right)),
        _ => Err(JsltError::InvalidInput(format!(
          "LessThanEquals (\"<=\") operator must be 2 numbers or strings (got \"{left} < {right}\")"
        ))),
      },
      OperatorBuilder::Equal => Ok(Value::Bool(left == right)),
    }
  }
}

#[derive(Debug)]
pub struct ForBuilder<B> {
  pub(super) source: Box<ExprBuilder>,

  pub(super) condition: Option<ExprBuilder>,

  pub(super) output: Box<B>,
}

impl<B> FromParis for ForBuilder<B>
where
  B: FromParis,
{
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let source = ExprBuilder::from_pairs(pairs)?;

    let output = B::from_pairs(pairs)?;

    let condition = match pairs.peek() {
      Some(pair) if matches!(pair.as_rule(), Rule::IfCondition) => {
        Some(ExprBuilder::from_pairs(&mut pair.into_inner())?)
      }
      _ => None,
    };

    Ok(ForBuilder {
      source: Box::new(source),
      condition,
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
