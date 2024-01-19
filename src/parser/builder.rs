use alloc::{borrow::ToOwned, boxed::Box, format, string::String, sync::Arc, vec::Vec};

use pest::iterators::{Pair, Pairs};
use serde_json::Value;

use crate::{
  context::{builtins, Context, DynamicFunction, JsltFunction},
  error::{JsltError, Result},
  expect_inner,
  parser::{value::ValueParser, FromPairs, Rule},
  Transform,
};

#[derive(Debug)]
pub enum ExprParser {
  Value(ValueParser),
  IfStatement(IfStatementParser),
  OperatorExpr(OperatorExprParser),
  FunctionDef(FunctionDefParser),
  FunctionCall(FunctionCallParser),
  VariableDef(VariableDefParser),
}

impl Transform for ExprParser {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    match self {
      ExprParser::Value(value) => value.transform_value(context, input),
      ExprParser::IfStatement(ifstmt) => ifstmt.transform_value(context, input),
      ExprParser::FunctionCall(fcall) => fcall.transform_value(context, input),
      ExprParser::FunctionDef(fdef) => fdef.transform_value(context, input),
      ExprParser::OperatorExpr(oper_expr) => oper_expr.transform_value(context, input),
      ExprParser::VariableDef(variable_def) => variable_def.transform_value(context, input),
    }
  }
}

impl FromPairs for ExprParser {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    while let Some(pair) = pairs.peek() {
      return match pair.as_rule() {
        Rule::COMMENT => {
          let _ = pairs.next();
          continue;
        }
        Rule::IfStatement => IfStatementParser::from_pairs(pairs).map(ExprParser::IfStatement),
        Rule::FunctionCall => FunctionCallParser::from_pairs(pairs).map(ExprParser::FunctionCall),
        Rule::FunctionDef => FunctionDefParser::from_pairs(pairs).map(ExprParser::FunctionDef),
        Rule::OperatorExpr => OperatorExprParser::from_pairs(pairs).map(ExprParser::OperatorExpr),
        Rule::VariableDef => VariableDefParser::from_pairs(pairs).map(ExprParser::VariableDef),
        _ => ValueParser::from_pairs(pairs).map(ExprParser::Value),
      };
    }

    Err(JsltError::UnexpectedInput(Rule::EOI, "EOI".to_owned()))
  }
}

#[derive(Debug)]
pub enum OperatorParser {
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
  NotEqual,
}

#[derive(Debug)]
pub struct OperatorExprParser {
  lhs: Box<ExprParser>,
  operator: OperatorParser,
  rhs: Box<ExprParser>,
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
        Box::new(ExprParser::from_pairs(&mut Pairs::single(
          $ident.pop().expect("Should have at least one value"),
        ))?)
      } else {
        Box::new(ExprParser::OperatorExpr(
          OperatorExprParser::from_inner_vec($ident)?,
        ))
      };

      let rhs = if right.len() == 1 {
        Box::new(ExprParser::from_pairs(&mut Pairs::single(
          right.pop().expect("Should have at least one value"),
        ))?)
      } else {
        Box::new(ExprParser::OperatorExpr(
          OperatorExprParser::from_inner_vec(right)?,
        ))
      };

      return Ok(OperatorExprParser {
        lhs,
        rhs,
        operator: OperatorParser::$op,
      });
    }
  };
}

impl OperatorExprParser {
  pub fn from_inner_vec(mut pairs: Vec<Pair<Rule>>) -> Result<Self> {
    impl_operator_parse!(pairs, And);
    impl_operator_parse!(pairs, Or);
    impl_operator_parse!(pairs, Gt);
    impl_operator_parse!(pairs, Gte);
    impl_operator_parse!(pairs, Lt);
    impl_operator_parse!(pairs, Lte);
    impl_operator_parse!(pairs, Equal);
    impl_operator_parse!(pairs, NotEqual);
    impl_operator_parse!(pairs, Add);
    impl_operator_parse!(pairs, Sub);
    impl_operator_parse!(pairs, Mul);
    impl_operator_parse!(pairs, Div);

    Err(JsltError::InvalidInput(format!(
      "Could not evaluate the expession {pairs:#?}",
    )))
  }
}

impl FromPairs for OperatorExprParser {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let pairs = expect_inner!(pairs, Rule::OperatorExpr)?;

    Self::from_inner_vec(pairs.collect())
  }
}

impl Transform for OperatorExprParser {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    let left = self
      .lhs
      .transform_value(Context::Borrowed(&context), input)?;
    let right = self.rhs.transform_value(context, input)?;

    match self.operator {
      OperatorParser::Add => match (&left, &right) {
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
        (Value::Object(left), Value::Object(right)) => Ok(Value::Object(
          left
            .into_iter()
            .chain(right)
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect(),
        )),
        _ => Err(JsltError::InvalidInput(format!(
          "Add (\"+\") operator must be 2 numbers or strings (got \"{left} + {right}\")"
        ))),
      },
      OperatorParser::Sub => match (&left, &right) {
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
      OperatorParser::Mul => match (&left, &right) {
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
      OperatorParser::Div => match (&left, &right) {
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
      OperatorParser::And => match (&left, &right) {
        (Value::Bool(true), Value::Bool(true)) => Ok(Value::Bool(true)),
        (Value::Bool(_) | Value::Null, Value::Bool(_) | Value::Null) => Ok(Value::Bool(false)),
        _ => Err(JsltError::InvalidInput(format!(
          "And (\"and\") operator must be 2 booleans (got \"{left} and {right}\")"
        ))),
      },
      OperatorParser::Or => match (&left, &right) {
        (Value::Bool(_) | Value::Null, Value::Bool(true))
        | (Value::Bool(true), Value::Bool(_) | Value::Null) => Ok(Value::Bool(true)),
        (Value::Bool(_) | Value::Null, Value::Bool(_) | Value::Null) => Ok(Value::Bool(false)),
        _ => Err(JsltError::InvalidInput(format!(
          "Or (\"/\") operator must be 2 booleans (got \"{left} or {right}\")"
        ))),
      },
      OperatorParser::Gt => match (&left, &right) {
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
      OperatorParser::Gte => match (&left, &right) {
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
      OperatorParser::Lt => match (&left, &right) {
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
      OperatorParser::Lte => match (&left, &right) {
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
      OperatorParser::Equal => Ok(Value::Bool(left == right)),
      OperatorParser::NotEqual => Ok(Value::Bool(left != right)),
    }
  }
}

#[derive(Debug)]
pub struct ForParser<B> {
  pub(super) source: Box<ExprParser>,

  pub(super) condition: Option<ExprParser>,

  pub(super) output: Box<B>,
}

impl<B> FromPairs for ForParser<B>
where
  B: FromPairs,
{
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let source = ExprParser::from_pairs(pairs)?;

    let output = B::from_pairs(pairs)?;

    let condition = match pairs.peek() {
      Some(pair) if matches!(pair.as_rule(), Rule::IfCondition) => {
        Some(ExprParser::from_pairs(&mut pair.into_inner())?)
      }
      _ => None,
    };

    Ok(ForParser {
      source: Box::new(source),
      condition,
      output: Box::new(output),
    })
  }
}

#[derive(Debug)]
pub struct IfStatementParser {
  condition: Box<ExprParser>,
  value: Box<ExprParser>,
  fallback: Option<Box<ExprParser>>,
}

impl FromPairs for IfStatementParser {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::IfStatement)?;

    let mut condition_paris = expect_inner!(pairs, Rule::IfCondition)?;

    let condition = ExprParser::from_pairs(&mut condition_paris).map(Box::new)?;

    let value = ExprParser::from_pairs(&mut pairs).map(Box::new)?;

    let fallback = (pairs.len() != 0)
      .then(|| ExprParser::from_pairs(&mut pairs).map(Box::new))
      .transpose()?;

    Ok(IfStatementParser {
      condition,
      value,
      fallback,
    })
  }
}

impl Transform for IfStatementParser {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    if builtins::boolean_cast(
      &self
        .condition
        .transform_value(Context::Borrowed(&context), input)?,
    ) {
      self.value.transform_value(context, input)
    } else {
      self
        .fallback
        .as_ref()
        .map(|fallback| fallback.transform_value(context, input))
        .unwrap_or_else(|| Ok(Value::Null))
    }
  }
}

#[derive(Debug)]
pub struct FunctionCallParser {
  name: String,
  arguments: Vec<ExprParser>,
}

impl FromPairs for FunctionCallParser {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::FunctionCall)?;

    let name = pairs
      .next()
      .ok_or(JsltError::UnexpectedEnd)?
      .as_str()
      .to_owned();

    let arguments = pairs
      .map(|pair| ExprParser::from_pairs(&mut Pairs::single(pair)))
      .collect::<Result<_>>()?;

    Ok(FunctionCallParser { name, arguments })
  }
}

impl Transform for FunctionCallParser {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    let function = context
      .functions
      .get(&self.name)
      .ok_or_else(|| JsltError::Unknown(format!("Unknown Functuion: {}", self.name)))?
      .clone();

    function.call(
      Context::Borrowed(&context),
      &self
        .arguments
        .iter()
        .map(|arg| arg.transform_value(Context::Borrowed(&context), input))
        .collect::<Result<Vec<_>>>()?,
    )
  }
}

#[derive(Debug)]
pub struct FunctionDefParser {
  name: String,
  arguments: Vec<String>,
  expr: Arc<ExprParser>,
  next: Box<ExprParser>,
}

impl FromPairs for FunctionDefParser {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::FunctionDef)?;

    let name = pairs
      .next()
      .ok_or(JsltError::UnexpectedEnd)?
      .as_str()
      .to_owned();

    let mut arguments = Vec::new();

    while pairs
      .peek()
      .map(|pair| matches!(pair.as_rule(), Rule::Ident))
      .unwrap_or(false)
    {
      arguments.push(pairs.next().expect("was peeked").as_str().to_owned());
    }

    let expr = ExprParser::from_pairs(&mut pairs).map(Arc::new)?;

    let next = ExprParser::from_pairs(&mut pairs).map(Box::new)?;

    Ok(FunctionDefParser {
      name,
      arguments,
      expr,
      next,
    })
  }
}

impl Transform for FunctionDefParser {
  fn transform_value(&self, mut context: Context<'_>, input: &Value) -> Result<Value> {
    let function = DynamicFunction {
      name: self.name.clone(),
      arguments: self.arguments.clone(),
      expr: self.expr.clone(),
    };

    context
      .to_mut()
      .functions
      .insert(self.name.clone(), JsltFunction::Dynamic(function));

    self.next.transform_value(context, input)
  }
}

#[derive(Debug)]
pub struct VariableDefParser {
  name: String,
  value: Box<ExprParser>,
  next: Box<ExprParser>,
}

impl FromPairs for VariableDefParser {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::VariableDef)?;

    let name = pairs
      .next()
      .ok_or(JsltError::UnexpectedEnd)?
      .as_str()
      .to_owned();

    let value = ExprParser::from_pairs(&mut pairs).map(Box::new)?;
    let next = ExprParser::from_pairs(&mut pairs).map(Box::new)?;

    Ok(VariableDefParser { name, value, next })
  }
}

impl Transform for VariableDefParser {
  fn transform_value(&self, mut context: Context<'_>, input: &Value) -> Result<Value> {
    let name = self.name.clone();
    let value = self
      .value
      .transform_value(Context::Borrowed(&context), input)?;

    context.to_mut().variables.insert(name, value);

    self.next.transform_value(context, input)
  }
}
