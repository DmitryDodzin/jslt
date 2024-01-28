use std::borrow::Cow;

use pest::iterators::Pairs;
use serde_json::Value;

use crate::{
  context::{builtins, Context},
  error::{JsltError, Result},
  expect_inner,
  parser::{FromPairs, Rule},
  transform::{
    expr::{ExprTransformer, ForTransformer},
    Transform,
  },
};

#[derive(Debug, Default)]
pub struct ArrayTransformer {
  inner: Vec<ArrayTransformerInner>,
}

impl FromPairs for ArrayTransformer {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::Array)?;

    let mut builder = ArrayTransformer::default();

    while let Some(next) = pairs.peek() {
      match next.as_rule() {
        Rule::ArrayFor => {
          builder
            .inner
            .push(ArrayTransformerInner::For(ArrayFor::from_pairs(
              &mut pairs
                .next()
                .expect("is not empty because of peek")
                .into_inner(),
            )?));
        }
        _ => builder
          .inner
          .push(ArrayTransformerInner::Item(ExprTransformer::from_pairs(
            &mut pairs,
          )?)),
      }
    }

    Ok(builder)
  }
}

impl Transform for ArrayTransformer {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    let mut items = Vec::new();

    for inner in &self.inner {
      match inner {
        ArrayTransformerInner::Item(jslt) => {
          items.push(jslt.transform_value(Context::Borrowed(&context), input)?)
        }
        ArrayTransformerInner::For(ArrayFor {
          source,
          condition,
          output,
        }) => {
          let source = source.transform_value(Context::Borrowed(&context), input)?;

          let input_iter: Box<dyn Iterator<Item = Cow<Value>>> = if source.is_object() {
            Box::new(
              source
                .as_object()
                .expect("Should be object")
                .into_iter()
                .map(|(key, value)| Cow::Owned(serde_json::json!({ "key": key, "value": value }))),
            )
          } else {
            Box::new(
              source
                .as_array()
                .expect("Should be array")
                .iter()
                .map(Cow::Borrowed),
            )
          };

          for input in input_iter {
            if let Some(condition) = condition {
              if !builtins::boolean_cast(
                &condition.transform_value(Context::Borrowed(&context), &input)?,
              ) {
                continue;
              }
            }

            items.push(output.transform_value(Context::Borrowed(&context), &input)?);
          }
        }
      }
    }

    Ok(Value::Array(items))
  }
}

#[derive(Debug)]
pub enum ArrayTransformerInner {
  Item(ExprTransformer),
  For(ArrayFor),
}

pub type ArrayFor = ForTransformer<ExprTransformer>;
