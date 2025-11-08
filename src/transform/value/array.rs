use std::{fmt, fmt::Write as _};

use jslt_macro::expect_inner;
use pest::iterators::Pairs;
use serde_json::Value;

use crate::{
  context::{Context, builtins},
  error::Result,
  format,
  parser::{FromPairs, Rule},
  transform::{
    Transform,
    expr::{ExprTransformer, ForTransformer},
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
            .push(ArrayTransformerInner::For(ArrayForTransformer::from_pairs(
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
        ArrayTransformerInner::For(ArrayForTransformer {
          source,
          condition,
          output,
        }) => {
          let source = source.transform_value(Context::Borrowed(&context), input)?;

          for input in ArrayForTransformer::source_iterator_extract(&source)? {
            if let Some(condition) = condition
              && !builtins::boolean_cast(
                &condition.transform_value(Context::Borrowed(&context), &input)?,
              )
            {
              continue;
            }

            items.push(output.transform_value(Context::Borrowed(&context), &input)?);
          }
        }
      }
    }

    Ok(Value::Array(items))
  }
}

impl format::Display for ArrayTransformer {
  fn fmt(&self, f: &mut format::Formatter<'_>) -> fmt::Result {
    if self.inner.is_empty() {
      f.write_str("[]")
    } else {
      f.write_str("[\n")?;

      let last_item_index = self.inner.len() - 1;
      let mut slot = None;
      let mut state = Default::default();

      let mut writer = format::PadAdapter::wrap(f, &mut slot, &mut state);

      for (index, item) in self.inner.iter().enumerate() {
        format::Display::fmt(item, &mut writer)?;

        if index != last_item_index {
          writer.write_str(",\n")?;
        } else {
          writer.write_str("\n")?;
        }
      }

      f.write_str("]")?;

      Ok(())
    }
  }
}

#[derive(Debug)]
pub enum ArrayTransformerInner {
  Item(ExprTransformer),
  For(ArrayForTransformer),
}

impl format::Display for ArrayTransformerInner {
  fn fmt(&self, f: &mut format::Formatter<'_>) -> fmt::Result {
    match self {
      ArrayTransformerInner::Item(value) => format::Display::fmt(value, f),
      ArrayTransformerInner::For(array_for) => format::Display::fmt(array_for, f),
    }
  }
}

pub type ArrayForTransformer = ForTransformer<ExprTransformer>;
