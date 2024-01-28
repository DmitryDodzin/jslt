use pest::iterators::Pairs;
use serde_json::Value;

use crate::{
  context::Context,
  error::{JsltError, Result},
  expect_inner,
  parser::{FromPairs, Rule},
  transform::{expr::ExprTransformer, value::VariableTransformer, Transform},
};

#[derive(Debug)]
pub struct AccessorTransformer {
  ident: String,
  keys: Vec<KeyAccessorTransformer>,
  nested: Option<Box<AccessorTransformer>>,
}

impl AccessorTransformer {
  fn index_by_value<'i>(input: &'i Value, index: &Value) -> Result<&'i Value> {
    match index {
      Value::String(str_key) => Ok(&input[str_key]),
      Value::Number(num_key) => num_key
        .as_u64()
        .map(|index| &input[index as usize])
        .ok_or(JsltError::IndexOutOfRange),
      _ => Err(JsltError::IndexOutOfRange),
    }
  }
}

impl FromPairs for AccessorTransformer {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let pairs = expect_inner!(pairs, Rule::Accessor)?;

    let mut ident = None;
    let mut nested = None;
    let mut keys = Vec::new();

    for pair in pairs {
      match pair.as_rule() {
        Rule::Ident => ident = Some(pair.as_str()),
        Rule::KeyAccessor => keys.push(KeyAccessorTransformer::from_pairs(&mut pair.into_inner())?),
        Rule::Accessor => {
          nested = Some(Box::new(AccessorTransformer::from_pairs(
            &mut Pairs::single(pair),
          )?));
        }
        _ => break,
      }
    }

    let ident = ident.unwrap_or("").to_owned();

    Ok(AccessorTransformer {
      ident,
      keys,
      nested,
    })
  }
}

impl Transform for AccessorTransformer {
  #[allow(clippy::only_used_in_recursion)]
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    let mut value = (!self.ident.is_empty())
      .then(|| &input[&self.ident])
      .unwrap_or(input);

    let mut temp_value_store = None;

    for key in &self.keys {
      let next_value = match key {
        KeyAccessorTransformer::Index(index) => AccessorTransformer::index_by_value(
          value,
          &index.transform_value(context.clone(), input)?,
        )?,
        KeyAccessorTransformer::Range { from, to } => {
          let from = from
            .as_ref()
            .map(|value| {
              let value = value.transform_value(context.clone(), input)?;

              value.as_u64().ok_or(JsltError::RangeNotNumber(value))
            })
            .transpose()?
            .unwrap_or(0);

          let to = to
            .as_ref()
            .map(|value| {
              let value = value.transform_value(context.clone(), input)?;

              value.as_u64().ok_or(JsltError::RangeNotNumber(value))
            })
            .transpose()?
            .unwrap_or_else(|| {
              value
                .as_array()
                .and_then(|array| array.len().try_into().ok())
                .unwrap_or(0)
            });

          temp_value_store.replace(Value::Array(
            (from..to)
              .map(|index| value[index as usize].clone())
              .collect::<Vec<_>>(),
          ));

          temp_value_store.as_ref().unwrap()
        }
      };

      value = next_value;
    }

    match &self.nested {
      Some(nested) => nested.transform_value(context, value),
      None => Ok(value.clone()),
    }
  }
}

#[derive(Debug)]
pub enum KeyAccessorTransformer {
  Index(ExprTransformer),
  Range {
    from: Option<ExprTransformer>,
    to: Option<ExprTransformer>,
  },
}

impl FromPairs for KeyAccessorTransformer {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let inner = pairs.peek().ok_or(JsltError::UnexpectedEnd)?;

    match inner.as_rule() {
      Rule::Number | Rule::String | Rule::Variable => Ok(KeyAccessorTransformer::Index(
        ExprTransformer::from_pairs(pairs)?,
      )),
      Rule::RangeAccessor => {
        let mut inner = pairs.next().expect("Sould be fine").into_inner();

        let kind = inner.next().ok_or(JsltError::UnexpectedEnd)?;

        match kind.as_rule() {
          Rule::FromRangeAccessor => {
            let mut inner = kind.into_inner();

            let from = ExprTransformer::from_pairs(
              &mut inner
                .next()
                .ok_or(JsltError::UnexpectedContent(Rule::RangeAccessor))
                .map(Pairs::single)?,
            )?;

            let to = inner
              .next()
              .map(|pair| ExprTransformer::from_pairs(&mut Pairs::single(pair)))
              .transpose()?;

            Ok(KeyAccessorTransformer::Range {
              from: Some(from),
              to,
            })
          }
          Rule::ToRangeAccessor => {
            let to = ExprTransformer::from_pairs(&mut kind.into_inner())?;

            Ok(KeyAccessorTransformer::Range {
              from: None,
              to: Some(to),
            })
          }
          _ => Err(JsltError::UnexpectedContent(Rule::RangeAccessor)),
        }
      }
      _ => Err(JsltError::UnexpectedContent(Rule::KeyAccessor)),
    }
  }
}
