use pest::iterators::Pairs;
use serde_json::Value;

use crate::{
  context::Context,
  error::{JsltError, Result},
  expect_inner,
  parser::{FromPairs, Rule},
  Transform,
};

#[derive(Debug)]
pub struct AccessorBuilder {
  ident: String,
  keys: Vec<KeyAccessorBuilder>,
  nested: Option<Box<AccessorBuilder>>,
}

impl FromPairs for AccessorBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let pairs = expect_inner!(pairs, Rule::Accessor)?;

    let mut ident = None;
    let mut nested = None;
    let mut keys = Vec::new();

    for pair in pairs {
      match pair.as_rule() {
        Rule::Ident => ident = Some(pair.as_str()),
        Rule::KeyAccessor => {
          let inner = pair
            .into_inner()
            .next()
            .ok_or(JsltError::UnexpectedContent(Rule::KeyAccessor))?;

          match inner.as_rule() {
            Rule::Number | Rule::String => {
              keys.push(KeyAccessorBuilder::Index(inner.as_str().parse()?));
            }
            Rule::RangeAccessor => {
              let mut inner = inner.into_inner();

              let from = inner
                .next()
                .ok_or(JsltError::UnexpectedContent(Rule::RangeAccessor))?
                .as_str()
                .parse()?;

              let to = inner
                .next()
                .ok_or(JsltError::UnexpectedContent(Rule::RangeAccessor))?
                .as_str()
                .parse()?;

              keys.push(KeyAccessorBuilder::Range { from, to });
            }
            _ => return Err(JsltError::UnexpectedContent(Rule::KeyAccessor)),
          }
        }
        Rule::Accessor => {
          nested = Some(Box::new(AccessorBuilder::from_pairs(&mut Pairs::single(
            pair,
          ))?));
        }
        _ => break,
      }
    }

    let ident = ident.unwrap_or("").to_owned();

    Ok(AccessorBuilder {
      ident,
      keys,
      nested,
    })
  }
}

impl Transform for AccessorBuilder {
  #[allow(clippy::only_used_in_recursion)]
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    let mut value = (!self.ident.is_empty())
      .then(|| &input[&self.ident])
      .unwrap_or(input);

    let mut temp_value_store = None;

    for key in &self.keys {
      let next_value = match key {
        KeyAccessorBuilder::Index(Value::String(str_key)) => &value[str_key],
        KeyAccessorBuilder::Index(Value::Number(num_key)) => num_key
          .as_u64()
          .map(|index| &value[index as usize])
          .ok_or(JsltError::IndexOutOfRange)?,
        KeyAccessorBuilder::Range { from, to } => {
          let from = from
            .as_u64()
            .ok_or(JsltError::RangeNotNumber(from.clone()))?;

          let to = to.as_u64().ok_or(JsltError::RangeNotNumber(to.clone()))?;

          temp_value_store.replace(Value::Array(
            (from..to)
              .map(|index| value[index as usize].clone())
              .collect::<Vec<_>>(),
          ));

          temp_value_store.as_ref().unwrap()
        }
        _ => return Err(JsltError::IndexOutOfRange),
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
pub enum KeyAccessorBuilder {
  Index(Value),
  Range { from: Value, to: Value },
}
