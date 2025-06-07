use std::{borrow::Cow, fmt, fmt::Write as _};

use jslt_macro::expect_inner;
use pest::iterators::Pairs;
use serde_json::Value;

use crate::{
  context::{Context, builtins},
  error::{JsltError, Result},
  format,
  parser::{FromPairs, Rule},
  transform::{Transform, expr::ExprTransformer},
};

#[derive(Debug)]
pub struct AccessorTransformer {
  ident: String,
  keys: Vec<KeyAccessorTransformer>,
  nested: Option<Box<AccessorTransformer>>,
}

impl AccessorTransformer {
  fn index_by_range<'i>(
    input: &'i Value,
    from: Option<&Value>,
    to: Option<&Value>,
  ) -> Result<Cow<'i, Value>> {
    let Some(length) = builtins::length_impl(input) else {
      return Ok(Cow::Owned(Value::Null));
    };

    let from = match from {
      None | Some(Value::Null) => 0,
      Some(index) if index.is_u64() => index.as_u64().expect("should be u64") as usize,
      Some(back_index) if back_index.is_i64() => {
        length - back_index.as_i64().expect("should be i64").unsigned_abs() as usize
      }
      Some(value) => return Err(JsltError::RangeNotNumber(value.clone())),
    }
    .min(length);

    let to = match to {
      None | Some(Value::Null) => length,
      Some(index) if index.is_u64() => index.as_u64().expect("should be u64") as usize,
      Some(back_index) if back_index.is_i64() => {
        length - back_index.as_i64().expect("should be i64").unsigned_abs() as usize
      }
      Some(value) => return Err(JsltError::RangeNotNumber(value.clone())),
    }
    .min(length);

    Ok(match input {
      Value::Array(array) => Cow::Owned(array[from..to].into()),
      Value::String(string) => Cow::Owned(string[from..to].into()),
      _ => unreachable!(),
    })
  }

  fn index_by_value<'i>(input: &'i Value, index: &Value) -> Result<Cow<'i, Value>> {
    let Some(length) = builtins::length_impl(input) else {
      return Ok(Cow::Owned(Value::Null));
    };

    match (input, index) {
      (Value::String(str_input), Value::Number(num_key)) => num_key
        .as_u64()
        .map(|index| {
          str_input
            .chars()
            .nth(index as usize)
            .map(|val| Cow::Owned(Value::String(val.into())))
            .unwrap_or_default()
        })
        .ok_or(JsltError::IndexOutOfRange),
      (input, Value::String(str_key)) => Ok(Cow::Borrowed(&input[str_key])),
      (input, Value::Number(num_key)) if num_key.is_u64() => {
        let index = num_key.as_u64().expect("should be u64") as usize;
        Ok(Cow::Borrowed(&input[index]))
      }
      (input, Value::Number(num_key)) if num_key.is_i64() => {
        let index =
          (length - num_key.as_i64().expect("should be i64").unsigned_abs() as usize).min(length);

        Ok(Cow::Borrowed(&input[index]))
      }
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
        Rule::String => ident = Some(pair.into_inner().as_str()),
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
        KeyAccessorTransformer::Index(index) => {
          let result = AccessorTransformer::index_by_value(
            value,
            &index.transform_value(context.clone(), input)?,
          )?;

          match result {
            Cow::Borrowed(value) => value,
            Cow::Owned(owned) => {
              temp_value_store.replace(owned);
              temp_value_store.as_ref().unwrap()
            }
          }
        }
        KeyAccessorTransformer::Range { from, to } => {
          let from = from
            .as_ref()
            .map(|value| value.transform_value(context.clone(), input))
            .transpose()?;

          let to = to
            .as_ref()
            .map(|value| value.transform_value(context.clone(), input))
            .transpose()?;

          let result = AccessorTransformer::index_by_range(value, from.as_ref(), to.as_ref())?;

          match result {
            Cow::Borrowed(value) => value,
            Cow::Owned(owned) => {
              temp_value_store.replace(owned);
              temp_value_store.as_ref().unwrap()
            }
          }
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

impl format::Display for AccessorTransformer {
  fn fmt(&self, f: &mut format::Formatter<'_>) -> fmt::Result {
    let AccessorTransformer {
      ident,
      keys,
      nested,
    } = self;

    if !ident.is_empty() {
      write!(f, ".{ident}")?;
    }

    for key in keys {
      format::Display::fmt(key, f)?;
    }

    if let Some(nested) = nested {
      format::Display::fmt(nested.as_ref(), f)?;
    }

    Ok(())
  }
}

pub struct RootAccessorDisplay<'a>(pub &'a AccessorTransformer);

impl format::Display for RootAccessorDisplay<'_> {
  fn fmt(&self, f: &mut format::Formatter<'_>) -> fmt::Result {
    let RootAccessorDisplay(accessor) = *self;

    if accessor.ident.is_empty() {
      f.write_char('.')?;
    }

    format::Display::fmt(accessor, f)
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
      _ => Ok(KeyAccessorTransformer::Index(ExprTransformer::from_pairs(
        pairs,
      )?)),
    }
  }
}

impl format::Display for KeyAccessorTransformer {
  fn fmt(&self, f: &mut format::Formatter<'_>) -> fmt::Result {
    f.write_char('[')?;

    match self {
      KeyAccessorTransformer::Index(expr) => format::Display::fmt(expr, f)?,
      KeyAccessorTransformer::Range { from, to } => {
        if let Some(from) = from {
          format::Display::fmt(from, f)?;
        }

        f.write_char(':')?;

        if let Some(to) = to {
          format::Display::fmt(to, f)?;
        }
      }
    }

    f.write_char(']')?;
    Ok(())
  }
}
