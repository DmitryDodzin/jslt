use std::ops::Deref;

use enum_dispatch::enum_dispatch;
use pest::iterators::Pairs;
use serde_json::Value;

use crate::{
  error::{JsltError, Result},
  parser::Rule,
};

pub trait FromParis: Sized {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self>;
}

#[enum_dispatch]
pub trait Transform {
  fn transform_value(&self, context: &Value) -> Result<Value>;
}

#[derive(Debug)]
#[enum_dispatch(Transform)]
pub enum JsltBuilder {
  Accessor(AccessorBuilder),
  Array(ArrayBuilder),
  Boolean(BooleanBuilder),
  Null(NullBuilder),
  Number(NumberBuilder),
  Object(ObjectBuilder),
  String(StringBuilder),
}

impl FromParis for JsltBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    for pair in pairs {
      return match pair.as_rule() {
        Rule::Accessor => Ok(JsltBuilder::Accessor(AccessorBuilder::from_pairs(
          &mut pair.into_inner(),
        )?)),
        Rule::Array => Ok(JsltBuilder::Array(ArrayBuilder::from_pairs(
          &mut pair.into_inner(),
        )?)),
        Rule::COMMENT => continue,
        Rule::Boolean => Ok(JsltBuilder::Boolean(BooleanBuilder(
          pair
            .as_str()
            .parse()
            .map_err(|_| JsltError::UnexpectedContent(Rule::Boolean))?,
        ))),
        Rule::Null => Ok(JsltBuilder::Null(NullBuilder)),
        Rule::Number => Ok(JsltBuilder::Number(NumberBuilder(pair.as_str().parse()?))),
        Rule::Object => Ok(JsltBuilder::Object(ObjectBuilder::from_pairs(
          &mut pair.into_inner(),
        )?)),
        Rule::String => Ok(JsltBuilder::String(StringBuilder::from_pairs(
          &mut pair.into_inner(),
        )?)),
        rule => Err(JsltError::UnexpectedInput(rule, pair.as_str().to_owned())),
      };
    }

    Err(JsltError::UnexpectedInput(Rule::EOI, "EOI".to_owned()))
  }
}

#[derive(Debug)]
pub struct AccessorBuilder {
  ident: String,
  keys: Vec<KeyAccessorBuilder>,
  nested: Option<Box<AccessorBuilder>>,
}

impl FromParis for AccessorBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
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
          nested = Some(Box::new(AccessorBuilder::from_pairs(
            &mut pair.into_inner(),
          )?));
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
  fn transform_value(&self, context: &Value) -> Result<Value> {
    let mut value = (!self.ident.is_empty())
      .then(|| &context[&self.ident])
      .unwrap_or(context);

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
      Some(nested) => nested.transform_value(value),
      None => Ok(value.clone()),
    }
  }
}

#[derive(Debug)]
pub enum KeyAccessorBuilder {
  Index(Value),
  Range { from: Value, to: Value },
}

#[derive(Debug, Default)]
pub struct ArrayBuilder {
  inner: Vec<ArrayBuilderInner>,
}

impl FromParis for ArrayBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut builder = ArrayBuilder::default();

    while let Some(next) = pairs.peek() {
      match next.as_rule() {
        Rule::COMMENT => {
          let _ = pairs.next();
        }
        Rule::ArrayFor => {
          builder
            .inner
            .push(ArrayBuilderInner::For(ArrayFor::from_pairs(
              &mut pairs
                .next()
                .expect("is not empty because of peek")
                .into_inner(),
            )?));
        }
        _ => builder
          .inner
          .push(ArrayBuilderInner::Item(JsltBuilder::from_pairs(pairs)?)),
      }
    }

    Ok(builder)
  }
}

impl Transform for ArrayBuilder {
  fn transform_value(&self, context: &Value) -> Result<Value> {
    let mut items = Vec::new();

    for inner in &self.inner {
      match inner {
        ArrayBuilderInner::Item(jslt) => items.push(jslt.transform_value(context)?),
        ArrayBuilderInner::For(ArrayFor { source, output }) => {
          let source = source.transform_value(context)?;

          for context in source.as_array().expect("Should be array") {
            items.push(output.transform_value(context)?);
          }
        }
      }
    }

    Ok(Value::Array(items))
  }
}

#[derive(Debug)]
pub enum ArrayBuilderInner {
  Item(JsltBuilder),
  For(ArrayFor),
}

#[derive(Debug)]
pub struct BooleanBuilder(bool);

impl Transform for BooleanBuilder {
  fn transform_value(&self, _: &Value) -> Result<Value> {
    Ok(Value::Bool(self.0))
  }
}

#[derive(Debug)]
pub struct NumberBuilder(pub(super) serde_json::Number);

impl Transform for NumberBuilder {
  fn transform_value(&self, _: &Value) -> Result<Value> {
    Ok(Value::Number(self.0.clone()))
  }
}

#[derive(Debug)]
pub struct PairBuilder(JsltBuilder, JsltBuilder);

impl FromParis for PairBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let inner = pairs
      .next()
      .ok_or(JsltError::UnexpectedContent(Rule::Pair))?;

    if !matches!(inner.as_rule(), Rule::Pair) {
      return Err(JsltError::UnexpectedInput(
        Rule::Pair,
        inner.as_str().to_owned(),
      ));
    }

    let mut inner = inner.into_inner();

    let key = JsltBuilder::from_pairs(&mut inner)?;
    let value = JsltBuilder::from_pairs(&mut inner)?;

    Ok(PairBuilder(key, value))
  }
}

#[derive(Debug, Default)]
pub struct ObjectBuilder {
  inner: Vec<ObjectBuilderInner>,
}

impl FromParis for ObjectBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut builder = ObjectBuilder::default();

    for pair in pairs {
      match pair.as_rule() {
        Rule::COMMENT => continue,
        Rule::Pair => {
          builder
            .inner
            .push(ObjectBuilderInner::Pair(PairBuilder::from_pairs(
              &mut Pairs::single(pair),
            )?));
        }
        Rule::ObjectFor => {
          let mut inner_pairs = pair.into_inner();

          builder
            .inner
            .push(ObjectBuilderInner::For(ObjectFor::from_pairs(
              &mut inner_pairs,
            )?));
        }
        _ => unimplemented!("for Pair: {pair:#?}"),
      }
    }

    Ok(builder)
  }
}

impl Transform for ObjectBuilder {
  fn transform_value(&self, context: &Value) -> Result<Value> {
    let mut items = Vec::new();

    for inner in &self.inner {
      match inner {
        ObjectBuilderInner::Pair(PairBuilder(key, value)) => {
          items.push((
            key
              .transform_value(context)?
              .as_str()
              .expect("Should result in string")
              .to_owned(),
            value.transform_value(context)?,
          ));
        }
        ObjectBuilderInner::For(ObjectFor { source, output }) => {
          let PairBuilder(key, value) = output.deref();
          let source = source.transform_value(context)?;

          for context in source.as_array().expect("Should be array") {
            items.push((
              key
                .transform_value(context)?
                .as_str()
                .expect("Should result in string")
                .to_owned(),
              value.transform_value(context)?,
            ))
          }
        }
      }
    }

    Ok(Value::Object(items.into_iter().collect()))
  }
}

#[derive(Debug)]
pub enum ObjectBuilderInner {
  Pair(PairBuilder),
  For(ObjectFor),
}

#[derive(Debug)]
pub struct ForBuilder<B> {
  source: Box<JsltBuilder>,

  output: Box<B>,
}

impl<B> FromParis for ForBuilder<B>
where
  B: FromParis,
{
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let source = JsltBuilder::from_pairs(pairs)?;

    let output = B::from_pairs(pairs)?;

    Ok(ForBuilder {
      source: Box::new(source),
      output: Box::new(output),
    })
  }
}

type ArrayFor = ForBuilder<JsltBuilder>;
type ObjectFor = ForBuilder<PairBuilder>;

#[derive(Debug)]
pub struct StringBuilder(String);

impl FromParis for StringBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let inner = pairs
      .next()
      .ok_or(JsltError::UnexpectedContent(Rule::String))?;

    let rule = inner.as_rule();

    if matches!(rule, Rule::Inner) {
      Ok(StringBuilder(inner.as_str().to_owned()))
    } else {
      Err(JsltError::UnexpectedInput(rule, inner.as_str().to_owned()))
    }
  }
}

impl Transform for StringBuilder {
  fn transform_value(&self, _: &Value) -> Result<Value> {
    Ok(Value::String(self.0.clone()))
  }
}

#[derive(Debug)]
pub struct NullBuilder;

impl Transform for NullBuilder {
  fn transform_value(&self, _: &Value) -> Result<Value> {
    Ok(Value::Null)
  }
}
