use std::ops::Deref;

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
pub enum JsltBuilder {
  Accessor(AccessorBuilder),
  Array(ArrayBuilder),
  Object(ObjectBuilder),
  Value(ValueBuilder),
  FunctionCall(FunctionCallBuilder),
}

impl Transform for JsltBuilder {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    match self {
      JsltBuilder::Accessor(accessor) => accessor.transform_value(context, input),
      JsltBuilder::Array(array) => array.transform_value(context, input),
      JsltBuilder::Object(object) => object.transform_value(context, input),
      JsltBuilder::Value(value) => value.transform_value(context, input),
      JsltBuilder::FunctionCall(fcall) => fcall.transform_value(context, input),
    }
  }
}

impl FromParis for JsltBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    while let Some(pair) = pairs.peek() {
      return match pair.as_rule() {
        Rule::Accessor => AccessorBuilder::from_pairs(pairs).map(JsltBuilder::Accessor),
        Rule::Array => ArrayBuilder::from_pairs(pairs).map(JsltBuilder::Array),
        Rule::COMMENT => {
          let _ = pairs.next();
          continue;
        }
        Rule::Object => ObjectBuilder::from_pairs(pairs).map(JsltBuilder::Object),
        Rule::FunctionCall => FunctionCallBuilder::from_pairs(pairs).map(JsltBuilder::FunctionCall),
        _ => ValueBuilder::from_pairs(pairs).map(JsltBuilder::Value),
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

#[derive(Debug, Default)]
pub struct ArrayBuilder {
  inner: Vec<ArrayBuilderInner>,
}

impl FromParis for ArrayBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::Array)?;

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
          .push(ArrayBuilderInner::Item(JsltBuilder::from_pairs(
            &mut pairs,
          )?)),
      }
    }

    Ok(builder)
  }
}

impl Transform for ArrayBuilder {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    let mut items = Vec::new();

    for inner in &self.inner {
      match inner {
        ArrayBuilderInner::Item(jslt) => items.push(jslt.transform_value(context.clone(), input)?),
        ArrayBuilderInner::For(ArrayFor { source, output }) => {
          let source = source.transform_value(context.clone(), input)?;

          for input in source.as_array().expect("Should be array") {
            items.push(output.transform_value(context.clone(), input)?);
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
pub struct PairBuilder(JsltBuilder, JsltBuilder);

impl FromParis for PairBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut inner = expect_inner!(pairs, Rule::Pair)?;

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
    let pairs = expect_inner!(pairs, Rule::Object)?;

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
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    let mut items = Vec::new();

    for inner in &self.inner {
      match inner {
        ObjectBuilderInner::Pair(PairBuilder(key, value)) => {
          items.push((
            key
              .transform_value(context.clone(), input)?
              .as_str()
              .expect("Should result in string")
              .to_owned(),
            value.transform_value(context.clone(), input)?,
          ));
        }
        ObjectBuilderInner::For(ObjectFor { source, output }) => {
          let PairBuilder(key, value) = output.deref();
          let source = source.transform_value(context.clone(), input)?;

          for input in source.as_array().expect("Should be array") {
            items.push((
              key
                .transform_value(context.clone(), input)?
                .as_str()
                .expect("Should result in string")
                .to_owned(),
              value.transform_value(context.clone(), input)?,
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
pub struct FunctionCallBuilder {
  name: String,
  arguments: Vec<JsltBuilder>,
}

impl FromParis for FunctionCallBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::FunctionCall)?;

    let name = pairs.next().ok_or(JsltError::UnexpectedEnd)?;

    let arguments = pairs
      .map(|pair| JsltBuilder::from_pairs(&mut Pairs::single(pair)))
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
