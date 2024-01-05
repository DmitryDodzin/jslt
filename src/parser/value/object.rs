use std::{borrow::Cow, ops::Deref};

use pest::iterators::Pairs;
use serde_json::Value;

use crate::{
  context::{builtins::boolean_cast, Context},
  error::{JsltError, Result},
  expect_inner,
  parser::{
    builder::{ExprBuilder, ForBuilder},
    FromParis, Rule,
  },
  Transform,
};

#[derive(Debug)]
pub struct PairBuilder(ExprBuilder, ExprBuilder);

impl FromParis for PairBuilder {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut inner = expect_inner!(pairs, Rule::Pair)?;

    let key = ExprBuilder::from_pairs(&mut inner)?;
    let value = ExprBuilder::from_pairs(&mut inner)?;

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
            .push(ObjectBuilderInner::For(ObjectForBuilder::from_pairs(
              &mut inner_pairs,
            )?));
        }
        Rule::ObjectSpread => {
          let mut pairs = pair.into_inner();

          builder
            .inner
            .push(ObjectBuilderInner::Spread(ExprBuilder::from_pairs(
              &mut pairs,
            )?))
        }
        _ => unimplemented!("for Pair: {pair:#?}"),
      }
    }

    Ok(builder)
  }
}

impl Transform for ObjectBuilder {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    let mut items = serde_json::Map::new();

    for inner in &self.inner {
      match inner {
        ObjectBuilderInner::Pair(PairBuilder(key, value)) => {
          let key = key
            .transform_value(Context::Borrowed(&context), input)?
            .as_str()
            .expect("Should result in string")
            .to_owned();

          items.insert(
            key,
            value.transform_value(Context::Borrowed(&context), input)?,
          );
        }
        ObjectBuilderInner::For(ObjectForBuilder {
          source,
          output,
          condition,
        }) => {
          let PairBuilder(key, value) = output.deref();
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
              if !boolean_cast(&condition.transform_value(Context::Borrowed(&context), &input)?) {
                continue;
              }
            }

            let key = key
              .transform_value(Context::Borrowed(&context), &input)?
              .as_str()
              .expect("Should result in string")
              .to_owned();

            items.insert(
              key,
              value.transform_value(Context::Borrowed(&context), &input)?,
            );
          }
        }
        ObjectBuilderInner::Spread(expr) => {
          let source = input.as_object().expect("Should be object");

          for key in source.keys() {
            if items.contains_key(key) {
              continue;
            }

            items.insert(
              key.clone(),
              expr.transform_value(Context::Borrowed(&context), &source[key])?,
            );
          }
        }
      }
    }

    Ok(Value::Object(items))
  }
}

#[derive(Debug)]
pub enum ObjectBuilderInner {
  Pair(PairBuilder),
  For(ObjectForBuilder),
  Spread(ExprBuilder),
}

pub type ObjectForBuilder = ForBuilder<PairBuilder>;
