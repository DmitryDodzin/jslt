use std::ops::Deref;

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
        ObjectBuilderInner::For(ObjectFor {
          source,
          output,
          condition,
        }) => {
          let PairBuilder(key, value) = output.deref();
          let source = source.transform_value(context.clone(), input)?;

          for input in source.as_array().expect("Should be array") {
            if let Some(condition) = condition {
              if !boolean_cast(&condition.transform_value(context.clone(), input)?) {
                continue;
              }
            }

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

pub type ObjectFor = ForBuilder<PairBuilder>;
