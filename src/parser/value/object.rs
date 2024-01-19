use alloc::{
  borrow::{Cow, ToOwned},
  boxed::Box,
  vec::Vec,
};
use core::ops::Deref;

use pest::iterators::Pairs;
use serde_json::Value;

use crate::{
  context::{builtins::boolean_cast, Context},
  error::{JsltError, Result},
  expect_inner,
  parser::{
    builder::{ExprParser, ForParser},
    FromPairs, Rule,
  },
  Transform,
};

#[derive(Debug)]
pub struct PairParser(ExprParser, ExprParser);

impl FromPairs for PairParser {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut inner = expect_inner!(pairs, Rule::Pair)?;

    let key = ExprParser::from_pairs(&mut inner)?;
    let value = ExprParser::from_pairs(&mut inner)?;

    Ok(PairParser(key, value))
  }
}

#[derive(Debug, Default)]
pub struct ObjectParser {
  inner: Vec<ObjectParserInner>,
}

impl FromPairs for ObjectParser {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let pairs = expect_inner!(pairs, Rule::Object)?;

    let mut builder = ObjectParser::default();

    for pair in pairs {
      match pair.as_rule() {
        Rule::COMMENT => continue,
        Rule::Pair => {
          builder
            .inner
            .push(ObjectParserInner::Pair(PairParser::from_pairs(
              &mut Pairs::single(pair),
            )?));
        }
        Rule::ObjectFor => {
          let mut inner_pairs = pair.into_inner();

          builder
            .inner
            .push(ObjectParserInner::For(ObjectForParser::from_pairs(
              &mut inner_pairs,
            )?));
        }
        Rule::ObjectSpread => {
          let mut pairs = pair.into_inner();

          builder
            .inner
            .push(ObjectParserInner::Spread(ExprParser::from_pairs(
              &mut pairs,
            )?))
        }
        _ => unimplemented!("for Pair: {pair:#?}"),
      }
    }

    Ok(builder)
  }
}

impl Transform for ObjectParser {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    let mut items = serde_json::Map::new();

    for inner in &self.inner {
      match inner {
        ObjectParserInner::Pair(PairParser(key, value)) => {
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
        ObjectParserInner::For(ObjectForParser {
          source,
          output,
          condition,
        }) => {
          let PairParser(key, value) = output.deref();
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
        ObjectParserInner::Spread(expr) => {
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
pub enum ObjectParserInner {
  Pair(PairParser),
  For(ObjectForParser),
  Spread(ExprParser),
}

pub type ObjectForParser = ForParser<PairParser>;
