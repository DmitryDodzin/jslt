use std::borrow::Cow;

use pest::iterators::Pairs;
use serde_json::Value;

use crate::{
  context::{builtins, Context},
  error::{JsltError, Result},
  expect_inner,
  parser::{
    builder::{ExprParser, ForParser},
    FromPairs, Rule,
  },
  Transform,
};

#[derive(Debug, Default)]
pub struct ArrayParser {
  inner: Vec<ArrayParserInner>,
}

impl FromPairs for ArrayParser {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self> {
    let mut pairs = expect_inner!(pairs, Rule::Array)?;

    let mut builder = ArrayParser::default();

    while let Some(next) = pairs.peek() {
      match next.as_rule() {
        Rule::COMMENT => {
          let _ = pairs.next();
        }
        Rule::ArrayFor => {
          builder
            .inner
            .push(ArrayParserInner::For(ArrayFor::from_pairs(
              &mut pairs
                .next()
                .expect("is not empty because of peek")
                .into_inner(),
            )?));
        }
        _ => builder
          .inner
          .push(ArrayParserInner::Item(ExprParser::from_pairs(&mut pairs)?)),
      }
    }

    Ok(builder)
  }
}

impl Transform for ArrayParser {
  fn transform_value(&self, context: Context<'_>, input: &Value) -> Result<Value> {
    let mut items = Vec::new();

    for inner in &self.inner {
      match inner {
        ArrayParserInner::Item(jslt) => {
          items.push(jslt.transform_value(Context::Borrowed(&context), input)?)
        }
        ArrayParserInner::For(ArrayFor {
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
pub enum ArrayParserInner {
  Item(ExprParser),
  For(ArrayFor),
}

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

pub type ArrayFor = ForParser<ExprParser>;
