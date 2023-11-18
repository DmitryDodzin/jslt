mod builder;
mod value;

pub use builder::*;
use pest::iterators::Pairs;

use crate::error::Result;

pub trait FromParis: Sized {
  fn from_pairs(pairs: &mut Pairs<Rule>) -> Result<Self>;
}

#[derive(pest_derive::Parser)]
#[grammar = "./src/parser/jslt.pest"]
pub struct JsltParser;
