mod builder;

pub use builder::*;

#[derive(pest_derive::Parser)]
#[grammar = "./src/parser/jslt.pest"]
pub struct JsltParser;
