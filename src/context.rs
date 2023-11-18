use std::{borrow::Cow, collections::HashMap, fmt};

use serde_json::Value;

use crate::error::Result;

mod builtins;

#[derive(Clone)]
pub enum JsltFunction {
  Static(&'static dyn Fn(&[Value]) -> Result<Value>),
}

impl JsltFunction {
  pub fn call(&self, arguments: &[Value]) -> Result<Value> {
    match self {
      JsltFunction::Static(function) => function(arguments),
    }
  }
}

impl fmt::Debug for JsltFunction {
  fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      JsltFunction::Static(_) => fmt.debug_tuple("Static").finish(),
    }
  }
}

macro_rules! include_builtin {
  ($functions:ident, $ident:ident, $name:expr) => {
    $functions.insert($name.to_owned(), JsltFunction::Static(&builtins::$ident))
  };
  ($functions:ident, $ident:ident) => {
    include_builtin!($functions, $ident, stringify!($ident))
  };
}

#[derive(Clone, Debug)]
pub struct JsltContext {
  pub functions: HashMap<String, JsltFunction>,
}

impl Default for JsltContext {
  fn default() -> Self {
    let mut functions = HashMap::default();

    include_builtin!(functions, contains);
    include_builtin!(functions, size);
    include_builtin!(functions, error);
    include_builtin!(functions, min);
    include_builtin!(functions, max);
    include_builtin!(functions, is_number, "is-number");
    include_builtin!(functions, is_integer, "is-integer");
    include_builtin!(functions, is_decimal, "is-decimal");
    include_builtin!(functions, number);
    include_builtin!(functions, round);
    include_builtin!(functions, floor);
    include_builtin!(functions, ceiling);
    include_builtin!(functions, sum);
    include_builtin!(functions, r#mod);
    include_builtin!(functions, hash_int, "hash-int");

    JsltContext { functions }
  }
}

pub type Context<'s> = Cow<'s, JsltContext>;
