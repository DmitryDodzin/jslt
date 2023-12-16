use std::{borrow::Cow, collections::HashMap, fmt, sync::Arc};

use serde_json::Value;

use crate::{error::Result, parser::ExprBuilder, Transform};

pub(crate) mod builtins;

#[derive(Clone)]
pub enum JsltFunction {
  Static(&'static (dyn Fn(&[Value]) -> Result<Value> + Send + Sync)),
  Dynamic(DynamicFunction),
}

impl JsltFunction {
  pub fn call(&self, arguments: &[Value]) -> Result<Value> {
    match self {
      JsltFunction::Static(function) => function(arguments),
      JsltFunction::Dynamic(function) => function.call(arguments),
    }
  }
}

impl fmt::Debug for JsltFunction {
  fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      JsltFunction::Static(_) => fmt.debug_tuple("Static").finish(),
      JsltFunction::Dynamic(function) => fmt.debug_tuple("Dynamic").field(&function).finish(),
    }
  }
}

#[derive(Debug, Clone)]
pub struct DynamicFunction {
  pub name: String,
  pub arguments: Vec<String>,
  pub expr: Arc<ExprBuilder>,
  pub context: JsltContext,
}

impl DynamicFunction {
  pub fn call(&self, arguments: &[Value]) -> Result<Value> {
    let arguments = self
      .arguments
      .iter()
      .zip(arguments)
      .map(|(name, value)| (name.clone(), value.clone()));

    let mut context = self.context.clone();

    context.variables.extend(arguments);

    self
      .expr
      .transform_value(Cow::Borrowed(&context), &Value::Null)
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
  pub variables: HashMap<String, Value>,
}

impl Default for JsltContext {
  fn default() -> Self {
    let mut functions = HashMap::default();
    let variables = HashMap::default();

    include_builtin!(functions, contains);
    include_builtin!(functions, size);
    include_builtin!(functions, error);
    include_builtin!(functions, fallback);
    include_builtin!(functions, min);
    include_builtin!(functions, max);
    include_builtin!(functions, is_number, "is-number");
    include_builtin!(functions, is_integer, "is-integer");
    include_builtin!(functions, is_decimal, "is-decimal");
    include_builtin!(functions, number);
    include_builtin!(functions, round);
    include_builtin!(functions, floor);
    include_builtin!(functions, ceiling);
    include_builtin!(functions, random);
    include_builtin!(functions, sum);
    include_builtin!(functions, r#mod, "mod");
    include_builtin!(functions, hash_int, "hash-int");
    include_builtin!(functions, is_string, "is-string");
    include_builtin!(functions, string);
    include_builtin!(functions, test);
    // include_builtin!(functions, capture);
    include_builtin!(functions, split);
    include_builtin!(functions, join);
    include_builtin!(functions, lowercase);
    include_builtin!(functions, uppercase);
    include_builtin!(functions, sha256_hex, "sha256-hex");
    include_builtin!(functions, starts_with, "starts-with");
    include_builtin!(functions, ends_with, "ends-with");
    include_builtin!(functions, from_json, "from-json");
    include_builtin!(functions, to_json, "to-json");
    include_builtin!(functions, replace);
    include_builtin!(functions, trim);
    include_builtin!(functions, uuid);
    include_builtin!(functions, boolean);
    include_builtin!(functions, not);
    include_builtin!(functions, is_boolean, "is-boolean");
    include_builtin!(functions, is_object, "is-object");
    include_builtin!(functions, get_key, "get-key");
    include_builtin!(functions, array);
    include_builtin!(functions, is_array, "is-array");
    // include_builtin!(functions, flatten);
    include_builtin!(functions, all);
    include_builtin!(functions, any);
    include_builtin!(functions, zip);
    // include_builtin!(functions, zip_with_index, "zip-with-index");
    include_builtin!(functions, index_of, "index-of");
    include_builtin!(functions, now);
    // include_builtin!(functions, parse_time, "parse-time");
    // include_builtin!(functions, format_time, "format-time");
    include_builtin!(functions, parse_url, "parse-url");

    JsltContext {
      functions,
      variables,
    }
  }
}

pub type Context<'s> = Cow<'s, JsltContext>;
