use serde_json::Value;

use crate::error::{JsltError, Result};

macro_rules! static_function {
  ($vis:vis fn $ident:ident () -> Result<Value> $block:block ) => {
    $vis fn $ident ( _: &[Value] ) -> Result<Value> $block
  };
  ($vis:vis fn $ident:ident ($param:ident: &Value) -> Result<Value> $block:block ) => {
    $vis fn $ident ( arguments: &[Value] ) -> Result<Value> {
      let $param = &arguments[0];

      $block
    }
  };
  ($vis:vis fn $ident:ident ($param1:ident: &Value, $param2:ident: &Value) -> Result<Value> $block:block ) => {
    $vis fn $ident ( arguments: &[Value] ) -> Result<Value> {
      let $param1 = &arguments[0];
      let $param2 = &arguments[1];

      $block
    }
  };
}

static_function! {
  pub fn contains(_element: &Value, _sequence: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn size(input: &Value) -> Result<Value> {
    match input {
      Value::Array(array) => Ok(array.len().into()),
      Value::Object(object) => Ok(object.len().into()),
      Value::String(string) => Ok(string.len().into()),
      _ => Err(JsltError::Unknown(format!(
        "Expecting input to be sequence but got {input:?}"
      ))),
    }
  }
}

static_function! {
  pub fn error(message: &Value) -> Result<Value> {
    Err(JsltError::Unknown(message.as_str().map(str::to_owned).unwrap_or_else(|| message.to_string())))
  }
}

static_function! {
  pub fn min(_left: &Value, _right: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn max(_left: &Value, _right: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn is_number(_maybe_number: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn is_integer(_maybe_integer: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn is_decimal(_maybe_decimal: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn number(_maybe_number: &Value, _fallback: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn round(_value: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn floor(_value: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn ceiling(_value: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn sum(_values: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn r#mod(_left: &Value, _right: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn hash_int(_left: &Value, _right: &Value) -> Result<Value> {
    todo!()
  }
}
