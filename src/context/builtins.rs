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
  ($vis:vis fn $ident:ident ($param1:ident: &Value, $param2:ident: Option<&Value>) -> Result<Value> $block:block ) => {
    $vis fn $ident ( arguments: &[Value] ) -> Result<Value> {
      let $param1 = &arguments[0];
      let $param2 = arguments.get(1).as_ref();

      $block
    }
  };
  ($vis:vis fn $ident:ident ($param1:ident: &Value, $param2:ident: &Value, $param3:ident: &Value) -> Result<Value> $block:block ) => {
    $vis fn $ident ( arguments: &[Value] ) -> Result<Value> {
      let $param1 = &arguments[0];
      let $param2 = &arguments[1];
      let $param3 = &arguments[2];

      $block
    }
  };
  ($vis:vis fn $ident:ident ($param1:ident: &Value, $param2:ident: &Value, $param3:ident: Option<&Value>) -> Result<Value> $block:block ) => {
    $vis fn $ident ( arguments: &[Value] ) -> Result<Value> {
      let $param1 = &arguments[0];
      let $param2 = &arguments[1];
      let $param3 = &arguments.get(2).as_ref();

      $block
    }
  };
}

static_function! {
  pub fn contains(element: &Value, sequence: &Value) -> Result<Value> {
    match sequence {
      Value::Array(array) => Ok(array.contains(element).into()),
      Value::Object(object) => Ok(
        element
          .as_str()
          .map(|element| object.contains_key(element))
          .unwrap_or(object.contains_key(&element.to_string()))
          .into(),
      ),
      Value::String(string) => Ok(
        element
          .as_str()
          .map(|element| string.contains(element))
          .unwrap_or(false)
          .into(),
      ),
      _ => Err(JsltError::InvalidInput(
        "Arguments must be array | object | string".to_string(),
      )),
    }
  }
}

static_function! {
  pub fn size(input: &Value) -> Result<Value> {
    match input {
      Value::Array(array) => Ok(array.len().into()),
      Value::Object(object) => Ok(object.len().into()),
      Value::String(string) => Ok(string.len().into()),
      Value::Null => Ok(Value::Null),
      _ => Err(JsltError::InvalidInput(
        format!("Arguments must be array | object | string (got {input})")
      )),
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
  pub fn is_number(maybe_number: &Value) -> Result<Value> {
    Ok(Value::Bool(matches!(maybe_number, Value::Number(_))))
  }
}

static_function! {
  pub fn is_integer(maybe_integer: &Value) -> Result<Value> {
    Ok(Value::Bool(matches!(maybe_integer, Value::Number(number) if number.is_i64() || number.is_u64())))
  }
}

static_function! {
  pub fn is_decimal(maybe_decimal: &Value) -> Result<Value> {
    Ok(Value::Bool(matches!(maybe_decimal, Value::Number(number) if number.is_f64())))
  }
}

static_function! {
  pub fn number(maybe_number: &Value, fallback: &Value) -> Result<Value> {
    match maybe_number {
      Value::String(str_number) => Ok(str_number.parse().map(Value::Number).ok().unwrap_or_else(|| fallback.clone())),
      Value::Number(_) => Ok(maybe_number.clone()),
      _ => Ok(fallback.clone()),
    }
  }
}

static_function! {
pub fn round(value: &Value) -> Result<Value> {
  match value {
    Value::Number(number) if number.is_f64() => {
      let rounded = number.as_f64().expect("Should be f64").round();

      Ok(Value::Number(if rounded > 0.0 {
        (rounded as u64).into()
      } else {
        (rounded as i64).into()
      }))
    }
    Value::Number(_) => Ok(value.clone()),
    _ => Err(JsltError::InvalidInput(
      "Input of round must be number".to_string(),
    )),
  }
}
}

static_function! {
pub fn floor(value: &Value) -> Result<Value> {
  match value {
    Value::Number(number) if number.is_f64() => {
      let rounded = number.as_f64().expect("Should be f64").floor();

      Ok(Value::Number(if rounded > 0.0 {
        (rounded as u64).into()
      } else {
        (rounded as i64).into()
      }))
    }
    Value::Number(_) => Ok(value.clone()),
    _ => Err(JsltError::InvalidInput(
      "Input of round must be number".to_string(),
    )),
  }
}
}

static_function! {
  pub fn ceiling(value: &Value) -> Result<Value> {
    match value {
      Value::Number(number) if number.is_f64() => {
        let rounded = number.as_f64().expect("Should be f64").ceil();

        Ok(Value::Number(if rounded > 0.0 {
          (rounded as u64).into()
        } else {
          (rounded as i64).into()
        }))
      }
      Value::Number(_) => Ok(value.clone()),
      _ => Err(JsltError::InvalidInput(
        "Input of round must be number".to_string(),
      )),
    }
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

// String

static_function! {
  pub fn is_string(maybe_string: &Value) -> Result<Value> {
    Ok(Value::Bool(matches!(maybe_string, Value::String(_))))
  }
}

static_function! {
  pub fn string(_value: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn test(_value: &Value, _regex: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn capture(_input: &Value, _regex: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn split(_input: &Value, _regex: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn join(_array: &Value, _separator: &Value) -> Result<Value> {
    todo!()
  }
}
static_function! {
  pub fn lowercase(_string: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn uppercase(_string: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn sha256_hex(_string: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn starts_with(_tested: &Value, _prefix: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn ends_with(_tested: &Value, _prefix: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn from_json(_string: &Value, _fallback: Option<&Value>) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn to_json(_value: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn replace(_value: &Value, _regexp: &Value, _out: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn trim(_string: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn uuid(_a: &Value, _b: &Value) -> Result<Value> {
    todo!()
  }
}

// Boolean

static_function! {
  pub fn boolean(_value: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn not(_value: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn is_boolean(_value: &Value) -> Result<Value> {
    todo!()
  }
}

// Object

static_function! {
  pub fn is_object(_value: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn get_key(_object: &Value, _key: &Value, _fallback: Option<&Value>) -> Result<Value> {
    todo!()
  }
}

// Array

static_function! {
  pub fn array(_value: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn is_array(_value: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn flatten(_array: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn all(_array: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn any(_array: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn zip(_values_left: &Value, _values_right: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn zip_with_index(_values: &Value) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn index_of(_array: &Value, _value: &Value) -> Result<Value> {
    todo!()
  }
}

// Time

static_function! {
  pub fn now() -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn parse_time(_time: &Value, _format: &Value, _fallback: Option<&Value>) -> Result<Value> {
    todo!()
  }
}

static_function! {
  pub fn format_time(_timestamp: &Value, _format: &Value, _timezone: Option<&Value>) -> Result<Value> {
    todo!()
  }
}

// Miscellaneous

static_function! {
  pub fn parse_url(_url: &Value) -> Result<Value> {
    todo!()
  }
}
