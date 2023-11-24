#![allow(dead_code)]

use std::{
  collections::hash_map::DefaultHasher,
  hash::{Hash, Hasher},
  time::SystemTime,
};

use regex::Regex;
use serde_json::{json, Value};
use url::Url;
use uuid::Uuid;

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
      let $param2 = arguments.get(1);

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
      let $param3 = arguments.get(2);

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

pub fn fallback(arguments: &[Value]) -> Result<Value> {
  for argument in arguments {
    if !(matches!(argument, Value::Null)
      || matches!(argument, Value::Array(items) if items.is_empty())
      || matches!(argument, Value::Object(items) if items.is_empty()))
    {
      return Ok(argument.clone());
    }
  }

  Ok(Value::Null)
}

static_function! {
  pub fn min(left: &Value, right: &Value) -> Result<Value> {
    match (left, right) {
      (Value::Number(left), Value::Number(right)) if left.is_u64() && right.is_u64() => Ok(
        left
          .as_u64()
          .expect("should be u64")
          .min(right.as_u64().expect("should be u64"))
          .into(),
      ),
      (Value::Number(left), Value::Number(right)) if left.is_i64() && right.is_i64() => Ok(
        left
          .as_i64()
          .expect("should be i64")
          .min(right.as_i64().expect("should be i64"))
          .into(),
      ),
      (Value::Number(left), Value::Number(right)) => {
        if left.as_f64() < right.as_f64() {
          Ok(Value::Number(left.clone()))
        } else {
          Ok(Value::Number(right.clone()))
        }
      }
      (Value::String(left), Value::String(right)) => Ok(Value::String(left.min(right).to_string())),
      (_, Value::Null) | (Value::Null, _) => Ok(Value::Null),
      _ => Err(JsltError::InvalidInput(format!(
        "Unimplemented operation min between ({left} and {right}), maybe use number() before passing to min"
      ))),
    }
  }
}

static_function! {
  pub fn max(left: &Value, right: &Value) -> Result<Value> {
    match (left, right) {
      (Value::Number(left), Value::Number(right)) if left.is_u64() && right.is_u64() => Ok(
        left
          .as_u64()
          .expect("should be u64")
          .max(right.as_u64().expect("should be u64"))
          .into(),
      ),
      (Value::Number(left), Value::Number(right)) if left.is_i64() && right.is_i64() => Ok(
        left
          .as_i64()
          .expect("should be i64")
          .max(right.as_i64().expect("should be i64"))
          .into(),
      ),
      (Value::Number(left), Value::Number(right)) => {
        if left.as_f64() > right.as_f64() {
          Ok(Value::Number(left.clone()))
        }  else {
          Ok(Value::Number(right.clone()))
        }
      }
      (Value::String(left), Value::String(right)) => Ok(Value::String(left.max(right).to_string())),
      (_, Value::Null) | (Value::Null, _) => Ok(Value::Null),
      _ => Err(JsltError::InvalidInput(format!(
        "Unimplemented operation max between ({left} and {right}, maybe use number() before passing to max"
      ))),
    }
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
  pub fn number(maybe_number: &Value, fallback: Option<&Value>) -> Result<Value> {
    match maybe_number {
      Value::String(str_number) => Ok(
        str_number
          .parse::<u64>()
          .map(|long| long.into())
          .or(str_number.parse::<i64>().map(|int| int.into()))
          .or(str_number.parse::<f64>().map(|int| int.into()))
          .unwrap_or_else(|_| fallback.cloned().into()),
      ),
      Value::Number(_) => Ok(maybe_number.clone()),
      _ => Ok(fallback.cloned().into()),
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
      Value::Null => Ok(Value::Null),
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
      Value::Null => Ok(Value::Null),
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
      Value::Null => Ok(Value::Null),
      _ => Err(JsltError::InvalidInput(
        "Input of round must be number".to_string(),
      )),
    }
  }
}

static_function! {
  pub fn random() -> Result<Value> {
    Ok(Value::Number(serde_json::Number::from_f64(rand::random()).unwrap_or_else(|| 0.into())))
  }
}

static_function! {
  pub fn sum(values: &Value) -> Result<Value> {
    match values {
      Value::Array(items) if items.iter().all(|item| matches!(item, Value::Number(num) if num.is_u64())) => {
        Ok(items.iter().filter_map(|item| item.as_u64()).sum::<u64>().into())
      }
      Value::Array(items) if items.iter().all(|item| matches!(item, Value::Number(num) if num.is_i64())) => {
        Ok(items.iter().filter_map(|item| item.as_i64()).sum::<i64>().into())
      }
      Value::Array(items) if items.iter().all(|item| item.is_number()) => {
        Ok(items.iter().filter_map(|item| item.as_f64()).sum::<f64>().into())
      }
      Value::Null => Ok(Value::Null),
      _ => Err(JsltError::InvalidInput(
        "Input of round must be array full of numbers".to_string(),
      )),
    }
  }
}

static_function! {
  pub fn r#mod(left: &Value, right: &Value) -> Result<Value> {
    match (left, right) {
      (Value::Number(left), Value::Number(right)) if left.is_i64() && right.is_i64() => {
        Ok(Value::Number(
          left
            .as_i64()
            .expect("Should be i64")
            .rem_euclid(right.as_i64().expect("Should be i64"))
            .into(),
        ))
      }
      (_, Value::Null) | (Value::Null, _) => Ok(Value::Null),
      _ => Err(JsltError::InvalidInput(
        "Input of mod must be natural numbers".to_string(),
      )),
    }
  }
}

fn hash_value<T: Hasher>(hasher: &mut T, value: &Value) {
  match value {
    Value::Array(array) => {
      for item in array {
        hash_value(hasher, item);
      }
    }
    Value::Bool(value) => value.hash(hasher),
    Value::Number(value) => value.hash(hasher),
    Value::Null => 0_u8.hash(hasher),
    Value::String(value) => value.hash(hasher),
    Value::Object(map) => {
      for (key, value) in map {
        key.hash(hasher);
        hash_value(hasher, value);
      }
    }
  }
}

static_function! {
  pub fn hash_int(value: &Value) -> Result<Value> {
    let mut hasher = DefaultHasher::new();
    hash_value(&mut hasher, value);
    Ok(hasher.finish().into())
  }
}

// String

static_function! {
  pub fn is_string(maybe_string: &Value) -> Result<Value> {
    Ok(Value::Bool(matches!(maybe_string, Value::String(_))))
  }
}

#[inline]
fn string_cast(value: &Value) -> String {
  match value {
    Value::String(value) => value.clone(),
    _ => value.to_string(),
  }
}

static_function! {
  pub fn string(value: &Value) -> Result<Value> {
    Ok(string_cast(value).into())
  }
}

static_function! {
  pub fn test(value: &Value, re: &Value) -> Result<Value> {
    match (value, re) {
      (Value::Null, _) => Ok(Value::Bool(false)),
      (Value::String(value), Value::String(re)) => {
        let re = Regex::new(re)
          .map_err(|err| JsltError::Unknown(format!("Unexpected error when parsing regex: {err}")))?;
        Ok(Value::Bool(re.is_match(value)))
      }
      _ => Err(JsltError::InvalidInput(
        "Input of lowercase must be string".to_string(),
      )),
    }
  }
}

static_function! {
  pub fn capture(_input: &Value, _regex: &Value) -> Result<Value> {
    unimplemented!()
  }
}

static_function! {
  pub fn split(value: &Value, re: &Value) -> Result<Value> {
    match (value, re) {
      (Value::Null, _) => Ok(Value::Null),
      (Value::String(value), Value::String(re)) => {
        let re = Regex::new(re)
          .map_err(|err| JsltError::Unknown(format!("Unexpected error when parsing re: {err}")))?;

        Ok(Value::Array(
          re.split(value)
            .map(str::to_owned)
            .map(Value::String)
            .collect(),
        ))
      }
      _ => Err(JsltError::InvalidInput(
        "Input of lowercase must be string".to_string(),
      )),
    }
  }
}

static_function! {
  pub fn join(array: &Value, separator: &Value) -> Result<Value> {
    match array {
      Value::Array(items) => {
        let separator = separator
          .as_str()
          .map(str::to_owned)
          .unwrap_or_else(|| separator.to_string());

        Ok(
          items
            .iter()
            .map(|item| {
              item
                .as_str()
                .map(str::to_owned)
                .unwrap_or_else(|| item.to_string())
            })
            .intersperse(separator)
            .collect::<String>()
            .into(),
        )
      }
      Value::Null => Ok(Value::Null),
      _ => Err(JsltError::InvalidInput(
        "Input of round must be array full of numbers".to_string(),
      )),
    }
  }
}

static_function! {
  pub fn lowercase(string: &Value) -> Result<Value> {
    match string {
      Value::String(value) => Ok(value.to_lowercase().into()),
      Value::Null => Ok(Value::Null),
      _ => Err(JsltError::InvalidInput(
        "Input of lowercase must be string".to_string(),
      )),
    }
  }
}

static_function! {
  pub fn uppercase(string: &Value) -> Result<Value> {
    match string {
      Value::String(value) => Ok(value.to_uppercase().into()),
      Value::Null => Ok(Value::Null),
      _ => Err(JsltError::InvalidInput(
        "Input of uppercase must be string".to_string(),
      )),
    }
  }
}

static_function! {
  pub fn sha256_hex(value: &Value) -> Result<Value> {
    match value {
      Value::Null => Ok(Value::Null),
      _ => Ok(sha256::digest(string_cast(value)).into())
    }
  }
}

static_function! {
  pub fn starts_with(_tested: &Value, _prefix: &Value) -> Result<Value> {
    unimplemented!()
  }
}

static_function! {
  pub fn ends_with(_tested: &Value, _prefix: &Value) -> Result<Value> {
    unimplemented!()
  }
}

static_function! {
  pub fn from_json(string: &Value, fallback: Option<&Value>) -> Result<Value> {
    match (string, fallback) {
      (Value::String(value), Some(fallback)) => Ok(serde_json::from_str(value).ok().unwrap_or_else(|| fallback.clone())),
      (Value::String(value), None) => Ok(serde_json::from_str(value)?),
      (Value::Null, _) => Ok(Value::Null),
      _ => Err(JsltError::InvalidInput(
        "Input of from-json must be string".to_string(),
      )),
    }
  }
}

static_function! {
  pub fn to_json(value: &Value) -> Result<Value> {
    Ok(Value::String(serde_json::to_string(value)?))
  }
}

static_function! {
  pub fn replace(_value: &Value, _regexp: &Value, _out: &Value) -> Result<Value> {
    unimplemented!()
  }
}

static_function! {
  pub fn trim(string: &Value) -> Result<Value> {
    match string {
      Value::String(value) => Ok(value.trim().into()),
      Value::Null => Ok(Value::Null),
      _ => Ok(string.to_string().trim().into())
    }
  }
}

pub fn uuid(arguments: &[Value]) -> Result<Value> {
  if arguments.is_empty() {
    return Ok(Value::String(Uuid::new_v4().hyphenated().to_string()));
  }

  let uuid = match (&arguments[0], &arguments[1]) {
    (Value::Number(left), Value::Number(right)) if left.is_u64() && right.is_u64() => {
      unimplemented!()
      // Uuid::from_u64_pair(
      //   left.as_u64().expect("should be u64"),
      //   right.as_u64().expect("should be u64"),
      // )
    }
    (Value::Null, Value::Null) => Uuid::nil(),
    _ => {
      return Err(JsltError::InvalidInput(
        "Input of uuid must be either empty or with 2 numbers or nulls for zeroed".to_string(),
      ))
    }
  };

  Ok(Value::String(uuid.hyphenated().to_string()))
}

// Boolean

#[inline]
fn boolean_cast(value: &Value) -> bool {
  match value {
    Value::Array(value) => !value.is_empty(),
    Value::Bool(value) => *value,
    Value::Number(value) => {
      !(value.as_u64() == Some(0) || value.as_i64() == Some(0) || value.as_f64() == Some(0.0))
    }
    Value::Null => false,
    Value::Object(value) => !value.is_empty(),
    Value::String(value) => !value.is_empty(),
  }
}

static_function! {
  pub fn boolean(value: &Value) -> Result<Value> {
    Ok(Value::Bool(boolean_cast(value)))
  }
}

static_function! {
  pub fn not(value: &Value) -> Result<Value> {
    Ok(Value::Bool(!boolean_cast(value)))
  }
}

static_function! {
  pub fn is_boolean(maybe_boolean: &Value) -> Result<Value> {
    Ok(Value::Bool(matches!(maybe_boolean, Value::Bool(_))))
  }
}

// Object

static_function! {
  pub fn is_object(maybe_object: &Value) -> Result<Value> {
    Ok(Value::Bool(matches!(maybe_object, Value::Object(_))))
  }
}

static_function! {
  pub fn get_key(object: &Value, key: &Value, fallback: Option<&Value>) -> Result<Value> {
    match (object, key) {
      (Value::Object(map), Value::String(key)) => match (map.get(key), fallback) {
        (Some(Value::Null) | None, Some(fallback)) => Ok(fallback.clone()),
        (Some(value), _) => Ok(value.clone()),
        (None, None) => Ok(Value::Null),
      },
      _ => Err(JsltError::InvalidInput(
        "Input of get-key must be object with string key".to_string(),
      )),
    }
  }
}

// Array

static_function! {
  pub fn array(value: &Value) -> Result<Value> {
    match value {
      Value::Object(map) => Ok(Value::Array(
        map
          .iter()
          .map(|(key, value)| json!({ "key": key, "value": value }))
          .collect(),
      )),
      Value::Array(_) => Ok(value.clone()),
      Value::Null => Ok(Value::Null),
      Value::Number(_) | Value::Bool(_) | Value::String(_) => Err(JsltError::InvalidInput(
        "Input of array must not be string, number or boolean".to_string(),
      )),
    }
  }
}

static_function! {
  pub fn is_array(maybe_array: &Value) -> Result<Value> {
    Ok(Value::Bool(matches!(maybe_array, Value::Array(_))))
  }
}

static_function! {
  pub fn flatten(_array: &Value) -> Result<Value> {
    unimplemented!()
  }
}

static_function! {
  pub fn all(array: &Value) -> Result<Value> {
    match array {
      Value::Array(array) => {
        Ok(Value::Bool(array.iter().all(boolean_cast)))
      }
      Value::Null => Ok(Value::Null),
      _ => Err(JsltError::InvalidInput(
        "Input of all must be array".to_string(),
      ))
    }
  }
}

static_function! {
  pub fn any(array: &Value) -> Result<Value> {
    match array {
      Value::Array(array) => {
        Ok(Value::Bool(array.iter().any(boolean_cast)))
      }
      Value::Null => Ok(Value::Null),
      _ => Err(JsltError::InvalidInput(
        "Input of all must be array".to_string(),
      ))
    }
  }
}

static_function! {
  pub fn zip(left: &Value, right: &Value) -> Result<Value> {
    match (left, right) {
      (_, Value::Null) | (Value::Null, _) => Ok(Value::Null),
      (Value::Array(left), Value::Array(right)) if left.len() >= right.len() => {
        Ok(Value::Array(left.iter().cloned().zip(right.iter().cloned()).map(|(left, right)| Value::Array(vec![left, right])).collect()))
      }
      _ => Err(JsltError::InvalidInput(
        "Input of zip must be two arrays (right must be at least as long as left)".to_string(),
      ))
    }
  }
}

static_function! {
  pub fn zip_with_index(_values: &Value) -> Result<Value> {
    unimplemented!()
  }
}

static_function! {
  pub fn index_of(array: &Value, value: &Value) -> Result<Value> {
    match array {
      Value::Array(array) => {
        match array.iter().enumerate().find(|(_, item)| *item == value) {
          Some((index, _)) => Ok((index as u64).into()),
          None => Ok((-1).into())
        }
      }
      Value::Null => Ok(Value::Null),
      _ => Err(JsltError::InvalidInput(
        "Input of index-of must be array".to_string(),
      ))
    }
  }
}

// Time

static_function! {
  pub fn now() -> Result<Value> {
    let now = SystemTime::now()
      .duration_since(SystemTime::UNIX_EPOCH)
      .expect("Should be valid");

    Ok((now.as_secs_f64()).into())
  }
}

static_function! {
  pub fn parse_time(_time: &Value, _format: &Value, _fallback: Option<&Value>) -> Result<Value> {
    unimplemented!()
  }
}

static_function! {
  pub fn format_time(_timestamp: &Value, _format: &Value, _timezone: Option<&Value>) -> Result<Value> {
    unimplemented!()
  }
}

// Miscellaneous

static_function! {
  pub fn parse_url(url: &Value) -> Result<Value> {
    let url: Url = serde_json::from_value(url.clone())?;

    Ok(json!({
      "scheme": url.scheme(),
      "userinfo": match url.password() {
        Some(password) => format!("{}:{password}", url.username()),
        None => url.username().to_owned(),
      },
      "host": url.host_str(),
      "port": url.port(),
      "path": url.path(),
      "query": url.query(),
      "parameters": url.query_pairs().fold(serde_json::Map::new(), |mut map, (key, value)| {
        map.entry(key)
        .or_insert_with(|| json!([]))
          .as_array_mut().expect("Should be array").push(Value::String(value.into()));
        map
      }),
      "fragment": url.fragment(),
    }))
  }
}
