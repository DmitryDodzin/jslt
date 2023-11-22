use neon::prelude::*;
use serde_json::Value as JsonValue;

pub fn from_json_to_value<'c, C: Context<'c>>(
  cx: &mut C,
  value: JsonValue,
) -> JsResult<'c, JsValue> {
  match value {
    JsonValue::Array(values) => {
      let output = cx.empty_array();

      for (index, value) in values.into_iter().enumerate() {
        let value = from_json_to_value(cx, value)?;

        output.set(cx, index as u32, value)?;
      }

      Ok(output.upcast())
    }
    JsonValue::Bool(value) => Ok(cx.boolean(value).upcast()),
    JsonValue::Object(values) => {
      let output = cx.empty_object();

      for (key, value) in values {
        let key = cx.string(key);
        let value = from_json_to_value(cx, value)?;

        output.set(cx, key, value)?;
      }

      Ok(output.upcast())
    }
    JsonValue::Null => Ok(cx.null().upcast()),
    JsonValue::Number(value) => match value.as_f64() {
      Some(value) => Ok(cx.number(value).upcast()),
      None => cx.throw_range_error(format!("Could not confine number value to f64 ({value})")),
    },
    JsonValue::String(value) => Ok(cx.string(value).upcast()),
  }
}

pub fn from_value_to_json<'c, C: Context<'c>>(
  cx: &mut C,
  value: Handle<'c, JsValue>,
) -> NeonResult<JsonValue> {
  if value.is_a::<JsArray, _>(cx) {
    let array = value.downcast_or_throw::<JsArray, _>(cx)?;
    let mut output = Vec::with_capacity(array.len(cx) as usize);

    for index in 0..array.len(cx) {
      let value = array.get_value(cx, index)?;

      output.push(from_value_to_json(cx, value)?);
    }

    return Ok(JsonValue::Array(output));
  }

  if value.is_a::<JsBoolean, _>(cx) {
    return Ok(
      value
        .downcast_or_throw::<JsBoolean, _>(cx)?
        .value(cx)
        .into(),
    );
  }

  if value.is_a::<JsObject, _>(cx) {
    let value = value.downcast_or_throw::<JsObject, _>(cx)?;

    let map = value
      .get_own_property_names(cx)?
      .to_vec(cx)?
      .into_iter()
      .map(|key_value| {
        let key = key_value.downcast_or_throw::<JsString, _>(cx)?.value(cx);
        let value = value.get_value(cx, key_value)?;
        let value = from_value_to_json(cx, value)?;

        Ok((key, value))
      })
      .collect::<NeonResult<_>>()?;

    return Ok(JsonValue::Object(map));
  }

  if value.is_a::<JsNull, _>(cx) {
    return Ok(JsonValue::Null);
  }

  if value.is_a::<JsNumber, _>(cx) {
    let num = value.downcast_or_throw::<JsNumber, _>(cx)?.value(cx);

    return if num.trunc() == num {
      Ok((num as i64).into())
    } else {
      Ok(num.into())
    };
  }

  if value.is_a::<JsString, _>(cx) {
    return Ok(value.downcast_or_throw::<JsString, _>(cx)?.value(cx).into());
  }

  cx.throw_type_error(format!("Unable to convert value to json ({value:?})"))
}
