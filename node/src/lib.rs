use std::sync::Arc;

use jslt::Jslt;
use neon::prelude::*;

pub mod convert;

fn parse_schema_arg0(cx: &mut FunctionContext) -> NeonResult<Arc<Jslt>> {
  let maybe_compiled = cx.argument::<JsValue>(0)?;

  if maybe_compiled.is_a::<JsBox<CompiledSchema>, _>(cx) {
    let boxed = maybe_compiled.downcast_or_throw::<JsBox<CompiledSchema>, _>(cx)?;
    return Ok(boxed.0.clone());
  }

  let schema = maybe_compiled.downcast_or_throw::<JsString, _>(cx)?;

  schema
    .value(cx)
    .parse::<Jslt>()
    .map(Arc::new)
    .or_else(|err| cx.throw_error(err.to_string()))
}

fn transform(mut cx: FunctionContext) -> JsResult<JsValue> {
  let jslt = parse_schema_arg0(&mut cx)?;

  let input = match cx.argument_opt(1) {
    Some(value) => convert::from_value_to_json(&mut cx, value)?,
    None => ().into(),
  };

  let output = jslt
    .transform_value(&input)
    .or_else(|err| cx.throw_error(err.to_string()))?;

  convert::from_json_to_value(&mut cx, output)
}

fn transform_str(mut cx: FunctionContext) -> JsResult<JsString> {
  let jslt = parse_schema_arg0(&mut cx)?;
  let input = cx.argument::<JsString>(1)?;

  let input =
    serde_json::from_str(&input.value(&mut cx)).or_else(|err| cx.throw_error(err.to_string()))?;

  let output = jslt
    .transform_value(&input)
    .or_else(|err| cx.throw_error(err.to_string()))?;

  let output = serde_json::to_string(&output).or_else(|err| cx.throw_error(err.to_string()))?;

  Ok(cx.string(output))
}

#[derive(Debug)]
pub struct CompiledSchema(Arc<Jslt>);

impl Finalize for CompiledSchema {}

fn compile(mut cx: FunctionContext) -> JsResult<JsValue> {
  let jslt = parse_schema_arg0(&mut cx)?;

  Ok(cx.boxed(CompiledSchema(jslt)).upcast())
}

#[neon::main]
fn main(mut cx: ModuleContext) -> NeonResult<()> {
  cx.export_function("transform", transform)?;
  cx.export_function("transformStr", transform_str)?;
  cx.export_function("compile", compile)?;
  Ok(())
}
