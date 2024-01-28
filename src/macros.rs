#[macro_export]
macro_rules! expect_inner {
  ($pairs:ident, $($rule:tt)+) => {{
    let Some(pair) = $pairs.next() else {
      return Err(JsltError::UnexpectedEnd);
    };

    let rule = pair.as_rule();

    if !matches!(rule, $($rule)*) {
      return Err(JsltError::UnexpectedInput(
        $($rule)*,
        rule,
        pair.as_str().to_owned(),
      ));
    }

    Ok::<Pairs<_>, JsltError>(pair.into_inner())
  }};
}
