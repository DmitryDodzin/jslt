use std::sync::LazyLock;

use derive_syn_parse::Parse;
use quote::{format_ident, quote};
use syn::parse_macro_input;

static CRATE_NAME: LazyLock<&str> = LazyLock::new(|| {
  if "jslt" == std::env::var("CARGO_PKG_NAME").unwrap_or_default() {
    "crate"
  } else {
    "jslt"
  }
});

#[proc_macro_attribute]
pub fn static_function(
  _attr: proc_macro::TokenStream,
  item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  let mut item = parse_macro_input!(item as syn::ItemFn);

  let mut vis = syn::Visibility::Inherited;
  std::mem::swap(&mut item.vis, &mut vis);

  // replace original ident with `_wrapped_implementation` and store the original in ident
  let mut ident = format_ident!("_wrapped_implementation");
  std::mem::swap(&mut item.sig.ident, &mut ident);

  let arguments_ident = format_ident!("arguments");

  let arguments = item.sig.inputs.iter().enumerate().map(|(index, item)| {
    let required = !matches!(item, syn::FnArg::Typed(syn::PatType { ty, .. }) if matches!(**ty, syn::Type::Path(_)));
    let required = required.then(|| quote! { .unwrap_or(&serde_json::Value::Null) });

    quote! {
      #arguments_ident .get(#index) #required
    }
  });

  let jslt = format_ident!("{}", *CRATE_NAME);

  quote! {
    #vis fn #ident(#arguments_ident: &[serde_json::Value]) -> Result<serde_json::Value, #jslt::error::JsltError> {
      #item

      _wrapped_implementation( #(#arguments,)* )
    }
  }.into()
}

#[derive(Parse, Debug)]
struct ExpectInnerArgs {
  pairs: syn::Ident,
  _period_token: syn::Token![,],
  rule: syn::Type,
}

#[proc_macro]
pub fn expect_inner(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let ExpectInnerArgs { pairs, rule, .. } = parse_macro_input!(item as ExpectInnerArgs);

  let jslt = format_ident!("{}", *CRATE_NAME);

  quote! {
    {
      let Some(pair) = #pairs.next() else {
        return Err(#jslt::error::JsltError::UnexpectedEnd);
      };

      let rule = pair.as_rule();

      if !matches!(rule, #rule) {
        return Err(#jslt::error::JsltError::UnexpectedInput(
          #rule,
          rule,
          pair.as_str().to_owned(),
        ));
      }

      Ok::<Pairs<_>, #jslt::error::JsltError>(pair.into_inner())
    }
  }
  .into()
}
