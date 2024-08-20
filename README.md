# JSLT

### Everyones favorite [XSLT](https://www.w3schools.com/xml/xsl_intro.asp) but for json

Rust port for Schibsted's [jslt](https://github.com/schibsted/jslt#jslt)

```rust
use jslt::Jslt;
use serde_json::json;

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let jslt: Jslt = r#"
  {
    "result" : {
      for (.menu.popup.menuitem)
        .value : .onclick
    }
  }
  "#
  .parse()?;

  let input = json!({
    "menu": {
      "popup": {
        "menuitem": [
          {
            "value": "Open",
            "onclick": "OpenDoc()"
          },
          {
            "value": "Close",
            "onclick": "CloseDoc()"
          }
        ]
      }
    }
  });

  let output = jslt.transform_value(&input)?;

  assert_eq!(
    output,
    json!({
      "result" : {
        "Open" : "OpenDoc()",
        "Close" : "CloseDoc()"
      }
    })
  );

  Ok(())
}
```

## Cli

Test branch install
```bash
cargo install --features binary --git https://github.com/DmitryDodzin/jslt.git --branch qol

# Optional to install with http client with (ureq/curl)
cargo install --features binary  --features clio/http-ureq --git https://github.com/DmitryDodzin/jslt.git --branch qol
# or 
cargo install --features binary --features clio/http-curl --git https://github.com/DmitryDodzin/jslt.git --branch qol
```

Currently there are 2 binaries that ship with the libaray `jslt` and `jslt-fmt`.

```bash
echo '{"foo": "bar"}' | jslt '{ "foo_" + .foo : 2000 }'
# {"foo_bar":2000}
```


## Status: POC

There is very minial support for selectors, constants and for loops and no garantee this will continue any further in the current phase.

Quick support reference:

- [x] `.`
- [x] `.<name>`
- [x] `.[<index>]`
- [x] `.[<from> : <to>]`
- [x] `if (<expr>) <expr> else <expr>`
- [x] `let <name> = <expr>`
- [x] `$<name>`
- [x] `[for (<expr>) <expr>]`
- [x] `{for (<expr>) <expr> : <expr>}`
- [x] `def <name>(<name>, <name>...) <expr>	`
- [x] `// <anything up to end of line>`
- [x] `{ <key> : <expr> }`
- [x] `{ <key> : <expr>, * : . }`
- [x] `5 * 7 + 23.2`
- [x] `7 < 5`
- [x] `7 < 5 and .foo == "yes"`
 
Based on [Quick reference](https://github.com/schibsted/jslt#quick-references)

### Current Goals:

- [x] Create context for function registration and scopes for variables
- [ ] Enable `#![no_std]` sooner than later for possible nodejs support *

\* with `std` cargo flag for regular use.
