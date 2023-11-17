# JSLT

### Everyones favorite [XSLT](https://www.w3schools.com/xml/xsl_intro.asp) but for json's

Rust port for Schibsted's [jslt](https://github.com/schibsted/jslt#jslt)

```rust
use jslt::Jslt;
use serde_json::json;

let jslt: Jslt = r#"
{
  "result" : {for (.menu.popup.menuitem)
    .value : .onclick
  }
}
"#
.parse()?;

let output = jslt.transform_value(
  &json!({
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
  })
)?;

assert_eq!(
  output, 
  json!({
    "result" : {
      "Open" : "OpenDoc()",
      "Close" : "CloseDoc()"
    }
  })
);
```

## Status: POC

There is very minial support for selectors, constants and for loops and no garantee this will continue any further in the current phase.

Quick support reference:

- [x] `.`
- [x] `.<name>`
- [x] `.[<index>]`
- [x] `.[<from> : <to>]`
- [ ] `if (<expr>) <expr> else <expr>`
- [ ] `let <name> = <expr>`
- [ ] `$<name>`
- [x] `[for (<expr>) <expr>]`
- [x] `{for (<expr>) <expr> : <expr>}`
- [ ] `def <name>(<name>, <name>...) <expr>	`
- [x] `// <anything up to end of line>`
- [x] `{ <key> : <expr> }`
- [ ] `{ <key> : <expr>, * : . }`
- [ ] `5 * 7 + 23.2`
- [ ] `7 < 5`
- [ ] `7 < 5 and .foo == "yes"`
 
Based on [Quick reference](https://github.com/schibsted/jslt#quick-references)

### Current Goals:

- [ ] Create context for function registration and scopes for variables
- [ ] Enable `#![no_std]` sooner than later for possible nodejs support *

\* with `std` cargo flag for regular use.
