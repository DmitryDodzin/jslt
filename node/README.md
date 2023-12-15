# JSLT

### Everyones favorite [XSLT](https://www.w3schools.com/xml/xsl_intro.asp) but for json

Node[\*](https://crates.io/crates/jslt) port for Schibsted's [jslt](https://github.com/schibsted/jslt#jslt)

```typescript
import { compile, transform } from 'jslt-node';

const SCHEMA = compile(
  `
    {
      "result" : {
        for (.menu.popup.menuitem)
          .value : .onclick
      }
    }
  `
);

const input = {
  menu: {
    popup: {
      menuitem: [
        {
          value: 'Open',
          onclick: 'OpenDoc()'
        },
        {
          value: 'Close',
          onclick: 'CloseDoc()'
        }
      ]
    }
  }
};

/*
{
  result : {
    Open: 'OpenDoc()',
    Close: 'CloseDoc()'
  }
}
*/
console.log(transform(SCHEMA, input));
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
- [x] `def <name>(<name>, <name>...) <expr> `
- [x] `// <anything up to end of line>`
- [x] `{ <key> : <expr> }`
- [ ] `{ <key> : <expr>, * : . }`
- [x] `5 * 7 + 23.2`
- [x] `7 < 5`
- [x] `7 < 5 and .foo == "yes"`
 
Based on [Quick reference](https://github.com/schibsted/jslt#quick-references)

