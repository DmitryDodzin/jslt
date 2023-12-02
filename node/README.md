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
