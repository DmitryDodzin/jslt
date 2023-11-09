pub use logos::Span;
use logos::{Lexer, Logos};

use crate::{error::Result, JsltError};

#[derive(Debug)]
pub struct Spanned<'s, T> {
  pub item: T,
  pub span: Span,
  pub slice: &'s str,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
pub enum Token {
  #[token(".")]
  Accessor,

  #[token("{")]
  BraceOpen,

  #[token("}")]
  BraceClose,

  #[token(":")]
  Colon,

  #[regex("\\\\.*\n")]
  Comment,

  #[token("else")]
  Else,

  #[regex("[a-zA-Z]+")]
  Identifier,

  #[token("for")]
  For,

  #[token("if")]
  If,

  #[regex(r"\d+")]
  Number,

  #[token("(")]
  ParenOpen,

  #[token(")")]
  ParenClose,

  #[token(",")]
  Period,

  #[token("[")]
  SquareOpen,

  #[token("]")]
  SquareClose,

  #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#)]
  String,
}

pub type SpannedToken<'s> = Spanned<'s, Token>;

pub struct TokenIter<'s> {
  lex: Lexer<'s, Token>,
}

impl<'s> TokenIter<'s> {
  pub fn new(input: &'s str) -> Self {
    TokenIter {
      lex: Token::lexer(input),
    }
  }
}

impl<'s> Iterator for TokenIter<'s> {
  type Item = Result<SpannedToken<'s>>;

  fn next(&mut self) -> Option<Self::Item> {
    self.lex.next().map(|res| {
      res
        .map(|item| Spanned {
          item,
          span: self.lex.span(),
          slice: self.lex.slice(),
        })
        .map_err(|_| JsltError::InvalidToken(self.lex.slice().to_string(), self.lex.span()))
    })
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn accessor() {
    let mut lex = Token::lexer(".foo.bar.baz");

    assert_eq!(lex.next(), Some(Ok(Token::Accessor)));
    assert_eq!(lex.next(), Some(Ok(Token::Identifier)));
    assert_eq!(lex.span(), 1..4);
    assert_eq!(lex.slice(), "foo");
    assert_eq!(lex.next(), Some(Ok(Token::Accessor)));
    assert_eq!(lex.next(), Some(Ok(Token::Identifier)));
    assert_eq!(lex.span(), 5..8);
    assert_eq!(lex.slice(), "bar");
    assert_eq!(lex.next(), Some(Ok(Token::Accessor)));
    assert_eq!(lex.next(), Some(Ok(Token::Identifier)));
    assert_eq!(lex.span(), 9..12);
    assert_eq!(lex.slice(), "baz");
  }
}
