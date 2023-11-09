mod lexer;

pub use lexer::*;

use crate::error::Result;

pub trait Parse<'s>: Sized {
  fn parse<T>(tokens: &mut T) -> Result<Self>
  where
    T: Iterator<Item = Result<SpannedToken<'s>>>;
}
