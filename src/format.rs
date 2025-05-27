use std::fmt;

pub struct PadAdapter<'buf, 'state> {
  buf: &'buf mut (dyn fmt::Write + 'buf),
  state: &'state mut PadAdapterState,
}

pub struct PadAdapterState {
  on_newline: bool,
}

impl Default for PadAdapterState {
  fn default() -> Self {
    PadAdapterState { on_newline: true }
  }
}

impl<'buf, 'state> PadAdapter<'buf, 'state> {
  pub fn wrap<'slot, 'fmt: 'buf + 'slot>(
    fmt: &'fmt mut Formatter<'_>,
    slot: &'slot mut Option<Self>,
    state: &'state mut PadAdapterState,
  ) -> Formatter<'slot> {
    fmt.wrap_buf(move |buf| slot.insert(PadAdapter { buf, state }))
  }
}

impl fmt::Write for PadAdapter<'_, '_> {
  fn write_str(&mut self, s: &str) -> fmt::Result {
    for s in s.split_inclusive('\n') {
      if self.state.on_newline {
        self.buf.write_str("  ")?;
      }

      self.state.on_newline = s.ends_with('\n');
      self.buf.write_str(s)?;
    }

    Ok(())
  }

  fn write_char(&mut self, c: char) -> fmt::Result {
    if self.state.on_newline {
      self.buf.write_str("  ")?;
    }
    self.state.on_newline = c == '\n';
    self.buf.write_char(c)
  }
}

pub struct Formatter<'a> {
  buf: &'a mut (dyn fmt::Write + 'a),
}

impl<'a> Formatter<'a> {
  pub fn from_fmt(buf: &'a mut fmt::Formatter) -> Self {
    Formatter { buf }
  }
}

impl Formatter<'_> {
  fn wrap_buf<'b, 'c, F>(&'b mut self, wrap: F) -> Formatter<'c>
  where
    'b: 'c,
    F: FnOnce(&'b mut (dyn fmt::Write + 'b)) -> &'c mut (dyn fmt::Write + 'c),
  {
    Formatter {
      // We want to change this
      buf: wrap(self.buf),
    }
  }

  pub fn write_fmt(&mut self, fmt: fmt::Arguments<'_>) -> fmt::Result {
    self.buf.write_fmt(fmt)
  }
}

impl fmt::Write for Formatter<'_> {
  fn write_str(&mut self, s: &str) -> fmt::Result {
    self.buf.write_str(s)
  }
}

pub trait Display {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result;
}

pub fn format<T>(value: &T) -> Formatted<T> {
  Formatted(value)
}

#[derive(Debug)]
pub struct Formatted<'a, T: ?Sized>(&'a T);

impl<T> fmt::Display for Formatted<'_, T>
where
  T: Display,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Formatted(display) = self;
    display.fmt(&mut Formatter::from_fmt(f))
  }
}
