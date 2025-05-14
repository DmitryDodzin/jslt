#![feature(try_blocks)]

use std::io::{Read, Write};

use clap::Parser;
use clio::{Input, Output};
use jslt::Jslt;

#[derive(Debug, Parser)]
#[command(version, about, styles = jslt::_binary::get_clap_styles())]
struct Args {
  /// Input file path, use '-' for stdin
  #[clap(value_parser, default_value = "-")]
  input: Input,

  /// Output file path, use '-' for stdout
  #[clap(long, short, value_parser, default_value = "-")]
  output: Output,
}

fn main() {
  let mut args = Args::parse();

  let result: Result<(), Box<dyn std::error::Error>> = try {
    let mut buffer = String::new();

    args.input.read_to_string(&mut buffer)?;

    let jslt: Jslt = buffer.parse()?;

    writeln!(args.output, "{jslt}")?;
  };

  if let Err(err) = result {
    eprintln!("{err}");
  }
}
