#![feature(try_blocks)]

use std::io::{Read, Write};

use clap::Parser;
use clio::{Input, Output};
use jslt::Jslt;

#[derive(Debug, Parser)]
struct Args {
  /// Input file path
  #[clap(value_parser, default_value = "-")]
  input: Input,

  /// Output file path
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
