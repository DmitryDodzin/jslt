use std::io::{BufRead, BufReader, Write};

use clap::Parser;
use clio::{Input, Output};
use jslt::Jslt;

#[derive(Debug, Parser)]
struct Args {
  /// Jslt Schema
  schema: String,

  /// Use instead of input
  #[clap(long, conflicts_with = "input")]
  raw: Option<String>,

  /// When the input contains json values on each line
  #[clap(long, short, conflicts_with = "raw")]
  multiline: bool,

  /// Input file path
  #[clap(value_parser, default_value = "-", conflicts_with = "raw")]
  input: Input,

  /// Output file path
  #[clap(long, short, value_parser, default_value = "-")]
  output: Output,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let mut args = Args::parse();

  let jslt: Jslt = args.schema.parse()?;

  if args.multiline {
    for line in BufReader::new(args.input).lines().take_while(Result::is_ok) {
      let value = serde_json::from_str(&line.expect("line should be filtered to only success"))?;

      writeln!(args.output, "{}", jslt.transform_value(&value)?)?;
    }
  } else {
    let value = match args.raw {
      Some(raw) => serde_json::from_str(&raw)?,
      None => serde_json::from_reader(args.input)?,
    };

    writeln!(args.output, "{}", jslt.transform_value(&value)?)?;
  }

  Ok(())
}
