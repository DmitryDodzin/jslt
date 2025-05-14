#![feature(try_blocks)]

use std::io::{BufRead, BufReader, Read, Write};

use clap::Parser;
use clio::{Input, Output};
use jslt::Jslt;

#[derive(Debug, Parser)]
#[command(version, about, styles = jslt::_binary::get_clap_styles())]
struct Args {
  /// Jslt Schema
  #[clap(conflicts_with = "schema_file")]
  schema: Option<String>,

  #[clap(long, short = 'f', conflicts_with = "schema")]
  schema_file: Option<Input>,

  /// Use instead of input
  #[clap(long, conflicts_with = "input")]
  text: Option<String>,

  /// When the input contains json values on each line
  #[clap(long, short, conflicts_with = "text")]
  multiline: bool,

  /// Input file path
  #[clap(value_parser, default_value = "-", conflicts_with = "text")]
  input: Input,

  /// Pretty print output
  #[clap(long, short)]
  pretty: bool,

  /// Output file path
  #[clap(long, short, value_parser, default_value = "-")]
  output: Output,
}

fn main() {
  let mut args = Args::parse();

  let result: Result<(), Box<dyn std::error::Error>> = try {
    let schema = match (args.schema_file, args.schema) {
      (None, Some(schema)) => schema,
      (Some(mut schema_file), None) => {
        let mut output = String::new();
        schema_file.read_to_string(&mut output)?;
        output
      }
      _ => unreachable!(),
    };

    let jslt: Jslt = schema.parse()?;

    if args.multiline {
      for line in BufReader::new(args.input).lines().take_while(Result::is_ok) {
        let value = serde_json::from_str(&line.expect("line should be filtered to only success"))?;

        let value = jslt.transform_value(&value)?;

        if args.pretty {
          writeln!(args.output, "{}", serde_json::to_string_pretty(&value)?)?;
        } else {
          writeln!(args.output, "{value}")?;
        }
      }
    } else {
      let value = match args.text {
        Some(text) => serde_json::from_str(&text)?,
        None => serde_json::from_reader(args.input)?,
      };

      let value = jslt.transform_value(&value)?;

      if args.pretty {
        writeln!(args.output, "{}", serde_json::to_string_pretty(&value)?)?;
      } else {
        writeln!(args.output, "{value}")?;
      }
    }
  };

  if let Err(err) = result {
    eprintln!("{err}");
  }
}
