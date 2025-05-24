#![feature(try_blocks)]

use std::io::{BufRead, BufReader, Read, Write};

use clap::{Args, Parser};
use clio::{Input, Output};
use jslt::Jslt;

#[derive(Args, Debug)]
#[group(required = true, multiple = false)]
struct SchemaOpts {
  /// Jslt schema content
  #[clap(long, short, conflicts_with = "schema_path")]
  schema: Option<String>,

  /// Jslt schema file
  #[clap(long, short = 'f', conflicts_with = "schema")]
  schema_path: Option<Input>,
}

#[derive(Debug, Parser)]
#[command(version, about, styles = jslt::_binary::get_clap_styles())]
struct Opts {
  #[clap(flatten)]
  schema: SchemaOpts,

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

impl SchemaOpts {
  fn into_schema(self) -> std::io::Result<String> {
    match (self.schema_path, self.schema) {
      (Some(mut schema_path), None) => {
        let mut output = String::new();
        schema_path.read_to_string(&mut output)?;

        Ok(output)
      }
      (None, Some(schema)) => Ok(schema),
      _ => unreachable!(),
    }
  }
}

fn main() {
  let mut opts = Opts::parse();

  let result: Result<(), Box<dyn std::error::Error>> = try {
    let schema = opts.schema.into_schema()?;
    let jslt: Jslt = schema.parse()?;

    if opts.multiline {
      for line in BufReader::new(opts.input).lines().take_while(Result::is_ok) {
        let value = serde_json::from_str(&line.expect("line should be filtered to only success"))?;

        let value = jslt.transform_value(&value)?;

        if opts.pretty {
          writeln!(opts.output, "{}", serde_json::to_string_pretty(&value)?)?;
        } else {
          writeln!(opts.output, "{value}")?;
        }
      }
    } else {
      let value = match opts.text {
        Some(text) => serde_json::from_str(&text)?,
        None => serde_json::from_reader(opts.input)?,
      };

      let value = jslt.transform_value(&value)?;

      if opts.pretty {
        writeln!(opts.output, "{}", serde_json::to_string_pretty(&value)?)?;
      } else {
        writeln!(opts.output, "{value}")?;
      }
    }
  };

  if let Err(err) = result {
    eprintln!("{err}");
  }
}
