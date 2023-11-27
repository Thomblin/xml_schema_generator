//! run the xml_schema_generator lib as CLI
//! read an xml file
//! print the resulting struct either to stdout or save in a file
use log::{error, info};
use std::env;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::process;

use quick_xml::reader::Reader;

use xml_schema_generator::into_struct;

fn main() {
    env_logger::init();

    let args: Vec<String> = env::args().collect();

    let config = Config::build(&args).unwrap_or_else(|err| {
        error!("{err}");
        process::exit(1);
    });

    run(config).unwrap_or_else(|err| {
        error!("{err}");
        process::exit(1);
    });
}

// read file, generate struct and print/store the result
fn run(config: Config) -> Result<(), Box<dyn std::error::Error>> {
    info!("read {}", config.input_path);
    let xml = fs::read_to_string(config.input_path)?;
    let mut reader = Reader::from_str(&xml);

    let root = into_struct(&mut reader)?;

    let struct_as_string = "use serde::{Deserialize, Serialize};\n\n".to_owned()
        + &root.to_serde_struct(&Options::quick_xml_de());

    match config.output_path {
        Some(output_path) => {
            let mut output = File::create(&output_path)?;
            write!(output, "{}", struct_as_string)?;
            info!("struct written to '{}'", output_path);
        }
        None => {
            println!("{}", struct_as_string);
        }
    }

    Ok(())
}

// store input and output options
struct Config {
    input_path: String,
    output_path: Option<String>,
}

impl Config {
    // parse CLI arguments
    pub fn build(args: &[String]) -> Result<Config, &'static str> {
        if args.len() < 2 {
            return Err("error: missing file operand\nUsage: xml_schema_generator SOURCE [DEST]");
        }

        let input_path = args[1].clone();

        let output_path = match args.len() > 2 {
            true => Some(args[2].clone()),
            false => None,
        };

        Ok(Config {
            input_path,
            output_path,
        })
    }
}
