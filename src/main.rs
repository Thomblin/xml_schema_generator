//! run the xml_schema_generator lib as CLI
//! read an xml file
//! print the resulting struct either to stdout or save in a file
use clap::Parser;
use log::{error, info};
use std::fs;
use std::fs::File;
use std::io::Write;
use std::process;
use xml_schema_generator::Options;

use quick_xml::reader::Reader;

use xml_schema_generator::into_struct;

// store input and output options
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Config {
    /// xml file that shall be parsed
    input_path: String,
    /// rust file to store the result, or none to print to stdout
    output_path: Option<String>,
}

fn main() {
    env_logger::init();

    let config = Config::parse();

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
