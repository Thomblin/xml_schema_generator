//! run the xml_schema_generator lib as CLI
//! read an xml file
//! print the resulting struct either to stdout or save in a file
mod args;

use args::Args;
use clap::Parser;
use log::{error, info};
use quick_xml::reader::Reader;
use std::fs::File;
use std::io::Write;
use std::process;
use xml_schema_generator::{into_struct, read_file_as_utf8, Options};

fn main() {
    #[cfg(feature = "env_logger")]
    env_logger::init();

    let config = Args::parse();

    run(config).unwrap_or_else(|err| {
        error!("{err}");
        #[cfg(not(feature = "env_logger"))]
        eprintln!("{err}");
        process::exit(1);
    });
}

// read file, generate struct and print/store the result
fn run(config: Args) -> Result<(), Box<dyn std::error::Error>> {
    info!("read {}", config.input_path);
    let xml = read_file_as_utf8(config.input_path)?;
    let mut reader = Reader::from_str(&xml);

    let root = into_struct(&mut reader)?;

    let mut options: Options = config.parser.into();
    options = options.derive(&config.derive);
    options.sort = config.sort.into();

    let struct_as_string =
        "use serde::{Deserialize, Serialize};\n\n".to_owned() + &root.to_serde_struct(&options);

    match config.output_path {
        Some(output_path) => {
            let mut output = File::create(&output_path)?;
            write!(output, "{struct_as_string}")?;
            info!("struct written to '{output_path}'");
        }
        None => {
            println!("{struct_as_string}");
        }
    }

    Ok(())
}
