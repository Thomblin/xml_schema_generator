//! defines the command line arguments that can be passed to xml_schema_generator
use clap::Parser;
use xml_schema_generator::{Options, SortBy};

/// collection of command line arguments
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// define the parser that is used to parse the resulting struct
    #[clap(short, long, default_value_t, value_enum)]
    pub parser: ParserArg,
    /// define the #derive attribute to be added to each resulting struct
    #[clap(short, long, default_value = "Serialize, Deserialize")]
    pub derive: String,
    /// sorting order for attributes and children (use unsorted for mixed content, name for data-oriented XML)
    #[clap(short, long, default_value_t, value_enum)]
    pub sort: SortByArg,
    /// xml file that shall be parsed
    pub input_path: String,
    /// rust file to store the result, or none to print to stdout
    pub output_path: Option<String>,
}

/// supported parser variants
#[derive(clap::ValueEnum, Clone, Default, Debug)]
pub enum SortByArg {
    /// preserve document order (ideal for mixed content XML with comments, text, and alternating elements)
    #[default]
    Unsorted,
    /// sort alphabetically by name (ideal for data-oriented XML, config files, and API responses)
    Name,
}

impl From<SortByArg> for SortBy {
    fn from(val: SortByArg) -> Self {
        match val {
            SortByArg::Unsorted => SortBy::Unsorted,
            SortByArg::Name => SortBy::XmlName,
        }
    }
}

/// supported parser variants
#[derive(clap::ValueEnum, Clone, Default, Debug)]
pub enum ParserArg {
    #[default]
    QuickXmlDe,
    SerdeXmlRs,
}

/// simplify conversion of argument into parsing options
impl From<ParserArg> for Options {
    fn from(val: ParserArg) -> Self {
        match val {
            ParserArg::QuickXmlDe => Options::quick_xml_de(),
            ParserArg::SerdeXmlRs => Options::serde_xml_rs(),
        }
    }
}
