//! XML Schema Generator
//!
//! # Description
//!
//! Parse an XML file and generate a Rust struct that can be used to deserialize the given XML (or serialize one)
//!
//! # Examples
//!
//! How to implement the lib
//! ```
//! use quick_xml::reader::Reader;
//! use xml_schema_generator::{into_struct, Options};
//!
//! let xml = "<xml>...</xml>";
//! let mut reader = Reader::from_str(xml);
//!
//! if let Ok(root) = into_struct(&mut reader) {
//!     let struct_as_string = root.to_serde_struct(&Options::quick_xml_de());
//!     // save this result as a .rs file and use it to (de)serialize an XML document with quick_xml::de::from_str(xml)
//! }
//!
//! // you can even parse additional xml files to extend the structure to match those files as well
//! // see examples/parse_multiple_xml_rs
//! ```
//!
//! You find more examples in the [/examples directory](https://github.com/Thomblin/xml_schema_generator/tree/main/examples)
//!
//! # Install
//!
//! from source (env_logger is optional if you do not require additional output)
//!
//! ```bash
//!     cargo install xml_schema_generator --features="env_logger"
//! ```
//!
//! or download the latest binary at [GitHub](https://github.com/Thomblin/xml_schema_generator/releases)
//!
//! How to run the binary
//! ```bash    
//!     Usage: [RUST_LOG=info] xml_schema_generator [OPTIONS] <INPUT_PATH> [OUTPUT_PATH]
//!     
//!     Arguments:
//!       <INPUT_PATH>
//!               xml file that shall be parsed
//!     
//!       [OUTPUT_PATH]
//!               rust file to store the result, or none to print to stdout
//!     
//!     Options:
//!       -p, --parser <PARSER>
//!               define the parser that is used to parse the resulting struct
//!               
//!               [default: quick-xml-de]
//!               [possible values: quick-xml-de, serde-xml-rs]
//!     
//!       -d, --derive <DERIVE>
//!               define the #derive attribute to be added to each resulting struct
//!               
//!               [default: "Serialize, Deserialize"]
//!     
//!       -s, --sort <SORT>
//!               sorting order for attributes and children
//!               
//!               [default: unsorted]
//!     
//!               Possible values:
//!               - unsorted: the order remains as found in document
//!               - name:     sort attributes and children by name (as given in XML). attributes and children are not merged
//!     
//!       -h, --help
//!               Print help (see a summary with '-h')
//!     
//!       -V, --version
//!               Print version
//! ```

#[macro_use]
extern crate log;

mod element;
mod necessity;
mod options;
mod parser;

pub use element::Element;
pub use necessity::{merge_necessity, Necessity};
pub use options::{Options, SortBy};
pub use parser::{extend_struct, into_struct, ParserError};

#[cfg(test)]
mod tests {
    use crate::{into_struct, Options};
    use pretty_assertions::assert_eq;
    use quick_xml::reader::Reader;

    #[test]
    fn parse_xml_and_return_struct_as_str() {
        let xml = "<a b=\"c\">d</a>";
        let mut reader = Reader::from_str(xml);

        let root = into_struct(&mut reader).expect("expected to successfully parse into struct");

        let expected = "\
#[derive(Serialize, Deserialize)]
pub struct A {
    #[serde(rename = \"@b\")]
    pub b: String,
    #[serde(rename = \"$text\")]
    pub text: Option<String>,
}

";

        assert_eq!(expected, root.to_serde_struct(&Options::quick_xml_de()));
    }

    // https://github.com/Thomblin/xml_schema_generator/issues/3
    #[test]
    fn parse_multiple_children_as_vec() {
        let xml = "<a><b>asd</b><b>fgh</b></a>";
        let mut reader = Reader::from_str(xml);

        let root = into_struct(&mut reader).expect("expected to successfully parse into struct");

        let expected = "\
#[derive(Serialize, Deserialize)]
pub struct A {
    pub b: Vec<String>,
}

";

        assert_eq!(expected, root.to_serde_struct(&Options::quick_xml_de()));
    }

    // https://github.com/Thomblin/xml_schema_generator/issues/5
    #[test]
    fn create_struct_names_as_pascal_case() {
        let xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<?xml-stylesheet type=\"text/xsl\" href=\"redacted.xsl\"?>
<MVCI_MODULE_DESCRIPTION>
    <PINTYPE>
        <ID>0</ID>
    </PINTYPE>
</MVCI_MODULE_DESCRIPTION>";
        let mut reader = Reader::from_str(xml);

        let root = into_struct(&mut reader).expect("expected to successfully parse into struct");

        let expected = "\
#[derive(Serialize, Deserialize)]
pub struct MvciModuleDescription {
    #[serde(rename = \"$text\")]
    pub text: Option<String>,
    #[serde(rename = \"PINTYPE\")]
    pub pintype: Pintype,
}

#[derive(Serialize, Deserialize)]
pub struct Pintype {
    #[serde(rename = \"$text\")]
    pub text: Option<String>,
    #[serde(rename = \"ID\")]
    pub id: String,
}

";

        assert_eq!(expected, root.to_serde_struct(&Options::quick_xml_de()));
    }

    #[test]
    fn remove_namespace_prefix() {
        let xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<OTA_Rate xmlns=\"http://www.opentravel.org/OTA/2003/05\"
            xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
        xsi:schemaLocation=\"http://www.opentravel.org/OTA/2003/05 OTA_Rate.xsd\">
            <Success />
</OTA_Rate>";
        let mut reader = Reader::from_str(xml);

        let root = into_struct(&mut reader).expect("expected to successfully parse into struct");

        let expected = "\
#[derive(Serialize, Deserialize)]
pub struct OtaRate {
    #[serde(rename = \"@xmlns\")]
    pub xmlns: String,
    #[serde(rename = \"@xmlns:xsi\")]
    pub xmlns_xsi: String,
    #[serde(rename = \"@schemaLocation\")]
    pub xsi_schema_location: String,
    #[serde(rename = \"$text\")]
    pub text: Option<String>,
    #[serde(rename = \"Success\")]
    pub success: Success,
}

#[derive(Serialize, Deserialize)]
pub struct Success {
}

";

        assert_eq!(expected, root.to_serde_struct(&Options::quick_xml_de()));
    }

    #[test]
    fn handle_duplicate_children_correctly() {
        let xml = "
        <UpsellCandidate>
            <DailyPriceDiff>
                <Amount xmlns=\"\">0.63</Amount>
                <Currency xmlns=\"\">EUR</Currency>
            </DailyPriceDiff>
            <TotalPriceDiff>
                <Amount xmlns=\"\">4.43</Amount>
                <Currency xmlns=\"\">EUR</Currency>
            </TotalPriceDiff>
        </UpsellCandidate>";

        let mut reader = Reader::from_str(xml);

        let root = into_struct(&mut reader).expect("expected to successfully parse into struct");

        let expected = "\
#[derive(Serialize, Deserialize)]
pub struct UpsellCandidate {
    #[serde(rename = \"$text\")]
    pub text: Option<String>,
    #[serde(rename = \"DailyPriceDiff\")]
    pub daily_price_diff: DailyPriceDiff,
    #[serde(rename = \"TotalPriceDiff\")]
    pub total_price_diff: TotalPriceDiff,
}

#[derive(Serialize, Deserialize)]
pub struct DailyPriceDiff {
    #[serde(rename = \"$text\")]
    pub text: Option<String>,
    #[serde(rename = \"Amount\")]
    pub amount: DailyPriceDiffAmount,
    #[serde(rename = \"Currency\")]
    pub currency: DailyPriceDiffCurrency,
}

#[derive(Serialize, Deserialize)]
pub struct DailyPriceDiffAmount {
    #[serde(rename = \"@xmlns\")]
    pub xmlns: String,
    #[serde(rename = \"$text\")]
    pub text: Option<String>,
}

#[derive(Serialize, Deserialize)]
pub struct DailyPriceDiffCurrency {
    #[serde(rename = \"@xmlns\")]
    pub xmlns: String,
    #[serde(rename = \"$text\")]
    pub text: Option<String>,
}

#[derive(Serialize, Deserialize)]
pub struct TotalPriceDiff {
    #[serde(rename = \"$text\")]
    pub text: Option<String>,
    #[serde(rename = \"Amount\")]
    pub amount: TotalPriceDiffAmount,
    #[serde(rename = \"Currency\")]
    pub currency: TotalPriceDiffCurrency,
}

#[derive(Serialize, Deserialize)]
pub struct TotalPriceDiffAmount {
    #[serde(rename = \"@xmlns\")]
    pub xmlns: String,
    #[serde(rename = \"$text\")]
    pub text: Option<String>,
}

#[derive(Serialize, Deserialize)]
pub struct TotalPriceDiffCurrency {
    #[serde(rename = \"@xmlns\")]
    pub xmlns: String,
    #[serde(rename = \"$text\")]
    pub text: Option<String>,
}

";

        assert_eq!(expected, root.to_serde_struct(&Options::quick_xml_de()));
    }
}
