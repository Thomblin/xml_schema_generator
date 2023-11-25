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
//! use xml_schema_generator::{Element, into_struct};
//!
//! let xml = "<xml>...</xml>";
//! let mut reader = Reader::from_str(xml);
//!
//! if let Ok(root) = into_struct(&mut reader) {
//!     let struct_as_string = root.to_serde_struct();
//!     // save this result as a .rs file and use it to (de)serialize an XML document with serde
//! }
//! ```
//!
//! How to run the binary
//! ```bash
//!     # parse input.xml and print struct to stdout
//!     $ cargo run --features="env_logger" -- input.xml
//!     
//!     # parse input.xml and store struct to output.rs
//!     $ cargo run --features="env_logger" -- input.xml output.rs
//! ```
#[macro_use]
extern crate log;

mod element;
mod necessity;
mod parser;

pub use element::Element;
pub use necessity::{merge_necessity, Necessity};
pub use parser::{into_struct, ParserError};

#[cfg(test)]
mod tests {
    use crate::into_struct;
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

        assert_eq!(expected, root.to_serde_struct());
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

        assert_eq!(expected, root.to_serde_struct());
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
    #[serde(rename = \"ID\")]
    pub id: String,
    #[serde(rename = \"$text\")]
    pub text: Option<String>,
}

";

        assert_eq!(expected, root.to_serde_struct());
    }
}
