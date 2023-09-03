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
//! let mut root = Element::new(String::from("root"), Vec::new());
//! 
//! into_struct(&mut reader, &mut root);
//! 
//! let struct_as_string = root.to_serde_struct(); 
//! // save this result as a .rs file and use it to (de)serialize an XML document with serde
//! ```
//! 
//! How to run the binary
//! ```bash
//!     # parse input.xml and print struct to stdout
//!     $ cargo run -- input.xml
//!     
//!     # parse input.xml and store struct to output.rs
//!     $ cargo run -- input.xml output.rs
//! ```
#[macro_use]
extern crate log;

mod parser;
mod necessity;
mod element;

pub use parser::into_struct;
pub use element::Element;
pub use necessity::{Necessity, merge_necessity};

#[cfg(test)]
mod tests {
    use quick_xml::reader::Reader;
    use crate::{Element, into_struct};
    use pretty_assertions::assert_eq;

    #[test]
    fn parse_xml_and_return_struct_as_str() {
        let xml = "<a b=\"c\">d</a>";
        let mut reader = Reader::from_str(xml);
        let mut root = Element::new(String::from("root"), Vec::new());
        
        into_struct(&mut reader, &mut root);

        let expected = "\
#[derive(Serialize, Deserialize)]
pub struct Root {
    #[serde(rename = \"a\")]
    pub a: A,
}

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
}