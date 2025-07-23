mod utils;

use quick_xml::reader::Reader;
use wasm_bindgen::prelude::*;
use xml_schema_generator::{into_struct, Options};

#[wasm_bindgen]
pub fn transform_xml(s: &str) -> String {
    let mut reader = Reader::from_str(s);

    match into_struct(&mut reader) {
        Ok(root) => root.to_serde_struct(&Options::quick_xml_de()),
        Err(e) => {
            format!("{e:?}")
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::transform_xml;

    #[test]
    fn transform_xml_returns_struct() {
        let xml = "<foo>hello</foo>";

        let actual = transform_xml(xml);

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct Foo {\n",
            "    #[serde(rename = \"$text\")]\n",
            "    pub text: Option<String>,\n",
            "}\n\n"
        );

        assert_eq!(expected, actual);
    }

    #[test]
    fn transform_xml_returns_error() {
        let xml = "{foo: 'hello'}";

        let actual = transform_xml(xml);

        let expected = "ParsingError(\"invalid XML, no root element found\")";

        assert_eq!(expected, actual);
    }
}
