mod utils;

use wasm_bindgen::prelude::*;
use quick_xml::reader::Reader;
use xml_schema_generator::{into_struct, Options};

#[wasm_bindgen]
pub fn transform_xml(s: &str) -> String{
    let mut reader = Reader::from_str(s);
    
    match into_struct(&mut reader) {
        Ok(root) => {
            root.to_serde_struct(&Options::quick_xml_de())
        }
        Err(e) => {
            format!("{:?}", e)
        }
    }
}
