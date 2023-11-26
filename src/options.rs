pub struct Options {
    pub text_identifier: String,
    pub attribute_prefix: String,
    pub derive: String,
}

impl Options {
    pub fn quick_xml_de() -> Self {
        Self {
            text_identifier: "$text".to_string(),
            attribute_prefix: '@'.to_string(),
            derive: "Serialize, Deserialize".to_string(),
        }
    }

    pub fn serde_xml_rs() -> Self {
        Self {
            text_identifier: "$value".to_string(),
            attribute_prefix: "".to_string(),
            derive: "Serialize, Deserialize".to_string(),
        }
    }

    pub fn derive(mut self, derive: String) -> Self {
        self.derive = derive;
        self
    }
}
