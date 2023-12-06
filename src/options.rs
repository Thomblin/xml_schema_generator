//! configures how the resulting struct needs to be formatted to support a specific parser
//! currently supports quick_xml::de and serde_xml_rs

/// configuration of target parser
pub struct Options {
    /// representation of text content
    pub text_identifier: String,
    /// representation of element attributes
    pub attribute_prefix: String,
    /// derive attribute that shall be added to each resulting struct
    pub derive: String,
}

impl Options {
    /// format struct compatible with quick_xml::de
    pub fn quick_xml_de() -> Self {
        Self {
            text_identifier: "$text".to_string(),
            attribute_prefix: '@'.to_string(),
            derive: "Serialize, Deserialize".to_string(),
        }
    }

    /// format struct compatible with serde_xml_rs
    pub fn serde_xml_rs() -> Self {
        Self {
            text_identifier: "$text".to_string(),
            attribute_prefix: "".to_string(),
            derive: "Serialize, Deserialize".to_string(),
        }
    }

    /// define a custome #derive attribute
    pub fn derive(mut self, derive: &str) -> Self {
        self.derive = derive.to_string();
        self
    }
}
