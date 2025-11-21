//! configures how the resulting struct needs to be formatted to support a specific parser
//! currently supports quick_xml::de and serde_xml_rs

/// Defines the sorting order for XML attributes and child elements
pub enum SortBy {
    /// the order remains as found in document
    Unsorted,
    /// sort attributes and children by name (as given in XML). attributes and children are not merged
    XmlName,
}

/// Configuration options for generating Rust structs from XML
///
/// This struct controls how the XML schema is translated into Rust code,
/// including naming conventions and derive attributes.
pub struct Options {
    /// representation of text content
    pub text_identifier: String,
    /// representation of element attributes
    pub attribute_prefix: String,
    /// derive attribute that shall be added to each resulting struct
    pub derive: String,
    /// sorting order for attributes and children
    pub sort: SortBy,
}

impl Options {
    /// Creates options configured for the `quick_xml::de` parser
    ///
    /// Sets text identifier to "$text" and attribute prefix to '@'
    ///
    /// # Returns
    ///
    /// `Options` instance configured for quick_xml::de parser
    ///
    /// # Examples
    ///
    /// ```
    /// use xml_schema_generator::Options;
    ///
    /// let options = Options::quick_xml_de();
    /// assert_eq!("$text", options.text_identifier);
    /// assert_eq!("@", options.attribute_prefix);
    /// ```
    pub fn quick_xml_de() -> Self {
        Self {
            text_identifier: "$text".to_string(),
            attribute_prefix: '@'.to_string(),
            derive: "Serialize, Deserialize".to_string(),
            sort: SortBy::Unsorted,
        }
    }

    /// Creates options configured for the `serde_xml_rs` parser
    ///
    /// Sets text identifier to "$text" and uses no attribute prefix
    ///
    /// # Returns
    ///
    /// `Options` instance configured for serde_xml_rs parser
    ///
    /// # Examples
    ///
    /// ```
    /// use xml_schema_generator::Options;
    ///
    /// let options = Options::serde_xml_rs();
    /// assert_eq!("$text", options.text_identifier);
    /// assert_eq!("", options.attribute_prefix);
    /// ```
    pub fn serde_xml_rs() -> Self {
        Self {
            text_identifier: "$text".to_string(),
            attribute_prefix: "".to_string(),
            derive: "Serialize, Deserialize".to_string(),
            sort: SortBy::Unsorted,
        }
    }

    /// Sets a custom `#[derive(...)]` attribute for generated structs
    ///
    /// # Arguments
    ///
    /// * `derive` - Comma-separated list of traits to derive (e.g., "Serialize, Deserialize, Debug")
    ///
    /// # Returns
    ///
    /// The modified `Options` instance with the new derive attribute
    ///
    /// # Examples
    ///
    /// ```
    /// use xml_schema_generator::Options;
    ///
    /// let options = Options::quick_xml_de()
    ///     .derive("Serialize, Deserialize, Debug, Clone");
    ///
    /// assert_eq!("Serialize, Deserialize, Debug, Clone", options.derive);
    /// ```
    pub fn derive(mut self, derive: &str) -> Self {
        self.derive = derive.to_string();
        self
    }
}
