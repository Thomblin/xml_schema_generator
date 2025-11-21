//! Element represents the structure and characteristics of an XML element
//! it provides enough functionality to allow a simple parsing of an XML document into a tree of Elements

use std::collections::{HashMap, HashSet, VecDeque};

use crate::{
    necessity::{merge_necessity, Necessity},
    options::SortBy,
    Options,
};

use convert_string::ConvertString;
use identifier::{Map, Type};

#[cfg(test)]
pub mod macro_rule;

mod identifier;

/// Represents an XML element with its structure and characteristics
/// 
/// This struct captures all information about an XML element including its name,
/// text content, attributes, child elements, and whether it appears multiple times
/// within its parent element.
#[derive(Clone, Debug)]
pub struct Element<T> {
    pub name: T,
    pub text: Option<T>,
    standalone: bool,
    count: u32,
    attributes: Vec<Necessity<T>>,
    children: Vec<Necessity<Element<T>>>,
    position: Option<usize>,
}

impl<T: std::cmp::PartialEq + std::fmt::Display + std::fmt::Debug> Element<T> {
    /// Creates a new XML element with the given name and attributes
    /// 
    /// # Arguments
    /// 
    /// * `name` - The element's tag name
    /// * `attributes` - Vector of attribute names (all marked as mandatory initially)
    pub fn new(name: T, attributes: Vec<T>) -> Element<T> {
        Element {
            name,
            text: None,
            count: 1,
            standalone: true,
            attributes: attributes
                .into_iter()
                .map(|a| Necessity::Mandatory(a))
                .collect::<Vec<Necessity<T>>>(),
            children: Vec::new(),
            position: None,
        }
    }

    /// Formats the element name to be a valid Rust struct name in PascalCase
    /// 
    /// Converts the XML element name to PascalCase for use as a Rust struct identifier.
    /// 
    /// # Returns
    /// 
    /// A `String` containing the PascalCase version of the element name
    pub fn formatted_name(&self) -> String {
        format!("{}", self.name).to_pascal_case() // can we do better than this?
    }

    /// Returns whether this element appears at most once within its parent
    /// 
    /// # Returns
    /// 
    /// `true` if the element appears 0 or 1 times, `false` if it appears
    /// multiple times (requiring a `Vec` in the generated struct).
    pub fn standalone(&self) -> bool {
        self.standalone
    }

    /// Marks this element as appearing multiple times within its parent
    /// 
    /// This causes the generated struct to use `Vec<T>` instead of just `T`.
    pub fn set_multiple(&mut self) {
        self.standalone = false;
    }

    /// Returns the number of times this element appears in the current parent
    /// 
    /// This counter is used during XML parsing to track element occurrences.
    /// 
    /// # Returns
    /// 
    /// The count of how many times this element has been encountered
    pub fn count(&self) -> u32 {
        self.count
    }

    /// Increments the occurrence counter by one
    /// 
    /// Called during XML parsing when another instance of this element is encountered.
    pub fn increment(&mut self) {
        self.count += 1;
    }

    /// Merges the given attributes with this element's existing attributes
    /// 
    /// Attributes present in both lists remain mandatory; attributes present in only one
    /// list become optional. This is used when combining schemas from multiple XML documents.
    ///
    /// # Arguments
    /// 
    /// * `attributes` - Vector of attributes to merge with existing attributes
    /// 
    /// # Returns
    /// 
    /// The modified `Element` with merged attributes
    /// 
    /// # Example
    /// ```
    /// use xml_schema_generator::Element;
    /// use xml_schema_generator::Necessity;
    ///
    /// let mut root = Element::new("car", vec!["name", "colour"]);
    /// root = root.merge_attr(vec![Necessity::Mandatory("name"), Necessity::Mandatory("type")]);
    /// ```
    pub fn merge_attr(mut self, attributes: Vec<Necessity<T>>) -> Element<T> {
        self.attributes = merge_necessity(self.attributes, attributes);
        self
    }

    /// Adds a child element if one with the same name doesn't already exist
    /// 
    /// Sets the child's position index if not already set. Duplicate children
    /// (by name) are not added.
    /// 
    /// # Arguments
    /// 
    /// * `child` - The child element to add
    pub fn add_unique_child(&mut self, mut child: Element<T>) {
        if child.position.is_none() {
            child.position = Some(self.children.len());
        }
        add_unique(&mut self.children, Necessity::Mandatory(child));
    }

    /// Marks a child element as optional
    /// 
    /// Finds the child with the given name and changes it from `Necessity::Mandatory`
    /// to `Necessity::Optional`.
    /// 
    /// # Arguments
    /// 
    /// * `name` - The name of the child element to mark as optional
    pub fn set_child_optional(&mut self, name: &T) {
        let child = self.remove_child(name);
        if let Some(c) = child {
            add_unique(&mut self.children, Necessity::Optional(c.into_inner_t()));
        }
    }

    /// Finds and returns an immutable reference to a child element by name
    /// 
    /// # Arguments
    /// 
    /// * `name` - The name of the child element to find
    /// 
    /// # Returns
    /// 
    /// `Some(&Necessity<Element<T>>)` if found, `None` otherwise
    pub fn get_child(&self, name: &T) -> Option<&Necessity<Element<T>>> {
        self.children.iter().find(|c| c.inner_t().name == *name)
    }

    /// Finds and returns a mutable reference to a child element by name
    /// 
    /// # Arguments
    /// 
    /// * `name` - The name of the child element to find
    /// 
    /// # Returns
    /// 
    /// `Some(&mut Necessity<Element<T>>)` if found, `None` otherwise
    pub fn get_child_mut(&mut self, name: &T) -> Option<&mut Necessity<Element<T>>> {
        self.children.iter_mut().find(|c| c.inner_t().name == *name)
    }

    /// Finds, removes, and returns a child element by name
    /// 
    /// # Arguments
    /// 
    /// * `name` - The name of the child element to remove
    /// 
    /// # Returns
    /// 
    /// `Some(Necessity<Element<T>>)` containing the removed child if found, `None` otherwise
    pub fn remove_child(&mut self, name: &T) -> Option<Necessity<Element<T>>> {
        match self.children.iter().position(|c| c.inner_t().name == *name) {
            Some(index) => Some(self.children.remove(index)),
            None => None,
        }
    }

    /// Returns an immutable reference to all child elements
    /// 
    /// # Returns
    /// 
    /// A reference to the vector containing all child elements
    pub fn children(&self) -> &Vec<Necessity<Element<T>>> {
        &self.children
    }

    /// Returns whether this element contains only text content
    /// 
    /// # Returns
    /// 
    /// `true` if the element has text but no attributes or child elements.
    /// Such elements can be represented as `String` instead of custom structs.
    fn contains_only_text(&self) -> bool {
        self.text.is_some() && self.attributes.is_empty() && self.children.is_empty()
    }

    /// Computes how many parent names to include for unique struct naming
    /// 
    /// # Returns
    /// 
    /// A `HashMap` mapping struct names to the number of parent levels needed for uniqueness.
    /// For example, if "Charge" appears under different parents, it returns 2 to generate
    /// names like "LocationCharge" and "YdTaxCharge".
    ///
    /// returns HasMap:
    ///
    /// Car: 1 -> name does not need to change
    /// Locations: 1
    /// Location: 1
    /// Charge: 2 -> YdTaxCharge and LocationCharge
    /// YdTax: 1
    fn compute_name_hints(&self) -> HashMap<String, usize> {
        // collect the backtrace (path in tree) for each element of the tree

        // tmp store for backtrace inside fill_names()
        let mut trace: VecDeque<String> = VecDeque::new();
        // stores list of all struct name combinations in buckets with same name
        let mut names: HashMap<String, Vec<VecDeque<String>>> = HashMap::new();

        // add all element names and the list of it's parents to the names HashMap
        fn fill_names<T>(
            element: &Element<T>,
            trace: &mut VecDeque<String>,
            names: &mut HashMap<String, Vec<VecDeque<String>>>,
        ) where
            T: PartialEq,
            T: std::fmt::Display,
            T: std::fmt::Debug,
        {
            let name = element.formatted_name();

            trace.push_front(name.clone());

            match names.get_mut(&name) {
                Some(n) => {
                    n.push(trace.clone());
                }
                None => {
                    names.insert(name, vec![trace.clone()]);
                }
            }

            for child in element.children.iter() {
                fill_names(child.inner_t(), trace, names);
            }
            trace.pop_front();
        }
        fill_names(self, &mut trace, &mut names);

        // for each name, figure out how long the backtrace needs to be considered to receive unique names for each element
        fn minimal_different_lengths(vecs: &[VecDeque<String>]) -> usize {
            let mut buffer: Vec<String> = Vec::new();

            for _ in 0..vecs.len() {
                buffer.push(String::new());
            }

            for i in 0..vecs.iter().map(|v| v.len()).min().unwrap_or(0) {
                for (j, b) in buffer.iter_mut().enumerate().take(vecs.len()) {
                    if let Some(v) = vecs.get(j) {
                        if let Some(item) = v.get(i) {
                            b.push_str(item);
                        } else {
                            error!("vec[{j}][{i}] does not exist");
                        }
                    } else {
                        error!("vec[{j}] does not exist");
                    }
                }

                // if all Strings in buffer are different return current length
                if buffer.iter().collect::<HashSet<_>>().len() == vecs.len() {
                    return i + 1;
                }
            }

            // return length of longest VecDeque
            vecs.iter().map(|v| v.len()).max().unwrap_or(0)
        }

        let mut trace_length: HashMap<String, usize> = HashMap::new();

        for (name, traces) in names.iter() {
            if traces.len() == 1 {
                trace_length.insert(name.clone(), 1);
            } else {
                trace_length.insert(name.clone(), minimal_different_lengths(traces));
            }
        }

        trace_length
    }

    /// Expands the element's name by prepending parent names for uniqueness
    /// 
    /// Uses the trace length computed by `compute_name_hints` to determine how many
    /// parent names to prepend, generating names like "LocationCharge" or "YdTaxCharge".
    /// 
    /// # Arguments
    /// 
    /// * `trace` - Slice of parent element names in order from root to this element
    /// * `trace_length` - Map of element names to required trace depth for uniqueness
    /// 
    /// # Returns
    /// 
    /// A unique struct name formed by concatenating the appropriate parent names
    fn expand_name(&self, trace: &[String], trace_length: &HashMap<String, usize>) -> String {
        let mut name = String::new();

        if let Some(n) = trace_length.get(&self.formatted_name()) {
            let start = trace.len().saturating_sub(*n);
            name = trace[start..].join("")
        }
        name
    }
}

impl<T: std::cmp::PartialEq + std::fmt::Display + std::fmt::Debug + std::clone::Clone> Element<T> {
    /// Generates Rust struct definitions from this element and all its children
    /// 
    /// # Arguments
    /// 
    /// * `options` - Configuration for the target parser and code generation
    /// 
    /// # Returns
    /// 
    /// A `String` containing Rust code with serde-annotated structs that can
    /// be used to deserialize or serialize XML documents matching this schema
    pub fn to_serde_struct(&self, options: &Options) -> String {
        let trace_length = self.compute_name_hints();
        let mut trace = Vec::new();
        self.inner_to_serde_struct(options, &mut trace, &trace_length)
    }

    /// Internal recursive implementation of struct generation
    /// 
    /// Generates Rust struct code for this element and recursively processes all children.
    /// Maintains a trace of parent names for unique struct naming.
    /// 
    /// # Arguments
    /// 
    /// * `options` - Configuration for the target parser and code generation
    /// * `trace` - Mutable vector tracking the path from root to current element
    /// * `trace_length` - Map of element names to required trace depth for uniqueness
    /// 
    /// # Returns
    /// 
    /// A `String` containing the Rust struct definition for this element and all descendants
    fn inner_to_serde_struct(
        &self,
        options: &Options,
        trace: &mut Vec<String>,
        trace_length: &HashMap<String, usize>,
    ) -> String {
        let mut serde_struct = String::new();
        let mut serde_child_struct = String::new();

        trace.push(self.formatted_name());

        if !options.derive.is_empty() {
            serde_struct.push_str(&format!("#[derive({})]\n", options.derive));
        }

        serde_struct.push_str(&format!(
            "pub struct {} {{\n",
            self.expand_name(trace, trace_length)
        ));

        let mut used_attr_names = vec![];

        let mut attributes = self.attributes.clone();
        if let SortBy::XmlName = options.sort {
            attributes.sort_unstable_by_key(|a| a.inner_t().to_string());
        }

        let name_map = Map::new(self);

        for attr in attributes {
            let attr_real_name = attr.inner_t().to_string();
            let attr_name = match name_map.get_name(&attr_real_name, Type::Attribute) {
                Some(attr_name) => attr_name.clone(),
                None => attr_real_name.clone(),
            };

            let attr_local_name = match starts_with_xmlns(&attr_real_name) {
                true => attr_real_name,
                false => attr_real_name.remove_namespace(),
            };
            let serde_name = format!("{}{}", options.attribute_prefix, &attr_local_name);

            if attr_name != serde_name {
                serde_struct.push_str(&format!("    #[serde(rename = \"{serde_name}\")]\n"));
            }

            used_attr_names.push(attr_name.clone());

            match attr {
                Necessity::Mandatory(_) => {
                    serde_struct.push_str(&format!("    pub {}: {},\n", attr_name, "String"));
                }
                Necessity::Optional(_) => {
                    serde_struct
                        .push_str(&format!("    pub {}: Option<{}>,\n", attr_name, "String"));
                }
            }
        }

        if self.text.is_some() {
            serde_struct.push_str(&format!(
                "    #[serde(rename = \"{}\")]\n",
                options.text_identifier
            ));
            serde_struct.push_str(&format!(
                "    pub {}: {},\n",
                match name_map.get_name("text", Type::TextContent) {
                    Some(name) => name,
                    None => "text",
                },
                "Option<String>"
            ));
        }

        let mut children = self.children.clone();
        match options.sort {
            SortBy::XmlName => children.sort_unstable_by_key(|c| c.inner_t().name.to_string()),
            SortBy::Unsorted => children.sort_unstable_by_key(|c| c.inner_t().position),
        }

        for child in children.iter() {
            let child_real_name = child.inner_t().name.to_string();
            let child_plain_name = child_real_name.remove_namespace();
            let child_name = match name_map.get_name(&child_real_name, Type::ChildElement) {
                Some(child_name) => child_name.clone(),
                None => child_real_name.clone(),
            };

            if child_name != child_plain_name {
                // TODO can we do better and work properly with namespaces?
                serde_struct.push_str(&format!(
                    "    #[serde(rename = \"{}\")]\n",
                    &child_plain_name
                ));
            }

            let text_only_element = child.inner_t().contains_only_text();

            if !text_only_element {
                trace.push(child.inner_t().formatted_name());
            }

            if child.inner_t().standalone() {
                match child {
                    Necessity::Mandatory(c) => {
                        serde_struct.push_str(&format!(
                            "    pub {}: {},\n",
                            &child_name,
                            if text_only_element {
                                "String".to_string()
                            } else {
                                c.expand_name(trace, trace_length)
                            }
                        ));
                    }
                    Necessity::Optional(c) => {
                        serde_struct.push_str(&format!(
                            "    pub {}: Option<{}>,\n",
                            &child_name,
                            if text_only_element {
                                "String".to_string()
                            } else {
                                c.expand_name(trace, trace_length)
                            }
                        ));
                    }
                }
            } else {
                match child {
                    Necessity::Optional(_) => {
                        serde_struct.push_str(&format!(
                            "    pub {}: Option<Vec<{}>>,\n",
                            &child_name,
                            if text_only_element {
                                "String".to_string()
                            } else {
                                child.inner_t().expand_name(trace, trace_length)
                            }
                        ));
                    }
                    Necessity::Mandatory(_) => {
                        serde_struct.push_str(&format!(
                            "    pub {}: Vec<{}>,\n",
                            &child_name,
                            if text_only_element {
                                "String".to_string()
                            } else {
                                child.inner_t().expand_name(trace, trace_length)
                            }
                        ));
                    }
                }
            }

            if !text_only_element {
                trace.pop();

                serde_child_struct.push_str(&child.inner_t().inner_to_serde_struct(
                    options,
                    trace,
                    trace_length,
                ));
            }
        }

        serde_struct.push_str("}\n\n");

        serde_struct.push_str(&serde_child_struct);

        trace.pop();

        serde_struct
    }
}

/// Checks if the text represents an XML namespace attribute
/// 
/// # Arguments
/// 
/// * `text` - The attribute name to check
/// 
/// # Returns
/// 
/// `true` if the text starts with "xmlns:" indicating it's a namespace declaration, `false` otherwise
fn starts_with_xmlns(text: &str) -> bool {
    match text.find(':') {
        Some(index) => "xmlns:".eq(&text[..(index + 1)]),
        None => false,
    }
}

impl<T: std::cmp::PartialEq> PartialEq for Element<T> {
    /// Compares elements by name only
    /// 
    /// Two elements are considered equal if they have the same name.
    /// This should only be used to compare sibling elements within the same parent.
    /// 
    /// # Arguments
    /// 
    /// * `other` - The other element to compare with
    /// 
    /// # Returns
    /// 
    /// `true` if both elements have the same name, `false` otherwise
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

/// Adds an element to a vector only if it's not already present
/// 
/// Uses `PartialEq` to check for duplicates before adding.
/// 
/// # Arguments
/// 
/// * `vec` - Mutable reference to the vector to add to
/// * `data` - The element to add if not already present
fn add_unique<T: std::cmp::PartialEq>(vec: &mut Vec<T>, data: T) {
    if vec.contains(&data) {
        return;
    }

    vec.push(data);
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use quick_xml::name::QName;

    use super::{add_unique, starts_with_xmlns, Element};
    use crate::{necessity::Necessity, options::SortBy, Options};

    use super::macro_rule::element;

    impl<T: std::cmp::PartialEq + std::fmt::Display + std::fmt::Debug> Element<T> {
        pub fn add_unique_attr(&mut self, attribute: T) {
            add_unique(&mut self.attributes, Necessity::Mandatory(attribute));
        }

        pub fn has_attr(&self, name: &T) -> bool {
            self.attributes.iter().any(|c| c.inner_t() == name)
        }

        pub fn has_child(&self, name: &T) -> bool {
            self.children.iter().any(|c| c.inner_t().name == *name)
        }

        pub fn attributes(&self) -> &Vec<Necessity<T>> {
            &self.attributes
        }
    }

    #[test]
    fn starts_with_xmlns_returns_bool() {
        assert_eq!(true, starts_with_xmlns("xmlns:abc"));
        assert_eq!(false, starts_with_xmlns("abc"));
        assert_eq!(false, starts_with_xmlns("f:abc"));
    }

    #[test]
    fn add_unique_adds_new_value_to_vec() {
        let mut v = vec![1, 2];

        add_unique(&mut v, 4);

        let expected = vec![1, 2, 4];
        assert_eq!(expected, v);
    }

    #[test]
    fn add_unique_does_not_add_value_to_vec_if_it_already_exists() {
        let mut v = vec![1, 2, 3];

        add_unique(&mut v, 2);

        let expected = vec![1, 2, 3];
        assert_eq!(expected, v);
    }

    #[test]
    fn add_unique_should_work_with_qname() {
        let mut v = vec![QName(b"apple")];

        add_unique(&mut v, QName(b"potato"));

        let expected = vec![QName(b"apple"), QName(b"potato")];
        assert_eq!(expected, v);
    }

    #[test]
    fn create_new_tag_struct() {
        let root = element!("root");

        assert_eq!("root", root.name);
        assert_eq!(None, root.text);
        assert_eq!(0, root.attributes.len());
        assert_eq!(0, root.children.len());
    }

    #[test]
    fn adds_attribute_only_once() {
        let mut root = element!("root");

        root.add_unique_attr("alpha");
        root.add_unique_attr("beta");
        root.add_unique_attr("alpha");

        assert_eq!(
            2,
            root.attributes.len(),
            "expected attributes to contain 2 elements"
        );
        assert_eq!(
            true,
            root.attributes.contains(&Necessity::Mandatory("alpha")),
            "expected attributes to contain alpha"
        );
        assert_eq!(
            true,
            root.attributes.contains(&Necessity::Mandatory("beta")),
            "expected attributes to contain beta"
        );
    }

    #[test]
    fn adds_children_only_once() {
        let root = element!(
            "root",
            None,
            vec![],
            vec![element!("red"), element!("green"), element!("red")]
        );

        assert_eq!(
            2,
            root.children.len(),
            "expected attributes to contain 2 elements"
        );
        assert_eq!(
            true,
            root.children
                .contains(&Necessity::Mandatory(Element::new("green", Vec::new()))),
            "expected attributes to contain red child"
        );
        assert_eq!(
            true,
            root.children
                .contains(&Necessity::Mandatory(Element::new("red", Vec::new()))),
            "expected attributes to contain green child"
        );
    }

    #[test]
    fn has_children_returns_bool() {
        let root = element!("root", None, vec![], vec![element!("red")]);

        assert_eq!(true, root.has_child(&"red"));
        assert_eq!(false, root.has_child(&"green"));
    }

    #[test]
    fn get_children_returns_child() {
        let mut root = element!("root", None, vec![], vec![element!("red")]);

        match root.get_child_mut(&"red") {
            Some(a) => assert_eq!("red", a.inner_t().name),
            None => panic!("expected to find red element"),
        }

        if root.get_child_mut(&"green").is_some() {
            panic!("did not expect to find green element")
        }
    }

    #[test]
    fn set_multiple_changes_standalone_to_false() {
        let mut root = element!("root");

        assert_eq!(true, root.standalone());

        root.set_multiple();
        assert_eq!(false, root.standalone());
    }

    #[test]
    fn to_serde_struct_creates_minimal_output() {
        let a = element!("a");

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_adjusts_derive_attribute() {
        let a = element!("a");

        let expected = concat!(
            "#[derive(Serialize, Deserialize, Debug)]\n",
            "pub struct A {\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de().derive("Serialize, Deserialize, Debug"))
        );
    }

    #[test]
    fn to_serde_struct_removes_derive_attribute_if_empty() {
        let a = element!("a");

        let expected = concat!("pub struct A {\n", "}\n", "\n",);

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de().derive(""))
        );
    }

    #[test]
    fn to_serde_struct_removes_namespace() {
        let a = element!("ns:a");

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct NsA {\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_allows_protected_names() {
        let a = element!("type");

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct Type {\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_formats_name_to_pascal_case() {
        let a = element!("hello_world");

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct HelloWorld {\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_avoids_name_duplicates() {
        let b = element!("b", None, vec![], vec![element!("d")]);
        let c = element!("c", None, vec![], vec![element!("d")]);
        let a = element!("a", None, vec![], vec![b, c]);

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    pub b: B,\n",
            "    pub c: C,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct B {\n",
            "    pub d: BD,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct BD {\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct C {\n",
            "    pub d: CD,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct CD {\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_with_mandatory_text_only_child() {
        let b = element!("b", Some("asd"), vec![], vec![]);
        let a = element!("a", None, vec![], vec![b]);

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    pub b: String,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_with_optional_text_only_child() {
        let b = element!("b", Some("asd"), vec![], vec![]);
        let mut a = element!("a", None, vec![], vec![b]);
        a.set_child_optional(&"b");

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    pub b: Option<String>,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_multiple_text_only_children() {
        let b = element!("b", Some("asd"), vec![], vec![], multiple);
        let a = element!("a", None, vec![], vec![b]);

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    pub b: Vec<String>,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_with_text_only_child_removes_namespace() {
        let b = element!("ns:b", Some("asd"), vec![], vec![]);
        let a = element!("a", None, vec![], vec![b]);

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    #[serde(rename = \"b\")]\n",
            "    pub ns_b: String,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_with_text_only_child_renames_protected_names() {
        let b = element!("type", Some("asd"), vec![], vec![]);
        let a = element!("a", None, vec![], vec![b]);

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    #[serde(rename = \"type\")]\n",
            "    pub a_type: String,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_with_attributes() {
        let a = element!("a", None, vec!["b", "c"]);

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    #[serde(rename = \"@b\")]\n",
            "    pub b: String,\n",
            "    #[serde(rename = \"@c\")]\n",
            "    pub c: String,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_with_attributes_for_serde_xml_rs() {
        let a = element!("a", None, vec!["b", "c"]);

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    pub b: String,\n",
            "    pub c: String,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::serde_xml_rs())
        );
    }

    #[test]
    fn to_serde_struct_renames_protected_attribute_names_and_adds_class_name() {
        let a = element!("a", None, vec!["type"]);

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    #[serde(rename = \"@type\")]\n",
            "    pub a_type: String,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_renames_protected_attribute_names_and_adds_class_name_for_serde_xml_rs() {
        let a = element!("a", None, vec!["type"]);

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    #[serde(rename = \"type\")]\n",
            "    pub a_type: String,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::serde_xml_rs())
        );
    }

    #[test]
    fn to_serde_struct_avoids_collissions_when_renaming() {
        let a = element!("a", None, vec!["type", "a_type"]);

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    #[serde(rename = \"@type\")]\n",
            "    pub a_type: String,\n",
            "    #[serde(rename = \"@a_type\")]\n",
            "    pub a_type_attr: String,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_avoids_collissions_when_renaming_for_serde_xml_rs() {
        let a = element!("a", None, vec!["type", "a_type"]);

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    #[serde(rename = \"type\")]\n",
            "    pub a_type: String,\n",
            "    #[serde(rename = \"a_type\")]\n",
            "    pub a_type_attr: String,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::serde_xml_rs())
        );
    }

    #[test]
    fn to_serde_struct_avoids_name_collissions() {
        let a = element!("a", None, vec!["type", "a_type", "a_type_attr"]);

        // TODO: find a better strategy to avoid all sorts of possible collissions
        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    #[serde(rename = \"@type\")]\n",
            "    pub a_type: String,\n",
            "    #[serde(rename = \"@a_type\")]\n",
            "    pub a_type_attr: String,\n",
            "    #[serde(rename = \"@a_type_attr\")]\n",
            "    pub a_type_attr_1: String,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_renames_attribute_names_to_snake_case() {
        let a = element!("a", None, vec!["BOLD"]);

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    #[serde(rename = \"@BOLD\")]\n",
            "    pub bold: String,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_with_optional_attribute() {
        let a = element!("a", None, vec!["o"]);
        let a = a.merge_attr(vec![]); // o is now optional, because it is not in the list

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    #[serde(rename = \"@o\")]\n",
            "    pub o: Option<String>,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_with_text() {
        let a = element!("a", Some("text"));

        // TODO: should text always be optional? Maybe it is good enough if we ignore white spaces
        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    #[serde(rename = \"$text\")]\n",
            "    pub text: Option<String>,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_with_text_for_serde_xml_rs() {
        let a = element!("a", Some("text"));

        // TODO: should text always be optional? Maybe it is good enough if we ignore white spaces
        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    #[serde(rename = \"$text\")]\n",
            "    pub text: Option<String>,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::serde_xml_rs())
        );
    }

    #[test]
    fn to_serde_struct_with_empty_text() {
        let a = element!("a", Some(" "));

        // TODO: should we ignore white spaces and render no text attribute here?
        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    #[serde(rename = \"$text\")]\n",
            "    pub text: Option<String>,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_avoids_collission_with_text_and_text_attribute() {
        let a = element!("a", Some("text"), vec!["text", "a_text"]);

        // TODO: fix this
        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    #[serde(rename = \"@text\")]\n",
            "    pub text: String,\n",
            "    #[serde(rename = \"@a_text\")]\n",
            "    pub a_text: String,\n",
            "    #[serde(rename = \"$text\")]\n",
            "    pub text_content: Option<String>,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    // https://github.com/Thomblin/xml_schema_generator/issues/37
    #[test]
    fn to_serde_struct_avoids_collission_with_child_and_attribute() {
        let a = element!("example", None, vec!["foo"], vec![element!("foo")]);

        // TODO: fix this
        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct Example {\n",
            "    #[serde(rename = \"@foo\")]\n",
            "    pub foo_attr: String,\n",
            "    pub foo: Foo,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct Foo {\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    // https://github.com/Thomblin/xml_schema_generator/issues/37
    #[test]
    fn to_serde_struct_avoids_collission_with_child_and_text() {
        let a = element!("example", Some("hello"), vec![], vec![element!("text")]);

        // TODO: fix this
        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct Example {\n",
            "    #[serde(rename = \"$text\")]\n",
            "    pub text_content: Option<String>,\n",
            "    pub text: Text,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct Text {\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_with_mandatory_child() {
        let a = element!("a", None, vec![], vec![element!("b")]);

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    pub b: B,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct B {\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_with_optional_child() {
        let mut a = element!("a", None, vec![], vec![element!("b")]);
        a.set_child_optional(&"b");

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    pub b: Option<B>,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct B {\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_with_multiple_identical_children() {
        let a = element!(
            "a",
            None,
            vec![],
            vec![element!("b", None, vec![], vec![], multiple)]
        );

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    pub b: Vec<B>,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct B {\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_with_multiple_optional_children() {
        let mut a = element!(
            "a",
            None,
            vec![],
            vec![element!("b", None, vec![], vec![], multiple)]
        );

        a.set_child_optional(&"b");

        println!("{a:?}");

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "    pub b: Option<Vec<B>>,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct B {\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_transform_struct_name_to_pascal_case() {
        let a = element!(
            "MVCI_MODULE_DESCRIPTION",
            None,
            vec![],
            vec![element!("PINTYPE", None, vec![], vec![], multiple)]
        );

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct MvciModuleDescription {\n",
            "    #[serde(rename = \"PINTYPE\")]\n",
            "    pub pintype: Vec<Pintype>,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct Pintype {\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_struct_transforms_namespaces() {
        let a = element!(
            "root",
            None,
            vec!["xmlns:h", "h:c"],
            vec![element!("h:b", Some("y"))]
        );

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct Root {\n",
            "    #[serde(rename = \"@xmlns:h\")]\n",
            "    pub xmlns_h: String,\n",
            "    #[serde(rename = \"@c\")]\n",
            "    pub h_c: String,\n",
            "    #[serde(rename = \"b\")]\n",
            "    pub h_b: String,\n",
            "}\n\n",
        );

        assert_eq!(
            String::from(expected),
            a.to_serde_struct(&Options::quick_xml_de())
        );
    }

    #[test]
    fn to_serde_sorts_attributes_by_name() {
        let a = element!("root", None, vec!["b", "c", "a"]);

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct Root {\n",
            "    #[serde(rename = \"@a\")]\n",
            "    pub a: String,\n",
            "    #[serde(rename = \"@b\")]\n",
            "    pub b: String,\n",
            "    #[serde(rename = \"@c\")]\n",
            "    pub c: String,\n",
            "}\n\n",
        );

        let options = Options {
            sort: SortBy::XmlName,
            ..Options::quick_xml_de()
        };

        assert_eq!(String::from(expected), a.to_serde_struct(&options));
    }

    #[test]
    fn to_serde_sorts_children_by_name() {
        let a = element!(
            "root",
            None,
            vec![],
            vec![element!("b"), element!("c"), element!("a")]
        );

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct Root {\n",
            "    pub a: A,\n",
            "    pub b: B,\n",
            "    pub c: C,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct A {\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct B {\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct C {\n",
            "}\n",
            "\n",
        );

        let options = Options {
            sort: SortBy::XmlName,
            ..Options::quick_xml_de()
        };

        assert_eq!(String::from(expected), a.to_serde_struct(&options));
    }

    #[test]
    fn to_serde_struct_all_combined() {
        let mut root = Element::new("car", vec!["name", "colour", "xmlns:soap", "type"]);
        root = root.merge_attr(vec![
            Necessity::Mandatory("name"),
            Necessity::Mandatory("xmlns:soap"),
        ]);

        let mut locations = Element::new("Locations", vec![]);

        let mut location = Element::new("Location", vec!["id_rental"]);
        location.set_multiple();

        let mut address = Element::new("ns:address", vec![]); // TODO handle namespaces properly
        address.text = Some("whatever");

        let mut number = Element::new("ns:number", vec![]); // TODO handle namespaces properly
        number.text = Some("whatever");

        let mut yd_tax = Element::new("ns:yd_tax", vec!["age"]);
        yd_tax.text = Some("50");

        let charge = Element::new("charge", vec!["amount"]);
        let charge2 = Element::new("charge", vec!["amount", "fee"]);

        location.add_unique_child(address);
        location.add_unique_child(number);
        location.set_child_optional(&"ns:number");
        location.add_unique_child(charge);

        locations.add_unique_child(location);
        root.add_unique_child(locations);

        yd_tax.add_unique_child(charge2);
        root.add_unique_child(yd_tax);
        root.set_child_optional(&"ns:yd_tax");

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct Car {\n",
            "    #[serde(rename = \"@name\")]\n",
            "    pub name: String,\n",
            "    #[serde(rename = \"@colour\")]\n",
            "    pub colour: Option<String>,\n",
            "    #[serde(rename = \"@xmlns:soap\")]\n",
            "    pub xmlns_soap: String,\n",
            "    #[serde(rename = \"@type\")]\n",
            "    pub car_type: Option<String>,\n",
            "    #[serde(rename = \"Locations\")]\n",
            "    pub locations: Locations,\n",
            "    #[serde(rename = \"yd_tax\")]\n",
            "    pub ns_yd_tax: Option<NsYdTax>,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct Locations {\n",
            "    #[serde(rename = \"Location\")]\n",
            "    pub location: Vec<Location>,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct Location {\n",
            "    #[serde(rename = \"@id_rental\")]\n",
            "    pub id_rental: String,\n",
            "    #[serde(rename = \"address\")]\n",
            "    pub ns_address: String,\n",
            "    #[serde(rename = \"number\")]\n",
            "    pub ns_number: Option<String>,\n",
            "    pub charge: LocationCharge,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct LocationCharge {\n",
            "    #[serde(rename = \"@amount\")]\n",
            "    pub amount: String,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct NsYdTax {\n",
            "    #[serde(rename = \"@age\")]\n",
            "    pub age: String,\n",
            "    #[serde(rename = \"$text\")]\n", // TODO when do we need to add $text? should we just ignore whitespaces when creating this attribute?
            "    pub text: Option<String>,\n",
            "    pub charge: NsYdTaxCharge,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct NsYdTaxCharge {\n",
            "    #[serde(rename = \"@amount\")]\n",
            "    pub amount: String,\n",
            "    #[serde(rename = \"@fee\")]\n",
            "    pub fee: String,\n",
            "}\n",
            "\n",
        );

        assert_eq!(
            String::from(expected),
            root.to_serde_struct(&Options::quick_xml_de())
        );
    }

    // https://github.com/Thomblin/xml_schema_generator/issues/40
    #[test]
    fn to_serde_struct_with_cyrillic_names() {
        let classifier = element!("Классификатор", None, vec!["Ид"]);
        let commercial_information = element!(
            "КоммерческаяИнформация",
            None,
            vec!["Наименование"],
            vec![classifier]
        );

        let expected = concat!(
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct КоммерческаяИнформация {\n",
            "    #[serde(rename = \"Наименование\")]\n",
            "    pub наименование: String,\n",
            "    #[serde(rename = \"Классификатор\")]\n",
            "    pub классификатор: Классификатор,\n",
            "}\n",
            "\n",
            "#[derive(Serialize, Deserialize)]\n",
            "pub struct Классификатор {\n",
            "    #[serde(rename = \"Ид\")]\n",
            "    pub ид: String,\n",
            "}\n",
            "\n"
        );

        assert_eq!(
            String::from(expected),
            commercial_information.to_serde_struct(&Options::serde_xml_rs())
        );
    }
}
