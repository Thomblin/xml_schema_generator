//! Element represents the structure and characteristics of an XML element
//! it provides enough functionality to allow a simple parsing of an XML document into a tree of Elements

use std::collections::{HashMap, HashSet, VecDeque};

use crate::necessity::{merge_necessity, Necessity};

use convert_string::ConvertString;

/// represents the structure and characteristics of an XML element
#[derive(Debug)]
pub struct Element<T> {
    pub name: T,
    pub text: Option<T>,
    standalone: bool,
    count: u32,
    attributes: Vec<Necessity<T>>,
    children: Vec<Necessity<Element<T>>>,
}

impl<T: std::cmp::PartialEq + std::fmt::Display + std::fmt::Debug> Element<T> {
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
        }
    }

    /// format the name of the element to be properly represented as a Rust struct
    pub fn formatted_name(&self) -> String {
        format!("{}", self.name).to_pascal_case() // can we do better than this?
    }

    /// return true if this tag always appears only 0 or 1 times within it's parent element
    /// return false if this tag appears at least once more than 1 times within it's parent element
    pub fn standalone(&self) -> bool {
        self.standalone
    }

    /// call this function if this element appears more than once inside a parent element
    pub fn set_multiple(&mut self) {
        self.standalone = false;
    }

    /// return how often the element is present inside the current parent element, only used during parsing an XML document
    pub fn count(&self) -> u32 {
        self.count
    }

    /// increase the counter by one, only used during parsing an XML document
    pub fn increment(&mut self) {
        self.count += 1;
    }

    /// merge the given list of attributes into the list if the element's attributes
    /// attributes that not appear in both lists are marked as optional
    ///
    /// Example
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

    /// add a new child element to this element, if it does not exist yet
    pub fn add_unique_child(&mut self, child: Element<T>) {
        add_unique(&mut self.children, Necessity::Mandatory(child));
    }

    /// find a child element with the given name, mark it as optional if found
    pub fn set_child_optional(&mut self, name: &T) {
        let child = self.remove_child(name);
        if let Some(c) = child {
            add_unique(&mut self.children, Necessity::Optional(c.into_inner_t()));
        }
    }

    /// find a child element by name and return a reference to it if found
    pub fn get_child(&self, name: &T) -> Option<&Necessity<Element<T>>> {
        self.children.iter().find(|c| c.inner_t().name == *name)
    }

    /// find a child element by name and return it as mutable reference if found
    pub fn get_child_mut(&mut self, name: &T) -> Option<&mut Necessity<Element<T>>> {
        self.children.iter_mut().find(|c| c.inner_t().name == *name)
    }

    /// find a child element by name, remove it from this element and return it
    pub fn remove_child(&mut self, name: &T) -> Option<Necessity<Element<T>>> {
        match self.children.iter().position(|c| c.inner_t().name == *name) {
            Some(index) => Some(self.children.remove(index)),
            None => None,
        }
    }

    /// return a reference to the list of all child elements
    pub fn children(&self) -> &Vec<Necessity<Element<T>>> {
        &self.children
    }

    /// returns true if this element contains only text, but no attributes nor children
    /// this is used to create Rust structs more efficiently
    fn contains_only_text(&self) -> bool {
        self.text.is_some() && self.attributes.is_empty() && self.children.is_empty()
    }

    /// generate a String representing this element and all children elements recursivly as series of Rust structs
    /// those struct can be used to (de)serialize an XML document
    pub fn to_serde_struct(&self) -> String {
        let trace_length = self.compute_name_hints();
        let mut trace = Vec::new();
        self.inner_to_serde_struct(&mut trace, &trace_length)
    }

    /// returns for each tag name, how many parts of the tree need to be used to create a unique name that is readable and as short as possible
    ///
    /// Car -> Car
    ///     Locations -> Locations,Car
    ///         Location -> Location,Locations,Car
    ///             Charge -> Charge,Location,Locations,Car
    ///     YdTax -> Ydtax,Car
    ///         Charge -> Charge,YdTax,Car
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
        fn minimal_different_lengths(vecs: &Vec<VecDeque<String>>) -> usize {
            let mut buffer: Vec<String> = Vec::new();

            for _ in 0..vecs.len() {
                buffer.push(String::new());
            }

            for i in 0..vecs.iter().map(|v| v.len()).min().unwrap_or(0) {
                for j in 0..vecs.len() {
                    if let Some(v) = vecs.get(j) {
                        if let Some(item) = v.get(i) {
                            buffer[j].push_str(item);
                        } else {
                            error!("vec[{}][{}] does not exist", j, i);
                        }
                    } else {
                        error!("vec[{}] does not exist", j);
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

    /// expand the elemnt's name with X parent element names (as computed in compute_name_hints) to generate a unique name
    fn expand_name(&self, trace: &Vec<String>, trace_length: &HashMap<String, usize>) -> String {
        let mut name = String::new();

        if let Some(n) = trace_length.get(&self.formatted_name()) {
            if *n <= trace.len() {
                let start = trace.len() - *n;
                name = trace[start..].join("")
            } else {
                error!("invalid trace for tag {}", &self.name);
            }
        }
        name
    }

    /// generate a String representing this element and all children elements recursivly as series of Rust structs
    /// those struct can be used to (de)serialize an XML document
    fn inner_to_serde_struct(
        &self,
        trace: &mut Vec<String>,
        trace_length: &HashMap<String, usize>,
    ) -> String {
        let mut serde_struct = String::new();
        let mut serde_child_struct = String::new();

        // TODO there might be a better way to convert type T to &str
        let name = format!("{}", &self.name);

        trace.push(self.formatted_name());

        serde_struct.push_str("#[derive(Serialize, Deserialize)]\n");
        serde_struct.push_str(&format!(
            "pub struct {} {{\n",
            self.expand_name(trace, trace_length)
        ));

        for child in self.children.iter() {
            if child.inner_t().contains_only_text() {
                let child_name = format!("{}", &child.inner_t().name);

                // TODO can we do better and work properly with namespaces?
                serde_struct.push_str(&format!(
                    "    #[serde(rename = \"{}\")]\n",
                    &child_name.remove_namespace()
                ));

                match child {
                    Necessity::Mandatory(_) => {
                        serde_struct.push_str(&format!(
                            "    pub {}: {},\n",
                            child_name.to_valid_key(&name),
                            "String"
                        ));
                    }
                    Necessity::Optional(_) => {
                        serde_struct.push_str(&format!(
                            "    pub {}: Option<{}>,\n",
                            child_name.to_valid_key(&name),
                            "String"
                        ));
                    }
                }
            }
        }

        for attr in self.attributes.iter() {
            // TODO there might be a better way to convert type T to &str
            let attr_name = format!("{}", &attr.inner_t());

            serde_struct.push_str(&format!(
                "    #[serde(rename = \"@{}\")]\n",
                &attr.inner_t()
            ));

            match attr {
                Necessity::Mandatory(_) => {
                    serde_struct.push_str(&format!(
                        "    pub {}: {},\n",
                        attr_name.to_valid_key(&name),
                        "String"
                    ));
                }
                Necessity::Optional(_) => {
                    serde_struct.push_str(&format!(
                        "    pub {}: Option<{}>,\n",
                        attr_name.to_valid_key(&name),
                        "String"
                    ));
                }
            }
        }

        if self.text.is_some() {
            serde_struct.push_str("    #[serde(rename = \"$text\")]\n");
            serde_struct.push_str(&format!("    pub {}: {},\n", "text", "Option<String>"));
        }

        for child in self.children.iter() {
            if !child.inner_t().contains_only_text() {
                let child_name = child.inner_t().name.to_string();
                let key_name = child_name.to_valid_key(&name);

                // TODO can we do better and work properly with namespaces?
                serde_struct.push_str(&format!(
                    "    #[serde(rename = \"{}\")]\n",
                    &child_name.remove_namespace()
                ));

                trace.push(child.inner_t().formatted_name());

                if child.inner_t().standalone() {
                    match child {
                        Necessity::Mandatory(c) => {
                            serde_struct.push_str(&format!(
                                "    pub {}: {},\n",
                                &key_name,
                                c.expand_name(trace, trace_length)
                            ));
                        }
                        Necessity::Optional(c) => {
                            serde_struct.push_str(&format!(
                                "    pub {}: Option<{}>,\n",
                                &key_name,
                                c.expand_name(trace, trace_length)
                            ));
                        }
                    }
                } else {
                    serde_struct.push_str(&format!(
                        "    pub {}: Vec<{}>,\n",
                        &key_name,
                        child.inner_t().expand_name(trace, trace_length)
                    ));
                }

                trace.pop();

                serde_child_struct
                    .push_str(&child.inner_t().inner_to_serde_struct(trace, trace_length));
            }
        }

        serde_struct.push_str("}\n\n");

        serde_struct.push_str(&serde_child_struct);

        serde_struct
    }
}

impl<T: std::cmp::PartialEq> PartialEq for Element<T> {
    /// return true if two elements share the same name
    /// should only be used to compare two elements that share the same parent element
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

/// helper function to add an element to Vec if it is not contained already
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

    use super::{add_unique, Element};
    use crate::necessity::Necessity;

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
        let root = Element::new("root", Vec::new());

        assert_eq!(root.name, "root");
        assert_eq!(root.text, None);
        assert_eq!(root.attributes.len(), 0);
        assert_eq!(root.children.len(), 0);
    }

    #[test]
    fn adds_attribute_only_once() {
        let mut root = Element::new("root", Vec::new());

        root.add_unique_attr("alpha");
        root.add_unique_attr("beta");
        root.add_unique_attr("alpha");

        assert_eq!(
            root.attributes.len(),
            2,
            "expected attributes to contain 2 elements"
        );
        assert_eq!(
            root.attributes.contains(&Necessity::Mandatory("alpha")),
            true,
            "expected attributes to contain alpha"
        );
        assert_eq!(
            root.attributes.contains(&Necessity::Mandatory("beta")),
            true,
            "expected attributes to contain beta"
        );
    }

    #[test]
    fn adds_children_only_once() {
        let mut root = Element::new("root", Vec::new());

        let child_red = Element::new("red", Vec::new());
        let child_green = Element::new("green", Vec::new());
        let child_red2 = Element::new("red", Vec::new());

        root.add_unique_child(child_red);
        root.add_unique_child(child_green);
        root.add_unique_child(child_red2);

        assert_eq!(
            root.children.len(),
            2,
            "expected attributes to contain 2 elements"
        );
        assert_eq!(
            root.children
                .contains(&Necessity::Mandatory(Element::new("green", Vec::new()))),
            true,
            "expected attributes to contain red child"
        );
        assert_eq!(
            root.children
                .contains(&Necessity::Mandatory(Element::new("red", Vec::new()))),
            true,
            "expected attributes to contain green child"
        );
    }

    #[test]
    fn has_children_returns_bool() {
        let mut root = Element::new(String::from("root"), Vec::new());

        root.add_unique_child(Element::new(String::from("red"), Vec::new()));

        assert_eq!(root.has_child(&String::from("red")), true);
        assert_eq!(root.has_child(&String::from("green")), false);
    }

    #[test]
    fn get_children_returns_child() {
        let mut root = Element::new(String::from("root"), Vec::new());

        root.add_unique_child(Element::new(String::from("red"), Vec::new()));

        match root.get_child_mut(&String::from("red")) {
            Some(a) => assert_eq!(a.inner_t().name, "red"),
            None => panic!("expected to find red element"),
        }

        if root.get_child_mut(&String::from("green")).is_some() {
            panic!("did not expect to find green element")
        }
    }

    #[test]
    fn set_multiple_changes_standalone_to_false() {
        let mut root = Element::new(String::from("root"), Vec::new());

        assert_eq!(root.standalone(), true);

        root.set_multiple();
        assert_eq!(root.standalone(), false);
    }

    #[test]
    fn to_serde_struct_create_basic_structure() {
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
            "    #[serde(rename = \"address\")]\n",
            "    pub ns_address: String,\n",
            "    #[serde(rename = \"number\")]\n",
            "    pub ns_number: Option<String>,\n",
            "    #[serde(rename = \"@id_rental\")]\n",
            "    pub id_rental: String,\n",
            "    #[serde(rename = \"charge\")]\n",
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
            "    #[serde(rename = \"$text\")]\n", // TODO when do we need to addd $text? should we just ignore whitespaces when creating this attribute?
            "    pub text: Option<String>,\n",
            "    #[serde(rename = \"charge\")]\n",
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

        assert_eq!(root.to_serde_struct(), String::from(expected));
    }
}
