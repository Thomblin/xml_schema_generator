//! Defines helper functions to guarantee unqiue and valid Rust identifiers as they cannot always match the XML names

use std::collections::HashMap;

use crate::Element;
use convert_string::ConvertString;

/// different types of identifiers, as we can implement different naming strategies for better readability
#[derive(PartialEq, Eq, Hash)]
pub enum Type {
    TextContent,
    Attribute,
    ChildElement,
}

/// helper struct to parse a crate::Element and generate valid & unique identifiers
pub struct Map {
    map: HashMap<(String, Type), String>,
}

/// cache to store identifiers that are already in use to avoid collissions
struct ReservedNames {
    reserved_names: Vec<String>,
}

impl ReservedNames {
    // generate empty cache
    fn new() -> Self {
        Self {
            reserved_names: vec![],
        }
    }

    /// process name identifier and create a unique version, stores the return value in cache to avoid duplicates
    fn create_unused_name(&mut self, name: &String, r#type: Type) -> String {
        if r#type == Type::TextContent && "text" == name && self.reserved_names.contains(name) {
            return self.create_unused_name(&"text_content".to_string(), r#type);
        }

        if r#type == Type::Attribute
            && self.reserved_names.contains(name)
            && !name.ends_with("_attr")
        {
            return self.create_unused_name(&format!("{}_attr", name), r#type);
        }

        let mut unused_name = name.clone();
        let mut i = 0;

        while self.reserved_names.contains(&unused_name) {
            i += 1;
            unused_name = format!("{}_{}", name, i);
        }

        self.reserved_names.push(unused_name.clone());
        unused_name
    }
}

impl Map {
    /// parse the given element and generate unique, valid names that can be pulled with fn get_name
    pub fn new<T: std::fmt::Display>(element: &Element<T>) -> Self {
        let mut map = HashMap::new();
        let mut reserved_names = ReservedNames::new();

        let name = element.name.to_string();

        for child in element.children.iter() {
            let child_real_name = child.inner_t().name.to_string();
            let child_name = child_real_name.to_valid_key(&name);
            let child_name = reserved_names.create_unused_name(&child_name, Type::ChildElement);

            map.insert((child_real_name, Type::ChildElement), child_name);
        }

        for attr in element.attributes.iter() {
            let attr_real_name = attr.inner_t().to_string();
            let attr_name = attr_real_name.to_valid_key(&name);
            let attr_name = reserved_names.create_unused_name(&attr_name, Type::Attribute);

            map.insert((attr_real_name, Type::Attribute), attr_name);
        }

        let text_name = reserved_names.create_unused_name(&"text".to_string(), Type::TextContent);
        map.insert(
            ("text".to_string(), Type::TextContent),
            text_name.to_string(),
        );

        Map { map }
    }

    /// return a valid and unique identifier for the given name and type
    pub fn get_name(&self, name: &str, r#type: Type) -> Option<&String> {
        self.map.get(&(name.to_string(), r#type))
    }
}

#[cfg(test)]
mod tests {
    use crate::element::{identifier::Type, macro_rule::element};
    use crate::Element;

    use super::Map;

    #[test]
    fn return_all_names_unchanged() {
        let element = element!("foo", Some("hello"), vec!["a"], vec![element!("b")]);
        let map = Map::new(&element);

        assert_eq!("text", map.get_name("text", Type::TextContent).unwrap());
        assert_eq!("a", map.get_name("a", Type::Attribute).unwrap());
        assert_eq!("b", map.get_name("b", Type::ChildElement).unwrap());
    }

    #[test]
    fn convert_reservered_keyword_and_add_class_name() {
        let element = element!("foo", None, vec!["type"]);
        let map = Map::new(&element);

        assert_eq!("foo_type", map.get_name("type", Type::Attribute).unwrap());
    }

    #[test]
    fn convert_reservered_keyword_in_child_and_add_class_name() {
        let element = element!("foo", None, vec![], vec![element!("loop")]);
        let map = Map::new(&element);

        assert_eq!(
            "foo_loop",
            map.get_name("loop", Type::ChildElement).unwrap()
        );
    }

    #[test]
    fn convert_namespace_in_child() {
        let element = element!("foo", None, vec![], vec![element!("ns:loop")]);
        let map = Map::new(&element);

        assert_eq!(
            "ns_loop",
            map.get_name("ns:loop", Type::ChildElement).unwrap()
        );
    }

    #[test]
    fn convert_child_name_to_lowercase() {
        let element = element!("foo", None, vec![], vec![element!("FAR")]);
        let map = Map::new(&element);

        assert_eq!("far", map.get_name("FAR", Type::ChildElement).unwrap());
    }

    #[test]
    fn convert_text_content_if_child_with_name_text_exists() {
        let element = element!("foo", Some("hello"), vec![], vec![element!("text")]);
        let map = Map::new(&element);

        assert_eq!(
            "text_content",
            map.get_name("text", Type::TextContent).unwrap()
        );
        assert_eq!("text", map.get_name("text", Type::ChildElement).unwrap());
    }

    #[test]
    fn convert_text_content_if_child_with_name_text_content_exists() {
        let element = element!(
            "foo",
            Some("hello"),
            vec![],
            vec![element!("text"), element!("text_content")]
        );
        let map = Map::new(&element);

        assert_eq!("text", map.get_name("text", Type::ChildElement).unwrap());
        assert_eq!(
            "text_content",
            map.get_name("text_content", Type::ChildElement).unwrap()
        );
        assert_eq!(
            "text_content_1",
            map.get_name("text", Type::TextContent).unwrap()
        );
    }

    #[test]
    fn avoid_collission_with_text_and_text_attribute() {
        let element = element!("a", Some("text"), vec!["text", "a_text"]);

        let map = Map::new(&element);

        assert_eq!(
            "text_content",
            map.get_name("text", Type::TextContent).unwrap()
        );
        assert_eq!("text", map.get_name("text", Type::Attribute).unwrap());
        assert_eq!("a_text", map.get_name("a_text", Type::Attribute).unwrap());
    }

    #[test]
    fn avoid_collission_with_text_and_text_attribute2() {
        let element = element!(
            "a",
            Some("text"),
            vec!["text", "text_content", "text_content_1"]
        );

        let map = Map::new(&element);

        assert_eq!("text", map.get_name("text", Type::Attribute).unwrap());
        assert_eq!(
            "text_content",
            map.get_name("text_content", Type::Attribute).unwrap()
        );
        assert_eq!(
            "text_content_1",
            map.get_name("text_content_1", Type::Attribute).unwrap()
        );
        assert_eq!(
            "text_content_2",
            map.get_name("text", Type::TextContent).unwrap()
        );
    }

    #[test]
    fn avoid_collission_with_attribute_and_child() {
        let element = element!("a", None, vec!["foo"], vec![element!("foo")]);

        let map = Map::new(&element);

        assert_eq!("foo_attr", map.get_name("foo", Type::Attribute).unwrap());
        assert_eq!("foo", map.get_name("foo", Type::ChildElement).unwrap());
    }

    #[test]
    fn avoid_collission_with_attribute_and_child_attr() {
        let element = element!(
            "a",
            None,
            vec!["foo"],
            vec![element!("foo"), element!("foo_attr")]
        );

        let map = Map::new(&element);

        assert_eq!("foo", map.get_name("foo", Type::ChildElement).unwrap());
        assert_eq!(
            "foo_attr",
            map.get_name("foo_attr", Type::ChildElement).unwrap()
        );
        assert_eq!("foo_attr_1", map.get_name("foo", Type::Attribute).unwrap());
    }

    #[test]
    fn avoid_collission_with_attribute_attr_and_child_attr() {
        let element = element!(
            "a",
            None,
            vec!["foo", "foo_attr"],
            vec![element!("foo"), element!("foo_attr")]
        );

        let map = Map::new(&element);

        assert_eq!("foo", map.get_name("foo", Type::ChildElement).unwrap());
        assert_eq!(
            "foo_attr",
            map.get_name("foo_attr", Type::ChildElement).unwrap()
        );
        assert_eq!("foo_attr_1", map.get_name("foo", Type::Attribute).unwrap());
        assert_eq!(
            "foo_attr_2",
            map.get_name("foo_attr", Type::Attribute).unwrap()
        );
    }
}
