//! defines a helper macro to write shorter tests
use super::Element;

/// usage: element!(name: T, text: Option<T>, attributes: Vec<T>, children: Vec<Element<T>>) for a standalone element
/// or   : element!(name: T, text: Option<T>, attributes: Vec<T>, children: Vec<Element<T>>, multiple) if the element appears multiple times within it' parent
/// where T is String or &str usually (T: std::cmp::PartialEq + std::fmt::Display + std::fmt::Debug)
/// all parameters but name are optional
macro_rules! element {
    ($name:expr, $text:expr, $attributes:expr, $children:expr, multiple) => {{
        let mut temp = element!($name, $text, $attributes, $children);
        temp.set_multiple();
        temp
    }};
    ($name:expr, $text:expr, $attributes:expr, $children:expr) => {{
        let mut temp = Element::new($name, $attributes);
        temp.text = $text;
        for c in $children.into_iter() {
            temp.add_unique_child(c)
        }
        temp
    }};
    ($name:expr, $text:expr, $attributes:expr) => {{
        element!($name, $text, $attributes, vec![])
    }};
    ($name:expr, $text:expr) => {{
        element!($name, $text, vec![], vec![])
    }};
    ($name:expr) => {{
        element!($name, None, vec![], vec![])
    }};
}

pub(crate) use element;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_element() {
        let a = element!("a", Some("text"), vec!["attribute"], vec![]);

        assert_eq!("a", a.name);
        assert_eq!(Some("text"), a.text);
        assert_eq!(1, a.attributes().len());
        assert_eq!(0, a.children.len());
        assert!(a.standalone());
    }

    #[test]
    fn create_element_without_children() {
        let a = element!("a", Some("text"), vec!["attribute"]);

        assert_eq!("a", a.name);
        assert_eq!(Some("text"), a.text);
        assert_eq!(1, a.attributes().len());
        assert_eq!(0, a.children.len());
        assert!(a.standalone());
    }

    #[test]
    fn create_element_without_children_or_attributes() {
        let a = element!("a", Some("text"));

        assert_eq!("a", a.name);
        assert_eq!(Some("text"), a.text);
        assert_eq!(0, a.attributes().len());
        assert_eq!(0, a.children.len());
        assert!(a.standalone());
    }

    #[test]
    fn create_element_with_name_only() {
        let a = element!("a");

        assert_eq!("a", a.name);
        assert_eq!(None, a.text);
        assert_eq!(0, a.attributes().len());
        assert_eq!(0, a.children.len());
        assert!(a.standalone());
    }

    #[test]
    fn create_element_without_text() {
        let a = element!("a", None, vec![], vec![]);

        assert_eq!("a", a.name);
        assert_eq!(None, a.text);
    }

    #[test]
    fn create_element_as_multiple() {
        let a = element!("a", None, vec![], vec![], multiple);

        assert!(!a.standalone());
    }

    #[test]
    fn create_element_with_multiple_attributes() {
        let a = element!("a", None, vec!["a1", "a2"], vec![]);

        assert_eq!(2, a.attributes().len());
        assert_eq!(
            &"a1",
            a.attributes().first()
                .expect("expected to get first attribute")
                .inner_t()
        );
        assert_eq!(
            &"a2",
            a.attributes()
                .get(1)
                .expect("expected to get second attribute")
                .inner_t()
        );
    }

    #[test]
    fn create_element_with_children() {
        let c = element!("c", None, vec![], vec![]);
        let b = element!("b", None, vec![], vec![]);
        let a = element!("a", None, vec![], vec![b, c]);

        assert_eq!(2, a.children.len());
        assert_eq!(
            "b",
            a.children()
                .first()
                .expect("expected to get child b")
                .inner_t()
                .name
        );
        assert_eq!(
            "c",
            a.children()
                .get(1)
                .expect("expected to get child c")
                .inner_t()
                .name
        );
    }
}
