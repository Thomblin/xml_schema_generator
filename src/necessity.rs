//! Necessity is a helper struct to surround an (xml) element and mark it as optional or mandatory inside the given XML document
//! this is used to properly generate a Rust Struct than can be used for deserialization

/// mark the included variable as optional or mandatory
#[derive(Debug)]
pub enum Necessity<T> {
    Optional(T),
    Mandatory(T)
}

impl<T: std::cmp::PartialEq> std::cmp::PartialEq for Necessity<T> {
    /// treat two Necessities as equal if they are both optional or mandatory and if their inner elements are equal
    fn eq(&self, other: &Necessity<T>) -> bool {
        if std::mem::discriminant(self) != std::mem::discriminant(other) {
            return false;
        }

        self.inner_t() == other.inner_t()
    }
}

impl<T> Necessity<T> {  
    /// return the inner element
    pub fn into_inner_t(self) -> T {
        match self {
            Necessity::Optional(t) => t,
            Necessity::Mandatory(t) => t
        }
    }

    /// return a reference to the inner element
    pub fn inner_t(&self) -> &T {
        match self {
            Necessity::Optional(t) => t,
            Necessity::Mandatory(t) => t
        }
    }

    /// return a mutable reference to the inner element
    pub fn inner_t_mut(&mut self) -> &mut T {
        match self {
            Necessity::Optional(t) => t,
            Necessity::Mandatory(t) => t
        }
    }
}

/// merge two lists of necessities
/// the result contains mandatory elements if they were present as mandatory in both given lists
/// all other elements are returned as optional
/// elements of the second list are appended to the first list
/// 
/// Example
/// ```
/// use xml_schema_generator::Necessity;
/// 
/// let vec: Vec<Necessity<u8>> = vec![Necessity::Mandatory(1), Necessity::Mandatory(2), Necessity::Mandatory(4) ];
/// let other: Vec<Necessity<u8>> = vec![Necessity::Mandatory(1), Necessity::Mandatory(3), Necessity::Optional(4)];
/// let expected: Vec<Necessity<u8>> = vec![Necessity::Mandatory(1), Necessity::Optional(2), Necessity::Optional(4), Necessity::Optional(3)];
/// 
/// let actual = xml_schema_generator::merge_necessity(vec, other);
/// 
/// assert_eq!(actual, expected);
/// ```
pub fn merge_necessity<T: std::cmp::PartialEq>(vec: Vec<Necessity<T>>, other: Vec<Necessity<T>>) -> Vec<Necessity<T>> {  
    let mut result: Vec<Necessity<T>> = Vec::new();
       
    for vec_item in vec.into_iter() {
        let mut found = false;
        let mut optional = true;
        for other_item in other.iter() {
            if other_item.inner_t() == vec_item.inner_t() {
                found = true;
                match (&other_item, &vec_item) {
                    (Necessity::Mandatory(_), Necessity::Mandatory(_)) => optional = false,
                    _ => (),
                }
                break;
            }
        }
        match found {
            true => match optional {
                true => result.push(Necessity::Optional(vec_item.into_inner_t())),
                false => result.push(Necessity::Mandatory(vec_item.into_inner_t())),
            },            
            false => result.push(Necessity::Optional(vec_item.into_inner_t())),
        }
    }

    for other_item in other.into_iter().rev() {
        let mut found = false;
        for result_item in result.iter() {
            if other_item.inner_t() == result_item.inner_t() {
                found = true;
                break;
            }
        }
        match found {
            true => (),
            false => result.push(Necessity::Optional(other_item.into_inner_t())),
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use crate::necessity::{merge_necessity, Necessity};

    #[test]
    fn merge_necessity_changes_necessity_states() {
        let vec: Vec<Necessity<u8>> = vec![Necessity::Mandatory(1), Necessity::Mandatory(2)];
        let other: Vec<Necessity<u8>> = vec![Necessity::Mandatory(1), Necessity::Mandatory(3)];
        let expected: Vec<Necessity<u8>> = vec![Necessity::Mandatory(1), Necessity::Optional(2), Necessity::Optional(3)];

        let actual = merge_necessity(vec, other);

        assert_eq!(actual, expected);
    }

    #[test]
    fn merge_necessity_keeps_optional() {
        let vec: Vec<Necessity<u8>> = vec![Necessity::Mandatory(1), Necessity::Optional(2), Necessity::Optional(3)];
        let other: Vec<Necessity<u8>> = vec![Necessity::Mandatory(1), Necessity::Mandatory(2), Necessity::Optional(3)];
        let expected: Vec<Necessity<u8>> = vec![Necessity::Mandatory(1), Necessity::Optional(2), Necessity::Optional(3)];

        let actual = merge_necessity(vec, other);

        assert_eq!(actual, expected);
    }

    #[test]
    fn merge_necessity_applies_optional() {
        let vec: Vec<Necessity<u8>> = vec![Necessity::Mandatory(1), Necessity::Mandatory(2)];
        let other: Vec<Necessity<u8>> = vec![Necessity::Mandatory(1), Necessity::Optional(2)];
        let expected: Vec<Necessity<u8>> = vec![Necessity::Mandatory(1), Necessity::Optional(2)];

        let actual = merge_necessity(vec, other);

        assert_eq!(actual, expected);
    }
}