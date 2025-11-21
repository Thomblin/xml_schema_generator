//! Necessity is a helper struct to surround an (xml) element and mark it as optional or mandatory inside the given XML document
//! this is used to properly generate a Rust Struct than can be used for deserialization

/// Marks an XML element or attribute as optional or mandatory
/// 
/// This enum wraps values and tracks whether they must always appear in the XML
/// or can be omitted. This is used to determine whether to generate `Option<T>` types.
#[derive(Clone, Debug)]
pub enum Necessity<T> {
    Optional(T),
    Mandatory(T),
}

impl<T: std::cmp::PartialEq> std::cmp::PartialEq for Necessity<T> {
    /// Compares two `Necessity` values for equality
    /// 
    /// Two `Necessity` values are equal if they have the same variant (both Optional or both Mandatory)
    /// and their inner elements are equal.
    /// 
    /// # Arguments
    /// 
    /// * `other` - The other `Necessity` to compare with
    /// 
    /// # Returns
    /// 
    /// `true` if both have the same variant and equal inner values, `false` otherwise
    fn eq(&self, other: &Necessity<T>) -> bool {
        if std::mem::discriminant(self) != std::mem::discriminant(other) {
            return false;
        }

        self.inner_t() == other.inner_t()
    }
}

impl<T> Necessity<T> {
    /// Consumes the `Necessity` wrapper and returns the inner element
    /// 
    /// Works for both `Optional` and `Mandatory` variants.
    /// 
    /// # Returns
    /// 
    /// The inner element of type `T`
    pub fn into_inner_t(self) -> T {
        match self {
            Necessity::Optional(t) => t,
            Necessity::Mandatory(t) => t,
        }
    }

    /// Returns an immutable reference to the inner element
    /// 
    /// Works for both `Optional` and `Mandatory` variants.
    /// 
    /// # Returns
    /// 
    /// An immutable reference to the inner element
    pub fn inner_t(&self) -> &T {
        match self {
            Necessity::Optional(t) => t,
            Necessity::Mandatory(t) => t,
        }
    }

    /// Returns a mutable reference to the inner element
    /// 
    /// Works for both `Optional` and `Mandatory` variants.
    /// 
    /// # Returns
    /// 
    /// A mutable reference to the inner element
    pub fn inner_t_mut(&mut self) -> &mut T {
        match self {
            Necessity::Optional(t) => t,
            Necessity::Mandatory(t) => t,
        }
    }
}

/// Merges two lists of necessities according to intersection rules
/// 
/// Elements present as mandatory in both lists remain mandatory; all other elements
/// become optional. Elements from the second list are appended to the result.
///
/// # Arguments
/// 
/// * `vec` - First vector of necessities
/// * `other` - Second vector of necessities to merge
/// 
/// # Returns
/// 
/// A new vector containing the merged necessities with appropriate optional/mandatory status
/// 
/// # Example
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
pub fn merge_necessity<T: std::cmp::PartialEq>(
    vec: Vec<Necessity<T>>,
    other: Vec<Necessity<T>>,
) -> Vec<Necessity<T>> {
    let mut result: Vec<Necessity<T>> = Vec::new();

    for vec_item in vec.into_iter() {
        let mut found = false;
        let mut optional = true;
        for other_item in other.iter() {
            if other_item.inner_t() == vec_item.inner_t() {
                found = true;
                if let (Necessity::Mandatory(_), Necessity::Mandatory(_)) = (&other_item, &vec_item)
                {
                    optional = false
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
        let expected: Vec<Necessity<u8>> = vec![
            Necessity::Mandatory(1),
            Necessity::Optional(2),
            Necessity::Optional(3),
        ];

        let actual = merge_necessity(vec, other);

        assert_eq!(expected, actual);
    }

    #[test]
    fn merge_necessity_keeps_optional() {
        let vec: Vec<Necessity<u8>> = vec![
            Necessity::Mandatory(1),
            Necessity::Optional(2),
            Necessity::Optional(3),
        ];
        let other: Vec<Necessity<u8>> = vec![
            Necessity::Mandatory(1),
            Necessity::Mandatory(2),
            Necessity::Optional(3),
        ];
        let expected: Vec<Necessity<u8>> = vec![
            Necessity::Mandatory(1),
            Necessity::Optional(2),
            Necessity::Optional(3),
        ];

        let actual = merge_necessity(vec, other);

        assert_eq!(expected, actual);
    }

    #[test]
    fn merge_necessity_applies_optional() {
        let vec: Vec<Necessity<u8>> = vec![Necessity::Mandatory(1), Necessity::Mandatory(2)];
        let other: Vec<Necessity<u8>> = vec![Necessity::Mandatory(1), Necessity::Optional(2)];
        let expected: Vec<Necessity<u8>> = vec![Necessity::Mandatory(1), Necessity::Optional(2)];

        let actual = merge_necessity(vec, other);

        assert_eq!(expected, actual);
    }
}
