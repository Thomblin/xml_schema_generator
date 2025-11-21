//! parse a given XML document into a tree of Element structs

use std::collections::HashMap;
use std::io::BufRead;

use quick_xml::events::{BytesStart, Event};
use quick_xml::reader::Reader;

use crate::element::Element;
use crate::necessity::Necessity;

/// Converts quick_xml byte arrays to UTF-8 strings
/// 
/// # Arguments
/// 
/// * `e` - Byte array from quick_xml events
/// 
/// # Errors
/// 
/// Returns `ParserError::FromUtf8Error` if the bytes are not valid UTF-8
fn to_str<T: AsRef<[u8]>>(e: T) -> Result<String, ParserError> {
    String::from_utf8(e.as_ref().to_vec()).map_err(ParserError::FromUtf8Error)
}

/// Errors that can occur during XML parsing
#[derive(Debug)]
pub enum ParserError {
    /// Error from the quick_xml parser with position information
    QuickXmlError(u64, quick_xml::Error),
    /// UTF-8 conversion error
    FromUtf8Error(std::string::FromUtf8Error),
    /// XML attribute parsing error
    AttrError(quick_xml::events::attributes::AttrError),
    /// General parsing error with description
    ParsingError(String),
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::QuickXmlError(position, error) => {
                write!(f, "Error at position {position} : {error:?}")
            }
            Self::FromUtf8Error(e) => {
                write!(f, "{e}")
            }
            Self::AttrError(e) => {
                write!(f, "{e}")
            }
            Self::ParsingError(e) => {
                write!(f, "{e}")
            }
        }
    }
}

impl std::error::Error for ParserError {}

/// Parses an XML document into an Element tree structure
/// 
/// Reads the entire XML document and builds a tree of `Element` nodes representing
/// the schema, tracking which elements and attributes are optional or mandatory.
/// 
/// # Arguments
/// 
/// * `reader` - quick_xml Reader containing the XML document
/// 
/// # Returns
/// 
/// Returns the root `Element` or a `ParserError` if parsing fails
/// 
/// # Errors
/// 
/// Returns errors for malformed XML, encoding issues, or missing root elements
/// 
/// # Examples
/// 
/// ```
/// use quick_xml::Reader;
/// use xml_schema_generator::{into_struct, Options};
/// 
/// let xml = r#"<person id="123"><name>John</name></person>"#;
/// let mut reader = Reader::from_str(xml);
/// 
/// let element = into_struct(&mut reader).unwrap();
/// assert_eq!("person", element.name);
/// 
/// let struct_code = element.to_serde_struct(&Options::quick_xml_de());
/// assert!(struct_code.contains("pub struct Person"));
/// ```
pub fn into_struct<R>(reader: &mut Reader<R>) -> Result<Element<String>, ParserError>
where
    R: BufRead,
{
    let root = Element::new(String::from("root"), Vec::new());
    let mut root = build_struct(reader, root)?;

    let name = match root.children().first() {
        Some(element) => Ok(element.inner_t().name.clone()),
        None => Err(ParserError::ParsingError(
            "invalid XML, no root element found".into(),
        )),
    }?;

    match root.remove_child(&name) {
        Some(element) => Ok(element.into_inner_t()),
        None => Err(ParserError::ParsingError(
            "invalid XML, no root element found".into(),
        )),
    }
}

/// Extends an existing Element tree with data from another XML document
/// 
/// Merges attributes and child elements from the new XML document into the existing
/// element tree, marking differences as optional. This is useful for combining schemas
/// from multiple XML files.
/// 
/// # Arguments
/// 
/// * `reader` - quick_xml Reader containing the XML document
/// * `root` - Existing Element tree to extend
/// 
/// # Returns
/// 
/// Returns the merged `Element` or a `ParserError` if parsing fails
/// 
/// # Examples
/// 
/// ```
/// use quick_xml::Reader;
/// use xml_schema_generator::{into_struct, extend_struct};
/// 
/// // Parse first XML document
/// let xml1 = r#"<person id="1"><name>John</name></person>"#;
/// let mut reader1 = Reader::from_str(xml1);
/// let element = into_struct(&mut reader1).unwrap();
/// 
/// // Extend with second XML document that has an additional field
/// let xml2 = r#"<person id="2"><name>Jane</name><age>30</age></person>"#;
/// let mut reader2 = Reader::from_str(xml2);
/// let merged = extend_struct(&mut reader2, element).unwrap();
/// 
/// // The 'age' field will be marked as optional since it's not in both documents
/// assert!(merged.get_child(&"age".to_string()).is_some());
/// ```
pub fn extend_struct<R>(
    reader: &mut Reader<R>,
    root: Element<String>,
) -> Result<Element<String>, ParserError>
where
    R: BufRead,
{
    let mut wrapper = Element::new(String::from("root"), Vec::new());
    wrapper.add_unique_child(root);

    let mut root = build_struct(reader, wrapper)?;

    let name = match root.children().first() {
        Some(element) => Ok(element.inner_t().name.clone()),
        None => Err(ParserError::ParsingError(
            "invalid XML, no root element found".into(),
        )),
    }?;

    match root.remove_child(&name) {
        Some(element) => Ok(element.into_inner_t()),
        None => Err(ParserError::ParsingError(
            "invalid XML, no root element found".into(),
        )),
    }
}

/// Recursively builds an Element tree from XML events
/// 
/// Processes XML events from the reader, creating child elements and tracking their
/// attributes. Handles optional element detection and duplicate merging.
/// 
/// # Arguments
/// 
/// * `reader` - Mutable reference to the quick_xml Reader
/// * `root` - The root element to build the tree under
/// 
/// # Returns
/// 
/// The completed element tree, or a `ParserError` if parsing fails
fn build_struct<R>(
    reader: &mut Reader<R>,
    mut root: Element<String>,
) -> Result<Element<String>, ParserError>
where
    R: BufRead,
{
    let mut buf = Vec::new();
    let mut known_elements: Vec<String> = Vec::new();

    loop {
        match reader.read_event_into(&mut buf) {
            Ok(Event::Start(e)) => {
                let (children_count, check_optional_tags) =
                    count_children(root.get_child(&to_str(e.name())?));

                root = parse_tag::<R>(root, &e, &mut known_elements, Some(reader))?;

                if check_optional_tags {
                    root = tag_optional_children(root, e, children_count)?;
                }
            }
            Ok(Event::Text(e)) => root.text = Some(to_str(e.into_inner())?),
            Ok(Event::CData(e)) => root.text = Some(to_str(e.into_inner())?),
            Ok(Event::Empty(e)) => {
                // we don't pass the reader to parse_tag here, as we do not want to iterate into an empty element
                root = parse_tag::<R>(root, &e, &mut known_elements, None)?;
                root = tag_optional_children(root, e, HashMap::new())?;
            }
            Ok(Event::Eof | Event::End(_)) => return Ok(root),
            Ok(Event::Comment(_)) => (),
            Ok(Event::Decl(_)) => (),
            Ok(Event::PI(_)) => (),
            Ok(Event::DocType(_)) => (),
            Ok(Event::GeneralRef(_)) => (),
            Err(e) => return Err(ParserError::QuickXmlError(reader.buffer_position(), e)),
        }
        buf.clear();
    }
}

/// Records child element counts before parsing to detect optional elements
/// 
/// # Arguments
/// 
/// * `tag` - Optional reference to the element whose children should be counted
/// 
/// # Returns
/// 
/// A tuple containing:
/// - `HashMap<String, u32>`: Map of child names to their occurrence counts
/// - `bool`: Flag indicating whether optional element detection should be performed
fn count_children(tag: Option<&Necessity<Element<String>>>) -> (HashMap<String, u32>, bool) {
    let mut children_count: HashMap<String, u32> = HashMap::new();
    let mut check_optional_tags = false;

    if let Some(current_tag) = tag {
        check_optional_tags = true;
        for child in current_tag.inner_t().children().iter() {
            if let Necessity::Mandatory(c) = child {
                children_count.insert(c.name.clone(), c.count());
            }
        }
    }
    (children_count, check_optional_tags)
}

/// Compares before/after child counts to mark optional elements
/// 
/// Children whose count didn't increase or are missing in subsequent instances
/// of the parent element are marked as optional in the schema.
/// 
/// # Arguments
/// 
/// * `root` - The parent element containing the children to check
/// * `e` - The XML tag being processed
/// * `children_count` - Map of child names to their counts before parsing
/// 
/// # Returns
/// 
/// The modified root element with optional children marked, or a `ParserError`
fn tag_optional_children(
    mut root: Element<String>,
    e: BytesStart<'_>,
    children_count: HashMap<String, u32>,
) -> Result<Element<String>, ParserError> {
    let mut to_optional = Vec::new();

    if let Some(current_tag) = root.get_child(&to_str(e.name())?) {
        let parent = current_tag.inner_t();

        for (child_name, child_count) in children_count.iter() {
            if let Some(c) = parent.get_child(child_name) {
                if child_count == &c.inner_t().count() {
                    to_optional.push(child_name.clone());
                }
            }
        }

        for child in parent.children().iter() {
            if let Necessity::Mandatory(c) = child {
                if !children_count.contains_key(&c.name) {
                    to_optional.push(c.name.clone());
                }
            }
        }
    }

    if let Some(current_tag) = root.get_child_mut(&to_str(e.name())?) {
        let parent = current_tag.inner_t_mut();

        while let Some(name) = to_optional.pop() {
            parent.set_child_optional(&name);
        }
    }

    Ok(root)
}

/// Parses an XML tag and merges it with existing elements
/// 
/// Extracts the tag name and attributes, merging them with any existing child element
/// of the same name. Detects when elements appear multiple times and marks them accordingly.
/// 
/// # Arguments
/// 
/// * `root` - The parent element to add the parsed tag to
/// * `e` - The XML tag start event from quick_xml
/// * `known_elements` - Mutable vector tracking elements seen multiple times
/// * `reader` - Optional reader for parsing nested content (None for empty tags)
/// 
/// # Returns
/// 
/// The modified root element with the parsed tag added, or a `ParserError`
fn parse_tag<R>(
    mut root: Element<String>,
    e: &BytesStart<'_>,
    known_elements: &mut Vec<String>,
    reader: Option<&mut Reader<R>>,
) -> Result<Element<String>, ParserError>
where
    R: BufRead,
{
    let name = to_str(e.name())?;

    let new_child = match root.remove_child(&name) {
        Some(Necessity::Mandatory(child) | Necessity::Optional(child)) => {
            let mut attributes = Vec::new();
            for attr in e.attributes() {
                match attr {
                    Ok(attr) => attributes.push(Necessity::Mandatory(to_str(attr.key)?)),
                    Err(e) => return Err(ParserError::AttrError(e)),
                };
            }

            let mut new_child = child.merge_attr(attributes);

            if known_elements.contains(&name) {
                new_child.set_multiple();
            }

            new_child.increment();

            if let Some(reader) = reader {
                new_child = build_struct(reader, new_child)?;
            }
            new_child
        }
        None => {
            let raw_attributes = e.attributes();
            let mut attributes = Vec::new();

            for attr in raw_attributes {
                match attr {
                    Ok(attr) => attributes.push(to_str(attr.key)?),
                    Err(e) => return Err(ParserError::AttrError(e)),
                };
            }

            let mut child = Element::new(name, attributes);

            if known_elements.contains(&to_str(e.name())?) {
                child.set_multiple();
            }

            if let Some(reader) = reader {
                child = build_struct(reader, child)?;
            }
            child
        }
    };

    let name = to_str(e.name())?;
    if !known_elements.contains(&name) {
        known_elements.push(name);
    }

    root.add_unique_child(new_child);
    Ok(root)
}

#[cfg(test)]
mod tests {
    use super::{build_struct, extend_struct, Element, Necessity};
    use crate::element::macro_rule::element;
    use crate::{into_struct, ParserError};
    use pretty_assertions::assert_eq;
    use quick_xml::reader::Reader;

    #[test]
    fn into_struct_can_parse_simple_xml() {
        let xml = "<a>b</a>";
        let mut reader = Reader::from_str(xml);
        let mut root = Element::new(String::from("root"), Vec::new());

        root = build_struct(&mut reader, root).expect("expected to successfully parse into struct");

        assert_eq!(String::from("root"), root.name);
        assert_eq!(
            0,
            root.attributes().len(),
            "did not expect attributes to be stored"
        );
        assert_eq!(1, root.children().len(), "exptected exactly one child");

        let child = root
            .get_child_mut(&String::from("a"))
            .expect("expected to find a child named 'a'");

        assert_eq!(
            String::from("a"),
            child.inner_t().name,
            "expected name to equal 'a'"
        );
        assert_eq!(
            &String::from("b"),
            child
                .inner_t()
                .text
                .as_ref()
                .expect("expected to find a text"),
            "expected text to equal 'b'"
        );
    }

    #[test]
    fn into_struct_can_parse_complex_xml() {
        let xml = "<Vehicle AirConditionInd=\"true\" TransmissionType=\"Manual\"
            PassengerQuantity=\"4\" CodeContext=\"\">
            <VehType VehicleCategory=\"1\" DoorCount=\"2\" />
            <PictureURL>https://example.com/my.jpg</PictureURL>
        </Vehicle>";

        let mut reader = Reader::from_str(xml);
        let mut root = Element::new(String::from("root"), Vec::new());

        root = build_struct(&mut reader, root).expect("expected to successfully parse into struct");

        assert_eq!(1, root.children().len(), "exptected exactly one child");

        let tag_vehicle = root
            .get_child_mut(&String::from("Vehicle"))
            .expect("expected to find a child named 'Vehicle'")
            .inner_t_mut();
        assert_eq!(
            String::from("Vehicle"),
            tag_vehicle.name,
            "expected name to equal 'Vehicle'"
        );
        assert_eq!(
            4,
            tag_vehicle.attributes().len(),
            "expected Vehicle to hold 4 attributes"
        );
        assert_eq!(
            &vec![
                Necessity::Mandatory(String::from("AirConditionInd")),
                Necessity::Mandatory(String::from("TransmissionType")),
                Necessity::Mandatory(String::from("PassengerQuantity")),
                Necessity::Mandatory(String::from("CodeContext"))
            ],
            tag_vehicle.attributes(),
            "exptected to find all attributes"
        );
        assert_eq!(
            2,
            tag_vehicle.children().len(),
            "expected Vehicle to hold 2 children"
        );

        let tag_vehicle_type = tag_vehicle
            .get_child_mut(&String::from("VehType"))
            .expect("expected to find a child named 'VehType'")
            .inner_t();
        assert_eq!(
            String::from("VehType"),
            tag_vehicle_type.name,
            "expected name to equal 'VehType'"
        );
        assert_eq!(
            2,
            tag_vehicle_type.attributes().len(),
            "expected Vehicle to hold 2 attributes"
        );
        assert_eq!(
            &vec![
                Necessity::Mandatory(String::from("VehicleCategory")),
                Necessity::Mandatory(String::from("DoorCount"))
            ],
            tag_vehicle_type.attributes(),
            "exptected to find all attributes"
        );
        assert_eq!(
            0,
            tag_vehicle_type.children().len(),
            "expected Vehicle to hold 0 children"
        );

        let tag_picture_url = tag_vehicle
            .get_child_mut(&String::from("PictureURL"))
            .expect("expected to find a child named 'PictureURL'")
            .inner_t();
        assert_eq!(
            String::from("PictureURL"),
            tag_picture_url.name,
            "expected name to equal 'PictureURL'"
        );
        assert_eq!(
            &String::from("https://example.com/my.jpg"),
            tag_picture_url
                .text
                .as_ref()
                .expect("expected to find a url"),
            "expected text to equal 'https://example.com/my.jpg'"
        );
        assert_eq!(
            0,
            tag_picture_url.attributes().len(),
            "expected Vehicle to hold 0 attributes"
        );
        assert_eq!(
            0,
            tag_picture_url.children().len(),
            "expected Vehicle to hold 0 children"
        );
    }

    #[test]
    fn into_struct_keeps_count_of_children() {
        let xml = "<a>
            <b><c/></b>
            <b><c/><c/></b>
        </a>";
        let mut reader = Reader::from_str(xml);
        let mut root = Element::new(String::from("root"), Vec::new());

        root = build_struct(&mut reader, root).expect("expected to successfully parse into struct");

        let tag_a = root
            .get_child_mut(&String::from("a"))
            .expect("expected to find a child named 'a'")
            .inner_t_mut();
        assert_eq!(true, tag_a.standalone(), "expected a to be standalone");

        let tag_b = tag_a
            .get_child_mut(&String::from("b"))
            .expect("expected to find a child named 'b'")
            .inner_t_mut();
        assert_eq!(
            false,
            tag_b.standalone(),
            "expected b to be appear more than once"
        );

        let tag_c = tag_b
            .get_child_mut(&String::from("c"))
            .expect("expected to find a child named 'c'")
            .inner_t();
        assert_eq!(
            false,
            tag_c.standalone(),
            "expected c to be appear more than once"
        );
    }

    #[test]
    fn into_struct_keeps_count_of_children2() {
        let xml = "<a>
            <b><c/><c/></b>
            <b><c/></b>
        </a>";
        let mut reader = Reader::from_str(xml);
        let mut root = Element::new(String::from("root"), Vec::new());

        root = build_struct(&mut reader, root).expect("expected to successfully parse into struct");

        let tag_a = root
            .get_child_mut(&String::from("a"))
            .expect("expected to find a child named 'a'")
            .inner_t_mut();
        assert_eq!(true, tag_a.standalone(), "expected a to be standalone");

        let tag_b = tag_a
            .get_child_mut(&String::from("b"))
            .expect("expected to find a child named 'b'")
            .inner_t_mut();
        assert_eq!(
            false,
            tag_b.standalone(),
            "expected b to be appear more than once"
        );

        let tag_c = tag_b
            .get_child_mut(&String::from("c"))
            .expect("expected to find a child named 'c'")
            .inner_t();
        assert_eq!(
            false,
            tag_c.standalone(),
            "expected c to be appear more than once"
        );
    }

    #[test]
    fn into_struct_merges_attributes() {
        let xml = "<a>
            <b a='a' b='b'>test</b>
            <b a='a' c='c'/>
        </a>";

        let mut reader = Reader::from_str(xml);
        let mut root = Element::new(String::from("root"), Vec::new());

        root = build_struct(&mut reader, root).expect("expected to successfully parse into struct");

        let tag_a = root
            .get_child_mut(&String::from("a"))
            .expect("expected to find a child named 'a'")
            .inner_t_mut();
        let tag_b = tag_a
            .get_child_mut(&String::from("b"))
            .expect("expected to find a child named 'b'")
            .inner_t();

        assert_eq!(
            3,
            tag_b.attributes().len(),
            "expected b to contain 3 attributes"
        );
        assert_eq!(
            true,
            tag_b.has_attr(&String::from("a")),
            "expected b to contain attribute a"
        );
        assert_eq!(
            true,
            tag_b.has_attr(&String::from("b")),
            "expected b to contain attribute b"
        );
        assert_eq!(
            true,
            tag_b.has_attr(&String::from("c")),
            "expected b to contain attribute c"
        );
    }

    #[test]
    fn into_struct_detects_optional_attributes() {
        let xml = "<Charges>\
            <Charge FirstOnly=\"EUR\" Mandatory=\"305.26\"/>\
            <Charge SecondOnly=\"EUR\" Mandatory=\"305.26\"/>\
        </Charges>";

        let mut reader = Reader::from_str(xml);
        let mut root = Element::new(String::from("root"), Vec::new());

        root = build_struct(&mut reader, root).expect("expected to successfully parse into struct");

        let tag_vehicle_charges = root
            .get_child_mut(&String::from("Charges"))
            .expect("expected to find a child named 'Charges'")
            .inner_t_mut();
        let tag_vehicle_charge = tag_vehicle_charges
            .get_child_mut(&String::from("Charge"))
            .expect("expected to find a child named 'Charge'")
            .inner_t();

        assert_eq!(
            3,
            tag_vehicle_charge.attributes().len(),
            "expected Charge to contain 3 attributes"
        );
        assert_eq!(
            true,
            tag_vehicle_charge.has_attr(&String::from("FirstOnly")),
            "expected Charge to contain attribute FirstOnly"
        );
        assert_eq!(
            true,
            tag_vehicle_charge.has_attr(&String::from("SecondOnly")),
            "expected Charge to contain attribute SecondOnly"
        );
        assert_eq!(
            true,
            tag_vehicle_charge.has_attr(&String::from("Mandatory")),
            "expected Charge to contain attribute Mandatory"
        );

        assert_eq!(
            true,
            tag_vehicle_charge
                .attributes()
                .contains(&Necessity::Optional(String::from("FirstOnly"))),
            "expected FirstOnly to be optional"
        );
        assert_eq!(
            true,
            tag_vehicle_charge
                .attributes()
                .contains(&Necessity::Optional(String::from("SecondOnly"))),
            "expected SecondOnly to be optional"
        );
        assert_eq!(
            true,
            tag_vehicle_charge
                .attributes()
                .contains(&Necessity::Mandatory(String::from("Mandatory"))),
            "expected Mandatory to be mandatory"
        );
    }

    #[test]
    fn into_struct_detects_optional_attributes2() {
        let xml = "
        <Avail>
            <Core Status=\"Available\">
                <Rate>
                    <Distance Unlimited=\"false\" Quantity=\"200\"
                    Name=\"Km\" Unit=\"Day\" />
                </Rate>
            </Core>
            <Core Status=\"Available\">
                <Rate>
                    <Distance Unlimited=\"false\" Quantity=\"225\"
                    Name=\"Km\" Unit=\"Day\" />
                </Rate>
            </Core>
            <Core Status=\"Available\">
                <Rate>
                    <Distance Unlimited=\"true\" Name=\"Mile\" />
                </Rate>
            </Core>
            <Core Status=\"Available\">
                <Rate>
                    <Distance Unlimited=\"false\" Quantity=\"200\"
                    Name=\"Km\" Unit=\"Day\" />
                </Rate>
            </Core>
        </Avail>
        ";

        let mut reader = Reader::from_str(xml);
        let mut root = Element::new(String::from("root"), Vec::new());

        root = build_struct(&mut reader, root).expect("expected to successfully parse into struct");
        let tag_vehicle_avail = root
            .get_child_mut(&String::from("Avail"))
            .expect("expected to find a child named 'Avail'")
            .inner_t_mut();
        let tag_vehicle_core = tag_vehicle_avail
            .get_child_mut(&String::from("Core"))
            .expect("expected to find a child named 'Core'")
            .inner_t_mut();
        let tag_rental_rate = tag_vehicle_core
            .get_child_mut(&String::from("Rate"))
            .expect("expected to find a child named 'Rate'")
            .inner_t_mut();
        let tag_vehicle_rate_distance = tag_rental_rate
            .get_child_mut(&String::from("Distance"))
            .expect("expected to find a child named 'Distance'")
            .inner_t_mut();

        assert_eq!(
            4,
            tag_vehicle_rate_distance.attributes().len(),
            "expected Distance to contain 4 attributes"
        );
        assert_eq!(
            true,
            tag_vehicle_rate_distance.has_attr(&String::from("Unlimited")),
            "expected Distance to contain attribute Unlimited"
        );
        assert_eq!(
            true,
            tag_vehicle_rate_distance.has_attr(&String::from("Quantity")),
            "expected Distance to contain attribute Quantity"
        );
        assert_eq!(
            true,
            tag_vehicle_rate_distance.has_attr(&String::from("Name")),
            "expected Distance to contain attribute Name"
        );
        assert_eq!(
            true,
            tag_vehicle_rate_distance.has_attr(&String::from("Unit")),
            "expected Distance to contain attribute Unit"
        );

        assert_eq!(
            true,
            tag_vehicle_rate_distance
                .attributes()
                .contains(&Necessity::Mandatory(String::from("Unlimited"))),
            "expected Unlimited to be mandatory"
        );
        assert_eq!(
            true,
            tag_vehicle_rate_distance
                .attributes()
                .contains(&Necessity::Optional(String::from("Quantity"))),
            "expected Quantity to be optional"
        );
        assert_eq!(
            true,
            tag_vehicle_rate_distance
                .attributes()
                .contains(&Necessity::Mandatory(String::from("Name"))),
            "expected Name to be mandatory"
        );
        assert_eq!(
            true,
            tag_vehicle_rate_distance
                .attributes()
                .contains(&Necessity::Optional(String::from("Unit"))),
            "expected Unit to be optional"
        );
    }

    #[test]
    fn into_struct_detects_optional_children() {
        let xml = "<Charges>
            <Charge><FirstOnly/><FirstOnly/><Mandatory/></Charge>
            <Charge><SecondOnly/><Mandatory/></Charge>
        </Charges>";

        let mut reader = Reader::from_str(xml);
        let mut root = Element::new(String::from("root"), Vec::new());

        root = build_struct(&mut reader, root).expect("expected to successfully parse into struct");

        let tag_vehicle_charges = root
            .get_child_mut(&String::from("Charges"))
            .expect("expected to find a child named 'Charges'")
            .inner_t_mut();
        let tag_vehicle_charge = tag_vehicle_charges
            .get_child_mut(&String::from("Charge"))
            .expect("expected to find a child named 'Charge'")
            .inner_t_mut();

        assert_eq!(
            3,
            tag_vehicle_charge.children().len(),
            "expected Charge to contain 3 children"
        );
        assert_eq!(
            true,
            tag_vehicle_charge.has_child(&String::from("FirstOnly")),
            "expected Charge to contain child FirstOnly"
        );
        assert_eq!(
            true,
            tag_vehicle_charge.has_child(&String::from("SecondOnly")),
            "expected Charge to contain child SecondOnly"
        );
        assert_eq!(
            true,
            tag_vehicle_charge.has_child(&String::from("Mandatory")),
            "expected Charge to contain child Mandatory"
        );

        let tag_first_only = tag_vehicle_charge
            .get_child_mut(&String::from("FirstOnly"))
            .expect("expected to find a child named 'FirstOnly'");
        match tag_first_only {
            Necessity::Optional(_) => (),
            Necessity::Mandatory(_) => panic!("expected FirstOnly to be optional"),
        }

        let tag_second_only = tag_vehicle_charge
            .get_child_mut(&String::from("SecondOnly"))
            .expect("expected to find a child named 'SecondOnly'");
        match tag_second_only {
            Necessity::Optional(_) => (),
            Necessity::Mandatory(_) => panic!("expected SecondOnly to be optional"),
        }

        let tag_mandatory = tag_vehicle_charge
            .get_child_mut(&String::from("Mandatory"))
            .expect("expected to find a child named 'Mandatory'");
        match tag_mandatory {
            Necessity::Optional(_) => panic!("expected Mandatory to be mandatory"),
            Necessity::Mandatory(_) => (),
        }
    }

    #[test]
    fn into_struct_detects_optional_children3() {
        let xml = "<Charges>
            <Charge CurrencyCode=\"EUR\">
                <Calculation UnitName=\"Day\" Quantity=\"7\" />
            </Charge>
            <Charge CurrencyCode=\"EUR\"/>
        </Charges>";

        let mut reader = Reader::from_str(xml);
        let mut root = Element::new(String::from("root"), Vec::new());

        root = build_struct(&mut reader, root).expect("expected to successfully parse into struct");

        let tag_vehicle_charges = root
            .get_child_mut(&String::from("Charges"))
            .expect("expected to find a child named 'Charges'")
            .inner_t_mut();
        let tag_vehicle_charge = tag_vehicle_charges
            .get_child_mut(&String::from("Charge"))
            .expect("expected to find a child named 'Charge'")
            .inner_t_mut();

        assert_eq!(
            1,
            tag_vehicle_charge.children().len(),
            "expected Charge to contain 1 children"
        );
        assert_eq!(
            true,
            tag_vehicle_charge.has_child(&String::from("Calculation")),
            "expected Charge to contain child Calculation"
        );

        let tag_vehicle_charge = tag_vehicle_charge
            .get_child(&String::from("Calculation"))
            .expect("expected to find a child named 'Calculation'");
        match tag_vehicle_charge {
            Necessity::Optional(_) => (),
            Necessity::Mandatory(_) => panic!("expected Calculation to be optional"),
        }
    }

    #[test]
    fn into_struct_detects_optional_children4() {
        let xml = "<Charges>
            <Charge CurrencyCode=\"EUR\"/>
            <Charge CurrencyCode=\"EUR\">
                <Calculation UnitName=\"Day\" Quantity=\"7\" />
            </Charge>
        </Charges>";

        let mut reader = Reader::from_str(xml);
        let mut root = Element::new(String::from("root"), Vec::new());

        root = build_struct(&mut reader, root).expect("expected to successfully parse into struct");

        let tag_vehicle_charges = root
            .get_child_mut(&String::from("Charges"))
            .expect("expected to find a child named 'Charges'")
            .inner_t_mut();
        let tag_vehicle_charge = tag_vehicle_charges
            .get_child_mut(&String::from("Charge"))
            .expect("expected to find a child named 'Charge'")
            .inner_t_mut();

        assert_eq!(
            1,
            tag_vehicle_charge.children().len(),
            "expected Charge to contain 1 children"
        );
        assert_eq!(
            true,
            tag_vehicle_charge.has_child(&String::from("Calculation")),
            "expected Charge to contain child Calculation"
        );

        let tag_vehicle_charge = tag_vehicle_charge
            .get_child(&String::from("Calculation"))
            .expect("expected to find a child named 'Calculation'");
        match tag_vehicle_charge {
            Necessity::Optional(_) => (),
            Necessity::Mandatory(_) => panic!("expected Calculation to be optional"),
        }
    }

    #[test]
    fn into_struct_returns_an_quickxml_error() {
        let xml = "<a></b>";
        let mut reader = Reader::from_str(xml);
        let root = Element::new(String::from("root"), Vec::new());

        match build_struct(&mut reader, root) {
            Err(ParserError::QuickXmlError(
                position,
                quick_xml::Error::IllFormed(quick_xml::errors::IllFormedError::MismatchedEndTag {
                    expected: a,
                    found: b,
                }),
            )) => {
                assert_eq!(7, position);
                assert_eq!("a".to_string(), a);
                assert_eq!("b".to_string(), b);
            }
            any => panic!("expected a specific ParserError for broken XML instead of {any:?}"),
        }
    }

    // https://github.com/Thomblin/xml_schema_generator/issues/3
    #[test]
    fn into_struct_detects_multiple_children_issue3() {
        let xml = "<a><b>asd</b><b>fgh</b></a>";
        let mut reader = Reader::from_str(xml);

        let mut root =
            into_struct(&mut reader).expect("expected to successfully parse into struct");

        let child = root
            .get_child_mut(&String::from("b"))
            .expect("expected to find a child named 'b'");

        assert_eq!(
            false,
            child.inner_t().standalone(),
            "expected b to appear multiple times within a"
        );
    }

    #[test]
    fn extend_struct_returns_similar_simple_struct() {
        let xml = "<a>b</a>";
        let mut reader = Reader::from_str(xml);

        let root = into_struct(&mut reader).expect("expected to successfully parse into struct");

        let mut reader = Reader::from_str(xml);
        let root =
            extend_struct(&mut reader, root).expect("expected to successfully extend struct");

        assert_eq!(root.name, String::from("a"));
        assert_eq!(0, root.children().len());
        assert_eq!(0, root.attributes().len());
        assert_eq!(true, root.standalone());
    }

    #[test]
    fn extend_struct_combines_attributes() {
        let xml = "<a b=\"x\" c=\"x\">b</a>";
        let mut reader = Reader::from_str(xml);

        let root = into_struct(&mut reader).expect("expected to successfully parse into struct");

        let xml = "<a b=\"x\" d=\"x\">b</a>";
        let mut reader = Reader::from_str(xml);
        let root =
            extend_struct(&mut reader, root).expect("expected to successfully extend struct");

        assert_eq!(3, root.attributes().len());
        assert_eq!(
            &vec![
                Necessity::Mandatory("b".to_string()),
                Necessity::Optional("c".to_string()),
                Necessity::Optional("d".to_string()),
            ],
            root.attributes()
        );
    }

    #[test]
    fn extend_struct_combines_children() {
        let xml = "<a><c>X</c><d>X</d></a>";
        let mut reader = Reader::from_str(xml);

        let root = into_struct(&mut reader).expect("expected to successfully parse into struct");

        let xml = "<a><d>X</d><e>X</e></a>";
        let mut reader = Reader::from_str(xml);
        let root =
            extend_struct(&mut reader, root).expect("expected to successfully extend struct");

        assert_eq!(3, root.children().len());
        assert_eq!(
            &vec![
                Necessity::Mandatory(element!("d".to_string(), Some("X".to_string()))),
                Necessity::Optional(element!("e".to_string(), Some("X".to_string()))),
                Necessity::Optional(element!("c".to_string(), Some("X".to_string()))),
            ],
            root.children()
        );
    }

    #[test]
    fn extend_struct_combines_standalone_and_multiple() {
        let xml = "<a><c>X</c></a>";
        let mut reader = Reader::from_str(xml);

        let root = into_struct(&mut reader).expect("expected to successfully parse into struct");

        let xml = "<a><c>X</c><c>X</c></a>";
        let mut reader = Reader::from_str(xml);
        let root =
            extend_struct(&mut reader, root).expect("expected to successfully extend struct");

        assert_eq!(1, root.children().len());
        assert_eq!(
            false,
            root.get_child(&"c".to_string())
                .unwrap()
                .inner_t()
                .standalone()
        );
    }

    #[test]
    fn extend_struct_combines_standalone_children() {
        let xml = "<a><c>X</c></a>";
        let mut reader = Reader::from_str(xml);

        let root = into_struct(&mut reader).expect("expected to successfully parse into struct");

        let xml = "<a><c>X</c></a>";
        let mut reader = Reader::from_str(xml);
        let root =
            extend_struct(&mut reader, root).expect("expected to successfully extend struct");

        assert_eq!(1, root.children().len());
        assert_eq!(
            true,
            root.get_child(&"c".to_string())
                .unwrap()
                .inner_t()
                .standalone()
        );
    }

    #[test]
    fn extend_struct_combines_multiple_and_standalone() {
        let xml = "<a><c>X</c><c>X</c></a>";
        let mut reader = Reader::from_str(xml);

        let root = into_struct(&mut reader).expect("expected to successfully parse into struct");

        let xml = "<a><c>X</c></a>";
        let mut reader = Reader::from_str(xml);
        let root =
            extend_struct(&mut reader, root).expect("expected to successfully extend struct");

        assert_eq!(1, root.children().len());
        assert_eq!(
            false,
            root.get_child(&"c".to_string())
                .unwrap()
                .inner_t()
                .standalone()
        );
    }

    #[test]
    fn extend_struct_combines_multiple_children() {
        let xml = "<a><c>X</c><c>X</c></a>";
        let mut reader = Reader::from_str(xml);

        let root = into_struct(&mut reader).expect("expected to successfully parse into struct");

        let xml = "<a><c>X</c><c>X</c></a>";
        let mut reader = Reader::from_str(xml);
        let root =
            extend_struct(&mut reader, root).expect("expected to successfully extend struct");

        assert_eq!(1, root.children().len());
        assert_eq!(
            false,
            root.get_child(&"c".to_string())
                .unwrap()
                .inner_t()
                .standalone()
        );
    }

    #[test]
    fn into_struct_can_parse_xml_with_namespace() {
        let xml = "<a xmlns:h=\"test\" h:c=\"x\"><h:b>y</h:b></a>";
        let mut reader = Reader::from_str(xml);

        let root = into_struct(&mut reader).expect("expected to successfully parse into struct");

        assert_eq!(2, root.attributes().len());
        assert_eq!(
            &vec![
                Necessity::Mandatory("xmlns:h".to_string()),
                Necessity::Mandatory("h:c".to_string()),
            ],
            root.attributes()
        );

        assert_eq!(1, root.children().len());
        assert_eq!(
            &element!("h:b".to_string(), Some("y".to_string())),
            root.get_child(&"h:b".to_string()).unwrap().inner_t()
        );
    }
}
