//! parse a given XML document into a tree of Element structs

use std::collections::HashMap;
use std::io::BufRead;

use quick_xml::events::{BytesStart, Event};
use quick_xml::reader::Reader;

use crate::element::Element;
use crate::necessity::Necessity;

/// helper function to convert quick_xml bytes to a String
/// TODO: maybe we can do better than this ...
fn to_str<T: AsRef<[u8]>>(e: T) -> Result<String, ParserError> {
    String::from_utf8(e.as_ref().to_vec()).map_err(ParserError::FromUtf8Error)
}

#[derive(Debug)]
pub enum ParserError {
    QuickXmlError(usize, quick_xml::Error),
    FromUtf8Error(std::string::FromUtf8Error),
    AttrError(quick_xml::events::attributes::AttrError),
    ParsingError(String),
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::QuickXmlError(position, error) => {
                write!(f, "Error at position {} : {:?}", position, error)
            }
            Self::FromUtf8Error(e) => {
                write!(f, "{}", e)
            }
            Self::AttrError(e) => {
                write!(f, "{}", e)
            }
            Self::ParsingError(e) => {
                write!(f, "{}", e)
            }
        }
    }
}

impl std::error::Error for ParserError {}

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

/// parse a given XML document into a tree of Element structs below the given root element
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
            Ok(Event::CData(e)) => {
                root.text = Some(to_str(e.into_inner())?);
            }
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
            Err(e) => return Err(ParserError::QuickXmlError(reader.buffer_position(), e)),
        }
        buf.clear();
    }
}

/// helper function to store the count of each child element before an XML element is evaluated
/// to be able to find optional child elements afterwards
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

/// helper function to compare the child count memorized in fn count_children
/// with the current child count after parsing an XML element to detect and mark optional children
/// that do not appear in each parent element
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
                if children_count.get(&c.name).is_none() {
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

/// parse the given tag, detect the name and attributes
/// if an element of the same name already exists inside the current parent element, merge attributes and children to represent the overall state correctly
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
    use quick_xml::reader::Reader;

    use pretty_assertions::assert_eq;

    use crate::{into_struct, ParserError};

    use super::{build_struct, Element, Necessity};

    #[test]
    fn into_struct_can_parse_simple_xml() {
        let xml = "<a>b</a>";
        let mut reader = Reader::from_str(xml);
        let mut root = Element::new(String::from("root"), Vec::new());

        root = build_struct(&mut reader, root).expect("expected to successfully parse into struct");

        assert_eq!(root.name, String::from("root"));
        assert_eq!(
            root.attributes().len(),
            0,
            "did not expect attributes to be stored"
        );
        assert_eq!(root.children().len(), 1, "exptected exactly one child");

        let child = root
            .get_child_mut(&String::from("a"))
            .expect("expected to find a child named 'a'");

        assert_eq!(
            child.inner_t().name,
            String::from("a"),
            "expected name to equal 'a'"
        );
        assert_eq!(
            child
                .inner_t()
                .text
                .as_ref()
                .expect("expected to find a text"),
            &String::from("b"),
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

        assert_eq!(root.children().len(), 1, "exptected exactly one child");

        let tag_vehicle = root
            .get_child_mut(&String::from("Vehicle"))
            .expect("expected to find a child named 'Vehicle'")
            .inner_t_mut();
        assert_eq!(
            tag_vehicle.name,
            String::from("Vehicle"),
            "expected name to equal 'Vehicle'"
        );
        assert_eq!(
            tag_vehicle.attributes().len(),
            4,
            "expected Vehicle to hold 4 attributes"
        );
        assert_eq!(
            tag_vehicle.attributes(),
            &vec![
                Necessity::Mandatory(String::from("AirConditionInd")),
                Necessity::Mandatory(String::from("TransmissionType")),
                Necessity::Mandatory(String::from("PassengerQuantity")),
                Necessity::Mandatory(String::from("CodeContext"))
            ],
            "exptected to find all attributes"
        );
        assert_eq!(
            tag_vehicle.children().len(),
            2,
            "expected Vehicle to hold 2 children"
        );

        let tag_vehicle_type = tag_vehicle
            .get_child_mut(&String::from("VehType"))
            .expect("expected to find a child named 'VehType'")
            .inner_t();
        assert_eq!(
            tag_vehicle_type.name,
            String::from("VehType"),
            "expected name to equal 'VehType'"
        );
        assert_eq!(
            tag_vehicle_type.attributes().len(),
            2,
            "expected Vehicle to hold 2 attributes"
        );
        assert_eq!(
            tag_vehicle_type.attributes(),
            &vec![
                Necessity::Mandatory(String::from("VehicleCategory")),
                Necessity::Mandatory(String::from("DoorCount"))
            ],
            "exptected to find all attributes"
        );
        assert_eq!(
            tag_vehicle_type.children().len(),
            0,
            "expected Vehicle to hold 0 children"
        );

        let tag_picture_url = tag_vehicle
            .get_child_mut(&String::from("PictureURL"))
            .expect("expected to find a child named 'PictureURL'")
            .inner_t();
        assert_eq!(
            tag_picture_url.name,
            String::from("PictureURL"),
            "expected name to equal 'PictureURL'"
        );
        assert_eq!(
            tag_picture_url
                .text
                .as_ref()
                .expect("expected to find a url"),
            &String::from("https://example.com/my.jpg"),
            "expected text to equal 'https://example.com/my.jpg'"
        );
        assert_eq!(
            tag_picture_url.attributes().len(),
            0,
            "expected Vehicle to hold 0 attributes"
        );
        assert_eq!(
            tag_picture_url.children().len(),
            0,
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
        assert_eq!(tag_a.standalone(), true, "expected a to be standalone");

        let tag_b = tag_a
            .get_child_mut(&String::from("b"))
            .expect("expected to find a child named 'b'")
            .inner_t_mut();
        assert_eq!(
            tag_b.standalone(),
            false,
            "expected b to be appear more than once"
        );

        let tag_c = tag_b
            .get_child_mut(&String::from("c"))
            .expect("expected to find a child named 'c'")
            .inner_t();
        assert_eq!(
            tag_c.standalone(),
            false,
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
        assert_eq!(tag_a.standalone(), true, "expected a to be standalone");

        let tag_b = tag_a
            .get_child_mut(&String::from("b"))
            .expect("expected to find a child named 'b'")
            .inner_t_mut();
        assert_eq!(
            tag_b.standalone(),
            false,
            "expected b to be appear more than once"
        );

        let tag_c = tag_b
            .get_child_mut(&String::from("c"))
            .expect("expected to find a child named 'c'")
            .inner_t();
        assert_eq!(
            tag_c.standalone(),
            false,
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
            tag_b.attributes().len(),
            3,
            "expected b to contain 3 attributes"
        );
        assert_eq!(
            tag_b.has_attr(&String::from("a")),
            true,
            "expected b to contain attribute a"
        );
        assert_eq!(
            tag_b.has_attr(&String::from("b")),
            true,
            "expected b to contain attribute b"
        );
        assert_eq!(
            tag_b.has_attr(&String::from("c")),
            true,
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
            tag_vehicle_charge.attributes().len(),
            3,
            "expected Charge to contain 3 attributes"
        );
        assert_eq!(
            tag_vehicle_charge.has_attr(&String::from("FirstOnly")),
            true,
            "expected Charge to contain attribute FirstOnly"
        );
        assert_eq!(
            tag_vehicle_charge.has_attr(&String::from("SecondOnly")),
            true,
            "expected Charge to contain attribute SecondOnly"
        );
        assert_eq!(
            tag_vehicle_charge.has_attr(&String::from("Mandatory")),
            true,
            "expected Charge to contain attribute Mandatory"
        );

        assert_eq!(
            tag_vehicle_charge
                .attributes()
                .contains(&Necessity::Optional(String::from("FirstOnly"))),
            true,
            "expected FirstOnly to be optional"
        );
        assert_eq!(
            tag_vehicle_charge
                .attributes()
                .contains(&Necessity::Optional(String::from("SecondOnly"))),
            true,
            "expected SecondOnly to be optional"
        );
        assert_eq!(
            tag_vehicle_charge
                .attributes()
                .contains(&Necessity::Mandatory(String::from("Mandatory"))),
            true,
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
            tag_vehicle_rate_distance.attributes().len(),
            4,
            "expected Distance to contain 4 attributes"
        );
        assert_eq!(
            tag_vehicle_rate_distance.has_attr(&String::from("Unlimited")),
            true,
            "expected Distance to contain attribute Unlimited"
        );
        assert_eq!(
            tag_vehicle_rate_distance.has_attr(&String::from("Quantity")),
            true,
            "expected Distance to contain attribute Quantity"
        );
        assert_eq!(
            tag_vehicle_rate_distance.has_attr(&String::from("Name")),
            true,
            "expected Distance to contain attribute Name"
        );
        assert_eq!(
            tag_vehicle_rate_distance.has_attr(&String::from("Unit")),
            true,
            "expected Distance to contain attribute Unit"
        );

        assert_eq!(
            tag_vehicle_rate_distance
                .attributes()
                .contains(&Necessity::Mandatory(String::from("Unlimited"))),
            true,
            "expected Unlimited to be mandatory"
        );
        assert_eq!(
            tag_vehicle_rate_distance
                .attributes()
                .contains(&Necessity::Optional(String::from("Quantity"))),
            true,
            "expected Quantity to be optional"
        );
        assert_eq!(
            tag_vehicle_rate_distance
                .attributes()
                .contains(&Necessity::Mandatory(String::from("Name"))),
            true,
            "expected Name to be mandatory"
        );
        assert_eq!(
            tag_vehicle_rate_distance
                .attributes()
                .contains(&Necessity::Optional(String::from("Unit"))),
            true,
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
            tag_vehicle_charge.children().len(),
            3,
            "expected Charge to contain 3 children"
        );
        assert_eq!(
            tag_vehicle_charge.has_child(&String::from("FirstOnly")),
            true,
            "expected Charge to contain child FirstOnly"
        );
        assert_eq!(
            tag_vehicle_charge.has_child(&String::from("SecondOnly")),
            true,
            "expected Charge to contain child SecondOnly"
        );
        assert_eq!(
            tag_vehicle_charge.has_child(&String::from("Mandatory")),
            true,
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
            tag_vehicle_charge.children().len(),
            1,
            "expected Charge to contain 1 children"
        );
        assert_eq!(
            tag_vehicle_charge.has_child(&String::from("Calculation")),
            true,
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
            tag_vehicle_charge.children().len(),
            1,
            "expected Charge to contain 1 children"
        );
        assert_eq!(
            tag_vehicle_charge.has_child(&String::from("Calculation")),
            true,
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
                quick_xml::Error::EndEventMismatch {
                    expected: a,
                    found: b,
                },
            )) => {
                assert_eq!(5, position);
                assert_eq!("a".to_string(), a);
                assert_eq!("b".to_string(), b);
            }
            any => panic!(
                "expected a specific ParserError for broken XML instead of {:?}",
                any
            ),
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
}
