use serde::{Deserialize, Serialize};

const XML: &str = "\
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<MVCI_MODULE_DESCRIPTION FILE_VERSION=\"1.22.2.0\" MVCI_PART2_STANDARD_VERSION=\"2.1.1\">
    <PINTYPE EID=\"ID_HI\">
        <ID>0</ID>
        <SHORT_NAME>HI</SHORT_NAME>
    </PINTYPE>
    <PINTYPE EID=\"ID_LOW\">
        <ID>1</ID>
        <SHORT_NAME>LOW</SHORT_NAME>
    </PINTYPE>
</MVCI_MODULE_DESCRIPTION>
";

#[derive(Serialize, Deserialize)]
pub struct MvciModuleDescription {
    #[serde(rename = "@FILE_VERSION")]
    pub file_version: String,
    #[serde(rename = "@MVCI_PART2_STANDARD_VERSION")]
    pub mvci_part2_standard_version: String,
    #[serde(rename = "$text")]
    pub text: Option<String>,
    #[serde(rename = "PINTYPE")]
    pub pintype: Vec<Pintype>,
}

#[derive(Serialize, Deserialize)]
pub struct Pintype {
    #[serde(rename = "ID")]
    pub id: String,
    #[serde(rename = "SHORT_NAME")]
    pub short_name: String,
    #[serde(rename = "@EID")]
    pub eid: String,
    #[serde(rename = "$text")]
    pub text: Option<String>,
}

#[test]
fn create_struct_that_supports_serde_xml_rs() {
    let root: MvciModuleDescription = serde_xml_rs::from_str(XML).unwrap();

    assert_eq!("1.22.2.0".to_string(), root.file_version);
}

#[test]
fn create_struct_that_supports_quick_xml_de() {
    let root: MvciModuleDescription = quick_xml::de::from_str(XML).unwrap();

    assert_eq!("1.22.2.0".to_string(), root.file_version);
}
