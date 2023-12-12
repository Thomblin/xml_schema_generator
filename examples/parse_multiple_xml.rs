use quick_xml::reader::Reader;
use serde::{Deserialize, Serialize};
use xml_schema_generator::{extend_struct, into_struct, Options};

const XML1: &str = "\
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<library name=\"Evergreen Books &amp; Beyond\">
  <book>
    <title>To Kill a Mockingbird</title>
    <author>Harper Lee</author>
    <publication_year>1960</publication_year>
  </book>
</library>
";

const XML2: &str = "\
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<library name=\"Evergreen Books &amp; Beyond\">  
  <book>
    <title>One Hundred Years of Solitude</title>
    <author>Gabriel Garcia Marquez</author>
  </book>
  
  <book>
    <title>1984</title>
    <author>George Orwell</author>
    <genre>Dystopian Fiction</genre>
    <publication_year>1949</publication_year>
  </book>
</library>
";

#[derive(Serialize, Deserialize)]
pub struct Library {
    #[serde(rename = "@name")]
    pub name: String,
    #[serde(rename = "$text")]
    pub text: Option<String>,
    pub book: Vec<Book>,
}

#[derive(Serialize, Deserialize)]
pub struct Book {
    pub title: String,
    pub author: String,
    pub publication_year: Option<String>,
    pub genre: Option<String>,
    #[serde(rename = "$text")]
    pub text: Option<String>,
}

fn main() {
    // create struct from first XML
    let root = match into_struct(&mut Reader::from_str(XML1)) {
        Ok(root) => root,
        Err(_) => panic!("expected to successfully parse into struct"),
    };

    // create struct with structure from second XML
    let root = match extend_struct(&mut Reader::from_str(XML2), root) {
        Ok(root) => root,
        Err(_) => panic!("expected to successfully parse second xml into struct"),
    };

    let struct_as_string = root.to_serde_struct(&Options::quick_xml_de());
    println!("{}", struct_as_string); // this prints the struct Library and Book as listed above

    // parse XML into generated struct
    let library: Library = quick_xml::de::from_str(XML1).unwrap();
    assert_eq!(1, library.book.len());

    let library: Library = quick_xml::de::from_str(XML2).unwrap();
    assert_eq!(2, library.book.len());
}
