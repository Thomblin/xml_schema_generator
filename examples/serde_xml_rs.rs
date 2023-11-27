use quick_xml::reader::Reader;
use serde::{Deserialize, Serialize};
use xml_schema_generator::{into_struct, Options};

const XML: &str = "\
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<library name=\"Evergreen Books &amp; Beyond\">
  <book>
    <title>To Kill a Mockingbird</title>
    <author>Harper Lee</author>
    <genre>Drama</genre>
    <publication_year>1960</publication_year>
  </book>
  
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
    // create struct from XML

    let mut reader = Reader::from_str(XML);

    if let Ok(root) = into_struct(&mut reader) {
        let struct_as_string = root.to_serde_struct(&Options::serde_xml_rs());
        println!("{}", struct_as_string); // this prints the struct Library and Book as listed above
    }

    // parse XML into generated struct
    let library: Library = serde_xml_rs::from_str(XML).unwrap();
    assert_eq!(3, library.book.len());
}
