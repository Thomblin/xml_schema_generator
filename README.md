# xml_schema_generator

[<img alt="github" src="https://img.shields.io/badge/github-thomblin/xml_schema_generator-8da0cb?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/thomblin/xml_schema_generator)
[<img alt="crates.io" src="https://img.shields.io/crates/v/xml_schema_generator?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/xml_schema_generator)
[<img alt="docs.rs" src="https://img.shields.io/docsrs/xml_schema_generator?logo=docs.rs&labelColor=555555" height="20">](https://docs.rs/xml_schema_generator)
[<img alt="build status" src="https://img.shields.io/github/actions/workflow/status/Thomblin/xml_schema_generator/rust.yml?branch=main&style=for-the-badge" height="20">](https://github.com/thomblin/xml_schema_generator/actions?query=branch%3Amain)


Library and Binary to convert a given XML input using quick_xml to generate a Struct (as String or file) that you can use in your Rust program to serialize or deserialize XML of the same format

You can add this dependency with:

```toml
[dependencies]
xml_schema_generator = "0.4.0"
```

## Example

How to implement the lib
```rust
    use quick_xml::reader::Reader;

    use xml_schema_generator::{Element, into_struct};

    let xml = "<a b=\"c\">d</a>";
    let mut reader = Reader::from_str(xml);
    let mut root = Element::new(String::from("root"), Vec::new());
    
    into_struct(&mut reader, &mut root);

    let rs_struct = root.to_serde_struct();
    // save this result as a .rs file and use it to (de)serialize an XML document with serde
```

How to run the binary
```bash
    # parse input.xml and print struct to stdout
    $ cargo run --features="env_logger" -- input.xml
    
    # parse input.xml and store struct to output.rs
    $ cargo run --features="env_logger" -- input.xml output.rs
```

# Contribution

Just create a well tested Pull Request in github

# Implemented features
 
☑ parse UTF-8 xml file
 
☑ generate Rust struct
 
☑ detect optional attributes
 
☑ detect optional children

☑ add a binary to run this lib independently

☑ replace panic! with Result<>
 
# Ideas (if needed or requested via github)
 
☐ properly parse namespaces and reflect them in the Rust Struct

☐ detect numeric and boolean fields
 
☐ improve the implementation of String, &str and \[u8\]
 
☐ improve performance
  
☐ generate XSD files
 
☐ support UTF-16
 
☐ suppport ISO_2022_JP

☐ parse multiple XML files into one result

# License
Apache-2.0