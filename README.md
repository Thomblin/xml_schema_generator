# xml_schema_generator

[<img alt="github" src="https://img.shields.io/badge/github-thomblin/xml_schema_generator-8da0cb?style=for-the-badge&labelColor=555555&logo=github" height="20">](https://github.com/thomblin/xml_schema_generator)
[<img alt="crates.io" src="https://img.shields.io/crates/v/xml_schema_generator?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/xml_schema_generator)
[<img alt="docs.rs" src="https://img.shields.io/docsrs/xml_schema_generator?logo=docs.rs&labelColor=555555" height="20">](https://docs.rs/xml_schema_generator)
[<img alt="build status" src="https://img.shields.io/github/actions/workflow/status/Thomblin/xml_schema_generator/general.yml?branch=main&style=for-the-badge" height="20">](https://github.com/thomblin/xml_schema_generator/actions?query=branch%3Amain)
[<img alt="audit status" src="https://img.shields.io/github/actions/workflow/status/Thomblin/xml_schema_generator/audit.yml?branch=main&style=for-the-badge&label=audit" height="20">](https://github.com/thomblin/xml_schema_generator/actions?query=branch%3Amain)


Library and Binary to convert a given XML input using quick_xml to generate a Struct (as String or file) that you can use in your Rust program to serialize or deserialize XML of the same format

You can add this dependency with:

```toml
[dependencies]
xml_schema_generator = "0.6.8"
```

## Example

How to implement the lib
```rust
    use quick_xml::reader::Reader;
    use xml_schema_generator::{into_struct, Options};

    let xml = "<a b=\"c\">d</a>";
    let mut reader = Reader::from_str(xml);
    
    if let Ok(root) = into_struct(&mut reader) {
        let struct_as_string = root.to_serde_struct(&Options::quick_xml_de());
        // save this result as a .rs file and use it to (de)serialize an XML document with quick_xml::de::from_str(xml)
    }
        
    // you can even parse additional compatible xml files to extend the structure to match those files as well
    // see examples/parse_multiple_xml_rs
```

You find more examples in the [/examples directory](https://github.com/Thomblin/xml_schema_generator/tree/main/examples)

# Install

from source (env_logger is optional if you do not require additional output)

```bash
    cargo install xml_schema_generator --features="env_logger"
```

or download the latest binary at [GitHub](https://github.com/Thomblin/xml_schema_generator/releases)

How to run the binary
```bash
    Usage: [RUST_LOG=info] xml_schema_generator [OPTIONS] <INPUT_PATH> [OUTPUT_PATH]

    Arguments:
    <INPUT_PATH>
            xml file that shall be parsed

    [OUTPUT_PATH]
            rust file to store the result, or none to print to stdout

    Options:
    -p, --parser <PARSER>
            define the parser that is used to parse the resulting struct
            
            [default: quick-xml-de]
            [possible values: quick-xml-de, serde-xml-rs]

    -d, --derive <DERIVE>
            define the #derive attribute to be added to each resulting struct
            
            [default: "Serialize, Deserialize"]

    -s, --sort <SORT>
            sorting order for attributes and children
            
            [default: unsorted]

            Possible values:
            - unsorted: the order remains as found in document
            - name:     sort attributes and children by name (as given in XML). attributes and children are not merged

    -h, --help
            Print help (see a summary with '-h')

    -V, --version
            Print version
```
# Web Assembly

You can take a look at the result of xml_schema_generator at [xml_schema_generator Github Pages](https://thomblin.github.io/xml_schema_generator/)

For contribution details take a look into the `wasm/README.md`

# Contribution

Just create a well tested Pull Request in github

# Implemented features
 
☑ parse UTF-8 xml file
 
☑ generate Rust struct
 
☑ detect optional attributes
 
☑ detect optional children

☑ add a binary to run this lib independently

☑ replace panic! with Result<>

☑ added serde_xml_rs support

☑ added Option to change the derive attribute

☑ add Options (serde_xml_rs and derive attribute) to binary
 
☑ parse multiple XML files into one result

☑ attributes and children can be sorted for improved readability

☑ web assembly integration

# Ideas (if needed or requested via github)
 
☐ properly parse namespaces and reflect them in the Rust Struct

☐ detect numeric and boolean fields

☐ support enum
 
☐ improve the implementation of String, &str and \[u8\]
 
☐ improve performance
   
☐ support UTF-16
 
☐ suppport ISO_2022_JP

# License
Apache-2.0