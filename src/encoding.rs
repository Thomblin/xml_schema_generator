//! Encoding detection and conversion utilities for XML files
//!
//! This module provides functionality to detect and convert different text encodings,
//! particularly UTF-8 and UTF-16, for XML processing.

use encoding_rs::{Encoding, UTF_16BE, UTF_16LE, UTF_8};
use std::io::Read;

/// Errors that can occur during encoding detection or conversion
#[derive(Debug, thiserror::Error)]
pub enum EncodingError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Unsupported encoding: {0}")]
    UnsupportedEncoding(String),
    #[error("Invalid UTF-16 sequence")]
    InvalidUtf16,
}

/// Detect the encoding of the given byte data
///
/// This function analyzes the byte order mark (BOM) and content patterns
/// to determine the most likely encoding of the input data.
pub fn detect_encoding(data: &[u8]) -> &'static Encoding {
    // Check for BOM first
    if data.len() >= 2 {
        match data[0..2] {
            [0xFE, 0xFF] => return UTF_16BE,
            [0xFF, 0xFE] => {
                // Could be UTF-16LE or UTF-8 with BOM
                if data.len() >= 4 && data[2] == 0x00 && data[3] == 0x00 {
                    return UTF_16LE; // UTF-32LE would be different, but we're focusing on UTF-16
                }
                return UTF_16LE;
            }
            [0xEF, 0xBB] => {
                if data.len() >= 3 && data[2] == 0xBF {
                    return UTF_8; // UTF-8 with BOM
                }
            }
            _ => {}
        }
    }

    // If no BOM, try to detect based on content patterns
    // For UTF-16 without BOM, we can look for null bytes in ASCII ranges
    if data.len() >= 4 {
        let mut even_nulls = 0;
        let mut odd_nulls = 0;
        let mut ascii_chars = 0;

        for (i, &byte) in data.iter().enumerate() {
            // Count ASCII characters (0x20-0x7E range) to focus on text content
            if (0x20..=0x7E).contains(&byte) {
                ascii_chars += 1;
            }

            // Count null bytes at even and odd positions
            if byte == 0 {
                if i % 2 == 0 {
                    even_nulls += 1;
                } else {
                    odd_nulls += 1;
                }
            }
        }

        // If we have enough ASCII characters and see a clear null byte pattern,
        // it's likely UTF-16
        if ascii_chars > 10 {
            let total_bytes = data.len();
            let null_ratio = (even_nulls + odd_nulls) as f64 / total_bytes as f64;

            // For UTF-16, we expect roughly 25-50% null bytes in the total data
            if null_ratio > 0.2 && null_ratio <= 0.5 {
                match even_nulls.cmp(&odd_nulls) {
                    std::cmp::Ordering::Greater => return UTF_16BE,
                    std::cmp::Ordering::Less => return UTF_16LE,
                    std::cmp::Ordering::Equal => {} // Continue to default
                }
            }
        }
    }

    // Default to UTF-8
    UTF_8
}

/// Convert the input data to a UTF-8 string using encoding detection
pub fn convert_to_utf8(data: &[u8]) -> Result<String, EncodingError> {
    let encoding = detect_encoding(data);
    let (utf8_string, _encoding_used, _had_bom) = encoding.decode(data);
    Ok(utf8_string.to_string())
}

/// Read a file and convert it to UTF-8 string
pub fn read_file_as_utf8<P: AsRef<std::path::Path>>(path: P) -> Result<String, EncodingError> {
    let mut file = std::fs::File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    convert_to_utf8(&buffer)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_utf8_with_bom() {
        let data = &[0xEF, 0xBB, 0xBF, 0x3C, 0x3F, 0x78]; // UTF-8 BOM + "<?x"
        assert_eq!(detect_encoding(data), UTF_8);
    }

    #[test]
    fn test_detect_utf16be_bom() {
        let data = &[0xFE, 0xFF, 0x00, 0x3C]; // UTF-16BE BOM + "<"
        assert_eq!(detect_encoding(data), UTF_16BE);
    }

    #[test]
    fn test_detect_utf16le_bom() {
        let data = &[0xFF, 0xFE, 0x3C, 0x00]; // UTF-16LE BOM + "<"
        assert_eq!(detect_encoding(data), UTF_16LE);
    }

    #[test]
    fn test_convert_utf16le_to_utf8() {
        // "test" in UTF-16LE
        let data = &[0x74, 0x00, 0x65, 0x00, 0x73, 0x00, 0x74, 0x00];
        let (utf8_string, _encoding_used, _had_bom) = UTF_16LE.decode(data);
        assert_eq!(utf8_string, "test");
    }

    #[test]
    fn test_convert_utf16be_to_utf8() {
        // "test" in UTF-16BE
        let data = &[0x00, 0x74, 0x00, 0x65, 0x00, 0x73, 0x00, 0x74];
        let (utf8_string, _encoding_used, _had_bom) = UTF_16BE.decode(data);
        assert_eq!(utf8_string, "test");
    }

    #[test]
    fn test_detect_utf16le_without_bom() {
        // Sample UTF-16LE data without BOM (from our test file)
        let data = &[
            0x3c, 0x00, 0x3f, 0x00, 0x78, 0x00, 0x6d, 0x00, 0x6c, 0x00, 0x20, 0x00, 0x76, 0x00,
            0x65, 0x00, 0x72, 0x00, 0x73, 0x00, 0x69, 0x00, 0x6f, 0x00, 0x6e, 0x00, 0x3d, 0x00,
            0x22, 0x00, 0x31, 0x00, 0x2e, 0x00, 0x30, 0x00, 0x22, 0x00, 0x20, 0x00, 0x65, 0x00,
            0x6e, 0x00, 0x63, 0x00, 0x6f, 0x00, 0x64, 0x00, 0x69, 0x00, 0x6e, 0x00, 0x67, 0x00,
            0x3d, 0x00, 0x22, 0x00, 0x55, 0x00, 0x54, 0x00, 0x46, 0x00, 0x2d, 0x00, 0x31, 0x00,
            0x36, 0x00, 0x22, 0x00, 0x3f, 0x00, 0x3e, 0x00, 0x0a, 0x00, 0x3c, 0x00, 0x6c, 0x00,
            0x69, 0x00, 0x62, 0x00, 0x72, 0x00, 0x61, 0x00, 0x72, 0x00, 0x79, 0x00,
        ];

        let encoding = detect_encoding(data);
        assert_eq!(encoding, UTF_16LE);

        let result = convert_to_utf8(data).unwrap();
        assert!(result.starts_with("<?xml version=\"1.0\""));
    }

    #[test]
    fn test_real_world_multilingual_utf16() {
        // Test with real-world multilingual UTF-16 content from tests folder
        let data = std::fs::read("tests/test_multilingual_utf16le_nobom.xml").unwrap();

        let encoding = detect_encoding(&data);
        assert_eq!(encoding, UTF_16LE);

        let utf8_content = convert_to_utf8(&data).unwrap();

        // Verify that we can see the multilingual content correctly
        assert!(utf8_content.contains("The Great Gatsby"));
        assert!(utf8_content.contains("红楼梦"));
        assert!(utf8_content.contains("ノルウェイの森"));
        assert!(utf8_content.contains("Война и мир"));
        assert!(utf8_content.contains("ألف ليلة وليلة"));
        assert!(utf8_content.contains("해리포터와 마법사의 돌"));

        // Verify XML structure is preserved
        assert!(utf8_content.contains("<?xml version=\"1.0\""));
        assert!(utf8_content.contains("<catalog>"));
        assert!(utf8_content.contains("</catalog>"));
    }
}
