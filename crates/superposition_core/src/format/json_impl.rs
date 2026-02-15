//! JSON format implementation placeholder
//!
//! This module will be populated with JSON-specific logic in a future task.

use crate::format::{ConfigFormat, FormatError};
use superposition_types::DetailedConfig;

/// JSON format implementation
pub struct JsonFormat;

impl ConfigFormat for JsonFormat {
    fn parse(_input: &str) -> Result<DetailedConfig, FormatError> {
        unimplemented!("JSON parsing will be implemented in a future task")
    }

    fn serialize(_detailed_config: DetailedConfig) -> Result<String, FormatError> {
        unimplemented!("JSON serialization will be implemented in a future task")
    }

    fn format_name() -> &'static str {
        "JSON"
    }
}
