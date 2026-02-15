//! Unified configuration format support
//!
//! This module provides parsers and serializers for multiple configuration
//! formats (TOML and JSON) with shared validation logic.

pub mod error;
pub mod json_impl;
pub mod toml_impl;

use superposition_types::DetailedConfig;

pub use error::{conversion_error, serialization_error, syntax_error, FormatError};

/// Trait for configuration format parsers/serializers
pub trait ConfigFormat {
    /// Parse a string into DetailedConfig
    fn parse(input: &str) -> Result<DetailedConfig, FormatError>;

    /// Serialize DetailedConfig to string
    fn serialize(detailed_config: DetailedConfig) -> Result<String, FormatError>;

    /// Get the format name for error messages
    fn format_name() -> &'static str;
}
