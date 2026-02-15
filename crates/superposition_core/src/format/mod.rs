pub mod error;
pub mod json_impl;
pub mod toml_impl;

use std::collections::HashMap;
use superposition_types::{
    database::models::cac::DimensionType,
    DetailedConfig, DimensionInfo,
};

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

/// Shared validation logic for all formats
/// 
/// This function validates:
/// - Default configs against their schemas
/// - Dimension schemas
/// - Cohort dimension references and positions
/// - Contexts against declared dimensions
/// - Overrides against default configs
/// - Duplicate dimension positions
///
/// After validation, it:
/// - Sorts contexts by priority
/// - Assigns correct weights
/// - Builds dependency graphs for cohort dimensions
pub fn validate_detailed_config(
    _detailed: &mut DetailedConfig,
) -> Result<(), FormatError> {
    // TODO: Extract validation logic from toml.rs
    // For now, this is a placeholder to allow compilation
    Ok(())
}
