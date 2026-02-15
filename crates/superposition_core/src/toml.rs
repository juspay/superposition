pub mod helpers;
#[cfg(test)]
pub mod test;

// Re-export from format module for backward compatibility
pub use crate::format::toml_impl::{
    parse_toml_config, serialize_to_toml, DetailedConfigToml, DimensionInfoToml,
};

// Re-export error type with backward-compatible name
pub use crate::format::FormatError as TomlError;
