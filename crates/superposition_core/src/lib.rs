uniffi::setup_scaffolding!("superposition_client");

pub mod config;
pub mod experiment;
pub mod ffi;
pub mod ffi_legacy;
pub mod helpers;
pub mod toml_parser;
pub mod validations;

pub use config::{eval_config, eval_config_with_reasoning, merge, MergeStrategy};
pub use experiment::{
    get_applicable_variants, get_satisfied_experiments, Experiments, FfiExperiment,
};
pub use ffi_legacy::{
    core_free_string, core_get_resolved_config, core_get_resolved_config_with_reasoning,
};
pub use superposition_types::Config;
pub use toml_parser::{serialize_to_toml, TomlError};

/// Parse TOML configuration string into structured components
///
/// This function parses a TOML string containing default-config, dimensions, and context sections,
/// and returns the parsed structures that can be used with other superposition_core functions.
///
/// # Arguments
/// * `toml_content` - TOML string containing default-config, dimensions, and context sections
///
/// # Returns
/// * `Ok(Config)` - Successfully parsed configuration with:
///   - `default_config`: Map of configuration keys to values
///   - `contexts`: Vector of context conditions
///   - `overrides`: HashMap of override configurations
///   - `dimensions`: HashMap of dimension information
/// * `Err(TomlError)` - Detailed error about what went wrong
///
/// # Example TOML Format
/// ```toml
/// [default-config]
/// timeout = { value = 30, schema = { type = "integer" } }
/// enabled = { value = true, schema = { type = "boolean" } }
///
/// [dimensions]
/// os = { schema = { type = "string" } }
/// region = { schema = { type = "string" } }
///
/// [context]
/// "os=linux" = { timeout = 60 }
/// "os=linux;region=us-east" = { timeout = 90, enabled = false }
/// ```
///
/// # Example Usage
/// ```rust,no_run
/// use superposition_core::parse_toml_config;
///
/// let toml_content = r#"
///     [default-config]
///     timeout = { value = 30, schema = { type = "integer" } }
///
///     [dimensions]
///     os = { schema = { type = "string" } }
///
///     [context]
///     "os=linux" = { timeout = 60 }
/// "#;
///
/// let parsed = parse_toml_config(toml_content)?;
/// println!("Parsed {} contexts", parsed.contexts.len());
/// # Ok::<(), superposition_core::TomlError>(())
/// ```
pub fn parse_toml_config(toml_content: &str) -> Result<Config, TomlError> {
    toml_parser::parse(toml_content)
}
