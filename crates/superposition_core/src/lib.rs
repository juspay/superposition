uniffi::setup_scaffolding!("superposition_client");

pub mod config;
pub mod experiment;
pub mod ffi;
pub mod ffi_legacy;
pub mod toml_parser;

pub use config::{eval_config, eval_config_with_reasoning, merge, MergeStrategy};
pub use experiment::{
    get_applicable_variants, get_satisfied_experiments, Experiments, FfiExperiment,
};
pub use ffi_legacy::{
    core_free_string, core_get_resolved_config, core_get_resolved_config_with_reasoning,
};
pub use toml_parser::{ParsedTomlConfig, TomlParseError};

use serde_json::{Map, Value};

/// Parse TOML configuration string into structured components
///
/// This function parses a TOML string containing default-config, dimensions, and context sections,
/// and returns the parsed structures that can be used with other superposition_core functions.
///
/// # Arguments
/// * `toml_content` - TOML string containing default-config, dimensions, and context sections
///
/// # Returns
/// * `Ok(ParsedTomlConfig)` - Successfully parsed configuration with:
///   - `default_config`: Map of configuration keys to values
///   - `contexts`: Vector of context conditions
///   - `overrides`: HashMap of override configurations
///   - `dimensions`: HashMap of dimension information
/// * `Err(TomlParseError)` - Detailed error about what went wrong
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
/// # Ok::<(), superposition_core::TomlParseError>(())
/// ```
pub fn parse_toml_config(toml_content: &str) -> Result<ParsedTomlConfig, TomlParseError> {
    toml_parser::parse(toml_content)
}

/// Parse TOML configuration and evaluate with input dimensions
///
/// This is a convenience function that combines TOML parsing and configuration evaluation
/// in a single call. It parses the TOML content and immediately evaluates it against the
/// provided input dimensions using the specified merge strategy.
///
/// # Arguments
/// * `toml_content` - TOML string with configuration
/// * `input_dimensions` - Map of dimension values for this evaluation (e.g., {"os": "linux", "region": "us-east"})
/// * `merge_strategy` - How to merge override values with defaults (MERGE or REPLACE)
///
/// # Returns
/// * `Ok(Map<String, Value>)` - Resolved configuration after applying context overrides
/// * `Err(String)` - Error message describing what went wrong
///
/// # Example Usage
/// ```rust,no_run
/// use superposition_core::{eval_toml_config, MergeStrategy};
/// use serde_json::{Map, Value};
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
/// let mut input_dims = Map::new();
/// input_dims.insert("os".to_string(), Value::String("linux".to_string()));
///
/// let config = eval_toml_config(toml_content, &input_dims, MergeStrategy::MERGE)?;
/// println!("Resolved timeout: {}", config["timeout"]);
/// # Ok::<(), String>(())
/// ```
pub fn eval_toml_config(
    toml_content: &str,
    input_dimensions: &Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> Result<Map<String, Value>, String> {
    let parsed = toml_parser::parse(toml_content).map_err(|e| e.to_string())?;

    eval_config(
        parsed.default_config,
        &parsed.contexts,
        &parsed.overrides,
        &parsed.dimensions,
        input_dimensions,
        merge_strategy,
        None, // filter_prefixes
    )
}

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
