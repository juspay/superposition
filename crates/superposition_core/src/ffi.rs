use serde_json::{Map, Value};
use std::collections::HashMap;
use superposition_types::{Context, DimensionInfo, Overrides};
use thiserror::Error;

use crate::{
    eval_config, eval_config_with_reasoning, experiment::ExperimentationArgs,
    get_applicable_variants, MergeStrategy,
};

#[derive(Debug, Error, uniffi::Error)]
pub enum OperationError {
    #[error("An unexpected error occurred: {0}")]
    Unexpected(String),
}

fn json_to_map(j: Map<String, Value>) -> serde_json::Result<HashMap<String, String>> {
    j.iter()
        .map(|(k, v)| serde_json::to_string(v).map(|v| (k.clone(), v)))
        .collect::<serde_json::Result<HashMap<String, String>>>()
}

fn json_from_map(m: HashMap<String, String>) -> serde_json::Result<Map<String, Value>> {
    m.iter()
        .map(|(k, v)| serde_json::from_str(v).map(|v| (k.clone(), v)))
        .collect::<serde_json::Result<Map<String, Value>>>()
}

type EvalFn = fn(
    Map<String, Value>,
    &[Context],
    &HashMap<String, Overrides>,
    &HashMap<String, DimensionInfo>,
    &Map<String, Value>,
    MergeStrategy,
    Option<Vec<String>>,
) -> Result<Map<String, Value>, String>;

#[allow(clippy::too_many_arguments)]
fn ffi_eval_logic(
    default_config: HashMap<String, String>,
    contexts: &[Context],
    overrides: HashMap<String, Overrides>,
    dimensions: HashMap<String, DimensionInfo>,
    query_data: HashMap<String, String>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    experimentation: Option<ExperimentationArgs>,
    eval_fn: EvalFn,
) -> Result<HashMap<String, String>, OperationError> {
    let _d = json_from_map(default_config)
        .map_err(|err| OperationError::Unexpected(err.to_string()))?;
    let mut _q = json_from_map(query_data)
        .map_err(|err| OperationError::Unexpected(err.to_string()))?;

    if let Some(e_args) = experimentation {
        // NOTE Parsing to allow for testing. This has to be migrated to the new
        // bucketing procedure.
        let identifier = e_args.targeting_key;
        let variants = get_applicable_variants(
            &dimensions,
            e_args.experiments,
            &e_args.experiment_groups,
            &_q,
            &identifier,
            filter_prefixes.clone(),
        )
        .map_err(OperationError::Unexpected)?;
        _q.insert("variantIds".to_string(), variants.into());
    }

    let r = eval_fn(
        _d,
        contexts,
        &overrides,
        &dimensions,
        &_q,
        merge_strategy,
        filter_prefixes,
    )
    .map_err(OperationError::Unexpected)?;

    json_to_map(r).map_err(|err| OperationError::Unexpected(err.to_string()))
}

#[allow(clippy::too_many_arguments)]
#[uniffi::export]
fn ffi_eval_config(
    default_config: HashMap<String, String>,
    contexts: &[Context],
    overrides: HashMap<String, Overrides>,
    dimensions: HashMap<String, DimensionInfo>,
    query_data: HashMap<String, String>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    experimentation: Option<ExperimentationArgs>,
) -> Result<HashMap<String, String>, OperationError> {
    ffi_eval_logic(
        default_config,
        contexts,
        overrides,
        dimensions,
        query_data,
        merge_strategy,
        filter_prefixes,
        experimentation,
        eval_config,
    )
}

#[allow(clippy::too_many_arguments)]
#[uniffi::export]
fn ffi_eval_config_with_reasoning(
    default_config: HashMap<String, String>,
    contexts: &[Context],
    overrides: HashMap<String, Overrides>,
    dimensions: HashMap<String, DimensionInfo>,
    query_data: HashMap<String, String>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    experimentation: Option<ExperimentationArgs>,
) -> Result<HashMap<String, String>, OperationError> {
    ffi_eval_logic(
        default_config,
        contexts,
        overrides,
        dimensions,
        query_data,
        merge_strategy,
        filter_prefixes,
        experimentation,
        eval_config_with_reasoning,
    )
}

#[uniffi::export]
fn ffi_get_applicable_variants(
    eargs: ExperimentationArgs,
    dimensions_info: HashMap<String, DimensionInfo>,
    query_data: HashMap<String, String>,
    prefix: Option<Vec<String>>,
) -> Result<Vec<String>, OperationError> {
    let _query_data = json_from_map(query_data.clone())
        .map_err(|err| OperationError::Unexpected(err.to_string()))?;

    let identifier = eargs.targeting_key;
    let r = get_applicable_variants(
        &dimensions_info,
        eargs.experiments,
        &eargs.experiment_groups,
        &_query_data,
        &identifier,
        prefix,
    )
    .map_err(OperationError::Unexpected)?;

    Ok(r)
}

/// Parsed TOML configuration result for FFI
///
/// Note: Complex structures are JSON-encoded as strings for uniffi compatibility
#[derive(uniffi::Record)]
pub struct ParsedTomlResult {
    /// Default configuration as a map of key -> JSON-encoded value
    pub default_config: HashMap<String, String>,
    /// Contexts array as JSON string
    pub contexts_json: String,
    /// Overrides map as JSON string
    pub overrides_json: String,
    /// Dimensions map as JSON string
    pub dimensions_json: String,
}

/// Parse TOML configuration string
///
/// # Arguments
/// * `toml_content` - TOML string with configuration
///
/// # Returns
/// * `Ok(ParsedTomlResult)` - Parsed configuration components
/// * `Err(OperationError)` - Detailed error message
///
/// # Example TOML
/// ```toml
/// [default-config]
/// timeout = { value = 30, schema = { type = "integer" } }
///
/// [dimensions]
/// os = { schema = { type = "string" } }
///
/// [context]
/// "os=linux" = { timeout = 60 }
/// ```
#[uniffi::export]
fn ffi_parse_toml_config(toml_content: String) -> Result<ParsedTomlResult, OperationError> {
    // Parse TOML
    let parsed = crate::parse_toml_config(&toml_content).map_err(|e| {
        OperationError::Unexpected(e.to_string())
    })?;

    // Convert default_config to HashMap<String, String> (JSON-encoded values)
    let default_config: HashMap<String, String> = parsed
        .default_config
        .into_iter()
        .map(|(k, v)| {
            let json_str = serde_json::to_string(&v).unwrap_or_else(|_| "null".to_string());
            (k, json_str)
        })
        .collect();

    // Serialize complex structures to JSON
    let contexts_json = serde_json::to_string(&parsed.contexts).map_err(|e| {
        OperationError::Unexpected(format!("Failed to serialize contexts: {}", e))
    })?;

    let overrides_json = serde_json::to_string(&parsed.overrides).map_err(|e| {
        OperationError::Unexpected(format!("Failed to serialize overrides: {}", e))
    })?;

    let dimensions_json = serde_json::to_string(&parsed.dimensions).map_err(|e| {
        OperationError::Unexpected(format!("Failed to serialize dimensions: {}", e))
    })?;

    Ok(ParsedTomlResult {
        default_config,
        contexts_json,
        overrides_json,
        dimensions_json,
    })
}

/// Parse TOML and evaluate configuration with input dimensions
///
/// # Arguments
/// * `toml_content` - TOML string with configuration
/// * `input_dimensions` - Map of dimension values (values are JSON-encoded strings)
/// * `merge_strategy` - "MERGE" or "REPLACE"
///
/// # Returns
/// * `Ok(HashMap)` - Resolved configuration (values are JSON-encoded strings)
/// * `Err(OperationError)` - Error message
#[uniffi::export]
fn ffi_eval_toml_config(
    toml_content: String,
    input_dimensions: HashMap<String, String>,
    merge_strategy: String,
) -> Result<HashMap<String, String>, OperationError> {
    // Convert input_dimensions from HashMap<String, String> to Map<String, Value>
    let dimensions_map: Map<String, Value> = input_dimensions
        .into_iter()
        .map(|(k, v)| {
            // Try to parse as JSON, fall back to string
            let value = serde_json::from_str(&v).unwrap_or_else(|_| Value::String(v));
            (k, value)
        })
        .collect();

    // Parse merge strategy
    let strategy: MergeStrategy = merge_strategy.parse().map_err(|e| {
        OperationError::Unexpected(format!("Invalid merge strategy: {}", e))
    })?;

    // Evaluate
    let result = crate::eval_toml_config(&toml_content, &dimensions_map, strategy)
        .map_err(|e| OperationError::Unexpected(e))?;

    // Convert result to HashMap<String, String>
    let result_map: HashMap<String, String> = result
        .into_iter()
        .map(|(k, v)| {
            let json_str = serde_json::to_string(&v).unwrap_or_else(|_| "null".to_string());
            (k, json_str)
        })
        .collect();

    Ok(result_map)
}
