use serde_json::{Map, Value};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use superposition_types::{Config, Context, DimensionInfo, Overrides};
use thiserror::Error;

use crate::{
    eval_config, eval_config_with_reasoning, experiment::ExperimentationArgs,
    experiment::FfiExperimentGroup, get_applicable_variants, ConfigFormat, FfiExperiment,
    JsonFormat, MergeStrategy, TomlFormat,
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

/// Parse TOML configuration string
///
/// # Arguments
/// * `toml_content` - TOML string with configuration
///
/// # Returns
/// * `Ok(Config)` - Parsed configuration with all components
/// * `Err(OperationError)` - Detailed error message
///
/// # Example TOML
/// ```toml
/// [default-configs]
/// timeout = { value = 30, schema = { type = "integer" } }
///
/// [dimensions]
/// os = { position = 1, schema = { type = "string" } }
///
/// [[overrides]]
/// _context_ = { os = "linux" }
/// timeout = 60
/// ```
#[uniffi::export]
fn ffi_parse_toml_config(toml_content: String) -> Result<Config, OperationError> {
    TomlFormat::parse_config(&toml_content)
        .map_err(|e| OperationError::Unexpected(e.to_string()))
}

/// Parse JSON configuration string
///
/// # Arguments
/// * `json_content` - JSON string with configuration
///
/// # Returns
/// * `Ok(Config)` - Parsed configuration with all components
/// * `Err(OperationError)` - Detailed error message
///
/// # Example JSON
/// ```json
/// {
///   "default-configs": {
///     "timeout": { "value": 30, "schema": { "type": "integer" } }
///   },
///   "dimensions": {
///     "os": { "position": 1, "schema": { "type": "string" } }
///   },
///   "overrides": [
///     {
///       "_context_": { "os": "linux" },
///       "timeout": 60
///     }
///   ]
/// }
/// ```
#[uniffi::export]
fn ffi_parse_json_config(json_content: String) -> Result<Config, OperationError> {
    JsonFormat::parse_config(&json_content)
        .map_err(|e| OperationError::Unexpected(e.to_string()))
}

pub struct ConfigCacheData {
    pub default_config: Map<String, Value>,
    pub contexts: Vec<Context>,
    pub overrides: HashMap<String, Overrides>,
    pub dimensions: HashMap<String, DimensionInfo>,
    pub experiments: Vec<FfiExperiment>,
    pub experiment_groups: Vec<FfiExperimentGroup>,
}

#[derive(uniffi::Object)]
pub struct ProviderCache {
    data: Mutex<ConfigCacheData>,
}

#[uniffi::export]
impl ProviderCache {
    #[uniffi::constructor]
    pub fn new() -> Arc<Self> {
        Arc::new(ProviderCache {
            data: Mutex::new(ConfigCacheData {
                default_config: Map::new(),
                contexts: Vec::new(),
                overrides: HashMap::new(),
                dimensions: HashMap::new(),
                experiments: Vec::new(),
                experiment_groups: Vec::new(),
            }),
        })
    }

    pub fn init_config(
        &self,
        default_config: HashMap<String, String>,
        contexts: Vec<Context>,
        overrides: HashMap<String, Overrides>,
        dimensions: HashMap<String, DimensionInfo>,
    ) -> Result<(), OperationError> {
        let default_config_map = json_from_map(default_config)
            .map_err(|err| OperationError::Unexpected(err.to_string()))?;

        let mut cache_data = self.data.lock().map_err(|err| {
            OperationError::Unexpected(format!("Failed to acquire cache lock: {}", err))
        })?;

        let (experiments, experiment_groups) = (
            cache_data.experiments.clone(),
            cache_data.experiment_groups.clone(),
        );

        cache_data.default_config = default_config_map;
        cache_data.contexts = contexts;
        cache_data.overrides = overrides;
        cache_data.dimensions = dimensions;
        cache_data.experiments = experiments;
        cache_data.experiment_groups = experiment_groups;

        Ok(())
    }

    pub fn init_experiments(
        &self,
        experimentation: ExperimentationArgs,
    ) -> Result<(), OperationError> {
        let mut cache_data = self.data.lock().map_err(|err| {
            OperationError::Unexpected(format!("Failed to acquire cache lock: {}", err))
        })?;

        cache_data.experiments = experimentation.experiments;
        cache_data.experiment_groups = experimentation.experiment_groups;

        Ok(())
    }

    pub fn eval_config(
        &self,
        query_data: HashMap<String, String>,
        merge_strategy: MergeStrategy,
        filter_prefixes: Option<Vec<String>>,
        targeting_key: Option<String>,
    ) -> Result<HashMap<String, String>, OperationError> {
        let cache_data = self.data.lock().map_err(|err| {
            OperationError::Unexpected(format!("Failed to acquire cache lock: {}", err))
        })?;

        let mut _q = json_from_map(query_data)
            .map_err(|err| OperationError::Unexpected(err.to_string()))?;

        if (!cache_data.experiments.is_empty()
            || !cache_data.experiment_groups.is_empty())
            && targeting_key.is_some()
        {
            let variants = get_applicable_variants(
                &cache_data.dimensions,
                cache_data.experiments.clone(),
                &cache_data.experiment_groups,
                &_q,
                targeting_key.as_deref().unwrap_or(""),
                filter_prefixes.clone(),
            )
            .map_err(OperationError::Unexpected)?;
            _q.insert("variantIds".to_string(), variants.into());
        }

        let r = eval_config(
            cache_data.default_config.clone(),
            &cache_data.contexts,
            &cache_data.overrides,
            &cache_data.dimensions,
            &_q,
            merge_strategy,
            filter_prefixes,
        )
        .map_err(OperationError::Unexpected)?;

        json_to_map(r).map_err(|err| OperationError::Unexpected(err.to_string()))
    }
}
