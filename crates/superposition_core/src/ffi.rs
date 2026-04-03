use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};

use serde_json::{Map, Value};
use superposition_types::experimental::Experimental;
use superposition_types::{Config, Context, DimensionInfo, Overrides};
use thiserror::Error;

use crate::experiment::{filter_experiments_by_context, ExperimentConfig};
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
        );
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
    );

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

#[uniffi::export]
fn ffi_parse_config_file_with_filters(
    file_content: String,
    format: String,
    dimension_data: Option<HashMap<String, String>>,
    prefix: Option<Vec<String>>,
) -> Result<Config, OperationError> {
    let dimension_data = dimension_data
        .map(json_from_map)
        .transpose()
        .map_err(|err| OperationError::Unexpected(err.to_string()))?;
    let prefix_list = prefix.map(HashSet::from_iter);

    let config = match format.to_lowercase().as_str() {
        "json" => JsonFormat::parse_config(&file_content)
            .map_err(|e| OperationError::Unexpected(e.to_string()))?,
        "toml" => TomlFormat::parse_config(&file_content)
            .map_err(|e| OperationError::Unexpected(e.to_string()))?,
        _ => {
            return Err(OperationError::Unexpected(format!(
                "Unsupported format: {}. Supported formats are 'json' and 'toml'.",
                format
            )));
        }
    };

    Ok(config.filter(dimension_data.as_ref(), prefix_list.as_ref()))
}

#[derive(Default)]
pub struct CacheData {
    pub config: Config,
    pub experiment: Option<ExperimentConfig>,
}

#[derive(uniffi::Object, Default)]
pub struct ProviderCache {
    pub(crate) data: Mutex<CacheData>,
}

impl Drop for ProviderCache {
    fn drop(&mut self) {
        log::trace!("[Rust] ProviderCache dropped — native memory freed");
    }
}

#[uniffi::export]
impl ProviderCache {
    #[uniffi::constructor]
    pub fn new() -> Arc<Self> {
        Arc::new(ProviderCache {
            data: Mutex::new(CacheData {
                config: Config::default(),
                experiment: None,
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

        cache_data.config.default_configs = default_config_map.into();
        cache_data.config.contexts = contexts;
        cache_data.config.overrides = overrides;
        cache_data.config.dimensions = dimensions;

        Ok(())
    }

    pub fn init_experiments(
        &self,
        experiments: Vec<FfiExperiment>,
        experiment_groups: Vec<FfiExperimentGroup>,
    ) -> Result<(), OperationError> {
        let mut cache_data = self.data.lock().map_err(|err| {
            OperationError::Unexpected(format!("Failed to acquire cache lock: {}", err))
        })?;

        cache_data.experiment = Some(ExperimentConfig {
            experiments,
            experiment_groups,
        });

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

        if let Some(experiment_config) = &cache_data.experiment {
            if (!experiment_config.experiments.is_empty()
                || !experiment_config.experiment_groups.is_empty())
                && targeting_key.as_ref().is_some_and(|key| !key.is_empty())
            {
                let variants = get_applicable_variants(
                    &cache_data.config.dimensions,
                    experiment_config.experiments.clone(),
                    &experiment_config.experiment_groups,
                    &_q,
                    targeting_key.as_deref().unwrap_or(""),
                    filter_prefixes.clone(),
                );
                _q.insert("variantIds".to_string(), variants.into());
            }
        }

        let r = eval_config(
            cache_data.config.default_configs.inner().clone(),
            &cache_data.config.contexts,
            &cache_data.config.overrides,
            &cache_data.config.dimensions,
            &_q,
            merge_strategy,
            filter_prefixes,
        )
        .map_err(OperationError::Unexpected)?;

        json_to_map(r).map_err(|err| OperationError::Unexpected(err.to_string()))
    }

    fn filter_config(
        &self,
        dimension_data: Option<HashMap<String, String>>,
        prefix: Option<Vec<String>>,
    ) -> Result<Config, OperationError> {
        let dimension_data = dimension_data
            .map(json_from_map)
            .transpose()
            .map_err(|err| OperationError::Unexpected(err.to_string()))?;
        let prefix_list = prefix.map(HashSet::from_iter);

        let config = {
            let cache_data = self.data.lock().map_err(|err| {
                OperationError::Unexpected(format!(
                    "Failed to acquire cache lock: {}",
                    err
                ))
            })?;
            cache_data.config.clone()
        };

        Ok(config.filter(dimension_data.as_ref(), prefix_list.as_ref()))
    }

    fn filter_experiment(
        &self,
        dimension_data: Option<HashMap<String, String>>,
        prefix: Option<Vec<String>>,
    ) -> Result<ExperimentConfig, OperationError> {
        let dimension_data = dimension_data
            .map(json_from_map)
            .transpose()
            .map_err(|err| OperationError::Unexpected(err.to_string()))?
            .unwrap_or_default();

        let (exps, exp_grps) = {
            let cache_data = self.data.lock().map_err(|err| {
                OperationError::Unexpected(format!(
                    "Failed to acquire cache lock: {}",
                    err
                ))
            })?;

            let exp_config = cache_data.experiment.as_ref().ok_or_else(|| {
                OperationError::Unexpected(
                    "Experiment configuration not initialized".to_string(),
                )
            })?;

            (
                exp_config.experiments.clone(),
                exp_config.experiment_groups.clone(),
            )
        };

        Ok(ExperimentConfig {
            experiments: filter_experiments_by_context(exps, &dimension_data, prefix),
            experiment_groups: FfiExperimentGroup::filter_by_eval(
                exp_grps,
                &dimension_data,
            ),
        })
    }

    fn get_applicable_variants(
        &self,
        dimension_data: Option<HashMap<String, String>>,
        prefix: Option<Vec<String>>,
        targeting_key: String,
    ) -> Result<Vec<String>, OperationError> {
        let dimension_data = dimension_data
            .map(json_from_map)
            .transpose()
            .map_err(|err| OperationError::Unexpected(err.to_string()))?
            .unwrap_or_default();

        let (exps, exp_grps, dimensions_info) = {
            let cache_data = self.data.lock().map_err(|err| {
                OperationError::Unexpected(format!(
                    "Failed to acquire cache lock: {}",
                    err
                ))
            })?;

            let exp_config = cache_data.experiment.as_ref().ok_or_else(|| {
                OperationError::Unexpected(
                    "Experiment configuration not initialized".to_string(),
                )
            })?;

            (
                exp_config.experiments.clone(),
                exp_config.experiment_groups.clone(),
                cache_data.config.dimensions.clone(),
            )
        };

        let variants = get_applicable_variants(
            &dimensions_info,
            exps,
            &exp_grps,
            &dimension_data,
            &targeting_key,
            prefix,
        );

        Ok(variants)
    }
}
