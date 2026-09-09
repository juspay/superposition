use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use serde_json::{Map, Value};
use superposition_types::experimental::Experimental;
use superposition_types::{
    Config, ConfigFilter, Context, DimensionInfo, ExtendedMap, Overrides, PrefixList,
};
use thiserror::Error;

use crate::eval_cache::{pairs_from_string_map, EvaluationCache};
use crate::experiment::{
    filter_experiments_by_context, get_satisfied_experiments, ExperimentConfig,
};
use crate::{
    eval, eval_config, experiment::ExperimentationArgs, experiment::FfiExperimentGroup,
    get_applicable_variants, ConfigFormat, FfiExperiment, JsonFormat, MergeStrategy,
    TomlFormat,
};

#[derive(Debug, Error, uniffi::Error)]
pub enum OperationError {
    #[error("An unexpected error occurred: {0}")]
    Unexpected(String),
}

fn json_to_map(j: Map<String, Value>) -> Result<HashMap<String, String>, OperationError> {
    j.iter()
        .map(|(k, v)| serde_json::to_string(v).map(|v| (k.clone(), v)))
        .collect::<serde_json::Result<HashMap<String, String>>>()
        .map_err(|err| OperationError::Unexpected(err.to_string()))
}

fn json_from_map(
    m: HashMap<String, String>,
) -> Result<Map<String, Value>, OperationError> {
    m.iter()
        .map(|(k, v)| serde_json::from_str(v).map(|v| (k.clone(), v)))
        .collect::<serde_json::Result<Map<String, Value>>>()
        .map_err(|err| OperationError::Unexpected(err.to_string()))
}

/// Parses the FFI query data and, when experimentation is requested, resolves the
/// applicable variants into it under `variantIds`.
///
/// The prefix lists are borrowed because the caller still needs them for the eval
/// itself; they are only cloned on the experimentation path.
fn prepare_query_data(
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: HashMap<String, String>,
    experimentation: Option<ExperimentationArgs>,
    filter_prefixes: Option<&Vec<String>>,
    filter_exclude_prefixes: Option<&Vec<String>>,
) -> Result<Map<String, Value>, OperationError> {
    let mut query_data = json_from_map(query_data)?;

    if let Some(e_args) = experimentation {
        // NOTE Parsing to allow for testing. This has to be migrated to the new
        // bucketing procedure.
        let identifier = e_args.targeting_key;
        let variants = get_applicable_variants(
            dimensions,
            e_args.experiments,
            &e_args.experiment_groups,
            query_data.clone(),
            &identifier,
            filter_prefixes.cloned(),
            filter_exclude_prefixes.cloned(),
        );
        query_data.insert("variantIds".to_string(), variants.into());
    }

    Ok(query_data)
}

#[allow(clippy::too_many_arguments)]
#[uniffi::export]
fn ffi_eval_config(
    default_config: ExtendedMap,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: HashMap<String, String>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    filter_exclude_prefixes: Option<Vec<String>>,
    experimentation: Option<ExperimentationArgs>,
) -> Result<HashMap<String, String>, OperationError> {
    let query_data = prepare_query_data(
        dimensions,
        query_data,
        experimentation,
        filter_prefixes.as_ref(),
        filter_exclude_prefixes.as_ref(),
    )?;

    json_to_map(eval(
        default_config,
        contexts,
        overrides,
        dimensions,
        query_data,
        merge_strategy,
        filter_prefixes,
        filter_exclude_prefixes,
    ))
}

#[uniffi::export]
fn ffi_eval(
    config: Config,
    query_data: HashMap<String, String>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    filter_exclude_prefixes: Option<Vec<String>>,
    experimentation: Option<ExperimentationArgs>,
) -> Result<HashMap<String, String>, OperationError> {
    let query_data = prepare_query_data(
        &config.dimensions,
        query_data,
        experimentation,
        filter_prefixes.as_ref(),
        filter_exclude_prefixes.as_ref(),
    )?;

    json_to_map(eval_config(
        config,
        query_data,
        merge_strategy,
        filter_prefixes,
        filter_exclude_prefixes,
    ))
}

#[uniffi::export]
fn ffi_get_applicable_variants(
    eargs: ExperimentationArgs,
    dimensions_info: HashMap<String, DimensionInfo>,
    query_data: HashMap<String, String>,
    prefix: Option<Vec<String>>,
    exclude_prefix: Option<Vec<String>>,
) -> Result<Vec<String>, OperationError> {
    let _query_data = json_from_map(query_data)?;

    let identifier = eargs.targeting_key;
    let r = get_applicable_variants(
        &dimensions_info,
        eargs.experiments,
        &eargs.experiment_groups,
        _query_data,
        &identifier,
        prefix,
        exclude_prefix,
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
    exclude_prefix: Option<Vec<String>>,
) -> Result<Config, OperationError> {
    let dimension_data = dimension_data.map(json_from_map).transpose()?;
    let prefix_list = prefix.map(PrefixList::from_iter);
    let exclude_prefix_list = exclude_prefix.map(PrefixList::from_iter);

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

    Ok(config.filter(
        dimension_data,
        prefix_list.as_ref(),
        exclude_prefix_list.as_ref(),
    ))
}

#[derive(Default)]
pub struct CacheData {
    pub config: Config,
    pub experiment: Option<ExperimentConfig>,
    /// LRU cache of `eval_config` results; disabled by default (`0` MB budget).
    pub(crate) evaluation_cache: EvaluationCache,
}

#[derive(uniffi::Object, Default)]
pub struct ProviderCache {
    pub(crate) data: Mutex<CacheData>,
}

impl ProviderCache {
    pub(crate) fn with_evaluation_cache(max_size_mb: u64) -> Self {
        ProviderCache {
            data: Mutex::new(CacheData {
                evaluation_cache: EvaluationCache::new(max_size_mb),
                ..CacheData::default()
            }),
        }
    }
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
                evaluation_cache: EvaluationCache::default(),
            }),
        })
    }

    /// Creates a provider cache that memoizes repeated `eval_config` queries in
    /// an in-process LRU cache.
    ///
    /// * `max_size_mb` — approximate memory budget for cached evaluations, in
    ///   megabytes. Non-positive values disable caching. The cache is emptied
    ///   whenever new config or experiment data is loaded via `init_config` /
    ///   `init_experiments`.
    ///
    /// Signed `i64` rather than `u64` so the generated Kotlin binding stays
    /// callable from Java (unsigned types are mangled inline classes on the
    /// JVM).
    #[uniffi::constructor]
    pub fn new_with_evaluation_cache(max_size_mb: i64) -> Arc<Self> {
        let max_size_mb = u64::try_from(max_size_mb).unwrap_or(0);
        Arc::new(Self::with_evaluation_cache(max_size_mb))
    }

    pub fn init_config(
        &self,
        default_config: HashMap<String, String>,
        contexts: Vec<Context>,
        overrides: HashMap<String, Overrides>,
        dimensions: HashMap<String, DimensionInfo>,
    ) -> Result<(), OperationError> {
        let default_config_map = json_from_map(default_config)?;

        let mut cache_data = self.data.lock().map_err(|err| {
            OperationError::Unexpected(format!("Failed to acquire cache lock: {}", err))
        })?;

        cache_data.evaluation_cache.clear();
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

        cache_data.evaluation_cache.clear();
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
        filter_exclude_prefixes: Option<Vec<String>>,
        targeting_key: Option<String>,
    ) -> Result<HashMap<String, String>, OperationError> {
        let mut cache_data = self.data.lock().map_err(|err| {
            OperationError::Unexpected(format!("Failed to acquire cache lock: {}", err))
        })?;

        // Version (0, 0): this cache is explicitly cleared by init_config /
        // init_experiments instead of being keyed by data generations.
        let cache_key = cache_data.evaluation_cache.is_enabled().then(|| {
            EvaluationCache::key(
                (0, 0),
                &pairs_from_string_map(&query_data),
                merge_strategy,
                filter_prefixes.as_deref(),
                filter_exclude_prefixes.as_deref(),
                targeting_key.as_deref(),
            )
        });

        if let Some(key) = &cache_key {
            if let Some(cached) = cache_data.evaluation_cache.get(key) {
                return json_to_map(cached);
            }
        }

        let mut _q: Map<String, Value> = json_from_map(query_data)?;

        if let Some(experiment_config) = &cache_data.experiment {
            if (!experiment_config.experiments.is_empty()
                || !experiment_config.experiment_groups.is_empty())
                && targeting_key.as_ref().is_some_and(|key| !key.is_empty())
            {
                let variants = get_applicable_variants(
                    &cache_data.config.dimensions,
                    experiment_config.experiments.clone(),
                    &experiment_config.experiment_groups,
                    _q.clone(),
                    targeting_key.as_deref().unwrap_or(""),
                    filter_prefixes.clone(),
                    filter_exclude_prefixes.clone(),
                );
                _q.insert("variantIds".to_string(), variants.into());
            }
        }

        let r = eval(
            cache_data.config.default_configs.clone(),
            &cache_data.config.contexts,
            &cache_data.config.overrides,
            &cache_data.config.dimensions,
            _q,
            merge_strategy,
            filter_prefixes,
            filter_exclude_prefixes,
        );

        if let Some(key) = cache_key {
            cache_data.evaluation_cache.insert(key, r.clone());
        }

        json_to_map(r)
    }

    fn filter_config(
        &self,
        dimension_data: Option<HashMap<String, String>>,
        prefix: Option<Vec<String>>,
        exclude_prefix: Option<Vec<String>>,
    ) -> Result<Config, OperationError> {
        let dimension_data = dimension_data.map(json_from_map).transpose()?;
        let prefix_list = prefix.map(PrefixList::from_iter);
        let exclude_prefix_list = exclude_prefix.map(PrefixList::from_iter);

        let config = {
            let cache_data = self.data.lock().map_err(|err| {
                OperationError::Unexpected(format!(
                    "Failed to acquire cache lock: {}",
                    err
                ))
            })?;
            cache_data.config.clone()
        };

        Ok(config.filter(
            dimension_data,
            prefix_list.as_ref(),
            exclude_prefix_list.as_ref(),
        ))
    }

    fn filter_experiment(
        &self,
        dimension_data: Option<HashMap<String, String>>,
        prefix: Option<Vec<String>>,
        exclude_prefix: Option<Vec<String>>,
        partial_apply: bool,
    ) -> Result<ExperimentConfig, OperationError> {
        let dimension_data = dimension_data
            .map(json_from_map)
            .transpose()?
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

        let exp_filter_fn = if partial_apply {
            filter_experiments_by_context
        } else {
            get_satisfied_experiments
        };

        let exp_grp_filter_fn = if partial_apply {
            FfiExperimentGroup::filter_by_eval
        } else {
            FfiExperimentGroup::get_satisfied
        };

        Ok(ExperimentConfig {
            experiments: exp_filter_fn(exps, &dimension_data, prefix, exclude_prefix),
            experiment_groups: exp_grp_filter_fn(exp_grps, &dimension_data),
        })
    }

    fn get_applicable_variants(
        &self,
        dimension_data: Option<HashMap<String, String>>,
        prefix: Option<Vec<String>>,
        exclude_prefix: Option<Vec<String>>,
        targeting_key: String,
    ) -> Result<Vec<String>, OperationError> {
        let dimension_data = dimension_data
            .map(json_from_map)
            .transpose()?
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
            dimension_data,
            &targeting_key,
            prefix,
            exclude_prefix,
        );

        Ok(variants)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn cache_with_default(cache: &ProviderCache, timeout: &str) {
        cache
            .init_config(
                HashMap::from([("timeout".to_string(), timeout.to_string())]),
                vec![],
                HashMap::new(),
                HashMap::new(),
            )
            .expect("init_config");
    }

    fn eval_once(cache: &ProviderCache) -> HashMap<String, String> {
        cache
            .eval_config(HashMap::new(), MergeStrategy::MERGE, None, None, None)
            .expect("eval_config")
    }

    #[test]
    fn repeated_queries_hit_the_evaluation_cache() {
        let cache = ProviderCache::new_with_evaluation_cache(1);
        cache_with_default(&cache, "30");

        let first = eval_once(&cache);
        assert_eq!(
            cache
                .data
                .lock()
                .unwrap()
                .evaluation_cache
                .cached_entry_count(),
            1
        );

        let second = eval_once(&cache);
        assert_eq!(first, second);
        assert_eq!(
            cache
                .data
                .lock()
                .unwrap()
                .evaluation_cache
                .cached_entry_count(),
            1
        );
    }

    #[test]
    fn reinitializing_config_clears_stale_results() {
        let cache = ProviderCache::new_with_evaluation_cache(1);
        cache_with_default(&cache, "30");
        let stale = eval_once(&cache);

        cache_with_default(&cache, "60");
        assert_eq!(
            cache
                .data
                .lock()
                .unwrap()
                .evaluation_cache
                .cached_entry_count(),
            0
        );

        let fresh = eval_once(&cache);
        assert_ne!(stale, fresh);
        assert_eq!(fresh.get("timeout").map(String::as_str), Some("60"));
    }

    #[test]
    fn default_constructor_keeps_caching_disabled() {
        let cache = ProviderCache::new();
        cache_with_default(&cache, "30");
        eval_once(&cache);
        assert_eq!(
            cache
                .data
                .lock()
                .unwrap()
                .evaluation_cache
                .cached_entry_count(),
            0
        );
    }
}
