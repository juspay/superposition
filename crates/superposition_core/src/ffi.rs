use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use serde_json::{Map, Value};
use superposition_types::experimental::Experimental;
use superposition_types::{
    Config, ConfigFilter, Context, DimensionInfo, ExtendedMap, Overrides, PrefixList,
};
use thiserror::Error;

use crate::experiment::{
    filter_experiments_by_context, get_satisfied_experiments, ExperimentConfig,
};
use crate::{
    eval, eval_config, experiment::ExperimentationArgs, experiment::FfiExperimentGroup,
    get_applicable_variants, FfiExperiment, MergeStrategy,
};

#[derive(Debug, Error, uniffi::Error)]
pub enum OperationError {
    #[error("An unexpected error occurred: {0}")]
    Unexpected(String),
}

fn json_to_map(j: Map<String, Value>) -> Result<HashMap<String, String>, OperationError> {
    j.into_iter()
        .map(|(k, v)| serde_json::to_string(&v).map(|v| (k, v)))
        .collect::<serde_json::Result<HashMap<String, String>>>()
        .map_err(|err| OperationError::Unexpected(err.to_string()))
}

fn json_from_map(
    m: HashMap<String, String>,
) -> Result<Map<String, Value>, OperationError> {
    m.into_iter()
        .map(|(k, v)| serde_json::from_str(&v).map(|v| (k, v)))
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

    crate::parse_config_file_with_filters(
        &file_content,
        &format,
        dimension_data,
        prefix_list.as_ref(),
        exclude_prefix_list.as_ref(),
    )
    .map_err(|e| OperationError::Unexpected(e.to_string()))
}

/// Each generation sits behind an `Arc` so readers can snapshot it and release the
/// lock before doing any real work. See [`ProviderCache::snapshot`].
#[derive(Default)]
pub struct CacheData {
    pub config: Arc<Config>,
    pub experiment: Option<Arc<ExperimentConfig>>,
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
                config: Arc::new(Config::default()),
                experiment: None,
            }),
        })
    }

    pub fn init_config(
        &self,
        default_configs: ExtendedMap,
        contexts: Vec<Context>,
        overrides: HashMap<String, Overrides>,
        dimensions: HashMap<String, DimensionInfo>,
    ) -> Result<(), OperationError> {
        let config = Arc::new(Config {
            default_configs,
            contexts,
            overrides,
            dimensions,
        });

        // Swap under the lock, drop the previous generation outside of it.
        let _old = {
            let mut cache_data = self.data.lock().map_err(|err| {
                OperationError::Unexpected(format!("Failed to acquire cache lock: {err}"))
            })?;
            std::mem::replace(&mut cache_data.config, config)
        };

        Ok(())
    }

    pub fn init_experiments(
        &self,
        experiments: Vec<FfiExperiment>,
        experiment_groups: Vec<FfiExperimentGroup>,
    ) -> Result<(), OperationError> {
        let experiment = Arc::new(ExperimentConfig {
            experiments,
            experiment_groups,
        });

        // Swap under the lock, drop the previous generation outside of it.
        let _old = {
            let mut cache_data = self.data.lock().map_err(|err| {
                OperationError::Unexpected(format!("Failed to acquire cache lock: {err}"))
            })?;
            cache_data.experiment.replace(experiment)
        };

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
        // Parse before taking the lock: no reason to make writers wait on JSON work.
        let query_data = json_from_map(query_data)?;

        let resolved = self
            .eval_config_inner(
                query_data,
                merge_strategy,
                filter_prefixes,
                filter_exclude_prefixes,
                targeting_key.as_deref(),
            )
            .map_err(OperationError::Unexpected)?;

        // Serialise off-lock: the snapshot is already detached from the cache.
        json_to_map(resolved)
    }

    fn filter_config(
        &self,
        dimension_data: Option<HashMap<String, String>>,
        prefix: Option<Vec<String>>,
        exclude_prefix: Option<Vec<String>>,
    ) -> Result<Config, OperationError> {
        let dimension_data = dimension_data.map(json_from_map).transpose()?;
        self.filter_config_inner(dimension_data, prefix, exclude_prefix)
            .map_err(OperationError::Unexpected)
    }

    fn filter_experiment_config(
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
        self.filter_experiment_config_inner(
            &dimension_data,
            prefix,
            exclude_prefix,
            partial_apply,
        )
        .map_err(OperationError::Unexpected)
    }

    fn get_applicable_variants(
        &self,
        dimension_data: HashMap<String, String>,
        prefix: Option<Vec<String>>,
        exclude_prefix: Option<Vec<String>>,
        targeting_key: String,
    ) -> Result<Vec<String>, OperationError> {
        let dimension_data = json_from_map(dimension_data)?;
        self.get_applicable_variants_inner(
            dimension_data,
            prefix,
            exclude_prefix,
            &targeting_key,
        )
        .map_err(OperationError::Unexpected)
    }
}

/// Shared, non-FFI cache operations. Both the UniFFI methods above and the C-ABI functions in
/// `ffi_legacy` call these, so the filter / variant logic lives in exactly one place; the two FFI
/// layers only differ in how they marshal arguments and errors.
impl ProviderCache {
    /// Clones the current generation out of the cache and releases the lock at once.
    ///
    /// Every read path goes through this, so the critical section is two refcount bumps
    /// and nothing more. That matters beyond contention: the lock is poisoned forever if
    /// a thread panics while holding it, and resolving a config can panic on malformed
    /// remote data. Keeping the resolution outside the guard means such a panic costs one
    /// call instead of bricking the cache for the life of the process.
    ///
    /// Both halves come from a single acquisition, so callers always see one consistent
    /// generation.
    fn snapshot(&self) -> Result<(Arc<Config>, Option<Arc<ExperimentConfig>>), String> {
        let cache_data = self
            .data
            .lock()
            .map_err(|err| format!("Failed to acquire cache lock: {err}"))?;
        Ok((cache_data.config.clone(), cache_data.experiment.clone()))
    }

    /// Resolves the cached config against `query_data`, layering in experiment variants
    /// when the cache holds experiments and a non-empty `targeting_key` was supplied.
    pub(crate) fn eval_config_inner(
        &self,
        mut query_data: Map<String, Value>,
        merge_strategy: MergeStrategy,
        filter_prefixes: Option<Vec<String>>,
        filter_exclude_prefixes: Option<Vec<String>>,
        targeting_key: Option<&str>,
    ) -> Result<Map<String, Value>, String> {
        let (config, experiment) = self.snapshot()?;

        let experiments = experiment.as_ref().filter(|experiment| {
            !experiment.experiments.is_empty() || !experiment.experiment_groups.is_empty()
        });
        let targeting_key = targeting_key.filter(|key| !key.is_empty());

        if let (Some(experiment_config), Some(targeting_key)) =
            (experiments, targeting_key)
        {
            let variants = get_applicable_variants(
                &config.dimensions,
                experiment_config.experiments.clone(),
                &experiment_config.experiment_groups,
                query_data.clone(),
                targeting_key,
                filter_prefixes.clone(),
                filter_exclude_prefixes.clone(),
            );
            query_data.insert("variantIds".to_string(), variants.into());
        }

        Ok(eval(
            config.default_configs.clone(),
            &config.contexts,
            &config.overrides,
            &config.dimensions,
            query_data,
            merge_strategy,
            filter_prefixes,
            filter_exclude_prefixes,
        ))
    }

    pub(crate) fn filter_config_inner(
        &self,
        dimension_data: Option<Map<String, Value>>,
        prefix: Option<Vec<String>>,
        exclude_prefix: Option<Vec<String>>,
    ) -> Result<Config, String> {
        let prefix_list = prefix.map(PrefixList::from_iter);
        let exclude_prefix_list = exclude_prefix.map(PrefixList::from_iter);

        let (config, _) = self.snapshot()?;

        Ok((*config).clone().filter(
            dimension_data,
            prefix_list.as_ref(),
            exclude_prefix_list.as_ref(),
        ))
    }

    pub(crate) fn filter_experiment_config_inner(
        &self,
        dimension_data: &Map<String, Value>,
        prefix: Option<Vec<String>>,
        exclude_prefix: Option<Vec<String>>,
        partial_apply: bool,
    ) -> Result<ExperimentConfig, String> {
        let (_, experiment) = self.snapshot()?;
        let exp_config = experiment
            .ok_or_else(|| "Experiment configuration not initialized".to_string())?;

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
            experiments: exp_filter_fn(
                exp_config.experiments.clone(),
                dimension_data,
                prefix,
                exclude_prefix,
            ),
            experiment_groups: exp_grp_filter_fn(
                exp_config.experiment_groups.clone(),
                dimension_data,
            ),
        })
    }

    pub(crate) fn get_applicable_variants_inner(
        &self,
        dimension_data: Map<String, Value>,
        prefix: Option<Vec<String>>,
        exclude_prefix: Option<Vec<String>>,
        targeting_key: &str,
    ) -> Result<Vec<String>, String> {
        let (config, experiment) = self.snapshot()?;
        let exp_config = experiment
            .ok_or_else(|| "Experiment configuration not initialized".to_string())?;

        Ok(get_applicable_variants(
            &config.dimensions,
            exp_config.experiments.clone(),
            &exp_config.experiment_groups,
            dimension_data,
            targeting_key,
            prefix,
            exclude_prefix,
        ))
    }
}
