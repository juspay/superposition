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
        let default_config_map = json_from_map(default_config)?;

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
        filter_exclude_prefixes: Option<Vec<String>>,
        targeting_key: Option<String>,
    ) -> Result<HashMap<String, String>, OperationError> {
        let cache_data = self.data.lock().map_err(|err| {
            OperationError::Unexpected(format!("Failed to acquire cache lock: {}", err))
        })?;

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

        json_to_map(r)
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
        self.filter_experiment_inner(
            &dimension_data,
            prefix,
            exclude_prefix,
            partial_apply,
        )
        .map_err(OperationError::Unexpected)
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
    pub(crate) fn filter_config_inner(
        &self,
        dimension_data: Option<Map<String, Value>>,
        prefix: Option<Vec<String>>,
        exclude_prefix: Option<Vec<String>>,
    ) -> Result<Config, String> {
        let prefix_list = prefix.map(PrefixList::from_iter);
        let exclude_prefix_list = exclude_prefix.map(PrefixList::from_iter);

        let config = {
            let cache_data = self
                .data
                .lock()
                .map_err(|err| format!("Failed to acquire cache lock: {}", err))?;
            cache_data.config.clone()
        };

        Ok(config.filter(
            dimension_data,
            prefix_list.as_ref(),
            exclude_prefix_list.as_ref(),
        ))
    }

    pub(crate) fn filter_experiment_inner(
        &self,
        dimension_data: &Map<String, Value>,
        prefix: Option<Vec<String>>,
        exclude_prefix: Option<Vec<String>>,
        partial_apply: bool,
    ) -> Result<ExperimentConfig, String> {
        let (exps, exp_grps) = {
            let cache_data = self
                .data
                .lock()
                .map_err(|err| format!("Failed to acquire cache lock: {}", err))?;
            let exp_config = cache_data
                .experiment
                .as_ref()
                .ok_or_else(|| "Experiment configuration not initialized".to_string())?;
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
            experiments: exp_filter_fn(exps, dimension_data, prefix, exclude_prefix),
            experiment_groups: exp_grp_filter_fn(exp_grps, dimension_data),
        })
    }

    pub(crate) fn get_applicable_variants_inner(
        &self,
        dimension_data: Map<String, Value>,
        prefix: Option<Vec<String>>,
        exclude_prefix: Option<Vec<String>>,
        targeting_key: &str,
    ) -> Result<Vec<String>, String> {
        let (exps, exp_grps, dimensions_info) = {
            let cache_data = self
                .data
                .lock()
                .map_err(|err| format!("Failed to acquire cache lock: {}", err))?;
            let exp_config = cache_data
                .experiment
                .as_ref()
                .ok_or_else(|| "Experiment configuration not initialized".to_string())?;
            (
                exp_config.experiments.clone(),
                exp_config.experiment_groups.clone(),
                cache_data.config.dimensions.clone(),
            )
        };

        Ok(get_applicable_variants(
            &dimensions_info,
            exps,
            &exp_grps,
            dimension_data,
            targeting_key,
            prefix,
            exclude_prefix,
        ))
    }
}
