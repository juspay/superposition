use rand::Rng;
use serde_json::{Map, Value};
use std::collections::HashMap;
use superposition_types::{Context, Overrides};
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
    &Map<String, Value>,
    MergeStrategy,
    Option<Vec<String>>,
) -> Result<Map<String, Value>, String>;

#[allow(clippy::too_many_arguments)]
fn ffi_eval_logic(
    default_config: HashMap<String, String>,
    contexts: &[Context],
    overrides: HashMap<String, Overrides>,
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
        let toss = e_args
            .targeting_key
            .parse::<i8>()
            .unwrap_or(rand::rng().random_range(0..=99))
            % 100;
        let variants = get_applicable_variants(
            &e_args.experiments,
            &_q,
            toss,
            filter_prefixes.clone(),
        )
        .map_err(OperationError::Unexpected)?;
        _q.insert("variantIds".to_string(), variants.into());
    }

    let r = eval_fn(
        _d,
        contexts,
        &overrides,
        &_q,
        merge_strategy,
        filter_prefixes,
    )
    .map_err(OperationError::Unexpected)?;

    json_to_map(r).map_err(|err| OperationError::Unexpected(err.to_string()))
}

#[uniffi::export]
fn ffi_eval_config(
    default_config: HashMap<String, String>,
    contexts: &[Context],
    overrides: HashMap<String, Overrides>,
    query_data: HashMap<String, String>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    experimentation: Option<ExperimentationArgs>,
) -> Result<HashMap<String, String>, OperationError> {
    ffi_eval_logic(
        default_config,
        contexts,
        overrides,
        query_data,
        merge_strategy,
        filter_prefixes,
        experimentation,
        eval_config,
    )
}

#[uniffi::export]
fn ffi_eval_config_with_reasoning(
    default_config: HashMap<String, String>,
    contexts: &[Context],
    overrides: HashMap<String, Overrides>,
    query_data: HashMap<String, String>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    experimentation: Option<ExperimentationArgs>,
) -> Result<HashMap<String, String>, OperationError> {
    ffi_eval_logic(
        default_config,
        contexts,
        overrides,
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
    query_data: HashMap<String, String>,
    prefix: Option<Vec<String>>,
) -> Result<Vec<String>, OperationError> {
    let _query_data = json_from_map(query_data.clone())
        .map_err(|err| OperationError::Unexpected(err.to_string()))?;

    // TODO Migrate to new bucketing procedure.
    let toss = eargs
        .targeting_key
        .parse::<i8>()
        .unwrap_or(rand::rng().random_range(0..=99))
        % 100;
    let r = get_applicable_variants(&eargs.experiments, &_query_data, toss, prefix)
        .map_err(OperationError::Unexpected)?;

    Ok(r)
}
