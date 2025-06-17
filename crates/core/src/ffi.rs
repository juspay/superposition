use serde_json::{Map, Value};
use std::collections::HashMap;
use superposition_types::{Context, Overrides};
use thiserror::Error;

use crate::{eval_config, eval_config_with_reasoning, MergeStrategy};

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

fn ffi_eval_logic<F>(
    default_config: HashMap<String, String>,
    contexts: &[Context],
    overrides: HashMap<String, Overrides>,
    query_data: HashMap<String, String>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    eval_fn: F,
) -> Result<HashMap<String, String>, OperationError>
where
    F: FnOnce(
        Map<String, Value>,
        &[Context],
        &HashMap<String, Overrides>,
        &Map<String, Value>,
        MergeStrategy,
        Option<Vec<String>>,
    ) -> Result<Map<String, Value>, String>,
{
    let _d = json_from_map(default_config)
        .map_err(|err| OperationError::Unexpected(err.to_string()))?;
    let _q = json_from_map(query_data)
        .map_err(|err| OperationError::Unexpected(err.to_string()))?;

    let r = eval_fn(
        _d,
        contexts,
        &overrides,
        &_q,
        merge_strategy,
        filter_prefixes,
    )
    .map_err(|err| OperationError::Unexpected(err))?;

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
) -> Result<HashMap<String, String>, OperationError> {
    ffi_eval_logic(
        default_config,
        contexts,
        overrides,
        query_data,
        merge_strategy,
        filter_prefixes,
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
) -> Result<HashMap<String, String>, OperationError> {
    ffi_eval_logic(
        default_config,
        contexts,
        overrides,
        query_data,
        merge_strategy,
        filter_prefixes,
        eval_config_with_reasoning,
    )
}

