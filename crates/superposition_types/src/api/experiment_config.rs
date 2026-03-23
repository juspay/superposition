use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_derives::{IsEmpty, QueryParam};

use crate::{
    api::experiments::ExperimentResponse, custom_query::CommaSeparatedStringQParams,
    custom_query::QueryParam, database::models::experimentation::ExperimentGroups,
    IsEmpty,
};

#[derive(Serialize, Deserialize)]
pub struct ExperimentConfig {
    pub experiments: Vec<ExperimentResponse>,
    pub experiment_groups: ExperimentGroups,
}

#[derive(Deserialize, Clone, PartialEq, IsEmpty, QueryParam, Default)]
pub struct ExperimentConfigFilters {
    #[query_param(skip_if_empty, iterable)]
    pub prefix: Option<CommaSeparatedStringQParams>,
}

#[derive(Deserialize)]
pub struct ExperimentConfigRequest {
    pub context: Option<Map<String, Value>>,
}
