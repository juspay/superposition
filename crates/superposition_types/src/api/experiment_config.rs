use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_derives::{IsEmpty, QueryParam};

use crate::{
    api::{experiments::ExperimentResponse, DimensionMatchStrategy},
    custom_query::{CommaSeparatedStringQParams, QueryParam},
    database::models::experimentation::ExperimentGroups,
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
    pub dimension_match_strategy: Option<DimensionMatchStrategy>,
}

#[derive(Deserialize)]
pub struct ExperimentConfigRequest {
    pub context: Option<Map<String, Value>>,
}
