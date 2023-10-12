use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Clone, Debug)]
pub struct Config {
    pub tenant: String,
    pub hostname: String,
    pub poll_frequency: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Deserialize, Serialize)]
pub(crate) enum ExperimentStatusType {
    CREATED,
    INPROGRESS,
    CONCLUDED,
}

#[derive(Deserialize, Serialize, Clone, Debug, PartialEq)]
pub(crate) enum VariantType {
    CONTROL,
    EXPERIMENTAL,
}

#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Variant {
    pub id: String,
    pub overrides: Value,
    pub(crate) variant_type: VariantType,
}

pub type Variants = Vec<Variant>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Experiment {
    pub variants: Variants,
    pub(crate) name: String,
    pub(crate) id: String,
    pub(crate) traffic_percentage: u8,
    pub(crate) context: Value,
    pub(crate) status: ExperimentStatusType,
}

pub type Experiments = Vec<Experiment>;

pub(crate) type ExperimentStore = HashMap<String, Experiment>;

#[derive(Serialize, Deserialize, Default)]
pub(crate) struct ListExperimentsResponse {
    pub(crate) total_items: i64,
    pub(crate) total_pages: i64,
    pub(crate) data: Experiments,
}
