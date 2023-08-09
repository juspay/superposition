use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Clone, Debug)]
pub struct Config {
    pub hostname: String,
    pub poll_frequency: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Deserialize, Serialize)]
pub(crate) enum ExperimentStatusType {
    INPROGRESS,
    CONCLUDED,
}

#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Variant {
    pub id: String,
    pub overrides: Value,
}

pub type Variants = Vec<Variant>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Experiment {
    pub(crate) variants: Variants,
    pub(crate) name: String,
    pub(crate) id: String,
    pub(crate) traffic_percentage: u8,
    pub(crate) context: Value,
    pub(crate) status: ExperimentStatusType,
}

pub type Experiments = Vec<Experiment>;

pub(crate) type ExperimentStore = HashMap<String, Experiment>;
