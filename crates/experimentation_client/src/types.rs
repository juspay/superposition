use std::collections::HashMap;

use superposition_types::{
    api::experiments::ExperimentResponse,
    database::models::experimentation::ExperimentGroup,
};

#[derive(Clone, Debug)]
pub struct Config {
    pub tenant: String,
    pub hostname: String,
    pub poll_frequency: u64,
}

pub type Experiments = Vec<ExperimentResponse>;

pub(crate) type ExperimentStore = HashMap<String, ExperimentResponse>;

pub(crate) type ExperimentGroupStore = HashMap<String, ExperimentGroup>;
