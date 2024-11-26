use std::collections::HashMap;

use serde::{Deserialize, Serialize};
use serde_json::Value;
use superposition_types::{
    experimentation::models::{ExperimentStatusType, VariantType},
    Exp, Overridden, Overrides,
};

#[derive(Clone, Debug)]
pub struct Config {
    pub tenant: String,
    pub hostname: String,
    pub poll_frequency: u64,
}

#[repr(C)]
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct Variant {
    pub id: String,
    pub overrides: Exp<Overrides>,
    pub(crate) variant_type: VariantType,
}

impl Overridden<Exp<Overrides>> for Variant {
    fn get_overrides(&self) -> Overrides {
        self.overrides.clone().into_inner()
    }
}

pub type Variants = Vec<Variant>;

#[repr(C)]
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
