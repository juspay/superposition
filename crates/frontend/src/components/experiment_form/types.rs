use serde_json::Value;
use serde::Serialize;
use crate::pages::ExperimentList::types::{
    DefaultConfig, Dimension, Variant, VariantType,
};

#[derive(Serialize)]
pub struct ExperimentCreateRequest {
    pub name: String,

    pub context: Value,
    pub variants: Vec<Variant>,
}