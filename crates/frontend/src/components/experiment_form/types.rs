use crate::types::Variant;
use serde::Serialize;
use serde_json::{Map, Value};

#[derive(Serialize)]
pub struct ExperimentCreateRequest {
    pub name: String,

    pub context: Value,
    pub variants: Vec<Variant>,
}

#[derive(Serialize, Debug)]
pub struct VariantUpdateRequest {
    pub id: String,
    pub overrides: Map<String, Value>,
}

#[derive(Serialize, Debug)]
pub struct ExperimentUpdateRequest {
    pub variants: Vec<VariantUpdateRequest>,
}
