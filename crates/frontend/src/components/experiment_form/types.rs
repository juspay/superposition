use crate::pages::ExperimentList::types::Variant;
use serde::Serialize;
use serde_json::Value;

#[derive(Serialize)]
pub struct ExperimentCreateRequest {
    pub name: String,

    pub context: Value,
    pub variants: Vec<Variant>,
}
