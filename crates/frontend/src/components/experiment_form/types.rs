use serde::Serialize;
use serde_json::{Map, Value};
use superposition_types::database::models::experimentation::Variant;

use crate::types::VariantFormT;

#[derive(Serialize)]
pub struct ExperimentCreateRequest {
    pub name: String,
    pub context: Value,
    pub variants: Vec<Variant>,
    pub description: String,
    pub change_reason: String,
}

#[derive(Serialize, Debug)]
pub struct VariantUpdateRequest {
    pub id: String,
    pub overrides: Map<String, Value>,
}

impl From<VariantFormT> for VariantUpdateRequest {
    fn from(value: VariantFormT) -> Self {
        Self {
            id: value.id,
            overrides: Map::from_iter(value.overrides),
        }
    }
}

impl FromIterator<VariantFormT> for Vec<VariantUpdateRequest> {
    fn from_iter<T: IntoIterator<Item = VariantFormT>>(iter: T) -> Self {
        iter.into_iter().map(VariantUpdateRequest::from).collect()
    }
}

#[derive(Serialize, Debug)]
pub struct ExperimentUpdateRequest {
    pub variants: Vec<VariantUpdateRequest>,
}
