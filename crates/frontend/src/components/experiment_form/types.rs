use crate::types::{Variant, VariantFormT};
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

impl From<VariantFormT> for VariantUpdateRequest {
    fn from(value: VariantFormT) -> Self {
        VariantUpdateRequest {
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
