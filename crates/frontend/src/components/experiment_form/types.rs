use serde_json::Map;
use superposition_types::{api::experiments::VariantUpdateRequest, Exp, Overrides};

use crate::types::VariantFormT;

impl TryFrom<VariantFormT> for VariantUpdateRequest {
    type Error = String;
    fn try_from(value: VariantFormT) -> Result<Self, Self::Error> {
        Ok(Self {
            id: value.id,
            overrides: Exp::<Overrides>::try_from(Map::from_iter(value.overrides))?,
        })
    }
}

impl FromIterator<VariantFormT> for Result<Vec<VariantUpdateRequest>, String> {
    fn from_iter<T: IntoIterator<Item = VariantFormT>>(iter: T) -> Self {
        iter.into_iter()
            .map(VariantUpdateRequest::try_from)
            .collect()
    }
}
