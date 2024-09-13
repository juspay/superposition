use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

use super::{ExperimentStatusType, Variant, Variants};

use super::VariantType;

#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct VariantFormT {
    pub id: String,
    pub variant_type: VariantType,
    pub overrides: Vec<(String, Value)>,
}

impl From<Variant> for VariantFormT {
    fn from(value: Variant) -> Self {
        VariantFormT {
            id: value.id,
            variant_type: value.variant_type,
            overrides: value.overrides.into_iter().collect(),
        }
    }
}

impl FromIterator<Variant> for Vec<VariantFormT> {
    fn from_iter<T: IntoIterator<Item = Variant>>(iter: T) -> Self {
        iter.into_iter().map(VariantFormT::from).collect()
    }
}

impl FromIterator<VariantFormT> for Vec<Variant> {
    fn from_iter<T: IntoIterator<Item = VariantFormT>>(iter: T) -> Self {
        iter.into_iter().map(Variant::from).collect()
    }
}

impl From<VariantFormT> for Variant {
    fn from(value: VariantFormT) -> Self {
        Variant {
            id: value.id,
            variant_type: value.variant_type,
            overrides: Map::from_iter(value.overrides),
            context_id: None,
            override_id: None,
        }
    }
}


#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Experiment {
    pub(crate) variants: Variants,
    pub(crate) name: String,
    pub(crate) id: String,
    pub(crate) traffic_percentage: u8,
    pub(crate) context: Vec<Condition>,
    pub(crate) status: ExperimentStatusType,
    pub(crate) override_keys: Value,
    pub(crate) created_by: String,
    pub(crate) created_at: DateTime<Utc>,
    pub(crate) last_modified: DateTime<Utc>,
    pub(crate) chosen_variant: Option<String>,
}

impl From<ExperimentResponse> for Experiment {
    fn from(value: ExperimentResponse) -> Self {
        Experiment {
            name: value.name,
            id: value.id,
            traffic_percentage: value.traffic_percentage as u8,
            status: value.status,
            override_keys: json!(value.override_keys),
            created_by: value.created_by,
            created_at: value.created_at,
            last_modified: value.last_modified,
            chosen_variant: value.chosen_variant,
            variants: serde_json::from_value(value.variants).unwrap_or_default(),
            context: extract_conditions(&value.context),
        }
    }
}
