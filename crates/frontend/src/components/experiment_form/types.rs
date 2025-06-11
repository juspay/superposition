use chrono::Utc;
use serde_json::Map;
use superposition_types::{api::experiments::VariantUpdateRequest, database::models::{experimentation::{ExperimentGroup, GroupType, TrafficPercentage}, Buckets, ChangeReason, Description}, Condition, Exp, Overrides};

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

pub fn default_experiment_group() -> ExperimentGroup {
    ExperimentGroup {
        id: -1,
        name: "None".to_string(),
        description: Description::default(),
        change_reason: ChangeReason::default(),
        context: Condition::default(),
        context_hash: "".to_string(),
        traffic_percentage: TrafficPercentage::default(),
        member_experiment_ids: Vec::new(),
        created_at: Utc::now(),
        created_by: "".to_string(),
        last_modified_at: Utc::now(),
        last_modified_by: "".to_string(),
        buckets: Buckets::new(),
        group_type: GroupType::SystemGenerated
    }
}
