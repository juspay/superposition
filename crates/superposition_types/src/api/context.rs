use bigdecimal::BigDecimal;
use serde::{Deserialize, Serialize};
use superposition_derives::{IsEmpty, QueryParam};

use crate::{
    api::DimensionMatchStrategy,
    custom_query::{CommaSeparatedStringQParams, QueryParam},
    database::models::{cac::Context, ChangeReason, Description},
    Cac, Condition, IsEmpty, Overrides, SortBy,
};

#[derive(
    Deserialize, PartialEq, Clone, strum_macros::EnumIter, strum_macros::Display,
)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum SortOn {
    Weight,
    CreatedAt,
    LastModifiedAt,
}

impl SortOn {
    pub fn label(&self) -> String {
        match self {
            Self::CreatedAt => "Created At".to_string(),
            Self::LastModifiedAt => "Last Modified At".to_string(),
            Self::Weight => "Weight".to_string(),
        }
    }
}

impl Default for SortOn {
    fn default() -> Self {
        Self::Weight
    }
}

#[derive(Deserialize, PartialEq, Default, Clone, IsEmpty, QueryParam)]
pub struct ContextListFilters {
    #[query_param(skip_if_empty, iterable)]
    pub prefix: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty, iterable)]
    pub exclude_prefix: Option<CommaSeparatedStringQParams>,
    pub sort_on: Option<SortOn>,
    pub sort_by: Option<SortBy>,
    #[query_param(skip_if_empty, iterable)]
    pub created_by: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty, iterable)]
    pub last_modified_by: Option<CommaSeparatedStringQParams>,
    #[query_param(skip_if_empty)]
    pub plaintext: Option<String>,
    pub dimension_match_strategy: Option<DimensionMatchStrategy>,
}

#[derive(Deserialize, Serialize, Clone, Debug, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum Identifier {
    Context(Cac<Condition>),
    Id(String),
}

#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct UpdateRequest {
    pub context: Identifier,
    #[serde(rename = "override")]
    pub override_: Cac<Overrides>,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct ContextValidationRequest {
    pub context: Cac<Condition>,
}

#[derive(Deserialize, Serialize, Clone)]
pub struct PutRequest {
    pub context: Cac<Condition>,
    pub r#override: Cac<Overrides>,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}

#[derive(Deserialize, Serialize, Clone)]
pub struct MoveRequest {
    pub context: Cac<Condition>,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}

#[derive(Deserialize, Serialize, Clone)]
#[serde(rename_all = "UPPERCASE")]
pub enum ContextAction {
    Put(PutRequest),
    Replace(UpdateRequest),
    Delete(String),
    Move { id: String, request: MoveRequest },
}

/// Flattened, and `dropped_keys` is omitted when empty, so the old payload shape is unchanged.
#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct ContextWithDroppedKeys {
    #[serde(flatten)]
    pub context: Context,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub dropped_keys: Vec<String>,
}

impl ContextWithDroppedKeys {
    pub fn new(context: Context, dropped_keys: Vec<String>) -> Self {
        Self {
            context,
            dropped_keys,
        }
    }
}

impl From<Context> for ContextWithDroppedKeys {
    fn from(context: Context) -> Self {
        Self::new(context, Vec::new())
    }
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "UPPERCASE")]
pub enum ContextBulkResponse {
    Put(ContextWithDroppedKeys),
    Replace(Context),
    Delete(String),
    Move(Context),
}

#[derive(Deserialize, Serialize)]
pub struct WeightRecomputeResponse {
    pub id: String,
    pub condition: Condition,
    pub old_weight: BigDecimal,
    pub new_weight: BigDecimal,
}

#[derive(Deserialize, Serialize)]
pub struct BulkOperation {
    pub operations: Vec<ContextAction>,
}

#[derive(Deserialize, Serialize)]
pub struct BulkOperationResponse {
    pub output: Vec<ContextBulkResponse>,
}

#[cfg(test)]
mod tests {
    use bigdecimal::BigDecimal;
    use chrono::Utc;
    use serde_json::{json, Value};

    use super::*;
    use crate::database::models::{ChangeReason, Description};

    fn sample_context() -> Context {
        let now = Utc::now();
        Context {
            id: "ctx-1".to_string(),
            value: Cac::<Condition>::try_from(
                json!({ "os": "android" }).as_object().unwrap().clone(),
            )
            .unwrap()
            .into_inner(),
            override_id: "ovr-1".to_string(),
            created_at: now,
            created_by: "test@superposition.io".to_string(),
            override_: Cac::<Overrides>::try_from(
                json!({ "colour": "red" }).as_object().unwrap().clone(),
            )
            .unwrap()
            .into_inner(),
            last_modified_at: now,
            last_modified_by: "test@superposition.io".to_string(),
            weight: "12345678901234567890.000000000001"
                .parse::<BigDecimal>()
                .unwrap(),
            description: Description::try_from("d".to_string()).unwrap(),
            change_reason: ChangeReason::try_from("c".to_string()).unwrap(),
        }
    }

    /// `flatten` goes through `deserialize_any`, where `BigDecimal` tends to break.
    #[test]
    fn context_with_dropped_keys_round_trips() {
        let original = ContextWithDroppedKeys::new(
            sample_context(),
            vec!["a".to_string(), "b".to_string()],
        );

        let encoded = serde_json::to_string(&original).unwrap();
        let decoded: ContextWithDroppedKeys = serde_json::from_str(&encoded).unwrap();

        assert_eq!(decoded.context.id, original.context.id);
        assert_eq!(decoded.context.weight, original.context.weight);
        assert_eq!(decoded.context.created_at, original.context.created_at);
        assert_eq!(decoded.context.override_, original.context.override_);
        assert_eq!(decoded.dropped_keys, vec!["a".to_string(), "b".to_string()]);
    }

    #[test]
    fn no_dropped_keys_serialises_to_the_plain_context_shape() {
        let context = sample_context();
        let wrapped: ContextWithDroppedKeys = context.clone().into();

        let wrapped_json: Value = serde_json::to_value(&wrapped).unwrap();
        let plain_json: Value = serde_json::to_value(&context).unwrap();

        assert_eq!(wrapped_json, plain_json);
        assert!(wrapped_json.get("dropped_keys").is_none());
    }

    #[test]
    fn a_payload_without_dropped_keys_still_parses() {
        let plain = serde_json::to_string(&sample_context()).unwrap();
        let decoded: ContextWithDroppedKeys = serde_json::from_str(&plain).unwrap();
        assert!(decoded.dropped_keys.is_empty());
    }
}
