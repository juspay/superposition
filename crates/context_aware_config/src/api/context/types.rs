use bigdecimal::BigDecimal;
use serde::{Deserialize, Serialize};
use superposition_types::{
    custom_query::CommaSeparatedStringQParams, database::models::cac::Context, Cac,
    Condition, Overrides, SortBy,
};

#[cfg_attr(test, derive(Debug, PartialEq))] // Derive traits only when running tests
#[derive(Deserialize, Clone)]
pub struct PutReq {
    pub context: Cac<Condition>,
    pub r#override: Cac<Overrides>,
    pub description: Option<String>,
    pub change_reason: String,
}

#[cfg_attr(test, derive(Debug, PartialEq))] // Derive traits only when running tests
#[derive(Deserialize, Clone)]
pub struct MoveReq {
    pub context: Cac<Condition>,
    pub description: Option<String>,
    pub change_reason: String,
}

#[derive(Deserialize, Clone)]
pub struct DimensionCondition {
    pub var: String,
}

#[derive(Serialize, Debug)]
pub struct PutResp {
    pub context_id: String,
    pub override_id: String,
    pub weight: BigDecimal,
    pub description: String,
    pub change_reason: String,
}

impl From<Context> for PutResp {
    fn from(value: Context) -> Self {
        PutResp {
            context_id: value.id,
            override_id: value.override_id,
            weight: value.weight,
            description: value.description,
            change_reason: value.change_reason,
        }
    }
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ContextFilterSortOn {
    CreatedAt,
    Weight,
}

impl Default for ContextFilterSortOn {
    fn default() -> Self {
        Self::Weight
    }
}

#[derive(Deserialize)]
pub struct ContextFilters {
    pub page: Option<u32>,
    pub size: Option<u32>,
    pub prefix: Option<CommaSeparatedStringQParams>,
    pub sort_on: Option<ContextFilterSortOn>,
    pub sort_by: Option<SortBy>,
    pub created_by: Option<CommaSeparatedStringQParams>,
}

#[cfg_attr(test, derive(Debug, PartialEq))] // Derive traits only when running tests
#[derive(serde::Deserialize, Clone)]
#[serde(rename_all = "UPPERCASE")]
pub enum ContextAction {
    Put(PutReq),
    Replace(PutReq),
    Delete(String),
    Move((String, MoveReq)),
}

#[derive(serde::Serialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum ContextBulkResponse {
    Put(PutResp),
    Replace(PutResp),
    Delete(String),
    Move(PutResp),
}

#[derive(Deserialize, Clone)]
pub struct FunctionsInfo {
    pub name: String,
    pub code: Option<String>,
}

#[derive(Serialize)]
pub struct WeightRecomputeResponse {
    pub id: String,
    pub condition: Condition,
    pub old_weight: BigDecimal,
    pub new_weight: BigDecimal,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{json, Map};
    use superposition_types::Cac;

    #[test]
    fn test_deserialize_context_action() {
        let put_request = json!({
            "context": {
                "foo": "bar",
                "bar": {
                    "baz": "baz"
                }
            },
            "override": {
                "foo": "baz"
            },
            "description": "",
            "change_reason": ""
        });

        let action_str = json!({
            "PUT": put_request
        })
        .to_string();

        let mut expected_condition = Map::new();
        expected_condition.insert("foo".to_string(), json!("bar"));
        expected_condition.insert("bar".to_string(), json!({ "baz": "baz"}));
        let context = Cac::<Condition>::try_from(expected_condition)
            .expect("Invalid context condition");

        let mut expected_override = Map::new();
        expected_override.insert("foo".to_string(), json!("baz"));
        let override_ = Cac::<Overrides>::try_from(expected_override)
            .expect("Invalid context override");

        let expected_action = ContextAction::Put(PutReq {
            context,
            r#override: override_,
            description: Some("".to_string()),
            change_reason: "".to_string(),
        });

        let action_deserialized =
            serde_json::from_str::<ContextAction>(&action_str).unwrap();

        assert_eq!(action_deserialized, expected_action);
    }
}
