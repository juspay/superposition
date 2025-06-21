use bigdecimal::BigDecimal;
use chrono::{DateTime, Utc};
use diesel::prelude::AsChangeset;
use serde::{Deserialize, Serialize};
use superposition_types::{
    api::context::UpdateRequest,
    database::{
        models::{
            cac::{Context, FunctionCode},
            ChangeReason, Description,
        },
        schema::contexts,
    },
    Cac, Condition, Overrides,
};

#[derive(Serialize, AsChangeset)]
#[diesel(table_name = contexts)]
pub(crate) struct UpdateContextOverridesChangeset {
    pub override_id: String,
    #[serde(rename = "override")]
    pub override_: Overrides,
    pub last_modified_at: DateTime<Utc>,
    pub last_modified_by: String,
    pub description: Option<Description>,
    pub change_reason: ChangeReason,
}

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

#[cfg_attr(test, derive(Debug, PartialEq))] // Derive traits only when running tests
#[derive(serde::Deserialize, Clone)]
#[serde(rename_all = "UPPERCASE")]
pub enum ContextAction {
    Put(PutReq),
    Replace(UpdateRequest),
    Delete(String),
    Move((String, MoveReq)),
}

#[derive(serde::Serialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum ContextBulkResponse {
    Put(PutResp),
    Replace(Context),
    Delete(String),
    Move(PutResp),
}

#[derive(Clone)]
pub struct FunctionsInfo {
    pub name: String,
    pub code: Option<FunctionCode>,
}

#[derive(Serialize)]
pub struct WeightRecomputeResponse {
    pub id: String,
    pub condition: Condition,
    pub old_weight: BigDecimal,
    pub new_weight: BigDecimal,
}

#[derive(Deserialize)]
pub struct BulkOperation {
    pub operations: Vec<ContextAction>,
}

#[derive(Serialize)]
pub struct BulkOperationResponse {
    pub output: Vec<ContextBulkResponse>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{json, Map};
    use superposition_types::Cac;

    #[test]
    fn test_deserialize_context_action() {
        let context = json!({
            "and": [
                {
                    "==": [
                        {"var": "foo"},
                        "bar"
                    ]
                },
                {
                    "==": [
                        {"var": "bar"},
                        "baz"
                    ]
                }
            ]
        });
        let put_request = json!({
            "context": context,
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

        let context = Cac::<Condition>::try_from(context.as_object().unwrap().clone())
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
