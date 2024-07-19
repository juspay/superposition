use serde::{Deserialize, Serialize};
use serde_json::Value;
use superposition_types::{Cac, Condition, Overrides};

#[cfg_attr(test, derive(Debug, PartialEq))] // Derive traits only when running tests
#[derive(Deserialize, Clone)]
pub struct PutReq {
    pub context: Cac<Condition>,
    pub r#override: Cac<Overrides>,
}

#[cfg_attr(test, derive(Debug, PartialEq))] // Derive traits only when running tests
#[derive(Deserialize, Clone)]
pub struct MoveReq {
    pub context: Cac<Condition>,
}

#[derive(Deserialize, Clone)]
pub struct DimensionCondition {
    pub var: String,
}

#[derive(Serialize, Debug)]
pub struct PutResp {
    pub context_id: String,
    pub override_id: String,
    pub priority: i32,
}

#[derive(Deserialize)]
pub struct PaginationParams {
    pub page: Option<u32>,
    pub size: Option<u32>,
}

#[cfg_attr(test, derive(Debug, PartialEq))] // Derive traits only when running tests
#[derive(serde::Deserialize, Clone)]
#[serde(rename_all = "UPPERCASE")]
pub enum ContextAction {
    Put(PutReq),
    Delete(String),
    Move((String, MoveReq)),
}

#[derive(serde::Serialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum ContextBulkResponse {
    Put(PutResp),
    Delete(String),
    Move(PutResp),
}

#[derive(Deserialize, Clone)]
pub struct FunctionsInfo {
    pub name: String,
    pub code: Option<String>,
}

#[derive(Serialize)]
pub struct PriorityRecomputeResponse {
    pub id: String,
    pub condition: Value,
    pub old_priority: i32,
    pub new_priority: i32,
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
            }
        });

        let action_str = json!({
            "PUT": put_request
        })
        .to_string();

        let mut expected_condition = Map::new();
        expected_condition.insert("foo".to_string(), json!("bar"));
        expected_condition.insert("bar".to_string(), json!({ "baz": "baz"}));
        let condition = Cac::<Condition>::try_from(expected_condition)
            .expect("Invalid context condition");

        let mut expected_override = Map::new();
        expected_override.insert("foo".to_string(), json!("baz"));
        let override_ = Cac::<Overrides>::try_from(expected_override)
            .expect("Invalid context override");

        let expected_action = ContextAction::Put(PutReq {
            context: condition,
            r#override: override_,
        });

        let action_deserialized =
            serde_json::from_str::<ContextAction>(&action_str).unwrap();

        assert_eq!(action_deserialized, expected_action);
    }
}
