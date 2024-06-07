use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

#[cfg_attr(test, derive(Debug, PartialEq))] // Derive traits only when running tests
#[derive(Deserialize, Clone)]
pub struct PutReq {
    pub context: Map<String, Value>,
    pub r#override: Map<String, Value>,
}

#[cfg_attr(test, derive(Debug, PartialEq))] // Derive traits only when running tests
#[derive(Deserialize, Clone)]
pub struct MoveReq {
    pub context: Map<String, Value>,
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
pub enum ContextAction {
    PUT(PutReq),
    DELETE(String),
    MOVE((String, MoveReq)),
}

#[derive(serde::Serialize)]
pub enum ContextBulkResponse {
    PUT(PutResp),
    DELETE(String),
    MOVE(PutResp),
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
    use serde_json::json;

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

        let mut expected_context = Map::new();
        expected_context.insert("foo".to_string(), json!("bar"));
        expected_context.insert("bar".to_string(), json!({ "baz": "baz"}));

        let mut expected_override = Map::new();
        expected_override.insert("foo".to_string(), json!("baz"));

        let expected_action = ContextAction::PUT(PutReq {
            context: expected_context,
            r#override: expected_override,
        });

        let action_deserialized =
            serde_json::from_str::<ContextAction>(&action_str).unwrap();

        assert_eq!(action_deserialized, expected_action);
    }
}
