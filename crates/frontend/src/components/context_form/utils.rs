use crate::utils::get_host;
use reqwest::StatusCode;
use serde_json::{json, Map, Value};

pub fn get_condition_schema(var: &str, op: &str, val: &str) -> Value {
    match op {
        "<=" => {
            let mut split_value = val.split(',');
            let first_operand =
                split_value.next().unwrap().trim().parse::<i64>().unwrap();
            let second_operand =
                split_value.next().unwrap().trim().parse::<i64>().unwrap();

            json!({
                op: [
                    first_operand,
                    { "var": var },
                    second_operand
                ]
            })
        }
        _ => {
            json!({
                op: [
                    {"var": var},
                    val
                ]
            })
        }
    }
}

pub fn construct_context(conditions: Vec<(String, String, String)>) -> Value {
    let condition_schemas = conditions
        .iter()
        .map(|(variable, operator, value)| {
            get_condition_schema(variable, operator, value)
        })
        .collect::<Vec<Value>>();

    let context = if condition_schemas.len() == 1 {
        condition_schemas[0].clone()
    } else {
        json!({ "and": condition_schemas })
    };

    context
}

pub fn construct_request_payload(
    overrides: Map<String, Value>,
    conditions: Vec<(String, String, String)>,
) -> Value {
    // Construct the override section
    let override_section: Map<String, Value> = overrides;

    // Construct the context section
    let context_section = construct_context(conditions);

    // Construct the entire request payload
    let request_payload = json!({
        "override": override_section,
        "context": context_section
    });

    request_payload
}

pub async fn create_context(
    tenant: String,
    overrides: Map<String, Value>,
    conditions: Vec<(String, String, String)>,
) -> Result<String, String> {
    let client = reqwest::Client::new();
    let host = get_host();
    let url = format!("{host}/context");
    let request_payload = construct_request_payload(overrides, conditions);
    let response = client
        .put(url)
        .header("x-tenant", tenant)
        .header("Authorization", "Bearer 12345678")
        .json(&request_payload)
        .send()
        .await
        .map_err(|e| e.to_string())?;
    match response.status() {
        StatusCode::OK => response.text().await.map_err(|e| e.to_string()),
        StatusCode::BAD_REQUEST => Err("Schema Validation Failed".to_string()),
        _ => Err("Internal Server Error".to_string()),
    }
}
