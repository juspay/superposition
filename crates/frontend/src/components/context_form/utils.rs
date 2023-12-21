use reqwest::StatusCode;
use serde_json::{json, Map, Value};

pub fn construct_context(conditions: Vec<(String, String, String)>) -> Value {
    let context = if conditions.len() == 1 {
        // Single condition
        let (variable, operator, value) = &conditions[0];
        json!({
            operator: [
                { "var": variable },
                value
            ]
        })
    } else {
        // Multiple conditions inside an "and"
        let and_conditions: Vec<Value> = conditions
            .into_iter()
            .map(|(variable, operator, value)| {
                json!({
                    operator: [
                        { "var": variable },
                        value
                    ]
                })
            })
            .collect();

        json!({ "and": and_conditions })
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
    let host = "http://localhost:8080";
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
