use super::Conditions;
use crate::utils::{construct_request_headers, get_host, parse_json_response, request};

use anyhow::Result;
use serde_json::{json, Map, Value};

pub fn context_payload(
    overrides: Map<String, Value>,
    conditions: Conditions,

    description: String,
    change_reason: String,
) -> Value {
    let context: Value = conditions.to_context_json();
    let payload = json!({
        "override": overrides,
        "context": context,
        "description": description,
        "change_reason": change_reason,
    });

    payload
}

pub async fn create_context(
    tenant: String,
    overrides: Map<String, Value>,
    conditions: Conditions,
    description: String,
    change_reason: String,
) -> Result<Value, String> {
    let host = get_host();
    let url = format!("{host}/context");
    let request_payload =
        context_payload(overrides, conditions, description, change_reason);
    let response = request(
        url,
        reqwest::Method::PUT,
        Some(request_payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn update_context(
    tenant: String,
    overrides: Map<String, Value>,
    conditions: Conditions,
    description: String,
    change_reason: String,
) -> Result<serde_json::Value, String> {
    let host = get_host();
    let url = format!("{host}/context/overrides");
    let request_payload =
        context_payload(overrides, conditions, description, change_reason);
    let response = request(
        url,
        reqwest::Method::PUT,
        Some(request_payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await?;

    parse_json_response(response).await
}
