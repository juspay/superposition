use anyhow::Result;
use serde_json::{json, Map, Value};
use superposition_types::{
    api::context::{Identifier, UpdateRequest},
    database::models::{ChangeReason, Description},
    Cac, Overrides,
};

use crate::utils::{construct_request_headers, get_host, parse_json_response, request};

use super::Conditions;

pub fn context_payload(
    overrides: Map<String, Value>,
    conditions: Conditions,
    description: String,
    change_reason: String,
) -> Value {
    let context = conditions.as_context_json();
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
    org_id: String,
) -> Result<Value, String> {
    let host = get_host();
    let url = format!("{host}/context");
    let request_payload =
        context_payload(overrides, conditions, description, change_reason);
    let response = request(
        url,
        reqwest::Method::PUT,
        Some(request_payload),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn update_context(
    tenant: String,
    context_id: String,
    overrides: Map<String, Value>,
    description: String,
    change_reason: String,
    org_id: String,
) -> Result<Value, String> {
    let host = get_host();
    let url = format!("{host}/context/overrides");
    let request_payload = UpdateRequest {
        context: Identifier::Id(context_id),
        override_: Cac::<Overrides>::try_from(overrides)?,
        description: Description::try_from(description).map(Some)?,
        change_reason: ChangeReason::try_from(change_reason)?,
    };
    let response = request(
        url,
        reqwest::Method::PUT,
        Some(request_payload),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}
