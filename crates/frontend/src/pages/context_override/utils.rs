use anyhow::Result;
use serde_json::{json, Map, Value};
use superposition_types::{
    api::context::{Identifier, UpdateRequest},
    database::models::{cac::Context, ChangeReason, Description},
    Cac, Overrides,
};

use crate::utils::{
    construct_request_headers, parse_json_response, request, use_host_server,
};

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
) -> Result<Context, String> {
    let host = use_host_server();
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

pub fn try_update_context_payload(
    context_id: String,
    overrides: Map<String, Value>,
    description: String,
    change_reason: String,
) -> Result<UpdateRequest, String> {
    Ok(UpdateRequest {
        context: Identifier::Id(context_id),
        override_: Cac::<Overrides>::try_from(overrides)?,
        description: Some(Description::try_from(description)?),
        change_reason: ChangeReason::try_from(change_reason)?,
    })
}

pub async fn update_context(
    request_payload: UpdateRequest,
    tenant: String,
    org_id: String,
) -> Result<Context, String> {
    let host = use_host_server();
    let url = format!("{host}/context/overrides");
    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(request_payload),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}
