use anyhow::Result;
use serde_json::{Map, Value, json};
use superposition_types::{
    Cac, Overrides,
    api::context::{Identifier, UpdateRequest},
    database::models::{ChangeReason, Description, cac::Context},
};

use crate::utils::{
    construct_request_headers, parse_json_response, request_with_workspace_lock_retry,
    use_host_server,
};

use super::Conditions;

pub fn context_payload(
    overrides: Map<String, Value>,
    conditions: Conditions,
    description: String,
    change_reason: String,
) -> Value {
    let context = Map::from(conditions);
    let payload = json!({
        "override": overrides,
        "context": context,
        "description": description,
        "change_reason": change_reason,
    });

    payload
}

pub async fn create_context(
    overrides: Map<String, Value>,
    conditions: Conditions,
    description: String,
    change_reason: String,
    workspace: &str,
    org_id: &str,
) -> Result<Context, String> {
    let host = use_host_server();
    let url = format!("{host}/context");
    let request_payload =
        context_payload(overrides, conditions, description, change_reason);
    let response = request_with_workspace_lock_retry(
        url,
        reqwest::Method::PUT,
        Some(request_payload),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
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
    workspace: &str,
    org_id: &str,
) -> Result<Context, String> {
    let host = use_host_server();
    let url = format!("{host}/context/overrides");
    let response = request_with_workspace_lock_retry(
        url,
        reqwest::Method::PATCH,
        Some(request_payload),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

/// Trims a context's overrides down to the entries that actually matched the
/// free-text filter, so a card lists only the searched key instead of every
/// override sitting on that context. Matches against both the key and the
/// value, mirroring the server-side `override::text ILIKE '%..%'` filter.
///
/// Falls back to the untrimmed map when nothing matches, so a context the API
/// returned never renders with an empty override table.
pub fn filter_overrides_by_plaintext(
    overrides: Map<String, Value>,
    plaintext: Option<&String>,
) -> Map<String, Value> {
    let Some(plaintext) = plaintext
        .map(|p| p.trim().to_lowercase())
        .filter(|p| !p.is_empty())
    else {
        return overrides;
    };

    let filtered = overrides
        .iter()
        .filter(|(key, value)| {
            key.to_lowercase().contains(&plaintext)
                || value.to_string().to_lowercase().contains(&plaintext)
        })
        .map(|(key, value)| (key.clone(), value.clone()))
        .collect::<Map<String, Value>>();

    if filtered.is_empty() {
        overrides
    } else {
        filtered
    }
}
