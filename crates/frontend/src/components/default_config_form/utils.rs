use serde_json::Value;
use superposition_types::{
    api::default_config::{
        DefaultConfigCreateRequest, DefaultConfigKey, DefaultConfigUpdateRequest,
    },
    database::models::{ChangeReason, Description},
    ExtendedMap,
};

use crate::utils::{
    construct_request_headers, parse_json_response, request, use_host_server,
};

#[allow(clippy::too_many_arguments)]
pub async fn create_default_config(
    key: String,
    value: Value,
    schema: Value,
    value_validation_function_name: Option<String>,
    value_compute_function_name: Option<String>,
    description: String,
    change_reason: String,
    workspace: &str,
    org_id: &str,
) -> Result<serde_json::Value, String> {
    let payload = DefaultConfigCreateRequest {
        key: DefaultConfigKey::try_from(key)?,
        schema: ExtendedMap::try_from(schema)?,
        value,
        value_validation_function_name,
        description: Description::try_from(description)?,
        change_reason: ChangeReason::try_from(change_reason)?,
        value_compute_function_name,
    };
    let host = use_host_server();
    let url = format!("{host}/default-config");

    let response = request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub fn try_update_payload(
    value: Value,
    schema: Value,
    value_validation_function_name: Option<String>,
    value_compute_function_name: Option<String>,
    description: String,
    change_reason: String,
) -> Result<DefaultConfigUpdateRequest, String> {
    Ok(DefaultConfigUpdateRequest {
        schema: Some(ExtendedMap::try_from(schema)?),
        value: Some(value),
        value_validation_function_name: Some(value_validation_function_name),
        description: Some(Description::try_from(description)?),
        change_reason: ChangeReason::try_from(change_reason)?,
        value_compute_function_name: Some(value_compute_function_name),
    })
}

pub async fn update_default_config(
    key: String,
    update_payload: DefaultConfigUpdateRequest,
    workspace: &str,
    org_id: &str,
) -> Result<serde_json::Value, String> {
    let host = use_host_server();
    let url = format!("{host}/default-config/{key}");

    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(update_payload),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}
