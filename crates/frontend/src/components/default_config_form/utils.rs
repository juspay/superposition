use serde_json::Value;
use superposition_types::{
    api::default_config::{
        DefaultConfigCreateRequest, DefaultConfigKey, DefaultConfigUpdateRequest,
    },
    database::models::{ChangeReason, Description},
    ExtendedMap,
};

use crate::utils::{construct_request_headers, get_host, parse_json_response, request};

#[allow(clippy::too_many_arguments)]
pub async fn create_default_config(
    tenant: String,
    org_id: String,
    key: String,
    value: Value,
    schema: Value,
    function_name: Option<String>,
    autocomplete_function_name: Option<String>,
    description: String,
    change_reason: String,
) -> Result<serde_json::Value, String> {
    let payload = DefaultConfigCreateRequest {
        key: DefaultConfigKey::try_from(key)?,
        schema: ExtendedMap::try_from(schema)?,
        value,
        function_name,
        description: Description::try_from(description)?,
        change_reason: ChangeReason::try_from(change_reason)?,
        autocomplete_function_name,
    };
    let host = get_host();
    let url = format!("{host}/default-config");

    let response = request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub fn try_update_payload(
    value: Value,
    schema: Value,
    function_name: Option<String>,
    autocomplete_function_name: Option<String>,
    description: String,
    change_reason: String,
) -> Result<DefaultConfigUpdateRequest, String> {
    Ok(DefaultConfigUpdateRequest {
        schema: Some(ExtendedMap::try_from(schema)?),
        value: Some(value),
        function_name: Some(function_name),
        description: Some(Description::try_from(description)?),
        change_reason: ChangeReason::try_from(change_reason)?,
        autocomplete_function_name: Some(autocomplete_function_name),
    })
}

pub async fn update_default_config(
    key: String,
    update_payload: DefaultConfigUpdateRequest,
    tenant: &str,
    org_id: &str,
) -> Result<serde_json::Value, String> {
    let host = get_host();
    let url = format!("{host}/default-config/{key}");

    let response = request(
        url,
        reqwest::Method::PUT,
        Some(update_payload),
        construct_request_headers(&[("x-tenant", tenant), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}
