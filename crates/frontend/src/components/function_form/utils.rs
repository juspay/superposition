use superposition_types::{
    api::functions::{FunctionExecutionRequest, FunctionExecutionResponse},
    database::models::cac::{Function, FunctionType},
};

use crate::utils::{construct_request_headers, get_host, parse_json_response, request};

use super::types::{FunctionCreateRequest, FunctionUpdateRequest};

#[allow(clippy::too_many_arguments)]
pub async fn create_function(
    function_name: String,
    function: String,
    runtime_version: String,
    description: String,
    change_reason: String,
    function_type: FunctionType,
    tenant: String,
    org_id: String,
) -> Result<Function, String> {
    let payload = FunctionCreateRequest {
        function_name,
        function,
        runtime_version,
        description,
        change_reason,
        function_type,
    };

    let host = get_host();
    let url = format!("{host}/function");
    let response = request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

#[allow(clippy::too_many_arguments)]
pub async fn update_function(
    function_name: String,
    function: String,
    runtime_version: String,
    description: String,
    change_reason: String,
    function_type: FunctionType,
    tenant: String,
    org_id: String,
) -> Result<Function, String> {
    let payload = FunctionUpdateRequest {
        function,
        runtime_version,
        description,
        change_reason,
        function_type,
    };

    let host = get_host();
    let url = format!("{host}/function/{function_name}");

    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;
    parse_json_response(response).await
}

pub async fn test_function(
    function_name: String,
    stage: String,
    function_args: &FunctionExecutionRequest,
    tenant: String,
    org_id: String,
) -> Result<FunctionExecutionResponse, String> {
    let host = get_host();
    let url = format!("{host}/function/{function_name}/{stage}/test");

    let response = request(
        url,
        reqwest::Method::PUT,
        Some(function_args),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}
