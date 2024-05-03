use super::types::{FunctionCreateRequest, FunctionUpdateRequest};
use crate::{
    types::{FunctionResponse, FunctionTestResponse},
    utils::{construct_request_headers, get_host, request},
};
use serde_json::Value;

pub async fn create_function(
    function_name: String,
    function: String,
    runtime_version: String,
    description: String,
    tenant: String,
) -> Result<FunctionResponse, String> {
    let payload = FunctionCreateRequest {
        function_name,
        function,
        runtime_version,
        description,
    };

    let host = get_host();
    let url = format!("{host}/function");
    request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await
}

pub async fn update_function(
    function_name: String,
    function: String,
    runtime_version: String,
    description: String,
    tenant: String,
) -> Result<FunctionResponse, String> {
    let payload = FunctionUpdateRequest {
        function,
        runtime_version,
        description,
    };

    let host = get_host();
    let url = format!("{host}/function/{function_name}");

    request(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await
}

pub async fn test_function(
    function_name: String,
    stage: String,
    val: Value,
    tenant: String,
) -> Result<FunctionTestResponse, String> {
    let host = get_host();
    let url = format!("{host}/function/{function_name}/{stage}/test");

    request(
        url,
        reqwest::Method::PUT,
        Some(val),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await
}
