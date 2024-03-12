use super::types::{FunctionCreateRequest, FunctionUpdateRequest};
use crate::utils::get_host;
use reqwest::StatusCode;
use serde_json::{json, Value};

pub async fn create_function(
    function_name: String,
    function: String,
    runtime_version: String,
    description: String,
    tenant: String,
) -> Result<String, String> {
    let payload = FunctionCreateRequest {
        function_name,
        function,
        runtime_version,
        description,
    };

    let client = reqwest::Client::new();
    let host = get_host();
    let url = format!("{host}/function");
    let request_payload = json!(payload);
    let response = client
        .post(url)
        .header("x-tenant", tenant)
        .json(&request_payload)
        .send()
        .await
        .map_err(|e| e.to_string())?;

    let status = response.status();
    let resp_data = response
        .text()
        .await
        .unwrap_or("Cannot decode response".to_string());
    match status {
        StatusCode::OK => Ok(resp_data),
        _ => Err(resp_data),
    }
}

pub async fn update_function(
    function_name: String,
    function: String,
    runtime_version: String,
    description: String,
    tenant: String,
) -> Result<String, String> {
    let payload = FunctionUpdateRequest {
        function,
        runtime_version,
        description,
    };

    let client = reqwest::Client::new();
    let host = get_host();
    let url = format!("{host}/function/{function_name}");
    let request_payload = json!(payload);

    let response = client
        .patch(url)
        .header("x-tenant", tenant)
        .json(&request_payload)
        .send()
        .await
        .map_err(|e| e.to_string())?;

    let status = response.status();
    let resp_data = response
        .text()
        .await
        .unwrap_or("Cannot decode response".to_string());
    match status {
        StatusCode::OK => Ok(resp_data),
        _ => Err(resp_data),
    }
}

pub async fn test_function(
    function_name: String,
    stage: String,
    val: Value,
    tenant: String,
) -> Result<String, String> {
    let client = reqwest::Client::new();
    let host = get_host();
    let url = format!("{host}/function/{function_name}/{stage}/test");

    let response = client
        .put(url)
        .header("x-tenant", tenant)
        .json(&val)
        .send()
        .await
        .map_err(|e| e.to_string())?;

    let status = response.status();
    let resp_data = response
        .text()
        .await
        .unwrap_or("Cannot decode response".to_string());
    match status {
        StatusCode::OK => Ok(resp_data),
        _ => Err(resp_data),
    }
}
