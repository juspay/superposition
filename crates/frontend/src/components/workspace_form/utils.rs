use super::types::{CreateWorkspaceRequest, UpdateWorkspaceRequest};
use crate::utils::{construct_request_headers, get_host, parse_json_response, request};
use serde_json::Value;

pub async fn create_workspace(
    payload: CreateWorkspaceRequest,
) -> Result<serde_json::Value, String> {
    let host = get_host();
    let url = format!("{host}/workspaces");

    let response = request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn update_workspace(
    key: String,
    payload: UpdateWorkspaceRequest,
) -> Result<serde_json::Value, String> {
    let host = get_host();
    let url = format!("{host}/workspaces/{key}");

    let response = request(
        url,
        reqwest::Method::PUT,
        Some(payload),
        construct_request_headers(&[])?,
    )
    .await?;

    parse_json_response(response).await
}

pub fn string_to_vec(input: &str) -> Result<Vec<String>, String> {
    let value: Value = serde_json::from_str(input)
        .map_err(|e| format!("Failed to parse JSON: {}", e))?;
    match value {
        Value::Array(arr) => arr
            .into_iter()
            .map(|v| {
                v.as_str()
                    .map(|s| s.to_string())
                    .ok_or_else(|| "Non-string element in array".to_string())
            })
            .collect(),
        _ => Err("Input is not a JSON array".to_string()),
    }
}
