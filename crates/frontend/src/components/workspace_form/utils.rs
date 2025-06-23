use crate::utils::{construct_request_headers, get_host, parse_json_response, request};
use serde_json::Value;
use superposition_types::{
    api::workspace::{CreateWorkspaceRequest, UpdateWorkspaceRequest},
    database::models::{Metrics, WorkspaceStatus},
};

pub async fn create_workspace(
    org_id: String,
    payload: CreateWorkspaceRequest,
) -> Result<serde_json::Value, String> {
    let host = get_host();
    let url = format!("{host}/workspaces");

    let response = request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-org-id", org_id.as_str())])?,
    )
    .await?;

    parse_json_response(response).await
}

#[allow(clippy::too_many_arguments)]
pub async fn update_workspace(
    key: String,
    org_id: String,
    workspace_admin_email: String,
    config_version: Value,
    workspace_status: WorkspaceStatus,
    mandatory_dimensions: Vec<String>,
    metrics: Metrics,
    allow_experiment_self_approval: bool,
) -> Result<serde_json::Value, String> {
    let payload = UpdateWorkspaceRequest {
        workspace_admin_email,
        config_version: Some(
            serde_json::from_value(config_version)
                .map_err(|e| format!("Invalid config version: {}", e))?,
        ),
        workspace_status: Some(workspace_status),
        mandatory_dimensions: Some(mandatory_dimensions),
        metrics: Some(metrics),
        allow_experiment_self_approval: Some(allow_experiment_self_approval),
    };
    let host = get_host();
    let url = format!("{host}/workspaces/{key}");

    let response = request(
        url,
        reqwest::Method::PUT,
        Some(payload),
        construct_request_headers(&[("x-org-id", org_id.as_str())])?,
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
