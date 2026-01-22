use superposition_types::{
    api::functions::{
        CreateFunctionRequest, FunctionExecutionRequest, FunctionExecutionResponse,
        FunctionName, Stage, UpdateFunctionRequest,
    },
    database::models::{
        ChangeReason, Description,
        cac::{Function, FunctionCode, FunctionRuntimeVersion, FunctionType},
    },
};

use crate::utils::{
    construct_request_headers, parse_json_response, request, use_host_server,
};

#[allow(clippy::too_many_arguments)]
pub async fn create_function(
    function_name: String,
    function: String,
    runtime_version: FunctionRuntimeVersion,
    description: String,
    change_reason: String,
    function_type: FunctionType,
    workspace: &str,
    org_id: &str,
) -> Result<Function, String> {
    let payload = CreateFunctionRequest {
        function_name: FunctionName::try_from(function_name)?,
        function: FunctionCode(function),
        runtime_version,
        description: Description::try_from(description)?,
        change_reason: ChangeReason::try_from(change_reason)?,
        function_type,
    };

    let host = use_host_server();
    let url = format!("{host}/function");
    let response = request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn update_function(
    function_name: String,
    function: String,
    runtime_version: FunctionRuntimeVersion,
    description: String,
    change_reason: String,
    workspace: &str,
    org_id: &str,
) -> Result<Function, String> {
    let payload = UpdateFunctionRequest {
        draft_code: Some(FunctionCode(function)),
        draft_runtime_version: Some(runtime_version),
        description: Some(Description::try_from(description)?),
        change_reason: ChangeReason::try_from(change_reason)?,
    };

    let host = use_host_server();
    let url = format!("{host}/function/{function_name}");

    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;
    parse_json_response(response).await
}

pub async fn test_function(
    function_name: String,
    stage: Stage,
    function_args: &FunctionExecutionRequest,
    workspace: &str,
    org_id: &str,
) -> Result<FunctionExecutionResponse, String> {
    let host = use_host_server();
    let url = format!("{host}/function/{function_name}/{stage}/test");

    let response = request(
        url,
        reqwest::Method::POST,
        Some(function_args),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}
