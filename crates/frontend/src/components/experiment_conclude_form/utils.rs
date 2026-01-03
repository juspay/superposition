use reqwest::StatusCode;
use superposition_types::{
    api::experiments::{ConcludeExperimentRequest, ExperimentResponse},
    database::models::ChangeReason,
};

use crate::{
    components::alert::AlertType,
    providers::alert_provider::enqueue_alert,
    types::ErrorResponse,
    utils::{
        construct_request_headers, parse_json_response, request_with_skip_error,
        use_host_server,
    },
};

pub async fn conclude_experiment(
    exp_id: String,
    variant_id: String,
    change_reason: String,
    workspace: &str,
    org_id: &str,
) -> Result<Result<ExperimentResponse, String>, String> {
    let payload = ConcludeExperimentRequest {
        change_reason: ChangeReason::try_from(change_reason)?,
        chosen_variant: variant_id,
        description: None,
    };

    let host = use_host_server();
    let url = format!("{host}/experiments/{exp_id}/conclude");

    let response = request_with_skip_error(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
        &[StatusCode::PRECONDITION_FAILED],
    )
    .await?;

    if response.status() == StatusCode::PRECONDITION_FAILED {
        return Ok(Err(response
            .json::<ErrorResponse>()
            .await
            .map_err(|e| {
                let error_message = e.to_string();
                enqueue_alert(error_message.clone(), AlertType::Error, 5000);
                error_message
            })?
            .message));
    }

    Ok(parse_json_response(response).await)
}
