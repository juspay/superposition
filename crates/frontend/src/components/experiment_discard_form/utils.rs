use superposition_types::{
    api::experiments::{ExperimentResponse, ExperimentStateChangeRequest},
    database::models::ChangeReason,
};

use crate::utils::{construct_request_headers, get_host, parse_json_response, request};

pub async fn discard_experiment(
    exp_id: &str,
    tenant: &str,
    org_id: &str,
    change_reason: String,
) -> Result<ExperimentResponse, String> {
    let payload = ExperimentStateChangeRequest {
        change_reason: ChangeReason::try_from(change_reason)?,
    };

    let host = get_host();
    let url = format!("{host}/experiments/{exp_id}/discard");

    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-tenant", tenant), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}
