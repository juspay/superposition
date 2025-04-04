use superposition_types::{
    api::experiments::{ConcludeExperimentRequest, ExperimentResponse},
    database::models::ChangeReason,
};

use crate::utils::{construct_request_headers, get_host, parse_json_response, request};

pub async fn conclude_experiment(
    exp_id: String,
    variant_id: String,
    tenant: &String,
    org_id: &String,
) -> Result<ExperimentResponse, String> {
    let payload = ConcludeExperimentRequest {
        change_reason: ChangeReason::try_from(String::from("concluding experiment"))?,
        chosen_variant: variant_id,
        description: None,
    };

    let host = get_host();
    let url = format!("{host}/experiments/{exp_id}/conclude");

    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-tenant", tenant), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}
