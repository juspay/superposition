use superposition_types::{
    api::experiments::{ExperimentResponse, RampRequest},
    database::models::{experimentation::TrafficPercentage, ChangeReason},
};

use crate::utils::{construct_request_headers, get_host, parse_json_response, request};

pub async fn ramp_experiment(
    exp_id: &String,
    percent: u8,
    change_reason: Option<String>,
    tenant: &String,
    org_id: &String,
) -> Result<ExperimentResponse, String> {
    let payload = RampRequest {
        change_reason: ChangeReason::try_from(
            change_reason.unwrap_or(format!("ramping to {percent}")),
        )?,
        traffic_percentage: TrafficPercentage::try_from(percent as i32)?,
    };

    let host = get_host();
    let url = format!("{host}/experiments/{exp_id}/ramp");

    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-tenant", tenant), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}
