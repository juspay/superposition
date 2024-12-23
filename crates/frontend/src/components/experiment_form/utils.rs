use super::types::{ExperimentCreateRequest, ExperimentUpdateRequest};
use crate::logic::Conditions;
use crate::types::VariantFormT;
use crate::utils::{construct_request_headers, get_host, parse_json_response, request};

pub fn validate_experiment(experiment: &ExperimentCreateRequest) -> Result<bool, String> {
    if experiment.name.is_empty() {
        return Err(String::from("experiment name should not be empty"));
    }
    Ok(true)
}

pub async fn create_experiment(
    conditions: Conditions,
    variants: Vec<VariantFormT>,
    name: String,
    tenant: String,
    description: String,
    change_reason: String,
    org_id: String,
) -> Result<serde_json::Value, String> {
    let payload = ExperimentCreateRequest {
        name,
        variants: FromIterator::from_iter(variants),
        context: conditions.to_context_json(),
        description,
        change_reason,
    };

    let _ = validate_experiment(&payload)?;

    let host = get_host();
    let url = format!("{host}/experiments");
    let response = request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn update_experiment(
    experiment_id: String,
    variants: Vec<VariantFormT>,
    tenant: String,
    org_id: String,
) -> Result<serde_json::Value, String> {
    let payload = ExperimentUpdateRequest {
        variants: FromIterator::from_iter(variants),
    };

    let host = get_host();
    let url = format!("{}/experiments/{}/overrides", host, experiment_id);

    let response = request(
        url,
        reqwest::Method::PUT,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}
