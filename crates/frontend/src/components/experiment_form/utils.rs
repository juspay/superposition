use serde_json::Value;
use superposition_types::cac::types::DimensionWithMandatory;

use crate::components::condition_pills::types::Condition;
use crate::components::context_form::utils::construct_context;
use crate::types::VariantFormT;
use crate::utils::{construct_request_headers, get_host, parse_json_response, request};

use super::types::{ExperimentCreateRequest, ExperimentUpdateRequest};

pub fn validate_experiment(experiment: &ExperimentCreateRequest) -> Result<bool, String> {
    if experiment.name.is_empty() {
        return Err(String::from("experiment name should not be empty"));
    }
    Ok(true)
}

pub async fn create_experiment(
    conditions: Vec<Condition>,
    variants: Vec<VariantFormT>,
    name: String,
    tenant: String,
    dimensions: Vec<DimensionWithMandatory>,
) -> Result<Value, String> {
    let payload = ExperimentCreateRequest {
        name,
        variants: FromIterator::from_iter(variants),
        context: construct_context(conditions, dimensions.clone()),
    };

    let _ = validate_experiment(&payload)?;

    let host = get_host();
    let url = format!("{host}/experiments");
    let response = request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn update_experiment(
    experiment_id: String,
    variants: Vec<VariantFormT>,
    tenant: String,
) -> Result<Value, String> {
    let payload = ExperimentUpdateRequest {
        variants: FromIterator::from_iter(variants),
    };

    let host = get_host();
    let url = format!("{}/experiments/{}/overrides", host, experiment_id);

    let response = request(
        url,
        reqwest::Method::PUT,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await?;

    parse_json_response(response).await
}
