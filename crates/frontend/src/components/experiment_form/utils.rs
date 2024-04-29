use super::types::{
    ExperimentCreateRequest, ExperimentUpdateRequest, VariantUpdateRequest,
};
use crate::components::context_form::utils::construct_context;
use crate::types::{Dimension, Variant};
use crate::utils::{construct_request_headers, get_host, request};
use serde_json::Value;

pub fn validate_experiment(experiment: &ExperimentCreateRequest) -> Result<bool, String> {
    if experiment.name.is_empty() {
        return Err(String::from("experiment name should not be empty"));
    }
    Ok(true)
}

pub async fn create_experiment(
    conditions: Vec<(String, String, String)>,
    variants: Vec<Variant>,
    name: String,
    tenant: String,
    dimensions: Vec<Dimension>,
) -> Result<Value, String> {
    let payload = ExperimentCreateRequest {
        name,
        variants,
        context: construct_context(conditions, dimensions),
    };

    let _ = validate_experiment(&payload)?;

    let host = get_host();
    let url = format!("{host}/experiments");
    request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await
}

pub async fn update_experiment(
    experiment_id: String,
    variants: Vec<Variant>,
    tenant: String,
) -> Result<Value, String> {
    let payload = ExperimentUpdateRequest {
        variants: variants
            .into_iter()
            .map(|variant| VariantUpdateRequest {
                id: variant.id,
                overrides: variant.overrides,
            })
            .collect::<Vec<VariantUpdateRequest>>(),
    };

    let host = get_host();
    let url = format!("{}/experiments/{}/overrides", host, experiment_id);

    request(
        url,
        reqwest::Method::PUT,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await
}
