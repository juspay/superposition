use superposition_types::{
    api::experiments::{
        ExperimentCreateRequest, ExperimentResponse, OverrideKeysUpdateRequest,
        VariantUpdateRequest,
    },
    database::models::{
        experimentation::{ExperimentType, Variant},
        ChangeReason, Description, Metrics,
    },
    Condition, Exp,
};

use crate::logic::Conditions;
use crate::types::VariantFormT;
use crate::utils::{construct_request_headers, get_host, parse_json_response, request};

pub fn validate_experiment(experiment: &ExperimentCreateRequest) -> Result<(), String> {
    if experiment.name.is_empty() {
        return Err(String::from("experiment name should not be empty"));
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub async fn create_experiment(
    conditions: Conditions,
    variants: Vec<VariantFormT>,
    metrics: Option<Metrics>,
    name: String,
    tenant: String,
    description: String,
    change_reason: String,
    org_id: String,
) -> Result<ExperimentResponse, String> {
    let payload = ExperimentCreateRequest {
        name,
        experiment_type: ExperimentType::Default,
        variants: Result::<Vec<Variant>, String>::from_iter(variants)?,
        context: Exp::<Condition>::try_from(conditions.as_context_json())?,
        metrics,
        description: Description::try_from(description)?,
        change_reason: ChangeReason::try_from(change_reason)?,
    };

    validate_experiment(&payload)?;

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
    metrics: Option<Metrics>,
    tenant: String,
    org_id: String,
    description: String,
    change_reason: String,
) -> Result<ExperimentResponse, String> {
    let payload = OverrideKeysUpdateRequest {
        variants: Result::<Vec<VariantUpdateRequest>, String>::from_iter(variants)?,
        metrics,
        description: Some(Description::try_from(description)?),
        change_reason: ChangeReason::try_from(change_reason)?,
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
