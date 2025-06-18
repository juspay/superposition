use serde_json::Value;
use superposition_types::{
    api::{
        experiments::{
            ExperimentCreateRequest, ExperimentResponse, OverrideKeysUpdateRequest,
            VariantUpdateRequest,
        },
        option_i64_from_value,
    },
    database::models::{
        experimentation::{ExperimentType, Variant},
        ChangeReason, Description, Metrics,
    },
    Condition, Exp,
};

use crate::logic::Conditions;
use crate::types::VariantFormT;
use crate::utils::{
    construct_request_headers, parse_json_response, request, use_host_server,
};

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
    experiment_type: ExperimentType,
    tenant: String,
    description: String,
    change_reason: String,
    org_id: String,
    experiment_group_id: Value,
) -> Result<ExperimentResponse, String> {
    let payload = ExperimentCreateRequest {
        name,
        experiment_type,
        variants: Result::<Vec<Variant>, String>::from_iter(variants)?,
        context: Exp::<Condition>::try_from(conditions.as_context_json())?,
        metrics,
        description: Description::try_from(description)?,
        change_reason: ChangeReason::try_from(change_reason)?,
        experiment_group_id: option_i64_from_value(experiment_group_id)
            .map_err(|e| format!("Invalid experiment group id: {e}"))?,
    };

    validate_experiment(&payload)?;

    let host = use_host_server();
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

#[allow(clippy::too_many_arguments)]
pub fn try_update_payload(
    variants: Vec<VariantFormT>,
    metrics: Option<Metrics>,
    description: String,
    change_reason: String,
    experiment_group_id: Value,
) -> Result<OverrideKeysUpdateRequest, String> {
    Ok(OverrideKeysUpdateRequest {
        variants: Result::<Vec<VariantUpdateRequest>, String>::from_iter(variants)?,
        metrics,
        description: Some(Description::try_from(description)?),
        change_reason: ChangeReason::try_from(change_reason)?,
        experiment_group_id: Some(
            serde_json::from_value(experiment_group_id)
                .map_err(|e| format!("Invalid experiment group id: {e}"))?,
        ),
    })
}

pub async fn update_experiment(
    experiment_id: &String,
    payload: OverrideKeysUpdateRequest,
    tenant: String,
    org_id: String,
) -> Result<ExperimentResponse, String> {
    let host = use_host_server();
    let url = format!("{}/experiments/{}/overrides", host, experiment_id);

    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}
