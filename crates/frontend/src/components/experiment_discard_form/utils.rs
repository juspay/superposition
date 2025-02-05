use leptos::logging::log;
use serde_json::json;

use crate::{types::ExperimentResponse, utils::get_host};

pub async fn discard_experiment(
    exp_id: &String,
    tenant: &String,
    org_id: &String,
    change_reason: &String,
) -> Result<ExperimentResponse, String> {
    let client = reqwest::Client::new();
    let host = get_host();
    match client
        .patch(format!("{host}/experiments/{}/discard", exp_id))
        .header("x-tenant", tenant)
        .header("x-org-id", org_id)
        .json(&json!({"change_reason": change_reason }))
        .send()
        .await
    {
        Ok(experiment) => {
            log!("experiment response {:?}", experiment);
            Ok(experiment
                .json::<ExperimentResponse>()
                .await
                .map_err(|err| err.to_string())?)
        }
        Err(e) => Err(e.to_string()),
    }
}
