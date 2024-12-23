use leptos::logging::log;
use serde_json::json;

use crate::{types::ExperimentResponse, utils::get_host};

pub async fn ramp_experiment(
    exp_id: &String,
    percent: u8,
    tenant: &String,
    org_id: &String,
) -> Result<ExperimentResponse, String> {
    let client = reqwest::Client::new();
    let host = get_host();
    match client
        .patch(format!("{host}/experiments/{}/ramp", exp_id))
        .header("x-tenant", tenant)
        .header("x-org-id", org_id)
        .json(&json!({ "traffic_percentage": percent }))
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
