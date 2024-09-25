use leptos::logging::log;
use serde_json::json;

use crate::{types::ExperimentResponse, utils::get_host};

pub async fn conclude_experiment(
    exp_id: String,
    variant_id: String,
    tenant: &String,
) -> Result<ExperimentResponse, String> {
    let client = reqwest::Client::new();
    let host = get_host();
    match client
        .patch(format!("{host}/experiments/{}/conclude", exp_id))
        .header("x-tenant", tenant)
        .json(&json!({ "chosen_variant": variant_id }))
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
