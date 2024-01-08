#![allow(non_snake_case)]

use crate::utils::get_host;

use self::types::Config;

pub mod ContextOverride;
pub mod DefaultConfig;
pub mod Dimensions;
pub mod Experiment;
pub mod ExperimentList;
pub mod Home;
pub mod NotFound;
pub mod types;

// Utils segments found here

pub async fn fetch_config(tenant: String) -> Result<Config, String> {
    let client = reqwest::Client::new();
    let host = get_host();
    let url = format!("{host}/config");
    match client.get(url).header("x-tenant", tenant).send().await {
        Ok(response) => {
            let config: Config = response.json().await.map_err(|e| e.to_string())?;
            Ok(config)
        }
        Err(e) => Err(e.to_string()),
    }
}
