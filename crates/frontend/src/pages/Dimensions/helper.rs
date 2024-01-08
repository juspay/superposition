use std::vec::Vec;

use crate::utils::get_host;

use super::types::Dimension;

pub async fn fetch_dimensions(tenant: &str) -> Result<Vec<Dimension>, String> {
    let client = reqwest::Client::new();
    let host = get_host();

    let url = format!("{}/dimension", host);
    let response: Vec<Dimension> = client
        .get(url)
        .header("x-tenant", tenant)
        .send()
        .await
        .map_err(|e| e.to_string())?
        .json()
        .await
        .map_err(|e| e.to_string())?;

    Ok(response)
}
