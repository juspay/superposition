use std::vec::Vec;

use leptos::{server, ServerFnError};

use super::types::Dimension;

#[server(GetDimensions, "/fxn", "GetJson")]
pub async fn fetch_dimensions(tenant: String) -> Result<Vec<Dimension>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = "http://localhost:8080";

    let url = format!("{}/dimension", host);
    let response: Vec<Dimension> = client
        .get(url)
        .header("x-tenant", &tenant)
        .send()
        .await
        .map_err(|e| ServerFnError::ServerError(e.to_string()))?
        .json()
        .await
        .map_err(|e| ServerFnError::ServerError(e.to_string()))?;

    Ok(response)
}
