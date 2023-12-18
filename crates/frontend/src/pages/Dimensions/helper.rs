use std::vec::Vec;

use super::types::Dimension;

pub async fn fetch_dimensions(tenant: &str) -> Result<Vec<Dimension>, String> {
    let client = reqwest::Client::new();
    let host = match std::env::var("APP_ENV").as_deref() {
        Ok("PROD") => {
            "https://context-aware-config.sso.internal.svc.k8s.apoc.mum.juspay.net"
        }
        Ok("SANDBOX") => "https://context-aware.internal.staging.mum.juspay.net",
        _ => "http://localhost:8080",
    };

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
