use super::types::DefaultConfigCreateReq;
use crate::utils::get_host;
use reqwest::StatusCode;

pub async fn create_default_config(
    key: String,
    tenant: String,
    payload: DefaultConfigCreateReq,
) -> Result<String, String> {
    let client = reqwest::Client::new();
    let host = get_host();
    let url = format!("{host}/default-config/{key}");

    let response = client
        .put(url)
        .header("x-tenant", tenant)
        .json(&payload)
        .send()
        .await
        .map_err(|e| e.to_string())?;
    match response.status() {
        StatusCode::OK | StatusCode::CREATED => {
            response.text().await.map_err(|e| e.to_string())
        }
        StatusCode::BAD_REQUEST => Err(response
            .text()
            .await
            .unwrap_or("Validation of configuration value failed, but the error could not be understood by the system. Contact an admin for help if this persists".to_string())),
        _ => Err("Internal Server Error".to_string()),
    }
}
