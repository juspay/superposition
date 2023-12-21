use super::types::ExperimentCreateRequest;
use crate::components::context_form::utils::construct_context;
use crate::pages::ExperimentList::types::Variant;
use reqwest::StatusCode;
use serde_json::json;

pub async fn create_experiment(
    conditions: Vec<(String, String, String)>,
    variants: Vec<Variant>,
    name: String,
    tenant: String,
) -> Result<String, String> {
    let payload = ExperimentCreateRequest {
        name: name,
        variants: variants,
        context: construct_context(conditions),
    };

    let client = reqwest::Client::new();
    let host = "http://localhost:8080";
    let url = format!("{host}/experiments");
    let request_payload = json!(payload);
    let response = client
        .post(url)
        .header("x-tenant", tenant)
        .header("Authorization", "Bearer 12345678")
        .json(&request_payload)
        .send()
        .await
        .map_err(|e| e.to_string())?;
    match response.status() {
        StatusCode::OK => response.text().await.map_err(|e| e.to_string()),
        StatusCode::BAD_REQUEST => Err("epxeriment data corrupt".to_string()),
        _ => Err("Internal Server Error".to_string()),
    }
}
