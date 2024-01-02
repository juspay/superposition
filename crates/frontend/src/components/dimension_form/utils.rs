use super::types::DimensionCreateReq;
use leptos::logging;
use reqwest::StatusCode;
use serde_json::Value;

pub fn parse_string_to_json_value_vec(input: &str) -> Vec<Value> {
    // Parse the input string into a serde_json::Value
    let parsed = serde_json::from_str::<Value>(input);

    // Ensure the Value is an Array and convert it to Vec<Value>
    match parsed {
        Ok(Value::Array(arr)) => arr,
        _ => {
            logging::log!("Not a valid json in the input");
            vec![]
        }
    }
}

pub async fn create_dimension(
    tenant: String,
    payload: DimensionCreateReq,
) -> Result<String, String> {
    let client = reqwest::Client::new();
    let host = "http://localhost:8080";
    let url = format!("{host}/dimension");

    let response = client
        .put(url)
        .header("x-tenant", tenant)
        .header("Authorization", "Bearer 12345678")
        .json(&payload)
        .send()
        .await
        .map_err(|e| e.to_string())?;
    match response.status() {
        StatusCode::OK => response.text().await.map_err(|e| e.to_string()),
        StatusCode::CREATED => response.text().await.map_err(|e| e.to_string()),
        StatusCode::BAD_REQUEST => Err("Schema Validation Failed".to_string()),
        _ => Err("Internal Server Error".to_string()),
    }
}