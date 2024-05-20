use serde_json::Value;

use crate::utils::{construct_request_headers, get_host, request};

pub async fn create_type(tenant: String, payload: Value) -> Result<Value, String> {
    let host = get_host();
    let url = format!("{host}/types");

    let response = request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await?;
    response.json().await.map_err(|e| e.to_string())
}

pub async fn update_type(
    tenant: String,
    type_name: String,
    payload: Value,
) -> Result<Value, String> {
    let host = get_host();
    let url = format!("{host}/types/{type_name}");

    let response = request(
        url,
        reqwest::Method::PUT,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await?;
    response.json().await.map_err(|e| e.to_string())
}

pub async fn delete_type(tenant: String, type_name: String) -> Result<Value, String> {
    let host = get_host();
    let url = format!("{host}/types/{type_name}");

    let payload: Option<()> = None;

    let response = request(
        url,
        reqwest::Method::DELETE,
        payload,
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await?;
    response.json().await.map_err(|e| e.to_string())
}
