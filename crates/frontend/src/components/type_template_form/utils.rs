use serde_json::Value;

use crate::utils::{construct_request_headers, get_host, request};

pub async fn create_type(
    tenant: String,
    payload: Value,
    org_id: String,
) -> Result<Value, String> {
    let host = get_host();
    let url = format!("{host}/types");

    let response = request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;
    response.json().await.map_err(|e| e.to_string())
}

pub async fn update_type(
    tenant: String,
    type_name: String,
    payload: Value,
    org_id: String,
) -> Result<Value, String> {
    let host = get_host();
    let url = format!("{host}/types/{type_name}");

    let response = request(
        url,
        reqwest::Method::PUT,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;
    response.json().await.map_err(|e| e.to_string())
}

pub async fn delete_type(
    tenant: String,
    type_name: String,
    org_id: String,
) -> Result<Value, String> {
    let host = get_host();
    let url = format!("{host}/types/{type_name}");

    let payload: Option<()> = None;

    let response = request(
        url,
        reqwest::Method::DELETE,
        payload,
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;
    response.json().await.map_err(|e| e.to_string())
}
