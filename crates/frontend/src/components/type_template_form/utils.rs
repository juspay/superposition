use serde_json::Value;

use crate::utils::{construct_request_headers, get_host, request};

pub async fn create_update_type(tenant: String, payload: Value) -> Result<Value, String> {
    let host = get_host();
    let url = format!("{host}/types");

    request(
        url,
        reqwest::Method::PUT,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await
}

pub async fn delete_type(tenant: String, type_name: String) -> Result<Value, String> {
    let host = get_host();
    let url = format!("{host}/types/{type_name}");

    let payload: Option<()> = None;

    request(
        url,
        reqwest::Method::DELETE,
        payload,
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await
}
