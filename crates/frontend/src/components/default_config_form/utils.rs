use super::types::DefaultConfigCreateReq;
use crate::utils::{construct_request_headers, get_host, parse_json_response, request};

pub async fn create_default_config(
    key: String,
    tenant: String,
    payload: DefaultConfigCreateReq,
) -> Result<serde_json::Value, String> {
    let host = get_host();
    let url = format!("{host}/default-config/{key}");

    let response = request(
        url,
        reqwest::Method::PUT,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await?;

    parse_json_response(response).await
}
