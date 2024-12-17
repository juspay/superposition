use superposition_types::cac::models::Dimension;

use crate::utils::{construct_request_headers, get_host, parse_json_response, request};

use super::types::{DimensionCreateReq, DimensionUpdateReq};

pub async fn create_dimension(
    tenant: String,
    payload: DimensionCreateReq,
) -> Result<Dimension, String> {
    let host = get_host();
    let url = format!("{host}/dimension");

    let response = request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn update_dimension(
    tenant: String,
    dimension_name: String,
    payload: DimensionUpdateReq,
) -> Result<Dimension, String> {
    let host = get_host();
    let url = format!("{host}/dimension/{dimension_name}");

    let response = request(
        url,
        reqwest::Method::PUT,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await?;

    parse_json_response(response).await
}
