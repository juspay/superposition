use superposition_types::database::models::cac::Dimension;

use crate::utils::{construct_request_headers, get_host, parse_json_response, request};

use super::types::{DimensionCreateReq, DimensionUpdateReq};

pub async fn create_dimension(
    tenant: String,
    payload: DimensionCreateReq,
    org_id: String,
) -> Result<Dimension, String> {
    let host = get_host();
    let url = format!("{host}/dimension");

    let response = request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn update_dimension(
    tenant: String,
    dimension_name: String,
    payload: DimensionUpdateReq,
    org_id: String,
) -> Result<Dimension, String> {
    let host = get_host();
    let url = format!("{host}/dimension/{dimension_name}");

    let response = request(
        url,
        reqwest::Method::PUT,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}
