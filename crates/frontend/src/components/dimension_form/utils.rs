use super::types::DimensionCreateReq;
use crate::{
    types::Dimension,
    utils::{construct_request_headers, get_host, parse_json_response, request},
};

pub async fn create_dimension(
    tenant: String,
    payload: DimensionCreateReq,
) -> Result<Dimension, String> {
    let host = get_host();
    let url = format!("{host}/dimension");

    let response = request(
        url,
        reqwest::Method::PUT,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await?;

    parse_json_response(response).await
}
