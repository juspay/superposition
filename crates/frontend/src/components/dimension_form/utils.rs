use serde_json::Value;
use superposition_types::{
    api::dimension::{CreateRequest, DimensionName, DimensionResponse, UpdateRequest},
    database::models::{cac::Position, ChangeReason, Description},
};

use crate::utils::{construct_request_headers, get_host, parse_json_response, request};

#[allow(clippy::too_many_arguments)]
pub async fn create_dimension(
    dimension: String,
    position: u32,
    schema: Value,
    dependencies: Vec<String>,
    validation_fn_name: Option<String>,
    autocomplete_fn_name: Option<String>,
    description: String,
    change_reason: String,
    tenant: String,
    org_id: String,
) -> Result<DimensionResponse, String> {
    let payload = CreateRequest {
        dimension: DimensionName::try_from(dimension)?,
        position: Position::from(position),
        schema,
        dependencies: Some(dependencies),
        function_name: validation_fn_name,
        autocomplete_function_name: autocomplete_fn_name,
        description: Description::try_from(description)?,
        change_reason: ChangeReason::try_from(change_reason)?,
    };

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

pub fn try_update_payload(
    position: u32,
    schema: Value,
    dependencies: Vec<String>,
    validation_fn_name: Option<String>,
    autocomplete_fn_name: Option<String>,
    description: String,
    change_reason: String,
) -> Result<UpdateRequest, String> {
    Ok(UpdateRequest {
        position: Some(Position::from(position)),
        schema: Some(schema),
        dependencies: Some(dependencies),
        function_name: Some(validation_fn_name),
        autocomplete_function_name: Some(autocomplete_fn_name),
        description: Some(Description::try_from(description)?),
        change_reason: ChangeReason::try_from(change_reason)?,
    })
}

pub async fn update_dimension(
    tenant: String,
    dimension_name: String,
    payload: UpdateRequest,
    org_id: String,
) -> Result<DimensionResponse, String> {
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
