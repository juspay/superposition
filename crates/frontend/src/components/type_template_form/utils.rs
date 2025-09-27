use serde_json::Value;
use superposition_types::{
    api::type_templates::{
        TypeTemplateCreateRequest, TypeTemplateName, TypeTemplateUpdateRequest,
    },
    database::models::{cac::TypeTemplate, ChangeReason, Description},
    ExtendedMap,
};

use crate::utils::{construct_request_headers, get_host, request};

pub async fn create_type(
    type_name: String,
    type_schema: Value,
    description: String,
    change_reason: String,
    tenant: String,
    org_id: String,
) -> Result<TypeTemplate, String> {
    let payload = TypeTemplateCreateRequest {
        type_name: TypeTemplateName::try_from(type_name)?,
        type_schema: ExtendedMap::try_from(type_schema)?,
        description: Description::try_from(description)?,
        change_reason: ChangeReason::try_from(change_reason)?,
    };

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

pub fn try_update_payload(
    schema: Value,
    description: String,
    change_reason: String,
) -> Result<TypeTemplateUpdateRequest, String> {
    Ok(TypeTemplateUpdateRequest {
        type_schema: ExtendedMap::try_from(schema)?,
        description: Some(Description::try_from(description)?),
        change_reason: ChangeReason::try_from(change_reason)?,
    })
}
pub async fn update_type(
    type_name: String,
    payload: TypeTemplateUpdateRequest,
    tenant: String,
    org_id: String,
) -> Result<TypeTemplate, String> {
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
    type_name: String,
    tenant: String,
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
