use actix_http::header::{self, HeaderMap, HeaderName, HeaderValue};
use actix_web::web::Data;
use reqwest::{Response, StatusCode};
use serde::de::DeserializeOwned;
use serde_json::{Map, Value};
use service_utils::{
    helpers::extract_dimensions,
    service::types::{AppState, OrganisationId, WorkspaceContext, WorkspaceId},
};
use std::str::FromStr;
use superposition_macros::{response_error, unexpected_error};
use superposition_types::{
    database::models::cac::Context as ContextResp, result as superposition, Condition,
    User,
};

use super::types::ContextBulkResponse;

pub fn construct_header_map(
    workspace_id: &WorkspaceId,
    organisation_id: &OrganisationId,
    other_headers: Vec<(&str, String)>,
) -> superposition::Result<HeaderMap> {
    let mut headers = HeaderMap::new();
    let workspace_val = HeaderValue::from_str(workspace_id).map_err(|err| {
        log::error!("failed to set header: {}", err);
        unexpected_error!("Something went wrong")
    })?;
    headers.insert(HeaderName::from_static("x-tenant"), workspace_val);

    let org_val = HeaderValue::from_str(organisation_id).map_err(|err| {
        log::error!("failed to set header: {}", err);
        unexpected_error!("Something went wrong")
    })?;
    headers.insert(HeaderName::from_static("x-org-id"), org_val);

    for (header, value) in other_headers {
        let header_name = HeaderName::from_str(header).map_err(|err| {
            log::error!("failed to set header: {}", err);
            unexpected_error!("Something went wrong")
        })?;

        HeaderValue::from_str(value.as_str())
            .map(|header_val| headers.insert(header_name, header_val))
            .map_err(|err| {
                log::error!("failed to set header: {}", err);
                unexpected_error!("Something went wrong")
            })?;
    }

    Ok(headers)
}

pub async fn parse_error_response(
    response: reqwest::Response,
) -> superposition::Result<(StatusCode, superposition::ErrorResponse)> {
    let status_code = response.status();
    let error_response = response
        .json::<superposition::ErrorResponse>()
        .await
        .map_err(|err: reqwest::Error| {
            log::error!("failed to parse error response: {}", err);
            unexpected_error!("Something went wrong")
        })?;
    log::error!("http call to CAC failed with err {:?}", error_response);

    Ok((status_code, error_response))
}

pub async fn process_cac_http_response<T: DeserializeOwned>(
    response: Result<Response, reqwest::Error>,
) -> superposition::Result<T> {
    let internal_server_error = unexpected_error!("Something went wrong.");
    match response {
        Ok(res) if res.status().is_success() => {
            let ok_resp = res.json::<T>().await.map_err(|err| {
                log::error!("failed to parse JSON response with error: {}", err);
                internal_server_error
            })?;
            Ok(ok_resp)
        }
        Ok(res) => {
            log::error!("http call to CAC failed with status_code {}", res.status());

            if res.status().is_client_error() {
                let (status_code, error_response) = parse_error_response(res).await?;
                Err(response_error!(status_code, error_response.message))
            } else {
                Err(internal_server_error)
            }
        }
        Err(err) => {
            log::error!("reqwest failed to send request to CAC with error: {}", err);
            Err(internal_server_error)
        }
    }
}

pub async fn process_cac_bulk_operation_http_response(
    response: Result<Response, reqwest::Error>,
) -> superposition::Result<(Vec<ContextBulkResponse>, Option<String>)> {
    let internal_server_error = unexpected_error!("Something went wrong.");
    match response {
        Ok(res) if res.status().is_success() => {
            let config_version = res
                .headers()
                .get("x-config-version")
                .and_then(|val| val.to_str().ok().map(String::from));
            let bulk_resp =
                res.json::<Vec<ContextBulkResponse>>()
                    .await
                    .map_err(|err| {
                        log::error!("failed to parse JSON response with error: {}", err);
                        internal_server_error
                    })?;
            Ok((bulk_resp, config_version))
        }
        Ok(res) => {
            log::error!("http call to CAC failed with status_code {}", res.status());

            if res.status().is_client_error() {
                let (status_code, error_response) = parse_error_response(res).await?;
                Err(response_error!(status_code, error_response.message))
            } else {
                Err(internal_server_error)
            }
        }
        Err(err) => {
            log::error!("reqwest failed to send request to CAC with error: {}", err);
            Err(internal_server_error)
        }
    }
}

pub async fn get_partial_resolve_config(
    user: &User,
    state: &Data<AppState>,
    exp_context: &Condition,
    context_id: &str,
    workspace_request: &WorkspaceContext,
) -> superposition::Result<Map<String, Value>> {
    let http_client = reqwest::Client::new();
    let url = state.cac_host.clone() + "/config/resolve";
    let mut exp_context_dimension_value = extract_dimensions(exp_context)?;
    exp_context_dimension_value.insert(
        "context_id".to_string(),
        Value::String(context_id.to_string()),
    );

    let user_str = serde_json::to_string(user).map_err(|err| {
        log::error!("Something went wrong, failed to stringify user data {err}");
        unexpected_error!(
            "Something went wrong, failed to stringify user data {}",
            err
        )
    })?;

    let extra_headers = vec![("x-user", user_str)];

    let headers_map = construct_header_map(
        &workspace_request.workspace_id,
        &workspace_request.organisation_id,
        extra_headers,
    )?;
    let response = http_client
        .get(&url)
        .query(&exp_context_dimension_value)
        .headers(headers_map.into())
        .header(
            header::AUTHORIZATION,
            format!("Internal {}", state.superposition_token),
        )
        .send()
        .await;

    process_cac_http_response(response).await
}

pub async fn get_context_override(
    user: &User,
    state: &Data<AppState>,
    workspace_request: &WorkspaceContext,
    context_id: String,
) -> superposition::Result<ContextResp> {
    let http_client = reqwest::Client::new();
    let url = state.cac_host.clone() + "/context/" + &context_id;
    let user_str = serde_json::to_string(user).map_err(|err| {
        log::error!("Something went wrong, failed to stringify user data {err}");
        unexpected_error!(
            "Something went wrong, failed to stringify user data {}",
            err
        )
    })?;

    let extra_headers = vec![("x-user", user_str)];

    let headers_map = construct_header_map(
        &workspace_request.workspace_id,
        &workspace_request.organisation_id,
        extra_headers,
    )?;
    let response = http_client
        .get(&url)
        .headers(headers_map.into())
        .header(
            header::AUTHORIZATION,
            format!("Internal {}", state.superposition_token),
        )
        .send()
        .await;
    let resp_contexts = process_cac_http_response(response).await.map_err(|err| {
        log::error!("Failed to fetch context during cac http call");
        match err {
            superposition::AppError::ResponseError(val) if val.status_code == StatusCode::NOT_FOUND => {
                response_error!(StatusCode::PRECONDITION_FAILED,format!("Context not found in CAC for given experiment, you should discard this experiment: {}", val.message))
            }
            _ => err,
        }
    })?;
    Ok(resp_contexts)
}
