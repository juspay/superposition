use super::types::ContextBulkResponse;
use reqwest::{Response, StatusCode};
use superposition_macros::{response_error, unexpected_error};
use superposition_types::{
    database::models::cac::Context as ContextResp, result as superposition, Config,
};

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

pub async fn process_cac_get_context_http_response(
    response: Result<Response, reqwest::Error>,
) -> superposition::Result<ContextResp> {
    let internal_server_error = unexpected_error!("Something went wrong.");
    match response {
        Ok(res) if res.status().is_success() => {
            let context_resp = res.json::<ContextResp>().await.map_err(|err| {
                log::error!("failed to parse JSON response with error: {}", err);
                internal_server_error
            })?;
            Ok(context_resp)
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

pub async fn process_cac_get_config_http_response(
    response: Result<Response, reqwest::Error>,
) -> superposition::Result<Config> {
    let internal_server_error = unexpected_error!("Something went wrong.");
    match response {
        Ok(res) if res.status().is_success() => {
            let config_resp = res.json::<Config>().await.map_err(|err| {
                log::error!("failed to parse JSON response with error: {}", err);
                internal_server_error
            })?;
            Ok(config_resp)
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
                .and_then(|val| val.to_str().map_or(None, |v| Some(v.to_string())));
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
