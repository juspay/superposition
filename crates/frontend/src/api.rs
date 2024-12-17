use leptos::ServerFnError;
use superposition_types::{
    cac::{
        models::{ConfigVersion, DefaultConfig, Function, TypeTemplate},
        types::DimensionWithMandatory,
    },
    custom_query::PaginationParams,
    Config, PaginatedResponse,
};

use crate::{
    types::{ExperimentListFilters, ExperimentResponse},
    utils::{
        construct_request_headers, get_host, parse_json_response, request,
        use_host_server,
    },
};

// #[server(GetDimensions, "/fxn", "GetJson")]
pub async fn fetch_dimensions(
    filters: &PaginationParams,
    tenant: String,
) -> Result<PaginatedResponse<DimensionWithMandatory>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = format!("{}/dimension?{}", host, filters.to_string());
    let response: PaginatedResponse<DimensionWithMandatory> = client
        .get(url)
        .header("x-tenant", &tenant)
        .send()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?
        .json()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?;

    Ok(response)
}

// #[server(GetDefaultConfig, "/fxn", "GetJson")]
pub async fn fetch_default_config(
    filters: &PaginationParams,
    tenant: String,
) -> Result<PaginatedResponse<DefaultConfig>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = format!("{}/default-config?{}", host, filters.to_string());
    let response: PaginatedResponse<DefaultConfig> = client
        .get(url)
        .header("x-tenant", tenant)
        .send()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?
        .json()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?;

    Ok(response)
}

pub async fn fetch_snapshots(
    filters: &PaginationParams,
    tenant: String,
) -> Result<PaginatedResponse<ConfigVersion>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = format!("{host}/config/versions?{}", filters.to_string());
    let response: PaginatedResponse<ConfigVersion> = client
        .get(url)
        .header("x-tenant", tenant)
        .send()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?
        .json()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?;

    Ok(response)
}

pub async fn delete_context(
    tenant: String,
    context_id: String,
) -> Result<(), ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    // Use the first element of the context_id array in the URL
    let url = format!("{}/context/{}", host, context_id);

    let response = client
        .delete(&url) // Make sure to pass the URL by reference here
        .header("x-tenant", tenant)
        .send()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?;

    if response.status().is_success() {
        Ok(())
    } else {
        Err(ServerFnError::new(format!(
            "Failed to delete context with status: {}",
            response.status()
        )))
    }
}

pub async fn fetch_experiments(
    filters: &ExperimentListFilters,
    pagination: &PaginationParams,
    tenant: String,
) -> Result<PaginatedResponse<ExperimentResponse>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();
    let pagination = pagination.to_string();

    let url = if pagination.is_empty() {
        format!("{}/experiments?{}", host, filters.to_string())
    } else {
        format!(
            "{}/experiments?{}&{}",
            host,
            filters.to_string(),
            pagination
        )
    };
    let response: PaginatedResponse<ExperimentResponse> = client
        .get(url)
        .header("x-tenant", tenant)
        .send()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?
        .json()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?;

    Ok(response)
}

pub async fn fetch_functions(
    filters: &PaginationParams,
    tenant: String,
) -> Result<PaginatedResponse<Function>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = format!("{}/function?{}", host, filters.to_string());
    let response: PaginatedResponse<Function> = client
        .get(url)
        .header("x-tenant", tenant)
        .send()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?
        .json()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?;

    Ok(response)
}

pub async fn fetch_function(
    function_name: String,
    tenant: String,
) -> Result<Function, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = format!("{}/function/{}", host, function_name);
    let response: Function = client
        .get(url)
        .header("x-tenant", tenant)
        .send()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?
        .json()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?;

    Ok(response)
}

// #[server(GetConfig, "/fxn", "GetJson")]
pub async fn fetch_config(
    tenant: String,
    version: Option<String>,
) -> Result<Config, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = match version {
        Some(version) => format!("{}/config?version={}", host, version),
        None => format!("{}/config", host),
    };
    match client.get(url).header("x-tenant", tenant).send().await {
        Ok(response) => {
            let config: Config = response
                .json()
                .await
                .map_err(|e| ServerFnError::new(e.to_string()))?;
            Ok(config)
        }
        Err(e) => Err(ServerFnError::new(e.to_string())),
    }
}

// #[server(GetExperiment, "/fxn", "GetJson")]
pub async fn fetch_experiment(
    exp_id: String,
    tenant: String,
) -> Result<ExperimentResponse, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();
    let url = format!("{}/experiments/{}", host, exp_id);

    match client.get(url).header("x-tenant", tenant).send().await {
        Ok(experiment) => {
            let experiment = experiment
                .json()
                .await
                .map_err(|err| ServerFnError::new(err.to_string()))?;
            Ok(experiment)
        }
        Err(e) => Err(ServerFnError::new(e.to_string())),
    }
}

pub async fn delete_default_config(key: String, tenant: String) -> Result<(), String> {
    let host = get_host();
    let url = format!("{host}/default-config/{key}");

    request(
        url,
        reqwest::Method::DELETE,
        None::<serde_json::Value>,
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await?;

    Ok(())
}

pub async fn delete_dimension(name: String, tenant: String) -> Result<(), String> {
    let host = get_host();
    let url = format!("{host}/dimension/{name}");

    request(
        url,
        reqwest::Method::DELETE,
        None::<serde_json::Value>,
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await?;

    Ok(())
}

pub async fn fetch_types(
    filters: &PaginationParams,
    tenant: String,
) -> Result<PaginatedResponse<TypeTemplate>, ServerFnError> {
    let host = use_host_server();
    let url = format!("{host}/types?{}", filters.to_string());
    let err_handler = |e: String| ServerFnError::new(e.to_string());
    let response = request::<()>(
        url,
        reqwest::Method::GET,
        None,
        construct_request_headers(&[("x-tenant", &tenant)]).map_err(err_handler)?,
    )
    .await
    .map_err(err_handler)?;
    parse_json_response::<PaginatedResponse<TypeTemplate>>(response)
        .await
        .map_err(err_handler)
}
