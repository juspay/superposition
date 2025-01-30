use leptos::ServerFnError;
use superposition_types::{
    custom_query::PaginationParams,
    database::{
        models::{
            cac::{
                ConfigVersion, Context, DefaultConfig, Dimension, Function, TypeTemplate,
            },
            Workspace,
        },
        types::DimensionWithMandatory,
    },
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
    tenant: &str,
    org_id: &str,
) -> Result<PaginatedResponse<DimensionWithMandatory>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = format!("{}/dimension?{}", host, filters.to_string());
    let response: PaginatedResponse<DimensionWithMandatory> = client
        .get(url)
        .header("x-tenant", tenant)
        .header("x-org-id", org_id)
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
    tenant: &str,
    org_id: &str,
) -> Result<PaginatedResponse<DefaultConfig>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = format!("{}/default-config?{}", host, filters.to_string());
    let response: PaginatedResponse<DefaultConfig> = client
        .get(url)
        .header("x-tenant", tenant)
        .header("x-org-id", org_id)
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
    org_id: String,
) -> Result<PaginatedResponse<ConfigVersion>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = format!("{host}/config/versions?{}", filters.to_string());
    let response: PaginatedResponse<ConfigVersion> = client
        .get(url)
        .header("x-tenant", tenant)
        .header("x-org-id", org_id)
        .send()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?
        .json()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?;

    Ok(response)
}

pub async fn delete_context(
    context_id: String,
    tenant: String,
    org_id: String,
) -> Result<(), ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    // Use the first element of the context_id array in the URL
    let url = format!("{}/context/{}", host, context_id);

    let response = client
        .delete(&url) // Make sure to pass the URL by reference here
        .header("x-tenant", tenant)
        .header("x-org-id", org_id)
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
    org_id: String,
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
        .header("x-org-id", org_id)
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
    org_id: String,
) -> Result<PaginatedResponse<Function>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = format!("{}/function?{}", host, filters.to_string());
    let response: PaginatedResponse<Function> = client
        .get(url)
        .header("x-tenant", tenant)
        .header("x-org-id", org_id)
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
    org_id: String,
) -> Result<Function, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = format!("{}/function/{}", host, function_name);
    let response: Function = client
        .get(url)
        .header("x-tenant", tenant)
        .header("x-org-id", org_id)
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
    tenant: &str,
    version: Option<String>,
    org_id: String,
) -> Result<Config, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = match version {
        Some(version) => format!("{}/config?version={}", host, version),
        None => format!("{}/config", host),
    };
    match client
        .get(url)
        .header("x-tenant", tenant)
        .header("x-org-id", org_id)
        .send()
        .await
    {
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
    exp_id: &str,
    tenant: &str,
    org_id: &str,
) -> Result<ExperimentResponse, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();
    let url = format!("{}/experiments/{}", host, exp_id);

    match client
        .get(url)
        .header("x-tenant", tenant)
        .header("x-org-id", org_id)
        .send()
        .await
    {
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

pub async fn delete_default_config(
    key: String,
    tenant: String,
    org_id: String,
) -> Result<(), String> {
    let host = get_host();
    let url = format!("{host}/default-config/{key}");

    request(
        url,
        reqwest::Method::DELETE,
        None::<serde_json::Value>,
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    Ok(())
}

pub async fn delete_dimension(
    name: String,
    tenant: String,
    org_id: String,
) -> Result<(), String> {
    let host = get_host();
    let url = format!("{host}/dimension/{name}");

    request(
        url,
        reqwest::Method::DELETE,
        None::<serde_json::Value>,
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    Ok(())
}

pub async fn fetch_organisations() -> Result<Vec<String>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();
    let url = format!("{host}/organisations");

    match client.get(url).send().await {
        Ok(organisations) => {
            let organisations = organisations
                .json()
                .await
                .map_err(|err| ServerFnError::new(err.to_string()))?;
            Ok(organisations)
        }
        Err(e) => Err(ServerFnError::new(e.to_string())),
    }
}

pub async fn fetch_types(
    filters: &PaginationParams,
    tenant: String,
    org_id: String,
) -> Result<PaginatedResponse<TypeTemplate>, ServerFnError> {
    let host = use_host_server();
    let url = format!("{host}/types?{}", filters.to_string());
    let err_handler = |e: String| ServerFnError::new(e.to_string());
    let response = request::<()>(
        url,
        reqwest::Method::GET,
        None,
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])
            .map_err(err_handler)?,
    )
    .await
    .map_err(err_handler)?;
    parse_json_response::<PaginatedResponse<TypeTemplate>>(response)
        .await
        .map_err(err_handler)
}

pub async fn fetch_workspaces(
    filters: &PaginationParams,
    org_id: &String,
) -> Result<PaginatedResponse<Workspace>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();
    let url = format!("{}/workspaces?{}", host, filters.to_string());
    let response: PaginatedResponse<Workspace> = client
        .get(url)
        .header("x-org-id", org_id)
        .send()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?
        .json()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?;
    Ok(response)
}

pub async fn fetch_type(tenant: &str, name: &str) -> Result<TypeTemplate, ServerFnError> {
    let host = use_host_server();
    let url = format!("{host}/types/{name}");
    let err_handler = |e: String| ServerFnError::new(e.to_string());
    let response = request::<()>(
        url,
        reqwest::Method::GET,
        None,
        construct_request_headers(&[("x-tenant", &tenant)]).map_err(err_handler)?,
    )
    .await
    .map_err(err_handler)?;
    parse_json_response::<TypeTemplate>(response)
        .await
        .map_err(err_handler)
}

pub async fn fetch_context(tenant: &str, id: &str) -> Result<Context, ServerFnError> {
    let host = use_host_server();
    let url = format!("{host}/context/{id}");
    let err_handler = |e: String| ServerFnError::new(e.to_string());
    let response = request::<()>(
        url,
        reqwest::Method::GET,
        None,
        construct_request_headers(&[("x-tenant", &tenant)]).map_err(err_handler)?,
    )
    .await
    .map_err(err_handler)?;
    parse_json_response::<Context>(response)
        .await
        .map_err(err_handler)
}

pub async fn fetch_default_config_key(
    tenant: &str,
    key: &str,
) -> Result<DefaultConfig, ServerFnError> {
    let host = use_host_server();
    let url = format!("{host}/default-config/{key}");
    let err_handler = |e: String| ServerFnError::new(e.to_string());
    let response = request::<()>(
        url,
        reqwest::Method::GET,
        None,
        construct_request_headers(&[("x-tenant", &tenant)]).map_err(err_handler)?,
    )
    .await
    .map_err(err_handler)?;
    parse_json_response::<DefaultConfig>(response)
        .await
        .map_err(err_handler)
}

pub async fn fetch_dimension(
    tenant: &str,
    name: &str,
) -> Result<Dimension, ServerFnError> {
    let host = use_host_server();
    let url = format!("{host}/dimension/{name}");
    let err_handler = |e: String| ServerFnError::new(e.to_string());
    let response = request::<()>(
        url,
        reqwest::Method::GET,
        None,
        construct_request_headers(&[("x-tenant", &tenant)]).map_err(err_handler)?,
    )
    .await
    .map_err(err_handler)?;
    parse_json_response::<Dimension>(response)
        .await
        .map_err(err_handler)
}
