use crate::utils::{
    construct_request_headers, get_host, parse_json_response, request, use_host_server,
};
use leptos::ServerFnError;
use serde_json::{Map, Value};
use superposition_types::{
    api::{
        context::ContextListFilters,
        default_config::DefaultConfigFilters,
        experiments::{
            ExperimentListFilters, ExperimentResponse, ExperimentStateChangeRequest,
        },
        functions::{
            FunctionExecutionRequest, FunctionExecutionResponse, ListFunctionFilters,
        },
        webhook::{CreateWebhookRequest, UpdateWebhookRequest, WebhookName},
        workspace::WorkspaceResponse,
    },
    custom_query::{DimensionQuery, PaginationParams, QueryMap},
    database::{
        models::{
            cac::{ConfigVersion, Context, DefaultConfig, Function, TypeTemplate},
            experimentation::ExperimentGroup,
            others::{CustomHeaders, HttpMethod, PayloadVersion, Webhook, WebhookEvent},
            ChangeReason, Description, NonEmptyString,
        },
        types::DimensionWithMandatory,
    },
    Config, PaginatedResponse,
};

// #[server(GetDimensions, "/fxn", "GetJson")]
pub async fn fetch_dimensions(
    filters: &PaginationParams,
    tenant: String,
    org_id: String,
) -> Result<PaginatedResponse<DimensionWithMandatory>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = format!("{}/dimension?{}", host, filters);
    let response: PaginatedResponse<DimensionWithMandatory> = client
        .get(url)
        .header("x-tenant", &tenant)
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
    pagination: &PaginationParams,
    filters: &DefaultConfigFilters,
    tenant: String,
    org_id: String,
) -> Result<PaginatedResponse<DefaultConfig>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = format!("{}/default-config?{}&{}", host, pagination, filters);
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

    let url = format!("{host}/config/versions?{}", filters);
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
    tenant: String,
    context_id: String,
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
        format!("{}/experiments?{}", host, filters)
    } else {
        format!("{}/experiments?{}&{}", host, filters, pagination)
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
    pagination: &PaginationParams,
    filters: &ListFunctionFilters,
    tenant: String,
    org_id: String,
) -> Result<PaginatedResponse<Function>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();
    let url = format!("{}/function?{}&{}", host, filters, pagination);
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
    tenant: String,
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

pub async fn fetch_context(
    tenant: String,
    org_id: String,
    pagination: &PaginationParams,
    context_filters: &ContextListFilters,
    dimension_params: &DimensionQuery<QueryMap>,
) -> Result<PaginatedResponse<Context>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();
    let url =
        format!("{host}/context/list?{pagination}&{context_filters}&{dimension_params}");

    match client
        .get(url)
        .header("x-tenant", tenant)
        .header("x-org-id", org_id)
        .send()
        .await
    {
        Ok(response) => {
            let config = response
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
    org_id: String,
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
    let url = format!("{host}/types?{}", filters);
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
) -> Result<PaginatedResponse<WorkspaceResponse>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();
    let url = format!("{}/workspaces?{}", host, filters);
    let response: PaginatedResponse<WorkspaceResponse> = client
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

#[allow(clippy::too_many_arguments)]
pub async fn create_webhook(
    name: String,
    description: String,
    enabled: bool,
    url: String,
    method: HttpMethod,
    payload_version: PayloadVersion,
    custom_headers: CustomHeaders,
    events: Vec<WebhookEvent>,
    change_reason: String,
    tenant: String,
    org_id: String,
) -> Result<Webhook, String> {
    let payload = CreateWebhookRequest {
        name: WebhookName::try_from(name)?,
        description: Description::try_from(description)?,
        enabled,
        url: NonEmptyString::try_from(url)?,
        method,
        payload_version: Some(payload_version),
        custom_headers: Some(custom_headers),
        events,
        change_reason: ChangeReason::try_from(change_reason)?,
    };
    let host = get_host();
    let url = format!("{host}/webhook");

    let response = request(
        url,
        reqwest::Method::POST,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

#[allow(clippy::too_many_arguments)]
pub async fn update_webhook(
    webhook_name: String,
    enabled: bool,
    url: String,
    method: HttpMethod,
    payload_version: PayloadVersion,
    custom_headers: CustomHeaders,
    events: Vec<WebhookEvent>,
    description: String,
    change_reason: String,
    tenant: String,
    org_id: String,
) -> Result<Webhook, String> {
    let payload = UpdateWebhookRequest {
        enabled: Some(enabled),
        url: Some(NonEmptyString::try_from(url)?),
        method: Some(method),
        payload_version: Some(payload_version),
        custom_headers: Some(custom_headers),
        events: Some(events),
        description: Some(Description::try_from(description)?),
        change_reason: ChangeReason::try_from(change_reason)?,
    };
    let host = get_host();
    let url = format!("{host}/webhook/{webhook_name}");

    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn fetch_webhooks(
    filters: &PaginationParams,
    tenant: String,
    org_id: String,
) -> Result<PaginatedResponse<Webhook>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = format!("{}/webhook?{}", host, filters);
    let response: PaginatedResponse<Webhook> = client
        .get(url)
        .header("x-tenant", &tenant)
        .header("x-org-id", org_id)
        .send()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?
        .json()
        .await
        .map_err(|e| ServerFnError::new(e.to_string()))?;

    Ok(response)
}

pub async fn delete_webhooks(
    name: String,
    tenant: String,
    org_id: String,
) -> Result<(), String> {
    let host = get_host();
    let url = format!("{host}/webhook/{name}");

    request(
        url,
        reqwest::Method::DELETE,
        None::<serde_json::Value>,
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;

    Ok(())
}

pub async fn resolve_config(
    tenant: &str,
    context: &str,
    org_id: &str,
    show_reasoning: bool,
    context_id: Option<&String>,
) -> Result<Map<String, Value>, String> {
    let client = reqwest::Client::new();
    let host = use_host_server();
    let mut url = format!("{host}/config/resolve?{context}");
    if let Some(context_id) = context_id {
        url = format!("{url}&context_id={context_id}");
    };
    if show_reasoning {
        url = format!("{url}&show_reasoning=true");
    }
    match client
        .get(url)
        .header("x-tenant", tenant)
        .header("x-org-id", org_id)
        .send()
        .await
    {
        Ok(response) => parse_json_response(response).await,
        Err(e) => Err(e.to_string()),
    }
}

pub async fn pause_experiment(
    exp_id: &str,
    change_reason: String,
    tenant: &str,
    org_id: &str,
) -> Result<ExperimentResponse, String> {
    let payload = ExperimentStateChangeRequest {
        change_reason: ChangeReason::try_from(change_reason)?,
    };

    let host = get_host();
    let url = format!("{host}/experiments/{exp_id}/pause");

    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-tenant", tenant), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn resume_experiment(
    exp_id: &str,
    change_reason: String,
    tenant: &str,
    org_id: &str,
) -> Result<ExperimentResponse, String> {
    let payload = ExperimentStateChangeRequest {
        change_reason: ChangeReason::try_from(change_reason)?,
    };

    let host = get_host();
    let url = format!("{host}/experiments/{exp_id}/resume");

    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-tenant", tenant), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn get_context(
    context_id: &str,
    tenant: &str,
    org_id: &str,
) -> Result<Context, String> {
    let host = use_host_server();
    let url = format!("{host}/context/{context_id}");

    let response = request(
        url,
        reqwest::Method::GET,
        None::<serde_json::Value>,
        construct_request_headers(&[("x-tenant", tenant), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn get_context_from_condition(
    condition: &Map<String, Value>,
    tenant: &str,
    org_id: &str,
) -> Result<Context, String> {
    let host = use_host_server();
    let url = format!("{host}/context/get");

    let response = request(
        url,
        reqwest::Method::POST,
        Some(condition),
        construct_request_headers(&[("x-tenant", tenant), ("x-org-id", org_id)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn execute_autocomplete_function(
    name: &str,
    value: &str,
    environment: &Value,
    fn_name: &str,
    tenant: &str,
    org_id: &str,
) -> Result<Vec<String>, String> {
    let host = use_host_server();
    let url = format!("{}/function/{}/PUBLISHED/test", host, fn_name);
    let payload = FunctionExecutionRequest::AutocompleteFunctionRequest {
        name: name.to_owned(),
        prefix: value.to_owned(),
        environment: environment.clone(),
    };
    let resp = request(
        url.clone(),
        reqwest::Method::PUT,
        Some(payload.clone()),
        construct_request_headers(&[("x-tenant", tenant), ("x-org-id", org_id)])?,
    )
    .await?;

    let function_execution_response =
        parse_json_response::<FunctionExecutionResponse>(resp).await?;

    let result = function_execution_response
        .fn_output
        .as_array()
        .unwrap_or(&Vec::new())
        .iter()
        .map(|v| {
            v.as_str()
                .unwrap_or("Could not parse function execution response")
                .to_string()
        })
        .collect();
    Ok(result)
}

pub async fn fetch_experiment_groups(
    tenant: String,
    org_id: String,
) -> Result<PaginatedResponse<ExperimentGroup>, String> {
    let host = use_host_server();
    let url = format!("{}/experiment-groups", host);

    let response = request(
        url,
        reqwest::Method::GET,
        None::<serde_json::Value>,
        construct_request_headers(&[("x-tenant", &tenant), ("x-org-id", &org_id)])?,
    )
    .await?;
    parse_json_response(response).await
}
