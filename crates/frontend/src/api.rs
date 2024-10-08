use leptos::ServerFnError;

use crate::{
    types::{
        Config, DefaultConfig, Dimension, ExpListFilters, Experiment, ExperimentResponse,
        FetchTypeTemplateResponse, FunctionResponse, ListFilters, PaginatedResponse,
    },
    utils::{
        construct_request_headers, get_host, parse_json_response, request,
        use_host_server,
    },
};

// #[server(GetDimensions, "/fxn", "GetJson")]
pub async fn fetch_dimensions(
    filters: ListFilters,
    tenant: String,
) -> Result<PaginatedResponse<Dimension>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let mut query_params = vec![];
    if let Some(page) = filters.page {
        query_params.push(format!("page={}", page));
    }
    if let Some(count) = filters.count {
        query_params.push(format!("count={}", count));
    }

    let url = format!("{}/dimension?{}", host, query_params.join("&"));
    let response: PaginatedResponse<Dimension> = client
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
    filters: ListFilters,
    tenant: String,
) -> Result<PaginatedResponse<DefaultConfig>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let mut query_params = vec![];
    if let Some(page) = filters.page {
        query_params.push(format!("page={}", page));
    }
    if let Some(count) = filters.count {
        query_params.push(format!("count={}", count));
    }

    let url = format!("{}/default-config?{}", host, query_params.join("&"));
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

// #[server(GetExperiments, "/fxn", "GetJson")]
pub async fn fetch_experiments(
    filters: ExpListFilters,
    tenant: String,
) -> Result<PaginatedResponse<Experiment>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let mut query_params = vec![];
    if let Some(status) = filters.status {
        let status: Vec<String> = status.iter().map(|val| val.to_string()).collect();
        query_params.push(format!("status={}", status.join(",")));
    }
    if let Some(from_date) = filters.from_date {
        query_params.push(format!("from_date={}", from_date));
    }
    if let Some(to_date) = filters.to_date {
        query_params.push(format!("to_date={}", to_date));
    }
    if let Some(page) = filters.page {
        query_params.push(format!("page={}", page));
    }
    if let Some(count) = filters.count {
        query_params.push(format!("count={}", count));
    }

    let url = format!("{}/experiments?{}", host, query_params.join("&"));
    let response: PaginatedResponse<Experiment> = client
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
    filters: ListFilters,
    tenant: String,
) -> Result<PaginatedResponse<FunctionResponse>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let mut query_params = vec![];
    if let Some(page) = filters.page {
        query_params.push(format!("page={}", page));
    }
    if let Some(count) = filters.count {
        query_params.push(format!("count={}", count));
    }
    let url = format!("{}/function?{}", host, query_params.join("&"));
    let response: PaginatedResponse<FunctionResponse> = client
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
) -> Result<FunctionResponse, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = format!("{}/function/{}", host, function_name);
    let response: FunctionResponse = client
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
pub async fn fetch_config(tenant: String) -> Result<Config, ServerFnError> {
    let client = reqwest::Client::new();
    let host = use_host_server();

    let url = format!("{}/config", host);
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
    tenant: String,
    page: i64,
    count: i64,
) -> Result<FetchTypeTemplateResponse, ServerFnError> {
    let host = use_host_server();
    let url = format!("{host}/types?page={page}&count={count}");
    let err_handler = |e: String| ServerFnError::new(e.to_string());
    let response = request::<()>(
        url,
        reqwest::Method::GET,
        None,
        construct_request_headers(&[("x-tenant", &tenant)]).map_err(err_handler)?,
    )
    .await
    .map_err(err_handler)?;
    parse_json_response::<FetchTypeTemplateResponse>(response)
        .await
        .map_err(err_handler)
}
