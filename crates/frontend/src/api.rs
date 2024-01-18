use leptos::{server, ServerFnError};

use crate::types::{Config, DefaultConfig, Dimension, ExperimentsResponse, ListFilters};

#[server(GetDimensions, "/fxn", "GetJson")]
pub async fn fetch_dimensions(tenant: String) -> Result<Vec<Dimension>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = "http://localhost:8080";

    let url = format!("{}/dimension", host);
    let response: Vec<Dimension> = client
        .get(url)
        .header("x-tenant", &tenant)
        .send()
        .await
        .map_err(|e| ServerFnError::ServerError(e.to_string()))?
        .json()
        .await
        .map_err(|e| ServerFnError::ServerError(e.to_string()))?;

    Ok(response)
}

#[server(GetDefaultConfig, "/fxn", "GetJson")]
pub async fn fetch_default_config(
    tenant: String,
) -> Result<Vec<DefaultConfig>, ServerFnError> {
    let client = reqwest::Client::new();
    let host = "http://localhost:8080";

    let url = format!("{}/default-config", host);
    let response: Vec<DefaultConfig> = client
        .get(url)
        .header("x-tenant", tenant)
        .send()
        .await
        .map_err(|e| ServerFnError::ServerError(e.to_string()))?
        .json()
        .await
        .map_err(|e| ServerFnError::ServerError(e.to_string()))?;

    Ok(response)
}

#[server(GetExperiments, "/fxn", "GetJson")]
pub async fn fetch_experiments(
    filters: ListFilters,
    tenant: String,
) -> Result<ExperimentsResponse, ServerFnError> {
    let client = reqwest::Client::new();
    let host = "http://localhost:8080";

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
    let response: ExperimentsResponse = client
        .get(url)
        .header("x-tenant", tenant)
        .send()
        .await
        .map_err(|e| ServerFnError::ServerError(e.to_string()))?
        .json()
        .await
        .map_err(|e| ServerFnError::ServerError(e.to_string()))?;

    Ok(response)
}

#[server(GetConfig, "/fxn", "GetJson")]
pub async fn fetch_config(tenant: String) -> Result<Config, ServerFnError> {
    let client = reqwest::Client::new();
    let host = "http://localhost:8080";
    let url = format!("{host}/config");
    match client.get(url).header("x-tenant", tenant).send().await {
        Ok(response) => {
            let config: Config = response
                .json()
                .await
                .map_err(|e| ServerFnError::ServerError(e.to_string()))?;
            Ok(config)
        }
        Err(e) => Err(ServerFnError::ServerError(e.to_string())),
    }
}
