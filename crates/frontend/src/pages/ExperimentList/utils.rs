use super::types::{ExperimentsResponse, ListFilters};
use std::vec::Vec;

// pub struct ListFilters {
//     pub status: Option<StatusTypes>,
//     pub from_date: Option<DateTime<Utc>>,
//     pub to_date: Option<DateTime<Utc>>,
//     pub page: Option<i64>,
//     pub count: Option<i64>,
// }

pub async fn fetch_experiments(
    filters: ListFilters,
) -> Result<ExperimentsResponse, String> {
    let client = reqwest::Client::new();
    let host = match std::env::var("APP_ENV").as_deref() {
        Ok("PROD") => {
            "https://context-aware-config.sso.internal.svc.k8s.apoc.mum.juspay.net"
        }
        Ok("SANDBOX") => "https://context-aware.internal.staging.mum.juspay.net",
        _ => "http://localhost:8080",
    };

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

    let url = format!("{}/experiments?{}", host, query_params.join(","));
    let response: ExperimentsResponse = client
        .get(url)
        .header("x-tenant", "mjos")
        .send()
        .await
        .map_err(|e| e.to_string())?
        .json()
        .await
        .map_err(|e| e.to_string())?;

    Ok(response)
}
