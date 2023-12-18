use super::types::{DefaultConfig, Dimension, ExperimentsResponse, ListFilters};
use crate::components::{
    condition_pills::condition_pills::ConditionPills, table::types::Column,
};
use core::time::Duration;
use leptos::*;
use leptos_router::A;
use serde_json::{json, Map, Value};
use std::vec::Vec;
use web_sys::MouseEvent;

pub async fn fetch_experiments(
    filters: ListFilters,
    tenant: &String,
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

    let url = format!("{}/experiments?{}", host, query_params.join("&"));
    let response: ExperimentsResponse = client
        .get(url)
        .header("x-tenant", tenant)
        .send()
        .await
        .map_err(|e| e.to_string())?
        .json()
        .await
        .map_err(|e| e.to_string())?;

    Ok(response)
}

pub async fn fetch_dimensions(tenant: &str) -> Result<Vec<Dimension>, String> {
    let client = reqwest::Client::new();
    let host = match std::env::var("APP_ENV").as_deref() {
        Ok("PROD") => {
            "https://context-aware-config.sso.internal.svc.k8s.apoc.mum.juspay.net"
        }
        Ok("SANDBOX") => "https://context-aware.internal.staging.mum.juspay.net",
        _ => "http://localhost:8080",
    };

    let url = format!("{}/dimension", host);
    let response: Vec<Dimension> = client
        .get(url)
        .header("x-tenant", tenant)
        .send()
        .await
        .map_err(|e| e.to_string())?
        .json()
        .await
        .map_err(|e| e.to_string())?;

    Ok(response)
}

pub async fn fetch_default_config(tenant: &str) -> Result<Vec<DefaultConfig>, String> {
    let client = reqwest::Client::new();
    let host = match std::env::var("APP_ENV").as_deref() {
        Ok("PROD") => {
            "https://context-aware-config.sso.internal.svc.k8s.apoc.mum.juspay.net"
        }
        Ok("SANDBOX") => "https://context-aware.internal.staging.mum.juspay.net",
        _ => "http://localhost:8080",
    };

    let url = format!("{}/default-config", host);
    let response: Vec<DefaultConfig> = client
        .get(url)
        .header("x-tenant", tenant)
        .send()
        .await
        .map_err(|e| e.to_string())?
        .json()
        .await
        .map_err(|e| e.to_string())?;

    Ok(response)
}

pub fn experiment_table_columns() -> Vec<Column> {
    vec![
        Column::new(
            "name".to_string(),
            None,
            Some(|value: &str, row: &Map<String, Value>| {
                let (copied, set_copied) = create_signal(false);

                let experiment_name = value.to_string();
                let experiment_id = row.get("id").map_or(String::from(""), |value| {
                    value.as_str().unwrap_or("").to_string()
                });
                let experiment_id_copy = experiment_id.to_string();
                let handle_copy = move |event: MouseEvent| {
                    event.prevent_default();

                    let copy_code =
                        format!("navigator.clipboard.writeText({})", experiment_id_copy);
                    match js_sys::eval(&copy_code) {
                        Ok(_) => {
                            set_copied.set(true);
                            set_timeout(
                                move || {
                                    set_copied.set(false);
                                },
                                Duration::new(1, 0),
                            );
                        }
                        Err(_) => logging::log!("unable to copy to clipboard"),
                    }
                };
                view! {
                        <div>
                            <A href=experiment_id.to_string() class="btn-link">
                                {experiment_name}
                            </A>
                            <div class="text-gray-500">
                                <span class="text-xs">
                                    {experiment_id}
                                </span>
                                <i class="ri-file-copy-line cursor-pointer ml-2" on:click:undelegated=handle_copy></i>
                                <Show when=move || copied.get()>
                                    <div class="inline-block bg-gray-600 ml-2 rounded-xl px-2">
                                        <span class="text-white text-xs font-semibold">
                                            "copied!"
                                        </span>
                                    </div>
                                </Show>
                            </div>
                        </div>
                    }
                    .into_view()
            }),
        ),
        Column::new(
            "status".to_string(),
            None,
            Some(|value: &str, _| {
                let badge_color = match value {
                    "CREATED" => "badge-success",
                    "INPROGRESS" => "badge-warning",
                    "CONCLUDED" => "badge-info",
                    &_ => "info",
                };
                let class = format!("badge {}", badge_color);
                view! {
                    <div class={class}>
                        <span class="text-white font-semibold text-xs">
                            {value.to_string()}
                        </span>
                    </div>
                }
                .into_view()
            }),
        ),
        Column::new(
            "context".to_string(),
            None,
            Some(|_, row: &Map<String, Value>| {
                let context = match row.get("context") {
                    Some(value) => value.to_owned(),
                    None => json!(""),
                };

                view! {
                    <div class="inline-flex flex-col gap-y-2">
                        <ConditionPills context=context />
                    </div>
                }
                .into_view()
            }),
        ),
        Column::new(
            "chosen_variant".to_string(),
            None,
            Some(|value: &str, _| {
                let label = match value {
                    "null" => "".to_string(),
                    other => other.to_string(),
                };

                view! {
                    <span>{label}</span>
                }
                .into_view()
            }),
        ),
        Column::default("created_at".to_string()),
        Column::default("created_by".to_string()),
        Column::default("last_modified".to_string()),
    ]
}
