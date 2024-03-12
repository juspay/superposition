use crate::components::table::types::Column;
use crate::utils::get_host;
use leptos::*;
use leptos_router::A;
use reqwest::StatusCode;
use serde_json::{Map, Value};
use std::vec::Vec;

pub fn function_table_columns() -> Vec<Column> {
    vec![
        Column::new(
            "function_name".to_string(),
            None,
            |value: &str, _row: &Map<String, Value>| {
                let function_name = value.to_string();
                view! {
                    <div>
                        <A href=function_name.clone() class="btn-link">
                            {function_name}
                        </A>

                    </div>
                }
                .into_view()
            },
        ),
        Column::default("function_description".to_string()),
        Column::default("published_runtime_version".to_string()),
        Column::default("draft_runtime_version".to_string()),
        Column::default("published_at".to_string()),
        Column::default("published_by".to_string()),
    ]
}

pub async fn publish_function(
    function_name: String,
    tenant: String,
) -> Result<String, String> {
    let client = reqwest::Client::new();
    let host = get_host();
    let url = format!("{host}/function/{function_name}/publish");
    let response = client
        .put(url)
        .header("x-tenant", tenant)
        .send()
        .await
        .map_err(|e| e.to_string())?;
    let status = response.status();
    let resp_data = response
        .text()
        .await
        .unwrap_or("Cannot decode response".to_string());
    match status {
        StatusCode::OK => Ok(resp_data),
        _ => Err(resp_data),
    }
}
