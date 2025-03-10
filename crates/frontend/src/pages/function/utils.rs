use crate::components::description_icon::InfoDescription;
use crate::components::table::types::{Column, ColumnSortable, Expandable};
use crate::utils::get_host;
use leptos::*;
use leptos_router::A;
use reqwest::StatusCode;
use serde_json::{Map, Value};
use std::vec::Vec;

pub fn function_table_columns() -> Vec<Column> {
    let expand = move |name: &str, row: &Map<String, Value>| {
        let function_name = name.to_string();
        let description = row
            .get("description")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        let change_reason = row
            .get("change_reason")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        view! {
            <div class="flex items-center gap-2">
                <A href=function_name.clone() class="btn-link">
                    {function_name}
                </A>
                <InfoDescription description=description change_reason=change_reason />
            </div>
        }
        .into_view()
    };

    vec![
        Column::new(
            "function_name".to_string(),
            false,
            expand,
            ColumnSortable::No,
            Expandable::Disabled,
        ),
        Column::default("description".to_string()),
        Column::default("function_type".to_string()),
        Column::default("published_runtime_version".to_string()),
        Column::default("draft_runtime_version".to_string()),
        Column::default("published_at".to_string()),
        Column::default("published_by".to_string()),
    ]
}

pub async fn publish_function(
    function_name: String,
    tenant: String,
    org_id: String,
) -> Result<String, String> {
    let client = reqwest::Client::new();
    let host = get_host();
    let url = format!("{host}/function/{function_name}/publish");
    let response = client
        .put(url)
        .header("x-tenant", tenant)
        .header("x-org-id", org_id)
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
