use leptos::*;
use leptos_router::A;
use reqwest::StatusCode;

use crate::components::table::types::{Column, ColumnSortable, Expandable};
use crate::utils::get_host;

pub fn function_table_columns() -> Vec<Column> {
    vec![
        Column::new(
            "function_name".to_string(),
            false,
            |value: &str, _| {
                let function_name = value.to_string();
                view! {
                    <A href=function_name.clone() class="btn-link">
                        {function_name}
                    </A>
                }
                .into_view()
            },
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
