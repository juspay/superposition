use leptos::*;
use leptos_router::A;
use reqwest::StatusCode;
use superposition_types::{
    api::functions::FunctionStateChangeRequest, database::models::ChangeReason,
};

use crate::components::table::types::{
    default_column_formatter, Column, ColumnSortable, Expandable,
};
use crate::utils::{construct_request_headers, request, use_host_server};

pub fn function_table_columns() -> Vec<Column> {
    vec![
        Column::new(
            "function_name".to_string(),
            false,
            |value: &str, _| {
                let function_name = value.to_string();
                view! {
                    <A href=function_name.clone() class="text-blue-500 underline underline-offset-2">
                        {function_name}
                    </A>
                }
                .into_view()
            },
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::default("function_type".to_string()),
        Column::default("published_runtime_version".to_string()),
        Column::default("published_at".to_string()),
        Column::default("published_by".to_string()),
    ]
}

pub async fn publish_function(
    function_name: String,
    change_reason: String,
    workspace: &str,
    org_id: &str,
) -> Result<String, String> {
    let payload = FunctionStateChangeRequest {
        change_reason: ChangeReason::try_from(change_reason)?,
    };

    let host = use_host_server();
    let url = format!("{host}/function/{function_name}/publish");

    let response = request(
        url,
        reqwest::Method::PATCH,
        Some(payload),
        construct_request_headers(&[("x-workspace", workspace), ("x-org-id", org_id)])?,
    )
    .await?;

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
