pub mod filter;

pub use filter::{AuditLogFilterWidget, AuditLogFilters, FilterSummary};
use leptos::*;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use superposition_macros::box_params;
use superposition_types::{
    api::workspace::WorkspaceResponse,
    custom_query::{CustomQuery, PaginationParams, Query},
    database::models::cac::EventLog,
    PaginatedResponse,
};

use crate::{
    api::fetch_audit_logs,
    components::{
        skeleton::Skeleton,
        stat::Stat,
        table::{
            types::{
                default_column_formatter, Column, ColumnSortable, Expandable,
                TablePaginationProps,
            },
            Table,
        },
    },
    query_updater::{use_param_updater, use_signal_from_query},
    types::{OrganisationId, Tenant},
};

#[derive(Serialize, Deserialize, Clone, Default)]
struct AuditLogResource {
    pub(self) audit_logs: PaginatedResponse<EventLog>,
}

fn audit_log_table_columns() -> Vec<Column> {
    vec![
        Column::new(
            "timestamp".to_string(),
            false,
            move |value: &str, _row: &Map<String, Value>| {
                let timestamp = value.to_string();
                view! { <span class="text-gray-900">{timestamp}</span> }.into_view()
            },
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::new(
            "user_name".to_string(),
            false,
            move |value: &str, _row: &Map<String, Value>| {
                let user_name = value.to_string();
                view! { <span class="text-gray-900">{user_name}</span> }.into_view()
            },
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::new(
            "table_name".to_string(),
            false,
            move |value: &str, _row: &Map<String, Value>| {
                let table_name = value.to_string();
                view! { <span class="text-gray-900">{table_name}</span> }.into_view()
            },
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::new(
            "action".to_string(),
            false,
            move |value: &str, _row: &Map<String, Value>| {
                let action = value.to_string();
                let badge_class = match value {
                    "INSERT" => "badge-success",
                    "UPDATE" => "badge-warning",
                    "DELETE" => "badge-error",
                    _ => "badge-info",
                };
                view! { <span class=format!("badge {}", badge_class)>{action}</span> }
                    .into_view()
            },
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::new(
            "original_data".to_string(),
            false,
            move |value: &str, _row: &Map<String, Value>| {
                let original_data = value.to_string();
                view! { <pre class="text-xs text-gray-700 whitespace-pre-wrap">{original_data}</pre> }.into_view()
            },
            ColumnSortable::No,
            Expandable::Enabled(100),
            default_column_formatter,
        ),
        Column::new(
            "new_data".to_string(),
            false,
            move |value: &str, _row: &Map<String, Value>| {
                let new_data = value.to_string();
                view! { <pre class="text-xs text-gray-700 whitespace-pre-wrap">{new_data}</pre> }.into_view()
            },
            ColumnSortable::No,
            Expandable::Enabled(100),
            default_column_formatter,
        ),
    ]
}

#[component]
pub fn AuditLog() -> impl IntoView {
    let _workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let filters_rws = use_signal_from_query(move |query_string| {
        Query::<AuditLogFilters>::extract_non_empty(&query_string).into_inner()
    });

    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
    });

    use_param_updater(move || {
        box_params![filters_rws.get(), pagination_params_rws.get()]
    });

    let audit_log_resource = create_blocking_resource(
        move || {
            (
                workspace.get().0,
                filters_rws.get(),
                pagination_params_rws.get(),
                org.get().0,
            )
        },
        |(workspace, filters, pagination_params, org_id)| async move {
            let audit_logs_result = fetch_audit_logs(
                &filters,
                &pagination_params,
                workspace.to_string(),
                org_id.clone(),
            )
            .await;

            AuditLogResource {
                audit_logs: audit_logs_result.unwrap_or_default(),
            }
        },
    );

    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    });

    view! {
        <Suspense fallback=move || view! { <Skeleton /> }>
            <div class="h-full flex flex-col gap-4">
                {move || {
                    let value = audit_log_resource.get();
                    let total_items = match value {
                        Some(v) => v.audit_logs.total_items.to_string(),
                        _ => "0".to_string(),
                    };
                    view! {
                        <div class="flex justify-between">
                            <Stat
                                heading="Audit Log Entries"
                                icon="ri-file-list-3-line"
                                number=total_items
                            />
                            <div class="flex items-end gap-4">
                                <AuditLogFilterWidget
                                    pagination_params_rws
                                    filters_rws
                                />
                            </div>
                        </div>
                    }
                }}
                <FilterSummary filters_rws />
                <div class="card w-full bg-base-100 rounded-xl overflow-hidden shadow">
                    <div class="card-body overflow-y-auto overflow-x-visible">
                        {move || {
                            let value = audit_log_resource.get();
                            let pagination_params = pagination_params_rws.get();
                            let table_columns = audit_log_table_columns();

                            match value {
                                Some(v) => {
                                    let data = v
                                        .audit_logs
                                        .data
                                        .iter()
                                        .map(|ele| {
                                            let mut ele_map = json!(ele)
                                                .as_object()
                                                .unwrap()
                                                .to_owned();
                                            ele_map
                                                .insert(
                                                    "timestamp".to_string(),
                                                    json!(ele.timestamp.format("%Y-%m-%d %H:%M:%S UTC").to_string()),
                                                );
                                            ele_map
                                        })
                                        .collect::<Vec<Map<String, Value>>>()
                                        .to_owned();
                                    let pagination_props = TablePaginationProps {
                                        enabled: true,
                                        count: pagination_params.count.unwrap_or_default(),
                                        current_page: pagination_params.page.unwrap_or_default(),
                                        total_pages: v.audit_logs.total_pages,
                                        on_page_change: handle_page_change,
                                    };
                                    view! {
                                        <Table
                                            class="!overflow-y-auto"
                                            rows=data
                                            key_column="id"
                                            columns=table_columns
                                            pagination=pagination_props
                                        />
                                    }
                                        .into_view()
                                }
                                None => view! { Loading.... }.into_view(),
                            }
                        }}
                    </div>
                </div>
            </div>
        </Suspense>
    }
}

