mod filter;

use filter::{AuditLogFilterWidget, FilterSummary};
use leptos::*;
use serde_json::{Map, Value, json};
use superposition_macros::box_params;
use superposition_types::{
    api::audit_log::AuditQueryFilters,
    custom_query::{CustomQuery, PaginationParams, Query},
};

use crate::{
    api::audit_log,
    components::{
        change_summary::{ChangeLogPopup, ChangeSummary},
        datetime::DatetimeStr,
        skeleton::Skeleton,
        stat::Stat,
        table::{
            Table,
            types::{
                Column, ColumnSortable, Expandable, TablePaginationProps,
                default_column_formatter, default_formatter,
            },
        },
    },
    query_updater::{use_param_updater, use_signal_from_query},
    types::{OrganisationId, Workspace},
};

#[derive(Clone)]
pub struct DiffData {
    pub id: String,
    pub table_name: String,
    pub original_data: Map<String, Value>,
    pub new_data: Map<String, Value>,
}

fn sort_callback(
    filters_rws: RwSignal<AuditQueryFilters>,
    pagination_params_rws: RwSignal<PaginationParams>,
) -> Callback<()> {
    Callback::new(move |_| {
        let filters = filters_rws.get();
        let sort_by = filters.sort_by.unwrap_or_default().flip();

        let new_filters = AuditQueryFilters {
            sort_by: Some(sort_by),
            ..filters
        };
        pagination_params_rws.update(|f| f.reset_page());
        filters_rws.set(new_filters);
    })
}

fn audit_log_table_columns(
    filters_rws: RwSignal<AuditQueryFilters>,
    diff_data_rws: RwSignal<Option<DiffData>>,
    pagination_params_rws: RwSignal<PaginationParams>,
) -> Vec<Column> {
    let current_sort_by = filters_rws.with(|f| f.sort_by.clone().unwrap_or_default());

    vec![
        Column::new(
            "table_name".to_string(),
            false,
            default_formatter,
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::new(
            "timestamp".to_string(),
            false,
            move |value, _row| {
                view! { <DatetimeStr datetime=value.into() /> }
            },
            ColumnSortable::Yes {
                sort_fn: sort_callback(filters_rws, pagination_params_rws),
                sort_by: current_sort_by.clone(),
                currently_sorted: true,
            },
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
                view! { <span class=format!("badge {badge_class}")>{action}</span> }
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
                view! { <pre class="text-xs text-gray-700 whitespace-pre-wrap">{value.to_string()}</pre> }.into_view()
            },
            ColumnSortable::No,
            Expandable::Enabled(100),
            default_column_formatter,
        ),
        Column::new(
            "new_data".to_string(),
            false,
            move |value: &str, _row: &Map<String, Value>| {
                view! { <pre class="text-xs text-gray-700 whitespace-pre-wrap">{value.to_string()}</pre> }.into_view()
            },
            ColumnSortable::No,
            Expandable::Enabled(100),
            default_column_formatter,
        ),
        Column::new(
            "".to_string(),
            false,
            move |_value: &str, row: &Map<String, Value>| {
                let data = DiffData {
                    id: row
                        .get("id")
                        .and_then(|v| v.as_str())
                        .unwrap_or_default()
                        .to_string(),
                    table_name: row
                        .get("table_name")
                        .and_then(|v| v.as_str())
                        .unwrap_or_default()
                        .to_string(),
                    original_data: row
                        .get("original_data")
                        .and_then(|v| v.as_object())
                        .cloned()
                        .unwrap_or_default(),
                    new_data: row
                        .get("new_data")
                        .and_then(|v| v.as_object())
                        .cloned()
                        .unwrap_or_default(),
                };
                view! {
                    <p
                        on:click=move|_| diff_data_rws.set(Some(data.clone()))
                        class="text-blue-500 underline underline-offset-2 cursor-pointer"
                    >
                        {"Click to compare values"}
                    </p>
                }
                .into_view()
            },
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        ),
    ]
}

#[component]
pub fn AuditLog() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let diff_data_rws = RwSignal::new(None as Option<DiffData>);

    let filters_rws = use_signal_from_query(move |query_string| {
        Query::<AuditQueryFilters>::extract_non_empty(&query_string).into_inner()
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
                filters_rws.get(),
                pagination_params_rws.get(),
                workspace.get().0,
                org.get().0,
            )
        },
        |(filters, pagination_params, workspace, org_id)| async move {
            audit_log::fetch(&filters, &pagination_params, &workspace, &org_id)
                .await
                .unwrap_or_default()
        },
    );

    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|p| p.page = Some(page));
    });

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
            {move || {
                let value = audit_log_resource.get().unwrap_or_default();
                let table_rows = value
                    .data
                    .iter()
                    .map(|ele| json!(ele).as_object().unwrap().clone())
                    .collect::<Vec<Map<String, Value>>>();
                let pagination_params = pagination_params_rws.get();
                let pagination_props = TablePaginationProps {
                    enabled: true,
                    count: pagination_params.count.unwrap_or_default(),
                    current_page: pagination_params.page.unwrap_or_default(),
                    total_pages: value.total_pages,
                    on_page_change: handle_page_change,
                };
                view! {
                    <div class="h-full flex flex-col gap-4">
                        <div class="flex justify-between">
                            <Stat
                                heading="Audit Log Entries"
                                icon="ri-file-list-3-line"
                                number=value.total_items.to_string()
                            />
                            <div class="self-end">
                                <AuditLogFilterWidget filters_rws pagination_params_rws />
                            </div>
                        </div>
                        <FilterSummary filters_rws />
                        <div class="card w-full bg-base-100 rounded-xl overflow-hidden shadow">
                            <div class="card-body overflow-y-auto overflow-x-visible">
                                <Table
                                    class="!overflow-y-auto"
                                    rows=table_rows
                                    key_column="id"
                                    columns=audit_log_table_columns(
                                        filters_rws,
                                        diff_data_rws,
                                        pagination_params_rws,
                                    )
                                    pagination=pagination_props
                                />
                            </div>
                        </div>
                    </div>
                }
            }}
        </Suspense>
        {move || {
            match diff_data_rws.get() {
                Some(data) => {
                    view! {
                        <ChangeLogPopup
                            title=data.id
                            description=format!("Changes made to {}", data.table_name)
                            confirm_text="Done"
                            close_text="Close"
                            on_confirm=move |_| diff_data_rws.set(None)
                            on_close=move |_| diff_data_rws.set(None)
                        >
                            <ChangeSummary
                                title="Fields which changed"
                                key_column="Property"
                                old_values=diff_data_rws
                                    .with(|d| {
                                        d.as_ref()
                                            .map(|d| d.original_data.clone())
                                            .unwrap_or_default()
                                    })
                                new_values=diff_data_rws
                                    .with(|d| {
                                        d.as_ref().map(|d| d.new_data.clone()).unwrap_or_default()
                                    })
                            />
                        </ChangeLogPopup>
                    }
                }
                None => ().into_view(),
            }
        }}
    }
}
