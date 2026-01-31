mod filter;

use leptos::*;
use leptos_router::A;
use serde_json::{Map, Value, json};
use superposition_macros::box_params;
use superposition_types::api::variables::{SortOn, VariableFilters};
use superposition_types::{
    SortBy,
    custom_query::{CustomQuery, PaginationParams, Query},
};

use crate::api::variables;
use crate::components::datetime::DatetimeStr;
use crate::components::{
    button::Button,
    drawer::PortalDrawer,
    skeleton::Skeleton,
    stat::Stat,
    table::{
        Table,
        types::{
            Column, ColumnSortable, Expandable, TablePaginationProps,
            default_column_formatter,
        },
    },
    variable_form::VariableForm,
};

use filter::{FilterSummary, VariableFilterWidget};

use crate::query_updater::{use_param_updater, use_signal_from_query};
use crate::types::{OrganisationId, Workspace};

#[derive(Clone)]
enum Action {
    Create,
    None,
}

fn variable_table_columns(
    filters_rws: RwSignal<VariableFilters>,
    pagination_params_rws: RwSignal<PaginationParams>,
) -> Vec<Column> {
    let expand = move |key_name: &str, _row: &Map<String, Value>| {
        let label = key_name.to_string();

        view! {
            <A href=label.clone() class="text-blue-500 underline underline-offset-2">
                {label}
            </A>
        }
        .into_view()
    };

    let current_filters = filters_rws.get();
    let current_sort_on = current_filters.sort_on.unwrap_or_default();
    let current_sort_by = current_filters.sort_by.unwrap_or_default();

    vec![
        Column::new(
            "name".to_string(),
            false,
            expand,
            ColumnSortable::Yes {
                sort_fn: Callback::new(move |_| {
                    let filters = filters_rws.get();
                    let current_sort_on = filters.sort_on.unwrap_or_default();
                    let sort_by = if current_sort_on == SortOn::Name {
                        filters.sort_by.unwrap_or_default().flip()
                    } else {
                        SortBy::Desc
                    };
                    let new_filters = VariableFilters {
                        sort_on: Some(SortOn::Name),
                        sort_by: Some(sort_by),
                        ..filters
                    };
                    pagination_params_rws.update(|f| f.reset_page());
                    filters_rws.set(new_filters);
                }),
                sort_by: current_sort_by.clone(),
                currently_sorted: current_sort_on == SortOn::Name,
            },
            Expandable::Disabled,
            |_| default_column_formatter("Variable Name"),
        ),
        Column::default("value".to_string()),
        Column::new(
            "created_at".to_string(),
            false,
            |value, _| {
                view! {
                    <DatetimeStr datetime=value.into() />
                }
            },
            ColumnSortable::Yes {
                sort_fn: Callback::new(move |_| {
                    let filters = filters_rws.get();
                    let current_sort_on = filters.sort_on.unwrap_or_default();
                    let sort_by = if current_sort_on == SortOn::CreatedAt {
                        filters.sort_by.unwrap_or_default().flip()
                    } else {
                        SortBy::Desc
                    };
                    let new_filters = VariableFilters {
                        sort_on: Some(SortOn::CreatedAt),
                        sort_by: Some(sort_by),
                        ..filters
                    };
                    pagination_params_rws.update(|f| f.reset_page());
                    filters_rws.set(new_filters);
                }),
                sort_by: current_sort_by.clone(),
                currently_sorted: current_sort_on == SortOn::CreatedAt,
            },
            Expandable::Enabled(100),
            default_column_formatter,
        ),
        Column::new(
            "last_modified_at".to_string(),
            false,
            |value, _| {
                view! {
                    <DatetimeStr datetime=value.into() />
                }
            },
            ColumnSortable::Yes {
                sort_fn: Callback::new(move |_| {
                    let filters = filters_rws.get();
                    let current_sort_on = filters.sort_on.unwrap_or_default();
                    let sort_by = if current_sort_on == SortOn::LastModifiedAt {
                        filters.sort_by.unwrap_or_default().flip()
                    } else {
                        SortBy::Desc
                    };
                    let new_filters = VariableFilters {
                        sort_on: Some(SortOn::LastModifiedAt),
                        sort_by: Some(sort_by),
                        ..filters
                    };
                    pagination_params_rws.update(|f| f.reset_page());
                    filters_rws.set(new_filters);
                }),
                sort_by: current_sort_by.clone(),
                currently_sorted: current_sort_on == SortOn::LastModifiedAt,
            },
            Expandable::Enabled(100),
            |_| default_column_formatter("Modified At"),
        ),
    ]
}

#[component]
pub fn VariablesList() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let filters_rws = use_signal_from_query(move |query_string| {
        Query::<VariableFilters>::extract_non_empty(&query_string).into_inner()
    });

    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
    });

    let action_rws = RwSignal::new(Action::None);

    use_param_updater(move || {
        box_params![pagination_params_rws.get(), filters_rws.get()]
    });

    let variables_resource = create_blocking_resource(
        move || {
            (
                workspace.get().0,
                pagination_params_rws.get(),
                org.get().0,
                filters_rws.get(),
            )
        },
        |(workspace_id, pagination_params, org_id, filters)| async move {
            variables::fetch(&filters, &pagination_params, &workspace_id, &org_id)
                .await
                .unwrap_or_default()
        },
    );

    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    });

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
            {move || {
                let variables = variables_resource.get().unwrap_or_default();
                let table_rows = variables
                    .data
                    .into_iter()
                    .map(|var| json!(var).as_object().unwrap().to_owned())
                    .collect::<Vec<Map<String, Value>>>();
                let total_variables = variables.total_items.to_string();
                let pagination_params = pagination_params_rws.get();
                let current_page = pagination_params.page.unwrap_or_default();
                let total_pages = variables.total_pages;
                let pagination_props = TablePaginationProps {
                    enabled: true,
                    count: pagination_params.count.unwrap_or_default(),
                    current_page,
                    total_pages,
                    on_page_change: handle_page_change,
                };

                view! {
                    <div class="h-full flex flex-col gap-4">
                        <div class="flex justify-between">
                            <Stat
                                heading="Variables"
                                icon="ri-braces-line"
                                number=total_variables
                            />
                            <div class="flex items-end gap-4">
                                <VariableFilterWidget filters_rws pagination_params_rws />
                                <Button
                                    on_click=move |_| action_rws.set(Action::Create)
                                    text="Create Variable"
                                    icon_class="ri-add-line"
                                />
                            </div>
                        </div>
                        <FilterSummary filters_rws />
                        <div class="card w-full bg-base-100 rounded-lg overflow-hidden shadow">
                            <div class="card-body overflow-y-auto overflow-x-visible">
                                <Table
                                    class="!overflow-y-auto"
                                    rows=table_rows
                                    key_column="name"
                                    columns=variable_table_columns(
                                        filters_rws,
                                        pagination_params_rws,
                                    )
                                    pagination=pagination_props
                                />
                            </div>
                        </div>
                    </div>
                }
            }}
            {move || match action_rws.get() {
                Action::Create => {
                    view! {
                        <PortalDrawer
                            title="Create Variable"
                            handle_close=move |_| action_rws.set(Action::None)
                        >
                            <VariableForm
                                edit=false
                                handle_submit=move |_| {
                                    filters_rws.set(VariableFilters::default());
                                    pagination_params_rws.update(|f| f.reset_page());
                                    variables_resource.refetch();
                                    action_rws.set(Action::None);
                                }
                            />
                        </PortalDrawer>
                    }
                        .into_view()
                }
                Action::None => view! {}.into_view(),
            }}
        </Suspense>
    }
}
