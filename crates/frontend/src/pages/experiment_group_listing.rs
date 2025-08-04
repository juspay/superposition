mod filter;

use std::time::Duration;

use filter::{ExperimentGroupFilterWidget, FilterSummary};
use leptos::*;
use leptos_router::A;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use superposition_macros::box_params;
use superposition_types::{
    api::{
        dimension::DimensionResponse,
        experiment_groups::{ExpGroupFilters, SortOn},
        workspace::WorkspaceResponse,
    },
    custom_query::{CustomQuery, PaginationParams, Query},
    database::models::experimentation::ExperimentGroup,
    PaginatedResponse,
};
use web_sys::MouseEvent;

use crate::{
    api::{
        experiment_groups::{delete, fetch_all},
        fetch_dimensions,
    },
    components::{
        condition_pills::Condition as ConditionComponent,
        drawer::{close_drawer, Drawer, DrawerBtn},
        experiment_group_form::{ChangeLogSummary, ChangeType, ExperimentGroupForm},
        skeleton::Skeleton,
        stat::Stat,
        table::{
            types::{
                default_column_formatter, default_formatter, Column, ColumnSortable,
                Expandable, TablePaginationProps,
            },
            Table,
        },
    },
    logic::Conditions,
    providers::{
        condition_collapse_provider::ConditionCollapseProvider,
        editor_provider::EditorProvider,
    },
    query_updater::{use_param_updater, use_signal_from_query},
    types::{OrganisationId, Tenant},
};

#[derive(Serialize, Deserialize, Clone, Default)]
struct CombinedResource {
    experiment_groups: PaginatedResponse<ExperimentGroup>,
    dimensions: Vec<DimensionResponse>,
}

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub id: String,
    pub name: String,
    pub description: String,
    pub context: Conditions,
    pub traffic_percentage: i32,
}

fn table_columns(filters_rws: RwSignal<ExpGroupFilters>) -> Vec<Column> {
    let workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();
    let current_filters = filters_rws.get();
    let current_sort_on = current_filters.sort_on.unwrap_or_default();
    let current_sort_by = current_filters.sort_by.unwrap_or_default();
    let get_row_string_fn = |row: &Map<String, Value>, key: &str| -> String {
        row.get(key)
            .and_then(Value::as_str)
            .map(String::from)
            .unwrap_or_default()
    };

    vec![
        Column::new(
            "name".to_string(),
            false,
            move |value: &str, row: &Map<String, Value>| {
                let group_name = value.to_string();
                let (copied_rs, copied_ws) = create_signal(false);
                let group_id = get_row_string_fn(row, "id");
                let group_id_copy = group_id.clone();
                let handle_copy = move |event: MouseEvent| {
                    event.prevent_default();

                    let copy_code =
                        format!("navigator.clipboard.writeText('{}')", &group_id_copy);
                    match js_sys::eval(&copy_code) {
                        Ok(_) => {
                            copied_ws.set(true);
                            set_timeout(
                                move || {
                                    copied_ws.set(false);
                                },
                                Duration::new(1, 0),
                            );
                        }
                        Err(_) => logging::log!("unable to copy to clipboard"),
                    }
                };

                view! {
                    <div>
                        <A href=group_id.to_string() class="text-blue-500 underline underline-offset-2">
                            {group_name}
                        </A>
                        <div class="text-gray-500">
                            <span class="text-xs">
                                {group_id}
                            </span>
                            <i class="ri-file-copy-line ml-2 cursor-pointer" on:click:undelegated=handle_copy></i>
                            <Show when=move || copied_rs.get()>
                                <div class="w-fit ml-2 px-2 flex justify-center items-center bg-gray-600 rounded-xl">
                                    <span class="text-white text-xs font-semibold">
                                        "copied!"
                                    </span>
                                </div>
                            </Show>
                        </div>
                    </div>
                }.into_view()
            },
            ColumnSortable::Yes {
                sort_fn: Callback::new(move |_| {
                    let filters = filters_rws.get();
                    let sort_by = filters.sort_by.unwrap_or_default().flip();
                    let new_filters = ExpGroupFilters {
                        sort_on: Some(SortOn::Name),
                        sort_by: Some(sort_by),
                        ..filters
                    };
                    filters_rws.set(new_filters);
                }),
                sort_by: current_sort_by.clone(),
                currently_sorted: current_sort_on == SortOn::Name,
            },
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::new(
            "context".to_string(),
            false,
            move |_, row: &Map<String, Value>| {
                let context = row
                    .get("context")
                    .and_then(|v| v.as_object().cloned())
                    .unwrap_or_default();
                let id = get_row_string_fn(row, "id");
                let conditions =
                    Conditions::from_context_json(&context).unwrap_or_default();

                view! {
                    <ConditionComponent conditions grouped_view=false id class="w-[300px]" strict_mode=workspace_settings.with_value(|w| w.strict_mode)  />
                }
                .into_view()
            },
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::default_with_cell_formatter(
            "traffic_percentage".to_string(),
            |value: &str, _| {
                let percentage = format!("{}%", value);
                view! {
                    <span class="badge badge-warning badge-lg text-white">
                        {percentage}
                    </span>
                }
                .into_view()
            },
        ),
        Column::default("group_type".to_string()),
        Column::default_with_sort(
            "created_at".to_string(),
            ColumnSortable::Yes {
                sort_fn: Callback::new(move |_| {
                    let filters = filters_rws.get();
                    let sort_by = filters.sort_by.unwrap_or_default().flip();
                    let new_filters = ExpGroupFilters {
                        sort_on: Some(SortOn::CreatedAt),
                        sort_by: Some(sort_by),
                        ..filters
                    };
                    filters_rws.set(new_filters);
                }),
                sort_by: current_sort_by.clone(),
                currently_sorted: current_sort_on == SortOn::CreatedAt,
            },
        ),
        Column::default("created_by".to_string()),
        Column::new(
            "last_modified_at".to_string(),
            false,
            default_formatter,
            ColumnSortable::Yes {
                sort_fn: Callback::new(move |_| {
                    let filters = filters_rws.get();
                    let sort_by = filters.sort_by.unwrap_or_default().flip();
                    let new_filters = ExpGroupFilters {
                        sort_on: Some(SortOn::LastModifiedAt),
                        sort_by: Some(sort_by),
                        ..filters
                    };
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
pub fn experiment_group_listing() -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let delete_inprogress_rws = RwSignal::new(false);

    let filters_rws = use_signal_from_query(move |query_string| {
        Query::<ExpGroupFilters>::extract_non_empty(&query_string).into_inner()
    });
    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
    });

    use_param_updater(move || {
        box_params!(pagination_params_rws.get(), filters_rws.get())
    });

    let delete_group_id_rws: RwSignal<Option<String>> = RwSignal::new(None);
    let selected_group_rws: RwSignal<Option<RowData>> = RwSignal::new(None);

    let source = move || {
        (
            filters_rws.get(),
            pagination_params_rws.get(),
            workspace.get().0,
            org.get().0,
        )
    };

    let experiment_groups_resource = create_blocking_resource(
        source,
        |(filters, pagination, tenant, org_id)| async move {
            let experiment_groups = fetch_all(&filters, &pagination, &tenant, &org_id)
                .await
                .unwrap_or_default();
            let dimensions =
                fetch_dimensions(&PaginationParams::all_entries(), tenant, org_id)
                    .await
                    .unwrap_or_default()
                    .data
                    .into_iter()
                    .filter(|d| d.dimension != "variantIds")
                    .collect();
            CombinedResource {
                experiment_groups,
                dimensions,
            }
        },
    );

    let handle_experiment_group_create = move |_| {
        filters_rws.set(ExpGroupFilters::default());
        pagination_params_rws.update(|f| f.reset_page());
        experiment_groups_resource.refetch();
        selected_group_rws.set(None);
        close_drawer("create_exp_group_drawer");
    };

    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    });

    let confirm_delete = Callback::new(move |_| {
        if let Some(id) = delete_group_id_rws.get_untracked() {
            delete_inprogress_rws.set(true);
            spawn_local(async move {
                let result =
                    delete(&id, &workspace.get_untracked(), &org.get_untracked()).await;
                delete_inprogress_rws.set(false);
                match result {
                    Ok(_) => {
                        logging::log!("Experiment Group deleted successfully");
                        delete_group_id_rws.set(None);
                        experiment_groups_resource.refetch();
                    }
                    Err(e) => {
                        logging::log!("Error deleting Experiment Group: {:?}", e);
                    }
                }
            });
        }
    });

    view! {
        <Suspense fallback=move || view! { <Skeleton /> }>
            <div class="h-full flex flex-col gap-4">
                {move || {
                    let total_items = experiment_groups_resource
                        .with(|c| c.as_ref().map(|r| r.experiment_groups.total_items))
                        .unwrap_or_default()
                        .to_string();
                    view! {
                        <div class="flex justify-between">
                            <Stat
                                heading="Experiment Groups"
                                icon="ri-flask-fill"
                                number=total_items
                            />
                            <div class="flex items-end gap-4">
                                <ExperimentGroupFilterWidget filters_rws pagination_params_rws />
                                <DrawerBtn
                                    drawer_id="create_exp_group_drawer".to_string()
                                    text="Create Group"
                                    icon_class="ri-add-line"
                                />
                            </div>
                        </div>
                    }
                }} <FilterSummary filters_rws />
                <div class="card w-full bg-base-100 rounded-xl overflow-hidden shadow">
                    <div class="card-body overflow-y-auto overflow-x-visible">
                        {move || {
                            let value = experiment_groups_resource.get();
                            let pagination_params = pagination_params_rws.get();
                            let table_columns = table_columns(filters_rws);
                            match value {
                                Some(v) => {
                                    let data = v
                                        .experiment_groups
                                        .data
                                        .iter()
                                        .map(|ele| {
                                            let mut ele_map = json!(ele)
                                                .as_object()
                                                .unwrap()
                                                .to_owned();
                                            ele_map
                                                .insert(
                                                    "created_at".to_string(),
                                                    json!(ele.created_at.format("%v %T").to_string()),
                                                );
                                            ele_map
                                                .insert(
                                                    "last_modified_at".to_string(),
                                                    json!(ele.last_modified_at.format("%v %T").to_string()),
                                                );
                                            ele_map
                                        })
                                        .collect::<Vec<Map<String, Value>>>()
                                        .to_owned();
                                    let pagination_props = TablePaginationProps {
                                        enabled: true,
                                        count: pagination_params.count.unwrap_or_default(),
                                        current_page: pagination_params.page.unwrap_or_default(),
                                        total_pages: v.experiment_groups.total_pages,
                                        on_page_change: handle_page_change,
                                    };
                                    view! {
                                        <ConditionCollapseProvider>
                                            <Table
                                                class="!overflow-y-auto"
                                                rows=data
                                                key_column="id"
                                                columns=table_columns
                                                pagination=pagination_props
                                            />
                                        </ConditionCollapseProvider>
                                    }
                                        .into_view()
                                }
                                None => view! { Loading.... }.into_view(),
                            }
                        }}

                    </div>
                </div>
            </div>
            {move || {
                let resource = experiment_groups_resource.get();
                let experiment_group = selected_group_rws.get();
                let is_edit = experiment_group.is_some();
                let header = if is_edit {
                    "Edit Experiment Group"
                } else {
                    "Create New Experiment Group"
                };
                let group = experiment_group.unwrap_or_default();
                view! {
                    <Drawer
                        id="create_exp_group_drawer"
                        width_class="max-w-[720px] min-w-[560px] w-[45vw]"
                        header=header
                        handle_close=move || {
                            selected_group_rws.set(None);
                            close_drawer("create_exp_group_drawer")
                        }
                    >
                        <EditorProvider>
                            <ExperimentGroupForm
                                group_id=group.id
                                context=group.context
                                group_name=group.name
                                group_description=group.description
                                traffic_percentage=group.traffic_percentage
                                handle_submit=handle_experiment_group_create
                                dimensions=resource.map(|r| r.dimensions).unwrap_or_default()
                                is_edit
                            />
                        </EditorProvider>
                    </Drawer>
                }
            }}
            {move || {
                if let Some(group_id) = delete_group_id_rws.get() {
                    view! {
                        <ChangeLogSummary
                            group_id=group_id.clone()
                            change_type=ChangeType::Delete
                            on_close=move |_| delete_group_id_rws.set(None)
                            on_confirm=confirm_delete
                            inprogress=delete_inprogress_rws
                        />
                    }
                } else {
                    ().into_view()
                }
            }}
        </Suspense>
    }
}
