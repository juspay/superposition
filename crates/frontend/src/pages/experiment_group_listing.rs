use std::time::Duration;

use leptos::*;
use leptos_router::A;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use superposition_macros::box_params;
use superposition_types::{
    api::{
        experiment_groups::{ExpGroupFilters, SortOn},
        workspace::WorkspaceResponse,
    },
    custom_query::{CustomQuery, PaginationParams, Query},
    database::{models::experimentation::ExperimentGroup, types::DimensionWithMandatory},
    PaginatedResponse,
};
use web_sys::MouseEvent;

use crate::{
    api::{
        experiment_groups::{delete, fetch_all},
        fetch_dimensions,
    },
    components::{
        alert::AlertType,
        button::Button,
        condition_pills::Condition as ConditionComponent,
        delete_modal::DeleteModal,
        description_icon::InfoDescription,
        drawer::{close_drawer, open_drawer, Drawer, DrawerBtn, DrawerButtonStyle},
        experiment_group_form::ExperimentGroupForm,
        form::label::Label,
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
    logic::Conditions,
    providers::{
        alert_provider::enqueue_alert,
        condition_collapse_provider::ConditionCollapseProvider,
        editor_provider::EditorProvider,
    },
    query_updater::{use_param_updater, use_signal_from_query},
    types::{OrganisationId, Tenant},
};

#[derive(Serialize, Deserialize, Clone, Default)]
struct CombinedResource {
    experiment_groups: PaginatedResponse<ExperimentGroup>,
    dimensions: Vec<DimensionWithMandatory>,
}

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub id: String,
    pub name: String,
    pub description: String,
    pub context: Conditions,
    pub traffic_percentage: i32,
}

fn table_columns(
    delete_modal_ws: WriteSignal<bool>,
    delete_group_rws: RwSignal<String>,
    selected_group_rws: RwSignal<Option<RowData>>,
    filters_rws: RwSignal<ExpGroupFilters>,
) -> Vec<Column> {
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
                let description = get_row_string_fn(row, "description");

                let change_reason = get_row_string_fn(row, "change_reason");
                view! {
                    <div>
                        <A href=group_id.to_string() class="btn-link">
                            {group_name}
                        </A>
                        <div class="text-gray-500">
                            <span class="text-xs">
                                {group_id}
                            </span>
                            <i class="ri-file-copy-line cursor-pointer ml-2" on:click:undelegated=handle_copy></i>
                            <Show when=move || copied_rs.get()>
                                <div class="inline-block bg-gray-600 ml-2 rounded-xl px-2">
                                    <span class="text-white text-xs font-semibold">
                                        "copied!"
                                    </span>
                                </div>
                            </Show>
                        </div>
                    </div>
                    <InfoDescription description=description change_reason=change_reason />
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
        Column::default_with_sort(
            "last_modified_at".to_string(),
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
        ),
        Column::default("last_modified_by".to_string()),
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
            "actions".to_string(),
            false,
            move |_, row: &Map<String, Value>| {
                let group = RowData {
                    id: get_row_string_fn(row, "id"),
                    name: get_row_string_fn(row, "name"),
                    description: get_row_string_fn(row, "description"),
                    context: Conditions::from_context_json(
                        &row.get("context")
                            .and_then(|v| v.as_object())
                            .cloned()
                            .unwrap_or_default(),
                    )
                    .unwrap_or_default(),
                    traffic_percentage: row
                        .get("traffic_percentage")
                        .and_then(|v| v.as_number())
                        .and_then(|p| p.as_i64().map(|i| i as i32))
                        .unwrap_or_default(),
                };
                let group = StoredValue::new(group);
                let edit_click_handler = move |_| {
                    let group = group.get_value();
                    logging::log!("{:?}", group);
                    selected_group_rws.set(Some(group));
                    open_drawer("create_exp_group_drawer");
                };

                let handle_delete = move |_| {
                    delete_group_rws.set(group.get_value().id);
                    delete_modal_ws.set(true);
                };

                view! {
                    <div class="join">
                        <span class="cursor-pointer" on:click=edit_click_handler>
                            <i class="ri-pencil-line ri-xl text-blue-500"></i>
                        </span>
                        <span class="cursor-pointer text-red-500" on:click=handle_delete>
                            <i class="ri-delete-bin-5-line ri-xl text-red-500"></i>
                        </span>
                    </div>
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
fn experiment_group_filter_widget(
    pagination_params_rws: RwSignal<PaginationParams>,
    filters_rws: RwSignal<ExpGroupFilters>,
) -> impl IntoView {
    let filters = filters_rws.get_untracked();
    let filters_buffer_rws = create_rw_signal(filters.clone());

    view! {
        <DrawerBtn
            drawer_id="experiment_group_filter_drawer".into()
            style=DrawerButtonStyle::Outline
        >
            Filters
            <i class="ri-filter-3-line"></i>
        </DrawerBtn>
        <Drawer
            id="experiment_group_filter_drawer".to_string()
            header="Experiment Group Filters"
            drawer_width="w-[50vw]"
            handle_close=move || close_drawer("experiment_group_filter_drawer")
        >
            <div class="flex flex-col gap-5">
                <div class="flex flex-col gap-5 justify-between">
                    <div class="form-control">
                        <Label title="Experiment Group Name" />
                        <input
                            type="text"
                            id="experiment-group-name-filter"
                            placeholder="eg: city experiment group"
                            class="input input-bordered rounded-md resize-y w-full max-w-md"
                            value=move || filters_buffer_rws.get().name.unwrap_or_default()
                            on:change=move |event| {
                                let name = event_target_value(&event);
                                let group_name = if name.trim().is_empty() {
                                    None
                                } else {
                                    Some(name)
                                };
                                filters_buffer_rws.update(|f| f.name = group_name);
                            }
                        />
                    </div>
                    <div class="form-control">
                        <Label title="Created By" />
                        <input
                            type="text"
                            id="experiment-group-created-by-filter"
                            class="input input-bordered rounded-md resize-y w-full max-w-md"
                            value=move || filters_buffer_rws.get().created_by.unwrap_or_default()
                            placeholder="eg: user@superposition.io"
                            on:change=move |event| {
                                let created_by = event_target_value(&event);
                                let created_by = if created_by.trim().is_empty() {
                                    None
                                } else {
                                    Some(created_by)
                                };
                                filters_buffer_rws.update(|filter| filter.created_by = created_by);
                            }
                        />
                    </div>
                    <div class="form-control">
                        <Label title="Last Modified By" />
                        <input
                            type="text"
                            id="experiment-last-modified-filter"
                            class="input input-bordered rounded-md resize-y w-full max-w-md"
                            placeholder="eg: user@superposition.io"
                            value=move || filters_buffer_rws.get().last_modified_by
                            on:change=move |event| {
                                let last_modified = event_target_value(&event);
                                let last_modified_by = if last_modified.trim().is_empty() {
                                    None
                                } else {
                                    Some(last_modified)
                                };
                                filters_buffer_rws
                                    .update(|filter| filter.last_modified_by = last_modified_by);
                            }
                        />

                    </div>
                </div>
                <div class="flex justify-end gap-2">
                    <Button
                        class="h-12 w-48"
                        text="Submit"
                        icon_class="ri-send-plane-line"
                        on_click=move |event| {
                            event.prevent_default();
                            let filter = filters_buffer_rws.get();
                            pagination_params_rws
                                .update(|f| {
                                    filters_rws.set_untracked(filter);
                                    f.reset_page()
                                });
                            close_drawer("experiment_group_filter_drawer")
                        }
                    />
                    <Button
                        class="h-12 w-48"
                        text="Reset"
                        icon_class="ri-restart-line"
                        on_click=move |event| {
                            event.prevent_default();
                            let filters = ExpGroupFilters::default();
                            filters_buffer_rws.set(filters.clone());
                            pagination_params_rws
                                .update(|f| {
                                    filters_rws.set_untracked(filters);
                                    f.reset_page()
                                });
                            close_drawer("experiment_group_filter_drawer")
                        }
                    />
                </div>
            </div>
        </Drawer>
    }
}

#[component]
pub fn experiment_group_listing() -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let filters_rws = use_signal_from_query(move |query_string| {
        Query::<ExpGroupFilters>::extract_non_empty(&query_string).into_inner()
    });
    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
    });

    use_param_updater(move || {
        box_params!(pagination_params_rws.get(), filters_rws.get())
    });

    let (delete_modal_rs, delete_modal_ws) = create_signal(false);
    let delete_group_id_rws = create_rw_signal(String::new());
    let selected_group_rws: RwSignal<Option<RowData>> = create_rw_signal(None);

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

    let handle_experiment_group_create = Callback::new(move |_| {
        filters_rws.set(ExpGroupFilters::default());
        pagination_params_rws.update(|f| f.reset_page());
        experiment_groups_resource.refetch();
        selected_group_rws.set(None);
        close_drawer("create_exp_group_drawer");
    });

    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    });

    view! {
        <div class="p-8">
            <Suspense fallback=move || view! { <Skeleton /> }>
                <div class="pb-4">

                    {move || {
                        let value = experiment_groups_resource.get();
                        let total_items = value
                            .map(|v| v.experiment_groups.total_items)
                            .unwrap_or(0)
                            .to_string();
                        view! {
                            <Stat
                                heading="Experiments"
                                icon="ri-test-tube-fill"
                                number=total_items
                            />
                        }
                    }}

                </div>
                <div class="card rounded-xl w-full bg-base-100 shadow">
                    <div class="card-body">
                        <div class="flex justify-between">
                            <h2 class="card-title">"Experiment Groups"</h2>
                            <DrawerBtn drawer_id="create_exp_group_drawer"
                                .to_string()>
                                Create Group <i class="ri-edit-2-line ml-2"></i>
                            </DrawerBtn>
                        </div>
                        {move || {
                            let value = experiment_groups_resource.get();
                            let pagination_params = pagination_params_rws.get();
                            let table_columns = table_columns(
                                delete_modal_ws,
                                delete_group_id_rws,
                                selected_group_rws,
                                filters_rws,
                            );
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
                                                    "last_modified".to_string(),
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
                                            <ExperimentGroupFilterWidget
                                                filters_rws
                                                pagination_params_rws
                                            />
                                            <Table
                                                rows=data
                                                key_column="name".to_string()
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
                            id="create_exp_group_drawer".to_string()
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

                <DeleteModal
                    modal_visible=delete_modal_rs
                    confirm_delete=Callback::new(move |_| {
                        let group_id = delete_group_id_rws.get();
                        if group_id.is_empty() {
                            enqueue_alert(
                                "No experiment group selected for deletion".to_string(),
                                AlertType::Error,
                                5000,
                            );
                            return;
                        }
                        spawn_local(async move {
                            let tenant = workspace.get().0;
                            let org_id = org.get().0;
                            if let Err(e) = delete(&group_id, &tenant, &org_id).await {
                                logging::error!("Failed to delete experiment group: {}", e);
                                enqueue_alert(
                                    format!(
                                        "Failed to delete experiment group {}: {}",
                                        group_id,
                                        e,
                                    ),
                                    AlertType::Error,
                                    5000,
                                );
                            }
                        });
                        enqueue_alert(
                            "Experiment group deleted successfully".to_string(),
                            AlertType::Success,
                            5000,
                        );
                        delete_modal_ws.set(false);
                        experiment_groups_resource.refetch();
                    })
                    set_modal_visible=delete_modal_ws
                    header_text="Are you sure you want to delete this config? Action is irreversible."
                        .to_string()
                />

            </Suspense>
        </div>
    }
}
