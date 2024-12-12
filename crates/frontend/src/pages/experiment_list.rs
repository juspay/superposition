pub mod utils;

use std::collections::HashSet;

use chrono::DateTime;
use futures::join;
use leptos::*;

use chrono::prelude::Utc;
use serde::{Deserialize, Serialize};

use crate::components::condition_pills::utils::extract_conditions;
use crate::components::context_form::utils::construct_context;
use crate::components::drawer::{close_drawer, Drawer, DrawerBtn};
use crate::components::dropdown::DropdownDirection;
use crate::components::skeleton::Skeleton;

use crate::components::{
    button::Button,
    context_form::ContextForm,
    experiment_form::ExperimentForm,
    input::DateInput,
    stat::Stat,
    table::{types::TablePaginationProps, Table},
};
use crate::providers::condition_collapse_provider::ConditionCollapseProvider;
use crate::providers::editor_provider::EditorProvider;
use crate::types::{
    ExperimentListFilters, ExperimentResponse, ExperimentStatusType, ListFilters,
    PaginatedResponse, StatusTypes,
};
use crate::utils::update_page_direction;

use self::utils::experiment_table_columns;
use crate::{
    api::{fetch_default_config, fetch_dimensions, fetch_experiments},
    types::{DefaultConfig, Dimension},
};
use serde_json::{json, Map, Value};

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CombinedResource {
    experiments: PaginatedResponse<ExperimentResponse>,
    dimensions: Vec<Dimension>,
    default_config: Vec<DefaultConfig>,
}

#[component]
fn experiment_table_filter_widget(
    filters_rws: RwSignal<ExperimentListFilters>,
    combined_resource: Resource<
        (String, ExperimentListFilters, ListFilters),
        CombinedResource,
    >,
) -> impl IntoView {
    let filters = filters_rws.get_untracked();
    let filters_buffer_rws = create_rw_signal(filters.clone());
    let context = filters
        .context
        .map(|i| extract_conditions(&i))
        .unwrap_or_default();

    let dim = combined_resource
        .get()
        .unwrap_or(CombinedResource {
            experiments: PaginatedResponse {
                total_items: 0,
                total_pages: 0,
                data: vec![],
            },
            dimensions: vec![],
            default_config: vec![],
        })
        .dimensions;

    let status_filter_management =
        move |checked: bool, filter_status: ExperimentStatusType| {
            let vec = filters_buffer_rws.get().status.unwrap_or_default();
            let mut old_status_vec: HashSet<ExperimentStatusType> =
                HashSet::from_iter(vec.clone().0);

            if checked {
                old_status_vec.insert(filter_status);
            } else {
                old_status_vec.remove(&filter_status);
            }
            let vector = old_status_vec.into_iter().collect();
            let filters = ExperimentListFilters {
                status: Some(StatusTypes(vector)),
                ..filters_buffer_rws.get()
            };
            filters_buffer_rws.set(filters)
        };
    view! {
        <DrawerBtn
            drawer_id="experiment_filter_drawer".into()
            style="cursor-pointer btn btn-purple-outline m-1".to_string()
        >
            Filters
            <i class="ri-filter-3-line"></i>
        </DrawerBtn>
        <Drawer
            id="experiment_filter_drawer".to_string()
            header="Experiment Filters"
            drawer_width="w-[50vw]"
            handle_close=move || {
                close_drawer("experiment_filter_drawer");
            }
        >
            <div class="card-body">
                <div class="my-4">
                    <ContextForm
                        dimensions=dim
                        context
                        dropdown_direction=DropdownDirection::Down
                        handle_change=move |context| {
                            let dimensions = combined_resource
                                .get()
                                .unwrap_or(CombinedResource {
                                    experiments: PaginatedResponse {
                                        total_items: 0,
                                        total_pages: 0,
                                        data: vec![],
                                    },
                                    dimensions: vec![],
                                    default_config: vec![],
                                })
                                .dimensions;
                            let json_context = construct_context(context, dimensions);
                            let json_context = if json_context.is_object()
                                && json_context.as_object().unwrap().is_empty()
                            {
                                None
                            } else {
                                Some(json_context)
                            };
                            let current_filters = filters_buffer_rws.get();
                            filters_buffer_rws
                                .set(ExperimentListFilters {
                                    context: json_context,
                                    ..current_filters
                                });
                        }
                        heading_sub_text=String::from("Search By Context")
                    />

                </div>
                <div class="w-full flex flex-row justify-start gap-10">
                    <div class="form-control">
                        <label class="label">
                            <span class="label-text">Last Modified From</span>
                        </label>
                        <DateInput
                            id="experiment_from_date_input".into()
                            class="w-[19rem] flex-auto mt-3 mr-3".into()
                            name="experiment_from_date".into()
                            value=filters
                                .from_date
                                .map(|s| s.format("%Y-%m-%d").to_string())
                                .unwrap_or_default()
                            on_change=Callback::new(move |new_date: DateTime<Utc>| {
                                let old_filters = filters_buffer_rws.get();
                                let new_filters = ExperimentListFilters {
                                    from_date: Some(new_date),
                                    ..old_filters
                                };
                                filters_buffer_rws.set(new_filters);
                            })
                        />
                    </div>
                    <i class="mt-5 mr-3 text-3xl ri-arrow-right-line"></i>
                    <div class="form-control">
                        <label class="label">
                            <span class="label-text">Last Modified To</span>
                        </label>
                        <DateInput
                            id="experiment_to_date_input".into()
                            class="w-[19rem] flex-auto mt-3 mr-3".into()
                            name="experiment_to_date".into()
                            on_change=Callback::new(move |new_date: DateTime<Utc>| {
                                let old_filters = filters_buffer_rws.get();
                                let new_filters = ExperimentListFilters {
                                    to_date: Some(new_date),
                                    ..old_filters
                                };
                                filters_buffer_rws.set(new_filters);
                            })
                        />
                    </div>
                </div>

                <div class="form-control w-full">
                    <label class="label">
                        <span class="label-text">Experiment Status</span>
                    </label>
                    <div class="flex flex-row justify-start gap-10">
                        <input
                            type="checkbox"
                            id="created-checkbox"
                            class="hidden peer/created-checkbox"
                            checked=move || {
                                filters_buffer_rws
                                    .get()
                                    .status
                                    .unwrap_or_default()
                                    .iter()
                                    .any(|item| *item == ExperimentStatusType::CREATED)
                            }
                            on:change=move |event| {
                                let checked = event_target_checked(&event);
                                status_filter_management(checked, ExperimentStatusType::CREATED)
                            }
                        />
                        <label
                            for="created-checkbox"
                            class="px-6 h-[30px] py-2 badge peer-checked/created-checkbox:badge-info cursor-pointer transition duration-300 ease-in-out"
                        >
                            Created
                        </label>

                        <input
                            type="checkbox"
                            id="inprogress-checkbox"
                            class="hidden peer/inprogress-checkbox"
                            checked=move || {
                                filters_buffer_rws
                                    .get()
                                    .status
                                    .unwrap_or_default()
                                    .iter()
                                    .any(|item| *item == ExperimentStatusType::INPROGRESS)
                            }
                            on:change=move |event| {
                                let checked = event_target_checked(&event);
                                status_filter_management(checked, ExperimentStatusType::INPROGRESS)
                            }
                        />
                        <label
                            for="inprogress-checkbox"
                            class="px-6 h-[30px] py-2 badge peer-checked/inprogress-checkbox:badge-warning cursor-pointer transition duration-300 ease-in-out"
                        >
                            InProgress
                        </label>

                        <input
                            type="checkbox"
                            id="conclude-checkbox"
                            class="hidden peer/conclude-checkbox"
                            checked=move || {
                                filters_buffer_rws
                                    .get()
                                    .status
                                    .unwrap_or_default()
                                    .iter()
                                    .any(|item| *item == ExperimentStatusType::CONCLUDED)
                            }
                            on:change=move |event| {
                                let checked = event_target_checked(&event);
                                status_filter_management(checked, ExperimentStatusType::CONCLUDED)
                            }
                        />
                        <label
                            for="conclude-checkbox"
                            class="px-6 h-[30px] py-2 badge peer-checked/conclude-checkbox:badge-success cursor-pointer transition duration-300 ease-in-out"
                        >
                            Concluded
                        </label>
                    </div>
                </div>
                <div class="flex flex-col gap-1 justify-between">
                    <div class="form-control">
                        <label class="label">
                            <span class="label-text">Experiment Name</span>
                        </label>
                        <input
                            type="text"
                            id="experiment-name-filter"
                            placeholder="eg: city experiment"
                            class="input input-bordered rounded-md resize-y w-full max-w-md"
                            value=move || filters_rws.get().experiment_name
                            on:change=move |event| {
                                let experiment_name = event_target_value(&event);
                                let experiment_name = if experiment_name.is_empty() {
                                    None
                                } else {
                                    Some(experiment_name)
                                };
                                let filters = filters_buffer_rws.get();
                                let filters = ExperimentListFilters {
                                    experiment_name: experiment_name,
                                    ..filters
                                };
                                filters_buffer_rws.set(filters);
                            }
                        />
                    </div>
                    <div class="form-control mt-4">
                        <label class="label">
                            <span class="label-text">Experiment IDs (Seperate by Comma)</span>
                        </label>
                        <input
                            type="text"
                            id="experiment-id-filter"
                            class="input input-bordered rounded-md resize-y w-full max-w-md"
                            value=move || filters_rws.get().experiment_ids
                            placeholder="eg: 7259558160762015744"
                            on:change=move |event| {
                                let id = event_target_value(&event);
                                let id = if id.is_empty() { None } else { Some(id) };
                                let filters = filters_buffer_rws.get();
                                let filters = ExperimentListFilters {
                                    experiment_ids: id,
                                    ..filters
                                };
                                filters_buffer_rws.set(filters);
                            }
                        />
                    </div>
                    <div class="form-control mt-4">
                        <label class="label">
                            <span class="label-text">Created By (Seperate by Comma)</span>
                        </label>
                        <input
                            type="text"
                            id="experiment-user-filter"
                            class="input input-bordered rounded-md resize-y w-full max-w-md"
                            placeholder="eg: user@superposition.io"
                            value=move || filters_rws.get().created_by
                            on:change=move |event| {
                                let user_names = event_target_value(&event);
                                let user_names = if user_names.is_empty() {
                                    None
                                } else {
                                    Some(user_names)
                                };
                                let filters = filters_buffer_rws.get();
                                let filters = ExperimentListFilters {
                                    created_by: user_names,
                                    ..filters
                                };
                                filters_buffer_rws.set(filters);
                            }
                        />

                    </div>
                </div>
                <div class="flex justify-start mt-8">
                    <Button
                        class="pl-[70px] pr-[70px] w-48 h-12".to_string()
                        text="Submit".to_string()
                        on_click=move |event| {
                            event.prevent_default();
                            let filter = filters_buffer_rws.get();
                            filters_rws.set(filter);
                            close_drawer("experiment_filter_drawer")
                        }
                    />
                    <Button
                        class="pl-[70px] pr-[70px] w-48 h-12".to_string()
                        text="Reset".to_string()
                        on_click=move |event| {
                            event.prevent_default();
                            let filters = ExperimentListFilters::default();
                            filters_rws.set(filters);
                            close_drawer("experiment_filter_drawer")
                        }
                    />

                </div>
            </div>
        </Drawer>
    }
}

#[component]
pub fn experiment_list() -> impl IntoView {
    // acquire tenant
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let filters_rws = create_rw_signal(ExperimentListFilters::default());

    let (pagination_filters_rs, pagination_filters_ws) = create_signal(ListFilters {
        page: Some(1),
        count: Some(10),
        all: None,
    });

    let (reset_exp_form, set_exp_form) = create_signal(0);

    let combined_resource: Resource<
        (String, ExperimentListFilters, ListFilters),
        CombinedResource,
    > = create_blocking_resource(
        move || {
            (
                tenant_rs.get(),
                filters_rws.get(),
                pagination_filters_rs.get(),
            )
        },
        |(current_tenant, filters, pagination_filters)| async move {
            // Perform all fetch operations concurrently
            let experiments_future = fetch_experiments(
                &filters,
                &pagination_filters,
                current_tenant.to_string(),
            );
            let dimensions_future =
                fetch_dimensions(&pagination_filters, current_tenant.to_string());
            let config_future =
                fetch_default_config(&pagination_filters, current_tenant.to_string());

            let (experiments_result, dimensions_result, config_result) =
                join!(experiments_future, dimensions_future, config_future);
            // Construct the combined result, handling errors as needed
            CombinedResource {
                experiments: experiments_result.unwrap_or(PaginatedResponse::default()),
                dimensions: dimensions_result
                    .unwrap_or(PaginatedResponse::default())
                    .data
                    .into_iter()
                    .filter(|d| d.dimension != "variantIds")
                    .collect(),
                default_config: config_result
                    .unwrap_or(PaginatedResponse::default())
                    .data,
            }
        },
    );

    let handle_submit_experiment_form = move || {
        combined_resource.refetch();
        set_exp_form.update(|val| {
            *val += 1;
        });
        close_drawer("create_exp_drawer");
    };

    let handle_next_click = Callback::new(move |total_pages: i64| {
        pagination_filters_ws.update(|f| {
            f.page = update_page_direction(f.page, total_pages, true);
        });
    });

    let handle_prev_click = Callback::new(move |_| {
        pagination_filters_ws.update(|f| {
            f.page = update_page_direction(f.page, 1, false);
        });
    });

    view! {
        <div class="p-8">
            <Suspense fallback=move || view! { <Skeleton /> }>
                <div class="pb-4">

                    {move || {
                        let value = combined_resource.get();
                        let total_items = match value {
                            Some(v) => v.experiments.total_items.to_string(),
                            _ => "0".to_string(),
                        };
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
                            <h2 class="card-title">Experiments</h2>
                            <div>
                                <DrawerBtn drawer_id="create_exp_drawer"
                                    .to_string()>
                                    Create Experiment <i class="ri-edit-2-line ml-2"></i>
                                </DrawerBtn>
                            </div>
                        </div>
                        {move || {
                            let value = combined_resource.get();
                            let pagination = pagination_filters_rs.get();
                            let table_columns = experiment_table_columns(filters_rws);
                            match value {
                                Some(v) => {
                                    let data = v
                                        .experiments
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
                                                    json!(ele.created_at.format("%v").to_string()),
                                                );
                                            ele_map
                                                .insert(
                                                    "last_modified".to_string(),
                                                    json!(ele.last_modified.format("%v").to_string()),
                                                );
                                            ele_map
                                        })
                                        .collect::<Vec<Map<String, Value>>>()
                                        .to_owned();
                                    let pagination_props = TablePaginationProps {
                                        enabled: true,
                                        count: pagination.count.unwrap_or_default(),
                                        current_page: pagination.page.unwrap_or_default(),
                                        total_pages: v.experiments.total_pages,
                                        on_next: handle_next_click,
                                        on_prev: handle_prev_click,
                                    };
                                    view! {
                                        <ConditionCollapseProvider>
                                            <div class="flex justify-start m-1">
                                                <ExperimentTableFilterWidget
                                                    filters_rws
                                                    combined_resource
                                                />
                                            </div>
                                            <Table
                                                cell_class="min-w-48 font-mono".to_string()
                                                rows=data
                                                key_column="id".to_string()
                                                columns=table_columns
                                                pagination=pagination_props
                                            />
                                        </ConditionCollapseProvider>
                                    }
                                }
                                None => view! { <div>Loading....</div> }.into_view(),
                            }
                        }}

                    </div>
                </div>

                {move || {
                    let dim = combined_resource
                        .get()
                        .unwrap_or(CombinedResource {
                            experiments: PaginatedResponse::default(),
                            dimensions: vec![],
                            default_config: vec![],
                        })
                        .dimensions;
                    let def_conf = combined_resource
                        .get()
                        .unwrap_or(CombinedResource {
                            experiments: PaginatedResponse::default(),
                            dimensions: vec![],
                            default_config: vec![],
                        })
                        .default_config;
                    let _ = reset_exp_form.get();
                    view! {
                        <Drawer
                            id="create_exp_drawer".to_string()
                            header="Create New Experiment"
                            handle_close=move || {
                                close_drawer("create_exp_drawer");
                                set_exp_form.update(|i| *i += 1);
                            }
                        >

                            <EditorProvider>
                                <ExperimentForm
                                    name="".to_string()
                                    context=vec![]
                                    variants=vec![]
                                    dimensions=dim.clone()
                                    default_config=def_conf.clone()
                                    handle_submit=handle_submit_experiment_form
                                />
                            </EditorProvider>
                        </Drawer>
                    }
                }}

            </Suspense>
        </div>
    }
}
