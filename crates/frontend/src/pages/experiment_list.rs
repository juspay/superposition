pub mod utils;

use std::collections::HashSet;

use chrono::{prelude::Utc, DateTime, Days, Duration};
use futures::join;
use leptos::*;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use strum::IntoEnumIterator;
use superposition_macros::box_params;
use superposition_types::{
    api::{
        default_config::DefaultConfigFilters,
        experiments::{ExperimentListFilters, ExperimentResponse},
        workspace::WorkspaceResponse,
    },
    custom_query::{CommaSeparatedQParams, CustomQuery, PaginationParams, Query},
    database::{
        models::{cac::DefaultConfig, experimentation::ExperimentStatusType},
        types::DimensionWithMandatory,
    },
    PaginatedResponse,
};
use utils::experiment_table_columns;

use crate::{
    api::{fetch_default_config, fetch_dimensions, fetch_experiments},
    components::{
        button::Button,
        context_form::ContextForm,
        drawer::{close_drawer, Drawer, DrawerBtn, DrawerButtonStyle},
        dropdown::DropdownDirection,
        experiment_form::ExperimentForm,
        input::DateInput,
        skeleton::Skeleton,
        stat::Stat,
        table::{types::TablePaginationProps, Table},
    },
    logic::{Condition, Conditions},
    providers::{
        condition_collapse_provider::ConditionCollapseProvider,
        editor_provider::EditorProvider,
    },
    query_updater::{use_param_updater, use_signal_from_query},
    types::{OrganisationId, Tenant, VariantFormTs},
};

#[derive(Serialize, Deserialize, Clone, Default)]
struct CombinedResource {
    experiments: PaginatedResponse<ExperimentResponse>,
    dimensions: Vec<DimensionWithMandatory>,
    default_config: Vec<DefaultConfig>,
}

#[component]
fn experiment_table_filter_widget(
    pagination_params_rws: RwSignal<PaginationParams>,
    filters_rws: RwSignal<ExperimentListFilters>,
    combined_resource: CombinedResource,
) -> impl IntoView {
    let filters = filters_rws.get_untracked();
    let filters_buffer_rws = create_rw_signal(filters.clone());
    let context: Conditions = filters
        .context
        .map(|arr| {
            arr.iter()
                .filter_map(|v| {
                    v.parse()
                        .ok()
                        .and_then(|v| Condition::try_from_condition_json(&v).ok())
                })
                .collect()
        })
        .map(Conditions)
        .unwrap_or_default();

    let (context_rs, context_ws) = create_signal(context);

    let dim = combined_resource.dimensions;

    let status_filter_management =
        move |checked: bool, filter_status: ExperimentStatusType| {
            filters_buffer_rws.update(|f| {
                let status_types = f.status.clone().unwrap_or_default();
                let mut old_status_vector: HashSet<ExperimentStatusType> =
                    HashSet::from_iter(status_types.0);

                if checked {
                    old_status_vector.insert(filter_status);
                } else {
                    old_status_vector.remove(&filter_status);
                }
                let new_status_vector = old_status_vector.into_iter().collect();
                f.status = Some(CommaSeparatedQParams(new_status_vector))
            })
        };

    let fn_environment = create_memo(move |_| {
        let context = context_rs.get();
        json!({
            "context": context,
            "overrides": [],
        })
    });

    view! {
        <DrawerBtn drawer_id="experiment_filter_drawer".into() style=DrawerButtonStyle::Outline>
            Filters
            <i class="ri-filter-3-line"></i>
        </DrawerBtn>
        <Drawer
            id="experiment_filter_drawer".to_string()
            header="Experiment Filters"
            drawer_width="w-[50vw]"
            handle_close=move || close_drawer("experiment_filter_drawer")
        >
            <div class="card-body">
                <div class="my-4">
                    <ContextForm
                        dimensions=dim
                        context_rs
                        context_ws
                        dropdown_direction=DropdownDirection::Down
                        handle_change=move |context: Conditions| {
                            filters_buffer_rws
                                .update(|f| {
                                    f.context = Some(
                                        CommaSeparatedQParams(
                                            context
                                                .iter()
                                                .map(|v| v.to_condition_json().to_string())
                                                .collect::<Vec<_>>(),
                                        ),
                                    );
                                });
                        }
                        heading_sub_text=String::from("Search By Context")
                        resolve_mode=true
                        fn_environment
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
                            min=String::from("2020-01-01")
                            value=filters
                                .from_date
                                .map(|s| s.format("%Y-%m-%d").to_string())
                                .unwrap_or_default()
                            on_change=Callback::new(move |new_date: DateTime<Utc>| {
                                filters_buffer_rws.update(|f| f.from_date = Some(new_date));
                            })
                            on_clear=Callback::new(move |_| {
                                filters_buffer_rws.update(|f| f.from_date = None);
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
                            value=filters
                                .to_date
                                .unwrap_or(Utc::now())
                                .format("%Y-%m-%d")
                                .to_string()
                            on_change=Callback::new(move |new_date: DateTime<Utc>| {
                                filters_buffer_rws
                                    .update(|f| {
                                        f.to_date = Some(
                                            new_date + Days::new(1) - Duration::seconds(1),
                                        );
                                    });
                            })
                            on_clear=Callback::new(move |_| {
                                filters_buffer_rws.update(|f| f.to_date = None);
                            })
                        />
                    </div>
                </div>

                <div class="form-control w-full">
                    <label class="label">
                        <span class="label-text">Experiment Status</span>
                    </label>
                    <div class="flex flex-row justify-start gap-10">
                        {ExperimentStatusType::iter()
                            .map(|status| {
                                let label = status.to_string();
                                let input_id = format!("{label}-checkbox");

                                view! {
                                    <div>
                                        <input
                                            type="checkbox"
                                            id=&input_id
                                            class="peer hidden"
                                            checked=move || {
                                                filters_buffer_rws
                                                    .get()
                                                    .status
                                                    .is_some_and(|s| s.iter().any(|item| *item == status))
                                            }
                                            on:change=move |event| {
                                                let checked = event_target_checked(&event);
                                                status_filter_management(checked, status)
                                            }
                                        />
                                        <label
                                            for=&input_id
                                            class="badge h-[30px] px-6 py-2 peer-checked:bg-purple-500 peer-checked:text-white cursor-pointer transition duration-300 ease-in-out"
                                        >
                                            {label}
                                        </label>
                                    </div>
                                }
                            })
                            .collect_view()}
                    </div>
                </div>
                <div class="flex flex-col gap-5 justify-between">
                    <div class="form-control">
                        <label class="label">
                            <span class="label-text">Experiment Name</span>
                        </label>
                        <input
                            type="text"
                            id="experiment-name-filter"
                            placeholder="eg: city experiment"
                            class="input input-bordered rounded-md resize-y w-full max-w-md"
                            value=move || filters_buffer_rws.get().experiment_name
                            on:change=move |event| {
                                let experiment_name = event_target_value(&event);
                                let experiment_name = if experiment_name.is_empty() {
                                    None
                                } else {
                                    Some(experiment_name)
                                };
                                filters_buffer_rws.update(|f| f.experiment_name = experiment_name);
                            }
                        />
                    </div>
                    <div class="form-control">
                        <label class="label">
                            <span class="label-text">Experiment IDs (Seperate by Comma)</span>
                        </label>
                        <input
                            type="text"
                            id="experiment-id-filter"
                            class="input input-bordered rounded-md resize-y w-full max-w-md"
                            value=move || {
                                filters_buffer_rws.get().experiment_ids.map(|d| d.to_string())
                            }
                            placeholder="eg: 7259558160762015744"
                            on:change=move |event| {
                                let ids = event_target_value(&event);
                                let ids = (!ids.is_empty())
                                    .then(|| serde_json::from_value(Value::String(ids)).ok())
                                    .flatten();
                                filters_buffer_rws.update(|filter| filter.experiment_ids = ids);
                            }
                        />
                    </div>
                    <div class="form-control">
                        <label class="label">
                            <span class="label-text">Created By (Seperate by Comma)</span>
                        </label>
                        <input
                            type="text"
                            id="experiment-user-filter"
                            class="input input-bordered rounded-md resize-y w-full max-w-md"
                            placeholder="eg: user@superposition.io"
                            value=move || filters_buffer_rws.get().created_by.map(|d| d.to_string())
                            on:change=move |event| {
                                let user_names = event_target_value(&event);
                                let user_names = (!user_names.is_empty())
                                    .then(|| serde_json::from_value(Value::String(user_names)).ok())
                                    .flatten();
                                filters_buffer_rws.update(|filter| filter.created_by = user_names);
                            }
                        />

                    </div>
                </div>
                <div class="flex justify-start mt-8">
                    <Button
                        class="w-48 px-[70px] h-12".to_string()
                        text="Submit".to_string()
                        on_click=move |event| {
                            event.prevent_default();
                            let filter = filters_buffer_rws.get();
                            pagination_params_rws
                                .update(|f| {
                                    filters_rws.set_untracked(filter);
                                    f.reset_page()
                                });
                            close_drawer("experiment_filter_drawer")
                        }
                    />
                    <Button
                        class="px-[70px] h-12 w-48".to_string()
                        text="Reset".to_string()
                        icon_class="ri-restart-line".into()
                        on_click=move |event| {
                            event.prevent_default();
                            pagination_params_rws
                                .update(|f| {
                                    filters_rws.set_untracked(ExperimentListFilters::default());
                                    f.reset_page()
                                });
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
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let (reset_exp_form, set_exp_form) = create_signal(0);
    let filters_rws = use_signal_from_query(move |query_string| {
        Query::<ExperimentListFilters>::extract_non_empty(&query_string).into_inner()
    });
    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
    });

    let workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();

    use_param_updater(move || {
        box_params!(pagination_params_rws.get(), filters_rws.get())
    });

    let combined_resource = create_blocking_resource(
        move || {
            (
                workspace.get().0,
                filters_rws.get(),
                pagination_params_rws.get(),
                org.get().0,
            )
        },
        |(current_tenant, filters, pagination_params, org_id)| async move {
            // Perform all fetch operations concurrently
            let fetch_all_filters = PaginationParams::all_entries();
            let default_config_filters = DefaultConfigFilters::default();
            let experiments_future = fetch_experiments(
                &filters,
                &pagination_params,
                current_tenant.to_string(),
                org_id.clone(),
            );
            let dimensions_future = fetch_dimensions(
                &fetch_all_filters,
                current_tenant.to_string(),
                org_id.clone(),
            );
            let config_future = fetch_default_config(
                &fetch_all_filters,
                &default_config_filters,
                current_tenant.to_string(),
                org_id.clone(),
            );
            let (experiments_result, dimensions_result, config_result) =
                join!(experiments_future, dimensions_future, config_future,);
            // Construct the combined result, handling errors as needed
            CombinedResource {
                experiments: experiments_result.unwrap_or_default(),
                dimensions: dimensions_result
                    .unwrap_or_default()
                    .data
                    .into_iter()
                    .filter(|d| d.dimension != "variantIds")
                    .collect(),
                default_config: config_result.unwrap_or_default().data,
            }
        },
    );

    let handle_submit_experiment_form = move |_| {
        filters_rws.set(ExperimentListFilters::default());
        pagination_params_rws.update(|f| f.reset_page());
        combined_resource.refetch();
        set_exp_form.update(|val| {
            *val += 1;
        });
        close_drawer("create_exp_drawer");
    };

    let handle_next_click = Callback::new(move |next_page: i64| {
        pagination_params_rws.update(|f| f.page = Some(next_page));
    });

    let handle_prev_click = Callback::new(move |prev_page: i64| {
        pagination_params_rws.update(|f| f.page = Some(prev_page));
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
                            let pagination_params = pagination_params_rws.get();
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
                                                    json!(ele.created_at.format("%v %T").to_string()),
                                                );
                                            ele_map
                                                .insert(
                                                    "last_modified".to_string(),
                                                    json!(ele.last_modified.format("%v %T").to_string()),
                                                );
                                            ele_map
                                        })
                                        .collect::<Vec<Map<String, Value>>>()
                                        .to_owned();
                                    let pagination_props = TablePaginationProps {
                                        enabled: true,
                                        count: pagination_params.count.unwrap_or_default(),
                                        current_page: pagination_params.page.unwrap_or_default(),
                                        total_pages: v.experiments.total_pages,
                                        on_next: handle_next_click,
                                        on_prev: handle_prev_click,
                                    };
                                    view! {
                                        <ConditionCollapseProvider>
                                            <ExperimentTableFilterWidget
                                                pagination_params_rws
                                                filters_rws
                                                combined_resource=v
                                            />
                                            <Table
                                                rows=data
                                                key_column="id".to_string()
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
                    let CombinedResource {
                        dimensions: dim,
                        default_config: def_conf,
                        experiments: _,
                    } = combined_resource.get().unwrap_or_default();
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
                                    context=Conditions::default()
                                    variants=VariantFormTs::default()
                                    dimensions=dim.clone()
                                    default_config=def_conf.clone()
                                    handle_submit=handle_submit_experiment_form
                                    metrics=workspace_settings.get_value().metrics
                                />
                            </EditorProvider>
                        </Drawer>
                    }
                }}

            </Suspense>
        </div>
    }
}
