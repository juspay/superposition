pub mod utils;

use std::collections::HashSet;

use chrono::{prelude::Utc, DateTime};
use futures::join;
use leptos::*;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use strum::IntoEnumIterator;
use superposition_types::{
    api::{
        default_config::DefaultConfigFilters,
        experiments::{ExperimentListFilters, ExperimentResponse},
    },
    custom_query::{CommaSeparatedQParams, PaginationParams},
    database::{
        models::{cac::DefaultConfig, experimentation::ExperimentStatusType},
        types::DimensionWithMandatory,
    },
    PaginatedResponse,
};
use utils::experiment_table_columns;

use crate::{
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
    logic::Condition,
};
use crate::{logic::Conditions, utils::PageDirection};

use crate::providers::condition_collapse_provider::ConditionCollapseProvider;
use crate::providers::editor_provider::EditorProvider;
use crate::types::VariantFormTs;
use crate::utils::update_page_direction;
use crate::{
    api::{fetch_default_config, fetch_dimensions, fetch_experiments},
    types::{OrganisationId, Tenant},
};

#[derive(Serialize, Deserialize, Clone, Default)]
struct CombinedResource {
    experiments: PaginatedResponse<ExperimentResponse>,
    dimensions: Vec<DimensionWithMandatory>,
    default_config: Vec<DefaultConfig>,
}

#[component]
fn experiment_table_filter_widget(
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

    let dim = combined_resource.dimensions;

    let status_filter_management =
        move |checked: bool, filter_status: ExperimentStatusType| {
            let status_types = filters_buffer_rws.get().status.unwrap_or_default();
            let mut old_status_vector: HashSet<ExperimentStatusType> =
                HashSet::from_iter(status_types.0);

            if checked {
                old_status_vector.insert(filter_status);
            } else {
                old_status_vector.remove(&filter_status);
            }
            let new_status_vector = old_status_vector.into_iter().collect();
            let filters = ExperimentListFilters {
                status: Some(CommaSeparatedQParams(new_status_vector)),
                ..filters_buffer_rws.get()
            };
            filters_buffer_rws.set(filters)
        };
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
                        context
                        dropdown_direction=DropdownDirection::Down
                        handle_change=move |context: Conditions| {
                            let current_filters = filters_buffer_rws.get();
                            filters_buffer_rws
                                .set(ExperimentListFilters {
                                    context: Some(
                                        CommaSeparatedQParams(
                                            context
                                                .iter()
                                                .map(|v| v.to_condition_json().to_string())
                                                .collect::<Vec<_>>(),
                                        ),
                                    ),
                                    ..current_filters
                                });
                        }
                        heading_sub_text=String::from("Search By Context")
                        resolve_mode=true
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
                        {ExperimentStatusType::iter()
                            .map(|status| {
                                let current_status = status.to_string().to_lowercase();
                                let label = status.to_string();
                                let input_id = format!("{current_status}-checkbox");
                                let input_peer_class = format!(
                                    "hidden peer/{current_status}-checkbox",
                                );
                                let label_class = format!(
                                    "px-6 h-[30px] py-2 badge peer-checked/{current_status}-checkbox:{} cursor-pointer transition duration-300 ease-in-out",
                                    status.badge_color(),
                                );
                                view! {
                                    <input
                                        type="checkbox"
                                        id=&input_id
                                        class=&input_peer_class
                                        checked=move || {
                                            filters_buffer_rws
                                                .get()
                                                .status
                                                .unwrap_or_default()
                                                .iter()
                                                .any(|item| *item == status)
                                        }
                                        on:change=move |event| {
                                            let checked = event_target_checked(&event);
                                            status_filter_management(checked, status)
                                        }
                                    />
                                    <label for=&input_id class=&label_class>
                                        {label}
                                    </label>
                                }
                            })
                            .collect::<Vec<_>>()}
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
                                    experiment_name,
                                    ..filters
                                };
                                filters_buffer_rws.set(filters);
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
                            value=move || filters_rws.get().experiment_ids.map(|d| d.to_string())
                            placeholder="eg: 7259558160762015744"
                            on:change=move |event| {
                                let ids = event_target_value(&event);
                                let ids = (!ids.is_empty())
                                    .then(|| serde_json::from_value(Value::String(ids)).ok())
                                    .flatten();
                                filters_buffer_rws
                                    .update(|filter| {
                                        filter.experiment_ids = ids;
                                    });
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
                            value=move || filters_rws.get().created_by.map(|d| d.to_string())
                            on:change=move |event| {
                                let user_names = event_target_value(&event);
                                let user_names = (!user_names.is_empty())
                                    .then(|| serde_json::from_value(Value::String(user_names)).ok())
                                    .flatten();
                                filters_buffer_rws
                                    .update(|filter| {
                                        filter.created_by = user_names;
                                    });
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
                            filters_rws.set(filter);
                            close_drawer("experiment_filter_drawer")
                        }
                    />
                    <Button
                        class="px-[70px] h-12 w-48".to_string()
                        text="Reset".to_string()
                        icon_class="ri-restart-line".into()
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
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let filters_rws = create_rw_signal(ExperimentListFilters::default());

    let (pagination_filters_rs, pagination_filters_ws) =
        create_signal(PaginationParams::default());

    let (reset_exp_form, set_exp_form) = create_signal(0);

    let combined_resource = create_blocking_resource(
        move || {
            (
                tenant_rws.get().0,
                filters_rws.get(),
                pagination_filters_rs.get(),
                org_rws.get().0,
            )
        },
        |(current_tenant, filters, pagination_filters, org_id)| async move {
            // Perform all fetch operations concurrently
            let fetch_all_filters = PaginationParams::all_entries();
            let default_config_filters = DefaultConfigFilters::default();
            let experiments_future = fetch_experiments(
                &filters,
                &pagination_filters,
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
                join!(experiments_future, dimensions_future, config_future);
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
        combined_resource.refetch();
        set_exp_form.update(|val| {
            *val += 1;
        });
        close_drawer("create_exp_drawer");
    };

    let handle_next_click = Callback::new(move |total_pages: i64| {
        pagination_filters_ws.update(|f| {
            f.page = update_page_direction(f.page, PageDirection::Next(total_pages));
        });
    });

    let handle_prev_click = Callback::new(move |_| {
        pagination_filters_ws.update(|f| {
            f.page = update_page_direction(f.page, PageDirection::Prev);
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
                                        count: pagination.count.unwrap_or_default(),
                                        current_page: pagination.page.unwrap_or_default(),
                                        total_pages: v.experiments.total_pages,
                                        on_next: handle_next_click,
                                        on_prev: handle_prev_click,
                                    };
                                    view! {
                                        <ConditionCollapseProvider>
                                            <ExperimentTableFilterWidget
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
                                }
                                None => view! { <div>Loading....</div> }.into_view(),
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
                                    name="".to_string()
                                    context=Conditions::default()
                                    variants=VariantFormTs::default()
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
