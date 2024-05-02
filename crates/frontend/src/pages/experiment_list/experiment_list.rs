use futures::join;
use leptos::logging::*;
use leptos::*;

use chrono::{prelude::Utc, TimeZone};
use serde::{Deserialize, Serialize};

use crate::components::drawer::drawer::{close_drawer, Drawer, DrawerBtn};
use crate::components::{
    experiment_form::experiment_form::ExperimentForm, pagination::pagination::Pagination,
    stat::stat::Stat, table::table::Table,
};

use crate::types::{ExperimentsResponse, ListFilters};

use super::utils::experiment_table_columns;
use crate::{
    api::{fetch_default_config, fetch_dimensions, fetch_experiments},
    types::{DefaultConfig, Dimension},
};
use serde_json::{json, Map, Value};

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CombinedResource {
    experiments: ExperimentsResponse,
    dimensions: Vec<Dimension>,
    default_config: Vec<DefaultConfig>,
}

#[component]
pub fn experiment_list() -> impl IntoView {
    // acquire tenant
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let (filters, set_filters) = create_signal(ListFilters {
        status: None,
        from_date: Utc.timestamp_opt(0, 0).single(),
        to_date: Utc.timestamp_opt(4130561031, 0).single(),
        page: Some(1),
        count: Some(10),
    });

    let (reset_exp_form, set_exp_form) = create_signal(0);
    let table_columns = create_memo(move |_| experiment_table_columns());

    let combined_resource: Resource<(String, ListFilters), CombinedResource> =
        create_blocking_resource(
            move || (tenant_rs.get(), filters.get()),
            |(current_tenant, filters)| async move {
                // Perform all fetch operations concurrently
                let experiments_future =
                    fetch_experiments(filters, current_tenant.to_string());
                let dimensions_future = fetch_dimensions(current_tenant.to_string());
                let config_future = fetch_default_config(current_tenant.to_string());

                let (experiments_result, dimensions_result, config_result) =
                    join!(experiments_future, dimensions_future, config_future);

                // Construct the combined result, handling errors as needed
                CombinedResource {
                    experiments: experiments_result.unwrap_or_else(|_| {
                        ExperimentsResponse {
                            total_items: 0,
                            total_pages: 0,
                            data: vec![],
                        }
                    }),
                    dimensions: dimensions_result.unwrap_or_else(|_| vec![]),
                    default_config: config_result.unwrap_or_else(|_| vec![]),
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

    // TODO: Add filters
    view! {
        <div class="p-8">
            <Suspense fallback=move || view! { <p>"Loading (Suspense Fallback)..."</p> }>
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
                        <div>

                            {move || {
                                let value = combined_resource.get();
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
                                        view! {
                                            <Table
                                                cell_style="min-w-48 font-mono".to_string()
                                                rows=data
                                                key_column="id".to_string()
                                                columns=table_columns.get()
                                            />
                                        }
                                    }
                                    None => view! { <div>Loading....</div> }.into_view(),
                                }
                            }}

                        </div>
                        <div class="mt-2 flex justify-end">

                            {move || {
                                let current_page = filters.get().page.unwrap_or(0);
                                let total_pages = match combined_resource.get() {
                                    Some(val) => val.experiments.total_pages,
                                    None => 0,
                                };
                                view! {
                                    <Pagination
                                        current_page=current_page
                                        total_pages=total_pages
                                        next=move || {
                                            set_filters
                                                .update(|f| {
                                                    f
                                                        .page = match f.page {
                                                        Some(p) if p < total_pages => Some(p + 1),
                                                        Some(p) => Some(p),
                                                        None => None,
                                                    }
                                                });
                                        }

                                        previous=move || {
                                            set_filters
                                                .update(|f| {
                                                    f
                                                        .page = match f.page {
                                                        Some(p) if p > 1 => Some(p - 1),
                                                        Some(p) => Some(p),
                                                        None => None,
                                                    }
                                                });
                                        }
                                    />
                                }
                            }}

                        </div>
                    </div>
                </div>

                {move || {
                    let dim = combined_resource
                        .get()
                        .unwrap_or(CombinedResource {
                            experiments: ExperimentsResponse {
                                total_items: 0,
                                total_pages: 0,
                                data: vec![],
                            },
                            dimensions: vec![],
                            default_config: vec![],
                        })
                        .dimensions;
                    let def_conf = combined_resource
                        .get()
                        .unwrap_or(CombinedResource {
                            experiments: ExperimentsResponse {
                                total_items: 0,
                                total_pages: 0,
                                data: vec![],
                            },
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
                            }
                        >

                            <ExperimentForm
                                name="".to_string()
                                context=vec![]
                                variants=vec![]
                                dimensions=dim.clone()
                                default_config=def_conf.clone()
                                handle_submit=handle_submit_experiment_form
                            />
                        </Drawer>
                    }
                }}

            </Suspense>
        </div>
    }
}
