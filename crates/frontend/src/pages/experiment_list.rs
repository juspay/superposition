pub mod filter;
pub mod utils;

use filter::{ExperimentTableFilterWidget, FilterSummary};
use futures::join;
use leptos::*;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use superposition_macros::box_params;
use superposition_types::{
    api::{
        default_config::DefaultConfigFilters,
        experiments::{ExperimentListFilters, ExperimentResponse},
        workspace::WorkspaceResponse,
    },
    custom_query::{CustomQuery, DimensionQuery, PaginationParams, Query, QueryMap},
    database::{models::cac::DefaultConfig, types::DimensionWithMandatory},
    PaginatedResponse,
};
use utils::experiment_table_columns;

use crate::{
    api::{fetch_default_config, fetch_dimensions, fetch_experiments},
    components::{
        drawer::{close_drawer, Drawer, DrawerBtn},
        experiment_form::ExperimentForm,
        skeleton::Skeleton,
        stat::Stat,
        table::{types::TablePaginationProps, Table},
    },
    logic::Conditions,
    providers::{
        condition_collapse_provider::ConditionCollapseProvider,
        editor_provider::EditorProvider,
    },
    query_updater::{use_param_updater, use_signal_from_query},
    types::{OrganisationId, Tenant, VariantFormTs},
};

#[derive(Serialize, Deserialize, Clone, Default)]
struct CombinedResource {
    pub(self) experiments: PaginatedResponse<ExperimentResponse>,
    pub(self) dimensions: Vec<DimensionWithMandatory>,
    pub(self) default_config: Vec<DefaultConfig>,
}

#[component]
pub fn experiment_list() -> impl IntoView {
    let workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let (reset_exp_form, set_exp_form) = create_signal(0);
    let filters_rws = use_signal_from_query(move |query_string| {
        Query::<ExperimentListFilters>::extract_non_empty(&query_string).into_inner()
    });
    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
    });
    let dimension_params_rws = use_signal_from_query(move |query_string| {
        DimensionQuery::<QueryMap>::extract_non_empty(&query_string)
    });

    use_param_updater(move || {
        box_params![
            filters_rws.get(),
            pagination_params_rws.get(),
            dimension_params_rws.get(),
        ]
    });

    let combined_resource = create_blocking_resource(
        move || {
            (
                workspace.get().0,
                filters_rws.get(),
                dimension_params_rws.get(),
                pagination_params_rws.get(),
                org.get().0,
            )
        },
        |(current_tenant, filters, dimension_params, pagination_params, org_id)| async move {
            // Perform all fetch operations concurrently
            let fetch_all_filters = PaginationParams::all_entries();
            let default_config_filters = DefaultConfigFilters::default();
            let experiments_future = fetch_experiments(
                &filters,
                &pagination_params,
                &dimension_params,
                &current_tenant,
                &org_id,
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

    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    });

    view! {
        <Suspense fallback=move || view! { <Skeleton /> }>
            <div class="h-full flex flex-col gap-4">
                {move || {
                    let value = combined_resource.get();
                    let total_items = match value {
                        Some(v) => v.experiments.total_items.to_string(),
                        _ => "0".to_string(),
                    };
                    view! {
                        <div class="flex justify-between">
                            <Stat
                                heading="Experiments"
                                icon="ri-test-tube-fill"
                                number=total_items
                            />
                            <div class="flex items-end gap-4">
                                <ExperimentTableFilterWidget
                                    dimension_params_rws
                                    pagination_params_rws
                                    filters_rws
                                    combined_resource=combined_resource.get().unwrap_or_default()
                                />
                                <DrawerBtn
                                    drawer_id="create_exp_drawer"
                                    text="Create Experiment"
                                    icon_class="ri-add-line"
                                />
                            </div>
                        </div>
                    }
                }} <FilterSummary filters_rws dimension_params_rws />
                <div class="card w-full bg-base-100 rounded-xl overflow-hidden shadow">
                    <div class="card-body overflow-y-auto overflow-x-visible">
                        {move || {
                            let value = combined_resource.get();
                            let pagination_params = pagination_params_rws.get();
                            let table_columns = experiment_table_columns(
                                filters_rws,
                                workspace_settings.with_value(|ws| ws.strict_mode),
                            );
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
                                        on_page_change: handle_page_change,
                                    };
                                    view! {
                                        <ConditionCollapseProvider>
                                            <Table
                                                class="!overflow-y-auto"
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
                        id="create_exp_drawer"
                        header="Create New Experiment"
                        width_class="max-w-[780px] min-w-[680px] w-[45vw]"
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
                                metrics=workspace_settings.with_value(|w| w.metrics.clone())
                            />
                        </EditorProvider>
                    </Drawer>
                }
            }}

        </Suspense>
    }
}
