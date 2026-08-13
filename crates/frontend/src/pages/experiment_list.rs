pub mod filter;
pub mod utils;

use filter::{ExperimentTableFilterWidget, FilterSummary};
use futures::join;
use leptos::*;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value, json};
use superposition_types::{
    PaginatedResponse,
    api::{
        default_config::DefaultConfigFilters,
        dimension::DimensionResponse,
        experiments::{ExperimentListFilters, ExperimentResponse},
    },
    custom_query::{CustomQuery, DimensionQuery, PaginationParams, Query, QueryMap},
    database::models::cac::DefaultConfig,
};
use utils::experiment_table_columns;

use crate::{
    api::{default_configs, dimensions, fetch_experiments},
    components::{
        button::ButtonAnchor,
        skeleton::Skeleton,
        stat::Stat,
        table::{Table, types::TablePaginationProps},
    },
    providers::condition_collapse_provider::ConditionCollapseProvider,
    query_updater::use_signal_from_query,
    types::{OrganisationId, Workspace},
};

#[derive(Serialize, Deserialize, Clone, Default)]
struct CombinedResource {
    pub(self) experiments: PaginatedResponse<ExperimentResponse>,
    pub(self) dimensions: Vec<DimensionResponse>,
    pub(self) default_config: Vec<DefaultConfig>,
}

#[component]
pub fn ExperimentList() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let (filters_rws, pagination_params_rws, dimension_params_rws) =
        use_signal_from_query(move |query_string| {
            (
                Query::<ExperimentListFilters>::extract_non_empty(query_string)
                    .into_inner(),
                Query::<PaginationParams>::extract_non_empty(query_string).into_inner(),
                DimensionQuery::<QueryMap>::extract_non_empty(query_string),
            )
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
        |(workspace, filters, dimension_params, pagination_params, org_id)| async move {
            let fetch_all_filters = PaginationParams::all_entries();
            let default_config_filters = DefaultConfigFilters::default();
            let experiments_future = fetch_experiments(
                &filters,
                &pagination_params,
                &dimension_params,
                &workspace,
                &org_id,
            );
            let dimensions_future =
                dimensions::list(&fetch_all_filters, &workspace, &org_id);
            let config_future = default_configs::list(
                &fetch_all_filters,
                &default_config_filters,
                &workspace,
                &org_id,
            );
            let (experiments_result, dimensions_result, config_result) =
                join!(experiments_future, dimensions_future, config_future,);
            CombinedResource {
                experiments: experiments_result.unwrap_or_default(),
                dimensions: dimensions_result.unwrap_or_default().data,
                default_config: config_result.unwrap_or_default().data,
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
                    let total_items = combined_resource
                        .with(|c| c.as_ref().map(|r| r.experiments.total_items))
                        .unwrap_or_default()
                        .to_string();
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
                                <ButtonAnchor
                                    class="h-fit"
                                    href="action/create"
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
                                pagination_params_rws,
                            );
                            match value {
                                Some(v) => {
                                    let data = v
                                        .experiments
                                        .data
                                        .iter()
                                        .map(|ele| json!(ele).as_object().unwrap().to_owned())
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

        </Suspense>
    }
}
