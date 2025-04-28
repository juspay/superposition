use leptos::*;

use leptos_router::A;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use superposition_macros::box_params;
use superposition_types::{
    custom_query::{CustomQuery, PaginationParams, Query},
    database::models::cac::Function,
    PaginatedResponse,
};

use crate::api::fetch_functions;
use crate::components::skeleton::Skeleton;
use crate::components::table::types::TablePaginationProps;
use crate::components::{stat::Stat, table::Table};
use crate::query_updater::{use_param_updater, use_signal_from_query};
use crate::types::{OrganisationId, Tenant};

use super::utils::function_table_columns;

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CombinedResource {
    functions: PaginatedResponse<Function>,
}

#[component]
pub fn function_list() -> impl IntoView {
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
    });

    use_param_updater(move || box_params!(pagination_params_rws.get()));

    let table_columns = create_memo(move |_| function_table_columns());

    let combined_resource = create_blocking_resource(
        move || {
            (
                tenant_rws.get().0,
                pagination_params_rws.get(),
                org_rws.get().0,
            )
        },
        |(current_tenant, pagination_params, org)| async move {
            let functions_future =
                fetch_functions(&pagination_params, current_tenant.to_string(), org);

            let functions_result = functions_future.await;
            CombinedResource {
                functions: functions_result.unwrap_or(PaginatedResponse::default()),
            }
        },
    );

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
                        let value = combined_resource
                            .get()
                            .unwrap_or(CombinedResource {
                                functions: PaginatedResponse::default(),
                            });
                        let total_items = value.functions.total_items.to_string();
                        view! {
                            <Stat heading="Functions" icon="ri-code-box-fill" number=total_items />
                        }
                    }}

                </div>
                <div class="card rounded-xl w-full bg-base-100 shadow">
                    <div class="card-body">
                        <div class="flex justify-between">
                            <h2 class="card-title">Functions</h2>
                            <div>

                                <A
                                    href="create".to_string()
                                    class="btn-purple font-medium rounded-lg text-sm px-5 py-2.5 text-center me-2 mb-2 btn-link"
                                >
                                    <button>
                                        Create Function <i class="ri-edit-2-line ml-2"></i>
                                    </button>
                                </A>
                            </div>
                        </div>
                        <div>

                            {move || {
                                let value = combined_resource.get();
                                let pagination_params = pagination_params_rws.get();
                                match value {
                                    Some(v) => {
                                        let data = v
                                            .functions
                                            .data
                                            .iter()
                                            .map(|ele| {
                                                let mut ele_map = json!(ele)
                                                    .as_object()
                                                    .unwrap()
                                                    .to_owned();
                                                ele_map
                                                    .insert(
                                                        "published_at".to_string(),
                                                        match ele.published_at {
                                                            Some(val) => json!(val.format("%v %T").to_string()),
                                                            None => json!("null".to_string()),
                                                        },
                                                    );
                                                ele_map
                                            })
                                            .collect::<Vec<Map<String, Value>>>()
                                            .to_owned();
                                        let total_pages = v.functions.total_pages;
                                        let pagination_props = TablePaginationProps {
                                            enabled: true,
                                            count: pagination_params.count.unwrap_or_default(),
                                            current_page: pagination_params.page.unwrap_or_default(),
                                            total_pages,
                                            on_next: handle_next_click,
                                            on_prev: handle_prev_click,
                                        };
                                        view! {
                                            <Table
                                                rows=data
                                                key_column="id".to_string()
                                                columns=table_columns.get()
                                                pagination=pagination_props
                                            />
                                        }
                                    }
                                    None => view! { <div>Loading....</div> }.into_view(),
                                }
                            }}

                        </div>
                    </div>
                </div>
            </Suspense>
        </div>
    }
}
