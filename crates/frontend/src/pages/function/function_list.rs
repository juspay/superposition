use leptos::logging::*;
use leptos::*;

use chrono::{prelude::Utc, TimeZone};
use leptos_router::A;
use serde::{Deserialize, Serialize};

use crate::components::{
    pagination::pagination::Pagination, stat::stat::Stat, table::table::Table,
};

use crate::types::{FunctionResponse, ListFilters};

use super::utils::function_table_columns;
use crate::api::fetch_functions;
use serde_json::{json, Map, Value};

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CombinedResource {
    functions: Vec<FunctionResponse>,
}

#[component]
pub fn function_list() -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let (filters, set_filters) = create_signal(ListFilters {
        status: None,
        from_date: Utc.timestamp_opt(0, 0).single(),
        to_date: Utc.timestamp_opt(4130561031, 0).single(),
        page: Some(1),
        count: Some(10),
    });
    let table_columns = create_memo(move |_| function_table_columns());

    let combined_resource: Resource<String, CombinedResource> = create_blocking_resource(
        move || (tenant_rs.get()),
        |current_tenant| async move {
            let functions_future = fetch_functions(current_tenant.to_string());

            let functions_result = functions_future.await;
            CombinedResource {
                functions: functions_result.unwrap_or_else(|_| vec![]),
            }
        },
    );

    view! {
        <div class="p-8">
            <Suspense fallback=move || view! { <p>"Loading (Suspense Fallback)..."</p> }>
                <div class="pb-4">

                    {move || {
                        let value = combined_resource.get();
                        let total_items = match value {
                            Some(v) => std::vec::Vec::len(&v.functions).to_string(),
                            _ => "0".to_string(),
                        };
                        view! {
                            <Stat heading="Functions" icon="ri-code-box-fill" number=total_items/>
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
                                match value {
                                    Some(v) => {
                                        let data = v
                                            .functions
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
                                                            Some(val) => json!(val.format("%v").to_string()),
                                                            None => json!("null".to_string()),
                                                        },
                                                    );
                                                ele_map
                                            })
                                            .collect::<Vec<Map<String, Value>>>()
                                            .to_owned();
                                        view! {
                                            <Table
                                                cell_style="".to_string()
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
                                    Some(val) => {
                                        (val.functions.len() as f64 / 10 as f64).ceil() as i64
                                    }
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
            </Suspense>
        </div>
    }
}
