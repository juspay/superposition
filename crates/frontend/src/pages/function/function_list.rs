use leptos::*;

use serde::{Deserialize, Serialize};
use serde_json::{Map, Value, json};
use superposition_types::{
    PaginatedResponse,
    api::functions::ListFunctionFilters,
    custom_query::{CustomQuery, PaginationParams, Query},
    database::models::cac::Function,
};

use crate::components::skeleton::Skeleton;
use crate::components::table::types::TablePaginationProps;
use crate::components::{stat::Stat, table::Table};
use crate::query_updater::use_signal_from_query;
use crate::types::{OrganisationId, Workspace};
use crate::{api::fetch_functions, components::button::ButtonAnchor};

use super::utils::function_table_columns;

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CombinedResource {
    functions: PaginatedResponse<Function>,
}

#[component]
pub fn FunctionList() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let (pagination_params_rws, filters_rws) =
        use_signal_from_query(move |query_string| {
            (
                Query::<PaginationParams>::extract_non_empty(query_string).into_inner(),
                Query::<ListFunctionFilters>::extract_non_empty(query_string)
                    .into_inner(),
            )
        });

    let table_columns = StoredValue::new(function_table_columns());

    let combined_resource = create_blocking_resource(
        move || {
            (
                workspace.get().0,
                pagination_params_rws.get(),
                filters_rws.get(),
                org.get().0,
            )
        },
        |(workspace, pagination, filters, org)| async move {
            let functions_future =
                fetch_functions(&pagination, &filters, &workspace, &org);

            let functions_result = functions_future.await;
            CombinedResource {
                functions: functions_result.unwrap_or(PaginatedResponse::default()),
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
                    let value = combined_resource
                        .get()
                        .unwrap_or(CombinedResource {
                            functions: PaginatedResponse::default(),
                        });
                    let total_items = value.functions.total_items.to_string();
                    view! {
                        <div class="flex justify-between">
                            <Stat heading="Functions" icon="ri-code-box-fill" number=total_items />
                            <ButtonAnchor
                                href="action/create"
                                text="Create Function"
                                class="self-end h-10"
                                icon_class="ri-add-line"
                            />
                        </div>
                    }
                }} <div class="card w-full bg-base-100 rounded-xl overflow-hidden shadow">
                    <div class="card-body overflow-y-auto overflow-x-visible">
                        {move || {
                            let value = combined_resource.get();
                            let pagination_params = pagination_params_rws.get();
                            match value {
                                Some(v) => {
                                    let data = v
                                        .functions
                                        .data
                                        .iter()
                                        .map(|ele| json!(ele).as_object().unwrap().to_owned())
                                        .collect::<Vec<Map<String, Value>>>()
                                        .to_owned();
                                    let total_pages = v.functions.total_pages;
                                    let pagination_props = TablePaginationProps {
                                        enabled: true,
                                        count: pagination_params.count.unwrap_or_default(),
                                        current_page: pagination_params.page.unwrap_or_default(),
                                        total_pages,
                                        on_page_change: handle_page_change,
                                    };
                                    view! {
                                        <Table
                                            class="!overflow-y-auto"
                                            rows=data
                                            key_column="function_name"
                                            columns=table_columns.get_value()
                                            pagination=pagination_props
                                        />
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
