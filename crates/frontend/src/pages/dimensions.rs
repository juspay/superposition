use leptos::*;
use serde_json::{json, Map, Value};
use superposition_macros::box_params;
use superposition_types::custom_query::{CustomQuery, PaginationParams, Query};

use crate::api::{delete_dimension, fetch_dimensions};
use crate::components::dimension_form::DimensionForm;
use crate::components::drawer::{close_drawer, open_drawer, Drawer, DrawerBtn};
use crate::components::skeleton::Skeleton;
use crate::components::table::types::{ColumnSortable, Expandable};
use crate::components::{
    delete_modal::DeleteModal,
    stat::Stat,
    table::{
        types::{Column, TablePaginationProps},
        Table,
    },
};
use crate::query_updater::{use_param_updater, use_signal_from_query};
use crate::types::{OrganisationId, Tenant};

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub dimension: String,
    pub position: u32,
    pub schema: Value,
    pub function_name: Option<Value>,
    pub mandatory: bool,
    pub dependencies: Vec<String>,
}

#[component]
pub fn dimensions() -> impl IntoView {
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let (delete_modal_visible_rs, delete_modal_visible_ws) = create_signal(false);
    let (delete_id_rs, delete_id_ws) = create_signal::<Option<String>>(None);
    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_query(&query_string)
            .map(|q| q.into_inner())
            .unwrap_or_default()
    });

    use_param_updater(move || box_params!(pagination_params_rws.get()));

    let dimensions_resource = create_blocking_resource(
        move || {
            (
                tenant_rws.get().0,
                pagination_params_rws.get(),
                org_rws.get().0,
            )
        },
        |(current_tenant, pagination_params, org_id)| async move {
            fetch_dimensions(&pagination_params, current_tenant, org_id)
                .await
                .unwrap_or_default()
        },
    );

    let confirm_delete = Callback::new(move |_| {
        if let Some(id) = delete_id_rs.get().clone() {
            spawn_local(async move {
                let result =
                    delete_dimension(id, tenant_rws.get().0, org_rws.get().0).await;

                match result {
                    Ok(_) => {
                        logging::log!("Dimension deleted successfully");
                        dimensions_resource.refetch();
                    }
                    Err(e) => {
                        logging::log!("Error deleting Dimension: {:?}", e);
                    }
                }
            });
        }
        delete_id_ws.set(None);
        delete_modal_visible_ws.set(false);
    });
    let handle_next_click = Callback::new(move |next_page: i64| {
        pagination_params_rws.update(|f| f.page = Some(next_page));
    });

    let handle_prev_click = Callback::new(move |prev_page: i64| {
        pagination_params_rws.update(|f| f.page = Some(prev_page));
    });

    let selected_dimension = create_rw_signal::<Option<RowData>>(None);

    let table_columns = create_memo(move |_| {
        let action_col_formatter = move |_: &str, row: &Map<String, Value>| {
            logging::log!("Dimension row: {:?}", row);
            let row_dimension = row["dimension"].to_string().replace('"', "");
            let row_position_str = row["position"].to_string().replace('"', "");
            let row_position = row_position_str.parse::<u32>().unwrap_or(0_u32);

            let schema = row["schema"].clone().to_string();
            let schema = serde_json::from_str::<Value>(&schema).unwrap_or(Value::Null);

            let function_name = row["function_name"].to_string();
            let fun_name = match function_name.as_str() {
                "null" => None,
                _ => Some(json!(function_name.replace('"', ""))),
            };

            let mandatory = row["mandatory"].as_bool().unwrap_or(false);

            let dependencies = row["dependencies"]
                .as_array()
                .unwrap_or(&vec![])
                .iter()
                .filter_map(|dep| dep.as_str().map(String::from))
                .collect::<Vec<String>>();

            let dimension_name = row_dimension.clone();

            let edit_click_handler = move |_| {
                let row_data = RowData {
                    dimension: row_dimension.clone(),
                    position: row_position,
                    schema: schema.clone(),
                    function_name: fun_name.clone(),
                    mandatory,
                    dependencies: dependencies.clone(),
                };
                logging::log!("{:?}", row_data);
                selected_dimension.set(Some(row_data));
                open_drawer("dimension_drawer");
            };

            if dimension_name.clone() == *"variantIds" {
                view! {
                    <div class="join">
                        <span class="cursor-pointer" on:click=edit_click_handler>
                            <i class="ri-pencil-line ri-xl text-blue-500"></i>
                        </span>
                    </div>
                }
                .into_view()
            } else {
                let handle_dimension_delete = move |_| {
                    delete_id_ws.set(Some(dimension_name.clone()));
                    delete_modal_visible_ws.set(true);
                };
                view! {
                    <div class="join">
                        <span class="cursor-pointer" on:click=edit_click_handler>
                            <i class="ri-pencil-line ri-xl text-blue-500"></i>
                        </span>
                        <span class="cursor-pointer text-red-500" on:click=handle_dimension_delete>
                            <i class="ri-delete-bin-5-line ri-xl text-red-500"></i>
                        </span>
                    </div>
                }
                .into_view()
            }
        };
        vec![
            Column::default_no_collapse("dimension".to_string()),
            Column::default("position".to_string()),
            Column::default("schema".to_string()),
            Column::default("mandatory".to_string()),
            Column::default("function_name".to_string()),
            Column::default("dependencies".to_string()),
            Column::default("created_by".to_string()),
            Column::default("created_at".to_string()),
            Column::new(
                "actions".to_string(),
                false,
                action_col_formatter,
                ColumnSortable::No,
                Expandable::Enabled(100),
            ),
        ]
    });

    view! {
        <div class="p-8">
            <Suspense fallback=move || {
                view! { <Skeleton /> }
            }>
                {move || {
                    let handle_close = move || {
                        close_drawer("dimension_drawer");
                        selected_dimension.set(None);
                    };
                    let dimensions = dimensions_resource.get().unwrap_or_default().data;
                    if let Some(selected_dimension_data) = selected_dimension.get() {
                        view! {
                            <Drawer
                                id="dimension_drawer".to_string()
                                header="Edit Dimension"
                                handle_close=handle_close
                            >
                                <DimensionForm
                                    edit=true
                                    position=selected_dimension_data.position
                                    dimension_name=selected_dimension_data.dimension
                                    dimension_schema=selected_dimension_data.schema
                                    dependencies=selected_dimension_data.dependencies
                                    function_name=selected_dimension_data.function_name
                                    dimensions
                                    handle_submit=move || {
                                        dimensions_resource.refetch();
                                        selected_dimension.set(None);
                                        close_drawer("dimension_drawer");
                                    }
                                />

                            </Drawer>
                        }
                    } else {
                        view! {
                            <Drawer
                                id="dimension_drawer".to_string()
                                header="Create New Dimension"
                                handle_close=handle_close
                            >
                                <DimensionForm
                                    dimensions
                                    handle_submit=move || {
                                        dimensions_resource.refetch();
                                        close_drawer("dimension_drawer");
                                    }
                                />
                            </Drawer>
                        }
                    }
                }}
                {move || {
                    let value = dimensions_resource.get().unwrap_or_default();
                    let total_items = value.data.len().to_string();
                    let table_rows = value
                        .data
                        .iter()
                        .map(|ele| {
                            let mut ele_map = json!(ele).as_object().unwrap().clone();
                            ele_map
                                .insert(
                                    "created_at".to_string(),
                                    json!(ele.created_at.format("%v %T").to_string()),
                                );
                            ele_map
                        })
                        .collect::<Vec<Map<String, Value>>>();
                    let pagination_params = pagination_params_rws.get();
                    let pagination_props = TablePaginationProps {
                        enabled: true,
                        count: pagination_params.count.unwrap_or_default(),
                        current_page: pagination_params.page.unwrap_or_default(),
                        total_pages: value.total_pages,
                        on_next: handle_next_click,
                        on_prev: handle_prev_click,
                    };
                    view! {
                        <div class="pb-4">
                            <Stat heading="Dimensions" icon="ri-ruler-2-fill" number=total_items />
                        </div>
                        <div class="card rounded-xl w-full bg-base-100 shadow">
                            <div class="card-body">
                                <div class="flex justify-between">
                                    <h2 class="card-title chat-bubble text-gray-800 dark:text-white bg-white font-mono">
                                        "Dimensions"
                                    </h2>
                                    <DrawerBtn drawer_id="dimension_drawer"
                                        .to_string()>
                                        Create Dimension <i class="ri-edit-2-line ml-2"></i>
                                    </DrawerBtn>
                                </div>
                                <Table
                                    rows=table_rows
                                    key_column="id".to_string()
                                    columns=table_columns.get()
                                    pagination=pagination_props
                                />
                            </div>
                        </div>
                    }
                }}
                <DeleteModal
                    modal_visible=delete_modal_visible_rs
                    confirm_delete=confirm_delete
                    set_modal_visible=delete_modal_visible_ws
                    header_text="Are you sure you want to delete this dimension? Action is irreversible."
                        .to_string()
                />
            </Suspense>
        </div>
    }
}
