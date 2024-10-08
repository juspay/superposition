use crate::components::dimension_form::DimensionForm;
use crate::components::drawer::{close_drawer, open_drawer, Drawer, DrawerBtn};
use crate::components::skeleton::Skeleton;
use crate::components::{
    delete_modal::DeleteModal,
    stat::Stat,
    table::{
        types::{Column, TablePaginationProps},
        Table,
    },
};
use crate::types::{ListFilters, PaginatedResponse};
use leptos::*;

use serde_json::{json, Map, Value};

use crate::api::{delete_dimension, fetch_dimensions};

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub dimension: String,
    pub priority: u32,
    pub schema: Value,
    pub function_name: Option<Value>,
    pub mandatory: bool,
}

#[component]
pub fn dimensions() -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let (delete_modal_visible_rs, delete_modal_visible_ws) = create_signal(false);
    let (delete_id_rs, delete_id_ws) = create_signal::<Option<String>>(None);
    let (filters, set_filters) = create_signal(ListFilters {
        page: Some(1),
        count: Some(10),
    });
    let dimensions_resource = create_blocking_resource(
        move || (tenant_rs.get(), filters.get()),
        |(current_tenant, filters)| async move {
            match fetch_dimensions(filters, current_tenant).await {
                Ok(data) => data,
                Err(_) => PaginatedResponse {
                    total_items: 0,
                    total_pages: 0,
                    data: vec![],
                },
            }
        },
    );

    let confirm_delete = Callback::new(move |_| {
        if let Some(id) = delete_id_rs.get().clone() {
            spawn_local(async move {
                let result = delete_dimension(id, tenant_rs.get()).await;

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
    let handle_next_click = Callback::new(move |total_pages: i64| {
        set_filters.update(|f| {
            f.page = match f.page {
                Some(p) if p < total_pages => Some(p + 1),
                Some(p) => Some(p),
                None => None,
            }
        });
    });

    let handle_prev_click = Callback::new(move |_| {
        set_filters.update(|f| {
            f.page = match f.page {
                Some(p) if p > 1 => Some(p - 1),
                Some(p) => Some(p),
                None => None,
            }
        });
    });

    let selected_dimension = create_rw_signal::<Option<RowData>>(None);

    let table_columns = create_memo(move |_| {
        let action_col_formatter = move |_: &str, row: &Map<String, Value>| {
            logging::log!("Dimension row: {:?}", row);
            let row_dimension = row["dimension"].to_string().replace('"', "");
            let row_priority_str = row["priority"].to_string().replace('"', "");
            let row_priority = row_priority_str.parse::<u32>().unwrap_or(0_u32);

            let schema = row["schema"].clone().to_string();
            let schema = serde_json::from_str::<Value>(&schema).unwrap_or(Value::Null);

            let function_name = row["function_name"].to_string();
            let fun_name = match function_name.as_str() {
                "null" => None,
                _ => Some(json!(function_name.replace('"', ""))),
            };
            let mandatory = row["mandatory"].as_bool().unwrap_or(false);
            let dimension_name = row_dimension.clone();

            let edit_click_handler = move |_| {
                let row_data = RowData {
                    dimension: row_dimension.clone(),
                    priority: row_priority.clone(),
                    schema: schema.clone(),
                    function_name: fun_name.clone(),
                    mandatory: mandatory.clone(),
                };
                logging::log!("{:?}", row_data);
                selected_dimension.set(Some(row_data));
                open_drawer("dimension_drawer");
            };

            if dimension_name.clone() == String::from("variantIds") {
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
            Column::default("dimension".to_string()),
            Column::default("priority".to_string()),
            Column::default("schema".to_string()),
            Column::default("mandatory".to_string()),
            Column::default("function_name".to_string()),
            Column::default("created_by".to_string()),
            Column::default("created_at".to_string()),
            Column::new("actions".to_string(), None, action_col_formatter),
        ]
    });

    view! {
        <div class="p-8">
            {move || {
                let handle_close = move || {
                    close_drawer("dimension_drawer");
                    selected_dimension.set(None);
                };
                if let Some(selected_dimension_data) = selected_dimension.get() {
                    view! {
                        <Drawer
                            id="dimension_drawer".to_string()
                            header="Edit Dimension"
                            handle_close=handle_close
                        >
                            <DimensionForm
                                edit=true
                                priority=selected_dimension_data.priority
                                dimension_name=selected_dimension_data.dimension
                                dimension_schema=selected_dimension_data.schema
                                function_name=selected_dimension_data.function_name
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
                            <DimensionForm handle_submit=move || {
                                dimensions_resource.refetch();
                                close_drawer("dimension_drawer");
                            }/>
                        </Drawer>
                    }
                }
            }}
            <Suspense fallback=move || {
                view! { <Skeleton/> }
            }>
                {move || {
                    let value = dimensions_resource
                        .get()
                        .unwrap_or(PaginatedResponse {
                            total_items: 0,
                            total_pages: 0,
                            data: vec![],
                        });
                    let total_items = value.data.len().to_string();
                    let table_rows = value
                        .data
                        .iter()
                        .map(|ele| {
                            let mut ele_map = json!(ele).as_object().unwrap().clone();
                            ele_map
                                .insert(
                                    "created_at".to_string(),
                                    json!(ele.created_at.format("%v").to_string()),
                                );
                            ele_map
                        })
                        .collect::<Vec<Map<String, Value>>>();
                    let filters = filters.get();
                    let pagination_props = TablePaginationProps {
                        enabled: true,
                        count: filters.count.unwrap_or_default(),
                        current_page: filters.page.unwrap_or_default(),
                        total_pages: value.total_pages,
                        on_next: handle_next_click,
                        on_prev: handle_prev_click,
                    };
                    view! {
                        <div class="pb-4">
                            <Stat heading="Dimensions" icon="ri-ruler-2-fill" number=total_items/>
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
                                    cell_class="min-w-48 font-mono".to_string()
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
