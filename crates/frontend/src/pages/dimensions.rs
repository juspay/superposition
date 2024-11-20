use crate::components::button::Button;
use crate::components::skeleton::Skeleton;
use crate::components::{
    delete_modal::DeleteModal,
    stat::Stat,
    table::{types::Column, Table},
};
use leptos::*;
use leptos_router::A;
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
    let tenant_rs = use_context::<Signal<String>>().unwrap();
    let (delete_modal_visible_rs, delete_modal_visible_ws) = create_signal(false);
    let (delete_id_rs, delete_id_ws) = create_signal::<Option<String>>(None);
    let dimensions_resource = create_blocking_resource(
        move || tenant_rs.get(),
        |tenant| async move {
            match fetch_dimensions(&tenant).await {
                Ok(data) => data,
                Err(_) => vec![],
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

    let table_columns = create_memo(move |_| {
        let action_col_formatter = move |_: &str, row: &Map<String, Value>| {
            logging::log!("Dimension row: {:?}", row);
            let row_dimension = row["dimension"].to_string().replace('"', "");
            let dimension_name = row_dimension.clone();

            if dimension_name.clone() == String::from("variantIds") {
                view! {
                    <div class="join">
                        <A href=format!("{row_dimension}/update")>
                            <i class="ri-pencil-line ri-xl text-blue-500"></i>
                        </A>
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
                        <A href=format!("{row_dimension}/update")>
                            <i class="ri-pencil-line ri-xl text-blue-500"></i>
                        </A>
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
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
            {move || {
                let value = dimensions_resource.get().unwrap_or(vec![]);
                let total_items = value.len().to_string();
                let table_rows = value
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
                                <A href="new">
                                    <Button text="Create Key" on_click=move |_| {} />
                                </A>
                            </div>
                            <Table
                                cell_class="min-w-48 font-mono".to_string()
                                rows=table_rows
                                key_column="id".to_string()
                                columns=table_columns.get()
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
    }
}
