use crate::api::{delete_dimension, fetch_dimensions};
use crate::components::button::Button;
use crate::components::skeleton::Skeleton;
use crate::components::table::types::ColumnSortable;
use crate::components::{
    delete_modal::DeleteModal,
    stat::Stat,
    table::{
        types::{Column, TablePaginationProps},
        Table,
    },
};
use crate::types::{OrganisationId, Tenant};
use crate::utils::update_page_direction;

use leptos::*;
use leptos_router::A;
use serde_json::{json, Map, Value};
use superposition_types::custom_query::PaginationParams;

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub dimension: String,
    pub position: u32,
    pub schema: Value,
    pub function_name: Option<Value>,
    pub mandatory: bool,
}

#[component]
pub fn dimensions() -> impl IntoView {
    let tenant_s = use_context::<Signal<Tenant>>().unwrap();
    let org_s = use_context::<Signal<OrganisationId>>().unwrap();
    let (delete_modal_visible_rs, delete_modal_visible_ws) = create_signal(false);
    let (delete_id_rs, delete_id_ws) = create_signal::<Option<String>>(None);
    let (filters, set_filters) = create_signal(PaginationParams::default());
    let dimensions_resource = create_blocking_resource(
        move || (tenant_s.get().0, filters.get(), org_s.get().0),
        |(tenant, filters, org_id)| async move {
            fetch_dimensions(&filters, &tenant, &org_id)
                .await
                .unwrap_or_default()
        },
    );

    let confirm_delete = Callback::new(move |_| {
        if let Some(id) = delete_id_rs.get().clone() {
            spawn_local(async move {
                let result =
                    delete_dimension(id, tenant_s.get().0, org_s.get().0).await;

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
            f.page = update_page_direction(f.page, total_pages, true);
        });
    });

    let handle_prev_click = Callback::new(move |_| {
        set_filters.update(|f| {
            f.page = update_page_direction(f.page, 1, false);
        });
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
            Column::default("position".to_string()),
            Column::default("schema".to_string()),
            Column::default("mandatory".to_string()),
            Column::default("function_name".to_string()),
            Column::default("created_by".to_string()),
            Column::default("created_at".to_string()),
            Column::new(
                "actions".to_string(),
                None,
                action_col_formatter,
                ColumnSortable::No,
            ),
        ]
    });

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
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
    }
}
