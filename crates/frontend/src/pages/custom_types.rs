use leptos::*;
use serde::Deserialize;
use serde_json::{json, Map, Value};
use superposition_types::custom_query::PaginationParams;

use crate::components::table::types::ColumnSortable;
use crate::components::type_template_form::utils::delete_type;
use crate::components::{
    alert::AlertType,
    delete_modal::DeleteModal,
    drawer::{close_drawer, open_drawer, Drawer, DrawerBtn},
    skeleton::Skeleton,
    stat::Stat,
    table::Table,
    type_template_form::TypeTemplateForm,
};
use crate::providers::alert_provider::enqueue_alert;
use crate::types::{OrganisationId, Tenant};
use crate::utils::unwrap_option_or_default_with_error;
use crate::{api::fetch_types, components::table::types::Column};

#[derive(Debug, Clone, PartialEq, Deserialize)]
struct TypeTemplateRow {
    pub type_name: String,
    pub type_schema: Value,
}

const TYPE_DRAWER_ID: &str = "type_template_drawer";

#[component]
pub fn types_page() -> impl IntoView {
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let types_resource = create_blocking_resource(
        move || (tenant_rws.get().0, org_rws.get().0),
        |(t, org_id)| async move {
            fetch_types(&PaginationParams::default(), t, org_id)
                .await
                .map_or_else(
                    |err| {
                        logging::log!("failed to get types due to: {:?}", err);
                        vec![]
                    },
                    |types| types.data,
                )
        },
    );

    let (delete_modal_visible_rs, delete_modal_visible_ws) = create_signal(false);
    let (delete_row_rs, delete_row_ws) = create_signal::<Option<TypeTemplateRow>>(None);

    let confirm_delete = Callback::new(move |_| {
        if let Some(row_data) = delete_row_rs.get().clone() {
            spawn_local(async move {
                let tenant = tenant_rws.get().0;
                let org = org_rws.get().0;
                let api_response =
                    delete_type(tenant, row_data.clone().type_name, org).await;
                match api_response {
                    Ok(_) => {
                        enqueue_alert(
                            "type template deleted successfully".to_string(),
                            AlertType::Success,
                            5000,
                        );
                        types_resource.refetch();
                    }
                    Err(err) => enqueue_alert(err, AlertType::Error, 5000),
                }
                types_resource.refetch();
            });
        }
        delete_row_ws.set(None);
        delete_modal_visible_ws.set(false);
    });
    let selected_type = create_rw_signal::<Option<TypeTemplateRow>>(None);
    let table_columns = create_memo(move |_| {
        vec![
            Column::default("type_name".to_string()),
            Column::default("type_schema".to_string()),
            Column::default("created_by".to_string()),
            Column::default("created_at".to_string()),
            Column::default("last_modified_at".to_string()),
            Column::new(
                "actions".into(),
                None,
                move |_: &str, row: &Map<String, Value>| {
                    let edit_row_json = json!(row);
                    let delete_row_json = edit_row_json.clone();
                    let edit_click = move |_| {
                        let row_data = serde_json::from_value::<TypeTemplateRow>(
                            edit_row_json.clone(),
                        )
                        .unwrap();
                        selected_type.set(Some(row_data));
                        open_drawer(TYPE_DRAWER_ID);
                    };
                    let delete_click = move |_| {
                        let row_data = serde_json::from_value::<TypeTemplateRow>(
                            delete_row_json.clone(),
                        )
                        .unwrap();
                        delete_row_ws.set(Some(row_data));
                        delete_modal_visible_ws.set(true);
                    };
                    view! {
                        <div class="join">
                            <span class="cursor-pointer" on:click=edit_click>
                                <i class="ri-pencil-line ri-xl text-blue-500"></i>
                            </span>
                            <span class="cursor-pointer" on:click=delete_click>
                                <i class="ri-delete-bin-line ri-xl text-red-500"></i>
                            </span>
                        </div>
                    }
                    .into_view()
                },
                ColumnSortable::No,
            ),
        ]
    });

    view! {
        {move || {
            let handle_close = move || {
                close_drawer(TYPE_DRAWER_ID);
                selected_type.set(None);
            };
            if let Some(selected_type_data) = selected_type.get() {
                view! {
                    <Drawer
                        id=TYPE_DRAWER_ID.to_string()
                        header="Edit Type Template"
                        handle_close=handle_close
                    >
                        <TypeTemplateForm
                            edit=true
                            type_name=selected_type_data.type_name
                            type_schema=selected_type_data.type_schema
                            handle_submit=move || {
                                types_resource.refetch();
                                selected_type.set(None);
                                close_drawer(TYPE_DRAWER_ID);
                            }
                        />

                    </Drawer>
                }
            } else {
                view! {
                    <Drawer
                        id=TYPE_DRAWER_ID.to_string()
                        header="Create New Type Template"
                        handle_close=handle_close
                    >
                        <TypeTemplateForm handle_submit=move || {
                            types_resource.refetch();
                            selected_type.set(None);
                            close_drawer(TYPE_DRAWER_ID);
                        } />

                    </Drawer>
                }
            }
        }}

        <div class="p-8">
            <Suspense fallback=move || view! { <Skeleton /> }>
                <div class="pb-4">
                    {move || {
                        let types = types_resource.get().unwrap_or(vec![]);
                        let data = types
                            .iter()
                            .map(|ele| {
                                let mut ele_map = unwrap_option_or_default_with_error(
                                        json!(ele).as_object(),
                                        &Map::new(),
                                    )
                                    .to_owned();
                                ele_map
                                    .insert(
                                        "created_at".to_string(),
                                        json!(ele.created_at.format("%v").to_string()),
                                    );
                                ele_map
                                    .insert(
                                        "last_modified_at".to_string(),
                                        json!(ele.last_modified_at.format("%v").to_string()),
                                    );
                                ele_map
                            })
                            .collect::<Vec<Map<String, Value>>>()
                            .to_owned();
                        view! {
                            <div class="pb-4">
                                <Stat
                                    heading="Type Templates"
                                    icon="ri-t-box-fill"
                                    number=types.len().to_string()
                                />
                            </div>
                            <div class="card rounded-xl w-full bg-base-100 shadow">
                                <div class="card-body">
                                    <div class="flex justify-between">
                                        <h2 class="card-title">Type Templates</h2>
                                        <div>
                                            <DrawerBtn drawer_id=TYPE_DRAWER_ID
                                                .to_string()>
                                                Create Type <i class="ri-add-fill ml-2"></i>
                                            </DrawerBtn>
                                        </div>
                                    </div>
                                    <Table
                                        cell_class="".to_string()
                                        rows=data
                                        key_column="id".to_string()
                                        columns=table_columns.get()
                                    />
                                </div>
                            </div>
                            <DeleteModal
                                modal_visible=delete_modal_visible_rs
                                confirm_delete=confirm_delete
                                set_modal_visible=delete_modal_visible_ws
                                header_text="Are you sure you want to delete this type template? Action is irreversible."
                                    .to_string()
                            />
                        }
                    }}

                </div>
            </Suspense>
        </div>
    }
}
