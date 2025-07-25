use leptos::*;
use serde::Deserialize;
use serde_json::{json, Map, Value};
use superposition_types::custom_query::PaginationParams;

use crate::components::table::types::{
    default_column_formatter, ColumnSortable, Expandable,
};
use crate::components::type_template_form::utils::delete_type;
use crate::components::type_template_form::{ChangeLogSummary, ChangeType};
use crate::components::{
    alert::AlertType,
    description_icon::InfoDescription,
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
    pub description: String,
}

const TYPE_DRAWER_ID: &str = "type_template_drawer";

#[component]
pub fn types_page() -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let delete_inprogress_rws = RwSignal::new(false);
    let types_resource = create_blocking_resource(
        move || (workspace.get().0, org.get().0),
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

    let (delete_type_rs, delete_type_ws) = create_signal(None);

    let confirm_delete = move |_| {
        if let Some(type_name) = delete_type_rs.get().clone() {
            delete_inprogress_rws.set(true);
            spawn_local(async move {
                let tenant = workspace.get().0;
                let org = org.get().0;
                let api_response = delete_type(tenant, type_name, org).await;
                delete_inprogress_rws.set(false);
                match api_response {
                    Ok(_) => {
                        enqueue_alert(
                            "type template deleted successfully".to_string(),
                            AlertType::Success,
                            5000,
                        );
                        delete_type_ws.set(None);
                        types_resource.refetch();
                    }
                    Err(err) => enqueue_alert(err, AlertType::Error, 5000),
                }
                types_resource.refetch();
            });
        }
    };

    let expand = move |type_name: &str, row: &Map<String, Value>| {
        let description = row
            .get("description")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        let change_reason = row
            .get("change_reason")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();

        let type_name = type_name.to_string();

        view! {
            <span class="mr-2">{type_name}</span>
            <InfoDescription description=description change_reason=change_reason />
        }
        .into_view()
    };

    let selected_type = create_rw_signal::<Option<TypeTemplateRow>>(None);
    let table_columns = create_memo(move |_| {
        vec![
            Column::new(
                "type_name".to_string(),
                false,
                expand,
                ColumnSortable::No,
                Expandable::Disabled,
                default_column_formatter,
            ),
            Column::default("type_schema".to_string()),
            Column::default("created_by".to_string()),
            Column::default("created_at".to_string()),
            Column::default("last_modified_at".to_string()),
            Column::default_with_cell_formatter(
                "actions".into(),
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
                        delete_type_ws.set(Some(row_data.type_name));
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
                    <Drawer id=TYPE_DRAWER_ID header="Edit Type Template" handle_close=handle_close>
                        <TypeTemplateForm
                            edit=true
                            type_name=selected_type_data.type_name
                            type_schema=selected_type_data.type_schema
                            handle_submit=move |_| {
                                types_resource.refetch();
                                selected_type.set(None);
                                close_drawer(TYPE_DRAWER_ID);
                            }
                            description=selected_type_data.description
                        />

                    </Drawer>
                }
            } else {
                view! {
                    <Drawer
                        id=TYPE_DRAWER_ID
                        header="Create New Type Template"
                        handle_close=handle_close
                    >
                        <TypeTemplateForm handle_submit=move |_| {
                            types_resource.refetch();
                            selected_type.set(None);
                            close_drawer(TYPE_DRAWER_ID);
                        } />

                    </Drawer>
                }
            }
        }}

        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
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
                                json!(ele.created_at.format("%v %T").to_string()),
                            );
                        ele_map
                            .insert(
                                "last_modified_at".to_string(),
                                json!(ele.last_modified_at.format("%v %T").to_string()),
                            );
                        ele_map
                    })
                    .collect::<Vec<Map<String, Value>>>()
                    .to_owned();
                view! {
                    <div class="h-full flex flex-col gap-4">
                        <div class="flex justify-between">
                            <Stat
                                heading="Type Templates"
                                icon="ri-t-box-fill"
                                number=types.len().to_string()
                            />
                            <DrawerBtn
                                drawer_id=TYPE_DRAWER_ID
                                class="self-end"
                                text="Create Type"
                                icon_class="ri-add-line"
                            />
                        </div>
                        <div class="card w-full bg-base-100 rounded-xl overflow-hidden shadow">
                            <div class="card-body overflow-y-auto overflow-x-visible">
                                <Table
                                    class="!overflow-y-auto"
                                    rows=data
                                    key_column="id".to_string()
                                    columns=table_columns.get()
                                />
                            </div>
                        </div>
                    </div>
                }
            }}
            {move || {
                if let Some(type_name) = delete_type_rs.get() {
                    view! {
                        <ChangeLogSummary
                            type_name
                            change_type=ChangeType::Delete
                            on_close=move |_| delete_type_ws.set(None)
                            on_confirm=confirm_delete
                            inprogress=delete_inprogress_rws
                        />
                    }
                } else {
                    ().into_view()
                }
            }}
        </Suspense>
    }
}
