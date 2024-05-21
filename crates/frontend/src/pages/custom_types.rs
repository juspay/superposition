use crate::components::type_template_form::utils::delete_type;
use crate::components::{
    drawer::{close_drawer, open_drawer, Drawer, DrawerBtn},
    skeleton::Skeleton,
    stat::Stat,
    table::Table,
    type_template_form::TypeTemplateForm,
};
use crate::utils::unwrap_option_or_default_with_error;
use crate::{api::fetch_types, components::table::types::Column};
use leptos::*;
use serde::Deserialize;
use serde_json::{json, Map, Value};

#[derive(Debug, Clone, PartialEq)]
struct TypeFilter {
    pub page: i64,
    pub count: i64,
}

#[derive(Debug, Clone, PartialEq, Deserialize)]
struct TypeTemplateRow {
    pub type_name: String,
    pub type_schema: Value,
}

const TYPE_DRAWER_ID: &str = "type_template_drawer";

#[component]
pub fn types_page() -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let (filters_rs, _) = create_signal(TypeFilter { page: 1, count: 10 });
    let types_resource = create_blocking_resource(
        move || (tenant_rs.get(), filters_rs.get()),
        |(t, filter)| async move {
            match fetch_types(t, filter.page, filter.count).await {
                Ok(types) => types.data,
                Err(err) => {
                    logging::log!("failed to get types due to: {:?}", err);
                    vec![]
                }
            }
        },
    );
    let selected_type = create_rw_signal::<Option<TypeTemplateRow>>(None);
    let table_columns = create_memo(move |_| {
        vec![
            Column::default("type_name".to_string()),
            Column::default("type_schema".to_string()),
            Column::default("created_by".to_string()),
            Column::default("created_at".to_string()),
            Column::default("last_modified".to_string()),
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
                        spawn_local({
                            async move {
                                let tenant = tenant_rs.get();
                                let _ =
                                    delete_type(tenant, row_data.clone().type_name).await;
                                types_resource.refetch();
                            }
                        });
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
                        }/>

                    </Drawer>
                }
            }
        }}

        <div class="p-8">
            <Suspense fallback=move || view! { <Skeleton/> }>
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
                                        "last_modified".to_string(),
                                        json!(ele.last_modified.format("%v").to_string()),
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
                                        cell_style="".to_string()
                                        rows=data
                                        key_column="id".to_string()
                                        columns=table_columns.get()
                                    />
                                </div>
                            </div>
                        }
                    }}

                </div>
            </Suspense>
        </div>
    }
}
