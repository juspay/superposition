use std::collections::HashMap;

use crate::components::default_config_form::default_config_form::DefaultConfigForm;
use crate::components::modal::modal::Modal;
use crate::components::table::{table::Table, types::Column};

use crate::api::fetch_default_config;
use crate::components::button::button::Button;
use crate::components::stat::stat::Stat;
use crate::utils::{close_modal, show_modal};
use leptos::*;
use serde_json::{json, Map, Value};

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub key: String,
    pub value: String,
    pub pattern: String,
    pub type_: String,
}

#[component]
pub fn DefaultConfig() -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let default_config_resource = create_blocking_resource(
        move || tenant_rs.get(),
        |current_tenant| async move {
            match fetch_default_config(current_tenant).await {
                Ok(data) => data,
                Err(_) => vec![],
            }
        },
    );

    let selected_config = create_rw_signal::<Option<RowData>>(None);

    let table_columns = create_memo(move |_| {
        let edit_col_formatter = move |_: &str, row: &Map<String, Value>| {
            logging::log!("{:?}", row);
            let row_key = row["key"].clone().to_string().replace("\"", "");
            let row_value = row["value"].clone().to_string().replace("\"", "");

            let schema = row["schema"].clone().to_string();
            let schema_object = serde_json::from_str::<HashMap<String, Value>>(&schema)
                .unwrap_or(HashMap::new());

            let pattern_or_enum = schema_object
                .keys()
                .find(|key| {
                    key.to_string() == "pattern".to_string()
                        || key.to_string() == "enum".to_string()
                })
                .and_then(|val| Some(val.clone()))
                .unwrap_or(String::new());

            let row_type = match schema_object.get("type") {
                Some(Value::String(type_)) if type_ == "string" => {
                    pattern_or_enum.clone()
                }
                Some(Value::String(type_)) if type_ == "number" => type_.clone(),
                Some(Value::String(_)) => String::from("other"),
                Some(_) | None => String::new(),
            };

            let row_pattern = match schema_object.get("type") {
                Some(Value::String(type_))
                    if type_ == "string" && pattern_or_enum == "pattern" =>
                {
                    schema_object
                        .get(&pattern_or_enum)
                        .and_then(|val| Some(val.clone().to_string()))
                        .unwrap_or(String::new())
                        .replace("\"", "")
                }
                Some(Value::String(type_))
                    if type_ == "string" && pattern_or_enum == "enum" =>
                {
                    schema_object
                        .get(&pattern_or_enum)
                        .and_then(|val| {
                            if let Value::Array(v) = val {
                                return format!(
                                    "[{}]",
                                    v.iter()
                                        .map(|v| v.to_string())
                                        .collect::<Vec<String>>()
                                        .join(",")
                                )
                                .into();
                            }
                            None
                        })
                        .unwrap_or(String::new())
                }
                Some(Value::String(type_)) if type_ == "number" => String::new(),
                Some(Value::String(_)) => schema,
                _ => String::new(),
            };

            let edit_click_handler = move |_| {
                let row_data = RowData {
                    key: row_key.clone(),
                    value: row_value.clone(),
                    type_: row_type.clone(),
                    pattern: row_pattern.clone(),
                };

                logging::log!("{:?}", row_data);

                selected_config.set(Some(row_data));
                show_modal("default_config_modal_form");
            };

            let edit_icon: HtmlElement<html::I> =
                view! { <i class="ri-pencil-line ri-xl text-blue-500"></i> };

            view! { <span on:click=edit_click_handler>{edit_icon}</span> }.into_view()
        };
        vec![
            Column::default("key".to_string()),
            Column::default("schema".to_string()),
            Column::default("value".to_string()),
            Column::default("created_at".to_string()),
            Column::default("created_by".to_string()),
            Column::new("EDIT".to_string(), None, edit_col_formatter),
        ]
    });

    view! {
        <div class="p-8">
            <Modal
                id="default_config_modal_form".to_string()
                handle_close=move || {
                    close_modal("default_config_modal_form");
                    selected_config.set(None);
                }
            >

                {move || {
                    if let Some(selected_config_data) = selected_config.get() {
                        view! {
                            <DefaultConfigForm
                                edit=true
                                config_key=selected_config_data.key
                                config_value=selected_config_data.value
                                config_type=selected_config_data.type_
                                config_pattern=selected_config_data.pattern
                                handle_submit=move || {
                                    default_config_resource.refetch();
                                    close_modal("default_config_modal_form");
                                    selected_config.set(None);
                                }
                            />
                        }
                    } else {
                        view! {
                            <DefaultConfigForm handle_submit=move || {
                                default_config_resource.refetch();
                                close_modal("default_config_modal_form");
                            }/>
                        }
                    }
                }}

            </Modal>
            <Suspense fallback=move || {
                view! { <p>"Loading (Suspense Fallback)..."</p> }
            }>

                {move || {
                    let default_config = default_config_resource.get().unwrap_or(vec![]);
                    let total_default_config_keys = default_config.len().to_string();
                    let table_rows = default_config
                        .into_iter()
                        .map(|config| {
                            let mut ele_map = json!(config).as_object().unwrap().to_owned();
                            ele_map
                                .insert(
                                    "created_at".to_string(),
                                    json!(config.created_at.format("%v").to_string()),
                                );
                            ele_map
                        })
                        .collect::<Vec<Map<String, Value>>>();
                    view! {
                        <div class="pb-4">
                            <Stat
                                heading="Config Keys"
                                icon="ri-tools-line"
                                number=total_default_config_keys
                            />
                        </div>
                        <div class="card rounded-lg w-full bg-base-100 shadow">
                            <div class="card-body">
                                <div class="flex justify-between">
                                    <h2 class="card-title chat-bubble text-gray-800 dark:text-white bg-white font-mono">
                                        "Default Config"
                                    </h2>
                                    <Button
                                        text="Create Key".to_string()
                                        on_click=move |_| {
                                            show_modal("default_config_modal_form")
                                        }
                                    />

                                </div>
                                <Table
                                    cell_style="min-w-48 font-mono".to_string()
                                    rows=table_rows
                                    key_column="id".to_string()
                                    columns=table_columns.get()
                                />
                            </div>
                        </div>
                    }
                }}

            </Suspense>
        </div>
    }
}
