use crate::api::fetch_default_config;
use crate::components::default_config_form::default_config_form::DefaultConfigForm;
use crate::components::drawer::drawer::{close_drawer, open_drawer, Drawer, DrawerBtn};
use crate::components::stat::stat::Stat;
use crate::components::table::{table::Table, types::Column};
use crate::types::BreadCrums;
use leptos::*;
use leptos_router::{use_navigate, use_query_map};
use serde_json::{json, Map, Value};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub key: String,
    pub value: String,
    pub pattern: String,
    pub type_: String,
    pub function_name: Option<Value>,
}

#[component]
pub fn default_config() -> impl IntoView {
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
    let key_prefix = create_rw_signal::<Option<String>>(None);
    let enable_grouping = create_rw_signal(false);
    let query_params = use_query_map();
    let bread_crums = Signal::derive(move || get_bread_crums(key_prefix.get()));

    create_effect(move |_| {
        let query_params_map = query_params.try_get();
        if let Some(query_map) = query_params_map {
            let opt_prefix = query_map.get("prefix");
            key_prefix.set(opt_prefix.cloned());
            if opt_prefix.is_some() {
                enable_grouping.set(true);
            }
        }
    });

    let folder_click_handler = move |key_name: Option<String>| {
        let tenant = tenant_rs.get();
        let redirect_url = match key_name {
            Some(prefix) => format!("admin/{tenant}/default-config?prefix={prefix}"),
            None => format!("admin/{tenant}/default-config"),
        };
        logging::log!("redirecting to {:?}", redirect_url.clone());
        let navigate = use_navigate();
        navigate(redirect_url.as_str(), Default::default());
    };

    let table_columns = create_memo(move |_| {
        let edit_col_formatter = move |_: &str, row: &Map<String, Value>| {
            logging::log!("{:?}", row);
            let row_key = row["key"].clone().to_string().replace("\"", "");
            let is_folder = row_key.contains(".");
            let row_value = row["value"].clone().to_string().replace("\"", "");

            let schema = row["schema"].clone().to_string();
            let schema_object = serde_json::from_str::<HashMap<String, Value>>(&schema)
                .unwrap_or(HashMap::new());

            let function_name = row["function_name"].clone().to_string();
            let fun_name = match function_name.as_str() {
                "null" => None,
                _ => Some(json!(function_name.replace("\"", ""))),
            };

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
                    function_name: fun_name.clone(),
                };
                logging::log!("{:?}", row_data);
                selected_config.set(Some(row_data));
                open_drawer("default_config_drawer");
            };

            let edit_icon: HtmlElement<html::I> =
                view! { <i class="ri-pencil-line ri-xl text-blue-500"></i> };

            if is_folder {
                view! { <span>{"-"}</span> }.into_view()
            } else {
                view! {
                    <span class="cursor-pointer" on:click=edit_click_handler>
                        {edit_icon}
                    </span>
                }
                .into_view()
            }
        };

        let expand = move |_: &str, row: &Map<String, Value>| {
            let key_name = row["key"].clone().to_string().replace("\"", "");
            let label = key_name.clone();
            let is_folder = key_name.contains(".");

            if is_folder && enable_grouping.get() {
                view! {
                    <span
                        class="cursor-pointer text-blue-500 underline underline-offset-2"
                        on:click=move |_| {
                            let mut key = key_name.clone();
                            if let Some(prefix_) = key_prefix.get() {
                                key = prefix_.clone() + &key;
                            }
                            folder_click_handler(Some(key.clone()))
                        }
                    >

                        {label}
                    </span>
                }
                .into_view()
            } else {
                view! { <span>{key_name}</span> }.into_view()
            }
        };

        vec![
            Column::new("key".to_string(), None, expand),
            Column::default("schema".to_string()),
            Column::default("value".to_string()),
            Column::default("function_name".to_string()),
            Column::default("created_at".to_string()),
            Column::default("created_by".to_string()),
            Column::new("EDIT".to_string(), None, edit_col_formatter),
        ]
    });

    let handle_close = move || {
        selected_config.set(None);
        close_drawer("default_config_drawer");
    };

    view! {
        <div class="p-8">
            <Suspense fallback=move || {
                view! { <p>"Loading (Suspense Fallback)..."</p> }
            }>

                {move || {
                    let prefix = key_prefix.get();
                    if let Some(selected_config_data) = selected_config.get() {
                        view! {
                            <Drawer
                                id="default_config_drawer".to_string()
                                header="Edit Key"
                                handle_close=handle_close
                            >
                                <DefaultConfigForm
                                    edit=true
                                    config_key=selected_config_data.key
                                    config_value=selected_config_data.value
                                    config_type=selected_config_data.type_
                                    config_pattern=selected_config_data.pattern
                                    function_name=selected_config_data.function_name
                                    prefix
                                    handle_submit=move || {
                                        default_config_resource.refetch();
                                        selected_config.set(None);
                                        close_drawer("default_config_drawer");
                                    }
                                />

                            </Drawer>
                        }
                    } else {
                        view! {
                            <Drawer
                                id="default_config_drawer".to_string()
                                header="Create New Key"
                                handle_close=handle_close
                            >
                                <DefaultConfigForm
                                    prefix
                                    handle_submit=move || {
                                        default_config_resource.refetch();
                                        selected_config.set(None);
                                        close_drawer("default_config_drawer");
                                    }
                                />

                            </Drawer>
                        }
                    }
                }}
                {move || {
                    let default_config = default_config_resource.get().unwrap_or(vec![]);
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
                    let mut filtered_rows = table_rows.clone();
                    if enable_grouping.get() {
                        let empty_map = Map::new();
                        let cols = filtered_rows
                            .get(0)
                            .unwrap_or(&empty_map)
                            .keys()
                            .map(|key| key.as_str())
                            .collect();
                        filtered_rows = modify_rows(filtered_rows.clone(), key_prefix.get(), cols);
                    }
                    let total_default_config_keys = filtered_rows.len().to_string();
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
                                <div class="flex justify-between pb-2">
                                    <BreadCrums bread_crums=bread_crums.get() folder_click_handler/>
                                    <div class="flex">
                                        <label
                                            on:click=move |_| {
                                                folder_click_handler(None);
                                                enable_grouping.set(!enable_grouping.get());
                                            }

                                            class="cursor-pointer label mr-10"
                                        >
                                            <span class="label-text mr-4">Enable Grouping</span>
                                            <input
                                                type="checkbox"
                                                class="toggle toggle-primary"
                                                checked=enable_grouping.get()
                                            />
                                        </label>
                                        <DrawerBtn drawer_id="default_config_drawer"
                                            .to_string()>
                                            Create Key <i class="ri-edit-2-line ml-2"></i>
                                        </DrawerBtn>
                                    </div>
                                </div>
                                <Table
                                    cell_style="min-w-48 font-mono".to_string()
                                    rows=filtered_rows
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

#[component]
pub fn bread_crums<NF>(
    bread_crums: Vec<BreadCrums>,
    folder_click_handler: NF,
) -> impl IntoView
where
    NF: Fn(Option<String>) + 'static + Clone,
{
    let last_index = bread_crums.len() - 1;
    view! {
        <div class="flex justify-between pt-3">

            {bread_crums
                .iter()
                .enumerate()
                .map(|(index, ele)| {
                    let value = ele.value.clone();
                    let is_link = ele.is_link.clone();
                    let handler = folder_click_handler.clone();
                    view! {
                        <div class="flex">
                            <h2
                                on:click=move |_| {
                                    if is_link {
                                        handler(value.clone())
                                    }
                                }

                                class=if ele.is_link {
                                    "cursor-pointer text-blue-500 underline underline-offset-2"
                                } else {
                                    ""
                                }
                            >

                                {ele.key.clone()}
                            </h2>
                            <h2 class="pl-4 pr-4">{if index < last_index { ">" } else { "" }}</h2>
                        </div>
                    }
                })
                .collect_view()}
        </div>
    }
}

pub fn get_bread_crums(key_prefix: Option<String>) -> Vec<BreadCrums> {
    let mut default_bread_crums = vec![BreadCrums {
        key: "Default Config".to_string(),
        value: None,
        is_link: true,
    }];

    let mut bread_crums = match key_prefix {
        Some(prefix) => {
            let prefix_arr = prefix
                .trim_matches('.')
                .split(".")
                .map(str::to_string)
                .collect::<Vec<String>>();
            prefix_arr
                .into_iter()
                .fold(String::new(), |mut prefix, ele| {
                    prefix.push_str(&ele);
                    prefix.push('.');
                    default_bread_crums.push(BreadCrums {
                        key: ele.clone(),
                        value: Some(prefix.clone()),
                        is_link: true,
                    });
                    prefix
                });
            default_bread_crums
        }
        None => default_bread_crums,
    };
    if let Some(last_crumb) = bread_crums.last_mut() {
        last_crumb.is_link = false;
    }
    bread_crums
}

pub fn modify_rows(
    filtered_rows: Vec<Map<String, Value>>,
    key_prefix: Option<String>,
    cols: Vec<&str>,
) -> Vec<Map<String, Value>> {
    let mut groups: HashSet<String> = HashSet::new();
    filtered_rows
        .into_iter()
        .filter_map(|mut ele| {
            let key = ele.get("key").unwrap().to_owned();

            let key_arr = match &key_prefix {
                Some(prefix) => key
                    .to_string()
                    .split(prefix)
                    .map(str::to_string)
                    .collect::<Vec<String>>(),
                None => vec!["".to_string(), key.to_string()],
            };
            // key_arr.get(1) retrieves the remaining part of the key, after removing the prefix.
            if let Some(filtered_key) = key_arr.get(1) {
                let new_key = filtered_key
                    .split(".")
                    .map(str::to_string)
                    .collect::<Vec<String>>();
                let key = new_key.get(0).unwrap().to_owned().replace("\"", "");
                if new_key.len() == 1 {
                    // key
                    ele.insert("key".to_string(), json!(key));
                } else {
                    // folder
                    let folder = key + ".";
                    if !groups.contains(&folder) {
                        cols.iter().for_each(|col| {
                            ele.insert(
                                col.to_string(),
                                json!(if *col == "key" {
                                    folder.clone()
                                } else {
                                    "-".to_string()
                                }),
                            );
                        });
                        groups.insert(folder);
                    } else {
                        return None;
                    }
                }
                Some(ele)
            } else {
                None
            }
        })
        .collect()
}
