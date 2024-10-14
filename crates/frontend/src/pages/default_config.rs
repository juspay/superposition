use crate::api::{delete_default_config, fetch_default_config};

use crate::components::default_config_form::DefaultConfigForm;
use crate::components::drawer::{close_drawer, open_drawer, Drawer, DrawerBtn};
use crate::components::skeleton::Skeleton;
use crate::components::stat::Stat;
use crate::components::table::{types::Column, Table};

use crate::schema::HtmlDisplay;
use crate::types::BreadCrums;
use crate::utils::{get_local_storage, set_local_storage};
use leptos::*;
use leptos_router::{use_navigate, use_query_map};
use serde_json::{json, Map, Value};

#[cfg(feature = "multi-delimiter")]
const GROUPING_DELIMITER: &[char] = &['.', ':'];

#[cfg(not(feature = "multi-delimiter"))]
const GROUPING_DELIMITER: &[char] = &['.'];

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub key: String,
    pub value: String,
    pub schema: Value,
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
    let bread_crums = Signal::derive(move || utils::get_bread_crums(key_prefix.get()));

    create_effect(move |_| {
        let enable_grouping_val =
            get_local_storage::<bool>("enable_grouping").unwrap_or(false);
        enable_grouping.set(enable_grouping_val);
    });

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

    let on_folder_click = move |key_name: Option<String>| {
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
        let grouping_enabled = enable_grouping.get();
        let actions_col_formatter = move |_: &str, row: &Map<String, Value>| {
            let row_key = row["key"].html_display();
            let row_value = row["value"].html_display();
            let is_folder = row_key.contains(GROUPING_DELIMITER);

            let schema = row["schema"].clone();

            let function_name = row["function_name"].html_display();
            let fun_name = match function_name.as_str() {
                "null" => None,
                _ => Some(json!(function_name)),
            };

            let row_key = StoredValue::new(row_key.clone());

            let edit_click_handler = move |_| {
                let row_data = RowData {
                    key: row_key.get_value(),
                    value: row_value.clone(),
                    schema: schema.clone(),
                    function_name: fun_name.clone(),
                };
                logging::log!("{:?}", row_data);
                selected_config.set(Some(row_data));
                open_drawer("default_config_drawer");
            };

            let handle_delete = move |_| {
                let tenant = tenant_rs.get();
                let prefix = key_prefix.get().unwrap_or_default();
                spawn_local({
                    async move {
                        let _ = delete_default_config(
                            format!("{prefix}{}", row_key.get_value()),
                            tenant,
                        )
                        .await;
                        default_config_resource.refetch();
                    }
                });
            };

            if is_folder && grouping_enabled {
                view! { <span>{"-"}</span> }.into_view()
            } else {
                view! {
                    <div class="join">
                        <span class="cursor-pointer" on:click=edit_click_handler>
                            <i class="ri-pencil-line ri-xl text-blue-500"></i>
                        </span>
                        <span class="cursor-pointer text-red-500" on:click=handle_delete>
                            <i class="ri-delete-bin-5-line ri-xl text-red-500"></i>
                        </span>
                    </div>
                }
                .into_view()
            }
        };

        let expand = move |_: &str, row: &Map<String, Value>| {
            let key_name = row["key"].html_display();
            let label = key_name.clone();
            let is_folder = key_name.contains(GROUPING_DELIMITER);

            if is_folder && grouping_enabled {
                view! {
                    <span
                        class="cursor-pointer text-blue-500 underline underline-offset-2"
                        on:click=move |_| {
                            let mut key = key_name.clone();
                            if let Some(prefix_) = key_prefix.get() {
                                key = prefix_.clone() + &key;
                            }
                            on_folder_click(Some(key.clone()))
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
            Column::new("actions".to_string(), None, actions_col_formatter),
        ]
    });

    let handle_close = move || {
        selected_config.set(None);
        close_drawer("default_config_drawer");
    };

    view! {
        <div class="p-8">
            <Suspense fallback=move || {
                view! { <Skeleton/> }
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
                                    type_schema=selected_config_data.schema
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
                    let data = default_config_resource.get().unwrap_or(vec![]);
                    let rows = utils::get_rows(data.clone(), key_prefix.get(), enable_grouping.get());
                    let total_items = rows.len().to_string();
                    view! {
                        <div class="pb-4">
                            <Stat heading="Config Keys" icon="ri-tools-line" number=total_items/>
                        </div>
                        <div class="card rounded-lg w-full bg-base-100 shadow">
                            <div class="card-body">
                                <div class="flex justify-between pb-2">
                                    <BreadCrums bread_crums=bread_crums.get() on_folder_click/>
                                    <div class="flex">
                                        <label
                                            on:click=move |_| {
                                                on_folder_click(None);
                                                enable_grouping.set(!enable_grouping.get());
                                                set_local_storage(
                                                    "enable_grouping",
                                                    &enable_grouping.get().to_string(),
                                                );
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
                                    cell_class="min-w-48 font-mono".to_string()
                                    rows=rows
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
pub fn bread_crums(
    bread_crums: Vec<BreadCrums>,
    #[prop(into)] on_folder_click: Callback<Option<String>, ()>,
) -> impl IntoView {
    view! {
        <div class="flex justify-between pt-3">

            {bread_crums
                .into_iter()
                .map(|ele| {
                    let value = ele.value.clone();
                    let item_class = if ele.is_link {
                        "cursor-pointer text-blue-500 underline underline-offset-2"
                    } else {
                        ""
                    };
                    view! {
                        <h2 class="flex after:content-['>'] after:mx-4 after:last:hidden">
                            <span
                                on:click=move |_| {
                                    if ele.is_link {
                                        on_folder_click.call(value.clone())
                                    }
                                }

                                class=item_class
                            >

                                {ele.key.clone()}
                            </span>
                        </h2>
                    }
                })
                .collect_view()}
        </div>
    }
}

mod utils {
    use std::collections::HashSet;

    use serde_json::{Map, Value};

    use crate::{
        types::{BreadCrums, DefaultConfig},
        utils::unwrap_option_or_default_with_error,
    };

    use super::GROUPING_DELIMITER;

    pub fn get_bread_crums(prefix: Option<String>) -> Vec<BreadCrums> {
        let mut bread_crums = vec![BreadCrums {
            key: "Default Config".to_string(),
            value: None,
            is_link: true,
        }];

        if let Some(s) = prefix {
            let mut acc = String::new();
            for frag in s.split_inclusive(GROUPING_DELIMITER) {
                acc.push_str(frag);
                bread_crums.push(BreadCrums {
                    key: frag.trim_matches(GROUPING_DELIMITER).to_string(),
                    value: Some(acc.clone()),
                    is_link: true,
                })
            }
        }

        if let Some(last_crumb) = bread_crums.last_mut() {
            last_crumb.is_link = false;
        }
        bread_crums
    }

    pub fn get_rows(
        data: Vec<DefaultConfig>,
        prefix: Option<String>,
        enable_grouping: bool,
    ) -> Vec<Map<String, Value>> {
        if !enable_grouping {
            return data.iter().map(|v| v.into_row()).collect();
        }

        let mut groups: HashSet<&str> = HashSet::new();
        let mut rows: Vec<Map<String, Value>> = vec![];
        for config_key in data.iter() {
            let suffix = prefix.as_ref().map_or(Some(config_key.key.as_str()), |s| {
                config_key.key.split(s).nth(1)
            });

            if let Some(suff) = suffix {
                let splits = suff
                    .split_inclusive(GROUPING_DELIMITER)
                    .collect::<Vec<&str>>();
                let is_leaf = splits.len() == 1;

                let grp_name = splits.first().expect(
                    format!(
                        "invalid key name, cannot end with characters {:?}",
                        GROUPING_DELIMITER
                    )
                    .as_str(),
                );

                if !is_leaf && groups.contains(grp_name) {
                    continue;
                }
                groups.insert(grp_name);

                let mut row_map = config_key.into_row();
                if is_leaf {
                    row_map
                        .insert("key".to_string(), Value::String(grp_name.to_string()));
                } else {
                    DefaultConfig::table_cols().iter().for_each(|c| {
                        if *c == "key" {
                            row_map.insert(
                                c.to_string(),
                                Value::String(grp_name.to_string()),
                            );
                        } else {
                            row_map.insert(c.to_string(), Value::String("-".to_string()));
                        }
                    })
                }

                rows.push(row_map);
            }
        }

        rows.sort_by(|a, b| {
            let key_a = unwrap_option_or_default_with_error(
                a.get("key").and_then(Value::as_str),
                "",
            );
            let key_b = unwrap_option_or_default_with_error(
                b.get("key").and_then(Value::as_str),
                "",
            );

            match (
                key_a.contains(GROUPING_DELIMITER),
                key_b.contains(GROUPING_DELIMITER),
            ) {
                (true, false) => std::cmp::Ordering::Less,
                (false, true) => std::cmp::Ordering::Greater,
                _ => std::cmp::Ordering::Equal,
            }
        });

        rows
    }
}
