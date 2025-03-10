use std::collections::HashSet;

use leptos::*;
use leptos_router::{use_navigate, use_query_map};
use serde_json::{json, Map, Value};
use superposition_types::{
    api::default_config::DefaultConfigFilters, custom_query::PaginationParams,
};

use crate::api::{delete_default_config, fetch_default_config};
use crate::components::table::types::Expandable;
use crate::components::{ 
    info_modal::{ConfigInfo, InfoModal},
    alert::AlertType,
    button::Button,
    default_config_form::DefaultConfigForm,
    delete_modal::DeleteModal,
    drawer::{close_drawer, open_drawer, Drawer, DrawerBtn},
    skeleton::Skeleton,
    stat::Stat,
    table::{
        types::{Column, ColumnSortable, TablePaginationProps},
        Table,
    },
};
use crate::providers::alert_provider::enqueue_alert;
use crate::types::{BreadCrums, OrganisationId, Tenant};
use crate::utils::{
    get_local_storage, set_local_storage, unwrap_option_or_default_with_error,
    update_page_direction,
};

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub key: String,
    pub value: Value,
    pub schema: Value,
    pub function_name: Option<Value>,
    pub description: String, 
    pub change_reason: String,
}

#[component]
pub fn default_config() -> impl IntoView {
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let enable_grouping = create_rw_signal(false);
    let (pagination_rs, pagination_ws) = create_signal(PaginationParams::default());
    let filters_rws = create_rw_signal(DefaultConfigFilters::default());
    let (delete_modal_visible_rs, delete_modal_visible_ws) = create_signal(false);
    let (delete_key_rs, delete_key_ws) = create_signal::<Option<String>>(None);
    let (info_modal_visible_rs, info_modal_visible_ws) = create_signal(false);
    let (selected_info, set_selected_info) = create_signal::<Option<ConfigInfo>>(None);


    let default_config_resource = create_blocking_resource(
        move || {
            (
                tenant_rws.get().0,
                pagination_rs.get(),
                org_rws.get().0,
                filters_rws.get(),
            )
        },
        |(current_tenant, pagination, org_id, filters)| async move {
            fetch_default_config(&pagination, &filters, current_tenant, org_id)
                .await
                .unwrap_or_default()
        },
    );

    let selected_config = create_rw_signal::<Option<RowData>>(None);
    let key_prefix = create_rw_signal::<Option<String>>(None);
    let query_params = use_query_map();
    let bread_crums = Signal::derive(move || get_bread_crums(key_prefix.get()));

    let set_filters_none = move || pagination_ws.set(PaginationParams::all_entries());

    let set_filters_default = move || pagination_ws.set(PaginationParams::default());

    let confirm_delete = Callback::new(move |_| {
        let tenant = tenant_rws.get().0;
        let org = org_rws.get().0;
        let prefix = key_prefix.get().unwrap_or_default();
        if let Some(key_name) = delete_key_rs.get() {
            spawn_local({
                async move {
                    let api_response = delete_default_config(
                        format!("{prefix}{}", key_name),
                        tenant,
                        org,
                    )
                    .await;
                    match api_response {
                        Ok(_) => {
                            enqueue_alert(
                                format!("Config {key_name} deleted successfully"),
                                AlertType::Success,
                                5000,
                            );
                            default_config_resource.refetch();
                        }
                        Err(err) => enqueue_alert(err, AlertType::Error, 5000),
                    }
                }
            });
        }
        delete_key_ws.set(None);
        delete_modal_visible_ws.set(false);
    });

    create_effect(move |_| {
        let enable_grouping_val =
            get_local_storage::<bool>("enable_grouping").unwrap_or(false);
        enable_grouping.set(enable_grouping_val);
        if enable_grouping_val {
            set_filters_none();
        } else {
            set_filters_default();
        }
    });
    create_effect(move |_| {
        let query_params_map = query_params.try_get();
        if let Some(query_map) = query_params_map {
            let opt_prefix = query_map.get("prefix");
            key_prefix.set(opt_prefix.cloned());
            if opt_prefix.is_some() {
                enable_grouping.set(true);
                set_filters_none();
            }
        }
    });

    let folder_click_handler = move |key_name: Option<String>| {
        let tenant = tenant_rws.get().0;
        let org_id = org_rws.get().0;
        let mut redirect_url = format!("admin/{org_id}/{tenant}/default-config");
        if let Some(prefix) = key_name {
            redirect_url = format!("{redirect_url}?prefix={prefix}")
        }
        logging::log!("redirecting to {:?}", redirect_url.clone());
        let navigate = use_navigate();
        navigate(redirect_url.as_str(), Default::default());
    };

    let handle_next_click = Callback::new(move |total_pages: i64| {
        pagination_ws.update(|f| {
            f.page = update_page_direction(f.page, total_pages, true);
        });
    });

    let handle_prev_click = Callback::new(move |_| {
        pagination_ws.update(|f| {
            f.page = update_page_direction(f.page, 1, false);
        });
    });

    let table_columns = create_memo(move |_| {
        let grouping_enabled = enable_grouping.get();
        let actions_col_formatter = move |_: &str, row: &Map<String, Value>| {
            let row_key = row["key"].to_string().replace('"', "");
            let is_folder = row_key.contains('.');
            let row_value = row["value"].clone();

            let schema = row["schema"].clone().to_string();
            let schema_object =
                serde_json::from_str::<Value>(&schema).unwrap_or(Value::Null);

            let function_name = row["function_name"].to_string();
            let fun_name = match function_name.as_str() {
                "null" => None,
                _ => Some(json!(function_name.replace('"', ""))),
            };

            let description = row.get("description")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        
            let change_reason = row.get("change_reason")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string();

            let key_name = StoredValue::new(row_key.clone());
            let stored_description = StoredValue::new(description.clone());
            let stored_change_reason = StoredValue::new(change_reason.clone());

            let edit_click_handler = move |_| {
                let row_data = RowData {
                    key: row_key.clone(),
                    value: row_value.clone(),
                    schema: schema_object.clone(),
                    function_name: fun_name.clone(),
                    description: stored_description.get_value(),
                    change_reason: stored_change_reason.get_value(),
                };
                logging::log!("{:?}", row_data);
                selected_config.set(Some(row_data));
                open_drawer("default_config_drawer");
            };

            let handle_delete = move |_| {
                delete_key_ws.set(Some(key_name.get_value()));
                delete_modal_visible_ws.set(true);
            };
            let show_info_modal = move |_: web_sys::MouseEvent| {
                set_selected_info.set(Some(ConfigInfo {
                    description: stored_description.get_value(),
                    change_reason: stored_change_reason.get_value(),
                }));
                info_modal_visible_ws.set(true);
            };
        

            if is_folder && grouping_enabled {
                view! { <span>{"-"}</span> }.into_view()
            } else {
                view! {
                    <div class="join">
                       <span class="cursor-pointer" on:click=show_info_modal>
                        <i class="ri-information-line ri-xl text-gray-500 hover:text-gray-700"></i>
                       </span>
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

        let expand = move |key_name: &str, _row: &Map<String, Value>| {
            let key_name = key_name.to_string();
            let label = key_name.clone();
            let is_folder = key_name.contains('.');

            if is_folder && grouping_enabled {
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
            Column::new(
                "key".to_string(),
                false,
                expand,
                ColumnSortable::No,
                Expandable::Disabled,
            ),
            Column::default("schema".to_string()),
            Column::default("value".to_string()),
            Column::default("function_name".to_string()),
            Column::default("created_at".to_string()),
            Column::default("created_by".to_string()),
            Column::new(
                "actions".to_string(),
                false,
                actions_col_formatter,
                ColumnSortable::No,
                Expandable::Disabled,
            ),
        ]
    });

    let handle_close = move || {
        selected_config.set(None);
        close_drawer("default_config_drawer");
    };

    view! {
        <div class="p-8">
            <Suspense fallback=move || {
                view! { <Skeleton /> }
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
                    let default_config = default_config_resource.get().unwrap_or_default();
                    let table_rows = default_config
                        .data
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
                            .first()
                            .unwrap_or(&empty_map)
                            .keys()
                            .map(|key| key.as_str())
                            .collect();
                        filtered_rows = modify_rows(filtered_rows.clone(), key_prefix.get(), cols);
                    }
                    let total_default_config_keys = default_config.total_items.to_string();
                    let filters = pagination_rs.get();
                    let (current_page, total_pages) = if enable_grouping.get() {
                        (1, 1)
                    } else {
                        (filters.page.unwrap_or_default(), default_config.total_pages)
                    };
                    let pagination_props = TablePaginationProps {
                        enabled: true,
                        count: filters.count.unwrap_or_default(),
                        current_page,
                        total_pages,
                        on_next: handle_next_click,
                        on_prev: handle_prev_click,
                    };
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
                                    <BreadCrums
                                        bread_crums=bread_crums.get()
                                        folder_click_handler
                                    />
                                    <div class="flex">
                                        <label
                                            on:click=move |_| {
                                                folder_click_handler(None);
                                                enable_grouping.set(!enable_grouping.get());
                                                set_local_storage(
                                                    "enable_grouping",
                                                    &enable_grouping.get().to_string(),
                                                );
                                                if enable_grouping.get() {
                                                    set_filters_none();
                                                } else {
                                                    set_filters_default();
                                                }
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
                                <DefaultConfigFilterWidget
                                    filters_rws
                                    key_prefix
                                />
                                <Table
                                    rows=filtered_rows
                                    key_column="id".to_string()
                                    columns=table_columns.get()
                                    pagination=pagination_props
                                />
                            </div>
                        </div>
                        <InfoModal
                            modal_visible=info_modal_visible_rs
                            description=selected_info.get().map_or("".to_string(), |info| info.description)
                            change_reason=selected_info.get().map_or("".to_string(), |info| info.change_reason)
                            set_modal_visible=info_modal_visible_ws
                        />
                        <DeleteModal
                            modal_visible=delete_modal_visible_rs
                            confirm_delete=confirm_delete
                            set_modal_visible=delete_modal_visible_ws
                            header_text="Are you sure you want to delete this config? Action is irreversible."
                                .to_string()
                        />
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
    view! {
        <div class="flex justify-between pt-3">

            {bread_crums
                .iter()
                .enumerate()
                .map(|(_, ele)| {
                    let value = ele.value.clone();
                    let is_link = ele.is_link;
                    let handler = folder_click_handler.clone();
                    view! {
                        <h2 class="flex after:content-['>'] after:mx-4 after:last:hidden">
                            <span
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
                            </span>
                        </h2>
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
                .split('.')
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

#[component]
fn default_config_filter_widget(
    filters_rws: RwSignal<DefaultConfigFilters>,
    key_prefix: RwSignal<Option<String>>,
) -> impl IntoView {
    let filters = filters_rws.get_untracked();
    let filters_buffer_rws = create_rw_signal(filters.clone());
    view! {
        <DrawerBtn
            drawer_id="default_config_filter_drawer".into()
            style="cursor-pointer btn btn-purple-outline m-1 w-[8rem]".to_string()
        >
            Filters
            <i class="ri-filter-3-line"></i>
        </DrawerBtn>
        <Drawer
            id="default_config_filter_drawer".to_string()
            header="Default Config Filters"
            drawer_width="w-[50vw]"
            handle_close=move || {
                close_drawer("default_config_filter_drawer");
            }
        >
            <div class="card-body">
                <div class="form-control flex flex-col gap-9 justify-between">
                    <label class="label">
                        <span class="label-text">Configuration Name</span>
                    </label>
                    <input
                        type="text"
                        id="default-config-name-filter"
                        placeholder="eg: city"
                        class="input input-bordered rounded-md resize-y w-full max-w-md"
                        value=move || {
                            let prefix = key_prefix.get();
                            if prefix.is_some() && filters_buffer_rws.get().name.is_none() {
                                prefix
                            } else {
                                filters_buffer_rws.get().name
                            }
                        }
                        on:change=move |event| {
                            let key_name = event_target_value(&event);
                            let key_name = if key_name.is_empty() {
                                None
                            } else {
                                Some(key_name)
                            };
                            let filters = DefaultConfigFilters { name: key_name };
                            filters_buffer_rws.set(filters);
                        }
                    />
                </div>
                <div class="flex justify-start">
                    <Button
                        class="h-12 w-48 px-[70px]".to_string()
                        text="Submit".to_string()
                        on_click=move |event| {
                            event.prevent_default();
                            let filter = filters_buffer_rws.get();
                            filters_rws.set(filter);
                            close_drawer("default_config_filter_drawer")
                        }
                    />
                    <Button
                        class="h-12 w-48 px-[70px]".to_string()
                        text="Reset".to_string()
                        on_click=move |event| {
                            event.prevent_default();
                            let filters = DefaultConfigFilters::default();
                            filters_rws.set(filters);
                            close_drawer("default_config_filter_drawer")
                        }
                    />

                </div>
            </div>
        </Drawer>
    }
}

pub fn modify_rows(
    filtered_rows: Vec<Map<String, Value>>,
    key_prefix: Option<String>,
    cols: Vec<&str>,
) -> Vec<Map<String, Value>> {
    let mut groups: HashSet<String> = HashSet::new();
    let mut grouped_rows: Vec<Map<String, Value>> = filtered_rows
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
                    .split('.')
                    .map(str::to_string)
                    .collect::<Vec<String>>();
                let key = new_key.first().unwrap().to_owned().replace('"', "");
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
        .collect();
    grouped_rows.sort_by(|a, b| {
        let key_a =
            unwrap_option_or_default_with_error(a.get("key").and_then(Value::as_str), "");
        let key_b =
            unwrap_option_or_default_with_error(b.get("key").and_then(Value::as_str), "");

        match (key_a.contains('.'), key_b.contains('.')) {
            (true, false) => std::cmp::Ordering::Less,
            (false, true) => std::cmp::Ordering::Greater,
            _ => std::cmp::Ordering::Equal,
        }
    });
    grouped_rows
}
