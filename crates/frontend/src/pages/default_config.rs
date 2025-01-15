use std::collections::HashSet;

use crate::api::{delete_default_config, fetch_default_config};
use crate::components::button::Button;
use crate::components::skeleton::Skeleton;
use crate::components::stat::Stat;
use crate::components::table::types::ColumnSortable;
use crate::components::table::{
    types::{Column, TablePaginationProps},
    Table,
};
use crate::types::{BreadCrums, OrganisationId, Tenant};
use crate::utils::{
    get_local_storage, set_local_storage, unwrap_option_or_default_with_error,
    update_page_direction,
};

use leptos::*;
use leptos_router::{use_navigate, use_query_map, A};
use serde_json::{json, Map, Value};
use superposition_types::custom_query::PaginationParams;

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub key: String,
    pub value: Value,
    pub schema: Value,
    pub function_name: Option<Value>,
}

#[component]
pub fn default_config() -> impl IntoView {
    let tenant_s = use_context::<Signal<Tenant>>().unwrap();
    let org_s = use_context::<Signal<OrganisationId>>().unwrap();
    let enable_grouping = create_rw_signal(false);
    let (filters, set_filters) = create_signal(PaginationParams::default());
    let default_config_resource = create_blocking_resource(
        move || (tenant_s.get().0, filters.get(), org_s.get().0),
        |(tenant, filters, org_id)| async move {
            fetch_default_config(&filters, &tenant, &org_id)
                .await
                .unwrap_or_default()
        },
    );

    let key_prefix = create_rw_signal::<Option<String>>(None);
    let query_params = use_query_map();
    let bread_crums = Signal::derive(move || get_bread_crums(key_prefix.get()));

    let set_filters_none = move || set_filters.set(PaginationParams::all_entries());

    let set_filters_default = move || set_filters.set(PaginationParams::default());

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
        let tenant = tenant_s.get().0;
        let redirect_url = match key_name {
            Some(prefix) => format!("admin/{tenant}/default-config?prefix={prefix}"),
            None => format!("admin/{tenant}/default-config"),
        };
        logging::log!("redirecting to {:?}", redirect_url.clone());
        let navigate = use_navigate();
        navigate(redirect_url.as_str(), Default::default());
    };

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
        let grouping_enabled = enable_grouping.get();
        let actions_col_formatter = move |_: &str, row: &Map<String, Value>| {
            let row_key = row["key"].to_string().replace('"', "");
            let is_folder = row_key.contains('.');

            let key_name = StoredValue::new(row_key.clone());

            let handle_delete = move |_| {
                let tenant = tenant_s.get().0;
                let org = org_s.get().0;
                let prefix = key_prefix.get().unwrap_or_default();
                spawn_local({
                    async move {
                        let _ = delete_default_config(
                            format!("{prefix}{}", key_name.get_value()),
                            tenant,
                            org,
                        )
                        .await;
                        default_config_resource.refetch();
                    }
                });
            };
            let prefix = key_prefix.get().unwrap_or_default();

            if is_folder && grouping_enabled {
                view! { <span>{"-"}</span> }.into_view()
            } else {
                view! {
                    <div class="join">
                        <A href=format!("{row_key}/update?prefix={prefix}")>
                            <i class="ri-pencil-line ri-xl text-blue-500"></i>
                        </A>
                        <span class="cursor-pointer text-red-500" on:click=handle_delete>
                            <i class="ri-delete-bin-5-line ri-xl text-red-500"></i>
                        </span>
                    </div>
                }
                .into_view()
            }
        };

        let expand = move |_: &str, row: &Map<String, Value>| {
            let key_name = row["key"].to_string().replace('"', "");
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
            Column::new("key".to_string(), None, expand, ColumnSortable::No),
            Column::default("schema".to_string()),
            Column::default("value".to_string()),
            Column::default("function_name".to_string()),
            Column::default("created_at".to_string()),
            Column::default("created_by".to_string()),
            Column::new(
                "actions".to_string(),
                None,
                actions_col_formatter,
                ColumnSortable::No,
            ),
        ]
    });

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>

            {move || {
                let prefix = query_params.get().get("prefix").cloned().unwrap_or_default();
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
                let filters = filters.get();
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
                                <BreadCrums bread_crums=bread_crums.get() folder_click_handler />
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

                                    <A href=format!("new?prefix={}", prefix)>
                                        <Button text="Create Key" on_click=move |_| {} />
                                    </A>
                                </div>
                            </div>
                            <Table
                                cell_class="min-w-48 font-mono".to_string()
                                rows=filtered_rows
                                key_column="id".to_string()
                                columns=table_columns.get()
                                pagination=pagination_props
                            />
                        </div>
                    </div>
                }
            }}
        </Suspense>
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
