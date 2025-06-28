mod types;

use std::collections::HashSet;

use leptos::*;
use leptos_router::A;
use serde_json::{json, Map, Value};
use superposition_macros::box_params;
use superposition_types::{
    api::default_config::DefaultConfigFilters,
    custom_query::{CustomQuery, PaginationParams, Query},
};
use types::PageParams;

use crate::api::{delete_default_config, fetch_default_config};
use crate::components::{
    alert::AlertType,
    button::Button,
    default_config_form::DefaultConfigForm,
    delete_modal::DeleteModal,
    description_icon::InfoDescription,
    drawer::{close_drawer, open_drawer, Drawer, DrawerBtn, DrawerButtonStyle},
    skeleton::Skeleton,
    stat::Stat,
    table::{
        types::{
            default_column_formatter, Column, ColumnSortable, Expandable,
            TablePaginationProps,
        },
        Table,
    },
};
use crate::providers::alert_provider::enqueue_alert;
use crate::query_updater::{use_param_updater, use_signal_from_query};
use crate::types::{BreadCrums, OrganisationId, Tenant};
use crate::utils::{unwrap_option_or_default_with_error, use_url_base};

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub key: String,
    pub value: Value,
    pub schema: Value,
    pub description: String,
    pub validation_function_name: Option<String>,
    pub autocomplete_function_name: Option<String>,
}

#[derive(Clone)]
pub enum DrawerType {
    Create,
    Edit(RowData),
    None,
}

#[component]
pub fn default_config() -> impl IntoView {
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let (delete_modal_visible_rs, delete_modal_visible_ws) = create_signal(false);
    let (delete_key_rs, delete_key_ws) = create_signal::<Option<String>>(None);
    let filters_rws = use_signal_from_query(move |query_string| {
        Query::<DefaultConfigFilters>::extract_non_empty(&query_string).into_inner()
    });
    let page_params_rws = use_signal_from_query(move |query_string| {
        Query::<PageParams>::extract_non_empty(&query_string).into_inner()
    });
    let pagination_params_rws = use_signal_from_query(move |query_string| {
        if page_params_rws.get_untracked().grouped {
            PaginationParams::all_entries()
        } else {
            Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
        }
    });
    let drawer_type = RwSignal::new(DrawerType::None);
    let bread_crums =
        Signal::derive(move || get_bread_crums(page_params_rws.get().prefix));

    use_param_updater(move || {
        box_params!(
            pagination_params_rws.get(),
            page_params_rws.get(),
            filters_rws.get()
        )
    });

    let default_config_resource = create_blocking_resource(
        move || {
            (
                tenant_rws.get().0,
                pagination_params_rws.get(),
                org_rws.get().0,
                filters_rws.get(),
            )
        },
        |(current_tenant, pagination_params, org_id, filters)| async move {
            fetch_default_config(&pagination_params, &filters, current_tenant, org_id)
                .await
                .unwrap_or_default()
        },
    );

    let confirm_delete = Callback::new(move |_| {
        let tenant = tenant_rws.get().0;
        let org = org_rws.get().0;
        let prefix = page_params_rws.with(|p| p.prefix.clone().unwrap_or_default());
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

    let handle_next_click = Callback::new(move |next_page: i64| {
        pagination_params_rws.update(|f| f.page = Some(next_page));
    });

    let handle_prev_click = Callback::new(move |prev_page: i64| {
        pagination_params_rws.update(|f| f.page = Some(prev_page));
    });

    let redirect_url = move |prefix: Option<String>| -> String {
        let base = use_url_base();
        let tenant = tenant_rws.get_untracked().0;
        let org_id = org_rws.get_untracked().0;
        let mut redirect_url = format!("{base}/admin/{org_id}/{tenant}/default-config");
        if let Some(prefix) = prefix {
            redirect_url = format!("{redirect_url}?prefix={prefix}");
        }
        redirect_url
    };

    let table_columns = create_memo(move |_| {
        let grouped = page_params_rws.with(|p| p.grouped);
        let actions_col_formatter = move |_: &str, row: &Map<String, Value>| {
            let row_key = row["key"].to_string().replace('"', "");
            let is_folder = row_key.contains('.');
            let row_value = row["value"].clone();

            let schema = row["schema"].clone().to_string();
            let schema_object =
                serde_json::from_str::<Value>(&schema).unwrap_or(Value::Null);

            // keeping the function_name field the same for backwards compatibility
            let validation_function_name = row
                .get("function_name")
                .and_then(|v| v.as_str().map(String::from));
            let autocomplete_function_name = row
                .get("autocomplete_function_name")
                .and_then(|v| v.as_str().map(String::from));

            let description = row
                .get("description")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string();

            let key_name = StoredValue::new(row_key.clone());

            let edit_click_handler = move |_| {
                let row_data = RowData {
                    key: row_key.clone(),
                    value: row_value.clone(),
                    schema: schema_object.clone(),
                    description: description.clone(),
                    validation_function_name: validation_function_name.clone(),
                    autocomplete_function_name: autocomplete_function_name.clone(),
                };
                logging::log!("{:?}", row_data);
                drawer_type.set(DrawerType::Edit(row_data));
                open_drawer("default_config_drawer");
            };

            let handle_delete = move |_| {
                delete_key_ws.set(Some(key_name.get_value()));
                delete_modal_visible_ws.set(true);
            };

            if is_folder && grouped {
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

        let expand = move |key_name: &str, row: &Map<String, Value>| {
            let key_name = key_name.to_string();
            let label = key_name.clone();
            let is_folder = key_name.contains('.');
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

            if is_folder && grouped {
                let prefix = page_params_rws.with(|p| {
                    p.prefix
                        .as_ref()
                        .map_or_else(|| key_name.clone(), |p| format!("{p}{key_name}"))
                });

                view! {
                    <A
                        class="cursor-pointer text-blue-500 underline underline-offset-2"
                        href=redirect_url(Some(prefix))
                    >
                        {label}
                    </A>
                }
                .into_view()
            } else {
                view! {
                    <span class="mr-2">{key_name}</span>
                    <InfoDescription description=description change_reason=change_reason />
                }
                .into_view()
            }
        };

        vec![
            Column::new(
                "key".to_string(),
                false,
                expand,
                ColumnSortable::No,
                Expandable::Disabled,
                default_column_formatter,
            ),
            Column::default("schema".to_string()),
            Column::default("value".to_string()),
            Column::default("function_name".to_string()),
            Column::default("autocomplete_function_name".to_string()),
            Column::default("created_at".to_string()),
            Column::default("created_by".to_string()),
            Column::new(
                "actions".to_string(),
                false,
                actions_col_formatter,
                ColumnSortable::No,
                Expandable::Disabled,
                default_column_formatter,
            ),
        ]
    });

    let handle_close = move || {
        drawer_type.set(DrawerType::None);
        close_drawer("default_config_drawer");
    };

    view! {
        <div class="p-8">
            <Suspense fallback=move || {
                view! { <Skeleton /> }
            }>
                {move || {
                    let prefix = page_params_rws.with(|p| p.prefix.clone());
                    match drawer_type.get() {
                        DrawerType::Edit(selected_config_data) => {
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
                                        description=selected_config_data.description
                                        validation_function_name=selected_config_data
                                            .validation_function_name
                                        autocomplete_function_name=selected_config_data
                                            .autocomplete_function_name
                                        prefix
                                        handle_submit=move || {
                                            default_config_resource.refetch();
                                            handle_close();
                                        }
                                    />

                                </Drawer>
                            }
                        }
                        DrawerType::Create => {
                            view! {
                                <Drawer
                                    id="default_config_drawer".to_string()
                                    header="Create New Key"
                                    handle_close=handle_close
                                >
                                    <DefaultConfigForm
                                        prefix
                                        handle_submit=move || {
                                            filters_rws.set(DefaultConfigFilters::default());
                                            if !page_params_rws.with(|p| p.grouped) {
                                                pagination_params_rws.set(PaginationParams::default());
                                            }
                                            default_config_resource.refetch();
                                            handle_close();
                                        }
                                    />

                                </Drawer>
                            }
                        }
                        DrawerType::None => ().into_view(),
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
                                    json!(config.created_at.format("%v %T").to_string()),
                                );
                            ele_map
                        })
                        .collect::<Vec<Map<String, Value>>>();
                    let mut filtered_rows = table_rows;
                    let page_params = page_params_rws.get();
                    if page_params.grouped {
                        let cols = filtered_rows
                            .first()
                            .map(|row| row.keys().cloned().collect())
                            .unwrap_or_default();
                        filtered_rows = modify_rows(
                            filtered_rows.clone(),
                            page_params.prefix,
                            cols,
                        );
                    }
                    let total_default_config_keys = default_config.total_items.to_string();
                    let pagination_params = pagination_params_rws.get();
                    let (current_page, total_pages) = if page_params.grouped {
                        (1, 1)
                    } else {
                        (pagination_params.page.unwrap_or_default(), default_config.total_pages)
                    };
                    let pagination_props = TablePaginationProps {
                        enabled: true,
                        count: pagination_params.count.unwrap_or_default(),
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
                                    <BreadCrums bread_crums=bread_crums.get() redirect_url />
                                    <div class="flex">
                                        <label
                                            on:click=move |_| {
                                                page_params_rws
                                                    .update(|params| {
                                                        params.grouped = !params.grouped;
                                                        params.prefix = None;
                                                    });
                                                let grouped = page_params_rws.with(|p| p.grouped);
                                                if !grouped {
                                                    pagination_params_rws.set(PaginationParams::default());
                                                }
                                            }
                                            class="cursor-pointer label mr-10"
                                        >
                                            <span class="label-text mr-4">Group Configs</span>
                                            <input
                                                type="checkbox"
                                                class="toggle toggle-primary"
                                                checked=page_params_rws.with(|p| p.grouped)
                                            />
                                        </label>
                                        <DrawerBtn
                                            drawer_id="default_config_drawer".to_string()
                                            on_click=Callback::new(move |_| {
                                                drawer_type.set(DrawerType::Create);
                                                open_drawer("default_config_drawer");
                                            })
                                        >
                                            Create Key
                                            <i class="ri-edit-2-line ml-2"></i>
                                        </DrawerBtn>
                                    </div>
                                </div>
                                <DefaultConfigFilterWidget
                                    filters_rws
                                    pagination_params_rws
                                    prefix=page_params_rws.with(|p| p.prefix.clone())
                                />
                                <Table
                                    rows=filtered_rows
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
                    header_text="Are you sure you want to delete this config? Action is irreversible."
                        .to_string()
                />
            </Suspense>
        </div>
    }
}

#[component]
pub fn bread_crums(
    bread_crums: Vec<BreadCrums>,
    #[prop(into)] redirect_url: Callback<Option<String>, String>,
) -> impl IntoView {
    view! {
        <div class="flex justify-between pt-3">
            {bread_crums
                .iter()
                .map(|ele| {
                    view! {
                        <h2 class="flex after:content-['>'] after:mx-4 after:last:hidden">
                            {if ele.is_link {
                                let href = redirect_url.call(ele.value.clone());
                                let label = ele.key.clone();
                                view! {
                                    <A
                                        class="flex after:content-['>'] after:mx-4 after:last:hidden placeholder:cursor-pointer text-blue-500 underline underline-offset-2"
                                        href
                                    >
                                        {label}
                                    </A>
                                }
                                    .into_view()
                            } else {
                                view! { <span>{ele.key.clone()}</span> }.into_view()
                            }}
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
    pagination_params_rws: RwSignal<PaginationParams>,
    filters_rws: RwSignal<DefaultConfigFilters>,
    #[prop(into)] prefix: Option<String>,
) -> impl IntoView {
    let filters = filters_rws.get_untracked();
    let filters_buffer_rws = create_rw_signal(filters.clone());
    view! {
        <DrawerBtn drawer_id="default_config_filter_drawer".into() style=DrawerButtonStyle::Outline>
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
                        value=if prefix.is_some()
                            && filters_buffer_rws.get_untracked().name.is_none()
                        {
                            prefix
                        } else {
                            filters_buffer_rws.get_untracked().name
                        }
                        on:change=move |event| {
                            let key_name = event_target_value(&event);
                            let key_name = if key_name.is_empty() { None } else { Some(key_name) };
                            let filters = DefaultConfigFilters {
                                name: key_name,
                            };
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
                            pagination_params_rws.update(|f| f.reset_page());
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
                            pagination_params_rws.update(|f| f.reset_page());
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
    cols: Vec<String>,
) -> Vec<Map<String, Value>> {
    let mut groups: HashSet<String> = HashSet::new();
    let mut grouped_rows: Vec<Map<String, Value>> = filtered_rows
        .into_iter()
        .filter_map(|mut ele| {
            let key = ele
                .get("key")
                .and_then(|v| v.as_str())
                .map(String::from)
                .unwrap_or_default();

            let key_arr = match &key_prefix {
                Some(prefix) => key
                    .splitn(2, prefix)
                    .map(String::from)
                    .collect::<Vec<String>>(),
                None => vec!["".to_string(), key],
            };
            // key_arr.get(1) retrieves the remaining part of the key, after removing the prefix.
            if let Some(filtered_key) = key_arr.get(1) {
                let new_key = filtered_key
                    .split('.')
                    .map(String::from)
                    .collect::<Vec<String>>();
                let key = new_key.first().map(String::from).unwrap_or_default();
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
                                Value::String(if *col == "key" {
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
