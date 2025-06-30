mod filter;
mod types;
pub mod utils;

use filter::{DefaultConfigFilterWidget, FilterSummary};
use leptos::*;
use leptos_router::A;
use serde_json::{json, Map, Value};
use superposition_macros::box_params;
use superposition_types::{
    api::default_config::DefaultConfigFilters,
    custom_query::{CustomQuery, PaginationParams, Query},
};
use types::PageParams;
use utils::{get_bread_crums, modify_rows, BreadCrums};

use crate::api::{delete_default_config, fetch_default_config};
use crate::components::{
    alert::AlertType,
    default_config_form::{ChangeLogSummary, ChangeType, DefaultConfigForm},
    description_icon::InfoDescription,
    drawer::{close_drawer, open_drawer, Drawer, DrawerBtn},
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
use crate::types::{OrganisationId, Tenant};
use crate::utils::use_url_base;

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
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
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
    let bread_crums = Signal::derive(move || {
        get_bread_crums(
            page_params_rws.with(|p| p.prefix.clone()),
            "Default Config".to_string(),
        )
    });

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
                workspace.get().0,
                pagination_params_rws.get(),
                org.get().0,
                filters_rws.get(),
            )
        },
        |(current_tenant, pagination_params, org_id, filters)| async move {
            fetch_default_config(&pagination_params, &filters, current_tenant, org_id)
                .await
                .unwrap_or_default()
        },
    );

    let confirm_delete = move |_| {
        let tenant = workspace.get_untracked().0;
        let org = org.get_untracked().0;
        let prefix = page_params_rws.with(|p| p.prefix.clone().unwrap_or_default());
        if let Some(key_name) = delete_key_rs.get_untracked() {
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
                            delete_key_ws.set(None);
                            default_config_resource.refetch();
                        }
                        Err(err) => enqueue_alert(err, AlertType::Error, 5000),
                    }
                }
            });
        }
    };

    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    });

    let redirect_url = move |prefix: Option<String>| -> String {
        let base = use_url_base();
        let tenant = workspace.get_untracked().0;
        let org_id = org.get_untracked().0;
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

            let handle_delete = move |_| delete_key_ws.set(Some(key_name.get_value()));

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
            let label = key_name.to_string();
            let is_folder = key_name.ends_with('.');
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

            if is_folder {
                let prefix = page_params_rws.with(|p| {
                    p.prefix
                        .as_ref()
                        .map_or_else(|| label.clone(), |p| format!("{p}{label}"))
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
                    <span class="mr-2">{label}</span>
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
                                        handle_submit=move |_| {
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
                                        handle_submit=move |_| {
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
                            "key",
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
                        on_page_change: handle_page_change,
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
                                <div class="flex justify-between">
                                    <div class="flex items-center gap-4">
                                        <BreadCrums bread_crums=bread_crums.get() redirect_url />
                                        <DefaultConfigFilterWidget
                                            filters_rws
                                            pagination_params_rws
                                            prefix=page_params_rws.with(|p| p.prefix.clone())
                                        />
                                    </div>
                                    <div class="flex gap-10">
                                        <label
                                            on:click=move |_| {
                                                batch(|| {
                                                    page_params_rws
                                                        .update(|params| {
                                                            params.grouped = !params.grouped;
                                                            params.prefix = None;
                                                        });
                                                    let grouped = page_params_rws.with(|p| p.grouped);
                                                    if !grouped {
                                                        pagination_params_rws.set(PaginationParams::default());
                                                    }
                                                });
                                            }
                                            class="label gap-4 cursor-pointer"
                                        >
                                            <span class="label-text min-w-max">Group Configs</span>
                                            <input
                                                type="checkbox"
                                                class="toggle toggle-primary"
                                                checked=page_params_rws.with(|p| p.grouped)
                                            />
                                        </label>
                                        <DrawerBtn
                                            drawer_id="default_config_drawer"
                                            on_click=move |_| {
                                                drawer_type.set(DrawerType::Create);
                                                open_drawer("default_config_drawer");
                                            }
                                        >
                                            Create Key
                                            <i class="ri-edit-2-line ml-2"></i>
                                        </DrawerBtn>
                                    </div>
                                </div>
                                <FilterSummary filters_rws />
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
            </Suspense>
            {move || {
                if let Some(delete_key) = delete_key_rs.get() {
                    view! {
                        <ChangeLogSummary
                            key_name=delete_key
                            change_type=ChangeType::Delete
                            on_close=move |_| delete_key_ws.set(None)
                            on_confirm=confirm_delete
                        />
                    }
                } else {
                    ().into_view()
                }
            }}
        </div>
    }
}
