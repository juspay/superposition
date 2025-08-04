mod types;

use std::collections::{hash_map::Entry, HashMap};

use leptos::*;
use leptos_router::A;
use serde_json::{json, Map, Value};
use superposition_macros::box_params;
use superposition_types::{
    api::workspace::WorkspaceResponse,
    custom_query::{CustomQuery, PaginationParams, Query},
};
use types::{ComparisonTable, ContextList, PageParams};

use crate::{
    api::{fetch_dimensions, resolve_config},
    components::{
        alert::AlertType,
        button::Button,
        condition_pills::Condition as ConditionComponent,
        context_form::ContextForm,
        skeleton::{Skeleton, SkeletonVariant},
        table::{
            types::{Column, ColumnSortable, Expandable},
            Table,
        },
    },
    logic::Conditions,
    pages::default_config_list::utils::{get_bread_crums, modify_rows, BreadCrums},
    providers::{
        alert_provider::enqueue_alert,
        condition_collapse_provider::ConditionCollapseProvider,
    },
    query_updater::{use_param_updater, use_signal_from_query, use_update_url_query},
    types::{OrganisationId, Tenant},
    utils::url_or_string,
};

const DEFAULT_CONFIG_COLUMN: &str = "default_config";
const KEY_COLUMN: &str = "config_key";

fn table_columns(
    contexts_vector_rws: RwSignal<ContextList>,
    strict_mode: bool,
    expand: Callback<String, View>,
) -> Vec<Column> {
    let mut contexts = contexts_vector_rws
        .with(|contexts| contexts.iter().map(|(k, _)| k.clone()).collect::<Vec<_>>());
    contexts.push(DEFAULT_CONFIG_COLUMN.to_string());

    let mut fixed_columns = vec![Column::default_with_cell_formatter(
        KEY_COLUMN.to_string(),
        move |key, _row| expand.call(key.to_string()),
    )];

    let column_formatter = move |value: &str| {
        let column = StoredValue::new(value.to_string());
        if value == DEFAULT_CONFIG_COLUMN {
            view! { <kbd class="kbd">Default Config</kbd> }.into_view()
        } else {
            let conditions =
                Conditions::try_from_resolve_context_str(value).unwrap_or_default();
            view! {
                <div class="flex flex-row gap-2 items-center">
                    <i
                        class="ri-close-circle-fill text-lg cursor-pointer"
                        on:click=move |_| {
                            contexts_vector_rws
                                .update(|context_vector| {
                                    context_vector.remove(&column.get_value());
                                })
                        }
                    />
                    <ConditionCollapseProvider>
                        <ConditionComponent
                            conditions
                            id=column.get_value()
                            grouped_view=false
                            class="xl:w-[400px] h-fit"
                            strict_mode
                        />
                    </ConditionCollapseProvider>
                </div>
            }
            .into_view()
        }
    };
    for context in contexts.into_iter().rev() {
        fixed_columns.push(Column::new(
            context,
            false,
            |value: &str, _row: &Map<String, Value>| {
                view! { <span>{url_or_string(value)}</span> }.into_view()
            },
            ColumnSortable::No,
            Expandable::Enabled(100),
            column_formatter,
        ));
    }
    fixed_columns
}

#[component]
pub fn compare_overrides() -> impl IntoView {
    let workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let (context_rs, context_ws) = create_signal::<Conditions>(Conditions::default());
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    // this vector stores the list of contexts the user is comparing
    // let contexts_vector_rws = RwSignal::new(Vec::new());
    let page_params_rws = use_signal_from_query(move |query_string| {
        Query::<PageParams>::extract_non_empty(&query_string).into_inner()
    });
    let context_vec_rws = use_signal_from_query(move |query_string| {
        ContextList::extract_non_empty(&query_string)
    });
    let bread_crums = Signal::derive(move || {
        get_bread_crums(
            page_params_rws.with(|p| p.prefix.clone()),
            "Compare Overrides".to_string(),
        )
    });

    use_param_updater(move || box_params!(page_params_rws.get(), context_vec_rws.get()));

    let dimension_resource = create_blocking_resource(
        move || (workspace.get().0, org.get().0),
        |(tenant, org)| async {
            fetch_dimensions(&PaginationParams::all_entries(), tenant, org)
                .await
                .unwrap_or_default()
        },
    );

    let source = move || {
        let tenant = workspace.get().0;
        let org_id = org.get().0;
        let contexts = context_vec_rws.get();
        (tenant, org_id, contexts)
    };
    let resolved_config_resource =
        create_blocking_resource(source, |(tenant, org_id, mut contexts)| async move {
            contexts.insert(DEFAULT_CONFIG_COLUMN.to_string(), Map::new());
            let mut contexts_config_vector_map: ComparisonTable = HashMap::new();
            for (context_key, context) in contexts.iter() {
                let context = context
                    .iter()
                    .map(|(k, v)| format!("{}={}", k, v))
                    .collect::<Vec<_>>()
                    .join("&");
                match resolve_config(&context, false, None, &tenant, &org_id).await {
                    Ok(config) => {
                        for (config_key, resolved_value) in config {
                            let mut row_vector = contexts_config_vector_map
                                .get(&config_key)
                                .cloned()
                                .unwrap_or_default();
                            row_vector.insert(
                                KEY_COLUMN.to_string(),
                                Value::String(config_key.clone()),
                            );
                            row_vector.insert(context_key.clone(), resolved_value);
                            contexts_config_vector_map.insert(config_key, row_vector);
                        }
                    }
                    Err(e) => {
                        logging::error!(
                            "Error resolving config for context {}: {}",
                            context_key,
                            e
                        );
                        enqueue_alert(e.clone(), AlertType::Error, 1000);
                    }
                }
            }
            let mut resolved_config_map: Vec<Map<String, Value>> =
                contexts_config_vector_map.into_values().collect();

            resolved_config_map.sort_by(|a, b| {
                let key_a = a.get(KEY_COLUMN).and_then(Value::as_str).unwrap_or("");
                let key_b = b.get(KEY_COLUMN).and_then(Value::as_str).unwrap_or("");
                key_a.to_lowercase().cmp(&key_b.to_lowercase())
            });
            resolved_config_map
        });
    let fn_environment = create_memo(move |_| {
        let context = context_rs.get();
        json!({
            "context": context,
            "overrides": [],
        })
    });

    let redirect_url = move |prefix: Option<String>| -> String {
        let get_updated_query = use_update_url_query();
        get_updated_query("prefix", prefix)
    };

    let expand = Callback::new(move |label: String| {
        let is_folder = label.ends_with('.');

        if is_folder {
            let prefix = page_params_rws.with(|p| {
                p.prefix
                    .as_ref()
                    .map_or_else(|| label.clone(), |p| format!("{p}{label}"))
            });
            view! {
                <A
                    class="text-blue-500 underline underline-offset-2"
                    href=redirect_url(Some(prefix))
                >
                    {label}
                </A>
            }
            .into_view()
        } else {
            view! { <span>{label}</span> }.into_view()
        }
    });

    view! {
        <div class="h-full flex flex-col gap-8">
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Block /> }
            }>
                <div class="card bg-base-100 shadow">
                    <div class="card-body collapse collapse-arrow" style="overflow: unset">
                        <input type="checkbox" checked=true />
                        <h2 class="card-title collapse-title h-fit !p-0">"Add Contexts"</h2>
                        <div class="collapse-content !p-0 flex flex-col gap-8">
                            {move || {
                                let dimensions = dimension_resource
                                    .with(|d| {
                                        d.as_ref().map(|d| d.data.clone()).unwrap_or_default()
                                    });
                                view! {
                                    <ContextForm
                                        dimensions
                                        context=context_rs.get_untracked()
                                        on_context_change=move |new_context| {
                                            context_ws.set(new_context)
                                        }
                                        heading_sub_text="Query your configs"
                                        resolve_mode=true
                                        fn_environment
                                    />
                                }
                            }}
                            {move || {
                                let loading = req_inprogess_rs.get();
                                view! {
                                    <Button
                                        id="compare"
                                        text="Add Comparision"
                                        class="self-end"
                                        icon_class="ri-add-line"
                                        on_click=move |_| {
                                            req_inprogress_ws.set(true);
                                            let context = context_rs.get();
                                            if context.is_empty() {
                                                enqueue_alert(
                                                    "Please provide a valid context to compare".into(),
                                                    AlertType::Error,
                                                    1000,
                                                );
                                                req_inprogress_ws.set(false);
                                                return;
                                            }
                                            let context = context.as_resolve_context();
                                            let query = Value::Object(context.clone()).to_string();
                                            context_vec_rws
                                                .update(|value| {
                                                    if let Entry::Vacant(e) = value.entry(query) {
                                                        e.insert(context);
                                                    } else {
                                                        enqueue_alert(
                                                            "This context has already been added to compare".into(),
                                                            AlertType::Error,
                                                            1000,
                                                        );
                                                    }
                                                });
                                            req_inprogress_ws.set(false);
                                        }
                                        loading=loading
                                    />
                                }
                            }}
                        </div>
                    </div>
                </div>
            </Suspense>
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Block /> }
            }>
                {move || {
                    let mut filtered_rows = resolved_config_resource.get().unwrap_or_default();
                    let table_columns = table_columns(
                        context_vec_rws,
                        workspace_settings.with_value(|w| w.strict_mode),
                        expand,
                    );
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
                            KEY_COLUMN,
                        );
                    }

                    view! {
                        <div class="card min-h-[200px] w-full overflow-hidden bg-base-100 rounded-xl shadow">
                            <div class="card-body overflow-y-auto overflow-x-visible">
                                <div class="flex justify-between">
                                    <BreadCrums redirect_url bread_crums=bread_crums.get() />
                                    <label
                                        on:click=move |_| {
                                            page_params_rws
                                                .update(|params| {
                                                    params.grouped = !params.grouped;
                                                    params.prefix = None;
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
                                </div>
                                <Table
                                    class="!overflow-y-auto"
                                    rows=filtered_rows
                                    key_column=KEY_COLUMN
                                    columns=table_columns
                                />
                            </div>
                        </div>
                    }
                }}

            </Suspense>
        </div>
    }
}
