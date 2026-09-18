mod types;

use std::collections::{HashMap, hash_map::Entry};

use leptos::*;
use leptos_router::A;
use serde_json::{Map, Value, json};
use superposition_types::{
    api::{config::ResolveConfigQuery, functions::FunctionEnvironment},
    custom_query::{CustomQuery, DimensionQuery, PaginationParams, Query},
};
use types::{ComparisonTable, ContextList, PageParams};

use crate::{
    api::{dimensions, resolve_config_detailed},
    components::{
        alert::AlertType,
        button::Button,
        condition_pills::Condition,
        context_form::ContextForm,
        skeleton::{Skeleton, SkeletonVariant},
        table::{
            Table,
            types::{Column, ColumnSortable, Expandable},
        },
    },
    logic::Conditions,
    pages::default_config_list::utils::{BreadCrums, get_bread_crums, modify_rows},
    providers::{
        alert_provider::enqueue_alert,
        condition_collapse_provider::ConditionCollapseProvider,
    },
    query_updater::{use_signal_from_query, use_update_url_query},
    types::{OrganisationId, Workspace},
    utils::url_or_string,
};

const DEFAULT_CONFIG_COLUMN: &str = "default_config";
const KEY_COLUMN: &str = "config_key";
/// Prefix for the per-cell provenance entries stashed alongside each resolved value in a row.
/// Keyed by the owning context column so the cell formatter can find which override set the
/// value. These keys never become table columns (columns are built from the context list).
const SOURCE_KEY_PREFIX: &str = "__source__::";

fn source_key(context_key: &str) -> String {
    format!("{SOURCE_KEY_PREFIX}{context_key}")
}

fn table_columns(
    contexts_vector_rws: RwSignal<ContextList>,
    expand: Callback<String, View>,
    workspace: Signal<Workspace>,
    org: Signal<OrganisationId>,
) -> Vec<Column> {
    let mut contexts =
        contexts_vector_rws.with(|contexts| contexts.keys().cloned().collect::<Vec<_>>());
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
                        <Condition
                            conditions
                            id=column.get_value()
                            grouped_view=false
                            resolve_summary=true
                            class="xl:w-[400px] h-fit"
                        />
                    </ConditionCollapseProvider>
                </div>
            }
            .into_view()
        }
    };
    for context in contexts.into_iter().rev() {
        let source_col = source_key(&context);
        let cell_formatter = move |value: &str, row: &Map<String, Value>| {
            let value_view = view! { <span>{url_or_string(value)}</span> };
            let source = row.get(&source_col).and_then(Value::as_object);
            let Some(context_id) = source
                .and_then(|s| s.get("context_id"))
                .and_then(Value::as_str)
                .map(String::from)
            else {
                // Value came from the default config (no matching override); render as-is.
                return value_view.into_view();
            };
            let conditions = source
                .and_then(|s| s.get("context"))
                .and_then(Value::as_object)
                .map(|m| Conditions::from_iter(m.clone()))
                .unwrap_or_default();
            let href = format!(
                "/admin/{}/{}/overrides/{}",
                org.get().0,
                workspace.get().0,
                context_id,
            );
            let pill_id = format!("compare-src-{context_id}");
            view! {
                <div class="dropdown dropdown-hover dropdown-right">
                    <div tabindex="0" class="flex flex-row items-center gap-1 cursor-help w-fit">
                        {value_view}
                        <i class="ri-links-line text-sm text-gray-400" />
                    </div>
                    <div
                        tabindex="0"
                        class="dropdown-content z-[50] card card-compact bg-base-100 shadow-lg border border-gray-200 p-3 w-max max-w-md flex flex-col gap-2"
                    >
                        <span class="text-xs font-semibold text-gray-500">"Set by override"</span>
                        <ConditionCollapseProvider>
                            <Condition
                                conditions
                                id=pill_id
                                grouped_view=false
                                resolve_summary=true
                                class="h-fit"
                            />
                        </ConditionCollapseProvider>
                        <A
                            href=href
                            class="link link-primary text-sm inline-flex items-center gap-1"
                        >
                            "View override"
                            <i class="ri-arrow-right-line" />
                        </A>
                    </div>
                </div>
            }
            .into_view()
        };
        fixed_columns.push(Column::new(
            context,
            false,
            cell_formatter,
            ColumnSortable::No,
            Expandable::Enabled(100),
            column_formatter,
        ));
    }
    fixed_columns
}

#[component]
pub fn CompareOverrides() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let (context_rs, context_ws) = create_signal::<Conditions>(Conditions::default());
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    // this vector stores the list of contexts the user is comparing
    // let contexts_vector_rws = RwSignal::new(Vec::new());
    let (page_params_rws, context_vec_rws) = use_signal_from_query(move |query_string| {
        (
            Query::<PageParams>::extract_non_empty(query_string).into_inner(),
            ContextList::extract_non_empty(query_string),
        )
    });
    let bread_crums = Signal::derive(move || {
        get_bread_crums(
            page_params_rws.with(|p| p.prefix.clone()),
            "Compare Overrides".to_string(),
        )
    });

    let dimension_resource = create_blocking_resource(
        move || (workspace.get().0, org.get().0),
        |(workspace, org)| async move {
            dimensions::list(&PaginationParams::all_entries(), &workspace, &org)
                .await
                .unwrap_or_default()
        },
    );

    let source = move || {
        let workspace = workspace.get().0;
        let org_id = org.get().0;
        let contexts = context_vec_rws.get();
        (workspace, org_id, contexts)
    };
    let resolved_config_resource = create_blocking_resource(
        source,
        |(workspace, org_id, mut contexts)| async move {
            contexts.insert(DEFAULT_CONFIG_COLUMN.to_string(), Map::new());
            let mut contexts_config_vector_map: ComparisonTable = HashMap::new();
            for (context_key, context) in contexts.iter() {
                let context = DimensionQuery::from(context.clone());
                match resolve_config_detailed(
                    &context,
                    &ResolveConfigQuery::default(),
                    &workspace,
                    &org_id,
                )
                .await
                {
                    Ok(config) => {
                        for (config_key, resolved) in config {
                            let mut row_vector = contexts_config_vector_map
                                .get(&config_key)
                                .cloned()
                                .unwrap_or_default();
                            row_vector.insert(
                                KEY_COLUMN.to_string(),
                                Value::String(config_key.clone()),
                            );
                            row_vector.insert(context_key.clone(), resolved.value);
                            // Stash which override contributed this value so the cell can
                            // link to it. Absent context_id => value came from default config.
                            if let Some(context_id) = resolved.context_id {
                                let condition = resolved
                                    .context
                                    .map(|c| Value::Object(c.into_inner()))
                                    .unwrap_or(Value::Null);
                                row_vector.insert(
                                    source_key(context_key),
                                    json!({ "context_id": context_id, "context": condition }),
                                );
                            }
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
        },
    );

    let fn_environment = Memo::new(move |_| FunctionEnvironment {
        context: context_rs.get().into(),
        overrides: Map::new(),
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
        <div class="h-full flex flex-col gap-4 min-h-0">
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Block /> }
            }>
                <div class="card bg-base-100 shadow">
                    <div class="card-body collapse collapse-arrow" style="overflow: unset">
                        // Start collapsed when a comparison already exists (e.g. a shared/saved
                        // link) so the table gets the screen; stay open when there's nothing to
                        // compare yet, to guide adding the first context. Still click-to-toggle.
                        <input
                            type="checkbox"
                            checked=context_vec_rws.with_untracked(|c| c.keys().next().is_none())
                        />
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
                                        heading_sub_text="Resolve your configs"
                                        resolve_mode=true
                                        compact=true
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
                                            let context = Map::from(context);
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
                    let table_columns = table_columns(context_vec_rws, expand, workspace, org);
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
                        <div class="card flex-1 min-h-[200px] w-full overflow-hidden bg-base-100 rounded-xl shadow">
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
