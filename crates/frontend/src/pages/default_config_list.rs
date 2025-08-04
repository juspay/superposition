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

use crate::components::{
    default_config_form::DefaultConfigForm,
    drawer::PortalDrawer,
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
use crate::query_updater::{
    use_param_updater, use_signal_from_query, use_update_url_query,
};
use crate::types::{OrganisationId, Tenant};
use crate::{api::fetch_default_config, components::button::Button};

#[derive(Clone)]
enum Action {
    Create,
    None,
}

#[component]
pub fn default_config_list() -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let action_rws = RwSignal::new(Action::None);
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

    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    });

    let redirect_url = move |prefix: Option<String>| -> String {
        let get_updated_query = use_update_url_query();
        get_updated_query("prefix", prefix)
    };

    let table_columns = create_memo(move |_| {
        let expand = move |key_name: &str, _row: &Map<String, Value>| {
            let label = key_name.to_string();
            let is_folder = key_name.ends_with('.');
            let prefix = page_params_rws.with(|p| {
                p.prefix
                    .as_ref()
                    .map_or_else(|| label.clone(), |p| format!("{p}{label}"))
            });

            if is_folder {
                view! {
                    <A href=redirect_url(Some(prefix))>
                        <i class="ri-folder-open-line mr-2" />
                        <span class="text-blue-500 underline underline-offset-2">{label}</span>
                    </A>
                }
                .into_view()
            } else {
                view! {
                    <A href=prefix class="ml-[22px] text-blue-500 underline underline-offset-2">
                        {label}
                    </A>
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
            Column::default("value".to_string()),
            Column::default("created_at".to_string()),
            Column::default_with_column_formatter("last_modified_at".to_string(), |_| {
                default_column_formatter("Modified At")
            }),
        ]
    });

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
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
                                Value::String(config.created_at.format("%v %T").to_string()),
                            );
                        ele_map
                            .insert(
                                "last_modified_at".to_string(),
                                Value::String(config.last_modified_at.format("%v %T").to_string()),
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
                    <div class="h-full flex flex-col gap-4">
                        <div class="flex justify-between">
                            <Stat
                                heading="Config Keys"
                                icon="ri-tools-line"
                                number=total_default_config_keys
                            />
                            <div class="flex items-end gap-4">
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
                                <DefaultConfigFilterWidget
                                    filters_rws
                                    pagination_params_rws
                                    prefix=page_params_rws.with(|p| p.prefix.clone())
                                />
                                <Button
                                    on_click=move |_| action_rws.set(Action::Create)
                                    text="Create Key"
                                    icon_class="ri-add-line"
                                />
                            </div>
                        </div>
                        <FilterSummary filters_rws />
                        <div class="card w-full bg-base-100 rounded-lg overflow-hidden shadow">
                            <div class="card-body overflow-y-auto overflow-x-visible">
                                <BreadCrums
                                    bread_crums=bread_crums.get()
                                    redirect_url
                                    show_root=false
                                />
                                <Table
                                    class="!overflow-y-auto"
                                    rows=filtered_rows
                                    key_column="key"
                                    columns=table_columns.get()
                                    pagination=pagination_props
                                />
                            </div>
                        </div>
                    </div>
                }
            }}
            {move || match action_rws.get() {
                Action::Create => {
                    view! {
                        <PortalDrawer
                            title="Create New Key"
                            handle_close=move |_| action_rws.set(Action::None)
                        >
                            <DefaultConfigForm
                                prefix=page_params_rws.with(|p| p.prefix.clone())
                                handle_submit=move |_| {
                                    filters_rws.set(DefaultConfigFilters::default());
                                    pagination_params_rws.update(|f| f.reset_page());
                                    default_config_resource.refetch();
                                    action_rws.set(Action::None);
                                }
                            />
                        </PortalDrawer>
                    }
                        .into_view()
                }
                Action::None => view! {}.into_view(),
            }}
        </Suspense>
    }
}
