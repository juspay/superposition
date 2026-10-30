mod filter;
pub mod utils;

use filter::{DefaultConfigFilterWidget, FilterSummary};
use leptos::leptos_dom::helpers::debounce;
use leptos::*;
use leptos_router::A;
use serde_json::{Map, Value, json};
use superposition_types::{
    api::default_config::{DefaultConfigFilters, ListDefaultConfigResponse, SortOn},
    custom_query::{CustomQuery, PaginationParams, Query, QueryParam},
};
use utils::{BreadCrums, get_bread_crums};

use crate::components::{
    button::ButtonAnchor,
    datetime::DatetimeStr,
    skeleton::{Skeleton, SkeletonVariant},
    stat::Stat,
    table::{
        Table,
        types::{
            Column, ColumnSortable, Expandable, TablePaginationProps,
            default_column_formatter,
        },
    },
};
use crate::query_updater::{use_signal_from_query, use_update_url_query};
use crate::types::{OrganisationId, Workspace};
use crate::{api::default_configs, pages::default_config::CreatePageParams};

fn sort_callback(
    sort_on: SortOn,
    filters_rws: RwSignal<DefaultConfigFilters>,
    pagination_params_rws: RwSignal<PaginationParams>,
) -> Callback<()> {
    Callback::new(move |_| {
        let filters = filters_rws.get();
        let sort_by = filters.sort_by.unwrap_or_default().flip();

        let new_filters = DefaultConfigFilters {
            sort_on: Some(sort_on),
            sort_by: Some(sort_by),
            ..filters
        };
        pagination_params_rws.update(|f| f.reset_page());
        filters_rws.set(new_filters);
    })
}

#[component]
pub fn DefaultConfigList() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let (filters_rws, pagination_params_rws) =
        use_signal_from_query(move |query_string| {
            let pagination =
                Query::<PaginationParams>::extract_non_empty(query_string).into_inner();
            let mut filters =
                Query::<DefaultConfigFilters>::extract_non_empty(query_string)
                    .into_inner();

            if pagination.page.unwrap_or(1) <= 1 {
                filters.init_with_grouping();
            }

            (filters, pagination)
        });
    let bread_crums = Signal::derive(move || {
        get_bread_crums(
            filters_rws.with(|p| p.prefix.clone()),
            "Default Config".to_string(),
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
        |(workspace, pagination_params, org_id, filters)| async move {
            default_configs::list_grouped(
                &pagination_params,
                filters,
                &workspace,
                &org_id,
            )
            .await
            .unwrap_or_default()
        },
    );

    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    });

    let redirect_url = move |prefix: Option<String>| -> String {
        let get_updated_query = use_update_url_query();
        let page_params = pagination_params_rws.get();

        get_updated_query(&[
            ("prefix", prefix),
            ("name", None),
            ("page", page_params.page.map(|_| "1".to_string())),
            ("count", page_params.count.map(|c| c.to_string())),
            ("all", page_params.all.map(|a| a.to_string())),
        ])
    };

    let table_columns = create_memo(move |_| {
        let expand = move |key_name: &str, row: &Map<String, Value>| {
            let label = key_name.to_string();
            let is_folder = row
                .get("folder")
                .and_then(Value::as_bool)
                .unwrap_or_default();
            let prefix = filters_rws.with(|p| {
                p.prefix
                    .as_ref()
                    .map_or_else(|| label.clone(), |p| format!("{p}{label}"))
            });

            if is_folder {
                view! {
                    <A href=redirect_url(Some(format!("{prefix}.")))>
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

        let current_sort_by = filters_rws.with(|f| f.sort_by.unwrap_or_default());
        let current_sort_on = filters_rws.with(|f| f.sort_on.unwrap_or_default());

        vec![
            Column::new(
                "key".to_string(),
                false,
                expand,
                ColumnSortable::Yes {
                    sort_fn: sort_callback(
                        SortOn::Key,
                        filters_rws,
                        pagination_params_rws,
                    ),
                    sort_by: current_sort_by,
                    currently_sorted: current_sort_on == SortOn::Key,
                },
                Expandable::Disabled,
                default_column_formatter,
            ),
            Column::default("value".to_string()),
            Column::new(
                "created_at".to_string(),
                false,
                |value, _| {
                    view! {
                        <DatetimeStr datetime=value.into() />
                    }
                },
                ColumnSortable::Yes {
                    sort_fn: sort_callback(
                        SortOn::CreatedAt,
                        filters_rws,
                        pagination_params_rws,
                    ),
                    sort_by: current_sort_by,
                    currently_sorted: current_sort_on == SortOn::CreatedAt,
                },
                Expandable::Enabled(100),
                default_column_formatter,
            ),
            Column::new(
                "last_modified_at".to_string(),
                false,
                |value, _| {
                    view! {
                        <DatetimeStr datetime=value.into() />
                    }
                },
                ColumnSortable::Yes {
                    sort_fn: sort_callback(
                        SortOn::LastModifiedAt,
                        filters_rws,
                        pagination_params_rws,
                    ),
                    sort_by: current_sort_by,
                    currently_sorted: current_sort_on == SortOn::LastModifiedAt,
                },
                Expandable::Enabled(100),
                |_| default_column_formatter("Modified At"),
            ),
        ]
    });

    view! {
        <div class="h-full flex flex-col gap-4">
            <div class="flex justify-between">
                <Suspense fallback=move || {
                    view! { <Skeleton variant=SkeletonVariant::Block style_class="h-24 w-48" /> }
                }>
                    {move || {
                        let default_config = default_config_resource.get().unwrap_or_default();
                        let total_default_config_keys = default_config.total_items.to_string();
                        view! {
                            <Stat
                                heading="Config Keys"
                                icon="ri-tools-line"
                                number=total_default_config_keys
                            />
                        }
                    }}
                </Suspense>
                <div class="flex items-end gap-4 lg+:flex-row flex-col-reverse">
                    <div class="flex gap-4">
                        <label class="input input-bordered flex items-center gap-2 h-9">
                            <i class="ri-search-line" />
                            <input
                                type="text"
                                class="grow"
                                key="search_input"
                                autofocus=true
                                placeholder="Search configs"
                                prop:value=move || {
                                    filters_rws.with(|p| { p.search.clone().unwrap_or_default() })
                                }
                                on:keyup=debounce(
                                    std::time::Duration::from_millis(600),
                                    move |event| {
                                        let search = event_target_value(&event);
                                        let search = (!search.trim().is_empty()).then_some(search);
                                        batch(|| {
                                            filters_rws
                                                .update(|params| {
                                                    logging::log!(
                                                        "Updating search param to: {:?}, search is none: {}", search, search.is_none()
                                                    );
                                                    params.grouped = search.is_none().then_some(true);
                                                    params.set_search(search);
                                                });
                                            pagination_params_rws.update(|p| p.reset_page());
                                        });
                                    },
                                )
                            />
                        </label>
                        <label
                            on:click=move |_| {
                                batch(|| {
                                    filters_rws
                                        .update(|params| {
                                            let grouped = params.grouped.unwrap_or_default();
                                            *params = DefaultConfigFilters::default();
                                            params.grouped = (!grouped).then_some(true);
                                        });
                                    pagination_params_rws.update(|p| p.reset_page());
                                });
                            }
                            class="label gap-4 cursor-pointer"
                        >
                            <span class="label-text min-w-max">Group Configs</span>
                            <input
                                type="checkbox"
                                class="toggle toggle-primary"
                                prop:checked=move || {
                                    filters_rws.with(|p| p.grouped.unwrap_or_default())
                                }
                            />
                        </label>
                    </div>
                    <div class="flex gap-4">
                        <DefaultConfigFilterWidget filters_rws pagination_params_rws />
                        {move || {
                            view! {
                                <ButtonAnchor
                                    class="self-end h-10"
                                    text="Create Config"
                                    icon_class="ri-add-line"
                                    href=format!(
                                        "action/create?{}",
                                        filters_rws
                                            .with(|p| { CreatePageParams::from(p).to_query_param() }),
                                    )
                                />
                            }
                        }}
                    </div>
                </div>
            </div>
            <FilterSummary filters_rws />
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Block /> }
            }>
                {move || {
                    let default_config = default_config_resource.get().unwrap_or_default();
                    let table_rows = default_config
                        .data
                        .into_iter()
                        .map(|resp| {
                            match resp {
                                ListDefaultConfigResponse::Config(config) => {
                                    json!(config).as_object().unwrap().clone()
                                }
                                ListDefaultConfigResponse::Group(key) => {
                                    Map::from_iter([
                                        ("key".to_string(), Value::String(key)),
                                        ("folder".to_string(), Value::Bool(true)),
                                    ])
                                }
                            }
                        })
                        .collect::<Vec<Map<String, Value>>>();
                    let pagination_params = pagination_params_rws.get();
                    let pagination_props = TablePaginationProps {
                        enabled: true,
                        count: pagination_params.count.unwrap_or_default(),
                        current_page: pagination_params.page.unwrap_or_default(),
                        total_pages: default_config.total_pages,
                        on_page_change: handle_page_change,
                    };

                    view! {
                        <div class="card w-full bg-base-100 rounded-lg overflow-hidden shadow">
                            <div class="card-body overflow-y-auto overflow-x-visible">
                                <BreadCrums
                                    bread_crums=bread_crums.get()
                                    redirect_url
                                    show_root=false
                                />
                                <Table
                                    class="!overflow-y-auto"
                                    rows=table_rows
                                    key_column="key"
                                    columns=table_columns.get()
                                    pagination=pagination_props
                                />
                            </div>
                        </div>
                    }
                }}
            </Suspense>
        </div>
    }
}
