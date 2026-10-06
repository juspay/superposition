mod filter;
mod types;
pub mod utils;

use filter::{DefaultConfigFilterWidget, FilterSummary};
use leptos::*;
use leptos_router::A;
use serde_json::{Map, Value, json};
use superposition_types::{
    api::default_config::DefaultConfigFilters,
    custom_query::{CustomQuery, PaginationParams, Query, QueryParam},
};
use types::PageParams;
use utils::{BreadCrums, get_bread_crums, modify_rows};

use crate::components::{
    button::ButtonAnchor,
    datetime::DatetimeStr,
    skeleton::Skeleton,
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

#[component]
pub fn DefaultConfigList() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let (filters_rws, pagination_params_rws, page_params_rws) =
        use_signal_from_query(move |query_string| {
            let page_params =
                Query::<PageParams>::extract_non_empty(query_string).into_inner();
            (
                Query::<DefaultConfigFilters>::extract_non_empty(query_string)
                    .into_inner(),
                if page_params.grouped {
                    PaginationParams::all_entries()
                } else {
                    Query::<PaginationParams>::extract_non_empty(query_string)
                        .into_inner()
                },
                page_params,
            )
        });

    let bread_crums = Signal::derive(move || {
        get_bread_crums(
            page_params_rws.with(|p| p.prefix.clone()),
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
            // `list_resolved` (not `list`) so `symlink_to` survives into the
            // row map `table_rows` below, for the badge in `expand`.
            default_configs::list_resolved(&pagination_params, &filters, &workspace, &org_id)
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
        let expand = move |key_name: &str, row: &Map<String, Value>| {
            let label = key_name.to_string();
            let is_folder = key_name.ends_with('.');
            let prefix = page_params_rws.with(|p| {
                p.prefix
                    .as_ref()
                    .map_or_else(|| label.clone(), |p| format!("{p}{label}"))
            });
            let symlink_to = row
                .get("symlink_to")
                .and_then(Value::as_str)
                .map(str::to_string);
            let symlink_badge = symlink_to.map(|target| {
                view! {
                    <span class="badge badge-sm badge-ghost ml-2" title="symlink">
                        <i class="ri-links-line mr-1" />
                        {format!("→ {target}")}
                    </span>
                }
            });

            if is_folder {
                view! {
                    <A href=redirect_url(Some(prefix))>
                        <i class="ri-folder-open-line mr-2" />
                        <span class="text-blue-500 underline underline-offset-2">{label}</span>
                    </A>
                    {symlink_badge}
                }
                .into_view()
            } else {
                view! {
                    <A href=prefix class="ml-[22px] text-blue-500 underline underline-offset-2">
                        {label}
                    </A>
                    {symlink_badge}
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
            Column::default_with_cell_formatter("created_at".to_string(), |value, _| {
                view! {
                    <DatetimeStr datetime=value.into() />
                }
            }),
            Column::new(
                "last_modified_at".to_string(),
                false,
                |value, _| {
                    view! {
                        <DatetimeStr datetime=value.into() />
                    }
                },
                ColumnSortable::No,
                Expandable::Enabled(100),
                |_| default_column_formatter("Modified At"),
            ),
        ]
    });

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
            {move || {
                // `DefaultConfigResponse` doesn't derive `Clone`, so pull
                // what's needed out through `.with()` (which hands back
                // `&Option<T>`, no `Clone` bound) rather than `.get()`.
                let (table_rows, total_items, total_pages_count) = default_config_resource
                    .with(|opt| match opt {
                        Some(default_config) => (
                            default_config
                                .data
                                .iter()
                                .map(|config| json!(config).as_object().unwrap().to_owned())
                                .collect::<Vec<Map<String, Value>>>(),
                            default_config.total_items,
                            default_config.total_pages,
                        ),
                        None => (Vec::new(), 0, 0),
                    });
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
                let total_default_config_keys = total_items.to_string();
                let pagination_params = pagination_params_rws.get();
                let (current_page, total_pages) = if page_params.grouped {
                    (1, 1)
                } else {
                    (pagination_params.page.unwrap_or_default(), total_pages_count)
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
                                <ButtonAnchor
                                    class="self-end h-10"
                                    text="Create Config"
                                    icon_class="ri-add-line"
                                    href=format!(
                                        "action/create?{}",
                                        page_params_rws
                                            .with(|p| {
                                                CreatePageParams::from(p.clone()).to_query_param()
                                            }),
                                    )
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
        </Suspense>
    }
}

#[cfg(test)]
mod tests {
    //! `symlink_to` needs to survive from `DefaultConfigResponse` into the
    //! `Map<String, Value>` row `expand` reads it from. The flatten plus
    //! `skip_serializing_if` on the field *should* carry it through
    //! re-serialization both ways, but the task that added the badge was
    //! told explicitly to verify that rather than assume it - so this pins
    //! it down rather than trusting serde's behavior by inspection alone.
    use chrono::Utc;
    use serde_json::{Value, json};
    use superposition_types::{
        ExtendedMap,
        api::default_config::DefaultConfigResponse,
        database::models::{ChangeReason, Description, cac::DefaultConfig},
    };

    fn sample_config(key: &str) -> DefaultConfig {
        DefaultConfig {
            key: key.to_string(),
            value: Value::String("hello".to_string()),
            created_at: Utc::now(),
            created_by: "tester".to_string(),
            schema: ExtendedMap::default(),
            value_validation_function_name: None,
            last_modified_at: Utc::now(),
            last_modified_by: "tester".to_string(),
            description: Description::default(),
            change_reason: ChangeReason::default(),
            value_compute_function_name: None,
        }
    }

    #[test]
    fn symlink_to_survives_into_the_row_map() {
        let response = DefaultConfigResponse {
            config: sample_config("alias.key"),
            symlink_to: Some("target.key".to_string()),
        };

        let row = json!(response).as_object().unwrap().to_owned();

        assert_eq!(
            row.get("symlink_to"),
            Some(&Value::String("target.key".to_string())),
            "symlink_to must survive serde flatten into the row map the \
             list page's `expand` closure reads from"
        );
        assert_eq!(row.get("key"), Some(&Value::String("alias.key".to_string())));
    }

    #[test]
    fn an_ordinary_key_has_no_symlink_to_in_its_row_map() {
        let response = DefaultConfigResponse {
            config: sample_config("ordinary.key"),
            symlink_to: None,
        };

        let row = json!(response).as_object().unwrap().to_owned();

        assert!(
            !row.contains_key("symlink_to"),
            "an ordinary key's row must not carry a symlink_to field at all \
             (skip_serializing_if), so `row.get(\"symlink_to\")` reliably \
             means None rather than an explicit null"
        );
    }
}
