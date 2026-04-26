use leptos::*;
use leptos_router::A;
use serde_json::{Map, Value, json};
use superposition_types::{
    api::copy_to::{CopyEntityType, CopyToRequest, RowSelectionMode},
    custom_query::{CustomQuery, PaginationParams, Query},
    database::models::ChangeReason,
};
use superposition_types::database::models::cac::DimensionType;

use crate::api::{copy_to, dimensions, workspaces};
use crate::components::alert::AlertType;
use crate::components::button::ButtonAnchor;
use crate::components::datetime::DatetimeStr;
use crate::components::skeleton::Skeleton;
use crate::components::table::types::{
    ColumnSortable, Expandable, default_column_formatter,
};
use crate::components::{
    stat::Stat,
    table::{
        Table,
        types::{Column, TablePaginationProps},
    },
};
use crate::providers::alert_provider::enqueue_alert;
use crate::query_updater::use_signal_from_query;
use crate::types::{DimensionTypeOptions, OrganisationId, Workspace};

#[component]
pub fn Dimensions() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let (pagination_params_rws,) = use_signal_from_query(move |query_string| {
        (Query::<PaginationParams>::extract_non_empty(query_string).into_inner(),)
    });

    let dimensions_resource = create_blocking_resource(
        move || (workspace.get().0, pagination_params_rws.get(), org.get().0),
        |(workspace, pagination_params, org_id)| async move {
            dimensions::list(&pagination_params, &workspace, &org_id)
                .await
                .unwrap_or_default()
        },
    );

    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    });

    let workspaces_resource = create_blocking_resource(
        move || org.get().0,
        |org_id| async move {
            workspaces::list(&PaginationParams::all_entries(), &org_id)
                .await
                .unwrap_or_default()
        },
    );

    let selected_dimensions_rws = create_rw_signal::<Vec<String>>(vec![]);
    let copy_all_rws = create_rw_signal(true);
    let target_workspace_rws = create_rw_signal(String::new());
    let skip_existing_rws = create_rw_signal(true);
    let change_reason_rws = create_rw_signal(String::new());
    let copy_inprogress_rws = create_rw_signal(false);

    let table_columns = create_memo(move |_| {
        let selected_dimensions_rws = selected_dimensions_rws;
        let copy_all_rws = copy_all_rws;
        vec![
            Column::new(
                "dimension".to_string(),
                false,
                move |dimension_name: &str, _row: &Map<String, Value>| {
                    let dimension_name = dimension_name.to_string();
                    let checked_dimension_name = dimension_name.clone();
                    let changed_dimension_name = dimension_name.clone();
                    let link_dimension_name = dimension_name.clone();
                    view! {
                        <div class="flex items-center gap-3">
                            <input
                                type="checkbox"
                                class="checkbox checkbox-sm"
                                prop:checked=move || {
                                    selected_dimensions_rws
                                        .get()
                                        .contains(&checked_dimension_name)
                                }
                                prop:disabled=move || copy_all_rws.get()
                                on:change=move |event| {
                                    let checked = event_target_checked(&event);
                                    selected_dimensions_rws.update(|selected| {
                                        if checked {
                                            if !selected.contains(&changed_dimension_name) {
                                                selected.push(changed_dimension_name.clone());
                                            }
                                        } else {
                                            selected.retain(|name| name != &changed_dimension_name);
                                        }
                                    });
                                }
                            />
                            <A
                                href=link_dimension_name.clone()
                                class="text-blue-500 underline underline-offset-2"
                            >
                                {link_dimension_name}
                            </A>
                        </div>
                    }
                    .into_view()
                },
                ColumnSortable::No,
                Expandable::Disabled,
                |_| view! { <span class="font-medium text-sm text-black">"Select"</span> }.into_view(),
            ),
            Column::default("position".to_string()),
            Column::default_with_cell_formatter(
                "dimension_type".to_string(),
                move |dimension_type: &str, _| {
                    let dim_type = serde_json::from_str::<DimensionType>(dimension_type)
                        .unwrap_or_default();
                    let dimension_type = DimensionTypeOptions::from(&dim_type);
                    view! {
                        <span>{dimension_type.to_string()}</span>
                    }
                    .into_view()
                },
            ),
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

    let on_copy = Callback::new(move |_| {
        if copy_inprogress_rws.get_untracked() {
            return;
        }

        let target_workspace = target_workspace_rws.get_untracked();
        if target_workspace.is_empty() {
            enqueue_alert(
                String::from("Please select a target workspace."),
                AlertType::Error,
                5000,
            );
            return;
        }

        let change_reason = match ChangeReason::try_from(change_reason_rws.get_untracked()) {
            Ok(change_reason) => change_reason,
            Err(err) => {
                enqueue_alert(err, AlertType::Error, 5000);
                return;
            }
        };

        let selection_mode = if copy_all_rws.get_untracked() {
            RowSelectionMode::All
        } else {
            RowSelectionMode::Selected
        };
        let selected_rows = selected_dimensions_rws.get_untracked();
        if matches!(selection_mode, RowSelectionMode::Selected) && selected_rows.is_empty() {
            enqueue_alert(
                String::from("Select at least one dimension to copy."),
                AlertType::Error,
                5000,
            );
            return;
        }

        copy_inprogress_rws.set(true);
        let workspace = workspace.get_untracked().0;
        let org_id = org.get_untracked().0;
        let skip_existing = skip_existing_rws.get_untracked();
        spawn_local(async move {
            let payload = CopyToRequest {
                entity_type: CopyEntityType::Dimensions,
                target_workspace,
                selection_mode,
                selected_rows,
                skip_existing,
                change_reason,
            };

            let result = copy_to::copy(payload, &workspace, &org_id).await;
            copy_inprogress_rws.set(false);
            match result {
                Ok(response) => {
                    enqueue_alert(
                        format!(
                            "Copied {} dimension(s), skipped {}.",
                            response.copied_count, response.skipped_count
                        ),
                        AlertType::Success,
                        5000,
                    );
                }
                Err(err) => enqueue_alert(err, AlertType::Error, 5000),
            }
        });
    });

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
            {move || {
                let value = dimensions_resource.get().unwrap_or_default();
                let table_rows = value
                    .data
                    .iter()
                    .map(|ele| json!(ele).as_object().unwrap().clone())
                    .collect::<Vec<Map<String, Value>>>();
                let available_workspaces = workspaces_resource
                    .get()
                    .unwrap_or_default()
                    .data
                    .into_iter()
                    .filter(|workspace_resp| workspace_resp.workspace_name != workspace.get().0)
                    .collect::<Vec<_>>();
                let pagination_params = pagination_params_rws.get();
                let pagination_props = TablePaginationProps {
                    enabled: true,
                    count: pagination_params.count.unwrap_or_default(),
                    current_page: pagination_params.page.unwrap_or_default(),
                    total_pages: value.total_pages,
                    on_page_change: handle_page_change,
                };
                view! {
                    <div class="h-full flex flex-col gap-4">
                        <div class="flex justify-between">
                            <Stat
                                heading="Dimensions"
                                icon="ri-ruler-2-fill"
                                number=value.total_items.to_string()
                            />
                            <ButtonAnchor
                                class="self-end h-10"
                                text="Create Dimension"
                                icon_class="ri-add-line"
                                href="action/create"
                            />
                        </div>
                        <div class="card w-full bg-base-100 rounded-xl shadow">
                            <div class="card-body gap-4">
                                <div class="flex flex-col gap-2 md:flex-row md:items-center md:justify-between">
                                    <div>
                                        <h2 class="card-title">"Copy To Workspace"</h2>
                                        <p class="text-sm text-gray-500">
                                            "Copy all dimensions by default, or switch to selected rows."
                                        </p>
                                    </div>
                                    <div class="text-sm text-gray-500">
                                        {move || format!(
                                            "Selected rows: {}",
                                            selected_dimensions_rws.get().len()
                                        )}
                                    </div>
                                </div>
                                <div class="grid gap-4 md:grid-cols-2 xl:grid-cols-4">
                                    <label class="form-control w-full">
                                        <span class="label-text mb-2 font-medium">"Target workspace"</span>
                                        <select
                                            class="select select-bordered w-full"
                                            on:change=move |event| {
                                                target_workspace_rws.set(event_target_value(&event));
                                            }
                                        >
                                            <option value="">"Select workspace"</option>
                                            {available_workspaces
                                                .into_iter()
                                                .map(|workspace_resp| {
                                                    let workspace_name = workspace_resp.workspace_name;
                                                    view! {
                                                        <option value=workspace_name.clone()>{workspace_name}</option>
                                                    }
                                                })
                                                .collect_view()}
                                        </select>
                                    </label>
                                    <label class="form-control w-full">
                                        <span class="label-text mb-2 font-medium">"Change reason"</span>
                                        <input
                                            class="input input-bordered w-full"
                                            placeholder="Reason for promotion"
                                            prop:value=change_reason_rws.get()
                                            on:input=move |event| {
                                                change_reason_rws.set(event_target_value(&event));
                                            }
                                        />
                                    </label>
                                    <label class="form-control w-full">
                                        <span class="label-text mb-2 font-medium">"Selection mode"</span>
                                        <div class="flex items-center gap-3 h-12 rounded-lg border border-base-300 px-3">
                                            <input
                                                type="checkbox"
                                                class="checkbox checkbox-sm"
                                                prop:checked=move || copy_all_rws.get()
                                                on:change=move |event| {
                                                    copy_all_rws.set(event_target_checked(&event));
                                                }
                                            />
                                            <span>{move || {
                                                if copy_all_rws.get() {
                                                    "Copy all dimensions"
                                                } else {
                                                    "Copy selected rows"
                                                }
                                            }}</span>
                                        </div>
                                    </label>
                                    <label class="form-control w-full">
                                        <span class="label-text mb-2 font-medium">"Existing target rows"</span>
                                        <div class="flex items-center gap-3 h-12 rounded-lg border border-base-300 px-3">
                                            <input
                                                type="checkbox"
                                                class="checkbox checkbox-sm"
                                                prop:checked=move || skip_existing_rws.get()
                                                on:change=move |event| {
                                                    skip_existing_rws.set(event_target_checked(&event));
                                                }
                                            />
                                            <span>{move || {
                                                if skip_existing_rws.get() {
                                                    "Skip existing"
                                                } else {
                                                    "Overwrite existing"
                                                }
                                            }}</span>
                                        </div>
                                    </label>
                                </div>
                                <div class="flex justify-end">
                                    <button
                                        class="btn btn-primary"
                                        prop:disabled=move || {
                                            copy_inprogress_rws.get()
                                                || target_workspace_rws.get().is_empty()
                                                || change_reason_rws.get().trim().is_empty()
                                                || (!copy_all_rws.get()
                                                    && selected_dimensions_rws.get().is_empty())
                                        }
                                        on:click=move |_| on_copy.call(())
                                    >
                                        {move || {
                                            if copy_inprogress_rws.get() {
                                                "Copying..."
                                            } else if copy_all_rws.get() {
                                                "Copy All Dimensions"
                                            } else {
                                                "Copy Selected Dimensions"
                                            }
                                        }}
                                    </button>
                                </div>
                            </div>
                        </div>
                        <div class="card w-full bg-base-100 rounded-xl overflow-hidden shadow">
                            <div class="card-body overflow-y-auto overflow-x-visible">
                                <Table
                                    class="!overflow-y-auto"
                                    rows=table_rows
                                    key_column="dimension"
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
