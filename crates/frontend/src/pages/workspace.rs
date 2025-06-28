use leptos::*;
use leptos_router::A;
use serde_json::{json, Map, Value};
use superposition_macros::box_params;
use superposition_types::{
    custom_query::{CustomQuery, PaginationParams, Query},
    database::models::{Metrics, WorkspaceStatus},
};

use crate::components::{
    drawer::{close_drawer, open_drawer, Drawer, DrawerBtn},
    skeleton::Skeleton,
    stat::Stat,
    table::{
        types::{Column, ColumnSortable, Expandable, TablePaginationProps},
        Table,
    },
    workspace_form::{types::RowData, WorkspaceForm},
};
use crate::query_updater::{use_param_updater, use_signal_from_query};
use crate::types::OrganisationId;
use crate::{api::fetch_workspaces, components::table::types::default_column_formatter};

#[component]
pub fn workspace() -> impl IntoView {
    let org_id = use_context::<Signal<OrganisationId>>().unwrap();
    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
    });

    use_param_updater(move || box_params!(pagination_params_rws.get()));

    let workspace_resource = create_blocking_resource(
        move || (pagination_params_rws.get(), org_id.get().0),
        |(pagination_params, org_id)| async move {
            fetch_workspaces(&pagination_params, &org_id)
                .await
                .unwrap_or_default()
        },
    );
    let selected_workspace = create_rw_signal::<Option<RowData>>(None);

    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    });

    let handle_close = move || {
        selected_workspace.set(None);
        close_drawer("workspace_drawer");
    };

    let table_columns = create_memo(move |_| {
        let actions_col_formatter = move |_: &str, row: &Map<String, Value>| {
            let workspace_name = row["workspace_name"].to_string().replace('"', "");
            let workspace_schema_name =
                row["workspace_schema_name"].to_string().replace('"', "");
            let workspace_status = row["workspace_status"]
                .to_string()
                .replace('"', "")
                .parse::<WorkspaceStatus>()
                .unwrap();
            let workspace_admin_email =
                row["workspace_admin_email"].to_string().replace('"', "");
            let config_version = row["config_version"].as_str().map(|s| s.to_string());
            let mandatory_dimensions = match row["mandatory_dimensions"].clone() {
                Value::Array(arr) => arr
                    .into_iter()
                    .filter_map(|v| match v {
                        Value::String(s) => Some(s),
                        _ => None,
                    })
                    .collect(),
                _ => Vec::new(),
            };
            let created_by = row["created_by"].to_string().to_string().replace('"', "");
            let created_at = row["created_at"].to_string().replace('"', "");
            let metrics: Metrics =
                serde_json::from_value(row["metrics"].clone()).unwrap_or_default();

            let allow_experiment_self_approval = row["allow_experiment_self_approval"]
                .as_bool()
                .unwrap_or_default();

            let edit_click_handler = move |_| {
                let row_data = RowData {
                    workspace_name: workspace_name.clone(),
                    workspace_schema_name: workspace_schema_name.clone(),
                    workspace_status,
                    workspace_admin_email: workspace_admin_email.clone(),
                    config_version: config_version
                        .clone()
                        .map_or(String::from("-"), |x| x.to_string()),
                    mandatory_dimensions: Some(mandatory_dimensions.clone()),
                    created_by: created_by.clone(),
                    created_at: created_at.clone(),
                    metrics: metrics.clone(),
                    allow_experiment_self_approval,
                };
                logging::log!("{:?}", row_data);
                selected_workspace.set(Some(row_data));
                open_drawer("workspace_drawer");
            };
            view! {
                <div class="join">
                    <span class="cursor-pointer" on:click=edit_click_handler>
                        <i class="ri-pencil-line ri-xl text-blue-500"></i>
                    </span>
                </div>
            }
            .into_view()
        };

        let navigate = move |_: &str, row: &Map<String, Value>| {
            let org_id = org_id.get().0;
            let workspace_name = row["workspace_name"].to_string().replace('"', "");

            view! {
                <A
                    class="cursor-pointer text-blue-500"
                    href=format!("/admin/{org_id}/{workspace_name}/default-config")
                >
                    {workspace_name}
                </A>
            }
            .into_view()
        };

        vec![
            Column::new(
                "workspace_name".to_string(),
                false,
                navigate,
                ColumnSortable::No,
                Expandable::Disabled,
                default_column_formatter,
            ),
            Column::default("workspace_admin_email".to_string()),
            Column::default("config_version".to_string()),
            Column::default("mandatory_dimensions".to_string()),
            Column::default("strict_mode".to_string()),
            Column::default("created_by".to_string()),
            Column::default("created_at".to_string()),
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

    view! {
        <div class="p-8">
            <Suspense fallback=move || {
                view! { <Skeleton /> }
            }>
                {move || {
                    if let Some(selected_workspace_data) = selected_workspace.get() {
                        let config_version = if selected_workspace_data.config_version == "-" {
                            Value::Null
                        } else {
                            Value::String(selected_workspace_data.config_version.clone())
                        };
                        view! {
                            <Drawer
                                id="workspace_drawer".to_string()
                                header="Edit Workspace"
                                handle_close=handle_close
                            >
                                <WorkspaceForm
                                    edit=true
                                    org_id=org_id
                                    workspace_admin_email=selected_workspace_data
                                        .workspace_admin_email
                                    config_version
                                    workspace_name=selected_workspace_data.workspace_name
                                    workspace_status=selected_workspace_data.workspace_status
                                    mandatory_dimensions=selected_workspace_data
                                        .mandatory_dimensions
                                        .unwrap_or_default()
                                    metrics=selected_workspace_data.metrics
                                    allow_experiment_self_approval=selected_workspace_data
                                        .allow_experiment_self_approval
                                    handle_submit=move |_| {
                                        workspace_resource.refetch();
                                        selected_workspace.set(None);
                                        close_drawer("workspace_drawer");
                                    }
                                />
                            </Drawer>
                        }
                    } else {
                        view! {
                            <Drawer
                                id="workspace_drawer".to_string()
                                header="Create Workspace"
                                handle_close=handle_close
                            >
                                <WorkspaceForm
                                    org_id=org_id
                                    handle_submit=move |_| {
                                        pagination_params_rws.update(|f| f.reset_page());
                                        workspace_resource.refetch();
                                        selected_workspace.set(None);
                                        close_drawer("workspace_drawer");
                                    }
                                />
                            </Drawer>
                        }
                    }
                }}
                {move || {
                    let workspaces = workspace_resource.get().unwrap_or_default();
                    let table_rows = workspaces
                        .data
                        .into_iter()
                        .map(|workspace| {
                            let mut ele_map = json!(workspace).as_object().unwrap().to_owned();
                            ele_map
                                .insert(
                                    "created_at".to_string(),
                                    json!(workspace.created_at.format("%v %T").to_string()),
                                );
                            ele_map
                        })
                        .collect::<Vec<Map<String, Value>>>();
                    let total_workspaces = workspaces.total_items.to_string();
                    let pagination_params = pagination_params_rws.get();
                    let (current_page, total_pages) = (
                        pagination_params.page.unwrap_or_default(),
                        workspaces.total_pages,
                    );
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
                                heading="Workspaces"
                                icon="ri-briefcase-fill"
                                number=total_workspaces
                            />
                        </div>
                        <div class="card rounded-lg w-full bg-base-100 shadow">
                            <div class="card-body">
                                <div class="flex justify-end pb-2">
                                    <div class="flex">
                                        <DrawerBtn drawer_id="workspace_drawer"
                                            .to_string()>
                                            Create Workspace <i class="ri-edit-2-line ml-2"></i>
                                        </DrawerBtn>
                                    </div>
                                </div>
                                <Table
                                    rows=table_rows
                                    key_column="id".to_string()
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
