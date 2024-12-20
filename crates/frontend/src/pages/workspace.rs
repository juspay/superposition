use leptos::*;
use leptos_router::use_navigate;
use serde_json::{json, Map, Value};
use superposition_types::custom_query::PaginationParams;

use crate::api::fetch_workspaces;
use crate::components::drawer::{close_drawer, open_drawer, Drawer, DrawerBtn};
use crate::components::skeleton::Skeleton;
use crate::components::stat::Stat;
use crate::components::table::types::ColumnSortable;
use crate::components::table::{
    types::{Column, TablePaginationProps},
    Table,
};
use crate::components::workspace_form::types::RowData;
use crate::components::workspace_form::types::WorkspaceStatus;
use crate::components::workspace_form::WorkspaceForm;
use crate::utils::update_page_direction;

#[component]
pub fn workspace() -> impl IntoView {
    let (filters, set_filters) = create_signal(PaginationParams::default_request());
    let workspace_resource = create_blocking_resource(
        move || (filters.get()),
        |filters: PaginationParams| async move {
            fetch_workspaces(&filters).await.unwrap_or_default()
        },
    );
    let selected_workspace = create_rw_signal::<Option<RowData>>(None);

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

            let edit_click_handler = move |_| {
                let row_data = RowData {
                    workspace_name: workspace_name.clone(),
                    workspace_schema_name: workspace_schema_name.clone(),
                    workspace_status: workspace_status.clone(),
                    workspace_admin_email: workspace_admin_email.clone(),
                    mandatory_dimensions: Some(mandatory_dimensions.clone()),
                    created_by: created_by.clone(),
                    created_at: created_at.clone(),
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

        let navigate_to_workspace = move |workspace_name: String| {
            let redirect_url = format!("admin/{workspace_name}/default-config");
            logging::log!("redirecting to {:?}", redirect_url.clone());
            let navigate = use_navigate();
            navigate(redirect_url.as_str(), Default::default());
        };

        let navigate = move |_: &str, row: &Map<String, Value>| {
            let workspace_schema_name =
                row["workspace_schema_name"].to_string().replace('"', "");
            let workspace_name = row["workspace_name"].to_string().replace('"', "");
            view! {
                <span
                    class="cursor-pointer text-blue-500"
                    on:click=move |_| { navigate_to_workspace(workspace_schema_name.clone()) }
                >

                    {workspace_name}
                </span>
            }
            .into_view()
        };

        vec![
            Column::new(
                "workspace_name".to_string(),
                None,
                navigate,
                ColumnSortable::No,
            ),
            Column::default("workspace_admin_email".to_string()),
            Column::default("mandatory_dimensions".to_string()),
            Column::default("created_by".to_string()),
            Column::default("created_at".to_string()),
            Column::new(
                "actions".to_string(),
                None,
                actions_col_formatter,
                ColumnSortable::No,
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
                        view! {
                            <Drawer
                                id="workspace_drawer".to_string()
                                header="Edit Workspace"
                                handle_close=handle_close
                            >
                                <WorkspaceForm
                                    edit=true
                                    workspace_admin_email=selected_workspace_data
                                        .workspace_admin_email
                                    workspace_name=selected_workspace_data.workspace_name
                                    workspace_status=selected_workspace_data.workspace_status
                                    mandatory_dimensions=selected_workspace_data
                                        .mandatory_dimensions
                                        .unwrap_or_default()
                                    handle_submit=move || {
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
                                <WorkspaceForm handle_submit=move || {
                                    workspace_resource.refetch();
                                    selected_workspace.set(None);
                                    close_drawer("workspace_drawer");
                                } />

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
                                    json!(workspace.created_at.format("%v").to_string()),
                                );
                            ele_map
                        })
                        .collect::<Vec<Map<String, Value>>>();
                    let total_workspaces = workspaces.total_items.to_string();
                    let filters = filters.get();
                    let (current_page, total_pages) = (
                        filters.page.unwrap_or_default(),
                        workspaces.total_pages,
                    );
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
                                    cell_class="min-w-48 font-mono".to_string()
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
