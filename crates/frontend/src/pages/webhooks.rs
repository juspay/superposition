use leptos::*;
use serde_json::{json, Map, Value};
use superposition_macros::box_params;
use superposition_types::{
    custom_query::{CustomQuery, PaginationParams, Query},
    database::models::others::{CustomHeaders, HttpMethod, PayloadVersion, WebhookEvent},
};

use crate::{
    api::{delete_webhooks, fetch_webhooks},
    components::{
        badge::Badge,
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
        webhook_form::{ChangeLogSummary, ChangeType, WebhookForm},
    },
    query_updater::{use_param_updater, use_signal_from_query},
    types::{OrganisationId, Tenant},
};

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub name: String,
    pub description: String,
    pub enabled: bool,
    pub url: String,
    pub method: HttpMethod,
    pub payload_version: PayloadVersion,
    pub custom_headers: CustomHeaders,
    pub events: Vec<WebhookEvent>,
}

#[component]
pub fn webhooks() -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let (delete_id_rs, delete_id_ws) = create_signal::<Option<String>>(None);
    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
    });

    use_param_updater(move || box_params!(pagination_params_rws.get()));

    let webhooks_resource = create_blocking_resource(
        move || (workspace.get().0, pagination_params_rws.get(), org.get().0),
        |(current_tenant, pagination_params, org_id)| async move {
            fetch_webhooks(&pagination_params, current_tenant, org_id)
                .await
                .unwrap_or_default()
        },
    );

    let confirm_delete = Callback::new(move |_| {
        if let Some(id) = delete_id_rs.get().clone() {
            spawn_local(async move {
                let result = delete_webhooks(id, workspace.get().0, org.get().0).await;

                match result {
                    Ok(_) => {
                        logging::log!("Webhook deleted successfully");
                        webhooks_resource.refetch();
                    }
                    Err(e) => {
                        logging::log!("Error deleting Webhook: {:?}", e);
                    }
                }
            });
        }
        delete_id_ws.set(None);
    });
    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    });

    let selected_webhook = create_rw_signal::<Option<RowData>>(None);

    let table_columns = create_memo(move |_| {
        let action_col_formatter = move |_: &str, row: &Map<String, Value>| {
            let row_name = row["name"].as_str().map(String::from).unwrap_or_default();

            let row_description = row["description"]
                .as_str()
                .map(String::from)
                .unwrap_or_default();

            let enabled = row["enabled"].as_bool().unwrap_or_default();

            let url = row["url"].as_str().map(String::from).unwrap_or_default();

            let method = serde_json::from_value::<HttpMethod>(row["method"].clone())
                .unwrap_or_default();

            let payload_version =
                serde_json::from_value::<PayloadVersion>(row["payload_version"].clone())
                    .unwrap_or_default();

            let custom_headers = row
                .get("custom_headers")
                .and_then(|value| {
                    serde_json::from_value::<CustomHeaders>(value.clone()).ok()
                })
                .unwrap_or_default();

            let events = row["events"]
                .as_array()
                .unwrap_or(&vec![])
                .iter()
                .filter_map(|event| {
                    serde_json::from_value::<WebhookEvent>(event.clone()).ok()
                })
                .collect::<Vec<WebhookEvent>>();

            let webhook_name = row_name.clone();

            let edit_click_handler = move |_| {
                let row_data = RowData {
                    name: row_name.clone(),
                    description: row_description.clone(),
                    enabled,
                    url: url.clone(),
                    method,
                    payload_version,
                    custom_headers: custom_headers.clone(),
                    events: events.clone(),
                };
                logging::log!("{:?}", row_data);
                selected_webhook.set(Some(row_data));
                open_drawer("webhook_drawer");
            };

            let handle_webhook_delete =
                move |_| delete_id_ws.set(Some(webhook_name.clone()));

            view! {
                <div class="join">
                    <span class="cursor-pointer" on:click=edit_click_handler>
                        <i class="ri-pencil-line ri-xl text-blue-500"></i>
                    </span>
                    <span class="cursor-pointer text-red-500" on:click=handle_webhook_delete>
                        <i class="ri-delete-bin-5-line ri-xl text-red-500"></i>
                    </span>
                </div>
            }
            .into_view()
        };
        let expand = move |webhook_name: &str, row: &Map<String, Value>| {
            let webhook_name = webhook_name.to_string();

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

            view! {
                <span class="mr-2">{webhook_name}</span>
                <InfoDescription description=description change_reason=change_reason />
            }
            .into_view()
        };
        vec![
            Column::new(
                "name".to_string(),
                false,
                expand,
                ColumnSortable::No,
                Expandable::Disabled,
                default_column_formatter,
            ),
            Column::default("enabled".to_string()),
            Column::default("url".to_string()),
            Column::default("method".to_string()),
            Column::default("payload_version".to_string()),
            Column::default("custom_headers".to_string()),
            Column::new(
                "events".to_string(),
                false,
                move |_, row: &Map<String, Value>| {
                    let events = row
                        .get("events")
                        .and_then(|v| {
                            serde_json::from_value::<Vec<WebhookEvent>>(v.clone()).ok()
                        })
                        .unwrap_or_default();
                    let options = RwSignal::new(events);

                    view! {
                        <Badge options />
                    }
                },
                ColumnSortable::No,
                Expandable::Disabled,
                default_column_formatter,
            ),
            Column::default("max_retries".to_string()),
            Column::default("last_triggered_at".to_string()),
            Column::default("created_by".to_string()),
            Column::default("created_at".to_string()),
            Column::default("last_modified_by".to_string()),
            Column::default("last_modified_at".to_string()),
            Column::default_with_cell_formatter(
                "actions".to_string(),
                action_col_formatter,
            ),
        ]
    });

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
            {move || {
                let value = webhooks_resource.get().unwrap_or_default();
                let total_items = value.data.len().to_string();
                let table_rows = value
                    .data
                    .iter()
                    .map(|ele| {
                        let mut ele_map = json!(ele).as_object().unwrap().clone();
                        ele_map
                            .insert(
                                "created_at".to_string(),
                                json!(ele.created_at.format("%v %T").to_string()),
                            );
                        ele_map
                            .insert(
                                "last_modified_at".to_string(),
                                json!(ele.last_modified_at.format("%v %T").to_string()),
                            );
                        ele_map
                    })
                    .collect::<Vec<Map<String, Value>>>();
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
                            <Stat heading="Webhooks" icon="ri-webhook-fill" number=total_items />
                            <DrawerBtn drawer_id="webhook_drawer" class="self-end flex gap-2">
                                Create Webhook
                                <i class="ri-edit-2-line" />
                            </DrawerBtn>
                        </div>
                        <div class="card w-full bg-base-100 rounded-xl overflow-hidden shadow">
                            <div class="card-body overflow-y-auto overflow-x-visible">
                                <Table
                                    class="!overflow-y-auto"
                                    rows=table_rows
                                    key_column="id".to_string()
                                    columns=table_columns.get()
                                    pagination=pagination_props
                                />
                            </div>
                        </div>
                    </div>
                }
            }}
            {move || {
                let handle_close = move || {
                    close_drawer("webhook_drawer");
                    selected_webhook.set(None);
                };
                if let Some(selected_webhook_data) = selected_webhook.get() {
                    view! {
                        <Drawer id="webhook_drawer" header="Edit Webhook" handle_close=handle_close>
                            <WebhookForm
                                edit=true
                                webhook_name=selected_webhook_data.name
                                description=selected_webhook_data.description
                                enabled=selected_webhook_data.enabled
                                url=selected_webhook_data.url
                                method=selected_webhook_data.method
                                payload_version=selected_webhook_data.payload_version
                                custom_headers=selected_webhook_data.custom_headers
                                events=selected_webhook_data.events
                                handle_submit=move |_| {
                                    webhooks_resource.refetch();
                                    selected_webhook.set(None);
                                    close_drawer("webhook_drawer");
                                }
                            />

                        </Drawer>
                    }
                } else {
                    view! {
                        <Drawer
                            id="webhook_drawer"
                            header="Create New Webhook"
                            handle_close=handle_close
                        >
                            <WebhookForm handle_submit=move |_| {
                                pagination_params_rws.update(|f| f.reset_page());
                                webhooks_resource.refetch();
                                selected_webhook.set(None);
                                close_drawer("webhook_drawer");
                            } />
                        </Drawer>
                    }
                }
            }}
            {move || {
                if let Some(webhook_name) = delete_id_rs.get() {
                    view! {
                        <ChangeLogSummary
                            webhook_name
                            change_type=ChangeType::Delete
                            on_close=move |_| delete_id_ws.set(None)
                            on_confirm=confirm_delete
                        />
                    }
                } else {
                    ().into_view()
                }
            }}
        </Suspense>
    }
}
