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
        delete_modal::DeleteModal,
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
        webhook_form::WebhookForm,
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
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let (delete_modal_visible_rs, delete_modal_visible_ws) = create_signal(false);
    let (delete_id_rs, delete_id_ws) = create_signal::<Option<String>>(None);
    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
    });

    use_param_updater(move || box_params!(pagination_params_rws.get()));

    let webhooks_resource = create_blocking_resource(
        move || {
            (
                tenant_rws.get().0,
                pagination_params_rws.get(),
                org_rws.get().0,
            )
        },
        |(current_tenant, pagination_params, org_id)| async move {
            fetch_webhooks(&pagination_params, current_tenant, org_id)
                .await
                .unwrap_or_default()
        },
    );

    let confirm_delete = Callback::new(move |_| {
        if let Some(id) = delete_id_rs.get().clone() {
            spawn_local(async move {
                let result =
                    delete_webhooks(id, tenant_rws.get().0, org_rws.get().0).await;

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
        delete_modal_visible_ws.set(false);
    });
    let handle_next_click = Callback::new(move |next_page: i64| {
        pagination_params_rws.update(|f| f.page = Some(next_page));
    });

    let handle_prev_click = Callback::new(move |prev_page: i64| {
        pagination_params_rws.update(|f| f.page = Some(prev_page));
    });

    let selected_webhook = create_rw_signal::<Option<RowData>>(None);

    let table_columns = create_memo(move |_| {
        let action_col_formatter = move |_: &str, row: &Map<String, Value>| {
            let row_name = row["name"].to_string().replace('"', "");

            let row_description = row["description"].to_string().replace('"', "");

            let enabled = row["enabled"].as_bool().unwrap_or(false);

            let url = row["url"].to_string().replace('"', "");

            let method = row["method"].to_string().replace('"', "");
            let method = serde_json::from_str::<HttpMethod>(&method).unwrap_or_default();

            let payload_version = row["payload_version"].to_string().replace('"', "");
            let payload_version =
                serde_json::from_str::<PayloadVersion>(&payload_version)
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
                    serde_json::from_str::<WebhookEvent>(&event.to_string()).ok()
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

            let handle_webhook_delete = move |_| {
                delete_id_ws.set(Some(webhook_name.clone()));
                delete_modal_visible_ws.set(true);
            };
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
            Column::new(
                "actions".to_string(),
                false,
                action_col_formatter,
                ColumnSortable::No,
                Expandable::Enabled(100),
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
                    let handle_close = move || {
                        close_drawer("webhook_drawer");
                        selected_webhook.set(None);
                    };
                    if let Some(selected_webhook_data) = selected_webhook.get() {
                        view! {
                            <Drawer
                                id="webhook_drawer".to_string()
                                header="Edit Webhook"
                                handle_close=handle_close
                            >
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
                                    handle_submit=move || {
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
                                id="webhook_drawer".to_string()
                                header="Create New Webhook"
                                handle_close=handle_close
                            >
                                <WebhookForm handle_submit=move || {
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
                        })
                        .collect::<Vec<Map<String, Value>>>();
                    let pagination_params = pagination_params_rws.get();
                    let pagination_props = TablePaginationProps {
                        enabled: true,
                        count: pagination_params.count.unwrap_or_default(),
                        current_page: pagination_params.page.unwrap_or_default(),
                        total_pages: value.total_pages,
                        on_next: handle_next_click,
                        on_prev: handle_prev_click,
                    };
                    view! {
                        <div class="pb-4">
                            <Stat heading="Webhooks" icon="ri-webhook-fill" number=total_items />
                        </div>
                        <div class="card rounded-xl w-full bg-base-100 shadow">
                            <div class="card-body">
                                <div class="flex justify-between">
                                    <h2 class="card-title chat-bubble text-gray-800 dark:text-white bg-white">
                                        "Webhooks"
                                    </h2>
                                    <DrawerBtn drawer_id="webhook_drawer"
                                        .to_string()>
                                        Create Webhook <i class="ri-edit-2-line ml-2"></i>
                                    </DrawerBtn>
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
                <DeleteModal
                    modal_visible=delete_modal_visible_rs
                    confirm_delete=confirm_delete
                    set_modal_visible=delete_modal_visible_ws
                    header_text="Are you sure you want to delete this Webhook? Action is irreversible."
                        .to_string()
                />
            </Suspense>
        </div>
    }
}
