use leptos::*;
use leptos_router::A;
use serde_json::{Map, Value, json};
use superposition_types::{
    custom_query::{CustomQuery, PaginationParams, Query},
    database::models::others::WebhookEvent,
};

use crate::{
    api::fetch_webhooks,
    components::{
        badge::Badge,
        button::Button,
        datetime::DatetimeStr,
        drawer::PortalDrawer,
        input::Toggle,
        skeleton::Skeleton,
        stat::Stat,
        table::{
            Table,
            types::{
                Column, ColumnSortable, Expandable, TablePaginationProps,
                default_column_formatter,
            },
        },
        webhook_form::WebhookForm,
    },
    query_updater::use_signal_from_query,
    types::{OrganisationId, Workspace},
};

#[derive(Clone)]
enum Action {
    Create,
    None,
}

#[component]
pub fn Webhooks() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let action_rws = RwSignal::new(Action::None);
    let (pagination_params_rws,) = use_signal_from_query(move |query_string| {
        (Query::<PaginationParams>::extract_non_empty(query_string).into_inner(),)
    });

    let webhooks_resource = create_blocking_resource(
        move || (workspace.get().0, pagination_params_rws.get(), org.get().0),
        |(workspace, pagination_params, org_id)| async move {
            fetch_webhooks(&pagination_params, &workspace, &org_id)
                .await
                .unwrap_or_default()
        },
    );

    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    });

    let table_columns = StoredValue::new(vec![
        Column::new(
            "name".to_string(),
            false,
            move |webhook_name: &str, _row: &Map<String, Value>| {
                let webhook_name = webhook_name.to_string();
                view! {
                    <A href=webhook_name.clone() class="text-blue-500 underline underline-offset-2">
                        {webhook_name}
                    </A>
                }
            },
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::default_with_cell_formatter(
            "enabled".to_string(),
            |_, row: &Map<String, Value>| {
                let value = row
                    .get("enabled")
                    .and_then(Value::as_bool)
                    .unwrap_or_default();

                view! {
                    <Toggle value disabled=true on_change=|_| () />
                }
            },
        ),
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

                view! {
                    <Badge options=Signal::derive(move || events.clone()) />
                }
            },
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::default("last_triggered_at".to_string()),
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
    ]);

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
            {move || {
                let value = webhooks_resource.get().unwrap_or_default();
                let total_items = value.total_items.to_string();
                let table_rows = value
                    .data
                    .iter()
                    .map(|ele| json!(ele).as_object().unwrap().clone())
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
                            <Button
                                class="self-end"
                                text="Create Webhook"
                                icon_class="ri-add-line"
                                on_click=move |_| action_rws.set(Action::Create)
                            />
                        </div>
                        <div class="card w-full bg-base-100 rounded-xl overflow-hidden shadow">
                            <div class="card-body overflow-y-auto overflow-x-visible">
                                <Table
                                    class="!overflow-y-auto"
                                    rows=table_rows
                                    key_column="name"
                                    columns=table_columns.get_value()
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
                            title="Create New Webhook"
                            handle_close=move |_| action_rws.set(Action::None)
                        >
                            <WebhookForm handle_submit=move |_| {
                                pagination_params_rws.update(|f| f.reset_page());
                                webhooks_resource.refetch();
                                action_rws.set(Action::None);
                            } />
                        </PortalDrawer>
                    }
                        .into_view()
                }
                Action::None => view! {}.into_view(),
            }}
        </Suspense>
    }
}
