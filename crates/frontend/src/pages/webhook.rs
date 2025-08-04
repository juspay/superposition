use std::ops::Deref;

use leptos::*;
use leptos_router::{use_navigate, use_params_map};
use serde_json::Value;
use superposition_types::database::models::others::Webhook;

use crate::api::{delete_webhooks, get_webhook};
use crate::components::badge::Badge;
use crate::components::description::ContentDescription;
use crate::components::input::Toggle;
use crate::components::{
    alert::AlertType,
    button::Button,
    drawer::PortalDrawer,
    input::{Input, InputType},
    skeleton::{Skeleton, SkeletonVariant},
    webhook_form::{ChangeLogSummary, ChangeType, WebhookForm},
};
use crate::providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider};
use crate::schema::{JsonSchemaType, SchemaType};
use crate::types::{OrganisationId, Tenant};
use crate::utils::use_url_base;

#[component]
fn webhook_info(webhook: Webhook) -> impl IntoView {
    view! {
        <div class="card bg-base-100 max-w-screen shadow">
            <div class="card-body">
                <h2 class="card-title">"Info"</h2>
                <div class="flex flex-col gap-4">
                    <div class="flex flex-row gap-6 flex-wrap">
                        <div class="h-fit w-[250px] flex gap-4">
                            <div class="stat-title">"Enabled"</div>
                            <div class="stat-value text-base">
                                <Toggle value=webhook.enabled disabled=true on_change=|_| {} />
                            </div>
                        </div>
                        <div class="h-fit w-[250px] flex gap-4">
                            <div class="stat-title">"Payload Version"</div>
                            <div class="stat-value text-base">
                                {webhook.payload_version.to_string()}
                            </div>
                        </div>
                    </div>
                    <div class="flex flex-row gap-6 flex-wrap">
                        <div class="h-fit w-[250px] flex gap-4">
                            <div class="stat-title">"URL"</div>
                            <div class="stat-value text-base">
                                {webhook.url.deref().to_string()}
                            </div>
                        </div>
                        <div class="h-fit w-[250px] flex gap-4">
                            <div class="stat-title">"Method"</div>
                            <div class="stat-value text-base">{webhook.method.to_string()}</div>
                        </div>
                    </div>
                    <div class="flex flex-row gap-6 flex-wrap">
                        <div class="h-fit flex items-center gap-4">
                            <div class="stat-title">"Events"</div>
                            <Badge options=Signal::derive(move || webhook.events.clone()) />
                        </div>
                    </div>
                    <div class="flex flex-row gap-6 flex-wrap">
                        <div class="h-fit w-[250px] flex gap-4">
                            <div class="stat-title">"Max Retries"</div>
                            <div class="stat-value text-base">
                                {webhook.max_retries.to_string()}
                            </div>
                        </div>
                        {webhook
                            .last_triggered_at
                            .map(|last_triggered_at| {
                                view! {
                                    <div class="h-fit w-[250px] flex gap-4">
                                        <div class="stat-title">"Last Triggered At"</div>
                                        <div class="stat-value text-base">
                                            {last_triggered_at.format("%v %T").to_string()}
                                        </div>
                                    </div>
                                }
                            })}
                    </div>
                    <div class="flex gap-4">
                        <div class="stat-title">"Custom Headers"</div>
                        <EditorProvider>
                            <Input
                                disabled=true
                                id="custom-headers"
                                class="rounded-md resize-y w-full max-w-md"
                                schema_type=SchemaType::Single(JsonSchemaType::Object)
                                value=Value::Object(webhook.custom_headers.deref().clone())
                                on_change=move |_| {}
                                r#type=InputType::Monaco(vec![])
                            />
                        </EditorProvider>
                    </div>
                </div>
            </div>
        </div>
    }.into_view()
}

#[derive(Clone)]
enum Action {
    None,
    Edit,
    Delete,
}

#[component]
pub fn webhook() -> impl IntoView {
    let path_params = use_params_map();
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let webhook_name = Memo::new(move |_| {
        path_params
            .with(|params| params.get("webhook_name").cloned().unwrap_or("1".into()))
    });
    let action_rws = RwSignal::new(Action::None);
    let delete_inprogress_rws = RwSignal::new(false);

    let webhook_resource = create_blocking_resource(
        move || (webhook_name.get(), workspace.get().0, org.get().0),
        |(webhook_name, workspace, org_id)| async move {
            get_webhook(&webhook_name, &workspace, &org_id).await.ok()
        },
    );

    let confirm_delete = move |_| {
        delete_inprogress_rws.set(true);
        spawn_local(async move {
            let result = delete_webhooks(
                webhook_name.get_untracked(),
                workspace.get_untracked().0,
                org.get_untracked().0,
            )
            .await;
            delete_inprogress_rws.set(false);
            match result {
                Ok(_) => {
                    logging::log!("Webhook deleted successfully");
                    let navigate = use_navigate();
                    let base = use_url_base();
                    let redirect_url = format!(
                        "{base}/admin/{}/{}/webhooks",
                        org.get().0,
                        workspace.get().0,
                    );
                    navigate(&redirect_url, Default::default());
                    enqueue_alert(
                        String::from("Webhook deleted successfully"),
                        AlertType::Success,
                        5000,
                    );
                }
                Err(e) => {
                    logging::error!("Error deleting webhook: {:?}", e);
                    enqueue_alert(e, AlertType::Error, 5000);
                }
            }
        });
    };

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let webhook = match webhook_resource.get() {
                    Some(Some(config)) => config,
                    _ => return view! { <h1>"Error fetching webhook"</h1> }.into_view(),
                };
                let webhook_st = StoredValue::new(webhook.clone());
                view! {
                    <div class="flex flex-col gap-4">
                        <div class="flex justify-between items-center">
                            <h1 class="text-2xl font-extrabold">{webhook.name.clone()}</h1>
                            <div class="flex flex-row join">
                                <Button
                                    force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                    on_click=move |_| action_rws.set(Action::Edit)
                                    icon_class="ri-edit-line"
                                    text="Edit"
                                />
                                <Button
                                    force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                    on_click=move |_| action_rws.set(Action::Delete)
                                    icon_class="ri-delete-bin-line"
                                    text="Delete"
                                />
                            </div>
                        </div>
                        <ContentDescription
                            description=webhook.description.clone()
                            change_reason=webhook.change_reason.clone()
                            created_by=webhook.created_by.clone()
                            created_at=webhook.created_at
                            last_modified_by=webhook.last_modified_by.clone()
                            last_modified_at=webhook.last_modified_at
                        />
                        <WebhookInfo webhook=webhook.clone() />
                    </div>
                    {match action_rws.get() {
                        Action::None => ().into_view(),
                        Action::Edit => {
                            view! {
                                <PortalDrawer
                                    title="Edit Webhook"
                                    handle_close=move |_| action_rws.set(Action::None)
                                >
                                    <WebhookForm
                                        edit=true
                                        webhook_name=webhook_st.with_value(|w| w.name.clone())
                                        description=webhook_st
                                            .with_value(|w| w.description.deref().to_string())
                                        enabled=webhook_st.with_value(|w| w.enabled)
                                        url=webhook_st.with_value(|w| w.url.deref().to_string())
                                        method=webhook_st.with_value(|w| w.method)
                                        payload_version=webhook_st.with_value(|w| w.payload_version)
                                        custom_headers=webhook_st
                                            .with_value(|w| w.custom_headers.clone())
                                        events=webhook_st.with_value(|w| w.events.clone())
                                        handle_submit=move |_| {
                                            webhook_resource.refetch();
                                            action_rws.set(Action::None);
                                        }
                                    />
                                </PortalDrawer>
                            }
                                .into_view()
                        }
                        Action::Delete => {
                            view! {
                                <ChangeLogSummary
                                    webhook_name=webhook_name.get()
                                    change_type=ChangeType::Delete
                                    on_close=move |_| action_rws.set(Action::None)
                                    on_confirm=confirm_delete
                                    inprogress=delete_inprogress_rws
                                />
                            }
                                .into_view()
                        }
                    }}
                }
                    .into_view()
            }}
        </Suspense>
    }
}
