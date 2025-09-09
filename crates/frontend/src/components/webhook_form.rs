use std::ops::Deref;

use leptos::*;
use serde_json::{Map, Value};
use strum::IntoEnumIterator;
use superposition_types::{
    api::webhook::UpdateWebhookRequest,
    database::models::{
        others::{CustomHeaders, HttpMethod, PayloadVersion, WebhookEvent},
        ChangeReason, Description, NonEmptyString,
    },
};
use web_sys::MouseEvent;

use crate::{
    api::{create_webhook, get_webhook, update_webhook},
    components::{
        alert::AlertType,
        button::Button,
        change_form::ChangeForm,
        change_summary::{ChangeLogPopup, ChangeSummary, JsonChangeSummary},
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        form::label::Label,
        input::{Input, InputType, Toggle},
        skeleton::{Skeleton, SkeletonVariant},
    },
    providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider},
    schema::{JsonSchemaType, SchemaType::Single},
    types::{OrganisationId, Tenant},
};

enum ResponseType {
    UpdatePrecheck,
    Response,
}

#[allow(clippy::too_many_arguments)]
fn try_update_payload(
    enabled: bool,
    url: String,
    method: HttpMethod,
    payload_version: PayloadVersion,
    custom_headers: CustomHeaders,
    events: Vec<WebhookEvent>,
    description: String,
    change_reason: String,
) -> Result<UpdateWebhookRequest, String> {
    Ok(UpdateWebhookRequest {
        enabled: Some(enabled),
        url: Some(NonEmptyString::try_from(url)?),
        method: Some(method),
        payload_version: Some(payload_version),
        custom_headers: Some(custom_headers),
        events: Some(events),
        description: Some(Description::try_from(description)?),
        change_reason: ChangeReason::try_from(change_reason)?,
    })
}

#[component]
pub fn webhook_form(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] webhook_name: String,
    #[prop(default = String::new())] description: String,
    #[prop(default = false)] enabled: bool,
    #[prop(default = String::new())] url: String,
    #[prop(default = HttpMethod::default())] method: HttpMethod,
    #[prop(default = PayloadVersion::default())] payload_version: PayloadVersion,
    #[prop(default = CustomHeaders::default())] custom_headers: CustomHeaders,
    #[prop(default = Vec::new())] events: Vec<WebhookEvent>,
    #[prop(into)] handle_submit: Callback<()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let (webhook_name_rs, webhook_name_ws) = create_signal(webhook_name);
    let (description_rs, description_ws) = create_signal(description);
    let (enabled_rs, enabled_ws) = create_signal(enabled);
    let (url_rs, url_ws) = create_signal(url);
    let (method_rs, method_ws) = create_signal(method);
    let (payload_version_rs, payload_version_ws) = create_signal(payload_version);
    let (custom_headers_rs, custom_headers_ws) = create_signal(custom_headers);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());
    let (events_rs, events_ws) = create_signal(events);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let update_request_rws = RwSignal::new(None);

    let handle_select_webhook_event_dropdown_option =
        Callback::new(move |selected_event: WebhookEvent| {
            events_ws.update(|value| value.push(selected_event));
        });

    let handle_remove_webhook_event_dropdown_option =
        Callback::new(move |selected_event: WebhookEvent| {
            events_ws.update(|value| value.retain(|d| d != &selected_event));
        });

    let on_submit = move || {
        req_inprogress_ws.set(true);

        let webhook_name = webhook_name_rs.get_untracked();
        let description = description_rs.get_untracked();
        let enabled = enabled_rs.get_untracked();
        let url = url_rs.get_untracked();
        let method = method_rs.get_untracked();
        let payload_version = payload_version_rs.get_untracked();
        let custom_headers = custom_headers_rs.get_untracked();
        let change_reason = change_reason_rs.get_untracked();
        let events = events_rs.get_untracked();
        let workspace = workspace.get_untracked().0;
        let org_id = org.get_untracked().0;

        spawn_local({
            async move {
                let result = match (edit, update_request_rws.get_untracked()) {
                    (true, Some((_, update_request))) => {
                        let future = update_webhook(
                            webhook_name,
                            update_request,
                            workspace,
                            org_id,
                        );
                        update_request_rws.set(None);
                        future.await.map(|_| ResponseType::Response)
                    }
                    (true, None) => {
                        let request_payload = try_update_payload(
                            enabled,
                            url,
                            method,
                            payload_version,
                            custom_headers,
                            events,
                            description,
                            change_reason,
                        );
                        match request_payload {
                            Ok(payload) => {
                                update_request_rws.set(Some((webhook_name, payload)));
                                Ok(ResponseType::UpdatePrecheck)
                            }
                            Err(e) => Err(e),
                        }
                    }
                    _ => create_webhook(
                        webhook_name,
                        description,
                        enabled,
                        url,
                        method,
                        payload_version,
                        custom_headers,
                        events,
                        change_reason,
                        workspace,
                        org_id,
                    )
                    .await
                    .map(|_| ResponseType::Response),
                };

                req_inprogress_ws.set(false);
                match result {
                    Ok(ResponseType::UpdatePrecheck) => (),
                    Ok(ResponseType::Response) => {
                        handle_submit.call(());
                        let success_message = if edit {
                            "Webhook updated successfully!"
                        } else {
                            "New Webhook created successfully!"
                        };
                        enqueue_alert(
                            String::from(success_message),
                            AlertType::Success,
                            5000,
                        );
                    }
                    Err(e) => {
                        enqueue_alert(e, AlertType::Error, 5000);
                    }
                }
            }
        });
    };

    let method_options = HttpMethod::iter().collect::<Vec<HttpMethod>>();
    let payload_version_options = PayloadVersion::iter().collect::<Vec<PayloadVersion>>();
    let events_options = WebhookEvent::iter().collect::<Vec<WebhookEvent>>();

    view! {
        <form class="w-full flex flex-col gap-5 bg-white text-gray-700">
            <div class="form-control">
                <Label
                    title="Webhook Name"
                    description="The name of the webhook. It should be unique within the workspace."
                />
                <Input
                    disabled=edit
                    r#type=InputType::Text
                    placeholder="Webhook Name"
                    class="input input-bordered w-full max-w-md"
                    value=Value::String(webhook_name_rs.get_untracked())
                    schema_type=Single(JsonSchemaType::String)
                    on_change=Callback::new(move |value: Value| {
                        webhook_name_ws.set(value.to_string().replace('"', ""));
                    })
                />
            </div>

            <ChangeForm
                title="Description".to_string()
                placeholder="Enter a description".to_string()
                value=description_rs.get_untracked()
                on_change=move |new_description| description_ws.set(new_description)
            />
            <ChangeForm
                title="Reason for Change".to_string()
                placeholder="Enter a reason for this change".to_string()
                value=change_reason_rs.get_untracked()
                on_change=move |new_change_reason| change_reason_ws.set(new_change_reason)
            />
            <div class="w-fit flex items-center gap-2">
                <Toggle
                    name="Enable Webhook"
                    value=enabled_rs.get_untracked()
                    on_change=move |v| enabled_ws.set(v)
                />
                <Label title="Enable Webhook" />
            </div>

            <div class="form-control">
                <Label title="URL" description="The URL to which the webhook will send requests." />
                <Input
                    placeholder="Enter the webhook URL"
                    class="textarea textarea-bordered w-full max-w-md"
                    value=Value::String(url_rs.get_untracked())
                    schema_type=Single(JsonSchemaType::String)
                    r#type=InputType::Text
                    on_change=Callback::new(move |value: Value| {
                        url_ws.set(value.to_string().replace('"', ""));
                    })
                />
            </div>

            <div class="form-control">
                <Label
                    title="Method"
                    description="HTTP method to be used for the webhook request."
                />
                <Dropdown
                    dropdown_width="w-100"
                    dropdown_icon="".to_string()
                    dropdown_text=method_rs.get_untracked().to_string()
                    dropdown_direction=DropdownDirection::Down
                    dropdown_btn_type=DropdownBtnType::Select
                    dropdown_options=method_options
                    on_select=Callback::new(move |selected_item: HttpMethod| {
                        logging::log!("selected item {:?}", selected_item);
                        method_ws.set(selected_item);
                    })
                />
            </div>

            <div class="form-control">
                <Label title="Paylaod Version" />
                <Dropdown
                    dropdown_width="w-100"
                    dropdown_icon="".to_string()
                    dropdown_text=payload_version_rs.get_untracked().to_string()
                    dropdown_direction=DropdownDirection::Down
                    dropdown_btn_type=DropdownBtnType::Select
                    dropdown_options=payload_version_options
                    on_select=Callback::new(move |selected_item: PayloadVersion| {
                        logging::log!("selected item {:?}", selected_item);
                        payload_version_ws.set(selected_item);
                    })
                />
            </div>

            <div class="form-control">
                <Label
                    title="Events"
                    description="Events for which this webhook will be triggered."
                />
                <Dropdown
                    dropdown_text="Add Events".to_string()
                    dropdown_direction=DropdownDirection::Down
                    dropdown_btn_type=DropdownBtnType::Select
                    dropdown_options=events_options
                    selected=events_rs.get_untracked()
                    multi_select=true
                    on_select=handle_select_webhook_event_dropdown_option
                    on_remove=handle_remove_webhook_event_dropdown_option
                />
            </div>

            <div class="form-control">
                <Label
                    title="Custom Headers"
                    description="Custom headers are optional and can be used to pass additional information with the webhook request."
                />
                <EditorProvider>
                    <Input
                        id="custom_headers"
                        class="rounded-md resize-y w-full max-w-md"
                        value=Value::Object((*custom_headers_rs.get_untracked()).clone())
                        schema_type=Single(JsonSchemaType::Object)
                        r#type=InputType::Monaco(vec![])
                        on_change=move |value: Value| {
                            if let Some(val) = value.as_object() {
                                custom_headers_ws.set(CustomHeaders::from(val.clone()));
                            }
                        }
                    />
                </EditorProvider>
            </div>

            {move || {
                let loading = req_inprogess_rs.get();
                view! {
                    <Button
                        class="self-end h-12 w-48"
                        text="Submit"
                        icon_class="ri-send-plane-line"
                        on_click=move |ev: MouseEvent| {
                            ev.prevent_default();
                            on_submit();
                        }
                        loading
                    />
                }
            }}

        </form>
        {move || match update_request_rws.get() {
            None => ().into_view(),
            Some((webhook_name, update_request)) => {
                view! {
                    <ChangeLogSummary
                        webhook_name
                        change_type=ChangeType::Update(update_request)
                        on_confirm=move |_| on_submit()
                        on_close=move |_| update_request_rws.set(None)
                    />
                }
            }
        }}
    }
}

#[derive(Clone)]
pub enum ChangeType {
    Delete,
    Update(UpdateWebhookRequest),
}

#[component]
pub fn change_log_summary(
    webhook_name: String,
    change_type: ChangeType,
    #[prop(into)] on_confirm: Callback<()>,
    #[prop(into)] on_close: Callback<()>,
    #[prop(into, default = Signal::derive(|| false))] inprogress: Signal<bool>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let webhook = create_local_resource(
        move || (webhook_name.clone(), workspace.get().0, org.get().0),
        |(webhook_name, workspace, org)| async move {
            get_webhook(&webhook_name, &workspace, &org).await
        },
    );

    let disabled_rws = RwSignal::new(true);
    let change_type = StoredValue::new(change_type);

    let (title, description, confirm_text) = match change_type.get_value() {
        ChangeType::Update(_) => (
            "Confirm Update",
            "Are you sure you want to update this webhook?",
            "Yes, Update",
        ),
        ChangeType::Delete => (
            "Confirm Delete",
            "Are you sure you want to delete this webhook? Action is irreversible.",
            "Yes, Delete",
        ),
    };

    view! {
        <ChangeLogPopup
            title
            description
            confirm_text
            on_confirm
            on_close
            disabled=disabled_rws
            inprogress
        >
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Block style_class="h-10".to_string() /> }
            }>
                {
                    Effect::new(move |_| {
                        let webhook = webhook.get();
                        if let Some(Ok(_)) = webhook {
                            disabled_rws.set(false);
                        } else if let Some(Err(e)) = webhook {
                            logging::error!("Error fetching webhook: {}", e);
                        }
                    });
                }
                {move || match webhook.get() {
                    Some(Ok(webhook)) => {
                        let (new_headers, new_values) = match change_type.get_value() {
                            ChangeType::Update(update_request) => {
                                let description = update_request
                                    .description
                                    .unwrap_or_else(|| webhook.description.clone())
                                    .deref()
                                    .to_string();
                                let enabled = update_request.enabled.unwrap_or(webhook.enabled);
                                let url = update_request.url.unwrap_or_else(|| webhook.url.clone());
                                let method = update_request.method.unwrap_or(webhook.method);
                                let payload_version = update_request
                                    .payload_version
                                    .unwrap_or(webhook.payload_version);
                                let events = update_request
                                    .events
                                    .unwrap_or_else(|| webhook.events.clone());
                                (
                                    Some(
                                        Value::Object(
                                            update_request
                                                .custom_headers
                                                .unwrap_or_else(|| webhook.custom_headers.clone())
                                                .deref()
                                                .clone(),
                                        ),
                                    ),
                                    Map::from_iter(
                                        vec![
                                            ("Description".to_string(), Value::String(description)),
                                            ("Enabled".to_string(), Value::Bool(enabled)),
                                            ("Url".to_string(), Value::String(url.to_string())),
                                            ("Method".to_string(), Value::String(method.to_string())),
                                            (
                                                "Payload Version".to_string(),
                                                Value::String(payload_version.to_string()),
                                            ),
                                            (
                                                "Events".to_string(),
                                                Value::Array(
                                                    events
                                                        .into_iter()
                                                        .map(|e| e.to_string())
                                                        .map(Value::String)
                                                        .collect(),
                                                ),
                                            ),
                                        ],
                                    ),
                                )
                            }
                            ChangeType::Delete => (None, Map::new()),
                        };
                        view! {
                            <ChangeSummary
                                title="Webhook changes"
                                key_column="Property"
                                old_values=Map::from_iter(
                                    vec![
                                        (
                                            "Description".to_string(),
                                            Value::String(webhook.description.deref().to_string()),
                                        ),
                                        ("Enabled".to_string(), Value::Bool(webhook.enabled)),
                                        ("Url".to_string(), Value::String(webhook.url.to_string())),
                                        (
                                            "Method".to_string(),
                                            Value::String(webhook.method.to_string()),
                                        ),
                                        (
                                            "Payload Version".to_string(),
                                            Value::String(webhook.payload_version.to_string()),
                                        ),
                                        (
                                            "Events".to_string(),
                                            Value::Array(
                                                webhook
                                                    .events
                                                    .into_iter()
                                                    .map(|e| e.to_string())
                                                    .map(Value::String)
                                                    .collect(),
                                            ),
                                        ),
                                    ],
                                )
                                new_values
                            />
                            <JsonChangeSummary
                                title="Custom Header changes"
                                old_values=Some(
                                    Value::Object(webhook.custom_headers.deref().clone()),
                                )
                                new_values=new_headers
                            />
                        }
                            .into_view()
                    }
                    Some(Err(e)) => {
                        logging::error!("Error fetching webhook: {}", e);
                        view! { <div>Error fetching webhook</div> }.into_view()
                    }
                    None => view! { <div>Loading...</div> }.into_view(),
                }}
            </Suspense>
        </ChangeLogPopup>
    }
}
