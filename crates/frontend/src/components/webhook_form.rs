use leptos::*;
use serde_json::Value;
use strum::IntoEnumIterator;
use superposition_types::database::models::others::{
    CustomHeaders, HttpMethod, PayloadVersion, WebhookEvent,
};
use web_sys::MouseEvent;

use crate::{
    api::{create_webhook, update_webhook},
    components::{
        alert::AlertType,
        button::Button,
        change_form::ChangeForm,
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        input::{Input, InputType, Toggle},
    },
    providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider},
    schema::{JsonSchemaType, SchemaType::Single},
    types::{OrganisationId, Tenant},
};

#[component]
pub fn webhook_form<NF>(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] webhook_name: String,
    #[prop(default = String::new())] description: String,
    #[prop(default = false)] enabled: bool,
    #[prop(default = String::new())] url: String,
    #[prop(default = HttpMethod::default())] method: HttpMethod,
    #[prop(default = PayloadVersion::default())] payload_version: PayloadVersion,
    #[prop(default = CustomHeaders::default())] custom_headers: CustomHeaders,
    #[prop(default = Vec::new())] events: Vec<WebhookEvent>,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();

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

    let handle_select_webhook_event_dropdown_option =
        Callback::new(move |selected_event: WebhookEvent| {
            events_ws.update(|value| value.push(selected_event));
        });

    let handle_remove_webhook_event_dropdown_option =
        Callback::new(move |selected_event: WebhookEvent| {
            events_ws.update(|value| value.retain(|d| d != &selected_event));
        });

    let on_submit = move |ev: MouseEvent| {
        req_inprogress_ws.set(true);
        ev.prevent_default();

        let webhook_name = webhook_name_rs.get();
        let description = description_rs.get();
        let enabled = enabled_rs.get();
        let url = url_rs.get();
        let method = method_rs.get();
        let payload_version = payload_version_rs.get();
        let custom_headers = custom_headers_rs.get();
        let change_reason = change_reason_rs.get();
        let events = events_rs.get();

        let handle_submit_clone = handle_submit.clone();
        spawn_local({
            let handle_submit = handle_submit_clone;

            async move {
                let result = if edit {
                    update_webhook(
                        webhook_name,
                        enabled,
                        url,
                        method,
                        payload_version,
                        custom_headers,
                        events,
                        description,
                        change_reason,
                        tenant_rws.get().0,
                        org_rws.get().0,
                    )
                    .await
                } else {
                    create_webhook(
                        webhook_name,
                        description,
                        enabled,
                        url,
                        method,
                        payload_version,
                        custom_headers,
                        events,
                        change_reason,
                        tenant_rws.get().0,
                        org_rws.get().0,
                    )
                    .await
                };
                match result {
                    Ok(_) => {
                        handle_submit();
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
                req_inprogress_ws.set(false);
            }
        });
    };
    view! {
        <form class="form-control w-full space-y-4 bg-white text-gray-700 font-mono">
            <div class="form-control">
                <label class="label">
                    <span class="label-text">Name</span>
                </label>
                <Input
                    disabled=edit
                    r#type=InputType::Text
                    placeholder="Webhook Name"
                    class="input input-bordered w-full max-w-md"
                    value=Value::String(webhook_name_rs.get())
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
                on_change=Callback::new(move |new_description| {
                    description_ws.set(new_description)
                })
            />
            <ChangeForm
                title="Reason for Change".to_string()
                placeholder="Enter a reason for this change".to_string()
                value=change_reason_rs.get_untracked()
                on_change=Callback::new(move |new_change_reason| {
                    change_reason_ws.set(new_change_reason)
                })
            />

            <div class="form-control">
                <label class="label">
                    <span class="label-text">Enable Webhook</span>
                </label>
                <Toggle
                    name="Enable Webhook"
                    value=enabled_rs.get()
                    on_change=Callback::new(move |flag: serde_json::Value| {
                        let flag = flag.as_bool().unwrap();
                        if flag {
                            enabled_ws.set(true);
                        } else {
                            enabled_ws.set(false);
                        }
                    })
                />
            </div>

            <div class="form-control">
                <label class="label">
                    <span class="label-text">URL</span>
                </label>
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
                <label class="label">
                    <span class="label-text">Method</span>
                </label>
                <Dropdown
                    dropdown_width="w-100"
                    dropdown_icon="".to_string()
                    dropdown_text=method_rs.get_untracked().to_string()
                    dropdown_direction=DropdownDirection::Down
                    dropdown_btn_type=DropdownBtnType::Select
                    dropdown_options={HttpMethod::iter().collect::<Vec<HttpMethod>>()}
                    on_select=Callback::new(move |selected_item: HttpMethod| {
                        logging::log!("selected item {:?}", selected_item);
                        method_ws.set(selected_item);
                    })
                />
            </div>

            <div class="form-control">
                <label class="label">
                    <span class="label-text">Payload Version</span>
                </label>
                <Dropdown
                    dropdown_width="w-100"
                    dropdown_icon="".to_string()
                    dropdown_text=payload_version_rs.get().to_string()
                    dropdown_direction=DropdownDirection::Down
                    dropdown_btn_type=DropdownBtnType::Select
                    dropdown_options={PayloadVersion::iter().collect::<Vec<PayloadVersion>>()}
                    on_select=Callback::new(move |selected_item: PayloadVersion| {
                        logging::log!("selected item {:?}", selected_item);
                        payload_version_ws.set(selected_item);
                    })
                />
            </div>

            <div class="form-control">
                <label class="label">
                    <span class="label-text">Events</span>
                </label>
                <Dropdown
                    dropdown_text="Add Events".to_string()
                    dropdown_direction=DropdownDirection::Down
                    dropdown_btn_type=DropdownBtnType::Select
                    dropdown_options={WebhookEvent::iter().collect::<Vec<WebhookEvent>>()}
                    selected=events_rs.get()
                    multi_select=true
                    on_select=handle_select_webhook_event_dropdown_option
                    on_remove=handle_remove_webhook_event_dropdown_option
                />
            </div>

            <div class="form-control">
                <label class="label">
                    <span class="label-text">Custom Headers</span>
                </label>
                <EditorProvider>
                    <Input
                        id="custom_headers"
                        class="rounded-md resize-y w-full max-w-md"
                        value=Value::Object((*custom_headers_rs.get_untracked()).clone())
                        schema_type=Single(JsonSchemaType::Object)
                        r#type=InputType::Monaco(vec![])
                        on_change=Callback::new(move |value: Value| {
                            if let Some(val)= value.as_object() {
                                custom_headers_ws.set(CustomHeaders::from(val.clone()));
                            }
                        })
                    />
                </EditorProvider>
            </div>

            <div class="form-control grid w-full justify-start">
                {move || {
                    let loading = req_inprogess_rs.get();
                    view! {
                        <Button
                            class="pl-[70px] pr-[70px] w-48 h-12".to_string()
                            text="Submit".to_string()
                            on_click=on_submit.clone()
                            loading
                        />
                    }
                }}

            </div>

        </form>
    }
}
