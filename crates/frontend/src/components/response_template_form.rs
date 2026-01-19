use leptos::*;
use serde_json::{json, Value};
use superposition_types::api::response_templates::ResponseTemplateUpdateRequest;
use web_sys::MouseEvent;

use crate::api::response_templates;
use crate::components::change_summary::{
    ChangeLogPopup, ChangeSummary, JsonChangeSummary,
};
use crate::components::form::label::Label;
use crate::components::skeleton::{Skeleton, SkeletonVariant};
use crate::components::{
    alert::AlertType,
    button::Button,
    change_form::ChangeForm,
    input::{Input, InputType},
};
use crate::providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider};
use crate::schema::{JsonSchemaType, SchemaType};
use crate::types::{OrganisationId, Workspace};

enum ResponseType {
    UpdatePrecheck,
    Response,
}

#[component]
pub fn response_template_form(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] name: String,
    #[prop(default = json!({}))] context: Value,
    #[prop(default = String::new())] content_type: String,
    #[prop(default = String::new())] template: String,
    #[prop(into)] handle_submit: Callback<()>,
    #[prop(default = String::new())] description: String,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let (error_message, set_error_message) = create_signal("".to_string());
    let (name_rs, name_ws) = create_signal(name);
    let (context_rs, context_ws) = create_signal(context);
    let (content_type_rs, content_type_ws) = create_signal(content_type);
    let (template_rs, template_ws) = create_signal(template);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());
    let update_request_rws = RwSignal::new(None);

    let on_submit = move |_| {
        req_inprogress_ws.set(true);

        let name = name_rs.get_untracked();
        let context = context_rs.get_untracked();
        let content_type = content_type_rs.get_untracked();
        let template = template_rs.get_untracked();
        let description = description_rs.get_untracked();
        let change_reason = change_reason_rs.get_untracked();
        let workspace = workspace.get_untracked();
        let org_id = org.get_untracked();

        spawn_local({
            async move {
                let result = match (edit, update_request_rws.get_untracked()) {
                    (true, Some((_, update_payload))) => {
                        let future = response_templates::update(
                            name,
                            update_payload,
                            &workspace,
                            &org_id,
                        );
                        update_request_rws.set(None);
                        future.await.map(|_| ResponseType::Response)
                    }
                    (true, None) => {
                        let update_payload = response_templates::try_update_payload(
                            content_type,
                            template,
                            change_reason,
                        );
                        match update_payload {
                            Ok(payload) => {
                                update_request_rws.set(Some((name, payload)));
                                Ok(ResponseType::UpdatePrecheck)
                            }
                            Err(e) => Err(e),
                        }
                    }
                    _ => response_templates::create(
                        name,
                        description,
                        change_reason,
                        context,
                        content_type,
                        template,
                        &workspace,
                        &org_id,
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
                            "Response template updated successfully!"
                        } else {
                            "New Response template created successfully!"
                        };
                        enqueue_alert(
                            String::from(success_message),
                            AlertType::Success,
                            5000,
                        );
                    }
                    Err(e) => {
                        set_error_message.set(e.clone());
                        enqueue_alert(e, AlertType::Error, 5000);
                    }
                }
            }
        });
    };

    view! {
        <form class="w-full flex flex-col gap-5 bg-white text-gray-700">
            <div class="form-control">
                <Label title="Name" />
                <input
                    disabled=edit
                    type="text"
                    placeholder="Template name"
                    name="name"
                    id="name"
                    class="input input-bordered w-full max-w-md"
                    value=move || name_rs.get()
                    on:change=move |ev| {
                        let value = event_target_value(&ev);
                        name_ws.set(value);
                    }
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

            <div class="form-control">
                <Label title="Context" />
                {move || {
                    let ctx = context_rs.get();
                    view! {
                        <EditorProvider>
                            <Input
                                id="context"
                                class="w-full max-w-md pt-3 rounded-md resize-y"
                                schema_type=SchemaType::Single(JsonSchemaType::Object)
                                value=ctx
                                on_change=move |new_context| context_ws.set(new_context)
                                r#type=InputType::Monaco(vec![])
                            />
                        </EditorProvider>
                    }
                }}
            </div>

            <div class="form-control">
                <Label title="Content Type" />
                <input
                    type="text"
                    placeholder="Content type (e.g., application/json)"
                    name="content_type"
                    id="content_type"
                    class="input input-bordered w-full max-w-md"
                    value=move || content_type_rs.get()
                    on:change=move |ev| {
                        let value = event_target_value(&ev);
                        content_type_ws.set(value);
                    }
                />
            </div>

            <div class="form-control">
                <Label title="Template" />
                {move || {
                    let tmpl = template_rs.get();
                    view! {
                        <EditorProvider>
                            <Input
                                id="template"
                                class="w-full max-w-md pt-3 rounded-md resize-y"
                                schema_type=SchemaType::Single(JsonSchemaType::String)
                                value=Value::String(tmpl)
                                on_change=move |new_template: Value| {
                                    if let Some(s) = new_template.as_str() {
                                        template_ws.set(s.to_string());
                                    }
                                }
                                r#type=InputType::Monaco(vec![])
                            />
                        </EditorProvider>
                    }
                }}
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
                            on_submit(())
                        }
                        loading
                    />
                }
            }}
            <p class="text-red-500">{move || error_message.get()}</p>

        </form>
        {move || match update_request_rws.get() {
            None => ().into_view(),
            Some((name, update_request)) => {
                view! {
                    <ChangeLogSummary
                        name
                        change_type=ChangeType::Update(update_request)
                        on_confirm=on_submit
                        on_close=move |_| update_request_rws.set(None)
                    />
                }
                    .into_view()
            }
        }}
    }
}

#[derive(Clone)]
pub enum ChangeType {
    Delete,
    Update(ResponseTemplateUpdateRequest),
}

#[component]
pub fn change_log_summary(
    name: String,
    change_type: ChangeType,
    #[prop(into)] on_confirm: Callback<()>,
    #[prop(into)] on_close: Callback<()>,
    #[prop(into, default = Signal::derive(|| false))] inprogress: Signal<bool>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let response_template = create_local_resource(
        move || (name.clone(), workspace.get().0, org.get().0),
        |(name, workspace, org)| async move {
            response_templates::get(&name, &workspace, &org).await
        },
    );

    let disabled_rws = RwSignal::new(true);
    let change_type = StoredValue::new(change_type);

    let (title, description, confirm_text) = match change_type.get_value() {
        ChangeType::Update(_) => (
            "Confirm Update",
            "Are you sure you want to update this response template?",
            "Yes, Update",
        ),
        ChangeType::Delete => (
            "Confirm Delete",
            "Are you sure you want to delete this response template? Action is irreversible.",
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
                        let response_template = response_template.get();
                        if let Some(Ok(_)) = response_template {
                            disabled_rws.set(false);
                        } else if let Some(Err(e)) = response_template {
                            logging::error!("Error fetching response template: {}", e);
                        }
                    });
                }
                {move || match response_template.get() {
                    Some(Ok(resp_temp)) => {
                        let (new_content_type, new_template) = match change_type.get_value() {
                            ChangeType::Update(update_request) => {
                                (
                                    update_request.content_type.clone(),
                                    update_request.template.clone(),
                                )
                            }
                            ChangeType::Delete => (None, None),
                        };
                        view! {
                            <ChangeSummary
                                title="Other changes"
                                key_column="Property"
                                old_values=serde_json::Map::from_iter(
                                    vec![("Content Type".to_string(), Value::String(resp_temp.content_type.clone()))],
                                )
                                new_values=serde_json::Map::from_iter(
                                    vec![("Content Type".to_string(), Value::String(new_content_type.unwrap_or_else(|| resp_temp.content_type.clone())))],
                                )
                            />
                            <JsonChangeSummary
                                title="Template changes"
                                old_values=Some(Value::String(resp_temp.template.clone()))
                                new_values=new_template.map(Value::String)
                            />
                        }
                            .into_view()
                    }
                    Some(Err(e)) => {
                        logging::error!("Error fetching response template: {}", e);
                        view! { <div>{"Error fetching response template"}</div> }.into_view()
                    }
                    None => view! { <div>Loading...</div> }.into_view(),
                }}
            </Suspense>
        </ChangeLogPopup>
    }
}
