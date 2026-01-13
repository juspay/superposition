pub mod utils;

use std::ops::Deref;

use leptos::*;
use serde_json::{Map, Value, json};
use superposition_types::api::type_templates::TypeTemplateUpdateRequest;
use utils::try_update_payload;
use web_sys::MouseEvent;

use crate::api::get_type_template;
use crate::components::change_summary::{
    ChangeLogPopup, ChangeSummary, JsonChangeSummary,
};
use crate::components::form::label::Label;
use crate::components::skeleton::{Skeleton, SkeletonVariant};
use crate::components::type_template_form::utils::create_type;
use crate::components::{
    alert::AlertType,
    button::Button,
    change_form::ChangeForm,
    input::{Input, InputType},
    type_template_form::utils::update_type,
};
use crate::providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider};
use crate::schema::{JsonSchemaType, SchemaType};
use crate::types::{OrganisationId, Workspace};

enum ResponseType {
    UpdatePrecheck,
    Response,
}

#[component]
pub fn type_template_form(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] type_name: String,
    #[prop(default = json!({"type": "number"}))] type_schema: Value,
    #[prop(into)] handle_submit: Callback<()>,
    #[prop(default = String::new())] description: String,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let (error_message, set_error_message) = create_signal("".to_string());
    let (type_name_rs, type_name_ws) = create_signal(type_name);
    let (type_schema_rs, type_schema_ws) = create_signal(type_schema);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());
    let update_request_rws = RwSignal::new(None);

    let on_submit = move |_| {
        req_inprogress_ws.set(true);

        let type_name = type_name_rs.get_untracked();
        let type_schema = type_schema_rs.get_untracked();
        let description = description_rs.get_untracked();
        let change_reason = change_reason_rs.get_untracked();
        let workspace = workspace.get_untracked();
        let org_id = org.get_untracked();

        spawn_local({
            async move {
                let result = match (edit, update_request_rws.get_untracked()) {
                    (true, Some((_, update_payload))) => {
                        let future =
                            update_type(type_name, update_payload, &workspace, &org_id);
                        update_request_rws.set(None);
                        future.await.map(|_| ResponseType::Response)
                    }
                    (true, None) => {
                        let update_payload =
                            try_update_payload(type_schema, description, change_reason);
                        match update_payload {
                            Ok(payload) => {
                                update_request_rws.set(Some((type_name, payload)));
                                Ok(ResponseType::UpdatePrecheck)
                            }
                            Err(e) => Err(e),
                        }
                    }
                    _ => create_type(
                        type_name,
                        type_schema,
                        description,
                        change_reason,
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
                            "Type updated successfully!"
                        } else {
                            "New Type created successfully!"
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
                <Label title="Type Name" />
                <input
                    disabled=edit
                    type="text"
                    placeholder="Type name"
                    name="type_name"
                    id="type_name"
                    class="input input-bordered w-full max-w-md"
                    value=move || type_name_rs.get()
                    on:change=move |ev| {
                        let value = event_target_value(&ev);
                        type_name_ws.set(value);
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
                <Label title="Type Schema" />
                {move || {
                    let schem = type_schema_rs.get();
                    let schema_type = SchemaType::Single(JsonSchemaType::from(&schem));
                    view! {
                        <EditorProvider>
                            <Input
                                id="type-schema"
                                class="w-full max-w-md pt-3 rounded-md resize-y"
                                schema_type
                                value=schem
                                on_change=move |new_type_schema| type_schema_ws.set(new_type_schema)
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
            Some((type_name, update_request)) => {
                view! {
                    <ChangeLogSummary
                        type_name
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
    Update(TypeTemplateUpdateRequest),
}

#[component]
pub fn change_log_summary(
    type_name: String,
    change_type: ChangeType,
    #[prop(into)] on_confirm: Callback<()>,
    #[prop(into)] on_close: Callback<()>,
    #[prop(into, default = Signal::derive(|| false))] inprogress: Signal<bool>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let type_template = create_local_resource(
        move || (type_name.clone(), workspace.get().0, org.get().0),
        |(type_name, workspace, org)| async move {
            get_type_template(&type_name, &workspace, &org).await
        },
    );

    let disabled_rws = RwSignal::new(true);
    let change_type = StoredValue::new(change_type);

    let (title, description, confirm_text) = match change_type.get_value() {
        ChangeType::Update(_) => (
            "Confirm Update",
            "Are you sure you want to update this type template?",
            "Yes, Update",
        ),
        ChangeType::Delete => (
            "Confirm Delete",
            "Are you sure you want to delete this type template? Action is irreversible.",
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
                        let type_template = type_template.get();
                        if let Some(Ok(_)) = type_template {
                            disabled_rws.set(false);
                        } else if let Some(Err(e)) = type_template {
                            logging::error!("Error fetching type template: {}", e);
                        }
                    });
                }
                {move || match type_template.get() {
                    Some(Ok(type_temp)) => {
                        let (title, new_schema, description) = match change_type.get_value() {
                            ChangeType::Update(update_request) => {
                                (
                                    "Schema update",
                                    Some(update_request.type_schema.clone()),
                                    update_request
                                        .description
                                        .unwrap_or_else(|| type_temp.description.clone()),
                                )
                            }
                            ChangeType::Delete => {
                                ("Schema to be deleted", None, type_temp.description.clone())
                            }
                        };
                        view! {
                            <JsonChangeSummary
                                title
                                old_values=Some(Value::from(type_temp.type_schema))
                                new_values=new_schema.map(Value::from)
                            />
                            <ChangeSummary
                                title="Other changes"
                                key_column="Property"
                                old_values=Map::from_iter(
                                    vec![
                                        (
                                            "Description".to_string(),
                                            Value::String(type_temp.description.deref().to_string()),
                                        ),
                                    ],
                                )
                                new_values=Map::from_iter(
                                    vec![
                                        (
                                            "Description".to_string(),
                                            Value::String(description.deref().to_string()),
                                        ),
                                    ],
                                )
                            />
                        }
                            .into_view()
                    }
                    Some(Err(e)) => {
                        logging::error!("Error fetching type template: {}", e);
                        view! { <div>{"Error fetching type template"}</div> }.into_view()
                    }
                    None => view! { <div>Loading...</div> }.into_view(),
                }}
            </Suspense>
        </ChangeLogPopup>
    }
}
