use leptos::*;
use serde_json::Value;
use superposition_types::database::models::Description;
use superposition_types::{
    api::secrets::UpdateSecretRequest, database::models::ChangeReason,
};
use web_sys::MouseEvent;

use crate::{
    api::secrets,
    components::{
        alert::AlertType,
        button::Button,
        change_form::ChangeForm,
        change_summary::ChangeLogPopup,
        form::label::Label,
        input::{Input, InputType},
    },
    providers::alert_provider::enqueue_alert,
    schema::{JsonSchemaType, SchemaType::Single},
    types::{OrganisationId, Workspace},
};

#[derive(Clone, Debug)]
pub enum ResponseType {
    Response,
    UpdatePrecheck,
}

fn try_update_payload(
    value: String,
    description: String,
    change_reason: String,
) -> Result<UpdateSecretRequest, String> {
    Ok(UpdateSecretRequest {
        value: Some(value),
        description: Some(Description::try_from(description)?),
        change_reason: ChangeReason::try_from(change_reason)?,
    })
}

#[component]
pub fn secret_form(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] secret_name: String,
    #[prop(default = String::new())] secret_value: String,
    #[prop(default = String::new())] description: String,
    #[prop(into)] handle_submit: Callback<()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let (name_rs, name_ws) = create_signal(secret_name.clone());
    let (value_rs, value_ws) = create_signal(secret_value);
    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());
    let (req_inprogress_rs, req_inprogress_ws) = create_signal(false);

    let update_request_rws: RwSignal<Option<(String, UpdateSecretRequest)>> =
        RwSignal::new(None);

    let on_submit = Callback::new(move |_| {
        req_inprogress_ws.set(true);
        let name_val = name_rs.get_untracked();
        let value_val = value_rs.get_untracked();
        let description_val = description_rs.get_untracked();
        let change_reason_val = change_reason_rs.get_untracked();
        let workspace = workspace.get_untracked();
        let org = org.get_untracked();

        spawn_local(async move {
            let result = match (edit, update_request_rws.get_untracked()) {
                (true, Some((_, update_request))) => {
                    let future =
                        secrets::update(name_val, update_request, &workspace.0, &org.0);
                    update_request_rws.set(None);
                    future.await.map(|_| ResponseType::Response)
                }
                (true, None) => {
                    let payload =
                        try_update_payload(value_val, description_val, change_reason_val);
                    match payload {
                        Ok(payload) => {
                            update_request_rws.set(Some((name_val, payload)));
                            Ok(ResponseType::UpdatePrecheck)
                        }
                        Err(e) => Err(e),
                    }
                }
                _ => secrets::create(
                    name_val,
                    value_val,
                    description_val,
                    change_reason_val,
                    &workspace.0,
                    &org.0,
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
                        "Secret updated successfully!"
                    } else {
                        "New Secret created successfully! Value will now be masked."
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
        });
    });

    view! {
        <form class="w-full flex flex-col gap-5 text-gray-700 bg-white">
            <div class="form-control">
                <Label title="Secret Name" />
                <Input
                    disabled=edit
                    r#type=InputType::Text
                    placeholder="Enter secret name (uppercase, digits, underscore)"
                    class="input input-bordered w-full max-w-md"
                    value=Value::String(name_rs.get_untracked())
                    schema_type=Single(JsonSchemaType::String)
                    on_change=move |value: Value| {
                        name_ws.set(value.as_str().map(String::from).unwrap_or_default());
                    }
                />
            </div>

            <ChangeForm
                title="Description"
                placeholder="Enter a description"
                value=description_rs.get_untracked()
                on_change=move |new_description| { description_ws.set(new_description) }
            />

            <ChangeForm
                title="Reason for Change"
                placeholder="Enter a reason for this change"
                value=change_reason_rs.get_untracked()
                on_change=move |new_change_reason| { change_reason_ws.set(new_change_reason) }
            />

            <div class="form-control">
                <Label title="Value" />
                <Input
                    r#type=InputType::Text
                    placeholder="Enter secret value (will be encrypted)"
                    class="input input-bordered w-full max-w-md font-mono"
                    value=Value::String(value_rs.get_untracked())
                    schema_type=Single(JsonSchemaType::String)
                    on_change=move |value: Value| {
                        value_ws.set(value.as_str().map(String::from).unwrap_or_default());
                    }
                />
                {move || {
                    if !edit {
                        view! {
                            <p class="text-sm text-orange-600 mt-2">
                                <i class="ri-alert-line"></i>
                                " Warning: The secret value will only be shown once during creation. After that, it will be masked."
                            </p>
                        }
                            .into_view()
                    } else {
                        ().into_view()
                    }
                }}
            </div>

            {move || {
                let loading = req_inprogress_rs.get();
                view! {
                    <Button
                        class="self-end h-12 w-48"
                        text="Submit"
                        icon_class="ri-send-plane-line"
                        on_click=move |ev: MouseEvent| {
                            ev.prevent_default();
                            on_submit.call(());
                        }
                        loading
                    />
                }
            }}
        </form>
        {move || match update_request_rws.get() {
            None => ().into_view(),
            Some((secret_name, update_request)) => {
                view! {
                    <ChangeLogSummary
                        secret_name
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
    Update(UpdateSecretRequest),
}

#[component]
pub fn change_log_summary(
    secret_name: String,
    change_type: ChangeType,
    #[prop(into)] on_confirm: Callback<()>,
    #[prop(into)] on_close: Callback<()>,
    #[prop(into, default = Signal::derive(|| false))] inprogress: Signal<bool>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let secret = create_local_resource(
        move || (secret_name.clone(), workspace.get().0, org.get().0),
        |(secret_name, workspace, org)| async move {
            secrets::get(&secret_name, &workspace, &org).await
        },
    );

    let disabled_rws = RwSignal::new(true);
    let change_type = StoredValue::new(change_type);

    let (title, description, confirm_text) = match change_type.get_value() {
        ChangeType::Update(_) => (
            "Confirm Update",
            "Are you sure you want to update this secret?",
            "Yes, Update",
        ),
        ChangeType::Delete => (
            "Confirm Delete",
            "Are you sure you want to delete this secret? Action is irreversible.",
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
            {
                Effect::new(move |_| {
                    let secret_result = secret.get();
                    if let Some(Ok(_)) = secret_result {
                        disabled_rws.set(false);
                    } else if let Some(Err(e)) = secret_result {
                        logging::error!("Error fetching secret: {}", e);
                    }
                });
            }
        </ChangeLogPopup>
    }
}
