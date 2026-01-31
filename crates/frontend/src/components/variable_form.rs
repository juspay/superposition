use leptos::*;
use serde_json::Map;
use serde_json::Value;
use superposition_types::database::models::Description;
use superposition_types::{
    api::variables::UpdateVariableRequest, database::models::ChangeReason,
};
use web_sys::MouseEvent;

use crate::{
    api::variables,
    components::{
        alert::AlertType,
        button::Button,
        change_form::ChangeForm,
        change_summary::{ChangeLogPopup, ChangeSummary},
        form::label::Label,
        input::{Input, InputType},
        skeleton::{Skeleton, SkeletonVariant},
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
) -> Result<UpdateVariableRequest, String> {
    Ok(UpdateVariableRequest {
        value: Some(value),
        description: Some(Description::try_from(description)?),
        change_reason: ChangeReason::try_from(change_reason)?,
    })
}

#[component]
pub fn VariableForm(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] variable_name: String,
    #[prop(default = String::new())] variable_value: String,
    #[prop(default = String::new())] description: String,
    #[prop(into)] handle_submit: Callback<()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let (name_rs, name_ws) = create_signal(variable_name.clone());
    let (value_rs, value_ws) = create_signal(variable_value);
    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());
    let (req_inprogress_rs, req_inprogress_ws) = create_signal(false);

    let update_request_rws: RwSignal<Option<(String, UpdateVariableRequest)>> =
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
                        variables::update(name_val, update_request, &workspace.0, &org.0);
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
                _ => variables::create(
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
                        "Variable updated successfully!"
                    } else {
                        "New Variable created successfully!"
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
                <Label title="Variable Name" />
                <Input
                    disabled=edit
                    r#type=InputType::Text
                    placeholder="Enter variable name (uppercase, digits, underscore)"
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
                    placeholder="Enter variable value"
                    class="input input-bordered w-full max-w-md"
                    value=Value::String(value_rs.get_untracked())
                    schema_type=Single(JsonSchemaType::String)
                    on_change=move |value: Value| {
                        value_ws.set(value.as_str().map(String::from).unwrap_or_default());
                    }
                />
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
            Some((variable_name, update_request)) => {
                view! {
                    <ChangeLogSummary
                        variable_name
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
    Update(UpdateVariableRequest),
}

#[component]
pub fn ChangeLogSummary(
    variable_name: String,
    change_type: ChangeType,
    #[prop(into)] on_confirm: Callback<()>,
    #[prop(into)] on_close: Callback<()>,
    #[prop(into, default = Signal::derive(|| false))] inprogress: Signal<bool>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let variable = create_local_resource(
        move || (variable_name.clone(), workspace.get().0, org.get().0),
        |(variable_name, workspace, org)| async move {
            variables::get(&variable_name, &workspace, &org).await
        },
    );

    let disabled_rws = RwSignal::new(true);
    let change_type = StoredValue::new(change_type);

    let (title, description, confirm_text) = match change_type.get_value() {
        ChangeType::Update(_) => (
            "Confirm Update",
            "Are you sure you want to update this variable?",
            "Yes, Update",
        ),
        ChangeType::Delete => (
            "Confirm Delete",
            "Are you sure you want to delete this variable? Action is irreversible.",
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
                        let variable_result = variable.get();
                        if let Some(Ok(_)) = variable_result {
                            disabled_rws.set(false);
                        } else if let Some(Err(e)) = variable_result {
                            logging::error!("Error fetching variable: {}", e);
                        }
                    });
                }
                {move || match variable.get() {
                    Some(Ok(var)) => {
                        let (old_values, new_values) = match change_type.get_value() {
                            ChangeType::Update(update_request) => {
                                (
                                    Map::from_iter(
                                        vec![
                                            ("Value".to_string(), Value::String(var.value.clone())),
                                        ],
                                    ),
                                    Map::from_iter(
                                        vec![
                                            (
                                                "Value".to_string(),
                                                Value::String(update_request.value.unwrap()),
                                            ),
                                        ],
                                    ),
                                )
                            }
                            ChangeType::Delete => {
                                (
                                    Map::from_iter(
                                        vec![("Value".to_string(), Value::String(var.value))],
                                    ),
                                    Map::new(),
                                )
                            }
                        };
                        view! {
                            <ChangeSummary
                                title="Variable changes"
                                key_column="Property"
                                old_values
                                new_values
                            />
                        }
                            .into_view()
                    }
                    Some(Err(e)) => {
                        logging::error!("Error fetching variable: {}", e);
                        view! { <div>Error fetching variable</div> }.into_view()
                    }
                    None => view! { <div>Loading...</div> }.into_view(),
                }}
            </Suspense>
        </ChangeLogPopup>
    }
}
