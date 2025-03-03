pub mod utils;

use crate::components::type_template_form::utils::create_type;
use crate::components::{
    alert::AlertType,
    button::Button,
    input::{Input, InputType},
    type_template_form::utils::update_type,
};
use crate::providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider};
use crate::schema::{JsonSchemaType, SchemaType};
use crate::types::{OrganisationId, Tenant};
use leptos::*;
use serde_json::{json, Value};
use web_sys::MouseEvent;

#[component]
pub fn type_template_form<NF>(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] type_name: String,
    #[prop(default = json!({"type": "number"}))] type_schema: Value,
    handle_submit: NF,
    #[prop(default = String::new())] description: String,
    #[prop(default = String::new())] change_reason: String,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();

    let (error_message, set_error_message) = create_signal("".to_string());
    let (type_name_rs, type_name_ws) = create_signal(type_name);
    let (type_schema_rs, type_schema_ws) = create_signal(type_schema);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(change_reason);

    let on_submit = move |ev: MouseEvent| {
        req_inprogress_ws.set(true);
        ev.prevent_default();
        let type_name = type_name_rs.get();
        let type_schema = type_schema_rs.get();

        let handle_submit_clone = handle_submit.clone();
        spawn_local({
            let handle_submit = handle_submit_clone;
            async move {
                let result = if edit {
                    let payload = json!({
                        "type_schema": type_schema,
                        "description": description_rs.get(),
                        "change_reason": change_reason_rs.get(),
                    });
                    update_type(tenant_rws.get().0, type_name, payload, org_rws.get().0)
                        .await
                } else {
                    let description = description_rs.get();
                    let change_reason = change_reason_rs.get();
                    let payload = json!({
                        "type_name": type_name,
                        "type_schema": type_schema,
                        "description": description,
                        "change_reason": change_reason
                    });
                    create_type(tenant_rws.get().0, payload.clone(), org_rws.get().0)
                        .await
                };
                match result {
                    Ok(_) => {
                        handle_submit();
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
                req_inprogress_ws.set(false);
            }
        });
    };
    view! {
        <form class="form-control w-full space-y-4 bg-white text-gray-700 font-mono">
            <div class="form-control">
                <label class="label">
                    <span class="label-text">Type Name</span>
                </label>
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

            <div class="form-control">
                <label class="label">
                    <span class="label-text">Description</span>
                </label>
                <textarea
                    placeholder="Enter description"
                    class="textarea textarea-bordered w-full max-w-md"
                    value=description_rs.get()
                    on:change=move |ev| {
                        let value = event_target_value(&ev);
                        description_ws.set(value);
                    }
                />
            </div>

            <div class="form-control">
                <label class="label">
                    <span class="label-text">Reason for Change</span>
                </label>
                <textarea
                    placeholder="Enter a reason for this change"
                    class="textarea textarea-bordered w-full max-w-md"
                    value=change_reason_rs.get()
                    on:change=move |ev| {
                        let value = event_target_value(&ev);
                        change_reason_ws.set(value);
                    }
                />
            </div>

            <div class="form-control">
                <label class="label">
                    <span class="label-text">Type Schema</span>
                </label>
                {move || {
                    let schem = type_schema_rs.get();
                    let schema_type = SchemaType::Single(JsonSchemaType::from(&schem));
                    view! {
                        <EditorProvider>
                            <Input
                                id="type-schema"
                                class="mt-5 rounded-md resize-y w-full max-w-md pt-3"
                                schema_type
                                value=schem
                                on_change=Callback::new(move |new_type_schema| {
                                    type_schema_ws.set(new_type_schema)
                                })
                                r#type=InputType::Monaco(vec![])
                            />
                        </EditorProvider>
                    }
                }}

            </div>

            <div class="form-control grid w-full mt-5 justify-start">
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
            <div>
                <p class="text-red-500">{move || error_message.get()}</p>
            </div>
        </form>
    }
}
