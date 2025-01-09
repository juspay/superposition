pub mod types;
pub mod utils;

use self::utils::{create_function, test_function, update_function};
use crate::{
    components::{button::Button, monaco_editor::MonacoEditor},
    types::{FunctionTestResponse, OrganisationId, Tenant},
};
use leptos::*;
use serde_json::{from_str, json, Value};
use web_sys::MouseEvent;

const TEMPLATE_FN: &str = r#"async function validate(key, value) {
    return true;
}
"#;

#[component]
pub fn function_editor<NF>(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] function_name: String,
    #[prop(default = String::from(TEMPLATE_FN))] function: String,
    #[prop(default = String::new())] runtime_version: String,
    #[prop(default = String::new())] description: String,
    #[prop(default = String::new())] change_reason: String,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let (function_name, set_function_name) = create_signal(function_name);
    let (function, set_function) = create_signal(function);
    let (runtime_version, set_runtime_version) = create_signal(runtime_version);
    let (error_message, set_error_message) = create_signal("".to_string());
    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(change_reason);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    if !edit {
        set_runtime_version.set("1.0.0".to_string())
    };
    let on_submit = move |event: MouseEvent| {
        req_inprogress_ws.set(true);
        event.prevent_default();
        logging::log!("Submitting function form");

        let tenant = tenant_rws.get().0;
        let org = org_rws.get().0;
        let f_function_name = function_name.get();
        let f_function = function.get();
        let f_runtime_version = runtime_version.get();
        let f_description = description_rs.get();
        let f_change_reason = change_reason_rs.get();
        let handle_submit_clone = handle_submit.clone();

        logging::log!("Function Name in editor: {:?}", function_name);

        spawn_local({
            async move {
                let result = if edit {
                    update_function(
                        f_function_name,
                        f_function,
                        f_runtime_version,
                        f_description,
                        f_change_reason,
                        tenant,
                        org,
                    )
                    .await
                } else {
                    create_function(
                        f_function_name,
                        f_function,
                        f_runtime_version,
                        f_description,
                        f_change_reason,
                        tenant,
                        org,
                    )
                    .await
                };

                match result {
                    Ok(_) => {
                        handle_submit_clone();
                    }
                    Err(e) => {
                        set_error_message.set(e);
                    }
                }
                req_inprogress_ws.set(false);
            }
        });
    };

    view! {
        <div>
            <form id="MyForm">

                <div class="flex flex-row w-full justify-between">
                    <div class="form-group">
                        <MonacoEditor
                            node_id="code_editor_fn"
                            data=function.get_untracked()
                            on_change=move |value| set_function.set(value)
                            classes=vec!["min-w-[1000px]", "min-h-[500px]"]
                        />
                    </div>

                    <div class="mx-auto w-auto" style="width: 250px">

                        <Show when=move || { !edit }>
                            <div class="form-control ">
                                <label class="label">
                                    <span class="label-text">Function Name</span>
                                </label>
                                <input
                                    disabled=edit
                                    value=move || function_name.get()
                                    on:input=move |ev| {
                                        set_function_name.set(event_target_value(&ev))
                                    }

                                    type="text"
                                    name="funName"
                                    id="funName"
                                    placeholder="ex: myfunction"
                                    class="input input-bordered w-full max-w-md"
                                />
                            </div>
                        </Show>

                        <div class="form-control">
                            <label class="label">
                                <span class="label-text">Draft Runtime Version</span>
                            </label>
                            <input
                                disabled=true
                                value=move || runtime_version.get()
                                on:input=move |ev| set_runtime_version.set(event_target_value(&ev))
                                type="text"
                                name="runVersion"
                                id="runVersion"
                                placeholder="Js Runtime Version"
                                class="input input-bordered  "
                            />
                        </div>

                        <div class="form-control">
                            <label class="label">
                                <span class="label-text">Description</span>
                            </label>
                            <textarea
                                type="text"
                                class="input input-bordered shadow-md"
                                name="description"
                                id="description"
                                placeholder="explain function"
                                on:change=move |ev| {
                                    let value = event_target_value(&ev);
                                    logging::log!("{:?}", value);
                                    description_ws.set(value);
                                }
                            >

                                {description_rs.get()}
                            </textarea>
                        </div>

                        <div class="form-control">
                            <label class="label">
                                <span class="label-text">Reason for Change</span>
                            </label>
                            <textarea
                                type="text"
                                class="input input-bordered shadow-md"
                                name="change_reason"
                                id="change_reason"
                                placeholder="Reason for change"
                                on:change=move |ev| {
                                    let value = event_target_value(&ev);
                                    logging::log!("{:?}", value);
                                    change_reason_ws.set(value);
                                }
                            >

                                {change_reason_rs.get()}
                            </textarea>
                        </div>

                        <div class="flex justify-end mt-8">
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

                        <div class="flex">
                            <p class="text-red-500">{move || error_message.get()}</p>
                        </div>
                    </div>

                </div>
            </form>
        </div>
    }
}

#[component]
pub fn test_form(function_name: String, stage: String) -> impl IntoView {
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let (error_message, set_error_message) = create_signal(String::new());
    let (output_message, set_output_message) =
        create_signal::<Option<FunctionTestResponse>>(None);
    let (val, set_val) = create_signal(json!({}));
    let (key, set_key) = create_signal(String::new());
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let on_submit = move |event: MouseEvent| {
        req_inprogress_ws.set(true);
        event.prevent_default();
        logging::log!("Submitting function form");

        let tenant = tenant_rws.get().0;
        let org = org_rws.get().0;
        let f_function_name = function_name.clone();
        let f_val = json!({
            "key": key.get(),
            "value": val.get()
        });
        let f_stage = stage.clone();

        logging::log!("{:?}", function_name);
        logging::log!("{:?}", val);

        spawn_local({
            async move {
                let result =
                    test_function(f_function_name, f_stage, f_val, tenant, org).await;

                match result {
                    Ok(resp) => {
                        set_error_message.set(String::new());
                        set_output_message.set(Some(resp));
                    }
                    Err(e) => {
                        set_output_message.set(None);
                        set_error_message.set(e);
                    }
                }
                req_inprogress_ws.set(false);
            }
        });
    };

    view! {
        <div class="mx-auto w-auto" style="width: 250px">

            <form id="MyForm">

                <div class="form-control">
                    <label class="label">
                        <span class="label-text">Key Name</span>
                    </label>
                    <input
                        disabled=false
                        value=move || key.get()
                        on:input=move |ev| set_key.set(event_target_value(&ev))
                        type="text"
                        name="key name"
                        id="keyName"
                        placeholder="key"
                        class="input input-bordered"
                    />
                </div>

                <div class="form-control">
                    <label class="label">
                        <span class="label-text">Value</span>
                    </label>
                    <textarea
                        type="text"
                        class="input input-bordered shadow-md"
                        name="value"
                        id="value"
                        style="min-height: 150px"
                        placeholder="value"
                        on:change=move |ev| {
                            let value = event_target_value(&ev);
                            match from_str::<Value>(&value) {
                                Ok(test_val) => {
                                    set_val.set(test_val);
                                    set_error_message.set("".to_string());
                                    set_output_message.set(None);
                                }
                                Err(_) => {
                                    set_val.set(json!(value));
                                    set_error_message.set("".to_string());
                                    set_output_message.set(None);
                                }
                            };
                        }
                    >

                        {"".to_string()}
                    </textarea>

                </div>

                <div class="flex justify-end mt-8">
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

                <div class="mt-7">
                    <p class="text-red-500">{move || error_message.get()}</p>
                </div>

                <div>
                    <p class="text-green-700">
                        {move || {
                            output_message
                                .get()
                                .map_or(
                                    String::new(),
                                    |o| { format!("{}\n{}", o.message, o.stdout) },
                                )
                        }}

                    </p>
                </div>

            </form>
        </div>
    }
}
