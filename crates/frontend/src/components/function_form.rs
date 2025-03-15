pub mod types;
pub mod utils;

use self::utils::{create_function, test_function, update_function};
use crate::providers::alert_provider::enqueue_alert;
use crate::{
    components::{
        alert::AlertType,
        button::Button,
        dropdown::{Dropdown, DropdownDirection},
        monaco_editor::MonacoEditor,
    },
    types::{OrganisationId, Tenant},
};
use leptos::*;
use serde_json::{from_str, Value};
use strum::IntoEnumIterator;
use superposition_types::api::functions::{
    FunctionExecutionRequest, FunctionExecutionResponse,
};
use superposition_types::database::models::cac::FunctionTypes;
use wasm_bindgen::prelude::*;
use web_sys::MouseEvent;

use super::dropdown::utils::DropdownOption;

const VALIDATE_TEMPLATE_FN: &str = r#"// key: string - dimension or config name
// value: string - obeys the json schema type defined, json parse this if you want an object
// returns: boolean
async function validate(key, value) {
    return true;
}
"#;

const AUTOCOMPLETE_TEMPLATE_FN: &str = r#"// name: string - dimension or config name
// prefix: string - characters entered in the input field
// environment: object { context: Object, overrides: Object } - captures out elements in the form like context, overrides etc.
// returns: [string]
async function autocomplete(name, prefix, environment) {
    return [];
}
"#;

impl DropdownOption for FunctionTypes {
    fn key(&self) -> String {
        self.to_string()
    }

    fn label(&self) -> String {
        self.to_string()
    }
}

#[wasm_bindgen(module = "/src-js/utils.js")]
extern "C" {
    #[wasm_bindgen(js_name = setEditorCode)]
    fn set_editor_code(code: String);
}

#[component]
pub fn function_editor<NF>(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] function_name: String,
    #[prop(default = String::from(VALIDATE_TEMPLATE_FN))] function: String,
    #[prop(default = String::new())] runtime_version: String,
    #[prop(default = String::new())] description: String,
    #[prop(default = String::new())] change_reason: String,
    #[prop(default = FunctionTypes::Validation)] function_type: FunctionTypes,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let (function_name, set_function_name) = create_signal(function_name);
    let (function_code_rs, function_code_ws) = create_signal(function);
    let (runtime_version, set_runtime_version) = create_signal(runtime_version);
    let (error_message, set_error_message) = create_signal("".to_string());
    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(change_reason);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let (function_type_rs, function_type_ws) = create_signal(function_type);
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
        let f_function = function_code_rs.get();
        let f_runtime_version = runtime_version.get();
        let f_description = description_rs.get();
        let f_change_reason = change_reason_rs.get();
        let f_type = function_type_rs.get();
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
                        f_type,
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
                        f_type,
                        tenant,
                        org,
                    )
                    .await
                };

                match result {
                    Ok(_) => {
                        handle_submit_clone();
                        let success_message = if edit {
                            "Function updated successfully!"
                        } else {
                            "New Function created successfully!"
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
        <div>
            <form id="MyForm">

                <div class="flex flex-row w-full justify-between">
                    {move || {
                        function_type_rs
                            .with(|function_type| {
                                view! {
                                    <div
                                        class="form-group"
                                        id=format!("code_editor_fn_{}", function_type)
                                    >
                                        <MonacoEditor
                                            node_id="code_editor_fn"
                                            data=function_code_rs.get_untracked()
                                            on_change=move |value| function_code_ws.set(value)
                                            classes=vec!["min-w-[1000px]", "min-h-[500px]"]
                                        />
                                    </div>
                                }
                            })
                    }} <div class="mx-auto w-auto" style="width: 250px">

                        <Show when=move || { !edit }>
                            <div class="form-control">
                                <Dropdown
                                    dropdown_width="w-80"
                                    dropdown_icon="ri-functions".to_string()
                                    dropdown_text=function_type_rs.get().to_string()
                                    dropdown_direction=DropdownDirection::Down
                                    dropdown_options=FunctionTypes::iter().collect()
                                    on_select=Callback::new(move |selected: FunctionTypes| {
                                        let code = match selected {
                                            FunctionTypes::Validation => VALIDATE_TEMPLATE_FN,
                                            FunctionTypes::Autocomplete => AUTOCOMPLETE_TEMPLATE_FN,
                                        };
                                        function_code_ws.set(code.to_string());
                                        function_type_ws.set(selected);
                                    })
                                />
                            </div>
                            <div class="form-control">
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
pub fn test_form(
    function_name: String,
    function_args: FunctionExecutionRequest,
    stage: String,
) -> impl IntoView {
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let (error_message, set_error_message) = create_signal(String::new());
    let (output_message_rs, out_message_ws) =
        create_signal::<Option<FunctionExecutionResponse>>(None);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let (function_args_rs, function_args_ws) = create_signal(function_args);
    let on_submit = move |event: MouseEvent| {
        req_inprogress_ws.set(true);
        event.prevent_default();
        logging::log!("Submitting function form");

        let tenant = tenant_rws.get().0;
        let org = org_rws.get().0;
        let f_function_name = function_name.clone();
        let f_args = function_args_rs.get();
        let f_stage = stage.clone();

        logging::log!("{:?}", function_name);
        logging::log!("{:?}", f_args);

        spawn_local({
            async move {
                let result =
                    test_function(f_function_name, f_stage, &f_args, tenant, org).await;

                match result {
                    Ok(resp) => {
                        set_error_message.set(String::new());
                        out_message_ws.set(Some(resp));
                    }
                    Err(e) => {
                        out_message_ws.set(None);
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

                {move || match function_args_rs.get_untracked() {
                    FunctionExecutionRequest::ValidateFunctionRequest { key, value } => {
                        view! {
                            <div class="form-control">
                                <label class="label">
                                    <span class="label-text">Key Name</span>
                                </label>
                                <input
                                    disabled=false
                                    value=key
                                    on:input=move |ev| {
                                        function_args_ws
                                            .update(|args| {
                                                if let FunctionExecutionRequest::ValidateFunctionRequest {
                                                    key,
                                                    ..
                                                } = args {
                                                    *key = event_target_value(&ev);
                                                }
                                            })
                                    }
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
                                                function_args_ws
                                                    .update(|args| {
                                                        if let FunctionExecutionRequest::ValidateFunctionRequest {
                                                            value,
                                                            ..
                                                        } = args {
                                                            *value = test_val;
                                                        }
                                                    });
                                                set_error_message.set("".to_string());
                                                out_message_ws.set(None);
                                            }
                                            Err(_) => {
                                                set_error_message.set("".to_string());
                                                out_message_ws.set(None);
                                            }
                                        };
                                    }
                                >
                                    {value.to_string()}
                                </textarea>
                            </div>
                        }
                    }
                    FunctionExecutionRequest::AutocompleteFunctionRequest {
                        name,
                        prefix,
                        environment,
                    } => {
                        view! {
                            <div class="form-control">
                                <label class="label">
                                    <span class="label-text">Name</span>
                                </label>
                                <input
                                    disabled=false
                                    value=name
                                    on:input=move |ev| {
                                        function_args_ws
                                            .update(|args| {
                                                if let FunctionExecutionRequest::AutocompleteFunctionRequest {
                                                    name,
                                                    ..
                                                } = args {
                                                    *name = event_target_value(&ev);
                                                }
                                            })
                                    }
                                    type="text"
                                    name="key name"
                                    id="keyName"
                                    placeholder="key"
                                    class="input input-bordered"
                                />
                            </div>
                            <div class="form-control">
                                <label class="label">
                                    <span class="label-text">Prefix</span>
                                </label>
                                <input
                                    disabled=false
                                    value=prefix
                                    on:input=move |ev| {
                                        function_args_ws
                                            .update(|args| {
                                                if let FunctionExecutionRequest::AutocompleteFunctionRequest {
                                                    prefix,
                                                    ..
                                                } = args {
                                                    *prefix = event_target_value(&ev);
                                                }
                                            })
                                    }
                                    type="text"
                                    name="key name"
                                    id="keyName"
                                    placeholder="key"
                                    class="input input-bordered"
                                />
                            </div>

                            <div class="form-control">
                                <label class="label">
                                    <span class="label-text">Environment</span>
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
                                                function_args_ws
                                                    .update(|args| {
                                                        if let FunctionExecutionRequest::AutocompleteFunctionRequest {
                                                            environment,
                                                            ..
                                                        } = args {
                                                            *environment = test_val;
                                                        }
                                                    });
                                                set_error_message.set("".to_string());
                                                out_message_ws.set(None);
                                            }
                                            Err(_) => {
                                                set_error_message.set("".to_string());
                                                out_message_ws.set(None);
                                            }
                                        };
                                    }
                                >
                                    {environment.to_string()}
                                </textarea>
                            </div>
                        }
                    }
                }}
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
                            output_message_rs
                                .get()
                                .map_or(
                                    String::new(),
                                    |o| { format!("Function returned = {}\n", o.fn_output) },
                                )
                        }}

                    </p>
                </div>
                <div>
                    <p class="text-green-700">
                        {move || {
                            output_message_rs
                                .get()
                                .map_or(
                                    String::new(),
                                    |o| { format!("logs: \n {}", o.stdout) },
                                )
                        }}

                    </p>
                </div>
            </form>
        </div>
    }
}
