pub mod utils;

use leptos::*;
use serde_json::{from_str, Value};
use strum::IntoEnumIterator;
use superposition_types::api::functions::{
    FunctionExecutionRequest, FunctionExecutionResponse, Stage,
};
use superposition_types::database::models::cac::FunctionType;
use utils::{create_function, test_function, update_function};
use wasm_bindgen::prelude::*;
use web_sys::MouseEvent;

use crate::components::button::ButtonAnchor;
use crate::components::{
    alert::AlertType,
    button::Button,
    change_form::ChangeForm,
    dropdown::{Dropdown, DropdownDirection},
    form::label::Label,
    monaco_editor::MonacoEditor,
    skeleton::{Skeleton, SkeletonVariant},
};
use crate::providers::alert_provider::enqueue_alert;
use crate::types::{OrganisationId, Tenant};
use crate::utils::use_url_base;

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
// environment: object { context: [Object], overrides: [Object] } - captures out elements in the form like context, overrides etc.
// returns: [string]
async function autocomplete(name, prefix, environment) {
    return [];
}
"#;

impl DropdownOption for FunctionType {
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

#[derive(Clone, PartialEq)]
pub enum Mode {
    Viewer,
    Editor,
    Test,
}

#[component]
pub fn function_editor(
    #[prop(default = true)] edit: bool,
    #[prop(into, default = String::new())] function_name: String,
    #[prop(into, default = String::from(VALIDATE_TEMPLATE_FN))] function: String,
    #[prop(into, default = String::new())] runtime_version: String,
    #[prop(into, default = String::new())] description: String,
    #[prop(default = FunctionType::Validation)] function_type: FunctionType,
    #[prop(into)] handle_submit: Callback<()>,
    #[prop(into, default = Signal::derive(|| Mode::Editor))] mode: Signal<Mode>,
    #[prop(into, default = Signal::derive(|| Stage::Draft))] selected_tab: Signal<Stage>,
) -> impl IntoView {
    let function_name_rws = RwSignal::new(function_name);
    let function_code_rws = RwSignal::new(function);
    let runtime_version_rws = RwSignal::new(if edit {
        runtime_version
    } else {
        "1.0.0".to_string()
    });

    let description_rws = RwSignal::new(description);
    let change_reason_rws = RwSignal::new(String::new());
    let req_inprogress_rws = RwSignal::new(false);
    let function_type_rws = RwSignal::new(function_type);

    let client_side = RwSignal::new(false);
    Effect::new(move |_| client_side.set(true));

    view! {
        <form class="w-full flex flex-col 2xl:flex-row gap-5 justify-between">
            <Show
                when=move || client_side.get()
                fallback=move || {
                    view! {
                        <Skeleton variant=SkeletonVariant::Block style_class="min-w-[800px]" />
                    }
                }
            >
                <div class="w-full min-w-[800px] p-1 bg-base-100 rounded-2xl shadow">
                    <MonacoEditor
                        node_id="code_editor_fn"
                        data=function_code_rws.get()
                        on_change=move |value| function_code_rws.set(value)
                        classes=vec!["w-[inherit] min-h-[500px]"]
                    />
                </div>
            </Show>
            <Show when=move || mode.get() == Mode::Editor>
                <EditForm
                    edit
                    function_name_rws
                    function_code_rws
                    runtime_version_rws
                    description_rws
                    change_reason_rws
                    function_type_rws
                    req_inprogress_rws
                    handle_submit
                />
            </Show>
            <Show when=move || mode.get() == Mode::Test>
                <TestForm
                    function_name=function_name_rws.get()
                    function_args=function_type_rws
                        .with(|f| match f {
                            FunctionType::Validation => {
                                FunctionExecutionRequest::validation_default()
                            }
                            FunctionType::Autocomplete => {
                                FunctionExecutionRequest::autocomplete_default()
                            }
                        })
                    stage=selected_tab.get()
                />
            </Show>
        </form>
    }
}

#[component]
fn edit_form(
    edit: bool,
    function_name_rws: RwSignal<String>,
    function_code_rws: RwSignal<String>,
    runtime_version_rws: RwSignal<String>,
    description_rws: RwSignal<String>,
    change_reason_rws: RwSignal<String>,
    function_type_rws: RwSignal<FunctionType>,
    req_inprogress_rws: RwSignal<bool>,
    #[prop(into)] handle_submit: Callback<()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let error_message_rws = RwSignal::new(String::new());

    let on_submit = move |event: MouseEvent| {
        req_inprogress_rws.set(true);
        event.prevent_default();
        logging::log!("Submitting function form");

        let tenant = workspace.get_untracked().0;
        let org = org.get_untracked().0;
        let f_function_name = function_name_rws.get_untracked();
        let f_function = function_code_rws.get_untracked();
        let f_runtime_version = runtime_version_rws.get_untracked();
        let f_description = description_rws.get_untracked();
        let f_change_reason = change_reason_rws.get_untracked();
        let f_type = function_type_rws.get_untracked();

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

                req_inprogress_rws.set(false);
                match result {
                    Ok(_) => {
                        handle_submit.call(());
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
                        error_message_rws.set(e.clone());
                        enqueue_alert(e, AlertType::Error, 5000);
                    }
                }
            }
        });
    };

    view! {
        <div class="w-full flex flex-col gap-5">
            <div class="w-full flex 2xl:flex-col flex-wrap 2xl:flex-nowrap gap-5">
                <Show when=move || !edit>
                    <div class="form-control w-full max-w-md">
                        <Label title="Function Name" />
                        <input
                            value=move || function_name_rws.get()
                            on:input=move |ev| function_name_rws.set(event_target_value(&ev))
                            type="text"
                            name="funName"
                            id="funName"
                            placeholder="ex: myfunction"
                            class="input input-bordered"
                        />
                    </div>
                    <div class="form-control w-full max-w-md">
                        <Label
                            title="Function Type"
                            description="Changing this would reset your function code"
                        />
                        <Dropdown
                            dropdown_width="w-80"
                            dropdown_icon="ri-functions".to_string()
                            dropdown_text=function_type_rws.get().to_string()
                            dropdown_direction=DropdownDirection::Left
                            dropdown_options=FunctionType::iter().collect()
                            on_select=Callback::new(move |selected: FunctionType| {
                                let code = match selected {
                                    FunctionType::Validation => VALIDATE_TEMPLATE_FN,
                                    FunctionType::Autocomplete => AUTOCOMPLETE_TEMPLATE_FN,
                                };
                                function_code_rws.set(code.to_string());
                                function_type_rws.set(selected);
                            })
                        />
                    </div>
                </Show>

                <div class="form-control w-full max-w-md">
                    <Label title="Draft Runtime Version" />
                    <input
                        disabled=true
                        value=move || runtime_version_rws.get()
                        on:input=move |ev| runtime_version_rws.set(event_target_value(&ev))
                        type="text"
                        name="runVersion"
                        id="runVersion"
                        placeholder="Js Runtime Version"
                        class="input input-bordered"
                    />
                </div>
                <ChangeForm
                    class="w-full max-w-md"
                    title="Description".to_string()
                    placeholder="Enter a description".to_string()
                    value=description_rws.get_untracked()
                    on_change=move |new_description| description_rws.set(new_description)
                />
                <ChangeForm
                    class="w-full max-w-md"
                    title="Reason for Change".to_string()
                    placeholder="Enter a reason for this change".to_string()
                    value=change_reason_rws.get_untracked()
                    on_change=move |new_change_reason| change_reason_rws.set(new_change_reason)
                />
            </div>

            {move || {
                let loading = req_inprogress_rws.get();
                view! {
                    <div class="flex justify-end gap-2">
                        <Button
                            class="h-12 w-48"
                            text="Submit"
                            icon_class="ri-send-plane-line"
                            on_click=on_submit.clone()
                            loading
                        />
                        <ButtonAnchor
                            class="h-12 w-48"
                            text="Cancel"
                            icon_class="ri-forbid-line"
                            href={
                                let base = use_url_base();
                                format!(
                                    "{base}/admin/{}/{}/function",
                                    org.get().0,
                                    workspace.get().0,
                                )
                            }
                            loading
                        />
                    </div>
                }
            }}
            <p class="text-red-500">{move || error_message_rws.get()}</p>
        </div>
    }
}

#[component]
pub fn test_form(
    function_name: String,
    function_args: FunctionExecutionRequest,
    #[prop(into)] stage: Stage,
) -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let (error_message, set_error_message) = create_signal(String::new());
    let (output_message_rs, out_message_ws) =
        create_signal::<Option<FunctionExecutionResponse>>(None);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let (function_args_rs, function_args_ws) = create_signal(function_args);
    let on_submit = move |event: MouseEvent| {
        req_inprogress_ws.set(true);
        event.prevent_default();
        logging::log!("Submitting function form");

        let tenant = workspace.get().0;
        let org = org.get().0;
        let f_function_name = function_name.clone();
        let f_args = function_args_rs.get();

        logging::log!("{:?}", function_name);
        logging::log!("{:?}", f_args);

        spawn_local({
            async move {
                let result =
                    test_function(f_function_name, stage, &f_args, tenant, org).await;

                req_inprogress_ws.set(false);
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
            }
        });
    };

    view! {
        <div class="w-full flex flex-col gap-5">
            <div class="w-full flex 2xl:flex-col flex-wrap 2xl:flex-nowrap gap-5">
                {move || match function_args_rs.get_untracked() {
                    FunctionExecutionRequest::ValidateFunctionRequest { key, value } => {
                        view! {
                            <div class="form-control w-full max-w-md">
                                <Label title="Key Name" />
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

                            <div class="form-control w-full max-w-md">
                                <Label title="Value" />
                                <textarea
                                    type="text"
                                    class="input input-bordered"
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
                            <div class="form-control w-full max-w-md">
                                <Label title="Name" />
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
                            <div class="form-control w-full max-w-md">
                                <Label title="Prefix" />
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

                            <div class="form-control w-full max-w-md">
                                <Label title="Environment" />
                                <textarea
                                    type="text"
                                    class="input input-bordered"
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
            </div>
            {move || {
                let loading = req_inprogess_rs.get();
                view! {
                    <Button
                        class="self-end h-12 w-48"
                        text="Submit"
                        icon_class="ri-send-plane-line"
                        on_click=on_submit.clone()
                        loading
                    />
                }
            }}
            <Show when=move || !error_message.get().is_empty()>
                <p class="text-red-500">{move || error_message.get()}</p>
            </Show>
            {move || {
                output_message_rs
                    .get()
                    .map(|output| {
                        let output_c = output.clone();
                        view! {
                            <p class="text-green-700">
                                {format!("Function returned = {}", output.fn_output)}
                            </p>
                            <Show when=move || !output_c.stdout.is_empty()>
                                <p class="text-green-700">
                                    {format!("Logs: \n {}", output.stdout)}
                                </p>
                            </Show>
                        }
                    })
            }}
        </div>
    }
}
