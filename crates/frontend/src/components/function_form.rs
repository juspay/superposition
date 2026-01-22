pub mod utils;

use leptos::*;
use serde_json::{Value, from_str};
use strum::IntoEnumIterator;
use superposition_types::api::functions::{
    FunctionEnvironment, FunctionExecutionRequest, FunctionExecutionResponse, KeyType,
    Stage,
};
use superposition_types::database::models::cac::{FunctionRuntimeVersion, FunctionType};
use utils::{create_function, test_function, update_function};
use wasm_bindgen::prelude::*;
use web_sys::MouseEvent;

use crate::components::{
    alert::AlertType,
    button::{Button, ButtonAnchor},
    change_form::ChangeForm,
    dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
    form::label::Label,
    input::{Input, InputType},
    monaco_editor::MonacoEditor,
    skeleton::{Skeleton, SkeletonVariant},
    tip::Tip,
};
use crate::providers::editor_provider::EditorProvider;
use crate::providers::{
    alert_provider::enqueue_alert, csr_provider::use_client_side_ready,
};
use crate::schema::{JsonSchemaType, SchemaType};
use crate::types::{OrganisationId, Workspace};
use crate::utils::use_url_base;

use super::dropdown::utils::DropdownOption;

pub const VALUE_VALIDATION_TEMPLATE_FN: &str = r#"Payload structure: {
  version: "1.0",
  value_validate: { key, value, type, environment: { context, overrides } }
}

Returns: boolean
"#;

pub const VALUE_VALIDATION_DEFAULT_FN: &str = r#"async function execute(payload) {
    const { value_validate } = payload;
    const { key, value, type, environment } = value_validate;

    // validation logic goes here

    return true;
}
"#;

pub const VALUE_COMPUTE_TEMPLATE_FN: &str = r#"Payload structure: {
  version: "1.0",
  value_compute: { name, prefix, type, environment: { context, overrides } }
}

Returns: [string]
"#;

pub const VALUE_COMPUTE_DEFAULT_FN: &str = r#"async function execute(payload) {
    const { value_compute } = payload;
    const { name, prefix, type, environment } = value_compute;

    // computation logic goes here

    return [];
}
"#;

pub const CONTEXT_VALIDATION_TEMPLATE_FN: &str = r#"Payload structure: {
  version: "1.0",
  context_validate: { environment: { context, overrides } },
}

Returns: boolean
"#;

pub const CONTEXT_VALIDATION_DEFAULT_FN: &str = r#"async function execute(payload) {
    const { context_validate } = payload;
    const { environment } = context_validate;

    // validation logic goes here

    return true;
}
"#;

pub const CHANGE_REASON_VALIDATION_TEMPLATE_FN: &str = r#"Payload structure: {
  version: "1.0",
  change_reason_validate: { change_reason }
}

Returns: boolean
"#;

pub const CHANGE_REASON_VALIDATION_DEFAULT_FN: &str = r#"async function execute(payload) {
    const { change_reason_validate } = payload;
    const { change_reason } = change_reason_validate;

    // validation logic goes here
    
    return true;
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

impl DropdownOption for FunctionRuntimeVersion {
    fn key(&self) -> String {
        self.to_string()
    }

    fn label(&self) -> String {
        self.to_string()
    }
}

impl DropdownOption for KeyType {
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
    #[prop(into, default = String::from(VALUE_VALIDATION_DEFAULT_FN))] function: String,
    #[prop(into, default = FunctionRuntimeVersion::default())]
    runtime_version: FunctionRuntimeVersion,
    #[prop(into, default = String::new())] description: String,
    #[prop(default = FunctionType::ValueValidation)] function_type: FunctionType,
    #[prop(into)] handle_submit: Callback<String>,
    #[prop(into)] on_cancel: Callback<()>,
    #[prop(into, default = Signal::derive(|| Mode::Editor))] mode: Signal<Mode>,
    #[prop(into, default = Signal::derive(|| Stage::Draft))] selected_tab: Signal<Stage>,
) -> impl IntoView {
    let client_side_ready = use_client_side_ready();
    let function_name_rws = RwSignal::new(function_name);
    let function_code_rws = RwSignal::new(function.clone());
    let runtime_version_rws = RwSignal::new(if edit {
        runtime_version
    } else {
        FunctionRuntimeVersion::default()
    });

    let description_rws = RwSignal::new(description);
    let change_reason_rws = RwSignal::new(String::new());
    let req_inprogress_rws = RwSignal::new(false);
    let function_type_rws = RwSignal::new(function_type);

    let on_cancel = Callback::new(move |_| {
        function_code_rws.set(function.clone());
        on_cancel.call(());
    });

    view! {
        <form class="w-full flex flex-col 2.5xl:flex-row gap-5 justify-between">
            <Show
                when=move || *client_side_ready.get()
                fallback=move || {
                    view! { <Skeleton variant=SkeletonVariant::Block style_class="w-full" /> }
                }
            >
                {
                    let function_code_signature = match function_type_rws.get() {
                        FunctionType::ValueValidation => VALUE_VALIDATION_TEMPLATE_FN,
                        FunctionType::ValueCompute => VALUE_COMPUTE_TEMPLATE_FN,
                        FunctionType::ContextValidation => CONTEXT_VALIDATION_TEMPLATE_FN,
                        FunctionType::ChangeReasonValidation => CHANGE_REASON_VALIDATION_TEMPLATE_FN,
                    };

                    view! {
                        <div class="w-full min-w-[800px] flex flex-col gap-3">
                            <Show when=move || mode.get() == Mode::Editor>
                                <Tip
                                    message="Reference variables using"
                                    code_snippet="VARS.KEY_NAME"
                                    example="VARS.API_KEY"
                                    code_signature=function_code_signature
                                />
                            </Show>
                            <MonacoEditor
                                node_id="code_editor_fn"
                                data=function_code_rws.get_untracked()
                                on_change=move |value| function_code_rws.set(value)
                                classes=vec![
                                    "w-[inherit] min-h-[500px] p-1 bg-base-100 rounded-2xl shadow",
                                ]
                                read_only=mode.with(|v| *v != Mode::Editor)
                                language=crate::components::monaco_editor::Languages::Javascript
                            />
                        </div>
                    }
                }
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
                    on_cancel
                />
            </Show>
            <Show when=move || mode.get() == Mode::Test>
                <TestForm
                    function_name=function_name_rws.get()
                    function_args=function_type_rws.with(|f| FunctionExecutionRequest::from(f))
                    stage=selected_tab.get()
                    on_cancel
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
    runtime_version_rws: RwSignal<FunctionRuntimeVersion>,
    description_rws: RwSignal<String>,
    change_reason_rws: RwSignal<String>,
    function_type_rws: RwSignal<FunctionType>,
    req_inprogress_rws: RwSignal<bool>,
    #[prop(into)] handle_submit: Callback<String>,
    #[prop(into)] on_cancel: Callback<()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let error_message_rws = RwSignal::new(String::new());

    let on_submit = move |event: MouseEvent| {
        req_inprogress_rws.set(true);
        event.prevent_default();
        logging::log!("Submitting function form");

        let workspace = workspace.get_untracked().0;
        let org = org.get_untracked().0;
        let f_function_name = function_name_rws.get_untracked();
        let f_function = function_code_rws.get_untracked();
        let f_runtime_version = runtime_version_rws.get_untracked();
        let f_description = description_rws.get_untracked();
        let f_change_reason = change_reason_rws.get_untracked();
        let f_type = function_type_rws.get_untracked();

        spawn_local({
            async move {
                let f_name = f_function_name.clone();
                let result = if edit {
                    update_function(
                        f_function_name,
                        f_function,
                        f_runtime_version,
                        f_description,
                        f_change_reason,
                        &workspace,
                        &org,
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
                        &workspace,
                        &org,
                    )
                    .await
                };

                req_inprogress_rws.set(false);
                match result {
                    Ok(_) => {
                        handle_submit.call(f_name);
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
        <div class="w-full 2.5xl:max-w-xl flex flex-col gap-5">
            <div class="w-full flex 2.5xl:flex-col flex-wrap 2.5xl:flex-nowrap gap-5">
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
                            dropdown_text=function_type_rws.get().to_string()
                            dropdown_direction=DropdownDirection::Down
                            dropdown_btn_type=DropdownBtnType::Select
                            dropdown_options=FunctionType::iter()
                                .filter(|ft| {
                                    !matches!(
                                        ft,
                                        FunctionType::ContextValidation
                                        | FunctionType::ChangeReasonValidation
                                    )
                                })
                                .collect()
                            on_select=move |selected: FunctionType| {
                                let code = match selected {
                                    FunctionType::ValueValidation => VALUE_VALIDATION_DEFAULT_FN,
                                    FunctionType::ValueCompute => VALUE_COMPUTE_DEFAULT_FN,
                                    FunctionType::ContextValidation => CONTEXT_VALIDATION_DEFAULT_FN,
                                    FunctionType::ChangeReasonValidation => {
                                        CHANGE_REASON_VALIDATION_DEFAULT_FN
                                    }
                                };
                                function_code_rws.set(code.to_string());
                                function_type_rws.set(selected);
                            }
                        />
                    </div>
                </Show>
                <div class="form-control w-full max-w-md">
                    <Label title="Draft Runtime Version" />
                    <Dropdown
                        dropdown_width="w-100"
                        dropdown_icon="".to_string()
                        dropdown_text=runtime_version_rws.get_untracked().to_string()
                        dropdown_direction=DropdownDirection::Down
                        dropdown_btn_type=DropdownBtnType::Select
                        dropdown_options=FunctionRuntimeVersion::iter().collect()
                        on_select=Callback::new(move |selected_item: FunctionRuntimeVersion| {
                            logging::log!("selected item {:?}", selected_item);
                            runtime_version_rws.set(selected_item);
                        })
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
                            text=if edit { "Edit" } else { "Create" }
                            icon_class=if edit { "ri-edit-2-line" } else { "ri-add-line" }
                            on_click=on_submit
                            loading
                        />
                        <Show when=move || edit>
                            <Button
                                class="h-12 w-48"
                                text="Cancel"
                                icon_class="ri-forbid-line"
                                on_click=move |_| on_cancel.call(())
                                loading
                            />
                        </Show>
                        <Show when=move || !edit>
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
                        </Show>
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
    #[prop(into)] on_cancel: Callback<()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
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

        let f_function_name = function_name.clone();
        let f_args = function_args_rs.get();

        logging::log!("{:?}", function_name);
        logging::log!("{:?}", f_args);

        spawn_local({
            async move {
                let workspace = workspace.get_untracked();
                let org = org.get_untracked();
                let result =
                    test_function(f_function_name, stage, &f_args, &workspace, &org)
                        .await;

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
        <div class="w-full 2.5xl:max-w-xl flex flex-col gap-5">
            <div class="w-full flex 2.5xl:flex-col flex-wrap 2.5xl:flex-nowrap gap-5">
                {match function_args_rs.get_untracked() {
                    FunctionExecutionRequest::ValueValidationFunctionRequest { key, value, .. } => {
                        view! {
                            <div class="form-control w-full max-w-md">
                                <Label title="Key Name" />
                                <input
                                    disabled=false
                                    value=key
                                    on:input=move |ev| {
                                        function_args_ws
                                            .update(|args| {
                                                if let FunctionExecutionRequest::ValueValidationFunctionRequest {
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
                                    class="textarea textarea-bordered"
                                    name="value"
                                    id="value"
                                    placeholder="value"
                                    on:change=move |ev| {
                                        let value = event_target_value(&ev);
                                        match from_str::<Value>(&value) {
                                            Ok(test_val) => {
                                                function_args_ws
                                                    .update(|args| {
                                                        if let FunctionExecutionRequest::ValueValidationFunctionRequest {
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
                            .into_view()
                    }
                    _ => ().into_view(),
                }}
                {match function_args_rs.get_untracked() {
                    FunctionExecutionRequest::ValueComputeFunctionRequest { name, prefix, .. } => {
                        view! {
                            <div class="form-control w-full max-w-md">
                                <Label title="Name" />
                                <input
                                    disabled=false
                                    value=name
                                    on:input=move |ev| {
                                        function_args_ws
                                            .update(|args| {
                                                if let FunctionExecutionRequest::ValueComputeFunctionRequest {
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
                                                if let FunctionExecutionRequest::ValueComputeFunctionRequest {
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
                        }
                            .into_view()
                    }
                    _ => ().into_view(),
                }}
                {match function_args_rs.get_untracked() {
                    FunctionExecutionRequest::ChangeReasonValidationFunctionRequest {
                        change_reason,
                    } => {
                        view! {
                            <ChangeForm
                                title="Change Reason".to_string()
                                placeholder="Enter a reason for this change".to_string()
                                value=change_reason.to_string()
                                on_change=Callback::new(move |reason| {
                                    function_args_ws
                                        .update(|args| {
                                            if let FunctionExecutionRequest::ChangeReasonValidationFunctionRequest {
                                                change_reason,
                                            } = args {
                                                **change_reason = reason;
                                            }
                                        });
                                })
                            />
                        }
                    }
                    _ => ().into_view(),
                }}
                {match function_args_rs.get_untracked() {
                    FunctionExecutionRequest::ValueValidationFunctionRequest { r#type, .. }
                    | FunctionExecutionRequest::ValueComputeFunctionRequest { r#type, .. } => {
                        view! {
                            <div class="form-control w-full max-w-md">
                                <Label title="Key Type" />
                                <Dropdown
                                    dropdown_width="w-100"
                                    dropdown_icon="".to_string()
                                    dropdown_text=r#type.to_string()
                                    dropdown_direction=DropdownDirection::Down
                                    dropdown_btn_type=DropdownBtnType::Select
                                    dropdown_options=KeyType::iter().collect()
                                    on_select=Callback::new(move |selected_item: KeyType| {
                                        logging::log!("selected item {:?}", selected_item);
                                        function_args_ws
                                            .update(|args| {
                                                match args {
                                                    FunctionExecutionRequest::ValueValidationFunctionRequest {
                                                        r#type,
                                                        ..
                                                    }
                                                    | FunctionExecutionRequest::ValueComputeFunctionRequest {
                                                        r#type,
                                                        ..
                                                    } => {
                                                        *r#type = selected_item;
                                                    }
                                                    _ => {}
                                                }
                                            });
                                    })
                                />
                            </div>
                        }
                            .into_view()
                    }
                    _ => ().into_view(),
                }}
                {match function_args_rs.get_untracked() {
                    FunctionExecutionRequest::ValueValidationFunctionRequest { environment, .. }
                    | FunctionExecutionRequest::ValueComputeFunctionRequest { environment, .. }
                    | FunctionExecutionRequest::ContextValidationFunctionRequest { environment } => {
                        view! {
                            <div class="form-control w-full max-w-md">
                                <Label title="Environment" />
                                <EditorProvider>
                                    <Input
                                        id="function-environment-input"
                                        class="rounded-md resize-y w-full max-w-md"
                                        schema_type=SchemaType::Single(JsonSchemaType::Object)
                                        value=serde_json::to_value(environment).unwrap_or_default()
                                        on_change=move |value| {
                                            match serde_json::from_value::<FunctionEnvironment>(value) {
                                                Ok(test_val) => {
                                                    function_args_ws
                                                        .update(|args| {
                                                            match args {
                                                                FunctionExecutionRequest::ValueValidationFunctionRequest {
                                                                    environment,
                                                                    ..
                                                                }
                                                                | FunctionExecutionRequest::ValueComputeFunctionRequest {
                                                                    environment,
                                                                    ..
                                                                }
                                                                | FunctionExecutionRequest::ContextValidationFunctionRequest {
                                                                    environment,
                                                                } => {
                                                                    *environment = test_val;
                                                                }
                                                                _ => {}
                                                            }
                                                        });
                                                    set_error_message.set("".to_string());
                                                    out_message_ws.set(None);
                                                }
                                                Err(_) => {
                                                    set_error_message.set("".to_string());
                                                    out_message_ws.set(None);
                                                }
                                            }
                                        }
                                        r#type=InputType::Monaco(vec![])
                                    />
                                </EditorProvider>
                            </div>
                        }
                            .into_view()
                    }
                    _ => ().into_view(),
                }}
            </div>
            {move || {
                let loading = req_inprogess_rs.get();
                view! {
                    <div class="flex justify-end gap-2">
                        <Button
                            class="h-12 w-48"
                            text="Test"
                            icon_class="ri-microscope-line"
                            on_click=on_submit.clone()
                            loading
                        />
                        <Button
                            class="h-12 w-48"
                            text="Cancel"
                            icon_class="ri-forbid-line"
                            on_click=move |_| on_cancel.call(())
                            loading
                        />
                    </div>
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
