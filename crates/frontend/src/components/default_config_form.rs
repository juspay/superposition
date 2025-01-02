pub mod types;
pub mod utils;

use leptos::*;
use serde_json::{json, Value};
use superposition_types::{
    cac::models::{Function, TypeTemplate},
    custom_query::PaginationParams,
};
use web_sys::MouseEvent;

use crate::providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider};
use crate::{
    api::{fetch_functions, fetch_types},
    components::{
        alert::AlertType,
        button::Button,
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        input::{Input, InputType},
    },
    schema::{EnumVariants, JsonSchemaType, SchemaType},
    types::FunctionsName,
};

use self::{
    types::{DefaultConfigCreateReq, DefaultConfigUpdateReq},
    utils::{create_default_config, update_default_config},
};

#[component]
pub fn default_config_form<NF>(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] config_key: String,
    #[prop(default = String::new())] config_type: String,
    #[prop(default = Value::Null)] type_schema: Value,
    #[prop(default = Value::Null)] config_value: Value,
    #[prop(default = None)] function_name: Option<Value>,
    #[prop(default = None)] prefix: Option<String>,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let (config_key_rs, config_key_ws) = create_signal(config_key);
    let (config_type_rs, config_type_ws) = create_signal(config_type);
    let (config_schema_rs, config_schema_ws) = create_signal(type_schema);
    let (config_value_rs, config_value_ws) = create_signal(config_value);
    let (function_name_rs, function_name_ws) = create_signal(function_name);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let functions_resource: Resource<String, Vec<Function>> = create_blocking_resource(
        move || tenant_rs.get(),
        |current_tenant| async move {
            fetch_functions(&PaginationParams::all_entries(), current_tenant)
                .await
                .map_or_else(|_| vec![], |data| data.data)
        },
    );

    let type_template_resource = create_blocking_resource(
        move || tenant_rs.get(),
        |current_tenant| async move {
            fetch_types(&PaginationParams::all_entries(), current_tenant)
                .await
                .map_or_else(|_| vec![], |response| response.data)
        },
    );

    let handle_select_dropdown_option =
        Callback::new(move |selected_function: FunctionsName| {
            function_name_ws.update(|value| {
                let function_name = selected_function.clone();
                leptos::logging::log!("function selected: {:?}", function_name);
                let fun_name = match function_name.as_str() {
                    "None" => None,
                    _ => Some(json!(function_name)),
                };
                *value = fun_name;
            });
        });

    let on_submit = move |ev: MouseEvent| {
        req_inprogress_ws.set(true);
        ev.prevent_default();
        let f_name = prefix.clone().map_or_else(
            || config_key_rs.get(),
            |prefix| prefix + &config_key_rs.get(),
        );
        let f_schema = config_schema_rs.get();
        let f_value = config_value_rs.get();

        let fun_name = function_name_rs.get();

        let create_payload = DefaultConfigCreateReq {
            key: config_key_rs.get(),
            schema: f_schema.clone(),
            value: f_value.clone(),
            function_name: fun_name.clone(),
        };

        let update_payload = DefaultConfigUpdateReq {
            schema: f_schema,
            value: f_value,
            function_name: fun_name,
        };

        let handle_submit_clone = handle_submit.clone();
        let is_edit = edit;
        spawn_local({
            let handle_submit = handle_submit_clone;
            async move {
                let result = if is_edit {
                    // Call update_default_config when edit is true
                    update_default_config(f_name, tenant_rs.get(), update_payload).await
                } else {
                    // Call create_default_config when edit is false
                    create_default_config(tenant_rs.get(), create_payload).await
                };

                match result {
                    Ok(_) => {
                        handle_submit();
                        let success_message = if is_edit {
                            "Default config updated successfully!"
                        } else {
                            "New default config created successfully!"
                        };
                        enqueue_alert(
                            String::from(success_message),
                            AlertType::Success,
                            5000,
                        );
                    }
                    Err(e) => {
                        logging::error!(
                            "An error occurred while trying to {} the default config: {}",
                            if is_edit { "update" } else { "create" },
                            e
                        );
                        enqueue_alert(e, AlertType::Error, 5000);
                    }
                }
                req_inprogress_ws.set(false);
            }
        });
    };
    view! {
        <EditorProvider>
            <form class="form-control w-full space-y-4 bg-white text-gray-700 font-mono">

                <div class="form-control">
                    <label class="label">
                        <span class="label-text">Key Name</span>
                    </label>
                    <input
                        disabled=edit
                        type="text"
                        placeholder="Key"
                        class="input input-bordered w-full max-w-md"
                        value=config_key_rs.get_untracked()
                        on:change=move |ev| {
                            let value = event_target_value(&ev);
                            config_key_ws.set(value);
                        }
                    />

                </div>

                <div class="divider"></div>
                <Suspense>
                    {move || {
                        let options = type_template_resource.get().unwrap_or(vec![]);
                        let config_t = if config_type_rs.get().is_empty() && edit {
                            "change current type template".into()
                        } else if config_type_rs.get().is_empty() && !edit {
                            "Choose a type template".into()
                        } else {
                            config_type_rs.get()
                        };
                        let config_type_schema = SchemaType::Single(
                            JsonSchemaType::from(&config_schema_rs.get()),
                        );
                        view! {
                            <div class="form-control">
                                <label class="label">
                                    <span class="label-text">Set Schema</span>
                                </label>
                                <Dropdown
                                    dropdown_width="w-100"
                                    dropdown_icon="".to_string()
                                    dropdown_text=config_t
                                    dropdown_direction=DropdownDirection::Down
                                    dropdown_btn_type=DropdownBtnType::Select
                                    dropdown_options=options
                                    on_select=Callback::new(move |selected_item: TypeTemplate| {
                                        logging::log!("selected item {:?}", selected_item);
                                        config_type_ws.set(selected_item.type_name);
                                        config_schema_ws.set(selected_item.type_schema);
                                    })
                                />

                                <Input
                                    id="type-schema"
                                    class="mt-5 rounded-md resize-y w-full max-w-md pt-3"
                                    schema_type=config_type_schema
                                    value=config_schema_rs.get()
                                    on_change=Callback::new(move |new_config_schema| {
                                        config_schema_ws.set(new_config_schema)
                                    })
                                    r#type=InputType::Monaco
                                />

                            </div>
                        }
                    }}

                </Suspense>
                <div class="divider"></div>

                {move || {
                    let input_format = match (
                        SchemaType::try_from(config_schema_rs.get()),
                        EnumVariants::try_from(config_schema_rs.get()),
                    ) {
                        (Ok(schema_type), Ok(enum_variants)) => {
                            let input_type = InputType::from((schema_type.clone(), enum_variants));
                            let class = match input_type {
                                InputType::Toggle => String::new(),
                                InputType::Select(_) => "mt-2".into(),
                                InputType::Integer | InputType::Number => "w-full max-w-md".into(),
                                _ => "rounded-md resize-y w-full max-w-md".into(),
                            };
                            view! {
                                <Input
                                    id="default-config-value-input"
                                    class
                                    schema_type
                                    value=config_value_rs.get()
                                    on_change=Callback::new(move |new_default_config: Value| {
                                        logging::log!(
                                            "new value entered for default config = {:?}", new_default_config
                                        );
                                        config_value_ws.set(new_default_config);
                                    })
                                    r#type=input_type
                                />
                            }
                                .into_view()
                        }
                        _ => {
                            let config_value = match config_value_rs.get() {
                                Value::String(s) => s,
                                _ => String::new(),
                            };
                            view! {
                                <textarea
                                    type="text"
                                    placeholder="Value"
                                    class="input input-bordered w-full max-w-md pt-3"
                                    on:change=move |ev| {
                                        logging::log!(
                                            "changing default config value with a text area = {:?}", event_target_value(&ev)
                                        );
                                        config_value_ws.set(Value::String(event_target_value(&ev)));
                                    }
                                >
                                    {config_value}
                                </textarea>
                            }
                                .into_view()
                        }
                    };
                    view! {
                        <div class="form-control">
                            <label class="label">
                                <span class="label-text">Default Value</span>
                            </label>
                            {input_format}
                        </div>
                        <div class="divider"></div>
                    }
                }}

                <Suspense>
                    {move || {
                        let functions = functions_resource.get().unwrap_or_default();
                        let mut function_names: Vec<FunctionsName> = vec![];
                        functions
                            .into_iter()
                            .for_each(|ele| {
                                function_names.push(ele.function_name);
                            });
                        function_names.sort();
                        function_names.insert(0, "None".to_string());
                        view! {
                            <div class="form-control">
                                <div class="gap-1">
                                    <label class="label flex-col justify-center items-start">
                                        <span class="label-text">Function Name</span>
                                        <span class="label-text text-slate-400">
                                            Assign Function validation to your key
                                        </span>
                                    </label>
                                </div>

                                <div class="mt-2">
                                    <Dropdown
                                        dropdown_width="w-100"
                                        dropdown_icon="".to_string()
                                        dropdown_text=function_name_rs
                                            .get()
                                            .and_then(|v| match v {
                                                Value::String(s) => Some(s),
                                                _ => None,
                                            })
                                            .map_or("Add Function".to_string(), |v| v.to_string())
                                        dropdown_direction=DropdownDirection::Down
                                        dropdown_btn_type=DropdownBtnType::Select
                                        dropdown_options=function_names
                                        on_select=handle_select_dropdown_option
                                    />
                                </div>
                            </div>
                        }
                    }}

                </Suspense>

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
        </EditorProvider>
    }
}
