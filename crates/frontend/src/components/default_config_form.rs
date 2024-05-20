pub mod types;
pub mod utils;

use leptos::*;
use serde_json::{json, Value};
use std::str::FromStr;
use web_sys::MouseEvent;

use crate::{
    api::{fetch_functions, fetch_types},
    components::{
        button::Button,
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
    },
    types::{CustomType, FunctionsName},
};

use self::{types::DefaultConfigCreateReq, utils::create_default_config};

#[component]
pub fn default_config_form<NF>(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] config_key: String,
    #[prop(default = String::new())] config_type: String,
    #[prop(default = Value::Null)] type_schema: Value,
    #[prop(default = String::new())] config_value: String,
    #[prop(default = None)] function_name: Option<Value>,
    #[prop(default = None)] prefix: Option<String>,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let (config_key, set_config_key) = create_signal(config_key);
    let (config_type_rs, config_type_ws) = create_signal(config_type);
    let (config_schema_rs, config_schema_ws) = create_signal(type_schema);
    let (config_value, set_config_value) = create_signal(config_value);
    let (function_name, set_function_name) = create_signal(function_name);

    let string_to_value_closure = |val: String| {
        Value::from_str(&val).unwrap_or_else(|_| {
            // do this for Value::String, since for some reason from_str
            // cannot convert unquoted rust strings to Value::String
            Value::from_str(format!("\"{}\"", val).as_str())
                .expect("Invalid default config value")
        })
    };

    let functions_resource: Resource<String, Vec<crate::types::FunctionResponse>> =
        create_blocking_resource(
            move || tenant_rs.get(),
            |current_tenant| async move {
                match fetch_functions(current_tenant).await {
                    Ok(data) => data,
                    Err(_) => vec![],
                }
            },
        );

    let type_template_resource = create_blocking_resource(
        move || tenant_rs.get(),
        |current_tenant| async move {
            match fetch_types(current_tenant, 1, 10000).await {
                Ok(response) => response.data,
                Err(_) => vec![],
            }
        },
    );

    let handle_select_dropdown_option = move |selected_function: FunctionsName| {
        set_function_name.update(|value| {
            let function_name = selected_function.clone();
            leptos::logging::log!("function selected: {:?}", function_name);
            let fun_name = match function_name.as_str() {
                "None" => None,
                _ => Some(json!(function_name)),
            };
            *value = fun_name;
        });
    };

    let (error_message, set_error_message) = create_signal("".to_string());

    let on_submit = move |ev: MouseEvent| {
        ev.prevent_default();
        let f_name = prefix
            .clone()
            .map_or_else(|| config_key.get(), |prefix| prefix + &config_key.get());
        let f_schema = config_schema_rs.get();
        let f_value = config_value.get();

        let fun_name = function_name.get();
        let f_value = string_to_value_closure(f_value);

        let payload = DefaultConfigCreateReq {
            schema: f_schema,
            value: f_value,
            function_name: fun_name,
        };

        let handle_submit_clone = handle_submit.clone();
        spawn_local({
            let handle_submit = handle_submit_clone;
            async move {
                let result = create_default_config(
                    f_name.clone(),
                    tenant_rs.get(),
                    payload.clone(),
                )
                .await;

                match result {
                    Ok(_) => {
                        handle_submit();
                    }
                    Err(e) => {
                        set_error_message.set(e);
                        // Handle error
                        // Consider logging or displaying the error
                    }
                }
            }
        });
    };
    view! {
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
                    value=config_key.get()
                    on:change=move |ev| {
                        let value = event_target_value(&ev);
                        set_config_key.set(value);
                    }
                />

            </div>

            <div class="divider"></div>
            <Suspense>
                {move || {
                    let options = type_template_resource.get().unwrap_or(vec![]);
                    let config_t = if config_type_rs.get().is_empty() {
                        "Choose a type template".into()
                    } else {
                        config_type_rs.get()
                    };
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
                                on_select=Box::new(move |selected_item: CustomType| {
                                    logging::log!("selected item {:?}", selected_item);
                                    config_type_ws.set(selected_item.type_name);
                                    config_schema_ws.set(selected_item.type_schema);
                                })
                            />

                            <textarea
                                type="text"
                                placeholder="JSON schema"
                                class="input input-bordered mt-5 rounded-md resize-y w-full max-w-md"
                                rows=8
                                on:change=move |ev| {
                                    config_schema_ws
                                        .set(string_to_value_closure(event_target_value(&ev)))
                                }
                            >

                                {format!("{}", config_schema_rs.get())}
                            </textarea>

                        </div>
                    }
                }}

            </Suspense>
            <div class="divider"></div>

            {move || {
                config_type_rs
                    .with(|c_type| {
                        let input_format = match c_type.as_str() {
                            "Number" | "Decimal" => {
                                view! {
                                    <input
                                        type="number"
                                        placeholder="Value"
                                        class="input input-bordered w-full max-w-md"
                                        value=config_value.get()
                                        on:change=move |ev| {
                                            logging::log!("{:?}", event_target_value(& ev));
                                            set_config_value.set(event_target_value(&ev));
                                        }
                                    />
                                }
                                    .into_view()
                            }
                            _ => {
                                view! {
                                    <textarea
                                        type="text"
                                        placeholder="Value"
                                        class="input input-bordered w-full max-w-md"
                                        on:change=move |ev| {
                                            logging::log!("{:?}", event_target_value(& ev));
                                            set_config_value.set(event_target_value(&ev));
                                        }
                                    >

                                        {config_value.get()}
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
                    })
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
                                    dropdown_text=function_name
                                        .get()
                                        .and_then(|v| match v {
                                            Value::String(s) => Some(s),
                                            _ => None,
                                        })
                                        .map_or("Add Function".to_string(), |v| v.to_string())
                                    dropdown_direction=DropdownDirection::Down
                                    dropdown_btn_type=DropdownBtnType::Select
                                    dropdown_options=function_names
                                    on_select=Box::new(handle_select_dropdown_option)
                                />
                            </div>
                        </div>
                    }
                }}

            </Suspense>

            <div class="form-control grid w-full justify-end">
                <Button
                    class="pl-[70px] pr-[70px]".to_string()
                    text="Submit".to_string()
                    on_click=on_submit
                />
            </div>

            {
                view! {
                    <div>
                        <p class="text-red-500">{move || error_message.get()}</p>
                    </div>
                }
            }

        </form>
    }
}
