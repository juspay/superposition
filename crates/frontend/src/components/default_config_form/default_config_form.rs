use leptos::*;
use serde_json::{json, Number, Value};
use std::str::FromStr;
use web_sys::MouseEvent;

use crate::{
    api::fetch_functions,
    components::{
        button::button::Button,
        dropdown::dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
    },
    types::FunctionsName,
    utils::parse_string_to_json_value_vec,
};

use super::{types::DefaultConfigCreateReq, utils::create_default_config};

#[component]
pub fn default_config_form<NF>(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] config_key: String,
    #[prop(default = String::new())] config_type: String,
    #[prop(default = String::new())] config_pattern: String,
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
    let (config_type, set_config_type) = create_signal(config_type);
    let (config_pattern, set_config_pattern) = create_signal(config_pattern);
    let (config_value, set_config_value) = create_signal(config_value);
    let (function_name, set_function_name) = create_signal(function_name);

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

    let (show_labels, set_show_labels) = create_signal(edit);

    let (error_message, set_error_message) = create_signal("".to_string());

    let on_submit = move |ev: MouseEvent| {
        ev.prevent_default();
        let f_name = prefix
            .clone()
            .map_or_else(|| config_key.get(), |prefix| prefix + &config_key.get());
        let f_type = config_type.get();
        let f_pattern = config_pattern.get();
        let f_value = config_value.get();
        let fun_name = function_name.get();

        let f_value = match f_type.as_str() {
            "number" => Value::Number(f_value.parse::<i64>().unwrap().into()),
            "decimal" => match f64::from_str(&f_value) {
                Ok(num) => match Number::from_f64(num) {
                    Some(number) => Value::Number(number),
                    None => Value::String(
                        "Invalid decimal format or precision issue".to_string(),
                    ),
                },
                Err(_) => Value::String("Invalid decimal format".to_string()),
            },
            "boolean" => match bool::from_str(&f_value) {
                Ok(boolean) => Value::Bool(boolean),
                _ => Value::String("Invalid Boolean".to_string()),
            },
            "pattern" | "enum" => Value::String(f_value),
            _ => Value::from_str(&f_value).expect("Error parsing JSON"),
        };

        let f_schema = match f_type.as_str() {
            "number" => {
                json!({
                    "type": f_type.to_string(),
                })
            }
            "decimal" => {
                json!({
                    "type": "number".to_string(),
                })
            }
            "boolean" => {
                json!({
                    "type": "boolean".to_string(),
                }
                )
            }
            "enum" => {
                json!({
                    "type": "string",
                    "enum": parse_string_to_json_value_vec(f_pattern.as_str())
                })
            }
            "pattern" => {
                json!({
                    "type": "string",
                    "pattern": f_pattern.to_string()
                })
            }
            _ => Value::from_str(&f_pattern).expect("Error parsing JSON"),
        };

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

            <div class="form-control">
                <label class="label">
                    <span class="label-text">Set Schema</span>
                </label>
                <select
                    name="schemaType[]"
                    on:change=move |ev| {
                        set_show_labels.set(true);
                        match event_target_value(&ev).as_str() {
                            "number" => {
                                set_config_type.set("number".to_string());
                            }
                            "decimal" => {
                                set_config_type.set("decimal".to_string());
                            }
                            "boolean" => {
                                set_config_type.set("boolean".to_string());
                            }
                            "enum" => {
                                set_config_type.set("enum".to_string());
                                set_config_pattern
                                    .set(format!("{:?}", vec!["android", "web", "ios"]));
                            }
                            "pattern" => {
                                set_config_type.set("pattern".to_string());
                                set_config_pattern.set(".*".to_string());
                            }
                            _ => {
                                set_config_type.set("other".to_string());
                                set_config_pattern.set("".to_string());
                            }
                        };
                    }

                    class="select select-bordered w-full max-w-md"
                >
                    <option disabled selected>
                        Choose Schema Type
                    </option>

                    <option
                        value="number"
                        selected=move || { config_type.get() == "number".to_string() }
                    >
                        "Number"
                    </option>
                    <option
                        value="decimal"
                        selected=move || { config_type.get() == "decimal".to_string() }
                    >
                        "Decimal (16 digits)"
                    </option>
                    <option
                        value="boolean"
                        selected=move || { config_type.get() == "boolean".to_string() }
                    >
                        "Boolean"
                    </option>
                    <option
                        value="enum"
                        selected=move || { config_type.get() == "enum".to_string() }
                    >
                        "String (Enum)"
                    </option>
                    <option
                        value="pattern"
                        selected=move || { config_type.get() == "pattern".to_string() }
                    >
                        "String (regex)"
                    </option>
                    <option
                        value="other"
                        selected=move || { config_type.get() == "other".to_string() }
                    >
                        "Other"
                    </option>
                </select>
            </div>

            <div class="divider"></div>

            {move || {
                view! {
                    <Show when=move || {
                        (config_type.get() == "number") || (config_type.get() == "decimal")
                    }>
                        <div class="form-control">
                            <label class="label">
                                <span class="label-text">Value</span>
                            </label>
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

                        </div>
                        <div class="divider"></div>
                    </Show>

                    <Show when=move || {
                        show_labels.get() && (config_type.get() != "number")
                            && (config_type.get() != "decimal")
                    }>
                        <div class="form-control">
                            <label class="label">
                                <span class="label-text ">Value</span>
                            </label>
                            <input
                                type="text"
                                placeholder="Value"
                                class="input input-bordered w-full max-w-md"
                                value=config_value.get()
                                on:change=move |ev| {
                                    logging::log!("{:?}", event_target_value(& ev));
                                    set_config_value.set(event_target_value(&ev));
                                }
                            />

                        </div>

                        <div class="divider"></div>

                        <Show when=move || (config_type.get() != "boolean")>
                            <div class="form-control">
                                <label class="label">
                                    <span class="label-text">{config_type.get()}</span>
                                </label>
                                <textarea
                                    type="text"
                                    class="input input-bordered w-full max-w-md pt-[10px]"
                                    on:change=move |ev| {
                                        let value = event_target_value(&ev);
                                        logging::log!("{:?}", value);
                                        set_config_pattern.set(value);
                                    }
                                >

                                    {config_pattern.get()}
                                </textarea>
                            </div>
                            <div class="divider"></div>
                        </Show>

                    </Show>
                }
            }}

            <Suspense>
                {move || {
                    let functions = functions_resource.get().unwrap_or(vec![]);
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
