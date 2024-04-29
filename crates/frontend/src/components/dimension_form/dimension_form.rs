use super::types::DimensionCreateReq;
use super::utils::create_dimension;
use crate::components::dropdown::dropdown::{
    Dropdown, DropdownBtnType, DropdownDirection,
};
use crate::types::FunctionsName;
use crate::utils::parse_string_to_json_value_vec;
use crate::{api::fetch_functions, components::button::button::Button};
use leptos::*;
use serde_json::{json, Value};
use std::str::FromStr;
use web_sys::MouseEvent;

#[component]
pub fn dimension_form<NF>(
    #[prop(default = false)] edit: bool,
    #[prop(default = 0)] priority: u16,
    #[prop(default = String::new())] dimension_name: String,
    #[prop(default = String::new())] dimension_type: String,
    #[prop(default = String::new())] dimension_pattern: String,
    #[prop(default = None)] function_name: Option<Value>,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let (priority, set_priority) = create_signal(priority);
    let (dimension_name, set_dimension_name) = create_signal(dimension_name);
    let (dimension_type, set_dimension_type) = create_signal(dimension_type);
    let (dimension_pattern, set_dimension_pattern) = create_signal(dimension_pattern);

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
        let f_priority = priority.get();
        let f_name = dimension_name.get();
        let f_type = dimension_type.get();
        let f_pattern = dimension_pattern.get();
        let fun_name = function_name.get();

        let f_schema = match f_type.as_str() {
            "number" => {
                json!({
                    "type": f_type.to_string()
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

        let payload = DimensionCreateReq {
            dimension: f_name,
            priority: f_priority,
            schema: f_schema,
            function_name: fun_name,
        };

        let handle_submit_clone = handle_submit.clone();
        spawn_local({
            let handle_submit = handle_submit_clone;
            async move {
                let result = create_dimension(tenant_rs.get(), payload.clone()).await;

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
                    <span class="label-text">Dimension</span>
                </label>
                <input
                    disabled=edit
                    type="text"
                    placeholder="Dimension"
                    class="input input-bordered w-full max-w-md"
                    value=dimension_name.get()
                    on:change=move |ev| {
                        let value = event_target_value(&ev);
                        set_dimension_name.set(value);
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
                            set_dimension_type.set("number".to_string());
                        }
                        "decimal" => {
                            set_dimension_type.set("decimal".to_string());
                        }
                        "boolean" => {
                            set_dimension_type.set("boolean".to_string());
                        }
                        "enum" => {
                            set_dimension_type.set("enum".to_string());
                            set_dimension_pattern
                                .set(format!("{:?}", vec!["android", "web", "ios"]));
                        }
                        "pattern" => {
                            set_dimension_type.set("pattern".to_string());
                            set_dimension_pattern.set(".*".to_string());
                        }
                        _ => {
                            set_dimension_type.set("other".to_string());
                            set_dimension_pattern.set("".to_string());
                        }
                    };
                }

                class="select select-bordered w-full max-w-md"
            >
                <option disabled selected>
                    Set Schema
                </option>

                <option
                    value="number"
                    selected=move || { dimension_type.get() == "number".to_string() }
                >

                    "Number"
                </option>
                <option
                  value="decimal"
                  selected= move || {dimension_type.get() == "decimal".to_string()}
                >
                  "Decimal (Max Value : 1.7976931348623157e+308)"
                  </option>
                <option
                  value= "boolean"
                  selected= move || {dimension_type.get() == "boolean".to_string()}
                >
                    "Boolean"
                </option>
                <option
                    value="enum"
                    selected=move || { dimension_type.get() == "enum".to_string() }
                >
                    "String (Enum)"
                </option>
                <option
                    value="pattern"
                    selected=move || { dimension_type.get() == "pattern".to_string() }
                >
                    "String (regex)"
                </option>
                <option
                    value="other"
                    selected=move || { dimension_type.get() == "other".to_string() }
                >
                    "Other"
                </option>
            </select>
            </div>

            <div class="divider"></div>

            {move || {
                view! {
                        <div class="form-control">
                            <label class="label">
                                <span class="label-text">Priority</span>
                            </label>
                            <input
                                type="Number"
                                placeholder="Priority"
                                class="input input-bordered w-full max-w-md"
                                value=priority.get()
                                on:change=move |ev| {
                                    logging::log!(
                                        "{:?}", event_target_value(& ev).parse::< u16 > ()
                                    );
                                    match event_target_value(&ev).parse::<u16>() {
                                        Ok(i_prio) => set_priority.set(i_prio),
                                        Err(e) => logging::log!("{e}"),
                                    };
                                }
                            />

                        </div>

                    <Show when=move || (show_labels.get() && ((dimension_type.get() == "enum") || (dimension_type.get() == "pattern") || (dimension_type.get() == "other")))>
                        <div class="form-control">
                            <label class="label font-mono">
                                <span class="label-text text-gray-700 font-mono">
                                    {dimension_type.get()}
                                </span>
                            </label>
                            <textarea
                                type="text"
                                class="input input-bordered w-full max-w-md pt-[10px]"
                                on:change=move |ev| {
                                    let value = event_target_value(&ev);
                                    logging::log!("{:?}", value);
                                    set_dimension_pattern.set(value);
                                }
                            >

                                {dimension_pattern.get()}
                            </textarea>

                        </div>
                        <div class="divider"></div>
                    </Show>
                }
            }}

            <Suspense>
            {move || {
                let mut functions = functions_resource.get().unwrap_or(vec![]);
                let mut function_names: Vec<FunctionsName> = vec!["None".to_string()];
                functions.sort_by(|a, b| a.function_name.cmp(&b.function_name));
                functions.into_iter().for_each(|ele| {
                    function_names.push(ele.function_name);
                });
                view! {
                    <div class="form-control">
                        <div class="gap-1">
                            <label class="label flex-col justify-center items-start">
                                <span class="label-text">Function Name</span>
                                <span class="label-text text-slate-400">Assign Function validation to your key</span>
                            </label>
                        </div>

                        <div class="mt-2">
                            <Dropdown
                                dropdown_width="w-100"
                                dropdown_icon="".to_string()
                                dropdown_text={function_name.get().and_then(|v|  match v {
                                    Value::String(s) => Some(s),
                                    _ => None,
                                }).map_or("Add Function".to_string(), |v| v.to_string())}
                                dropdown_direction=DropdownDirection::Down
                                dropdown_btn_type=DropdownBtnType::Select
                                dropdown_options=function_names
                                on_select=Box::new(handle_select_dropdown_option)
                            />
                        </ div>
                    </ div>
                }
            }}
            </ Suspense>

            <div class="form-control grid w-full justify-end">
                <Button class="pl-[70px] pr-[70px]".to_string() text="Submit".to_string() on_click=on_submit/>
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