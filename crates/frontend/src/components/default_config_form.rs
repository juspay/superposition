pub mod types;
pub mod utils;

use leptos::*;
use serde_json::{json, Map, Value};
use std::str::FromStr;
use web_sys::MouseEvent;

use crate::providers::editor_provider::EditorProvider;
use crate::{
    api::{fetch_functions, fetch_types},
    components::{
        button::Button,
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        input::{Input, InputType},
        input_components::{BooleanToggle, EnumDropdown},
    },
    schema::{JsonSchemaType, SchemaType},
    types::{FunctionsName, ListFilters, TypeTemplate},
    utils::get_key_type,
};

use self::{types::DefaultConfigCreateReq, utils::create_default_config};

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

    let functions_resource: Resource<String, Vec<crate::types::FunctionResponse>> =
        create_blocking_resource(
            move || tenant_rs.get(),
            |current_tenant| async move {
                match fetch_functions(
                    ListFilters {
                        page: None,
                        count: None,
                    },
                    current_tenant,
                )
                .await
                {
                    Ok(data) => data.data.into_iter().collect(),
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

    let (error_message, set_error_message) = create_signal("".to_string());

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
                    let config_type_schema = SchemaType::Single(JsonSchemaType::from(&config_schema_rs.get()));
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
                                on_change=Callback::new(move |new_config_schema| config_schema_ws.set(new_config_schema))
                                r#type=InputType::Monaco
                            />

                        </div>
                    }
                }}

            </Suspense>
            <div class="divider"></div>

            {move || {
                let schema: Map<String, Value> = serde_json::from_value(config_schema_rs.get())
                    .unwrap_or(Map::new());
                let key_type = get_key_type(&schema);
                let input_format = match key_type.as_str() {
                    "ENUM" => {
                        view! {
                            <EnumDropdown
                                schema
                                config_value=config_value_rs.get().as_str().map(String::from).unwrap_or_default()
                                handle_change=Callback::new(move |selected_enum: String| {
                                    config_value_ws.set(Value::String(selected_enum));
                                })

                                class=String::from("mt-2")
                            />
                        }
                            .into_view()
                    }
                    "BOOLEAN" => {
                        let value = config_value_rs.get();
                        if value.is_null() {
                            config_value_ws.set(Value::Bool(false));
                        }
                        view! {
                            <BooleanToggle
                                value=value.as_bool().unwrap_or(false)
                                on_change=Callback::new(move |flag: bool| config_value_ws.set(Value::Bool(flag)))
                            />
                        }
                            .into_view()
                    }
                    "INTEGER" => {
                        view! {
                            <input
                                type="number"
                                placeholder="Value"
                                class="input input-bordered w-full max-w-md"
                                value=config_value_rs.get().as_i64()
                                on:change=move |ev| {
                                    logging::log!("{:?}", event_target_value(&ev));
                                    let entered_integer = i64::from_str(&event_target_value(&ev)).unwrap_or(0);
                                    config_value_ws.set(json!(entered_integer));
                                }
                            />
                        }
                            .into_view()
                    }
                    "NUMBER" => {
                        view! {
                            <input
                                type="number"
                                placeholder="Value"
                                class="input input-bordered w-full max-w-md"
                                value=config_value_rs.get().as_f64()
                                on:change=move |ev| {
                                    logging::log!("{:?}", event_target_value(&ev));
                                    let entered_decimal = f64::from_str(&event_target_value(&ev)).unwrap_or_default();
                                    config_value_ws.set(json!(entered_decimal));
                                }
                            />
                        }
                            .into_view()
                    }
                    "OBJECT" => {
                        let config_type_schema = SchemaType::Single(JsonSchemaType::from(&config_schema_rs.get()));
                        view! {
                            <Input
                                id="default-config-value"
                                class="mt-5 rounded-md resize-y w-full max-w-md pt-3"
                                schema_type=config_type_schema
                                value=config_value_rs.get()
                                on_change=Callback::new(move |new_default_config: Value| {
                                    logging::log!("{:?}", new_default_config);
                                    config_value_ws.set(new_default_config);
                                })
                                r#type=InputType::Monaco
                            />

                        }.into_view()
                    },
                    _ => {
                        let config_value = match config_value_rs.get() {
                            Value::String(s) => s,
                            _ => String::new()
                        };
                        view! {
                            <textarea
                                type="text"
                                placeholder="Value"
                                class="input input-bordered w-full max-w-md pt-3"
                                on:change=move |ev| {
                                    logging::log!("{:?}", event_target_value(&ev));
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

            {
                view! {
                    <div>
                        <p class="text-red-500">{move || error_message.get()}</p>
                    </div>
                }
            }

        </form>
        </EditorProvider>
    }
}
