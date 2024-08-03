pub mod types;
pub mod utils;

use leptos::*;
use serde_json::{json, Map, Value};
use web_sys::MouseEvent;

use crate::{
    api::{fetch_functions, fetch_types},
    components::{
        button::Button,
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        input_components::{BooleanToggle, EnumDropdown},
        monaco_editor::{MonacoEditor, TextContentType, generate_uri_name, METASCHEMA_JSON_SCHEMA_URI},
    },
    types::{FunctionsName, TypeTemplate},
    utils::{get_key_type, string_to_value_closure},
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
    #[prop(default = String::new())] json_config_value: String,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let (config_key, set_config_key) = create_signal(config_key);
    let (config_type_rs, config_type_ws) = create_signal(config_type);
    let (config_schema_rs, config_schema_ws) = create_signal(type_schema);
    let (config_value, set_config_value) = create_signal(config_value);
    let (json_config_value, _) = create_signal(json_config_value);
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
            set_function_name.update(|value| {
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
                    let config_t = if config_type_rs.get().is_empty() && edit {
                        "change current type template".into()
                    } else if config_type_rs.get().is_empty() && !edit {
                        "Choose a type template".into()
                    } else {
                        config_type_rs.get()
                    };
                    let schema = move || config_schema_rs.get();
                    let config_textarea = if schema().is_null() {
                        String::from("")
                    } else {
                        format!("{}", config_schema_rs.get())
                    };
                    let (config_textarea_rs, _) = create_signal(config_textarea);
                    let uri_name = generate_uri_name();
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
                            <MonacoEditor
                                node_id="json_schema_editor"
                                data_rs=config_textarea_rs
                                language=TextContentType::Json
                                uri_name=uri_name.clone()
                                schemas=json!(
                                    [{
                                        "uri": METASCHEMA_JSON_SCHEMA_URI,
                                        "fileMatch": [uri_name]
                                    }]
                                )

                                validation=true
                                classes=vec![
                                    "min-h-[400px]",
                                    "min-w-[300px]",
                                    "border-2",
                                    "border-purple-500",
                                    "rounded-lg",
                                    "mt-5",
                                    "w-full",
                                    "max-w-md",
                                    "pt-3",
                                    "pb-2",
                                ]
                                update_fn=move |event| {
                                    let new_data = event_target_value(&event);
                                    config_schema_ws
                                        .set_untracked(string_to_value_closure(new_data))
                                }
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
                let object_editor_uri_name = generate_uri_name();
                let input_format = match key_type.as_str() {
                    "ENUM" => {
                        view! {
                            <EnumDropdown
                                schema
                                config_value=config_value.get()
                                handle_change=Callback::new(move |selected_enum: String| {
                                    set_config_value.set(selected_enum);
                                })

                                class=String::from("mt-2")
                            />
                        }
                            .into_view()
                    }
                    "BOOLEAN" => {
                        let value = config_value.get();
                        if value.is_empty() {
                            set_config_value
                                .set(value.parse::<bool>().unwrap_or(false).to_string());
                        }
                        view! {
                            <BooleanToggle
                                config_value={value.parse::<bool>().unwrap_or(false)}
                                update_value=Callback::new(move |flag: bool| {
                                    set_config_value.set(flag.to_string());
                                })
                            />
                        }
                            .into_view()
                    }
                    "NUMBER" | "INTEGER" => {
                        view! {
                            <input
                                type="number"
                                placeholder="Value"
                                class="input input-bordered w-full max-w-md"
                                value=config_value.get()
                                on:change=move |ev| {
                                    logging::log!("{:?}", event_target_value(&ev));
                                    set_config_value.set(event_target_value(&ev));
                                }
                            />
                        }
                            .into_view()
                    }
                    "OBJECT" => {
                        view! {
                            <MonacoEditor
                                node_id="object_editor"
                                data_rs=json_config_value
                                language=TextContentType::Json
                                uri_name=object_editor_uri_name.clone()
                                schemas=json!(
                                    [{
                                        "uri": object_editor_uri_name,
                                        "fileMatch": [object_editor_uri_name],
                                        "schema": schema
                                    }]
                                )
                                validation=true
                                classes=vec![
                                    "min-h-[400px]",
                                    "min-w-[300px]",
                                    "border-2",
                                    "border-purple-500",
                                    "rounded-lg",
                                    "w-full",
                                    "max-w-md",
                                    "pt-3",
                                    "pb-2",
                                ]
                                update_fn=move |event| {
                                    let new_data = event_target_value(&event);
                                    set_config_value
                                        .set_untracked(new_data);
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
                                class="input input-bordered w-full max-w-md pt-3"
                                on:change=move |ev| {
                                    logging::log!("{:?}", event_target_value(& ev));
                                    set_config_value.set(event_target_value(&ev));
                                }
                            />
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
                                    on_select=handle_select_dropdown_option
                                />
                            </div>
                        </div>
                    }
                }}

            </Suspense>

            <div class="form-control grid w-full justify-start">
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
