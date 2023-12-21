use std::collections::HashMap;
use std::rc::Rc;

use crate::components::table::{table::Table, types::Column};

use crate::components::button::button::Button;
use crate::components::stat::stat::Stat;
use crate::pages::DefaultConfig::types::Config;
use crate::pages::ExperimentList::utils::fetch_default_config;
use crate::utils::modal_action;
use js_sys;
use leptos::spawn_local;
use leptos::*;
use reqwest::StatusCode;
use serde_json::{json, Map, Value};
use web_sys::MouseEvent;

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub key: String,
    pub value: String,
}

// fn parse_string_to_json_value_vec(input: &str) -> Vec<Value> {
//     // Parse the input string into a serde_json::Value
//     let parsed = serde_json::from_str::<Value>(input)
//         .expect("Failed to parse JSON");

//     // Ensure the Value is an Array and convert it to Vec<Value>
//     match parsed {
//         Value::Array(arr) => arr,
//         _ => panic!("Input is not a JSON array"),
//     }
// }

pub async fn fetch_config(tenant: String) -> Result<Config, String> {
    let client = reqwest::Client::new();
    let host = "http://localhost:8080";
    let url = format!("{host}/config");
    match client.get(url).header("x-tenant", tenant).send().await {
        Ok(response) => {
            let config: Config = response.json().await.map_err(|e| e.to_string())?;
            Ok(config)
        }
        Err(e) => Err(e.to_string()),
    }
}

pub async fn create_default_config(
    tenant: String,
    key: String,
    value: String,
    key_type: String,
    pattern: String,
) -> Result<String, String> {
    let client = reqwest::Client::new();
    let host = "http://localhost:8080";
    let url = format!("{host}/default-config/{key}");
    let mut req_body: HashMap<&str, Value> = HashMap::new();
    let mut schema: Map<String, Value> = Map::new();
    match key_type.as_str() {
        "number" => {
            schema.insert("type".to_string(), Value::String(key_type.clone()));
            req_body.insert(
                "value",
                Value::Number(value.clone().parse::<i32>().unwrap().into()),
            );
        }
        // "Enum" => {
        //     let array = parse_string_to_json_value_vec(pattern.clone().as_str());
        //     schema.insert("type".to_string(),Value::String("string".to_string()));
        //     schema.insert("enum".to_string(), Value::Array(array));
        //     req_body.insert("value", Value::Array(value2));
        // },
        "Pattern" | "Enum" => {
            schema.insert("type".to_string(), Value::String("string".to_string()));
            schema.insert("pattern".to_string(), Value::String(pattern.clone()));
        }
        _ => {
            let json_pattern = serde_json::from_str::<Value>(&pattern.clone())
                .map_err(|e| e.to_string())?;
            req_body.insert("schema", json_pattern);
        }
    }

    if key_type != "number".to_string() {
        req_body.insert("value", Value::String(value));
    }

    req_body.insert("schema", Value::Object(schema));

    let response = client
        .put(url)
        .header("x-tenant", tenant)
        .header("Authorization", "Bearer 12345678")
        .json(&req_body)
        .send()
        .await
        .map_err(|e| e.to_string())?;
    match response.status() {
        StatusCode::OK => response.text().await.map_err(|e| e.to_string()),
        StatusCode::CREATED => response.text().await.map_err(|e| e.to_string()),
        StatusCode::BAD_REQUEST => Err("Schema Validation Failed".to_string()),
        _ => Err("Internal Server Error".to_string()),
    }
}

#[component]
fn ModalComponent(handle_submit: Rc<dyn Fn()>) -> impl IntoView {
    view! {
        <dialog id="my_modal_5" class="modal modal-bottom sm:modal-middle">
            <div class="modal-box relative bg-white">
                <form method="dialog" class="flex justify-end">
                    <button>
                        <i class="ri-close-fill"></i>
                    </button>
                </form>
                <FormComponent handle_submit=handle_submit/>
            </div>
        </dialog>
    }
}

#[component]
fn FormComponent(handle_submit: Rc<dyn Fn()>) -> impl IntoView {
    use leptos::html::Input;
    use leptos::html::Textarea;
    let handle_submit = handle_submit.clone();
    let global_state = use_context::<RwSignal<RowData>>();
    let _row_data = global_state.unwrap().get();

    let (key, set_key) = create_signal("".to_string());
    let (value, set_value) = create_signal("".to_string());
    let (keytype, set_keytype) = create_signal("".to_string());
    let (pattern, set_pattern) = create_signal("".to_string());

    let edit_signal = use_context::<RwSignal<bool>>();
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let (show_labels, set_show_labels) = create_signal(false);

    create_effect(move |_| {
        if let Some(row_data) = global_state {
            logging::log!("default config create effect");
            if edit_signal.unwrap().get() == true {
                set_key.set(row_data.get().key.clone().to_string());
                set_value.set(row_data.get().value.clone().to_string());
            } else {
                set_key.set("".to_string());
                set_value.set("".to_string());
            }
        }
    });

    let input_element: NodeRef<Input> = create_node_ref();
    let input_element_two: NodeRef<Input> = create_node_ref();
    let input_element_three: NodeRef<Textarea> = create_node_ref();

    let (error_message, set_error_message) = create_signal("".to_string());

    let on_submit = {
        let handle_submit = handle_submit.clone();
        move |ev: MouseEvent| {
            ev.prevent_default();

            let current_tenant = tenant_rs.get();
            let value1 = input_element.get().expect("<input> to exist").value();
            let value2 = input_element_two.get().expect("<input> to exist").value();
            let value3 = input_element_three.get().expect("<input> to exist").value();

            set_key.set(value1.clone());
            set_value.set(value2.clone());
            set_pattern.set(value3.clone());

            let handle_submit_clone = handle_submit.clone();

            spawn_local({
                let handle_submit = handle_submit_clone;
                async move {
                    let result = create_default_config(
                        current_tenant,
                        key.get(),
                        value.get(),
                        keytype.get(),
                        pattern.get(),
                    )
                    .await;

                    match result {
                        Ok(_) => {
                            handle_submit();
                            modal_action("my_modal_5", "close");
                        }
                        Err(e) => {
                            set_error_message.set(e);
                            // Handle error
                            // Consider logging or displaying the error
                        }
                    }
                }
            });
        }
    };

    view! {
        <form class="form-control w-full space-y-4 bg-white text-gray-700 font-mono">
            <div class="form-control">
                <label class="label font-mono">
                    <span class="label-text text-gray-700 font-mono">Key</span>
                </label>
                <input
                    disabled=move || { edit_signal.unwrap().get() }
                    type="text"
                    placeholder="Key"
                    class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                    value=key
                    node_ref=input_element
                />
            </div>

            <select
                name="schemaType[]"
                on:change=move |ev| {
                    set_show_labels.set(true);
                    match event_target_value(&ev).as_str() {
                        "number" => {
                            set_keytype.set("number".to_string());
                        }
                        "Enum" => {
                            set_keytype.set("Enum".to_string());
                            set_pattern.set(format!("{:?}", vec!["android", "web", "ios"]));
                        }
                        "Pattern" => {
                            set_keytype.set("Pattern".to_string());
                            set_pattern.set(".*".to_string());
                        }
                        _ => {
                            set_keytype.set("Other".to_string());
                            set_pattern.set("".to_string());
                        }
                    };
                }

                class="select select-bordered"
            >
                <option disabled selected>
                    Set Schema
                </option>

                <option value="number" selected=move || { keytype.get() == "number".to_string() }>
                    "Number"
                </option>
                <option value="Enum" selected=move || { keytype.get() == "Enum".to_string() }>
                    "String (Enum)"
                </option>
                <option value="Pattern" selected=move || { keytype.get() == "Pattern".to_string() }>
                    "String (regex)"
                </option>
                <option value="Other" selected=move || { keytype.get() == "Other".to_string() }>
                    "Other"
                </option>
            </select>

            {move || {
                view! {
                    <Show when=move || (keytype.get() == "number")>
                        <div class="form-control">
                            <label class="label font-mono">
                                <span class="label-text text-gray-700 font-mono">Value</span>
                            </label>
                            <input
                                type="number"
                                placeholder="Value"
                                class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                                value=value
                                node_ref=input_element_two
                            />
                        </div>
                    </Show>

                    <Show when=move || (show_labels.get() && (keytype.get() != "number"))>
                        <div class="form-control">
                            <label class="label font-mono">
                                <span class="label-text text-gray-700 font-mono">Value</span>
                            </label>
                            <input
                                type="text"
                                placeholder="Value"
                                class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                                value=value
                                node_ref=input_element_two
                            />
                        </div>
                        <div class="form-control">
                            <label class="label font-mono">
                                <span class="label-text text-gray-700 font-mono">
                                    {keytype.get()}
                                </span>
                            </label>
                            <textarea
                                type="text"
                                class="input input-bordered w-full bg-white text-gray-700 shadow-md"

                                node_ref=input_element_three
                            >
                                {pattern.get()}
                            </textarea>

                        </div>
                    </Show>
                }
            }}

            <div class="form-control mt-6">
                <Button text="Submit".to_string() on_click=on_submit/>
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

fn custom_formatter(_value: &str, row: &Map<String, Value>) -> View {
    let global_signal = use_context::<RwSignal<RowData>>().unwrap();
    let row_key = row["key"].clone().to_string().replace("\"", "");
    let row_value = row["value"].clone().to_string().replace("\"", "");

    let edit_signal = use_context::<RwSignal<bool>>().unwrap();
    let edit_click_handler = move |_| {
        let row_data = RowData {
            key: row_key.clone(),
            value: row_value.clone(),
        };
        edit_signal.set(true);
        global_signal.set(row_data);
        js_sys::eval("document.getElementById('my_modal_5').showModal();").unwrap();
    };

    let edit_icon: HtmlElement<html::I> =
        view! { <i class="ri-pencil-line ri-xl text-blue-500"></i> };

    view! { <span on:click=edit_click_handler>{edit_icon}</span> }.into_view()
}

#[component]
pub fn DefaultConfig() -> impl IntoView {
    // let (edit_row_data, set_edit_row_data) = create_signal(None);
    let global_state = create_rw_signal(RowData::default());
    provide_context(global_state);

    let edit_signal = create_rw_signal(true);
    provide_context(edit_signal);

    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let default_config_resource = create_blocking_resource(
        move || tenant_rs.get(),
        |current_tenant| async move {
            match fetch_default_config(&current_tenant).await {
                Ok(data) => data,
                Err(_) => vec![],
            }
        },
    );

    let table_columns = create_memo(move |_| {
        vec![
            Column::default("key".to_string()),
            Column::default("schema".to_string()),
            Column::default("value".to_string()),
            Column::default("created_at".to_string()),
            Column::default("created_by".to_string()),
            Column::new("EDIT".to_string(), None, Some(custom_formatter)),
        ]
    });

    view! {
        <div class="p-8">
            <ModalComponent handle_submit=Rc::new(move || default_config_resource.refetch())/>
            <Suspense fallback=move || {
                view! { <p>"Loading (Suspense Fallback)..."</p> }
            }>

                {
                    let edit_signal = edit_signal.clone();
                    move || {
                        let default_config = default_config_resource.get().unwrap_or(vec![]);
                        let total_default_config_keys = default_config.len().to_string();
                        let edit_signal = edit_signal.clone();
                        let table_rows = default_config
                            .into_iter()
                            .map(|config| {
                                let mut ele_map = json!(config).as_object().unwrap().to_owned();
                                ele_map
                                    .insert(
                                        "created_at".to_string(),
                                        json!(config.created_at.format("%v").to_string()),
                                    );
                                ele_map
                            })
                            .collect::<Vec<Map<String, Value>>>();
                        view! {
                            <div class="pb-4">
                                <Stat
                                    heading="Config Keys"
                                    icon="ri-tools-line"
                                    number=total_default_config_keys
                                />
                            </div>
                            <div class="card rounded-lg w-full bg-base-100 shadow">
                                <div class="card-body">
                                    <div class="flex justify-between">
                                        <h2 class="card-title chat-bubble text-gray-800 dark:text-white bg-white font-mono">
                                            "Default Config"
                                        </h2>
                                        <Button
                                            text="Create Key".to_string()
                                            on_click={
                                                let edit_signal_clone = edit_signal.to_owned();
                                                move |_| {
                                                    edit_signal_clone.set(false);
                                                    modal_action("my_modal_5", "open");
                                                }
                                            }
                                        />

                                    </div>
                                    <Table
                                        table_style="font-mono".to_string()
                                        rows=table_rows
                                        key_column="id".to_string()
                                        columns=table_columns.get()
                                    />
                                </div>
                            </div>
                        }
                    }
                }

            </Suspense>
        </div>
    }
}
