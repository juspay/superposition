use std::collections::HashMap;
use std::rc::Rc;

use crate::components::table::{
    table::Table,
    types::{Column, TableSettings},
};
use crate::components::Button::EditButton::EditButton;
use crate::pages::DefaultConfig::types::Config;
use js_sys;
use leptos::ev::SubmitEvent;
use leptos::spawn_local;
use leptos::*;
use leptos_router::use_query_map;
use serde_json::{Map, Value};

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub key: String,
    pub value: String,
}

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
    schema.insert("type".to_string(), Value::String(key_type));
    schema.insert("pattern".to_string(), Value::String(pattern));
    req_body.insert("value", Value::String(value));
    req_body.insert("schema", Value::Object(schema));
    let response = client
        .put(url)
        .header("x-tenant", tenant)
        .header("Authorization", "Bearer 12345678")
        .json(&req_body)
        .send()
        .await
        .map_err(|e| e.to_string())?;
    response.text().await.map_err(|e| e.to_string())
}

#[component]
fn ModalComponent(handle_submit: Rc<dyn Fn()>) -> impl IntoView {
    view! {
        <div class="p-6 bg-white text-gray-600">
           <EditButton text="Create DefaultConfig".to_string() modal= "my_modal_5".to_string() modalAction = "showModal()".to_string() />
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
        </div>
    }
}

#[component]
fn FormComponent(handle_submit: Rc<dyn Fn()>) -> impl IntoView {
    use leptos::html::Input;
    let handle_submit = handle_submit.clone();
    let global_state = use_context::<RwSignal<RowData>>();
    let row_data = global_state.unwrap().get();

    let (key, set_key) = create_signal(row_data.key);
    let (value, set_value) = create_signal(row_data.value);
    let (keytype, set_keytype) = create_signal("string".to_string());
    let (pattern, set_pattern) = create_signal(".*".to_string());

    create_effect(move |_| {
        if let Some(row_data) = global_state {
            set_key.set(row_data.get().key.clone().to_string());
            set_value.set(row_data.get().value.clone().to_string());
        }
    });

    let input_element: NodeRef<Input> = create_node_ref();
    let input_element_two: NodeRef<Input> = create_node_ref();
    let input_element_three: NodeRef<Input> = create_node_ref();
    let input_element_four: NodeRef<Input> = create_node_ref();

    let on_submit = {
        let handle_submit = handle_submit.clone();
        move |ev: SubmitEvent| {
            ev.prevent_default();

            let value1 = input_element.get().expect("<input> to exist").value();
            let value2 = input_element_two.get().expect("<input> to exist").value();
            let value3 = input_element_three.get().expect("<input> to exist").value();
            let value4 = input_element_four.get().expect("<input> to exist").value();

            set_key.set(value1.clone());
            set_value.set(value2.clone());
            set_keytype.set(value3.clone());
            set_pattern.set(value4.clone());
            let handle_submit_clone = handle_submit.clone();

            spawn_local({
                let handle_submit = handle_submit_clone;
                async move {
                    let result = create_default_config(
                        "mjos".to_string(),
                        key.get(),
                        value.get(),
                        keytype.get(),
                        pattern.get(),
                    )
                    .await;

                    match result {
                        Ok(_) => {
                            handle_submit();
                        }
                        Err(_) => {
                            // Handle error
                            // Consider logging or displaying the error
                        }
                    }
                }
            });
        }
    };

    view! {
        <form
            class="form-control w-full space-y-4 bg-white text-gray-700 font-mono"
            on:submit=on_submit
        >
            <div class="form-control">
                <label class="label font-mono">
                    <span class="label-text text-gray-700 font-mono">Key</span>
                </label>
                <input
                    type="text"
                    placeholder="Key"
                    class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                    value=key
                    node_ref=input_element
                />
            </div>
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
                    <span class="label-text text-gray-700 font-mono">Type</span>
                </label>
                <input
                    type="text"
                    placeholder="Type"
                    class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                    value=keytype
                    node_ref=input_element_three
                />
            </div>
            <div class="form-control">
                <label class="label font-mono">
                    <span class="label-text text-gray-700 font-mono">Pattern (regex)</span>
                </label>
                <input
                    type="text"
                    placeholder="Pattern"
                    class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                    value=pattern
                    node_ref=input_element_four
                />
            </div>
            <div class="form-control mt-6">
                <button
                    type="submit"
                    class="btn btn-primary shadow-md font-mono"
                    onclick="my_modal_5.close()"
                >
                    Submit
                </button>
            </div>
        </form>
    }
}

fn custom_formatter(value: &str, row: &Map<String, Value>) -> View {
    let intermediate_signal = use_context::<RwSignal<Option<RowData>>>().unwrap();
    let row_key = row["KEY"].clone().to_string().replace("\"", "");
    let row_value = row["VALUE"].clone().to_string().replace("\"", "");

    let edit_click_handler = move |_| {
        let row_data = RowData {
            key: row_key.clone(),
            value: row_value.clone(),
        };
        intermediate_signal.set(Some(row_data));
        js_sys::eval("document.getElementById('my_modal_5').showModal();").unwrap();
    };

    let edit_icon: HtmlElement<html::I> = view! { <i class="ri-pencil-line ri-xl text-blue-500" on:click=edit_click_handler></i> };

    view! { <span>{edit_icon}</span> }.into_view()
}

#[component]
pub fn DefaultConfig() -> impl IntoView {
    // let (edit_row_data, set_edit_row_data) = create_signal(None);
    let global_state = create_rw_signal(RowData::default());
    provide_context(global_state);

    let intermediate_signal = create_rw_signal(None::<RowData>);

    // Listener for intermediate signal
    create_effect(move |_| {
        if let Some(row_data) = intermediate_signal.get() {
            global_state.set(row_data.clone());
        }
    });

    provide_context(intermediate_signal.clone());

    let query = use_query_map();

    let tenant = query.with(|params_map| {
        params_map
            .get("tenant")
            .cloned()
            .unwrap_or_else(|| "mjos".to_string())
    });

    let config_data =
        create_blocking_resource(|| {}, move |_| fetch_config(tenant.clone()));

    let table_columns = create_memo(move |_| {
        vec![
            Column::default("KEY".to_string()),
            Column::default("VALUE".to_string()),
            Column::new("EDIT".to_string(), None, Some(custom_formatter)),
        ]
    });

    view! {
        <div class="p-8">
            <ModalComponent handle_submit=Rc::new(move || config_data.refetch())/>
            <Suspense fallback=move || {
                view! { <p>"Loading (Suspense Fallback)..."</p> }
            }>

                {move || {
                    config_data
                        .with(move |result| {
                            match result {
                                Some(Ok(config)) => {
                                    let mut default_config: Vec<Map<String, Value>> = Vec::new();
                                    let settings = TableSettings {
                                        redirect_prefix: None,
                                    };
                                    for (key, value) in config.default_configs.iter() {
                                        let mut map = Map::new();
                                        let trimmed_key = Value::String(
                                            key.trim_matches('"').to_string(),
                                        );
                                        let formatted_value = Value::String(
                                            format!("{}", value).trim_matches('"').to_string(),
                                        );
                                        map.insert("KEY".to_string(), trimmed_key);
                                        map.insert("VALUE".to_string(), formatted_value);
                                        default_config.push(map);
                                    }
                                    vec![
                                        view! {
                                            <div class="card rounded-lg w-full bg-base-100 shadow">
                                                <div class="card-body">
                                                    <h2 class="card-title chat-bubble text-gray-800 dark:text-white bg-white font-mono">
                                                        "Default Config"
                                                    </h2>
                                                    <Table
                                                        table_style="font-mono".to_string()
                                                        rows=default_config
                                                        key_column="id".to_string()
                                                        columns=table_columns.get()
                                                        settings=settings
                                                    />
                                                </div>

                                            </div>
                                        },
                                    ]
                                }
                                Some(Err(error)) => {
                                    vec![
                                        view! {
                                            <div class="text-red-500">
                                                {"Failed to fetch config data: "} {error}
                                            </div>
                                        },
                                    ]
                                }
                                None => vec![view! { <div>Loading....</div> }],
                            }
                        })
                }}

            </Suspense>
        </div>
    }
}
