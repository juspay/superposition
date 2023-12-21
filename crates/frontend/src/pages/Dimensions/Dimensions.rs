use std::collections::HashMap;
use std::rc::Rc;

use crate::components::button::button::Button;
use crate::components::{
    stat::stat::Stat,
    table::{table::Table, types::Column},
};
use crate::utils::modal_action;
use leptos::*;
use reqwest::StatusCode;
use serde_json::{json, Map, Value};
use web_sys::MouseEvent;

use crate::pages::Dimensions::helper::fetch_dimensions;

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub dimension: String,
    pub priority: String,
    pub type_: String,
    pub pattern: String,
}

fn parse_string_to_json_value_vec(input: &str) -> Vec<Value> {
    // Parse the input string into a serde_json::Value
    let parsed = serde_json::from_str::<Value>(input);

    // Ensure the Value is an Array and convert it to Vec<Value>
    match parsed {
        Ok(Value::Array(arr)) => arr,
        _ => {
            logging::log!("Not a valid json in the input");
            vec![]
        }
    }
}

pub fn custom_formatter(_value: &str, row: &Map<String, Value>) -> View {
    let global_signal = use_context::<RwSignal<RowData>>().unwrap();
    let row_dimension = row["dimension"].clone().to_string().replace("\"", "");
    let row_priority = row["priority"].clone().to_string().replace("\"", "");

    let schema = row["schema"].clone().to_string();
    let schema_object = serde_json::from_str::<serde_json::Value>(&schema).unwrap();
    let edit_signal = use_context::<RwSignal<bool>>().unwrap();

    let row_type = schema_object.get("type").unwrap().to_string();
    let row_pattern = schema_object
        .get("pattern")
        .unwrap_or(&Value::String("".to_string()))
        .to_string();

    let edit_click_handler = move |_| {
        let row_data = RowData {
            dimension: row_dimension.clone(),
            priority: row_priority.clone(),
            type_: row_type.clone(),
            pattern: row_pattern.clone(),
        };
        global_signal.set(row_data);
        edit_signal.set(true);
        js_sys::eval("document.getElementById('my_modal_5').showModal();").unwrap();
    };

    let edit_icon: HtmlElement<html::I> = view! { <i class="ri-pencil-line ri-xl text-blue-500" on:click=edit_click_handler></i> };

    view! { <span class="cursor-pointer">{edit_icon}</span> }.into_view()
}

pub async fn create_dimension(
    tenant: String,
    key: String,
    priority: String,
    key_type: String,
    pattern: String,
) -> Result<String, String> {
    let priority: i64 = priority.parse().unwrap();
    let client = reqwest::Client::new();
    let host = "http://localhost:8080";
    let url = format!("{host}/dimension");

    let mut req_body: HashMap<&str, Value> = HashMap::new();
    let mut schema: Map<String, Value> = Map::new();

    match key_type.as_str() {
        "number" => {
            schema.insert("type".to_string(), Value::String(key_type.clone()));
        }
        "Enum" => {
            let array = parse_string_to_json_value_vec(pattern.clone().as_str());
            schema.insert("type".to_string(), Value::String("string".to_string()));
            schema.insert("enum".to_string(), Value::Array(array));
        }
        "Pattern" => {
            schema.insert("type".to_string(), Value::String("string".to_string()));
            schema.insert("pattern".to_string(), Value::String(pattern.clone()));
        }
        _ => {
            let json_pattern = serde_json::from_str::<Value>(&pattern.clone())
                .map_err(|e| e.to_string())?;
            req_body.insert("schema", json_pattern);
        }
    }

    req_body.insert("dimension", Value::String(key));
    req_body.insert("priority", Value::Number(priority.into()));
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
fn ModalComponent(
    handle_submit: Rc<dyn Fn()>,
    tenant: ReadSignal<String>,
) -> impl IntoView {
    view! {
        <div class="pt-4">
            <FormComponent handle_submit=handle_submit tenant=tenant/>
        </div>
    }
}

#[component]
fn FormComponent(
    handle_submit: Rc<dyn Fn()>,
    tenant: ReadSignal<String>,
) -> impl IntoView {
    use leptos::html::Input;
    use leptos::html::Textarea;
    let handle_submit = handle_submit.clone();
    let global_state = use_context::<RwSignal<RowData>>();
    let row_data = global_state.unwrap().get();

    let (dimension, set_dimension) = create_signal(row_data.dimension);
    let (priority, set_priority) = create_signal(row_data.priority);
    let (keytype, set_keytype) = create_signal(row_data.type_);
    let (pattern, set_pattern) = create_signal(row_data.pattern);
    let (show_labels, set_show_labels) = create_signal(false);
    let edit_signal = use_context::<RwSignal<bool>>();

    create_effect(move |_| {
        if let Some(row_data) = global_state {
            logging::log!("default config create effect");
            if edit_signal.unwrap().get() == true {
                set_dimension.set(row_data.get().dimension.clone().to_string());
                set_priority.set(row_data.get().priority.clone());
                set_keytype.set(row_data.get().type_.clone().to_string());
                set_pattern.set(row_data.get().pattern.clone());
            } else {
                set_dimension.set("".to_string());
                set_priority.set("".to_string());
                set_keytype.set("".to_string());
                set_pattern.set("".to_string());
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

            let value1 = input_element.get().expect("<input> to exist").value();
            let value2 = input_element_two.get().expect("<input> to exist").value();
            let value3 = input_element_three.get().expect("<input> to exist").value();

            set_dimension.set(value1.clone());
            set_priority.set(value2.clone());
            set_pattern.set(value3.clone());
            let handle_submit_clone = handle_submit.clone();

            spawn_local({
                let handle_submit = handle_submit_clone;
                async move {
                    let result = create_dimension(
                        tenant.get(),
                        dimension.get(),
                        priority.get(),
                        keytype.get(),
                        pattern.get(),
                    )
                    .await;

                    match result {
                        Ok(_) => {
                            handle_submit();
                            // modal_action("my_modal_5","close");
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
        <dialog id="my_modal_5" class="modal modal-bottom sm:modal-middle">
            <div class="modal-box relative bg-white">
                <form method="dialog" class="flex justify-end">
                    <button>
                        <i class="ri-close-fill" onclick="my_modal_5.close()"></i>
                    </button>
                </form>
                <form class="form-control w-full space-y-4 bg-white text-gray-700 font-mono">
                    <div class="form-control">
                        <label class="label font-mono">
                            <span class="label-text text-gray-700 font-mono">Dimension</span>
                        </label>
                        <input
                            disabled=move || { edit_signal.unwrap().get() }
                            type="text"
                            placeholder="Dimension"
                            class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                            value=dimension
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

                        <option
                            value="number"
                            selected=move || { keytype.get() == "number".to_string() }
                        >
                            "Number"
                        </option>
                        <option
                            value="Enum"
                            selected=move || { keytype.get() == "Enum".to_string() }
                        >
                            "String (Enum)"
                        </option>
                        <option
                            value="Pattern"
                            selected=move || { keytype.get() == "Pattern".to_string() }
                        >
                            "String (regex)"
                        </option>
                        <option
                            value="Other"
                            selected=move || { keytype.get() == "Other".to_string() }
                        >
                            "Other"
                        </option>
                    </select>

                    {move || {
                        view! {
                            <Show when=move || (keytype.get() == "number")>
                                <div class="form-control">
                                    <label class="label font-mono">
                                        <span class="label-text text-gray-700 font-mono">
                                            Priority
                                        </span>
                                    </label>
                                    <input
                                        type="Number"
                                        placeholder="Priority"
                                        class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                                        value=priority
                                        node_ref=input_element_two
                                    />
                                </div>
                            </Show>

                            <Show when=move || (show_labels.get() && (keytype.get() != "number"))>
                                <div class="form-control">
                                    <label class="label font-mono">
                                        <span class="label-text text-gray-700 font-mono">
                                            Priority
                                        </span>
                                    </label>
                                    <input
                                        type="Number"
                                        placeholder="Priority"
                                        class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                                        value=priority
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
            </div>
        </dialog>
    }
}

#[component]
pub fn Dimensions() -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let global_state = create_rw_signal(RowData::default());
    provide_context(global_state);

    let edit_signal = create_rw_signal(true);
    provide_context(edit_signal);
    let (open_form, set_open_form) = create_signal(false);

    let dimensions = create_blocking_resource(
        move || tenant_rs.get(),
        |current_tenant| async move {
            match fetch_dimensions(&current_tenant).await {
                Ok(data) => data,
                Err(_) => vec![],
            }
        },
    );

    let table_columns = create_memo(move |_| {
        vec![
            Column::default("dimension".to_string()),
            Column::default("priority".to_string()),
            Column::default("schema".to_string()),
            Column::default("created_by".to_string()),
            Column::default("created_at".to_string()),
            Column::new("EDIT".to_string(), None, Some(custom_formatter)),
        ]
    });

    view! {
        <div class="p-8">
            <Suspense fallback=move || view! { <p>"Loading (Suspense Fallback)..."</p> }>
                <div class="pb-4">

                    {move || {
                        let value = dimensions.get();
                        let total_items = match value {
                            Some(v) => v.len().to_string(),
                            _ => "0".to_string(),
                        };
                        view! {
                            <Stat heading="Dimensions" icon="ri-ruler-2-fill" number=total_items/>
                        }
                    }}
                    <Show when=move || { open_form.get() }>
                        <ModalComponent
                            handle_submit=Rc::new(move || {
                                set_open_form.set(false);
                                dimensions.refetch()
                            })

                            tenant=tenant_rs
                        />
                    </Show>
                </div>

                <div class="card rounded-xl w-full bg-base-100 shadow">
                    <div class="card-body">
                        <div class="flex justify-between mb-2">
                            <h2 class="card-title">Dimensions</h2>
                            <Button
                                text="Create Dimension".to_string()
                                on_click={
                                    let edit_clone = edit_signal.to_owned();
                                    move |_| {
                                        edit_clone.set(false);
                                        set_open_form.set(true);
                                        modal_action("my_modal_5", "open");
                                    }
                                }
                            />

                        </div>
                        <div>

                            {move || {
                                let value = dimensions.get();
                                match value {
                                    Some(v) => {
                                        let data = v
                                            .iter()
                                            .map(|ele| {
                                                let mut ele_map = json!(ele).as_object().unwrap().clone();
                                                ele_map
                                                    .insert(
                                                        "created_at".to_string(),
                                                        json!(ele.created_at.format("%v").to_string()),
                                                    );
                                                ele_map
                                            })
                                            .collect::<Vec<Map<String, Value>>>()
                                            .to_owned();
                                        view! {
                                            <Table
                                                table_style="abc".to_string()
                                                rows=data
                                                key_column="id".to_string()
                                                columns=table_columns.get()
                                            />
                                        }
                                    }
                                    None => view! { <div>Loading....</div> }.into_view(),
                                }
                            }}

                        </div>
                    </div>
                </div>
            </Suspense>
        </div>
    }
}
