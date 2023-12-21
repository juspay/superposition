use std::rc::Rc;

use crate::api::{fetch_default_config, fetch_dimensions};
use crate::components::button::button::Button;
use crate::components::condition_pills::condition_pills::ConditionPills;
use crate::components::context_form::context_form::ContextForm;
use crate::components::override_form::override_form::OverrideForm;
use crate::components::table::{table::Table, types::Column};
use crate::pages::DefaultConfig::types::Config;
use crate::utils::modal_action;
use leptos::*;
use reqwest::StatusCode;
use serde_json::{json, Map, Value};
use web_sys::MouseEvent;

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

#[component]
fn ContextModalForm<NF>(handle_change: NF) -> impl IntoView
where
    NF: Fn(Vec<(String, String, String)>) + 'static + Clone,
{
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let dimensions = create_blocking_resource(
        move || tenant_rs.get(),
        move |current_tenant| fetch_dimensions(current_tenant.clone()),
    );

    view! {
        <div>
            <Suspense fallback=move || {
                view! { <p>"Loading (Suspense Fallback)..."</p> }
            }>

                {
                    let handle_change_clone = handle_change.clone();
                    move || {
                        let handle_change_clone_clone = handle_change_clone.clone();
                        dimensions
                            .with(move |result| {
                                match result {
                                    Some(Ok(dimension)) => {
                                        view! {
                                            <div>
                                                <ContextForm
                                                    dimensions=dimension.clone()
                                                    context=vec![]
                                                    is_standalone=false
                                                    handle_change=handle_change_clone_clone.clone()
                                                />
                                            </div>
                                        }
                                    }
                                    Some(Err(error)) => {
                                        view! {
                                            <div class="text-red-500">
                                                {"Failed to fetch config data: "} {error}
                                            </div>
                                        }
                                    }
                                    None => {
                                        view! { <div>Loading....</div> }
                                    }
                                }
                            })
                    }
                }

            </Suspense>
        </div>
    }
}

pub fn construct_request_payload(
    overrides: Map<String, Value>,
    conditions: Vec<(String, String, String)>,
) -> Value {
    // Construct the override section
    let override_section: Map<String, Value> = overrides;

    // Construct the context section
    let context_section = if conditions.len() == 1 {
        // Single condition
        let (variable, operator, value) = &conditions[0];
        json!({
            operator: [
                { "var": variable },
                value
            ]
        })
    } else {
        // Multiple conditions inside an "and"
        let and_conditions: Vec<Value> = conditions
            .into_iter()
            .map(|(variable, operator, value)| {
                json!({
                    operator: [
                        { "var": variable },
                        value
                    ]
                })
            })
            .collect();

        json!({ "and": and_conditions })
    };

    // Construct the entire request payload
    let request_payload = json!({
        "override": override_section,
        "context": context_section
    });

    request_payload
}

pub async fn create_context(
    tenant: String,
    overrides: Map<String, Value>,
    conditions: Vec<(String, String, String)>,
) -> Result<String, String> {
    let client = reqwest::Client::new();
    let host = "http://localhost:8080";
    let url = format!("{host}/context");
    let request_payload = construct_request_payload(overrides, conditions);
    let response = client
        .put(url)
        .header("x-tenant", tenant)
        .header("Authorization", "Bearer 12345678")
        .json(&request_payload)
        .send()
        .await
        .map_err(|e| e.to_string())?;
    match response.status() {
        StatusCode::OK => response.text().await.map_err(|e| e.to_string()),
        StatusCode::BAD_REQUEST => Err("Schema Validation Failed".to_string()),
        _ => Err("Internal Server Error".to_string()),
    }
}

#[component]
fn OverrideModalForm<NF>(handle_change: NF) -> impl IntoView
where
    NF: Fn(Map<String, Value>) + 'static + Clone,
{
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let default_config = create_blocking_resource(
        move || tenant_rs.get(),
        move |current_tenant| fetch_default_config(current_tenant.clone()),
    );

    view! {
        <div>
            <Suspense fallback=move || {
                view! { <p>"Loading (Suspense Fallback)..."</p> }
            }>

                {
                    let handle_change_clone = handle_change.clone();
                    move || {
                        let handle_change_clone_clone = handle_change_clone.clone();
                        default_config
                            .with(move |result| {
                                match result {
                                    Some(Ok(config)) => {
                                        view! {
                                            <div>
                                                <OverrideForm
                                                    overrides=Map::new()
                                                    default_config=config.clone()
                                                    is_standalone=false
                                                    handle_change=handle_change_clone_clone.clone()
                                                />
                                            </div>
                                        }
                                    }
                                    Some(Err(error)) => {
                                        view! {
                                            <div class="text-red-500">
                                                {"Failed to fetch config data: "} {error}
                                            </div>
                                        }
                                    }
                                    None => {
                                        view! { <div>Loading....</div> }
                                    }
                                }
                            })
                    }
                }

            </Suspense>
        </div>
    }
}

pub fn extract_and_format(condition: &Value) -> String {
    if condition.is_object() && condition.get("and").is_some() {
        // Handling complex "and" conditions
        let empty_vec = vec![];
        let conditions_json = condition
            .get("and")
            .and_then(|val| val.as_array())
            .unwrap_or(&empty_vec); // Default to an empty vector if not an array

        let mut formatted_conditions = Vec::new();
        for cond in conditions_json {
            formatted_conditions.push(format_condition(cond));
        }

        formatted_conditions.join(" && ")
    } else {
        // Handling single conditions
        format_condition(condition)
    }
}

fn format_condition(condition: &Value) -> String {
    if let Some(ref operator) = condition.as_object().and_then(|obj| obj.keys().next()) {
        let empty_vec = vec![];
        let operands = condition[operator].as_array().unwrap_or(&empty_vec);

        // Handling the "in" operator differently
        if operator.as_str() == "in" {
            let left_operand = &operands[0];
            let right_operand = &operands[1];

            let left_str = if left_operand.is_string() {
                format!("\"{}\"", left_operand.as_str().unwrap())
            } else {
                format!("{}", left_operand)
            };

            if right_operand.is_object() && right_operand["var"].is_string() {
                let var_str = right_operand["var"].as_str().unwrap();
                return format!("{} {} {}", left_str, operator, var_str);
            }
        }

        // Handling regular operators
        if let Some(first_operand) = operands.get(0) {
            if first_operand.is_object() && first_operand["var"].is_string() {
                let key = first_operand["var"].as_str().unwrap_or("UnknownVar");
                if let Some(value) = operands.get(1) {
                    if value.is_string() {
                        return format!(
                            "{} {} \"{}\"",
                            key,
                            operator,
                            value.as_str().unwrap()
                        );
                    } else {
                        return format!("{} {} {}", key, operator, value);
                    }
                }
            }
        }
    }

    "Invalid Condition".to_string()
}

#[component]
fn ModalComponent(handle_submit: Rc<dyn Fn()>) -> impl IntoView {
    let (context_condition, set_context_condition) =
        create_signal::<Vec<(String, String, String)>>(vec![]);
    let (overrides, set_overrides) = create_signal::<Map<String, Value>>(Map::new());

    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let handle_context_change = move |updated_ctx: Vec<(String, String, String)>| {
        set_context_condition.set(updated_ctx);
    };
    let handle_overrides_change = move |updated_overrides: Map<String, Value>| {
        set_overrides.set(updated_overrides);
    };

    let (error_message, set_error_message) = create_signal("".to_string());

    let on_submit = {
        move |ev: MouseEvent| {
            let handle_submit_clone = handle_submit.clone();
            let current_tenant = tenant_rs.get();
            ev.prevent_default();

            logging::log!("tirggering submit");

            spawn_local({
                let handle_submit = handle_submit_clone;
                async move {
                    let result = create_context(
                        current_tenant,
                        overrides.get(),
                        context_condition.get(),
                    )
                    .await;

                    match result {
                        Ok(_) => {
                            handle_submit();
                            modal_action("my_modal_5", "close")
                        }
                        Err(e) => {
                            if e.is_empty() {
                                set_error_message
                                    .set("Internal_Server_error".to_string());
                            } else {
                                set_error_message.set(e);
                            }
                        }
                    }
                }
            });
        }
    };

    view! {
        <dialog id="my_modal_5" class="modal">
            <div class="modal-box relative bg-white w-12/12 max-w-4xl">
                <form method="dialog" class="flex justify-end">
                    <button>
                        <i class="ri-close-fill"></i>
                    </button>
                </form>
                <form class="form-control w-full mt-8 bg-white text-gray-700 font-mono">
                    <div>
                        <ContextModalForm handle_change=handle_context_change/>
                    </div>
                    <div class="mt-7">
                        <OverrideModalForm handle_change=handle_overrides_change/>
                    </div>
                    <div class="form-control mt-7">
                        <Button
                            text="Submit".to_string()
                            on_click=move |ev: MouseEvent| on_submit(ev)
                        />
                    </div>
                    <div class="mt-7">
                        <p class="text-red-500">{move || error_message.get()}</p>
                    </div>
                </form>
            </div>
        </dialog>
    }
}

#[component]
pub fn ContextOverride() -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let context_data: Vec<(String, String, String)> = vec![];
    let ctx: RwSignal<Vec<(String, String, String)>> = create_rw_signal(context_data);

    let override_data = Map::new();

    let ovr_data = create_rw_signal(override_data);

    provide_context(ctx);

    provide_context(ovr_data);

    let config_data = create_blocking_resource(
        move || tenant_rs.get(),
        move |current_tenant| fetch_config(current_tenant.clone()),
    );

    let table_columns = create_memo(move |_| {
        vec![
            Column::default("KEY".to_string()),
            Column::default("VALUE".to_string()),
        ]
    });

    view! {
        <div class="p-8">
            <div class="flex justify-between">
                <h2 class="card-title">Overrides</h2>
                <Button
                    text="Create Context Overrides".to_string()
                    on_click=|_| modal_action("my_modal_5", "open")
                />
            </div>
            <div class="space-y-6">
                <ModalComponent handle_submit=Rc::new(move || config_data.refetch())/>
                <Suspense fallback=move || {
                    view! { <p>"Loading (Suspense Fallback)..."</p> }
                }>

                    {move || {
                        config_data
                            .with(move |result| {
                                match result {
                                    Some(Ok(config)) => {
                                        let mut contexts: Vec<Map<String, Value>> = Vec::new();
                                        let mut context_views = Vec::new();
                                        let mut override_signal = Map::new();
                                        for context in config.contexts.iter() {
                                            for key in context.override_with_keys.iter() {
                                                let mut map = Map::new();
                                                let ovr = config.overrides.get(key).unwrap();
                                                let ovr_obj = ovr.as_object().unwrap();
                                                for (key, value) in ovr_obj.iter() {
                                                    let trimmed_key = Value::String(
                                                        key.trim_matches('"').to_string(),
                                                    );
                                                    let formatted_value = Value::String(
                                                        format!("{}", value).trim_matches('"').to_string(),
                                                    );
                                                    override_signal
                                                        .insert(trimmed_key.to_string(), formatted_value.clone());
                                                    map.insert("KEY".to_string(), trimmed_key);
                                                    map.insert("VALUE".to_string(), formatted_value);
                                                    contexts.push(map.clone());
                                                }
                                            }
                                            context_views
                                                .push(
                                                    view! {
                                                        <div class="rounded-lg shadow-md bg-white dark:bg-gray-800 p-6 shadow-md">
                                                            <div class="flex justify-between">
                                                                <div class="flex items-center space-x-4">

                                                                    <h3 class="card-title text-base timeline-box text-gray-800 dark:text-white bg-white shadow-md font-mono">
                                                                        "Condition"
                                                                    </h3>
                                                                    <i class="ri-arrow-right-fill ri-xl text-blue-500"></i>
                                                                    <ConditionPills context=context.condition.clone()/>
                                                                </div>
                                                                <button class="p-2 rounded hover:bg-gray-200 dark:hover:bg-gray-600 transition-colors">
                                                                    <i class="ri-edit-line text-blue-500"></i>
                                                                </button>
                                                            </div>
                                                            <div class="space-x-4">
                                                                <Table
                                                                    table_style="font-mono".to_string()
                                                                    rows=contexts.clone()
                                                                    key_column="id".to_string()
                                                                    columns=table_columns.get()
                                                                />
                                                            </div>

                                                        </div>
                                                    },
                                                );
                                            contexts.clear();
                                        }
                                        ovr_data.set(override_signal);
                                        context_views
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
        </div>
    }
}
