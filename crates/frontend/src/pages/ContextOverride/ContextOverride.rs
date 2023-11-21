// use std::collections::HashMap;
use std::rc::Rc;

use crate::components::context_form::context_form::ContextForm;
use crate::components::override_form::override_form::OverrideForm;
use crate::components::table::types::TableSettings;
use crate::components::table::{table::Table, types::Column};
use crate::pages::DefaultConfig::types::Config;
use crate::pages::ExperimentList::types::{DefaultConfig, Dimension};
use leptos::ev::SubmitEvent;
// use leptos::spawn_local;
use leptos::svg::view;
use leptos::*;
use leptos_router::use_query_map;
use serde_json::{Map, Value};

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

// pub async fn create_context(
//     tenant: String,
//     key: String,
//     value: String,
//     key_type: String,
//     pattern: String,
// ) -> Result<String, String> {
//     let client = reqwest::Client::new();
//     let host = "http://localhost:8080";
//     let url = format!("{host}/context");
//     let mut req_body: HashMap<&str, Value> = HashMap::new();
//     let mut schema: Map<String, Value> = Map::new();
//     schema.insert("type".to_string(), Value::String(key_type));
//     schema.insert("pattern".to_string(), Value::String(pattern));
//     req_body.insert("value", Value::String(value));
//     req_body.insert("schema", Value::Object(schema));
//     let response = client
//         .put(url)
//         .header("x-tenant", tenant)
//         .header("Authorization", "Bearer 12345678")
//         .json(&req_body)
//         .send()
//         .await
//         .map_err(|e| e.to_string())?;
//     response.text().await.map_err(|e| e.to_string())
// }

pub async fn fetch_dimensions(tenant: String) -> Result<Vec<Dimension>, String> {
    let client = reqwest::Client::new();
    let host = "http://localhost:8080";
    let url = format!("{host}/dimension");
    match client.get(url).header("x-tenant", tenant).send().await {
        Ok(response) => {
            let dimensions = response.json().await.map_err(|e| e.to_string())?;
            Ok(dimensions)
        }
        Err(e) => Err(e.to_string()),
    }
}

#[component]
fn ContextModalForm() -> impl IntoView {
    let query = use_query_map();

    let tenant = query.with(|params_map| {
        params_map
            .get("tenant")
            .cloned()
            .unwrap_or_else(|| "mjos".to_string())
    });
    let tenant = tenant.clone();

    let dimensions =
        create_blocking_resource(|| {}, move |_| fetch_dimensions(tenant.clone()));

    let context = use_context::<RwSignal<Vec<(String, String, String)>>>();

    view! {
        <div>
            <Suspense fallback=move || {
                view! { <p>"Loading (Suspense Fallback)..."</p> }
            }>

                {move || {
                    dimensions
                        .with(move |result| {
                            match result {
                                Some(Ok(dimension)) => {
                                    view! {
                                        <div>
                                            <ContextForm
                                                dimensions=dimension.clone()
                                                context=context.unwrap().get().clone()
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
                }}

            </Suspense>
        </div>
    }
}

pub async fn fetch_default_config(tenant: String) -> Result<Vec<DefaultConfig>, String> {
    let client = reqwest::Client::new();
    let host = "http://localhost:8080";
    let url = format!("{host}/default-config");
    match client.get(url).header("x-tenant", tenant).send().await {
        Ok(response) => {
            let default_config = response.json().await.map_err(|e| e.to_string())?;
            Ok(default_config)
        }
        Err(e) => Err(e.to_string()),
    }
}

#[component]
fn OverrideModalForm() -> impl IntoView {
    let query = use_query_map();

    let tenant = query.with(|params_map| {
        params_map
            .get("tenant")
            .cloned()
            .unwrap_or_else(|| "mjos".to_string())
    });
    let tenant = tenant.clone();

    let default_config =
        create_blocking_resource(|| {}, move |_| fetch_default_config(tenant.clone()));

    let override_data = use_context::<RwSignal<Map<String, Value>>>();

    view! {
        <div>
            <Suspense fallback=move || {
                view! { <p>"Loading (Suspense Fallback)..."</p> }
            }>

                {move || {
                    default_config
                        .with(move |result| {
                            match result {
                                Some(Ok(config)) => {
                                    view! {
                                        <div>
                                            <OverrideForm
                                                overrides=override_data.unwrap().get().clone()
                                                default_config=config.clone()
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
                }}

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

        formatted_conditions.join(" and ")
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

// Vec[Condition] ->  Update it.
// Vec[Override] -> Update it.

#[component]
fn ModalComponent(handle_submit: Rc<dyn Fn()>) -> impl IntoView {
    view! {
        <div class="p-6 text-gray-600 space-y-6">
            <button class="btn btn-outline btn-primary" onclick="my_modal_5.showModal()">
                Create Context Overrides
                <i class="ri-edit-2-line ml-2"></i>
            </button>
            //
            <dialog id="my_modal_5" class="modal modal-bottom sm:modal-middle">
                <div class="modal-box relative bg-white space-y-6 w-11/12 max-w-3xl">
                    <form method="dialog" class="flex justify-end">
                        <button>
                            <i class="ri-close-fill"></i>
                        </button>
                    </form>
                    // on:submit=on_submit
                    <form class="form-control w-full space-y-4 bg-white text-gray-700 font-mono">
                        <ContextModalForm/>
                        <OverrideModalForm/>
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
                </div>
            </dialog>
        </div>
    }
}

fn parse_conditions(input: String) -> Vec<(String, String, String)> {
    let mut conditions = Vec::new();
    let operators = vec!["==", "in", "!="]; // Define your operators here

    // Split the string by "and" and iterate over each condition
    for condition in input.split("and") {
        let mut parts = Vec::new();
        let mut operator_found = "";

        // Check for each operator
        for operator in &operators {
            if condition.contains(operator) {
                operator_found = operator;
                parts = condition.split(operator).collect();
                break;
            }
        }

        if parts.len() == 2 {
            conditions.push((
                parts[0].trim().to_string(),
                operator_found.to_string(),
                parts[1].trim().to_string(),
            ));
        }
    }

    conditions
}

#[component]
pub fn ContextOverride() -> impl IntoView {
    let query = use_query_map();

    let tenant = query.with(|params_map| {
        params_map
            .get("tenant")
            .cloned()
            .unwrap_or_else(|| "mjos".to_string())
    });

    let context_data: Vec<(String, String, String)> = vec![];
    let ctx: RwSignal<Vec<(String, String, String)>> = create_rw_signal(context_data);

    let override_data = Map::new();

    let ovr_data = create_rw_signal(override_data);

    provide_context(ctx);

    provide_context(ovr_data);

    let config_data =
        create_blocking_resource(|| {}, move |_| fetch_config(tenant.clone()));

    let table_columns = create_memo(move |_| {
        vec![
            Column::default("KEY".to_string()),
            Column::default("VALUE".to_string()),
        ]
    });

    view! {
        <div class="p-8 space-y-6 bg-gray-120">
            <div class="container mx-auto space-y-6 p-8">
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
                                        let settings = TableSettings {
                                            redirect_prefix: None
                                        };
                                        let mut context_views = Vec::new();
                                        let mut new_ctx: Vec<(String, String, String)> = vec![];
                                        let mut override_signal = Map::new();
                                        for context in config.contexts.iter() {
                                            let condition = extract_and_format(&context.condition);
                                            let ctx_values = parse_conditions(condition.clone());
                                            new_ctx.extend(ctx_values);
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
                                                        <div class="rounded-lg shadow-md bg-white dark:bg-gray-800 p-6">
                                                            <div class="card-body bg-white dark:bg-gray-800 bg-white shadow-md">
                                                                <div class="flex justify-between">
                                                                    <div class="flex items-center space-x-4">
                                                                        <i class="ri-settings-5-line ri-xl text-blue-500"></i>
                                                                        <h2 class="card-title chat-bubble text-gray-800 dark:text-white font-mono">
                                                                            "Condition"
                                                                        </h2>
                                                                        <i class="ri-arrow-right-fill ri-xl text-blue-500"></i>
                                                                        <div class="badge badge-primary font-mono">{condition}</div>
                                                                    </div>
                                                                    <div class="card-title chat-bubble text-gray-800 dark:text-white font-mono">
                                                                        <i class="ri-edit-line text-blue-500"></i>
                                                                    </div>
                                                                </div>
                                                                <div class="space-x-4">
                                                                    <Table
                                                                        table_style="font-mono".to_string()
                                                                        rows=contexts.clone()
                                                                        key_column="id".to_string()
                                                                        columns=table_columns.get()
                                                                        settings= settings.clone()
                                                                    />
                                                                </div>
                                                            </div>

                                                        </div>
                                                    },
                                                );
                                            contexts.clear();
                                        }
                                        ctx.set(new_ctx);
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
