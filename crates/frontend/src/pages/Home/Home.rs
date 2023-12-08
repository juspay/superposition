use leptos::*;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use wasm_bindgen::JsCast;
use web_sys::{HtmlInputElement, HtmlSelectElement, HtmlSpanElement, MouseEvent};

use crate::{api::fetch_dimensions, components::context_form::context_form::ContextForm};

#[derive(Deserialize, Serialize, Clone)]
pub struct Config {
    pub contexts: Vec<Context>,
    pub overrides: Map<String, Value>,
    pub default_configs: Map<String, Value>,
}

#[derive(Deserialize, Serialize, Clone)]
pub struct Context {
    pub id: String,
    pub condition: Value,
    pub override_with_keys: [String; 1],
}

pub async fn fetch_config(tenant: String) -> Result<Config, String> {
    let client = reqwest::Client::new();
    let url = "http://localhost:8080/config";
    match client.get(url).header("x-tenant", tenant).send().await {
        Ok(response) => {
            let config: Config = response.json().await.map_err(|e| e.to_string())?;
            Ok(config)
        }
        Err(e) => Err(e.to_string()),
    }
}

async fn resolve_config(tenant: String, context: String) -> Result<Value, String> {
    let client = reqwest::Client::new();
    let url = format!("http://localhost:8080/config/resolve?{context}");
    match client.get(url).header("x-tenant", tenant).send().await {
        Ok(response) => {
            let config = response.json().await.map_err(|e| e.to_string())?;
            Ok(config)
        }
        Err(e) => Err(e.to_string()),
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

#[component]
pub fn home() -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let config_data = create_blocking_resource(
        move || tenant_rs.get(),
        move |tenant| fetch_config(tenant),
    );
    let dimension_resource = create_resource(
        move || tenant_rs.get(),
        |tenant| async {
            match fetch_dimensions(tenant).await {
                Ok(data) => data,
                Err(_) => vec![],
            }
        },
    );

    let gen_name_id = |s1: &String, s2: &String| format!("{s1}::{s2}");

    let gen_query_context = |query: Vec<(String, String, String)>| -> String {
        let mut context: Vec<String> = vec![];
        for (dimension, op, value) in query.iter() {
            let op = match op.as_str() {
                "==" => "=",
                _ => break, // query params do not support the other operators :  != and IN, do something differently later
            };
            context.push(format!("{}{op}{}", dimension.to_lowercase(), value.to_lowercase()));
        }
        context.join("&").to_string()
    };

    let resolve_click = move |ev: MouseEvent| {
        ev.prevent_default();
        let dimension_labels = document().get_elements_by_name("context-dimension-name");
        let dimension_ops = document().get_elements_by_name("context-dimension-operator");
        let dimension_values = document().get_elements_by_name("context-dimension-value");
        let mut query_vector: Vec<(String, String, String)> = vec![];
        for i in 0..dimension_labels.length() {
            query_vector.push((
                dimension_labels
                    .item(i)
                    .expect("missing input")
                    .dyn_ref::<HtmlSpanElement>()
                    .unwrap()
                    .inner_text(),
                dimension_ops
                    .item(i)
                    .expect("missing input")
                    .dyn_ref::<HtmlSelectElement>()
                    .unwrap()
                    .value(),
                dimension_values
                    .item(i)
                    .expect("missing input")
                    .dyn_ref::<HtmlInputElement>()
                    .unwrap()
                    .value(),
            ))
        }
        // strike out all config elements on the page
        let config_name_elements = document().get_elements_by_class_name("config-name");
        let config_value_elements = document().get_elements_by_class_name("config-value");
        for i in 0..config_name_elements.length() {
            let (config_name_element, config_value_element) = (
                config_name_elements.item(i).unwrap(),
                config_value_elements.item(i).unwrap(),
            );
            config_name_element.set_inner_html(
                format!(
                    "<s>{}</s>",
                    config_name_element
                        .inner_html()
                        .replace("<s>", "")
                        .replace("</s>", "")
                )
                .as_str(),
            );
            config_value_element.set_inner_html(
                format!(
                    "<s>{}</s>",
                    config_value_element
                        .inner_html()
                        .replace("<s>", "")
                        .replace("</s>", "")
                )
                .as_str(),
            );
        }
        logging::log!("query vector {:#?}", query_vector);
        // resolve the context and get the config that would apply
        spawn_local(async move {
            let context = gen_query_context(query_vector);
            let config = match resolve_config(tenant_rs.get(), context).await.unwrap() {
                Value::Object(m) => m,
                _ => Map::new(),
            };
            // unstrike those that we want to show the user
            for (dimension, value) in config.into_iter() {
                let search_field: String =
                    gen_name_id(&dimension, &value.as_str().unwrap().to_string());
                logging::log!("gen ID search param {}", search_field);
                let config_name_elements = document()
                    .get_elements_by_name(format!("{}-1", &search_field).as_str());
                let config_value_elements = document()
                    .get_elements_by_name(format!("{}-2", &search_field).as_str());
                for i in 0..config_name_elements.length() {
                    let item_one = config_name_elements.item(i).expect("missing span");
                    let item_two = config_value_elements.item(i).expect("missing span");

                    let (config_name_element, config_value_element) = (
                        item_one.dyn_ref::<HtmlSpanElement>().unwrap(),
                        item_two.dyn_ref::<HtmlSpanElement>().unwrap(),
                    );
                    let (name, value) = (
                        config_name_element
                            .inner_html()
                            .replace("<s>", "")
                            .replace("</s>", ""),
                        config_value_element
                            .inner_html()
                            .replace("<s>", "")
                            .replace("</s>", ""),
                    );

                    logging::log!("config name after replace {} and value {}", name, value);
                    config_name_element.set_inner_html(format!("{}", name).as_str());
                    config_value_element.set_inner_html(format!("{}", value).as_str());
                }
            }
        });
    };
    view! {
        <div class="flex w-full flex-row mt-5 justify-evenly">
            <Suspense fallback=move || {
                view! { <p>"Loading..."</p> }
            }>
                {move || {
                    dimension_resource
                        .with(|dimension| {
                            view! {
                                <div class="card m-10 bg-base w-4/12">
                                    <div class="card-body">
                                        <h2 class="card-title">Resolve Configs</h2>

                                        <ContextForm
                                            dimensions=dimension.to_owned().unwrap_or(vec![])
                                            context=vec![]
                                        />
                                        <div class="card-actions justify-end">
                                            <button class="btn btn-primary" on:click=resolve_click>
                                                Resolve
                                            </button>
                                        </div>
                                    </div>
                                </div>
                            }
                        })
                }}

            </Suspense>
            <Suspense fallback=move || {
                view! { <p>"Loading (Suspense Fallback)..."</p> }
            }>

                {config_data
                    .with(move |result| {
                        match result {
                            Some(Ok(config)) => {
                                let rows = |k: &String, v: &Value| {
                                    let key = k.replace("\"", "").trim().to_string();
                                    let value = format!("{}", v)
                                        .replace("\"", "")
                                        .trim()
                                        .to_string();
                                    let name_identifier = gen_name_id(&key, &value);
                                    view! {
                                        <tr>
                                            <td>
                                                <span name={format!("{}-1", &name_identifier)} class="config-name">
                                                    {key}
                                                </span>
                                            </td>
                                            <td>
                                                <span name={format!("{}-2", &name_identifier)} class="config-value">
                                                    {value}
                                                </span>
                                            </td>
                                        </tr>
                                    }
                                };
                                let contexts_views: Vec<_> = config
                                    .contexts
                                    .iter()
                                    .map(|context| {
                                        let condition = extract_and_format(&context.condition);
                                        let rows: Vec<_> = context
                                            .override_with_keys
                                            .iter()
                                            .filter_map(|key| config.overrides.get(key))
                                            .flat_map(|ovr| ovr.as_object().unwrap().iter())
                                            .map(|(k, v)| { rows(&k, &v) })
                                            .collect();
                                        view! {
                                            <div class="card bg-base-100 shadow m-6">
                                                <div class="card-body">
                                                    <h2 class="card-title">
                                                        "Condition: "
                                                        <div class="badge badge-lg badge-primary p-5">
                                                            {&condition}
                                                        </div>
                                                    </h2>
                                                    <table class="table mt-10">
                                                        <thead>
                                                            <tr>
                                                                <th>Key</th>
                                                                <th>Value</th>
                                                            </tr>
                                                        </thead>
                                                        <tbody>{rows}</tbody>
                                                    </table>

                                                </div>
                                            </div>
                                        }
                                    })
                                    .collect::<Vec<_>>();
                                let new_context_views = contexts_views
                                    .into_iter()
                                    .rev()
                                    .collect::<Vec<_>>();
                                let default_config: Vec<_> = config
                                    .default_configs
                                    .iter()
                                    .map(|(k, v)| { rows(&k, &v) })
                                    .collect();
                                vec![
                                    view! {
                                        <div class="mb-4 w-8/12 overflow-y-auto max-h-screen">
                                            {new_context_views}
                                            <div class="card bg-base-100 shadow m-6">
                                                <div class="card-body">
                                                    <h2 class="card-title">Default Configuration</h2>
                                                    <table class="table">
                                                        <thead>
                                                            <tr>
                                                                <th>Key</th>
                                                                <th>Value</th>
                                                            </tr>
                                                        </thead>
                                                        <tbody>{default_config}</tbody>
                                                    </table>
                                                </div>
                                            </div>
                                        </div>
                                    },
                                ]
                            }
                            Some(Err(error)) => {
                                vec![
                                    view! {
                                        <div class="error">
                                            {"Failed to fetch config data: "} {error}
                                        </div>
                                    },
                                ]
                            }
                            None => {
                                vec![view! { <div class="error">{"No config data fetched"}</div> }]
                            }
                        }
                    })}

            </Suspense>
        </div>
    }
}
