use leptos::*;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use wasm_bindgen::JsCast;
use web_sys::{HtmlInputElement, HtmlSelectElement, HtmlSpanElement, MouseEvent};

use crate::{
    api::fetch_dimensions,
    components::{button::button::Button, context_form::context_form::ContextForm},
};

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
    match client
        .get(url)
        .query(&[("show_reasoning", "true")])
        .header("x-tenant", tenant)
        .send()
        .await
    {
        Ok(response) => {
            let config = response.json().await.map_err(|e| e.to_string())?;
            Ok(config)
        }
        Err(e) => Err(e.to_string()),
    }
}

fn parse_conditions(input: String) -> Vec<(String, String, String)> {
    let mut conditions = Vec::new();
    let operators = vec!["==", "in"];

    // Split the string by "&&" and iterate over each condition
    for condition in input.split("&&") {
        let mut parts = Vec::new();
        let mut operator_found = "";

        // Check for each operator
        for operator in &operators {
            if condition.contains(operator) {
                operator_found = operator;
                parts = condition.split(operator).map(|s| s.trim()).collect();

                // TODO: add this when context update is enabled
                if parts.len() == 2 && operator == &"in" {
                    parts.swap(0, 1);
                }

                break;
            }
        }

        if parts.len() == 2 {
            let mut key = parts[0].to_string();
            let mut op = operator_found.to_string();
            let mut val = parts[1].to_string();
            // Add a space after key
            key.push(' ');
            if op == "==".to_string() {
                val = val.trim_matches('"').to_string();
                op = "is".to_string();
            } else {
                val = val.trim_matches('"').to_string();
                op = "has".to_string();
            }
            op.push(' ');

            conditions.push((key, op, val));
        }
    }

    conditions
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

fn gen_name_id(s0: &String, s1: &String, s2: &String) -> String {
    format!("{s0}::{s1}::{s2}")
}

#[component]
pub fn home() -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    // let (config_display_rs, config_display_ws) = create_signal(Map::new());
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

    let (_display_configs_rs, _display_configs_ws) = create_signal(true);

    let unstrike = |search_field_prefix: &String, config: &Map<String, Value>| {
        for (dimension, value) in config.into_iter() {
            let search_field_prefix = if search_field_prefix.is_empty() {
                dimension
            } else {
                &search_field_prefix
            };
            let search_field_prefix = gen_name_id(
                search_field_prefix,
                dimension,
                &value
                    .as_str()
                    .unwrap_or(&value.to_string().trim_matches('"')[..])
                    .to_string(),
            );
            logging::log!("search field prefix {:#?}", search_field_prefix);
            let config_name_elements = document()
                .get_elements_by_name(format!("{search_field_prefix}-1").as_str());
            let config_value_elements = document()
                .get_elements_by_name(format!("{search_field_prefix}-2").as_str());
            logging::log!("config_name_elements {:#?}", config_name_elements.length());
            logging::log!(
                "config_value_elements {:#?}",
                config_value_elements.length()
            );
            for i in 0..config_name_elements.length() {
                let item_one = config_name_elements.item(i).expect("missing span");
                let item_two = config_value_elements.item(i).expect("missing span");

                let (config_name_element, config_value_element) = (
                    item_one.dyn_ref::<HtmlSpanElement>().unwrap(),
                    item_two.dyn_ref::<HtmlSpanElement>().unwrap(),
                );
                let _ = config_name_element
                    .class_list()
                    .add_2("text-black", "font-bold");
                let _ = config_name_element.class_list().remove_1("text-gray-300");
                let _ = config_value_element
                    .class_list()
                    .add_2("text-black", "font-bold");
                let _ = config_value_element.class_list().remove_1("text-gray-300");
                logging::log!(
                    "config name after replace {} and value {}",
                    config_name_element.to_string(),
                    config_value_element.to_string()
                );
            }
        }
    };

    let gen_query_context = |query: Vec<(String, String, String)>| -> String {
        let mut context: Vec<String> = vec![];
        for (dimension, op, value) in query.iter() {
            let op = match op.as_str() {
                "==" => "=",
                _ => break, // query params do not support the other operators :  != and IN, do something differently later
            };
            context.push(format!(
                "{}{op}{}",
                dimension.to_lowercase(),
                value.to_lowercase()
            ));
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
            let _ = config_name_element
                .class_list()
                .remove_2("text-black", "font-bold");
            let _ = config_name_element.class_list().add_1("text-gray-300");
            let _ = config_value_element
                .class_list()
                .remove_2("text-black", "font-bold");
            let _ = config_value_element.class_list().add_1("text-gray-300");
        }
        logging::log!("query vector {:#?}", query_vector);
        // resolve the context and get the config that would apply
        spawn_local(async move {
            let context = gen_query_context(query_vector);
            let mut config = match resolve_config(tenant_rs.get(), context).await.unwrap()
            {
                Value::Object(m) => m,
                _ => Map::new(),
            };
            logging::log!("resolved config {:#?}", config);
            // unstrike those that we want to show the user
            // if metadata field is found, unstrike only that override
            match config.remove("metadata") {
                Some(Value::Array(metadata)) => {
                    if metadata.len() == 0 {
                        logging::log!("unstrike default config");
                        unstrike(&String::new(), &config);
                    }
                    for applied in metadata.iter() {
                        logging::log!("applied config {:#?}", applied);
                        applied["override"]
                            .as_array()
                            .unwrap_or(&vec![])
                            .iter()
                            .for_each(|override_id| {
                                logging::log!("unstrike {:#?}", override_id);
                                unstrike(
                                    &override_id.as_str().unwrap().to_string(),
                                    &config,
                                )
                            });
                    }
                }
                _ => {
                    logging::log!(
                        "no metadata recieved, default config is the config to be used"
                    );
                }
            }
            logging::log!("unstrike default config if needed");
            unstrike(&String::new(), &config);

            let resolution_card = document()
                .get_element_by_id("resolved_table_body")
                .expect("resolve table card not found");

            let mut table_rows = String::new();
            for (key, value) in config.iter() {
                table_rows.push_str(
                    format!(
                        "<tr><td>{key}</td><td>{}</td></tr>",
                        value.as_str().unwrap()
                    )
                    .as_str(),
                )
            }
            resolution_card.set_inner_html(&table_rows);
        });
    };
    view! {
        <div class="flex w-full flex-row mt-5 justify-evenly">
            <div class="card mr-5 ml-5 mt-6 h-4/5 shadow bg-base-100 w-4/12">
                <Suspense fallback=move || {
                    view! { <p>"Loading..."</p> }
                }>
                    {move || {
                        dimension_resource
                            .with(|dimension| {
                                view! {
                                    <div class="card m-2 bg-base-100">
                                        <div class="card-body">
                                            <h2 class="card-title">Resolve Configs</h2>

                                            <ContextForm
                                                dimensions=dimension.to_owned().unwrap_or(vec![])
                                                context=vec![]
                                                is_standalone=false
                                                handle_change=|_| ()
                                            />
                                            <div class="card-actions justify-end">
                                                <Button text="Resolve".to_string() on_click=resolve_click/>
                                            </div>
                                        </div>
                                    </div>
                                }
                            })
                    }}

                </Suspense>
                // config suspense
                <Suspense fallback=move || {
                    view! { <p>"Loading..."</p> }
                }>

                    {config_data
                        .with(move |conf| {
                            match conf {
                                Some(Ok(config)) => {
                                    let default_configs = config.default_configs.clone();
                                    view! {
                                        <div class="card m-2 bg-base-100">
                                            <div class="card-body">
                                                <h2 class="card-title">Resolved Config</h2>
                                                <table class="table">
                                                    <thead>
                                                        <tr>
                                                            <th>Config Key</th>
                                                            <th>Value</th>
                                                        </tr>
                                                    </thead>
                                                    <tbody id="resolved_table_body">
                                                        <For
                                                            each=move || { default_configs.clone().into_iter() }

                                                            key=|(key, value)| format!("{key}-{value}")
                                                            children=move |(config, value)| {
                                                                view! {
                                                                    <tr>
                                                                        <td>{config}</td>
                                                                        <td>
                                                                            {match value {
                                                                                Value::String(s) => s,
                                                                                Value::Number(num) => num.to_string(),
                                                                                Value::Bool(b) => b.to_string(),
                                                                                _ => "".into(),
                                                                            }}

                                                                        </td>

                                                                    </tr>
                                                                }
                                                            }
                                                        />

                                                    </tbody>
                                                </table>
                                            </div>
                                        </div>
                                    }
                                }
                                Some(Err(error)) => {
                                    view! {
                                        <div class="error">
                                            {"Failed to fetch config data: "} {error}
                                        </div>
                                    }
                                }
                                None => {
                                    view! { <div class="error">{"No config data fetched"}</div> }
                                }
                            }
                        })}

                </Suspense>
            </div>
            <Suspense fallback=move || {
                view! { <p>"Loading (Suspense Fallback)..."</p> }
            }>

                {config_data
                    .with(move |result| {
                        match result {
                            Some(Ok(config)) => {
                                let rows = |k: &String, v: &Value, striked: bool| {
                                    let mut view_vector = vec![];
                                    println!("{:?}", v);
                                    let default_iter = vec![(k.clone(), v.clone())];
                                    for (key, value) in v
                                        .as_object()
                                        .unwrap_or(&Map::from_iter(default_iter))
                                        .iter()
                                    {
                                        let key = key.replace("\"", "").trim().to_string();
                                        let value = format!("{}", value)
                                            .replace("\"", "")
                                            .trim()
                                            .to_string();
                                        let unique_name = gen_name_id(k, &key, &value);
                                        view_vector
                                            .push(
                                                view! {
                                                    < tr > < td > < span name = format!("{unique_name}-1") class
                                                    = "config-name" class : text - black = { ! striked } class :
                                                    font - bold = { ! striked } class : text - gray - 300 = {
                                                    striked } > { key } </ span > </ td > < td > < span name =
                                                    format!("{unique_name}-2") class = "config-value" class :
                                                    text - black = { ! striked } class : font - bold = { !
                                                    striked } class : text - gray - 300 = { striked } > { value
                                                    } </ span > </ td > </ tr >
                                                },
                                            )
                                    }
                                    view_vector
                                };
                                let contexts_views: Vec<_> = config
                                    .contexts
                                    .iter()
                                    .map(|context| {
                                        let condition = parse_conditions(
                                            extract_and_format(&context.condition),
                                        );
                                        let rows: Vec<_> = context
                                            .override_with_keys
                                            .iter()
                                            .filter_map(|key| {
                                                let o = config.overrides.get(key);
                                                if o.is_some() { Some((key, o.unwrap())) } else { None }
                                            })
                                            .map(|(k, v)| { rows(&k, &v, true) })
                                            .collect();
                                        view! {
                                            <div class="card bg-base-100 shadow m-6">
                                                <div class="card-body">
                                                    <h2 class="card-title">
                                                        {condition
                                                            .iter()
                                                            .map(|(dim, op, val)| {
                                                                view! {
                                                                    <span class="inline-flex items-center rounded-md bg-gray-50 px-2 py-1 text-xs ring-1 ring-inset ring-purple-700/10 shadow-md gap-x-2">
                                                                        <span class="font-mono font-medium context_condition text-gray-500">
                                                                            {dim}
                                                                        </span>
                                                                        <span class="font-mono font-medium text-gray-650 context_condition ">
                                                                            {op}
                                                                        </span>
                                                                        <span class="font-mono font-semibold context_condition">
                                                                            {val}
                                                                        </span>
                                                                    </span>
                                                                }
                                                            })
                                                            .collect_view()}

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
                                    .map(|(k, v)| { rows(&k, &v, false) })
                                    .collect();
                                vec![
                                    view! {
                                        <div class="mb-4 w-8/12 overflow-y-auto max-h-screen">
                                            // <div class="form-control p-10">
                                            // <label class="cursor-pointer label">
                                            // <span class="label-text ml-1">Display All Configs</span>
                                            // <input type="checkbox" class="toggle bg-purple-600"
                                            // on:click=move |_| display_configs_ws.update(|toggle| *toggle = !*toggle)
                                            // checked=move || display_configs_rs.get() />
                                            // </label>
                                            // </div>
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
                                        // <div class="form-control p-10">
                                        // <label class="cursor-pointer label">
                                        // <span class="label-text ml-1">Display All Configs</span>
                                        // <input type="checkbox" class="toggle bg-purple-600"
                                        // on:click=move |_| display_configs_ws.update(|toggle| *toggle = !*toggle)
                                        // checked=move || display_configs_rs.get() />
                                        // </label>
                                        // </div>

                                        // <div class="form-control p-10">
                                        // <label class="cursor-pointer label">
                                        // <span class="label-text ml-1">Display All Configs</span>
                                        // <input type="checkbox" class="toggle bg-purple-600"
                                        // on:click=move |_| display_configs_ws.update(|toggle| *toggle = !*toggle)
                                        // checked=move || display_configs_rs.get() />
                                        // </label>
                                        // </div>

                                        <div class="error">
                                            {"Failed to fetch config data: "} {error}
                                        </div>
                                    },
                                ]
                            }
                            None => {
                                vec![
                                    view! {
                                        // <div class="form-control p-10">
                                        // <label class="cursor-pointer label">
                                        // <span class="label-text ml-1">Display All Configs</span>
                                        // <input type="checkbox" class="toggle bg-purple-600"
                                        // on:click=move |_| display_configs_ws.update(|toggle| *toggle = !*toggle)
                                        // checked=move || display_configs_rs.get() />
                                        // </label>
                                        // </div>

                                        // <div class="form-control p-10">
                                        // <label class="cursor-pointer label">
                                        // <span class="label-text ml-1">Display All Configs</span>
                                        // <input type="checkbox" class="toggle bg-purple-600"
                                        // on:click=move |_| display_configs_ws.update(|toggle| *toggle = !*toggle)
                                        // checked=move || display_configs_rs.get() />
                                        // </label>
                                        // </div>

                                        // <div class="form-control p-10">
                                        // <label class="cursor-pointer label">
                                        // <span class="label-text ml-1">Display All Configs</span>
                                        // <input type="checkbox" class="toggle bg-purple-600"
                                        // on:click=move |_| display_configs_ws.update(|toggle| *toggle = !*toggle)
                                        // checked=move || display_configs_rs.get() />
                                        // </label>
                                        // </div>

                                        <div class="error">{"No config data fetched"}</div>
                                    },
                                ]
                            }
                        }
                    })}

            </Suspense>
        </div>
    }
}
