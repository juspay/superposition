use std::env;

use crate::types::Envs;
use leptos::*;
use serde_json::Value;
use wasm_bindgen::JsCast;

pub fn modal_action(name: &str, action: &str) {
    if let Some(window) = web_sys::window() {
        if let Some(document) = window.document() {
            if let Some(modal) = document.get_element_by_id(name) {
                logging::log!("Modal found");
                if let Some(el) = modal.dyn_ref::<web_sys::HtmlDialogElement>() {
                    if action == "close" {
                        el.close();
                    } else {
                        let _ = el.show_modal();
                    }
                }
            } else {
                logging::log!("Modal element not found");
            }
        }
    }
}

pub fn add_prefix(o_str: &str, prefix: &str) -> String {
    match prefix {
        "" | "/" => o_str.to_owned(),
        prefix => o_str.to_owned() + "/" + prefix,
    }
}

pub fn is_server() -> bool {
    env::var("SERVER_NAME").is_ok()
}

pub fn use_url_base() -> String {
    let service_prefix = use_service_prefix();
    match service_prefix.as_str() {
        "" | "/" => "".to_owned(),
        prefix => "/".to_owned() + prefix,
    }
}

pub fn use_host_server() -> String {
    let service_prefix = use_service_prefix();
    if is_server() {
        add_prefix("http://localhost:8080", &service_prefix)
    } else {
        get_host()
    }
}

pub fn get_host() -> String {
    let context = use_context::<Envs>();
    let service_prefix = use_service_prefix();
    let host = context
        .map(|ctx| ctx.host)
        .or_else(|| match js_sys::eval("__APP_ENVS?.host") {
            Ok(value) => value
                .dyn_into::<js_sys::JsString>()
                .expect("host is not a string")
                .as_string(),
            Err(e) => {
                logging::log!("Unable to fetch host from __APP_ENVS: {:?}", e);
                None
            }
        })
        .unwrap_or(String::from("http://localhost:xxxx"));

    add_prefix(&host, &service_prefix)
}

pub fn get_tenants() -> Vec<String> {
    let context = use_context::<Envs>();
    context
        .map(|ctx: Envs| ctx.tenants)
        .or_else(|| {
            let tenant_value = match js_sys::eval("__APP_ENVS?.tenants") {
                Ok(value) => value
                    .dyn_into::<js_sys::Array>()
                    .expect("tenants is not an array")
                    .to_vec()
                    .into_iter()
                    .map(|tenant| {
                        tenant.dyn_into::<js_sys::JsString>().ok().map(String::from)
                    })
                    .collect::<Option<Vec<String>>>(),
                Err(e) => {
                    logging::log!("Unable to fetch tenants from __APP_ENVS: {:?}", e);
                    None
                }
            };
            tenant_value
        })
        .unwrap_or(vec![])
}

pub fn use_env() -> Envs {
    let context = use_context::<Envs>();
    context
        .or_else(|| {
            let envs = match js_sys::eval("__APP_ENVS") {
                Ok(value) => {
                    let env_obj = value
                        .dyn_into::<js_sys::Object>()
                        .expect("__APP_ENV is not an object");
                    let env_str: &'static str = Box::leak(
                        js_sys::JSON::stringify(&env_obj)
                            .ok()
                            .map(String::from)
                            .unwrap_or(String::new())
                            .into_boxed_str(),
                    );
                    let envs = serde_json::from_str::<Envs>(env_str)
                        .expect("unable to parse to Envs struct");
                    Some(envs)
                }
                Err(e) => {
                    logging::log!("Unable to fetch __APP_ENVS: {:?}", e);
                    None
                }
            };
            envs
        })
        .expect("unable to get envs")
}

pub fn use_service_prefix() -> String {
    let context = use_context::<Envs>();
    context
        .map(|ctx: Envs| String::from(ctx.service_prefix))
        .or_else(|| {
            let service_prefix_value = match js_sys::eval("__APP_ENV?.service_prefix") {
                Ok(value) => value.dyn_into::<js_sys::JsString>().map(String::from).ok(),
                Err(e) => {
                    logging::log!(
                        "Unable to fetch service_prefix from __APP_ENVS: {:?}",
                        e
                    );
                    None
                }
            };
            service_prefix_value
        })
        .unwrap_or(String::new())
}

pub fn get_element_by_id<T>(id: &'static str) -> Option<T>
where
    T: wasm_bindgen::JsCast + Clone,
{
    let option_dom_ele = document().get_element_by_id(id);
    logging::log!("DOM element found {:?}", option_dom_ele);
    match option_dom_ele {
        Some(dom_ele) => dom_ele.dyn_ref::<T>().cloned(),
        None => None,
    }
}

pub fn show_modal(id: &'static str) {
    let option_dialog_ele = get_element_by_id::<web_sys::HtmlDialogElement>(id);
    if let Some(dialog_ele) = option_dialog_ele {
        let _ = dialog_ele.show_modal();
        logging::log!("{:?}", dialog_ele);
    }
}

pub fn close_modal(id: &'static str) {
    let option_dialog_ele = get_element_by_id::<web_sys::HtmlDialogElement>(id);
    if let Some(dialog_ele) = option_dialog_ele {
        let _ = dialog_ele.close();
        logging::log!("{:?}", dialog_ele);
    }
}

pub fn parse_string_to_json_value_vec(input: &str) -> Vec<Value> {
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

pub fn get_variable_name_and_value(
    operands: &Vec<Value>,
) -> Result<(&str, String), String> {
    let (obj_pos, variable_obj) = operands
        .iter()
        .enumerate()
        .find(|(_, operand)| {
            operand.is_object()
                && operand
                    .as_object()
                    .expect("unable to parse operands as object")
                    .get("var")
                    .is_some()
        })
        .ok_or(" failed to get variable name from operands list".to_string())?;

    let variable_name = variable_obj
        .as_object()
        .map_or(None, |obj| obj.get("var"))
        .map_or(None, |value| value.as_str())
        .ok_or(" failed to get variable name from operands list".to_string())?;

    let variable_value = operands
        .into_iter()
        .enumerate()
        .filter(|(idx, _)| *idx != obj_pos)
        .map(|(_, val)| val.to_string().replace("\"", ""))
        .collect::<Vec<String>>()
        .join(",");

    Ok((variable_name, variable_value))
}

pub fn extract_conditions(
    context_json: &Value,
) -> Result<Vec<(String, String, String)>, String> {
    // Assuming max 2-level nesting in context json logic
    let context = context_json.as_object().ok_or(
        "An error occurred while extracting dimensions: context not a valid JSON object"
            .to_string(),
    )?;

    let conditions = match context.get("and") {
        Some(conditions_json) => conditions_json
            .as_array()
            .ok_or("An error occurred while extracting dimensions: failed parsing conditions as an array".to_string())?
            .clone(),
        None => vec![context_json.clone()],
    };

    let mut condition_tuples = Vec::new();
    for condition in &conditions {
        let condition_obj = condition
            .as_object()
            .ok_or("failed to parse condition as an object".to_string())?;
        let operators = condition_obj.keys();

        for operator in operators {
            let operands = condition_obj[operator]
                .as_array()
                .ok_or("failed to parse operands as an arrays".to_string())?;

            let (variable_name, variable_value) = get_variable_name_and_value(operands)?;

            condition_tuples.push((
                String::from(variable_name),
                operator.to_owned(),
                variable_value.to_owned(),
            ));
        }
    }

    Ok(condition_tuples)
}
