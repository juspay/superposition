use std::env;

use crate::{
    components::alert::AlertType,
    providers::alert_provider::enqueue_alert,
    types::{DefaultConfig, Dimension, Envs, ErrorResponse},
};
use cfg_if::cfg_if;
use leptos::*;
use reqwest::header::{HeaderMap, HeaderName, HeaderValue};
use serde::de::DeserializeOwned;
use serde_json::{Map, Value};
use std::str::FromStr;
use url::Url;
use wasm_bindgen::JsCast;

#[allow(dead_code)]
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
    env::var("SERVICE_NAME").is_ok()
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
        .map_or(String::from("http://localhost:xxxx"), |host_from_env| {
            let opt_host_origin =
                web_sys::window().and_then(|window| window.location().origin().ok());
            match (host_from_env.as_str(), opt_host_origin) {
                ("" | "/", Some(host_origin)) => host_origin,
                _ => host_from_env,
            }
        });

    add_prefix(&host, &service_prefix)
}

pub fn get_tenants() -> Vec<String> {
    let context = use_context::<Envs>();
    context
        .map(|ctx: Envs| ctx.tenants)
        .or_else(|| match js_sys::eval("__APP_ENVS?.tenants") {
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
        })
        .unwrap_or_default()
}

#[allow(dead_code)]
pub fn use_env() -> Envs {
    let context = use_context::<Envs>();
    context
        .or_else(|| {
            let envs = match js_sys::eval("__APP_ENVS") {
                Ok(value) => {
                    let env_obj = value
                        .dyn_into::<js_sys::Object>()
                        .expect("__APP_ENVS is not an object");
                    let env_str: &'static str = Box::leak(
                        js_sys::JSON::stringify(&env_obj)
                            .ok()
                            .map(String::from)
                            .unwrap_or_default()
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
        .or_else(|| match js_sys::eval("__APP_ENVS?.service_prefix") {
            Ok(value) => value.dyn_into::<js_sys::JsString>().map(String::from).ok(),
            Err(e) => {
                logging::log!("Unable to fetch service_prefix from __APP_ENVS: {:?}", e);
                None
            }
        })
        .unwrap_or_default()
}

pub fn get_element_by_id<T>(id: &str) -> Option<T>
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

pub fn show_modal(id: &str) {
    let option_dialog_ele = get_element_by_id::<web_sys::HtmlDialogElement>(id);
    if let Some(dialog_ele) = option_dialog_ele {
        let _ = dialog_ele.show_modal();
        logging::log!("{:?}", dialog_ele);
    }
}

pub fn close_modal(id: &str) {
    let option_dialog_ele = get_element_by_id::<web_sys::HtmlDialogElement>(id);
    if let Some(dialog_ele) = option_dialog_ele {
        dialog_ele.close();
        logging::log!("{:?}", dialog_ele);
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
        .and_then(|obj| obj.get("var"))
        .and_then(|value| value.as_str())
        .ok_or(" failed to get variable name from operands list".to_string())?;

    let variable_value = operands
        .iter()
        .enumerate()
        .filter(|(idx, _)| *idx != obj_pos)
        .map(|(_, val)| val.to_string().replace('"', ""))
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

pub fn check_url_and_return_val(s: String) -> String {
    match Url::parse(&s) {
        Ok(_) => format!(
            "<a class='value_link' href={} target='_blank'>{}</a>",
            &s, &s
        ),
        Err(_) => s,
    }
}

pub enum ConfigType {
    DefaultConfig(DefaultConfig),
    Dimension(Dimension),
}

#[derive(Clone, Debug)]
pub enum ConfigValueType {
    Boolean,
    Number,
    String,
    Null,
    Integer,
    Other,
}

impl FromStr for ConfigValueType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "boolean" => Ok(ConfigValueType::Boolean),
            "number" => Ok(ConfigValueType::Number),
            "string" => Ok(ConfigValueType::String),
            "null" => Ok(ConfigValueType::Null),
            "integer" => Ok(ConfigValueType::Integer),
            _ => Ok(ConfigValueType::Other),
        }
    }
}

pub fn get_config_type(
    configs: &[ConfigType],
    key_name: &str,
) -> Option<Vec<ConfigValueType>> {
    let config = configs.iter().find(|conf| match conf {
        ConfigType::DefaultConfig(default_conf) => default_conf.key == key_name,
        ConfigType::Dimension(dimension) => dimension.dimension == key_name,
    });

    let type_from_str = |type_str: Option<&str>| {
        type_str
            .map(|t| ConfigValueType::from_str(t).ok())
            .flatten()
            .unwrap_or(ConfigValueType::Other)
    };

    let types_mapping = |schema_type: &Value| match schema_type {
        Value::Array(types) => types
            .iter()
            .map(|item: &Value| type_from_str(item.as_str()))
            .collect::<Vec<ConfigValueType>>(),
        Value::String(type_str) => vec![type_from_str(Some(type_str.as_str()))],
        _ => vec![ConfigValueType::Other],
    };

    config.and_then(|config| match config {
        ConfigType::DefaultConfig(default_conf) => {
            default_conf.schema.get("type").map(|t| types_mapping(t))
        }

        ConfigType::Dimension(dimension) => {
            dimension.schema.get("type").map(|t| types_mapping(t))
        }
    })
}

pub fn parse_value(val: &str, config_type: ConfigValueType) -> Result<Value, String> {
    match config_type {
        ConfigValueType::Boolean => bool::from_str(val)
            .map(Value::Bool)
            .map_err(|_| "Invalid boolean".to_string()),
        ConfigValueType::Number | ConfigValueType::Integer => {
            let parsed_value: Value = serde_json::from_str(val)
                .map_err(|_| "Invalid number or number array format".to_string())?;
            match parsed_value {
                Value::Number(num) => Ok(Value::Number(num)),
                Value::Array(arr) => {
                    for item in &arr {
                        if !item.is_number() {
                            return Err("Array contains non-number value".to_string());
                        }
                    }
                    Ok(Value::Array(arr))
                }
                _ => Err(format!(
                    "{:?} is either an invalid number or a invalid number array.",
                    val
                )),
            }
        }
        ConfigValueType::String => {
            let parsed_value: Result<Value, _> = serde_json::from_str(&val);
            match parsed_value {
                Ok(Value::String(s)) => Ok(Value::String(s)),
                Ok(Value::Array(arr)) => {
                    for item in &arr {
                        if !item.is_string() {
                            return Err("Array contains non-string value".to_string());
                        }
                    }
                    Ok(Value::Array(arr))
                }
                Ok(_) => Err(format!(
                    "{:?} is either an invalid string or a invalid string array.",
                    val
                )),
                Err(_) => Ok(Value::String(val.to_string())),
            }
        }
        ConfigValueType::Null if val == "null" => Ok(Value::Null),
        _ => Value::from_str(val).map_err(|err| format!("Error parsing JSON: {}", err)),
    }
}

pub fn get_config_value(
    name: &str,
    val: &str,
    configs: &[ConfigType],
) -> Result<Value, String> {
    let config_type = get_config_type(configs, name);

    match config_type {
        Some(possible_types) => {
            for possible_type in possible_types {
                if let Ok(parsed_value) = parse_value(val, possible_type) {
                    return Ok(parsed_value);
                }
            }
            Err("Error parsing config value".to_string())
        }
        None => {
            Value::from_str(val).map_err(|err| format!("Error parsing JSON: {}", err))
        }
    }
}

/********* Request Utils **********/

use once_cell::sync::Lazy;
static HTTP_CLIENT: Lazy<reqwest::Client> = Lazy::new(reqwest::Client::new);

pub fn construct_request_headers(entries: &[(&str, &str)]) -> Result<HeaderMap, String> {
    entries
        .iter()
        .map(|(name, value)| {
            let h_name = HeaderName::from_str(name);
            let h_value = HeaderValue::from_str(value);

            match (h_name, h_value) {
                (Ok(n), Ok(v)) => Some((n, v)),
                _ => None,
            }
        })
        .collect::<Option<Vec<(HeaderName, HeaderValue)>>>()
        .map(HeaderMap::from_iter)
        .ok_or(String::from("failed to parse headers"))
}

pub async fn parse_json_response<T>(response: reqwest::Response) -> Result<T, String>
where
    T: DeserializeOwned,
{
    response.json::<T>().await.map_err(|err| {
        enqueue_alert(err.to_string(), AlertType::Error, 5000);
        logging::error!("{}", err.to_string());
        err.to_string()
    })
}

pub async fn request<'a, T>(
    url: String,
    method: reqwest::Method,
    body: Option<T>,
    headers: HeaderMap,
) -> Result<reqwest::Response, String>
where
    T: serde::Serialize,
{
    let mut request_builder = HTTP_CLIENT.request(method.clone(), url).headers(headers);
    request_builder = match (method, body) {
        (reqwest::Method::GET | reqwest::Method::DELETE, _) => request_builder,
        (_, Some(data)) => request_builder.json(&data),
        _ => request_builder,
    };

    let response = request_builder
        .send()
        .await
        .map_err(|err| err.to_string())?;

    let status = response.status();
    if status.is_client_error() || status.is_server_error() {
        let error_msg = response
            .json::<ErrorResponse>()
            .await
            .map_or(String::from("Something went wrong"), |error| error.message);
        logging::error!("{}", error_msg);
        enqueue_alert(error_msg.clone(), AlertType::Error, 5000);

        return Err(error_msg);
    }

    Ok(response)
}

pub fn unwrap_option_or_default_with_error<T>(option: Option<T>, default: T) -> T {
    option.unwrap_or_else(|| {
        enqueue_alert(
            "Something went wrong. Please reload.".to_string(),
            AlertType::Error,
            5000,
        );
        default
    })
}

pub fn get_local_storage<T>(_key: &str) -> Option<T>
where
    T: serde::de::DeserializeOwned,
{
    cfg_if! {
        if #[cfg(target_arch = "wasm32")] {
            web_sys::window()
                .and_then(|win| win.local_storage().ok().flatten())
                .and_then(|storage| {
                    storage
                        .get_item(_key)
                        .ok()
                        .flatten()
                        .and_then(|value: String| serde_json::from_str::<T>(value.as_str()).ok())
                })
        } else {
            None
        }
    }
}

pub fn set_local_storage(_key: &str, _value: &str) -> Option<()> {
    cfg_if! {
        if #[cfg(target_arch = "wasm32")] {
            web_sys::window()
                .and_then(|win| win.local_storage().ok().flatten())
                .and_then(|storage| storage.set_item(_key, _value).ok())
        } else {
            None
        }
    }
}

pub fn get_key_type(schema: &Map<String, Value>) -> String {
    if schema.contains_key("enum") {
        String::from("ENUM")
    } else {
        match schema.get("type").unwrap_or(&Value::Null) {
            Value::String(str_) => str_.to_ascii_uppercase(),
            _ => String::from("STRING"),
        }
    }
}

pub fn string_to_value_closure(val: String) -> Value {
    Value::from_str(&val).unwrap_or_else(|_| {
        // do this for Value::String, since for some reason from_str
        // cannot convert unquoted rust strings to Value::String
        Value::from_str(format!("\"{}\"", val).as_str()).unwrap_or_default()
    })
}
