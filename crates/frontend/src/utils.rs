use std::{env, str::FromStr};

use cfg_if::cfg_if;
use leptos::*;
use reqwest::header::{HeaderMap, HeaderName, HeaderValue};
use serde::de::DeserializeOwned;
use serde_json::{Map, Value};
use superposition_types::cac::{models::DefaultConfig, types::DimensionWithMandatory};
use url::Url;
use wasm_bindgen::JsCast;

use crate::{
    components::alert::AlertType,
    providers::alert_provider::enqueue_alert,
    types::{Envs, ErrorResponse},
};

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
    // Default config is not actually dead code, it's used
    // but for some reason the compiler thinks it's dead code
    // this gets rid of the warning
    #[allow(dead_code)]
    DefaultConfig(DefaultConfig),
    Dimension(DimensionWithMandatory),
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

pub fn parse_value(val: &Value, config_type: ConfigValueType) -> Result<Value, String> {
    match config_type {
        ConfigValueType::Boolean => {
            match val {
                Value::Bool(_) => Ok(val.clone()),

                Value::String(s) => {
                    // Attempting to parse the string as a boolean
                    match s.to_lowercase().as_str() {
                        "true" => Ok(Value::Bool(true)),
                        "false" => Ok(Value::Bool(false)),
                        _ => Err(format!("Invalid boolean string: {:?}", s)), // Error if not a valid boolean string
                    }
                }

                _ => Err(format!("Invalid boolean value: {:?}", val)),
            }
        }

        ConfigValueType::Number | ConfigValueType::Integer => {
            match val {
                Value::Number(num) => Ok(Value::Number(num.clone())),

                Value::String(s) => {
                    // Attempting to parse as integer first, then as float
                    if let Ok(int_val) = s.parse::<i64>() {
                        Ok(Value::Number(int_val.into()))
                    } else if let Ok(float_val) = s.parse::<f64>() {
                        Ok(Value::Number(
                            serde_json::Number::from_f64(float_val).unwrap(),
                        ))
                    } else {
                        Err(format!("Invalid number format: {:?}", s))
                    }
                }

                Value::Array(arr) => {
                    // Ensuring all items in the array are numbers
                    if arr.iter().all(|item| item.is_number()) {
                        Ok(val.clone())
                    } else {
                        Err("Array contains non-number value".to_string())
                    }
                }

                _ => Err(format!(
                    "{:?} is neither a valid number nor an array of numbers.",
                    val
                )),
            }
        }

        ConfigValueType::String => match val {
            Value::String(_) => Ok(val.clone()),
            Value::Number(i) => Ok(Value::String(i.to_string())),
            Value::Bool(b) => Ok(Value::String(b.to_string())),
            Value::Array(arr) => {
                if arr.iter().all(|item| item.is_string()) {
                    Ok(val.clone())
                } else {
                    Err("Array contains non-string value".to_string())
                }
            }
            _ => Err(format!(
                "{:?} is neither a valid string nor an array of strings.",
                val
            )),
        },

        ConfigValueType::Null if val.is_null() => Ok(Value::Null),

        _ => Ok(val.clone()),
    }
}

pub fn get_config_value(
    name: &str,
    val: &Value,
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
        None => Ok(val.clone()),
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
    if status.is_client_error() {
        let resp_bytes = response
            .bytes()
            .await
            .map_err(|_| String::from("Something went wrong"))?;
        let error_msg = serde_json::from_slice::<ErrorResponse>(&resp_bytes).map_or(
            String::from(
                std::str::from_utf8(&resp_bytes.to_vec())
                    .unwrap_or("Something went wrong"),
            ),
            |error| error.message,
        );
        logging::error!("{}", error_msg);
        enqueue_alert(error_msg.clone(), AlertType::Error, 5000);
        return Err(error_msg);
    }
    if status.is_server_error() {
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

pub fn update_page_direction(
    page: Option<i64>,
    total_pages: i64,
    is_next: bool,
) -> Option<i64> {
    match page {
        Some(p) if is_next && p < total_pages => Some(p + 1), // Increment if is_next is true
        Some(p) if !is_next && p > 1 => Some(p - 1), // Decrement if is_next is false
        Some(p) => Some(p),
        None => None,
    }
}
