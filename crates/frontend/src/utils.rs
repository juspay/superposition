#[cfg(feature = "ssr")]
use std::env;
use std::str::FromStr;

use leptos::*;
use reqwest::{
    header::{HeaderMap, HeaderName, HeaderValue},
    StatusCode,
};
use serde::de::DeserializeOwned;
use serde_json::Value;
use url::Url;
use wasm_bindgen::JsCast;

use crate::{
    api::execute_autocomplete_function,
    components::alert::AlertType,
    providers::alert_provider::enqueue_alert,
    types::{AutoCompleteCallback, Envs, ErrorResponse, FunctionsName},
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

pub fn use_url_base() -> String {
    let service_prefix = use_service_prefix();
    match service_prefix.as_str() {
        "" | "/" => "".to_owned(),
        prefix => "/".to_owned() + prefix,
    }
}

pub fn use_host_server() -> String {
    #[cfg(feature = "ssr")]
    {
        let service_prefix = use_service_prefix();
        let port = env::var("PORT").unwrap_or_else(|_| "8080".to_string());
        add_prefix(&format!("http://localhost:{port}"), &service_prefix)
    }
    #[cfg(not(feature = "ssr"))]
    get_host()
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

pub fn url_or_string(s: &str) -> impl IntoView {
    match Url::parse(s) {
        Ok(_) => {
            let s = s.to_string();
            view! {
                <a class="value_link" href=s.clone() target="_blank">
                    {s}
                </a>
            }
            .into_view()
        }
        Err(_) => s.to_string().into_view(),
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

pub async fn request_with_skip_error<T>(
    url: String,
    method: reqwest::Method,
    body: Option<T>,
    headers: HeaderMap,
    skip_error: &[StatusCode],
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

    if skip_error.contains(&status) {
        return Ok(response);
    }

    if status.is_client_error() {
        let error_msg = response
            .json::<ErrorResponse>()
            .await
            .map_or(String::from("Something went wrong"), |error| error.message);
        logging::error!("{}", error_msg);
        enqueue_alert(error_msg.clone(), AlertType::Error, 5000);
        return Err(error_msg);
    }
    if status.is_server_error() {
        if status == 512 {
            enqueue_alert(
                "Webhook Call Failed, Please Check the Logs.".to_owned(),
                AlertType::Error,
                5000,
            );
        } else {
            let error_msg = response
                .json::<ErrorResponse>()
                .await
                .map_or(String::from("Something went wrong"), |error| error.message);
            logging::error!("{}", error_msg);
            enqueue_alert(error_msg.clone(), AlertType::Error, 5000);
            return Err(error_msg);
        }
    }

    Ok(response)
}

pub async fn request<T>(
    url: String,
    method: reqwest::Method,
    body: Option<T>,
    headers: HeaderMap,
) -> Result<reqwest::Response, String>
where
    T: serde::Serialize,
{
    request_with_skip_error(url, method, body, headers, &[]).await
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

pub fn unwrap_or_default_with_error<T: Default>(option: Option<T>, error: &str) -> T {
    option.unwrap_or_else(|| {
        enqueue_alert(error.to_string(), AlertType::Error, 5000);
        Default::default()
    })
}

pub fn set_function(selected_function: FunctionsName, value: &mut Option<String>) {
    let function_name = selected_function.clone();
    leptos::logging::log!("function selected: {:?}", function_name);
    let fun_name = match function_name.as_str() {
        "None" => None,
        _ => Some(function_name),
    };
    *value = fun_name;
}

pub fn autocomplete_fn_generator(
    key: String,
    autocomplete_fn_name: Option<String>,
    environment: Memo<Value>,
    tenant: String,
    org_id: String,
) -> Option<(String, AutoCompleteCallback)> {
    let fn_name = autocomplete_fn_name?;
    let return_key = key.clone();
    let callback = Callback::new(
        move |(value, suggestions): (String, WriteSignal<Vec<String>>)| {
            let key_copy = key.clone();
            let fn_copy = fn_name.clone();
            let environment = environment.get();
            let org_id = org_id.clone();
            let tenant = tenant.clone();
            logging::log!("Calling {fn_copy} for {key} {value} {}", environment);
            leptos::spawn_local(async move {
                match execute_autocomplete_function(
                &key_copy,
                &value,
                &environment,
                &fn_copy,
                &tenant,
                &org_id,
            )
            .await
            {
                Ok(vec) => suggestions.set(vec),
                Err(err) => logging::error!("An error occurred while running the autocomplete function: {err}"),
            };
            });
        },
    );
    Some((return_key, callback))
}

pub fn to_title_case(input: &str) -> String {
    let mut words = Vec::new();
    let mut current_word = String::new();

    for (i, c) in input.char_indices() {
        if c == '_' {
            if !current_word.is_empty() {
                words.push(current_word.clone());
                current_word.clear();
            }
        } else if c.is_uppercase()
            && i != 0
            && !input.chars().nth(i - 1).unwrap().is_uppercase()
        {
            if !current_word.is_empty() {
                words.push(current_word.clone());
                current_word.clear();
            }
            current_word.push(c);
        } else {
            current_word.push(c);
        }
    }

    if !current_word.is_empty() {
        words.push(current_word);
    }

    words
        .into_iter()
        .map(|w| {
            let mut c = w.chars();
            match c.next() {
                Some(f) => {
                    f.to_uppercase().collect::<String>() + &c.as_str().to_lowercase()
                }
                None => String::new(),
            }
        })
        .collect::<Vec<String>>()
        .join(" ")
}
