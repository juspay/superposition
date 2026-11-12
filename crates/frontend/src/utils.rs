#[cfg(feature = "ssr")]
use std::env;
use std::{str::FromStr, time::Duration};

use leptos::leptos_dom::is_browser;
use leptos::*;
use reqwest::{
    StatusCode,
    header::{HeaderMap, HeaderName, HeaderValue},
};
use serde::{Deserialize, de::DeserializeOwned};
use superposition_types::{
    api::functions::{FunctionEnvironment, KeyType},
    database::models::cac::{Function, FunctionType},
};
use url::Url;
use wasm_bindgen::JsCast;

use crate::{
    api::execute_value_compute_function,
    components::alert::AlertType,
    providers::alert_provider::enqueue_alert,
    types::{
        Envs, ErrorResponse, FunctionName, SsrSharedHttpRequestHeaders,
        ValueComputeCallback,
    },
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

#[cfg(not(feature = "ssr"))]
fn get_host() -> String {
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
        .or_else(|| match js_sys::eval("__APP_ENVS") {
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

/// Escapes HTML metacharacters so untrusted or config-controlled strings
/// render as inert literal text.
pub fn escape_html(s: &str) -> String {
    let mut escaped = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '&' => escaped.push_str("&amp;"),
            '<' => escaped.push_str("&lt;"),
            '>' => escaped.push_str("&gt;"),
            '"' => escaped.push_str("&quot;"),
            '\'' => escaped.push_str("&#x27;"),
            _ => escaped.push(ch),
        }
    }
    escaped
}

/// Renders `s` as HTML-escaped literal text inside a `<span>` via `inner_html`.
///
/// `leptos_dom` 0.6 (`ssr_in_order`, used by `SsrMode::Async` and `SsrMode::InOrder`)
/// emits `View::Text` nodes unescaped during SSR. Handing pre-escaped text to
/// `inner_html` avoids raw HTML injection in SSR while matching CSR and hydration.
pub fn safe_text(s: impl AsRef<str>) -> View {
    let escaped = escape_html(s.as_ref());
    view! { <span inner_html=escaped></span> }.into_view()
}

fn is_safe_http_url(s: &str) -> bool {
    Url::parse(s)
        .map(|u| matches!(u.scheme(), "http" | "https"))
        .unwrap_or(false)
}

pub fn url_or_string(s: &str) -> impl IntoView {
    if is_safe_http_url(s) {
        view! {
            <a class="value_link" href=s.to_string() target="_blank" rel="noopener noreferrer">
                {safe_text(s)}
            </a>
        }
        .into_view()
    } else {
        safe_text(s)
    }
}

pub fn check_url_and_return_val(s: String) -> String {
    let escaped = escape_html(&s);
    if is_safe_http_url(&s) {
        format!(
            "<a class='value_link' href='{escaped}' target='_blank' rel='noopener noreferrer'>{escaped}</a>"
        )
    } else {
        escaped
    }
}

/********* Request Utils **********/

use once_cell::sync::Lazy;
static HTTP_CLIENT: Lazy<reqwest::Client> = Lazy::new(reqwest::Client::new);

const MAX_WORKSPACE_LOCK_RETRIES: u32 = 3;
const WORKSPACE_LOCK_RETRY_BASE_DELAY: Duration = Duration::from_millis(250);

#[derive(Clone, Copy)]
enum RetryPolicy {
    None,
    WorkspaceLock,
}

impl RetryPolicy {
    fn max_retries(self) -> u32 {
        match self {
            Self::WorkspaceLock if is_browser() => MAX_WORKSPACE_LOCK_RETRIES,
            Self::None | Self::WorkspaceLock => 0,
        }
    }

    fn should_retry(
        self,
        status: StatusCode,
        error: &WorkspaceLockedErrorResponse,
        retry: u32,
        max_retries: u32,
    ) -> bool {
        matches!(self, Self::WorkspaceLock)
            && status == StatusCode::CONFLICT
            && error.lock.is_some()
            && retry < max_retries
    }

    fn delay(self, retry: u32) -> Duration {
        match self {
            Self::WorkspaceLock => WORKSPACE_LOCK_RETRY_BASE_DELAY
                .saturating_mul(2_u32.saturating_pow(retry)),
            Self::None => Duration::ZERO,
        }
    }
}

#[derive(Deserialize)]
struct WorkspaceLockedErrorResponse {
    message: String,
    #[serde(default)]
    lock: Option<serde_json::Value>,
}

async fn sleep(duration: Duration) {
    let (sender, receiver) = futures::channel::oneshot::channel();
    set_timeout(
        move || {
            _ = sender.send(());
        },
        duration,
    );
    _ = receiver.await;
}

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

async fn request_with_policy<T>(
    url: String,
    method: reqwest::Method,
    body: Option<T>,
    headers: HeaderMap,
    skip_error: &[StatusCode],
    retry_policy: RetryPolicy,
) -> Result<reqwest::Response, String>
where
    T: serde::Serialize,
{
    let ssr_headers = use_context::<Option<SsrSharedHttpRequestHeaders>>().flatten();
    let cookie = ssr_headers.and_then(|h| h.cookie.clone());

    let max_retries = retry_policy.max_retries();
    let mut retry = 0;

    loop {
        let mut request_builder = HTTP_CLIENT
            .request(method.clone(), &url)
            .headers(headers.clone());
        request_builder = match (&method, body.as_ref()) {
            (&reqwest::Method::GET, _) => request_builder,
            (_, Some(data)) => request_builder.json(data),
            _ => request_builder,
        };

        if let Some(cookie_value) = cookie.as_ref() {
            request_builder =
                request_builder.header(reqwest::header::COOKIE, cookie_value);
        }

        let response = request_builder
            .send()
            .await
            .map_err(|err| err.to_string())?;

        let status = response.status();

        if skip_error.contains(&status) {
            return Ok(response);
        }

        if status.is_client_error() {
            let error = response
                .json::<WorkspaceLockedErrorResponse>()
                .await
                .unwrap_or_else(|_| WorkspaceLockedErrorResponse {
                    message: String::from("Something went wrong"),
                    lock: None,
                });

            if retry_policy.should_retry(status, &error, retry, max_retries) {
                let delay = retry_policy.delay(retry);
                retry += 1;
                logging::log!(
                    "Workspace is locked; retrying request in {:?} ({}/{})",
                    delay,
                    retry,
                    max_retries,
                );
                sleep(delay).await;
                continue;
            }

            let error_msg = error.message;
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

        return Ok(response);
    }
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
    request_with_policy(url, method, body, headers, skip_error, RetryPolicy::None).await
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
    request_with_policy(url, method, body, headers, &[], RetryPolicy::None).await
}

pub async fn request_with_workspace_lock_retry<T>(
    url: String,
    method: reqwest::Method,
    body: Option<T>,
    headers: HeaderMap,
) -> Result<reqwest::Response, String>
where
    T: serde::Serialize,
{
    request_with_policy(url, method, body, headers, &[], RetryPolicy::WorkspaceLock).await
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

pub fn set_function(selected_function: FunctionName, value: &mut Option<String>) {
    let function_name = selected_function.clone();
    leptos::logging::log!("function selected: {:?}", function_name);
    let fun_name = match function_name.as_str() {
        "None" => None,
        _ => Some(function_name),
    };
    *value = fun_name;
}

pub fn value_compute_fn_generator(
    key: String,
    value_compute_function_name: Option<String>,
    environment: Memo<FunctionEnvironment>,
    r#type: &KeyType,
    workspace: String,
    org_id: String,
) -> Option<(String, ValueComputeCallback)> {
    let fn_name = value_compute_function_name?;
    let return_key = key.clone();
    let r#type = r#type.clone();
    let callback = Callback::new(
        move |(value, suggestions): (String, WriteSignal<Vec<String>>)| {
            let key_copy = key.clone();
            let fn_copy = fn_name.clone();
            let environment = environment.get();
            let org_id = org_id.clone();
            let workspace = workspace.clone();
            let type_clone = r#type.clone();
            logging::log!("Calling {fn_copy} for {key} {value}");

            leptos::spawn_local(async move {
                match execute_value_compute_function(
                    key_copy,
                    value,
                    type_clone,
                    environment,
                    &fn_copy,
                    &workspace,
                    &org_id,
                )
                .await
                {
                    Ok(vec) => suggestions.set(vec),
                    // TODO: Handle error properly, in case of error the loader is stuck
                    Err(err) => logging::error!(
                        "An error occurred while running the value_compute function: {err}"
                    ),
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

pub fn get_fn_names_by_type(
    functions: &[Function],
    f_type: FunctionType,
) -> Vec<FunctionName> {
    std::iter::once("None".to_string())
        .chain(
            functions
                .iter()
                .filter(|f| f.function_type == f_type)
                .map(|f| f.function_name.clone()),
        )
        .collect::<Vec<_>>()
}

#[cfg(all(test, not(feature = "ssr")))]
use tokio as _;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_escape_html_escapes_metacharacters() {
        let payload = r#"<div style='width:100vw'>hello & "world"</div>"#;
        assert_eq!(
            escape_html(payload),
            "&lt;div style=&#x27;width:100vw&#x27;&gt;hello &amp; &quot;world&quot;&lt;/div&gt;"
        );
    }

    #[test]
    fn test_check_url_and_return_val_neutralizes_html_and_unsafe_schemes() {
        assert_eq!(
            check_url_and_return_val("<img src=x onerror=alert(1)>".to_string()),
            "&lt;img src=x onerror=alert(1)&gt;"
        );
        assert_eq!(
            check_url_and_return_val("javascript:alert('xss')".to_string()),
            "javascript:alert(&#x27;xss&#x27;)"
        );
        assert_eq!(
            check_url_and_return_val("https://example.com/a?x=1&y='2'".to_string()),
            "<a class='value_link' href='https://example.com/a?x=1&amp;y=&#x27;2&#x27;' target='_blank' rel='noopener noreferrer'>https://example.com/a?x=1&amp;y=&#x27;2&#x27;</a>"
        );
    }
}

#[cfg(all(test, feature = "ssr"))]
mod ssr_tests {
    use super::*;
    use crate::{
        components::{
            alert::{Alert, AlertType},
            badge::GrayPill,
            change_form::ChangeForm,
            condition_pills::{ConditionExpression, ConditionId},
            datetime::DatetimeStr,
            description::ContentDescription,
            info_modal::InfoModal,
            table::types::{default_column_formatter, default_formatter},
        },
        logic::Condition,
    };
    use chrono::Utc;
    use futures::StreamExt;
    use serde_json::{Map, Value};
    use superposition_types::database::models::{ChangeReason, Description};

    fn render_in_order<F, V>(view_fn: F) -> String
    where
        F: FnOnce() -> V + 'static,
        V: IntoView,
    {
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("tokio runtime");
        let local = tokio::task::LocalSet::new();
        rt.block_on(local.run_until(async move {
            let stream =
                leptos::ssr::render_to_stream_in_order(move || view_fn().into_view());
            stream.collect::<Vec<String>>().await.concat()
        }))
    }

    fn assert_escaped_payload(html: &str, context: &str) {
        assert!(
            !html.contains("<img src=x"),
            "{context}: raw <img element leaked into SSR output: {html}"
        );
        assert!(
            !html.contains("<script>alert("),
            "{context}: raw <script> element leaked into SSR output: {html}"
        );
        assert!(
            !html.contains("<div style='width:100vw'>"),
            "{context}: raw <div> element leaked into SSR output: {html}"
        );
        assert!(
            html.contains("&lt;"),
            "{context}: expected escaped HTML entities in SSR output: {html}"
        );
    }

    #[test]
    fn test_leptos_in_order_raw_text_node_is_unescaped_repro() {
        let payload = "<img src=x onerror=alert(1)>";
        let html = render_in_order(move || view! { <div>{payload}</div> });
        // Confirms upstream leptos 0.6.11 ssr_in_order View::Text omits escaping.
        assert!(html.contains("<img src=x onerror=alert(1)>"));
    }

    #[test]
    fn test_safe_text_escapes_html_in_ssr_in_order_and_render_to_string() {
        let payload = "<img src=x onerror=alert(1)><div style='width:100vw'>banner";
        let in_order_html = render_in_order(move || safe_text(payload));
        assert!(!in_order_html.contains("<img"));
        assert!(!in_order_html.contains("<div"));
        assert!(in_order_html.contains(
            "&lt;img src=x onerror=alert(1)&gt;&lt;div style=&#x27;width:100vw&#x27;&gt;banner"
        ));

        let string_html =
            leptos::ssr::render_to_string(move || safe_text(payload)).to_string();
        assert!(!string_html.contains("<img"));
        assert!(!string_html.contains("<div"));
        assert!(string_html.contains(
            "&lt;img src=x onerror=alert(1)&gt;&lt;div style=&#x27;width:100vw&#x27;&gt;banner"
        ));
    }

    #[test]
    fn test_url_or_string_escapes_html_and_rejects_javascript_scheme() {
        let html_payload = "<img src=x onerror=alert(1)>";
        let rendered_html_payload = render_in_order(move || url_or_string(html_payload));
        assert!(!rendered_html_payload.contains("<img"));
        assert!(rendered_html_payload.contains("&lt;img src=x onerror=alert(1)&gt;"));

        let js_payload = "javascript:alert(1)";
        let rendered_js = render_in_order(move || url_or_string(js_payload));
        assert!(!rendered_js.contains("<a "));
        assert!(rendered_js.contains("javascript:alert(1)"));

        let valid_url = "https://example.com/path?a=1&b=<b>";
        let rendered_url = render_in_order(move || url_or_string(valid_url));
        assert!(
            rendered_url
                .contains("href=\"https://example.com/path?a=1&amp;b=&lt;b&gt;\"")
        );
        assert!(rendered_url.contains("rel=\"noopener noreferrer\""));
        assert!(!rendered_url.contains("<b>"));
        assert!(rendered_url.contains("&lt;b&gt;"));
    }

    #[test]
    fn test_table_formatters_escape_html_in_ssr_in_order() {
        let row = Map::new();
        let cell_html = render_in_order(move || {
            default_formatter(
                "<img src=x onerror=alert(1)><script>alert(1)</script>",
                &row,
            )
        });
        assert_escaped_payload(&cell_html, "default_formatter");

        let col_html = render_in_order(move || {
            default_column_formatter("<div style='width:100vw'>col")
        });
        assert_escaped_payload(&col_html, "default_column_formatter");
    }

    #[test]
    fn test_shared_components_escape_user_content_in_ssr_in_order() {
        let payload = "<img src=x onerror=alert(1)><div style='width:100vw'>banner";
        let textarea_breakout = "</textarea><script>alert(1)</script>";

        let description = Description::try_from(payload.to_string()).unwrap();
        let change_reason = ChangeReason::try_from(payload.to_string()).unwrap();
        let desc_html = render_in_order(move || {
            view! {
                <ContentDescription
                    description=description
                    created_by=payload.to_string()
                    created_at=Utc::now()
                    last_modified_by=payload.to_string()
                    last_modified_at=Utc::now()
                    change_reason=change_reason
                />
            }
        });
        assert_escaped_payload(&desc_html, "ContentDescription");

        let pill_html = render_in_order(move || {
            view! { <GrayPill data=payload.to_string() deletable=false on_delete=move |_: String| () /> }
        });
        assert_escaped_payload(&pill_html, "GrayPill");

        let alert_html = render_in_order(move || {
            view! { <Alert alert=Alert::new(1, payload.to_string(), AlertType::Info, 1000) /> }
        });
        assert_escaped_payload(&alert_html, "Alert");

        let change_form_html = render_in_order(move || {
            view! {
                <ChangeForm
                    title=payload.to_string()
                    placeholder="reason".to_string()
                    value=textarea_breakout.to_string()
                    on_change=Callback::new(|_| ())
                />
            }
        });
        assert_escaped_payload(&change_form_html, "ChangeForm");
        assert!(
            !change_form_html.contains("</textarea><script>"),
            "ChangeForm allowed textarea breakout: {change_form_html}"
        );

        let condition_html = render_in_order(move || {
            let (cond_id_rs, _) = create_signal(ConditionId::default());
            provide_context(cond_id_rs);
            view! {
                <ConditionExpression
                    id="cond-0"
                    list_id="cond"
                    resolve_summary=false
                    condition=Condition {
                        variable: payload.to_string(),
                        value: Value::String(payload.to_string()),
                    }
                />
            }
        });
        assert_escaped_payload(&condition_html, "ConditionExpression");

        let info_modal_html = render_in_order(move || {
            view! {
                <InfoModal
                    visible=true
                    description=payload.to_string()
                    change_reason=textarea_breakout.to_string()
                    on_close=Callback::new(|_| ())
                />
            }
        });
        assert_escaped_payload(&info_modal_html, "InfoModal");

        let dt_html = render_in_order(move || {
            view! { <DatetimeStr datetime="<img src=x onerror=alert(1)>".to_string() /> }
        });
        assert_escaped_payload(&dt_html, "DatetimeStr");
    }

    #[test]
    fn test_textarea_inner_html_after_event_handler_preserves_escaped_value() {
        let breakout = "</textarea><script>alert(1)</script>";
        let html = render_in_order(move || {
            view! { <textarea id="filter" on:change=move |_| {} inner_html=escape_html(breakout)></textarea> }
        });
        assert!(
            !html.contains("</textarea><script>"),
            "textarea breakout leaked: {html}"
        );
        assert!(
            html.contains("&lt;/textarea&gt;&lt;script&gt;alert(1)&lt;/script&gt;"),
            "expected escaped textarea body (inner_html must remain last attr in leptos 0.6 SSR): {html}"
        );
    }
}
