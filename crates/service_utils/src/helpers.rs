use std::{
    collections::HashMap,
    env::VarError,
    fmt::{self, Display},
    str::FromStr,
};

use actix_web::{error::ErrorInternalServerError, web::Data, Error};
use anyhow::anyhow;
use chrono::Utc;
use log::info;
use once_cell::sync::Lazy;
use regex::Regex;
use reqwest::{
    header::{HeaderMap, HeaderName, HeaderValue},
    StatusCode,
};
use serde::Serialize;
#[cfg(feature = "jsonlogic")]
use serde_json::Map;
use serde_json::Value;
#[cfg(feature = "jsonlogic")]
use superposition_types::Condition;
use superposition_types::{
    api::webhook::{HeadersEnum, WebhookEventInfo, WebhookResponse},
    database::models::others::{HttpMethod, Variable, Webhook, WebhookEvent},
    result::{self},
    PaginatedResponse,
};

use crate::service::types::{AppState, WorkspaceContext};

static VAR_REGEX: Lazy<regex::Regex> = Lazy::new(|| {
    regex::Regex::new(r"\{\{VARS\.([A-Z0-9_]+)\}\}")
        .expect("Invalid variable regex pattern")
});

const CONFIG_TAG_REGEX: &str = "^[a-zA-Z0-9_-]{1,64}$";

//WARN Do NOT use this fxn inside api requests, instead add the required
//env to AppState and get value from there. As this panics, it should
//only be used for envs needed during app start.
pub fn get_from_env_unsafe<F>(name: &str) -> Result<F, VarError>
where
    F: FromStr,
    <F as FromStr>::Err: std::fmt::Debug,
{
    std::env::var(name)
        .map(|val| val.parse().unwrap())
        .map_err(|e| {
            log::info!("{name} env not found with error: {e}");
            e
        })
}

pub fn get_from_env_or_default<F>(name: &str, default: F) -> F
where
    F: FromStr + Display,
    <F as FromStr>::Err: std::fmt::Debug,
{
    match std::env::var(name) {
        Ok(env) => env.parse().unwrap(),
        Err(err) => {
            info!(
                "{name} ENV failed to load due to {err}, using default value {default}"
            );
            default
        }
    }
}

pub trait ToActixErr<T> {
    fn map_err_to_internal_server<B>(
        self,
        log_prefix: &str,
        err_body: B,
    ) -> Result<T, Error>
    where
        B: fmt::Debug + fmt::Display + 'static;
}

impl<T, E> ToActixErr<T> for Result<T, E>
where
    E: fmt::Debug,
{
    fn map_err_to_internal_server<B>(
        self,
        log_prefix: &str,
        err_body: B,
    ) -> Result<T, Error>
    where
        B: fmt::Debug + fmt::Display + 'static,
    {
        self.map_err(|e| {
            log::info!("{log_prefix}, err: {e:?}");
            ErrorInternalServerError(err_body)
        })
    }
}

pub fn get_pod_info() -> (String, String) {
    let hostname: String = get_from_env_unsafe("HOSTNAME").expect("HOSTNAME is not set");
    let tokens = hostname
        .split('-')
        .map(str::to_string)
        .collect::<Vec<String>>();
    let mut tokens = tokens.iter().rev();
    let (pod_id, _replica_set, deployment_id) = (
        tokens.next().unwrap().to_owned(),
        tokens.next().unwrap().to_owned(),
        tokens.next().unwrap().to_owned(),
    );
    (pod_id, deployment_id)
}

#[cfg(feature = "jsonlogic")]
pub fn extract_dimensions(context: &Condition) -> result::Result<Map<String, Value>> {
    // Assuming max 2-level nesting in context json logic

    let conditions: Vec<Value> = match (*context).get("and") {
        Some(conditions_json) => conditions_json
            .as_array()
            .ok_or(result::AppError::BadArgument("Error extracting dimensions, failed parsing conditions as an array. Ensure the context provided obeys the rules of JSON logic".into()))?
            .clone(),
        None => vec![Value::Object(context.to_owned().into())],
    };

    let mut dimension_tuples = Vec::new();
    for condition in &conditions {
        let condition_obj =
            condition
                .as_object()
                .ok_or(result::AppError::BadArgument(
                    "Failed to parse condition as an object. Ensure the context provided obeys the rules of JSON logic".to_string()
                ))?;
        let operators = condition_obj.keys();

        for operator in operators {
            let operands = condition_obj[operator].as_array().ok_or(result::AppError::BadArgument(
                    "Failed to parse operands as an arrays. Ensure the context provided obeys the rules of JSON logic"
                            .into()
            ))?;

            let (variable_name, variable_value) = get_variable_name_and_value(operands)?;

            dimension_tuples.push((String::from(variable_name), variable_value.clone()));
        }
    }

    Ok(Map::from_iter(dimension_tuples))
}

#[cfg(feature = "jsonlogic")]
pub fn get_variable_name_and_value(operands: &[Value]) -> result::Result<(&str, &Value)> {
    let (obj_pos, variable_obj) = operands
        .iter()
        .enumerate()
        .find(|(_, operand)| {
            operand.is_object() && operand.as_object().unwrap().get("var").is_some()
        })
        .ok_or(result::AppError::BadArgument(
            "Failed to get variable name from operands list. Ensure the context provided obeys the rules of JSON logic"
                .into()
        ))?;

    let variable_name = variable_obj
        .as_object().and_then(|obj| obj.get("var")).and_then(|value| value.as_str())
        .ok_or(result::AppError::BadArgument(
            "Failed to get variable name as string. Ensure the context provided obeys the rules of JSON logic"
                .into()
        ))?;

    let value_pos = (obj_pos + 1) % 2;
    let variable_value =
        operands
            .get(value_pos)
            .ok_or(result::AppError::BadArgument(
                "Failed to get variable value from operands list. Ensure the context provided obeys the rules of JSON logic"
                    .into()
            ))?;

    Ok((variable_name, variable_value))
}

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

pub async fn request<T, R>(
    url: String,
    method: reqwest::Method,
    body: Option<T>,
    headers: HeaderMap,
) -> Result<R, reqwest::Error>
where
    T: serde::Serialize,
    R: serde::de::DeserializeOwned,
{
    let mut request_builder = HTTP_CLIENT.request(method.clone(), url).headers(headers);
    request_builder = match (method, body) {
        (reqwest::Method::GET | reqwest::Method::DELETE, _) => request_builder,
        (_, Some(data)) => request_builder.json(&data),
        _ => request_builder,
    };

    let response = request_builder.send().await?;

    response.json::<R>().await
}
pub fn generate_snowflake_id(state: &Data<AppState>) -> result::Result<i64> {
    let mut snowflake_generator = state.snowflake_generator.lock().map_err(|e| {
        log::error!("snowflake_id generation failed {}", e);
        result::AppError::UnexpectedError(anyhow!("snowflake_id generation failed {}", e))
    })?;
    let id = snowflake_generator.real_time_generate();
    // explicitly dropping snowflake_generator so that lock is released and it can be acquired in bulk-operations handler
    drop(snowflake_generator);
    Ok(id)
}

pub fn parse_config_tags(
    config_tags: Option<String>,
) -> result::Result<Option<Vec<String>>> {
    let regex = Regex::new(CONFIG_TAG_REGEX).map_err(|err| {
        log::error!("regex match failed for tags {}", err);
        result::AppError::UnexpectedError(anyhow!("Something went wrong"))
    })?;
    match config_tags {
        None => Ok(None),
        Some(val) => {
            let tags = val
                .split(',')
                .map(|s| {
                    if !regex.is_match(s) {
                        Err(result::AppError::BadArgument(
                            "Invalid config_tags value".to_string(),
                        ))
                    } else {
                        Ok(s.to_owned())
                    }
                })
                .collect::<result::Result<Vec<String>>>()?;
            Ok(Some(tags))
        }
    }
}

pub async fn execute_webhook_call<T>(
    webhook: &Webhook,
    payload: &T,
    config_version_opt: &Option<String>,
    workspace_request: &WorkspaceContext,
    event: WebhookEvent,
    state: &Data<AppState>,
) -> bool
where
    T: Serialize,
{
    if !webhook.enabled {
        log::info!("Webhook is disabled, skipping call");
        return true;
    }

    let substitute_variables =
        |template: &Value, variables: &HashMap<String, String>| -> String {
            match template {
                Value::String(s) => VAR_REGEX
                    .replace_all(s, |caps: &regex::Captures| {
                        caps.get(1)
                            .map(|m| m.as_str())
                            .and_then(|key| variables.get(key).map(String::as_str))
                            .unwrap_or(s.as_str())
                    })
                    .into_owned(),
                other => other.to_string(),
            }
        };
    let has_variables = webhook.custom_headers.values().any(|value_json| {
        value_json
            .as_str()
            .map(|s| VAR_REGEX.is_match(s))
            .unwrap_or_default()
    });

    let variables_map = if has_variables {
        let variables_url = format!("{}/variables", state.cac_host);

        let headers = match construct_request_headers(&[
            ("x-tenant", &workspace_request.workspace_id),
            ("x-org-id", &workspace_request.organisation_id),
            (
                "Authorization",
                &format!("Internal {}", state.superposition_token),
            ),
        ]) {
            Ok(h) => h,
            Err(e) => {
                log::error!("Failed to construct request headers: {}", e);
                return false;
            }
        };

        match request::<(), PaginatedResponse<Variable>>(
            variables_url,
            reqwest::Method::GET,
            None,
            headers,
        )
        .await
        {
            Ok(res) => res
                .data
                .into_iter()
                .map(|v| (v.name.to_string(), v.value))
                .collect::<HashMap<_, _>>(),
            Err(e) => {
                log::error!("Failed to fetch variables: {}", e);
                HashMap::new()
            }
        }
    } else {
        HashMap::new()
    };

    let mut headers = HeaderMap::new();

    let insert_header = |headers: &mut HeaderMap, name: &str, value: &str| {
        if let (Ok(k), Ok(v)) = (HeaderName::from_str(name), HeaderValue::from_str(value))
        {
            headers.insert(k, v);
        }
    };

    insert_header(
        &mut headers,
        &HeadersEnum::ConfigVersion.to_string(),
        &config_version_opt.clone().unwrap_or_default(),
    );
    insert_header(
        &mut headers,
        &HeadersEnum::WorkspaceId.to_string(),
        &workspace_request.workspace_id,
    );

    webhook.custom_headers.iter().for_each(|(key, value)| {
        let value_str = value
            .as_str()
            .map(String::from)
            .unwrap_or_else(|| value.to_string());

        let rendered = if let Some(decrypted) = state.encrypted_keys.get(&value_str) {
            decrypted.to_string()
        } else {
            substitute_variables(value, &variables_map)
        };
        insert_header(&mut headers, key, &rendered);
    });

    let request_builder = match webhook.method {
        HttpMethod::Post => state.http_client.post(&*webhook.url),
        HttpMethod::Get => state.http_client.get(&*webhook.url),
        HttpMethod::Put => state.http_client.put(&*webhook.url),
        HttpMethod::Delete => state.http_client.delete(&*webhook.url),
        HttpMethod::Patch => state.http_client.patch(&*webhook.url),
        HttpMethod::Head => state.http_client.head(&*webhook.url),
    };

    let response = request_builder
        .headers(headers)
        .json(&WebhookResponse {
            event_info: WebhookEventInfo {
                webhook_event: event,
                time: Utc::now().to_string(),
                workspace_id: workspace_request.workspace_id.to_string(),
                organisation_id: workspace_request.organisation_id.to_string(),
                config_version: config_version_opt.clone(),
            },
            payload,
        })
        .send()
        .await;

    match response {
        Ok(res) if res.status() == StatusCode::OK => {
            log::info!("webhook call succeeded: {:?}", res.status());
            true
        }
        Ok(res) => {
            log::error!("Webhook failed: {:?} - {:?}", res.status(), res.headers());
            false
        }
        Err(err) => {
            log::error!("Webhook call failed: {:?}", err);
            false
        }
    }
}
