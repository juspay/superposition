use std::{
    collections::HashMap,
    env::VarError,
    fmt::{self, Display},
    str::FromStr,
};

use actix_web::{Error, HttpRequest, error::ErrorInternalServerError, web::Data};
use anyhow::anyhow;
use chrono::{DateTime, Timelike, Utc};
use diesel::{
    ExpressionMethods, OptionalExtension, PgArrayExpressionMethods, QueryDsl,
    RunQueryDsl, SelectableHelper,
};
use log::warn;
use once_cell::sync::Lazy;
use regex::Regex;
use reqwest::header::{HeaderMap, HeaderName, HeaderValue};
use serde::Serialize;
use superposition_types::{
    DBConnection, DimensionInfo, Resource,
    api::webhook::{Action, WebhookEventInfo, WebhookResponse},
    database::{
        models::{
            Workspace,
            others::{Webhook, WebhookEvent},
        },
        schema::{
            dimensions::{self, dimension},
            webhooks::{self, dsl::webhooks as webhook_dsl},
        },
        superposition_schema::superposition::workspaces,
    },
    result::{self},
};

use crate::{
    kronos_dispatch::submit_webhook_job,
    service::types::{AppState, SchemaName, WorkspaceContext},
};

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
            warn!(
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

pub fn get_workspace(
    workspace_schema_name: &SchemaName,
    db_conn: &mut DBConnection,
) -> Result<Workspace, diesel::result::Error> {
    workspaces::dsl::workspaces
        .filter(workspaces::workspace_schema_name.eq(workspace_schema_name.to_string()))
        .get_result::<Workspace>(db_conn)
}

pub struct WebhookData<T> {
    pub payload: T,
    pub resource: Resource,
    pub action: Action,
    pub event: WebhookEvent,
    pub config_version_opt: Option<String>,
}

pub async fn execute_webhook_call<T>(
    data: WebhookData<T>,
    workspace_context: &WorkspaceContext,
    state: &Data<AppState>,
    conn: &mut DBConnection,
) -> bool
where
    T: Serialize,
{
    let WebhookData {
        event,
        resource,
        action,
        config_version_opt,
        payload,
    } = data;

    let webhook = match webhook_dsl
        .filter(webhooks::events.contains(vec![event]))
        .schema_name(&workspace_context.schema_name)
        .first::<Webhook>(conn)
        .optional()
    {
        Ok(Some(webhook)) => webhook,
        Ok(None) => {
            log::info!("No webhook found for this event, skipping event: {}", event);
            return true;
        }
        Err(e) => {
            log::error!("Failed to fetch webhook for event: {}", e);
            return false;
        }
    };

    if !webhook.enabled {
        log::info!("Webhook is disabled, skipping call");
        return true;
    }

    let webhook_response = WebhookResponse {
        event_info: WebhookEventInfo {
            webhook_event: event,
            resource,
            action,
            time: Utc::now().to_string(),
            workspace_id: workspace_context.workspace_id.to_string(),
            organisation_id: workspace_context.organisation_id.to_string(),
            config_version: config_version_opt,
        },
        payload,
    };

    // Idempotency key: SP schema + webhook_name + event type + millisecond timestamp.
    // The schema prefix namespaces keys across SP workspaces, which all share one Kronos
    // workspace in service mode. Ensures each trigger attempt gets a unique job while
    // allowing dedup on retries.
    let idempotency_key = format!(
        "{}_{}_{}_{}",
        *workspace_context.schema_name,
        webhook.name,
        event,
        Utc::now().timestamp_millis()
    );

    // Service mode: all SP schemas share one Kronos workspace. Library mode: target the SP
    // schema directly.
    let target_workspace = state
        .kronos_workspace
        .clone()
        .unwrap_or_else(|| workspace_context.schema_name.to_string());

    match submit_webhook_job(
        state.kronos_client.as_ref(),
        &target_workspace,
        &workspace_context.organisation_id,
        &workspace_context.workspace_id,
        &webhook.name,
        &webhook_response,
        &idempotency_key,
        webhook.max_retries,
    )
    .await
    {
        Ok(execution_id) => {
            log::info!(
                "Webhook job submitted: webhook='{}' execution_id='{}'",
                webhook.name,
                execution_id
            );
            true
        }
        Err(e) => {
            log::error!("Failed to submit webhook job for '{}': {}", webhook.name, e);
            false
        }
    }
}

pub fn fetch_dimensions_info_map(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> result::DieselResult<HashMap<String, DimensionInfo>> {
    let dimensions_map = dimensions::table
        .select((dimension, DimensionInfo::as_select()))
        .schema_name(schema_name)
        .load::<(String, DimensionInfo)>(conn)?
        .into_iter()
        .collect();

    Ok(dimensions_map)
}

pub fn is_not_modified(max_created_at: Option<DateTime<Utc>>, req: &HttpRequest) -> bool {
    let is_smithy = matches!(req.method(), &actix_web::http::Method::POST);
    let nanosecond_erasure = |t: DateTime<Utc>| t.with_nanosecond(0);

    let last_modified = req
        .headers()
        .get("if-modified-since")
        .and_then(|header_val| {
            let header_str = header_val.to_str().ok()?;
            if is_smithy {
                DateTime::parse_from_rfc3339(header_str)
            } else {
                DateTime::parse_from_rfc2822(header_str)
            }
            .map(|datetime| datetime.with_timezone(&Utc))
            .ok()
        })
        .and_then(nanosecond_erasure);

    log::info!("last modified {last_modified:?}");
    let parsed_max: Option<DateTime<Utc>> = max_created_at.and_then(nanosecond_erasure);
    max_created_at.is_some() && parsed_max <= last_modified
}
