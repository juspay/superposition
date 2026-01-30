use std::{
    collections::HashMap,
    env::VarError,
    fmt::{self, Display},
    str::FromStr,
};

use actix_web::{Error, error::ErrorInternalServerError, web::Data};
use anyhow::anyhow;
use chrono::Utc;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
use jsonschema::{ValidationError, error::ValidationErrorKind};
use log::info;
use once_cell::sync::Lazy;
use regex::Regex;
use reqwest::{
    StatusCode,
    header::{HeaderMap, HeaderName, HeaderValue},
};
use secrecy::ExposeSecret;
use serde::Serialize;
use superposition_macros::unexpected_error;
use superposition_types::{
    DBConnection, DimensionInfo,
    api::webhook::{HeadersEnum, WebhookEventInfo, WebhookResponse},
    database::{
        models::{
            Workspace,
            others::{CustomHeaders, HttpMethod, Webhook, WebhookEvent},
        },
        schema::{
            dimensions::{self, dimension},
            secrets::dsl as secrets_dsl,
            variables::dsl as variables_dsl,
        },
        superposition_schema::superposition::workspaces,
    },
    result::{self},
};

use crate::encryption::{EncryptionError, decrypt_secret, decrypt_workspace_key};
use crate::service::types::{AppState, SchemaName, WorkspaceContext};

// using named group to capture which type (secrets/variables) the regex was
// because variables and secrets need to be handled differently inside webhook execution
static CONFIG_REFERENCE_REGEX: Lazy<regex::Regex> = Lazy::new(|| {
    regex::Regex::new(r"\{\{(?P<type>VARS|SECRETS)\.(?P<name>[A-Z0-9_]+)\}\}")
        .expect("Invalid config pattern")
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

pub fn validation_err_to_str(errors: Vec<ValidationError>) -> Vec<String> {
    errors.into_iter().map(|error| {
        match error.kind {
            ValidationErrorKind::AdditionalItems { limit } => {
                format!("input array contain more items than expected, limit is {limit}")
            }
            ValidationErrorKind::AdditionalProperties { unexpected } => {
                format!("unexpected properties `{}`", unexpected.join(", "))
            }
            ValidationErrorKind::AnyOf => {
                "not valid under any of the schemas listed in the 'anyOf' keyword".to_string()
            }
            ValidationErrorKind::BacktrackLimitExceeded { error: _ } => {
                "backtrack limit exceeded while matching regex".to_string()
            }
            ValidationErrorKind::Constant { expected_value } => {
                format!("value doesn't match expected constant `{expected_value}`")
            }
            ValidationErrorKind::Contains => {
                "array doesn't contain items conforming to the specified schema".to_string()
            }
            ValidationErrorKind::ContentEncoding { content_encoding } => {
                format!("value doesn't respect the defined contentEncoding `{content_encoding}`")
            }
            ValidationErrorKind::ContentMediaType { content_media_type } => {
                format!("value doesn't respect the defined contentMediaType `{content_media_type}`")
            }
            ValidationErrorKind::Enum { options } => {
                format!("value doesn't match any of specified options {}", options)
            }
            ValidationErrorKind::ExclusiveMaximum { limit } => {
                format!("value is too large, limit is {limit}")
            }
            ValidationErrorKind::ExclusiveMinimum { limit } => {
                format!("value is too small, limit is {limit}")
            }
            ValidationErrorKind::FalseSchema => {
                "everything is invalid for `false` schema".to_string()
            }
            ValidationErrorKind::FileNotFound { error: _ } => {
                "referenced file not found".to_string()
            }
            ValidationErrorKind::Format { format } => {
                format!("value doesn't match the specified format `{}`", format)
            }
            ValidationErrorKind::FromUtf8 { error: _ } => {
                "invalid UTF-8 data".to_string()
            }
            ValidationErrorKind::InvalidReference { reference } => {
                format!("`{}` is not a valid reference", reference)
            }
            ValidationErrorKind::InvalidURL { error } => {
                format!("invalid URL: {}", error)
            }
            ValidationErrorKind::JSONParse { error } => {
                format!("error parsing JSON: {}", error)
            }
            ValidationErrorKind::MaxItems { limit } => {
                format!("too many items in array, limit is {}", limit)
            }
            ValidationErrorKind::Maximum { limit } => {
                format!("value is too large, maximum is {}", limit)
            }
            ValidationErrorKind::MaxLength { limit } => {
                format!("string is too long, maximum length is {}", limit)
            }
            ValidationErrorKind::MaxProperties { limit } => {
                format!("too many properties in object, limit is {}", limit)
            }
            ValidationErrorKind::MinItems { limit } => {
                format!("not enough items in array, minimum is {}", limit)
            }
            ValidationErrorKind::Minimum { limit } => {
                format!("value is too small, minimum is {}", limit)
            }
            ValidationErrorKind::MinLength { limit } => {
                format!("string is too short, minimum length is {}", limit)
            }
            ValidationErrorKind::MinProperties { limit } => {
                format!("not enough properties in object, minimum is {}", limit)
            }
            ValidationErrorKind::MultipleOf { multiple_of } => {
                format!("value is not a multiple of {}", multiple_of)
            }
            ValidationErrorKind::Not { schema } => {
                format!("negated schema `{}` failed validation", schema)
            }
            ValidationErrorKind::OneOfMultipleValid => {
                "value is valid under more than one schema listed in the 'oneOf' keyword".to_string()
            }
            ValidationErrorKind::OneOfNotValid => {
                "value is not valid under any of the schemas listed in the 'oneOf' keyword".to_string()
            }
            ValidationErrorKind::Pattern { pattern } => {
                format!("value doesn't match the pattern `{}`", pattern)
            }
            ValidationErrorKind::PropertyNames { error } => {
                format!("object property names are invalid: {}", error)
            }
            ValidationErrorKind::Required { property } => {
                format!("required property `{}` is missing", property)
            }
            ValidationErrorKind::Resolver { url, error } => {
                format!("error resolving reference `{}`: {}", url, error)
            }
            ValidationErrorKind::Schema => {
                "resolved schema failed to compile".to_string()
            }
            ValidationErrorKind::Type { kind } => {
                format!("value doesn't match the required type(s) `{:?}`", kind)
            }
            ValidationErrorKind::UnevaluatedProperties { unexpected } => {
                format!("unevaluated properties `{}`", unexpected.join(", "))
            }
            ValidationErrorKind::UniqueItems => {
                "array contains non-unique elements".to_string()
            }
            ValidationErrorKind::UnknownReferenceScheme { scheme } => {
                format!("unknown reference scheme `{}`", scheme)
            }
            ValidationErrorKind::Utf8 { error } => {
                format!("invalid UTF-8 string: {}", error)
            }
        }
    }).collect()
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
) -> result::Result<Workspace> {
    let workspace = workspaces::dsl::workspaces
        .filter(workspaces::workspace_schema_name.eq(workspace_schema_name.to_string()))
        .get_result::<Workspace>(db_conn)?;
    Ok(workspace)
}

fn has_pattern_in_headers(headers: &CustomHeaders) -> (bool, bool) {
    let mut has_vars = false;
    let mut has_secrets = false;
    for value in headers.values() {
        let ref_type = value
            .as_str()
            .and_then(|s| CONFIG_REFERENCE_REGEX.captures(s))
            .and_then(|caps| caps.name("type"))
            .map(|m| m.as_str());

        match ref_type {
            Some("VARS") => has_vars = true,
            Some("SECRETS") => has_secrets = true,
            _ => (),
        }
    }
    (has_vars, has_secrets)
}

fn substitute_templates(
    template: &str,
    variables: &HashMap<String, String>,
    secrets: &HashMap<String, String>,
) -> String {
    CONFIG_REFERENCE_REGEX
        .replace(template, |caps: &regex::Captures| {
            let ref_type = caps.name("type").map(|m| m.as_str());
            let ref_name = caps.name("name").map(|m| m.as_str());

            match (ref_type, ref_name) {
                (Some("VARS"), Some(name)) => variables.get(name).cloned(),
                (Some("SECRETS"), Some(name)) => secrets.get(name).cloned(),
                _ => None,
            }
            .unwrap_or(template.to_string())
        })
        .into_owned()
}

fn fetch_variables(
    workspace_context: &WorkspaceContext,
    conn: &mut DBConnection,
) -> result::Result<HashMap<String, String>> {
    let variables_map = variables_dsl::variables
        .select((variables_dsl::name, variables_dsl::value))
        .schema_name(&workspace_context.schema_name)
        .load(conn)?
        .into_iter()
        .collect();

    Ok(variables_map)
}

fn fetch_secrets(
    workspace_context: &WorkspaceContext,
    state: &Data<AppState>,
    conn: &mut DBConnection,
) -> result::Result<HashMap<String, String>> {
    let encryption_key = workspace_context.settings.encryption_key.as_str();

    let master_encryption_key = match state.master_encryption_key {
        Some(ref key) => key,
        None => {
            log::warn!("Master encryption key not configured, skipping secrets");
            return Ok(HashMap::new());
        }
    };

    let workspace_key = match decrypt_workspace_key(encryption_key, master_encryption_key)
    {
        Ok(key) => key,
        Err(e) => {
            log::error!("Failed to decrypt workspace key: {}", e);
            return Err(unexpected_error!("Failed to decrypt workspace key"));
        }
    };

    let db_secrets: Vec<(String, String)> = secrets_dsl::secrets
        .schema_name(&workspace_context.schema_name)
        .select((secrets_dsl::name, secrets_dsl::encrypted_value))
        .load(conn)
        .map_err(|e| {
            log::error!("Failed to load secrets: {}", e);
            unexpected_error!("Failed to load secrets")
        })?;

    let result: Result<HashMap<String, String>, EncryptionError> = db_secrets
        .into_iter()
        .map(|(name, encrypted_value)| {
            decrypt_secret(&encrypted_value, &workspace_key)
                .map(|decrypted| (name, decrypted.expose_secret().to_string()))
        })
        .collect();

    result.map_err(|e| {
        log::error!("Failed to decrypt secrets: {}", e);
        unexpected_error!("Failed to decrypt secrets")
    })
}

pub async fn execute_webhook_call<T>(
    webhook: &Webhook,
    payload: &T,
    config_version_opt: &Option<String>,
    workspace_context: &WorkspaceContext,
    event: WebhookEvent,
    state: &Data<AppState>,
    conn: &mut DBConnection,
) -> bool
where
    T: Serialize,
{
    if !webhook.enabled {
        log::info!("Webhook is disabled, skipping call");
        return true;
    }

    let (has_vars, has_secrets) = has_pattern_in_headers(&webhook.custom_headers);

    let variables = if has_vars {
        match fetch_variables(workspace_context, conn) {
            Ok(vars_map) => vars_map,
            Err(e) => {
                log::error!("Failed to fetch variables for webhook: {}", e);
                return false;
            }
        }
    } else {
        HashMap::new()
    };

    let secrets = if has_secrets {
        match fetch_secrets(workspace_context, state, conn) {
            Ok(secrets_map) => secrets_map,
            Err(e) => {
                log::error!("Failed to fetch secrets for webhook: {}", e);
                return false;
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
        &workspace_context.workspace_id,
    );

    for (key, value) in webhook.custom_headers.iter() {
        let value_str = value
            .as_str()
            .map(String::from)
            .unwrap_or_else(|| value.to_string());
        let rendered = substitute_templates(&value_str, &variables, &secrets);
        insert_header(&mut headers, key, &rendered);
    }

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
                workspace_id: workspace_context.workspace_id.to_string(),
                organisation_id: workspace_context.organisation_id.to_string(),
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

pub fn fetch_dimensions_info_map(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> result::Result<HashMap<String, DimensionInfo>> {
    let dimensions_map = dimensions::table
        .select((dimension, DimensionInfo::as_select()))
        .schema_name(schema_name)
        .load::<(String, DimensionInfo)>(conn)?
        .into_iter()
        .collect();

    Ok(dimensions_map)
}
