use superposition_types::custom_query::CommaSeparatedStringQParams;

use actix_http::header::HeaderValue;
use actix_web::{
    web::{Header, Json},
    HttpRequest, HttpResponseBuilder,
};
use cac_client::{eval_cac, eval_cac_with_reasoning};
use chrono::{DateTime, Timelike, Utc};
use diesel::{dsl::max, ExpressionMethods, QueryDsl, RunQueryDsl};
use serde_json::{Map, Value};
use service_utils::service::types::{AppHeader, SchemaName, WorkspaceContext};
use superposition_macros::{bad_argument, db_error, unexpected_error};
use superposition_types::{
    api::config::{ContextPayload, MergeStrategy, ResolveConfigQuery},
    custom_query::{DimensionQuery, QueryMap},
    database::schema::{
        config_versions::dsl as config_versions, event_log::dsl as event_log,
    },
    result as superposition, Config, DBConnection,
};
use uuid::Uuid;

use crate::helpers::{evaluate_remote_cohorts, generate_cac};

pub fn apply_prefix_filter_to_config(
    prefix: &Option<CommaSeparatedStringQParams>,
    mut config: Config,
) -> superposition::Result<Config> {
    if let Some(prefix) = prefix {
        config = config.filter_by_prefix(&prefix.iter().map(Clone::clone).collect());
    }

    Ok(config)
}

pub fn get_config_version(
    version: &Option<String>,
    workspace_request: &WorkspaceContext,
) -> superposition::Result<Option<i64>> {
    version.as_ref().map_or_else(
        || Ok(workspace_request.settings.config_version),
        |version| {
            if *version == *"latest" {
                log::trace!("latest config request");
                return Ok(None);
            }
            version.parse::<i64>().map_or_else(
                |e| {
                    log::error!(
                        "failed to decode version as integer: {version}, error: {e}"
                    );
                    Err(bad_argument!("version is not of type integer"))
                },
                |v| Ok(Some(v)),
            )
        },
    )
}

pub fn add_audit_id_to_header(
    conn: &mut DBConnection,
    resp_builder: &mut HttpResponseBuilder,
    schema_name: &SchemaName,
) {
    if let Ok(uuid) = event_log::event_log
        .select(event_log::id)
        .filter(event_log::table_name.eq("contexts"))
        .order_by(event_log::timestamp.desc())
        .schema_name(schema_name)
        .first::<Uuid>(conn)
    {
        resp_builder.insert_header((AppHeader::XAuditId.to_string(), uuid.to_string()));
    } else {
        log::error!("Failed to fetch contexts from event_log");
    }
}

pub fn add_last_modified_to_header(
    max_created_at: Option<DateTime<Utc>>,
    is_smithy: bool,
    resp_builder: &mut HttpResponseBuilder,
) {
    if let Some(date) = max_created_at {
        let value = if is_smithy {
            // Smithy needs to be in this format otherwise they can't
            // deserialize it.
            HeaderValue::from_str(date.to_rfc3339().as_str())
        } else {
            HeaderValue::from_str(date.to_rfc2822().as_str())
        };
        if let Ok(header_value) = value {
            resp_builder
                .insert_header((AppHeader::LastModified.to_string(), header_value));
        } else {
            log::error!("failed parsing datetime_utc {:?}", value);
        }
    }
}

pub fn add_config_version_to_header(
    config_version: &Option<i64>,
    resp_builder: &mut HttpResponseBuilder,
) {
    if let Some(val) = config_version {
        resp_builder.insert_header((
            AppHeader::XConfigVersion.to_string(),
            val.clone().to_string(),
        ));
    }
}

pub fn get_max_created_at(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> Result<DateTime<Utc>, diesel::result::Error> {
    config_versions::config_versions
        .select(max(config_versions::created_at))
        .schema_name(schema_name)
        .first::<Option<DateTime<Utc>>>(conn)
        .and_then(|res| res.ok_or(diesel::result::Error::NotFound))
}

pub fn is_not_modified(max_created_at: Option<DateTime<Utc>>, req: &HttpRequest) -> bool {
    let nanosecond_erasure = |t: DateTime<Utc>| t.with_nanosecond(0);
    let last_modified = req
        .headers()
        .get("If-Modified-Since")
        .and_then(|header_val| {
            let header_str = header_val.to_str().ok()?;
            DateTime::parse_from_rfc2822(header_str)
                .map(|datetime| datetime.with_timezone(&Utc))
                .ok()
        })
        .and_then(nanosecond_erasure);
    log::info!("last modified {last_modified:?}");
    let parsed_max: Option<DateTime<Utc>> = max_created_at.and_then(nanosecond_erasure);
    max_created_at.is_some() && parsed_max <= last_modified
}

pub fn generate_config_from_version(
    version: &mut Option<i64>,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Config> {
    if let Some(val) = version {
        let config = config_versions::config_versions
            .select(config_versions::config)
            .filter(config_versions::id.eq(*val))
            .schema_name(schema_name)
            .get_result::<Value>(conn)
            .map_err(|err| {
                log::error!("failed to fetch config with error: {}", err);
                db_error!(err)
            })?;
        serde_json::from_value::<Config>(config).map_err(|err| {
            log::error!("failed to decode config: {}", err);
            unexpected_error!("failed to decode config")
        })
    } else {
        match config_versions::config_versions
            .select((config_versions::id, config_versions::config))
            .order(config_versions::created_at.desc())
            .schema_name(schema_name)
            .first::<(i64, Value)>(conn)
        {
            Ok((latest_version, config)) => {
                *version = Some(latest_version);
                serde_json::from_value::<Config>(config).or_else(|err| {
                    log::error!("failed to decode config: {}", err);
                    generate_cac(conn, schema_name)
                })
            }
            Err(err) => {
                log::error!("failed to find latest config: {err}");
                generate_cac(conn, schema_name)
            }
        }
    }
}

pub fn setup_query_data(
    req: &HttpRequest,
    body: &Option<Json<ContextPayload>>,
    dimension_params: &DimensionQuery<QueryMap>,
) -> superposition::Result<(bool, QueryMap)> {
    let is_smithy: bool;
    let query_data = if req.method() == actix_web::http::Method::GET {
        is_smithy = false;
        (**dimension_params).clone()
    } else {
        is_smithy = true;
        body.as_ref()
            .ok_or(bad_argument!(
                "When using POST, context needs to be provided in the body."
            ))?
            .context
            .clone()
            .into()
    };
    Ok((is_smithy, query_data))
}

pub fn resolve(
    config: &mut Config,
    mut query_data: QueryMap,
    merge_strategy: Header<MergeStrategy>,
    conn: &mut DBConnection,
    query_filters: &ResolveConfigQuery,
    workspace_request: &WorkspaceContext,
) -> superposition::Result<Map<String, Value>> {
    if let Some(context_id) = &query_filters.context_id {
        config.contexts = if let Some(index) = config
            .contexts
            .iter()
            .position(|ctx| ctx.id == context_id.clone())
        {
            config.contexts[..index].to_vec()
        } else {
            return Err(bad_argument!(
                "context with id {} not found in CAC",
                context_id
            ));
        };
    }

    if query_filters.resolve_remote.unwrap_or_default() {
        query_data = QueryMap::from(evaluate_remote_cohorts(
            &config.dimensions,
            &query_data,
            conn,
            &workspace_request.schema_name,
        )?);
    }

    let merge_strategy = merge_strategy.into_inner();
    let show_reason = query_filters.show_reasoning.unwrap_or_default();
    let response = if show_reason {
        eval_cac_with_reasoning(config, &query_data, merge_strategy).map_err(|err| {
            log::error!("failed to eval cac with err: {}", err);
            unexpected_error!("cac eval failed")
        })
    } else {
        eval_cac(config, &query_data, merge_strategy).map_err(|err| {
            log::error!("failed to eval cac with err: {}", err);
            unexpected_error!("cac eval failed")
        })
    }?;

    Ok(response)
}
