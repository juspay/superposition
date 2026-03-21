use actix_web::{
    HttpRequest,
    web::{Data, Header, Json},
};
use cac_client::{eval_cac, eval_cac_with_reasoning};
use chrono::{DateTime, Utc};
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, dsl::max};
use serde_json::{Map, Value};
use service_utils::{
    redis::{CONFIG_VERSION_KEY_SUFFIX, read_through_cache},
    service::types::{AppState, EncryptionKey, SchemaName, WorkspaceContext},
};
use superposition_macros::{bad_argument, db_error, unexpected_error};
use superposition_types::{
    Config, DBConnection,
    api::config::{ContextPayload, MergeStrategy, ResolveConfigQuery},
    custom_query::{CommaSeparatedStringQParams, CustomQuery, DimensionQuery, QueryMap},
    database::schema::config_versions::dsl as config_versions,
    result as superposition,
};

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

pub async fn get_config_version(
    version: &Option<String>,
    workspace_context: &WorkspaceContext,
    state: &Data<AppState>,
) -> superposition::Result<i64> {
    match version.as_ref() {
        Some(v) if *v != *"latest" => v.parse::<i64>().map_or_else(
            |e| {
                log::error!("failed to decode version as integer: {v}, error: {e}");
                Err(bad_argument!("version is not of type integer"))
            },
            Ok,
        ),
        _ => match workspace_context.settings.config_version {
            Some(v) => Ok(v),
            None => read_through_cache::<i64>(
                format!(
                    "{}{CONFIG_VERSION_KEY_SUFFIX}",
                    *workspace_context.schema_name
                ),
                &workspace_context.schema_name,
                &state.redis,
                &state.db_pool,
                |conn| {
                    config_versions::config_versions
                        .select(config_versions::id)
                        .order_by(config_versions::created_at.desc())
                        .schema_name(&workspace_context.schema_name)
                        .first::<i64>(conn)
                },
            )
            .await
            .map_err(|e| unexpected_error!("Config version not found due to: {}", e)),
        },
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
    body: Option<Json<ContextPayload>>,
    dimension_params: DimensionQuery<QueryMap>,
) -> superposition::Result<(bool, QueryMap)> {
    let is_smithy = matches!(req.method(), &actix_web::http::Method::POST);
    let query_data = if req.method() == actix_web::http::Method::GET {
        dimension_params.into_inner()
    } else {
        body.map(|b| b.into_inner().context)
            .map(Into::into)
            .unwrap_or_default()
    };
    Ok((is_smithy, query_data))
}

pub fn resolve(
    config: &mut Config,
    mut query_data: QueryMap,
    merge_strategy: Header<MergeStrategy>,
    conn: &mut DBConnection,
    query_filters: &ResolveConfigQuery,
    workspace_context: &WorkspaceContext,
    master_encryption_key: &Option<EncryptionKey>,
) -> superposition::Result<Map<String, Value>> {
    *config = apply_prefix_filter_to_config(&query_filters.prefix, config.clone())?;

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
            workspace_context,
            master_encryption_key,
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
