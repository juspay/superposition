use std::collections::HashMap;

use actix_web::{
    HttpRequest,
    web::{Data, Header, Json},
};
use cac_client::{eval_cac, eval_cac_with_reasoning};
use chrono::{DateTime, Utc};
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, dsl::max};
use serde_json::{Map, Value};
use service_utils::{
    helpers::is_not_modified,
    redis::{
        CONFIG_KEY_SUFFIX, CONFIG_VERSION_KEY_SUFFIX, LAST_MODIFIED_KEY_SUFFIX,
        read_through_cache,
    },
    service::types::{AppState, EncryptionKey, SchemaName, WorkspaceContext},
};
use superposition_macros::{bad_argument, db_error, unexpected_error};
use superposition_types::{
    Config, DBConnection,
    api::config::{
        ContextPayload, DetailedResolvedConfigValue, DetailedResolvedConfiguration,
        ExplainKeyQuery, ExplainResolveQuery, Explanation, ExplanationTimelineItem,
        MergeStrategy, ResolveConfigQuery,
    },
    custom_query::{CommaSeparatedStringQParams, CustomQuery, DimensionQuery, QueryMap},
    database::{
        models::Description,
        schema::{
            config_versions::dsl as config_versions,
            default_configs::dsl as default_configs,
        },
    },
    logic::evaluate_local_cohorts,
    result as superposition,
};

use crate::helpers::{evaluate_remote_cohorts, generate_cac};

struct DefaultConfigMetadata {
    description: String,
    schema: Value,
}

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

pub struct ResolveRequestData {
    pub config: Config,
    pub config_version: i64,
    pub max_created_at: Option<DateTime<Utc>>,
    pub is_smithy: bool,
    pub query_data: QueryMap,
}

impl ResolveRequestData {
    pub async fn prepare(
        req: &HttpRequest,
        body: Option<Json<ContextPayload>>,
        dimension_params: DimensionQuery<QueryMap>,
        version: &Option<String>,
        workspace_context: &WorkspaceContext,
        state: &Data<AppState>,
    ) -> superposition::Result<Option<Self>> {
        let schema_name = &workspace_context.schema_name;

        let max_created_at = read_through_cache::<DateTime<Utc>>(
            format!("{}{LAST_MODIFIED_KEY_SUFFIX}", **schema_name),
            schema_name,
            &state.redis,
            &state.db_pool,
            |conn| get_max_created_at(conn, schema_name),
        )
        .await
        .ok();

        if is_not_modified(max_created_at, req) {
            return Ok(None);
        }

        let config_version =
            get_config_version(version, workspace_context, state).await?;

        let config = read_through_cache::<Config>(
            format!("{}::{}{CONFIG_KEY_SUFFIX}", **schema_name, config_version,),
            schema_name,
            &state.redis,
            &state.db_pool,
            |conn| {
                generate_config_from_version(
                    &mut Some(config_version),
                    conn,
                    &workspace_context.schema_name,
                )
                .map_err(|err| {
                    log::error!(
                        "failed to generate config from version with error: {}",
                        err
                    );
                    diesel::result::Error::NotFound
                })
            },
        )
        .await
        .map_err(|e| unexpected_error!("failed to generate config: {}", e))?;

        let (is_smithy, query_data) = setup_query_data(req, body, dimension_params)?;

        Ok(Some(Self {
            config,
            config_version,
            max_created_at,
            is_smithy,
            query_data,
        }))
    }
}

fn apply_context_id_filter(
    config: &mut Config,
    context_id: &Option<String>,
) -> superposition::Result<()> {
    if let Some(context_id) = context_id {
        config.contexts = if let Some(index) =
            config.contexts.iter().position(|ctx| ctx.id == *context_id)
        {
            config.contexts[..index].to_vec()
        } else {
            return Err(bad_argument!(
                "context with id {} not found in CAC",
                context_id
            ));
        };
    }

    Ok(())
}

fn apply_resolution_filters(
    config: &mut Config,
    query_filters: &ResolveConfigQuery,
) -> superposition::Result<()> {
    *config = apply_prefix_filter_to_config(&query_filters.prefix, config.clone())?;
    apply_context_id_filter(config, &query_filters.context_id)
}

fn prepare_remote_query_data(
    config: &Config,
    query_data: QueryMap,
    conn: &mut DBConnection,
    resolve_remote: Option<bool>,
    workspace_context: &WorkspaceContext,
    master_encryption_key: &Option<EncryptionKey>,
) -> superposition::Result<QueryMap> {
    if resolve_remote.unwrap_or_default() {
        Ok(QueryMap::from(evaluate_remote_cohorts(
            &config.dimensions,
            &query_data,
            conn,
            workspace_context,
            master_encryption_key,
        )?))
    } else {
        Ok(query_data)
    }
}

fn evaluate_resolved_config(
    config: &Config,
    query_data: &QueryMap,
    merge_strategy: MergeStrategy,
    show_reason: bool,
) -> superposition::Result<Map<String, Value>> {
    if show_reason {
        eval_cac_with_reasoning(config, query_data, merge_strategy).map_err(|err| {
            log::error!("failed to eval cac with err: {}", err);
            unexpected_error!("cac eval failed")
        })
    } else {
        eval_cac(config, query_data, merge_strategy).map_err(|err| {
            log::error!("failed to eval cac with err: {}", err);
            unexpected_error!("cac eval failed")
        })
    }
}

fn fetch_default_config_metadata(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
    keys: &[String],
) -> superposition::Result<HashMap<String, DefaultConfigMetadata>> {
    if keys.is_empty() {
        return Ok(HashMap::new());
    }

    let rows = default_configs::default_configs
        .filter(default_configs::key.eq_any(keys))
        .select((
            default_configs::key,
            default_configs::schema,
            default_configs::description,
        ))
        .schema_name(schema_name)
        .load::<(String, Value, Description)>(conn)
        .map_err(|err| {
            log::error!(
                "failed to fetch default config metadata with error: {}",
                err
            );
            db_error!(err)
        })?;

    Ok(rows
        .into_iter()
        .map(|(key, schema, description)| {
            (
                key,
                DefaultConfigMetadata {
                    description: String::from(&description),
                    schema,
                },
            )
        })
        .collect())
}

fn build_resolved_config(
    resolved_config: Map<String, Value>,
    metadata: &HashMap<String, DefaultConfigMetadata>,
) -> DetailedResolvedConfiguration {
    resolved_config
        .into_iter()
        .map(|(key, value)| {
            let (description, schema) = metadata
                .get(&key)
                .map(|metadata| (metadata.description.clone(), Some(&metadata.schema)))
                .unwrap_or_else(|| (String::new(), None));

            (
                key,
                DetailedResolvedConfigValue {
                    description,
                    schema: schema.cloned(),
                    value,
                },
            )
        })
        .collect()
}

pub fn resolve(
    config: &mut Config,
    query_data: QueryMap,
    merge_strategy: Header<MergeStrategy>,
    conn: &mut DBConnection,
    query_filters: &ResolveConfigQuery,
    workspace_context: &WorkspaceContext,
    master_encryption_key: &Option<EncryptionKey>,
) -> superposition::Result<Map<String, Value>> {
    apply_resolution_filters(config, query_filters)?;
    let query_data = prepare_remote_query_data(
        config,
        query_data,
        conn,
        query_filters.resolve_remote,
        workspace_context,
        master_encryption_key,
    )?;
    let merge_strategy = merge_strategy.into_inner();
    let show_reason = query_filters.show_reasoning.unwrap_or_default();

    evaluate_resolved_config(config, &query_data, merge_strategy, show_reason)
}

pub fn resolve_detailed(
    config: &Config,
    context_data: QueryMap,
    merge_strategy: Header<MergeStrategy>,
    conn: &mut DBConnection,
    resolve_options: &ResolveConfigQuery,
    workspace_context: &WorkspaceContext,
    master_encryption_key: &Option<EncryptionKey>,
) -> superposition::Result<DetailedResolvedConfiguration> {
    let mut resolution_config = config.clone();
    apply_resolution_filters(&mut resolution_config, resolve_options)?;

    let context_data = prepare_remote_query_data(
        &resolution_config,
        context_data,
        conn,
        resolve_options.resolve_remote,
        workspace_context,
        master_encryption_key,
    )?;
    let merge_strategy = merge_strategy.into_inner();
    let show_reason = resolve_options.show_reasoning.unwrap_or_default();

    let resolved_config = evaluate_resolved_config(
        &resolution_config,
        &context_data,
        merge_strategy.clone(),
        show_reason,
    )?;
    let keys = resolved_config.keys().cloned().collect::<Vec<_>>();
    let metadata =
        fetch_default_config_metadata(conn, &workspace_context.schema_name, &keys)?;
    let detailed_config = build_resolved_config(resolved_config, &metadata);
    Ok(detailed_config)
}

#[allow(clippy::too_many_arguments)]
pub fn explain_resolved_config(
    config: &Config,
    context_data: QueryMap,
    merge_strategy: Header<MergeStrategy>,
    conn: &mut DBConnection,
    resolve_options: &ExplainResolveQuery,
    explain_key: &ExplainKeyQuery,
    workspace_context: &WorkspaceContext,
    master_encryption_key: &Option<EncryptionKey>,
) -> superposition::Result<Explanation> {
    let mut explanation_config = config.clone();
    apply_context_id_filter(&mut explanation_config, &resolve_options.context_id)?;

    let context_data = prepare_remote_query_data(
        &explanation_config,
        context_data,
        conn,
        resolve_options.resolve_remote,
        workspace_context,
        master_encryption_key,
    )?;
    let merge_strategy = merge_strategy.into_inner();

    let mut current_value = explanation_config
        .default_configs
        .get(&explain_key.key)
        .cloned()
        .ok_or_else(|| {
            bad_argument!("default config key {} not found", explain_key.key)
        })?;
    let context_data =
        evaluate_local_cohorts(&explanation_config.dimensions, &context_data);
    let mut timeline = Vec::new();

    for context in &explanation_config.contexts {
        if !superposition_types::apply(&context.condition, &context_data) {
            continue;
        }

        let override_id = context.override_with_keys.get_key().clone();
        let value_before = current_value.clone();

        if let Some(override_value) = explanation_config
            .overrides
            .get(&override_id)
            .and_then(|overrides| overrides.get(&explain_key.key))
        {
            match &merge_strategy {
                MergeStrategy::REPLACE => current_value = override_value.clone(),
                MergeStrategy::MERGE => {
                    superposition_core::merge(&mut current_value, override_value)
                }
            }
        }

        let condition = serde_json::to_value(&context.condition).map_err(|err| {
            log::error!("failed to encode context condition with error: {}", err);
            unexpected_error!("failed to encode context condition")
        })?;

        timeline.push(ExplanationTimelineItem {
            context_id: context.id.clone(),
            condition,
            override_id,
            value_before,
            value_after: current_value.clone(),
        });
    }

    Ok(Explanation {
        key: explain_key.key.clone(),
        timeline,
    })
}
