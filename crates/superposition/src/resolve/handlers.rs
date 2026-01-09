use actix_web::{
    routes,
    web::{Data, Header, Json},
    HttpRequest, HttpResponse, Scope,
};
use chrono::{DateTime, Utc};
use context_aware_config::api::config::fetch_audit_id;
use context_aware_config::api::config::helpers::{
    add_config_version_to_header, add_last_modified_to_header,
    generate_config_from_version, get_config_version, get_max_created_at,
    is_not_modified, resolve, setup_query_data,
};
use diesel::{BoolExpressionMethods, ExpressionMethods, QueryDsl, RunQueryDsl};
use serde_json::{Map, Value};
use service_utils::{
    redis::{
        fetch_from_redis_else_writeback, AUDIT_ID_KEY_SUFFIX, CONFIG_KEY_SUFFIX,
        CONFIG_VERSION_KEY_SUFFIX, EXPERIMENT_GROUPS_LIST_KEY_SUFFIX,
        LAST_MODIFIED_KEY_SUFFIX,
    },
    service::{
        get_db_connection,
        types::{AppHeader, AppState, DbConnection, WorkspaceContext},
    },
};
use std::collections::{HashMap, HashSet};
use superposition_core::experiment::{
    get_applicable_buckets_from_group, get_applicable_variants_from_group_response,
    FfiExperiment, FfiExperimentGroup,
};
use superposition_derives::authorized;
use superposition_macros::{db_error, not_found, unexpected_error};
use superposition_types::{
    api::config::{ContextPayload, MergeStrategy, ResolveConfigQuery},
    custom_query::{self as superposition_query, CustomQuery, DimensionQuery, QueryMap},
    database::{
        models::experimentation::{Experiment, ExperimentGroup, ExperimentStatusType},
        schema::{experiment_groups::dsl as experiment_groups, experiments::dsl},
    },
    logic::evaluate_local_cohorts,
    result as superposition, Config, PaginatedResponse,
};

use super::types::IdentifierQuery;

pub fn endpoints() -> Scope {
    Scope::new("").service(resolve_with_exp_handler)
}

#[allow(clippy::too_many_arguments)]
#[authorized]
#[routes]
#[get("")]
#[post("")]
async fn resolve_with_exp_handler(
    req: HttpRequest,
    body: Option<Json<ContextPayload>>,
    merge_strategy: Header<MergeStrategy>,
    dimension_params: DimensionQuery<QueryMap>,
    query_filters: superposition_query::Query<ResolveConfigQuery>,
    identifier_query: superposition_query::Query<IdentifierQuery>,
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
) -> superposition::Result<HttpResponse> {
    let query_filters = query_filters.into_inner();
    let identifier_query = identifier_query.into_inner();
    let schema_name = workspace_context.schema_name.clone();

    let max_created_at = fetch_from_redis_else_writeback::<DateTime<Utc>>(
        format!("{}{LAST_MODIFIED_KEY_SUFFIX}", *schema_name),
        &schema_name,
        state.redis.clone(),
        state.db_pool.clone(),
        |db_pool| {
            let DbConnection(mut conn) = get_db_connection(db_pool)?;
            get_max_created_at(&mut conn, &schema_name).map_err(|e| {
                log::error!("failed to fetch max timestamp from event_log: {e}");
                db_error!(e)
            })
        },
    )
    .await
    .ok();

    if identifier_query.identifier.is_none() && is_not_modified(max_created_at, &req) {
        return Ok(HttpResponse::NotModified().finish());
    }

    let (is_smithy, mut query_data) = setup_query_data(&req, &body, &dimension_params)?;

    let config_version = fetch_from_redis_else_writeback::<i64>(
        format!("{}{CONFIG_VERSION_KEY_SUFFIX}", *schema_name),
        &schema_name,
        state.redis.clone(),
        state.db_pool.clone(),
        |db_pool| {
            let DbConnection(mut conn) = get_db_connection(db_pool)?;
            get_config_version(&query_filters.version, &workspace_context, &mut conn)
        },
    )
    .await
    .map_err(|e| unexpected_error!("Config version not found due to: {}", e))?;
    
    let mut config = fetch_from_redis_else_writeback::<Config>(
        format!("{}::{}{CONFIG_KEY_SUFFIX}", *schema_name, config_version),
        &schema_name,
        state.redis.clone(),
        state.db_pool.clone(),
        |db_pool| {
            let DbConnection(mut conn) = get_db_connection(db_pool)?;
            generate_config_from_version(
                &mut Some(config_version),
                &mut conn,
                &workspace_context.schema_name,
            )
        },
    )
    .await
    .map_err(|e| unexpected_error!("failed to generate config: {}", e))?;

    if let (None, Some(identifier)) =
        (query_filters.version.clone(), identifier_query.identifier)
    {
        let context_map: &Map<String, Value> = &query_data;

        // Fetch experiment groups from redis
        let experiment_groups =
            fetch_from_redis_else_writeback::<PaginatedResponse<ExperimentGroup>>(
                format!("{}{EXPERIMENT_GROUPS_LIST_KEY_SUFFIX}", *schema_name),
                &schema_name,
                state.redis.clone(),
                state.db_pool.clone(),
                |db_pool| {
                    let DbConnection(mut conn) = get_db_connection(db_pool)?;
                    let groups = experiment_groups::experiment_groups
                        .schema_name(&workspace_context.schema_name)
                        .load::<ExperimentGroup>(&mut conn)
                        .map_err(|e| {
                            log::error!("failed to fetch experiment groups: {e}");
                            db_error!(e)
                        })?;
                    let total_items = groups.len() as i64;
                    Ok(PaginatedResponse {
                        total_pages: 1,
                        total_items,
                        data: groups,
                    })
                },
            )
            .await;

        let experiment_groups: Vec<ExperimentGroup> = experiment_groups
            .map(|paginated| paginated.data)
            .unwrap_or_default();

        // Convert to FfiExperimentGroup for superposition_core functions
        let ffi_experiment_groups: Vec<FfiExperimentGroup> = experiment_groups
            .into_iter()
            .map(FfiExperimentGroup::from)
            .collect();

        let context =
            Value::Object(evaluate_local_cohorts(&config.dimensions, context_map));

        let buckets = get_applicable_buckets_from_group(
            &ffi_experiment_groups,
            &context,
            &identifier,
        );

        let exp_ids = buckets
            .iter()
            .filter_map(|(_, bucket)| bucket.experiment_id.parse::<i64>().ok())
            .collect::<HashSet<_>>();

        // Fetch experiments from database (these are filtered by specific IDs, so caching all wouldn't help much)
        let exps: HashMap<String, FfiExperiment> = if !exp_ids.is_empty() {
            let DbConnection(mut conn) = get_db_connection(state.db_pool.clone())?;
            dsl::experiments
                .filter(
                    dsl::id
                        .eq_any(exp_ids)
                        .and(dsl::status.eq(ExperimentStatusType::INPROGRESS)),
                )
                .schema_name(&workspace_context.schema_name)
                .load::<Experiment>(&mut conn)
                .map_err(|e| {
                    log::error!("failed to fetch experiments: {e}");
                    db_error!(e)
                })?
                .into_iter()
                .map(|exp| {
                    let ffi_exp = FfiExperiment::from(exp);
                    let id = ffi_exp.id.clone();
                    (id, ffi_exp)
                })
                .collect()
        } else {
            HashMap::new()
        };

        let applicable_variants =
            get_applicable_variants_from_group_response(&exps, &context, &buckets);
        query_data.insert("variantIds".to_string(), applicable_variants.into());
    }

    let resolved_config = {
        let DbConnection(mut conn) = get_db_connection(state.db_pool.clone())?;
        resolve(
            &mut config,
            query_data,
            merge_strategy,
            &mut conn,
            &query_filters,
            &workspace_context,
        )?
    };

    let mut resp = HttpResponse::Ok();
    add_last_modified_to_header(max_created_at, is_smithy, &mut resp);

    // Fetch audit_id from redis
    if let Ok(audit_id) = fetch_from_redis_else_writeback::<String>(
        format!("{}{AUDIT_ID_KEY_SUFFIX}", schema_name.0),
        &schema_name,
        state.redis.clone(),
        state.db_pool.clone(),
        |db_pool| {
            let DbConnection(mut conn) = get_db_connection(db_pool)?;
            fetch_audit_id(&mut conn, &workspace_context.schema_name)
                .ok_or(not_found!("Audit ID not found"))
        },
    )
    .await
    {
        resp.insert_header((AppHeader::XAuditId.to_string(), audit_id));
    }

    add_config_version_to_header(&Some(config_version), &mut resp);
    Ok(resp.json(resolved_config))
}
