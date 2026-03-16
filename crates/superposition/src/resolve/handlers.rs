use actix_web::{
    HttpRequest, HttpResponse, Scope, routes,
    web::{Data, Header, Json},
};
use chrono::{DateTime, Utc};
use context_aware_config::api::config::helpers::{
    generate_config_from_version, get_config_version, get_max_created_at,
    is_not_modified, resolve, setup_query_data,
};
use experimentation_platform::api::experiments::handlers::get_applicable_variants_helper;
use serde_json::{Map, Value};
use service_utils::{
    redis::{CONFIG_KEY_SUFFIX, LAST_MODIFIED_KEY_SUFFIX, read_through_cache},
    service::types::{AppHeader, AppState, WorkspaceContext},
};
use superposition_derives::{authorized, declare_resource};
use superposition_macros::unexpected_error;
use superposition_types::{
    Config,
    api::config::{ContextPayload, MergeStrategy, ResolveConfigQuery},
    custom_query::{self as superposition_query, CustomQuery, DimensionQuery, QueryMap},
    result as superposition,
};

use super::types::IdentifierQuery;

declare_resource!(Config);

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

    let max_created_at = read_through_cache::<DateTime<Utc>>(
        format!("{}{LAST_MODIFIED_KEY_SUFFIX}", *schema_name),
        &schema_name,
        &state.redis,
        &state.db_pool,
        |conn| get_max_created_at(conn, &schema_name),
    )
    .await
    .ok();

    if identifier_query.identifier.is_none() && is_not_modified(max_created_at, &req) {
        return Ok(HttpResponse::NotModified().finish());
    }

    let (is_smithy, mut query_data) = setup_query_data(&req, &body, &dimension_params)?;

    let config_version =
        get_config_version(&query_filters.version, &workspace_context, &state).await?;

    let mut config = read_through_cache::<Config>(
        format!("{}::{}{CONFIG_KEY_SUFFIX}", *schema_name, config_version),
        &schema_name,
        &state.redis,
        &state.db_pool,
        |conn| {
            generate_config_from_version(
                &mut Some(config_version),
                conn,
                &workspace_context.schema_name,
            )
            .map_err(|err| {
                log::error!("failed to generate config from version with error: {}", err);
                // can't throw the AppError from here because fetch_from_redis_else_writeback
                // expects a DieselResult error type, so we log the actual error and return NotFound
                // which will trigger generate_cac in the fallback and if
                // that also fails then it will return the actual error
                diesel::result::Error::NotFound
            })
        },
    )
    .await
    .map_err(|e| unexpected_error!("failed to generate config: {}", e))?;

    if let (None, Some(identifier)) =
        (&query_filters.version, identifier_query.identifier)
    {
        let context_map: &Map<String, Value> = &query_data;
        let (applicable_variants, _) = get_applicable_variants_helper(
            context_map.clone(),
            &config.dimensions,
            identifier,
            &workspace_context,
            &state,
        )
        .await?;
        query_data.insert("variantIds".to_string(), applicable_variants.into());
    };

    let resolved_config = {
        let mut conn = state.db_pool.get().map_err(|e| {
            log::error!("Unable to get db connection from pool, error: {e}");
            unexpected_error!("Unable to get db connection from pool: {}", e)
        })?;
        resolve(
            &mut config,
            query_data,
            merge_strategy,
            &mut conn,
            &query_filters,
            &workspace_context,
            &state.master_encryption_key,
        )?
    };

    let mut resp = HttpResponse::Ok();
    AppHeader::add_last_modified(max_created_at, is_smithy, &mut resp);

    AppHeader::add_config_version(&Some(config_version), &mut resp);
    Ok(resp.json(resolved_config))
}
