use actix_web::{
    HttpRequest, HttpResponse, Scope, routes,
    web::{Data, Header, Json},
};
use context_aware_config::api::config::helpers::{
    add_audit_id_to_header, add_config_version_to_header, add_last_modified_to_header,
    generate_config_from_version, get_config_version, get_max_created_at,
    is_not_modified, resolve, setup_query_data,
};
use experimentation_platform::api::experiments::handlers::get_applicable_variants_helper;
use serde_json::{Map, Value};
use service_utils::{
    run_query,
    service::types::{AppState, WorkspaceContext},
};
use superposition_derives::authorized;
use superposition_types::{
    api::config::{ContextPayload, MergeStrategy, ResolveConfigQuery},
    custom_query::{self as superposition_query, CustomQuery, DimensionQuery, QueryMap},
    result as superposition,
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
    // TODO: Granularise the connection usage in this function once all crates are migrated
    let max_created_at = run_query!(state.db_pool, |conn| {
        get_max_created_at(conn, &workspace_context.schema_name)
    })
    .map_err(|e| log::error!("failed to fetch max timestamp from event_log : {e}"))
    .ok();

    if identifier_query.identifier.is_none() && is_not_modified(max_created_at, &req) {
        return Ok(HttpResponse::NotModified().finish());
    }

    let (is_smithy, mut query_data) = setup_query_data(&req, &body, &dimension_params)?;
    let mut config_version =
        get_config_version(&query_filters.version, &workspace_context)?;

    // This is needed as `generate_config_from_version` updates config_version value
    // in case nothing was found either from query params or workspace settings
    // This value is separately needed, as in the following check the value before the modification is required
    let config_ver = config_version.to_owned();

    // TODO: Granularise the connection usage in this function once all crates are migrated
    let mut config = run_query!(state.db_pool, |conn| {
        generate_config_from_version(
            &mut config_version,
            conn,
            &workspace_context.schema_name,
        )
    })?;

    if let (None, Some(identifier)) = (config_ver, identifier_query.identifier) {
        let context_map: &Map<String, Value> = &query_data;
        // TODO: Granularise the connection usage in this function once all crates are migrated
        let (applicable_variants, _) = run_query!(state.db_pool, |conn| {
            get_applicable_variants_helper(
                conn,
                context_map.clone(),
                &config.dimensions,
                identifier,
                &workspace_context,
            )
        })?;
        query_data.insert("variantIds".to_string(), applicable_variants.into());
    }

    // TODO: Granularise the connection usage in this function once all crates are migrated
    let resolved_config = run_query!(state.db_pool, |conn| {
        resolve(
            &mut config,
            query_data,
            merge_strategy,
            conn,
            &query_filters,
            &workspace_context,
            &state.master_encryption_key,
        )
    })?;

    let mut resp = HttpResponse::Ok();
    add_last_modified_to_header(max_created_at, is_smithy, &mut resp);
    // TODO: Granularise the connection usage in this function once all crates are migrated
    run_query!(state.db_pool, |conn| {
        add_audit_id_to_header(conn, &mut resp, &workspace_context.schema_name);
        Ok::<(), superposition::AppError>(())
    })?;
    add_config_version_to_header(&config_version, &mut resp);
    Ok(resp.json(resolved_config))
}
