use std::collections::HashMap;

use actix_web::{
    routes,
    web::{Header, Json},
    HttpRequest, HttpResponse, Scope,
};
use cac_client::{eval_cac, eval_cac_with_reasoning};
use experimentation_client::{
    get_applicable_buckets_from_group, get_applicable_variants_from_group_response,
    ExperimentResponse,
};
use experimentation_platform::api::{
    experiment_groups::helpers::fetch_all_experiments_groups,
    experiments::helpers::fetch_all_experiments,
};
use serde_json::Value;
use service_utils::service::types::{DbConnection, WorkspaceContext};
use superposition_macros::{bad_argument, unexpected_error};
use superposition_types::{
    api::config::{ContextPayload, MergeStrategy, ResolveConfigQuery},
    custom_query::{self as superposition_query, CustomQuery, DimensionQuery, QueryMap},
    logic::evaluate_local_cohorts,
    result as superposition, DBConnection,
};

use super::types::ExtendedResolveConfigQuery;
use context_aware_config::{
    api::config::helpers::{
        add_audit_id_to_header, add_config_version_to_header,
        add_last_modified_to_header, apply_prefix_filter_to_config,
        generate_config_from_version, get_config_version, get_max_created_at, resolve,
    },
    helpers::evaluate_remote_cohorts,
};

pub fn endpoints() -> Scope {
    Scope::new("").service(evaluate)
}

#[allow(clippy::too_many_arguments)]
fn resolve_with_targetting_key(
    req: HttpRequest,
    body: Option<Json<ContextPayload>>,
    merge_strategy: Header<MergeStrategy>,
    conn: &mut DBConnection,
    dimension_params: DimensionQuery<QueryMap>,
    query_filters: &ResolveConfigQuery,
    targetting_key: &str,
    workspace_context: WorkspaceContext,
) -> superposition::Result<HttpResponse> {
    let max_created_at = get_max_created_at(conn, &workspace_context.schema_name)
        .map_err(|e| log::error!("failed to fetch max timestamp from event_log : {e}"))
        .ok();

    let is_smithy: bool;
    let mut query_data = if req.method() == actix_web::http::Method::GET {
        is_smithy = false;
        dimension_params.into_inner()
    } else {
        // Must be smithy calling w/ POST :D
        is_smithy = true;
        body.ok_or(bad_argument!(
            "When using POST, context needs to be provided in the body."
        ))?
        .into_inner()
        .context
        .into()
    };

    let config_version =
        get_config_version(&query_filters.version, &workspace_context, conn)?;

    let mut config = generate_config_from_version(
        &mut None, // not passing config_version here to avoid evaluating with the some older version mentioned in the workspace.
        conn,
        &workspace_context.schema_name,
    )?;

    config = apply_prefix_filter_to_config(&query_filters.prefix, config)?;

    let merge_strategy = merge_strategy.into_inner();
    let show_reason = query_filters.show_reasoning.unwrap_or_default();

    let experiments = fetch_all_experiments(conn, &workspace_context.schema_name)?
        .into_iter()
        .map(|exp| {
            let exp_response = ExperimentResponse::from(exp);
            let id = exp_response.id.clone();
            (id, exp_response)
        })
        .collect::<HashMap<String, ExperimentResponse>>();

    let experiment_groups =
        fetch_all_experiments_groups(conn, &workspace_context.schema_name)?;

    if query_filters.resolve_remote.unwrap_or_default() {
        query_data = QueryMap::from(evaluate_remote_cohorts(
            &config.dimensions,
            &query_data,
            conn,
            &workspace_context.schema_name,
        )?);
    }
    let context = Value::Object(evaluate_local_cohorts(&config.dimensions, &query_data));

    let buckets =
        get_applicable_buckets_from_group(&experiment_groups, &context, targetting_key);

    let applicable_variants =
        get_applicable_variants_from_group_response(&experiments, &context, &buckets);

    query_data.insert("variantIds".to_string(), applicable_variants.into());

    let response = if show_reason {
        eval_cac_with_reasoning(&config, &query_data, merge_strategy).map_err(|err| {
            log::error!("failed to eval cac with err: {}", err);
            unexpected_error!("cac eval failed")
        })?
    } else {
        eval_cac(&config, &query_data, merge_strategy).map_err(|err| {
            log::error!("failed to eval cac with err: {}", err);
            unexpected_error!("cac eval failed")
        })?
    };

    let mut resp = HttpResponse::Ok();
    add_last_modified_to_header(max_created_at, is_smithy, &mut resp);
    add_audit_id_to_header(conn, &mut resp, &workspace_context.schema_name);
    add_config_version_to_header(&config_version, &mut resp);

    Ok(resp.json(response))
}

#[routes]
#[get("")]
#[post("")]
async fn evaluate(
    req: HttpRequest,
    body: Option<Json<ContextPayload>>,
    merge_strategy: Header<MergeStrategy>,
    db_conn: DbConnection,
    dimension_params: DimensionQuery<QueryMap>,
    query_filters: superposition_query::Query<ExtendedResolveConfigQuery>,
    workspace_context: WorkspaceContext,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let query_filters = query_filters.into_inner();
    let resolve_query_filters = ResolveConfigQuery {
        version: query_filters.version,
        show_reasoning: query_filters.show_reasoning,
        resolve_remote: query_filters.resolve_remote,
        context_id: query_filters.context_id,
        prefix: query_filters.prefix,
    };

    if let Some(targetting_key) = query_filters.targetting_key {
        resolve_with_targetting_key(
            req,
            body,
            merge_strategy,
            &mut conn,
            dimension_params,
            &resolve_query_filters,
            &targetting_key,
            workspace_context,
        )
    } else {
        resolve(
            req,
            body,
            merge_strategy,
            &mut conn,
            dimension_params,
            resolve_query_filters,
            workspace_context,
        )
    }
}
