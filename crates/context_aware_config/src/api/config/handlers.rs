use std::collections::HashMap;

#[cfg(feature = "high-performance-mode")]
use actix_http::StatusCode;
#[cfg(feature = "high-performance-mode")]
use actix_web::http::header::ContentType;
use actix_web::{
    HttpRequest, HttpResponse, Scope, get, put, routes,
    web::{Data, Header, Json, Path, Query},
};
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
#[cfg(feature = "high-performance-mode")]
use fred::interfaces::KeysInterface;
use itertools::Itertools;
use serde_json::{Map, Value, json};
#[cfg(feature = "high-performance-mode")]
use service_utils::service::types::AppHeader;
use service_utils::{
    helpers::fetch_dimensions_info_map,
    service::types::{AppState, DbConnection, WorkspaceContext},
};
use superposition_derives::authorized;
#[cfg(feature = "high-performance-mode")]
use superposition_macros::response_error;
use superposition_macros::{bad_argument, unexpected_error};
use superposition_types::{
    Cac, Condition, Config, Context, DBConnection, DimensionInfo, OverrideWithKeys,
    Overrides, PaginatedResponse, User,
    api::{
        config::{ConfigQuery, ContextPayload, MergeStrategy, ResolveConfigQuery},
        context::PutRequest,
    },
    custom_query::{
        self as superposition_query, CustomQuery, DimensionQuery, PaginationParams,
        QueryMap,
    },
    database::{
        models::{
            ChangeReason,
            cac::{ConfigVersion, ConfigVersionListItem},
        },
        schema::config_versions::dsl as config_versions,
    },
    result as superposition,
};

use crate::api::context::{self, helpers::query_description};
use crate::{
    api::config::helpers::{
        add_audit_id_to_header, add_config_version_to_header,
        add_last_modified_to_header, generate_config_from_version, get_config_version,
        get_max_created_at, is_not_modified,
    },
    helpers::{calculate_context_weight, generate_cac},
};

use super::helpers::{apply_prefix_filter_to_config, resolve, setup_query_data};

#[allow(clippy::let_and_return)]
pub fn endpoints() -> Scope {
    let scope = Scope::new("")
        .service(get_handler)
        .service(resolve_handler)
        .service(reduce_handler)
        .service(list_version_handler)
        .service(get_version_handler);
    #[cfg(feature = "high-performance-mode")]
    let scope = scope.service(get_fast_handler);
    scope
}

fn generate_subsets(map: &Map<String, Value>) -> Vec<Map<String, Value>> {
    let mut subsets = Vec::new();
    let keys: Vec<String> = map.keys().cloned().collect_vec();
    let all_subsets_keys = generate_subsets_keys(keys);

    for subset_keys in &all_subsets_keys {
        let mut subset_map = Map::new();

        for key in subset_keys {
            if let Some(value) = map.get(key) {
                subset_map.insert(key.to_string(), value.clone());
            }
        }

        subsets.push(subset_map);
    }

    subsets
}

fn generate_subsets_keys(keys: Vec<String>) -> Vec<Vec<String>> {
    let mut res = vec![[].to_vec()];
    for element in keys {
        let len = res.len();
        for ind in 0..len {
            let mut sub = res[ind].clone();
            sub.push(element.clone());
            res.push(sub);
        }
    }
    res
}

fn reduce(
    contexts_overrides_values: Vec<(Context, Map<String, Value>, Value, String)>,
    default_config_val: &Value,
) -> superposition::Result<Vec<Map<String, Value>>> {
    let mut dimensions: Vec<Map<String, Value>> = Vec::new();
    for (context, overrides, key_val, override_id) in contexts_overrides_values {
        let mut ct_dimensions: Map<String, Value> = context.condition.clone().into();

        ct_dimensions.insert("key_val".to_string(), key_val);
        let request_payload = json!({
            "override": overrides,
            "context": context.condition,
            "id": context.id,
            "to_be_deleted": overrides.is_empty(),
            "override_id": override_id,
        });
        ct_dimensions.insert("req_payload".to_string(), request_payload);
        dimensions.push(ct_dimensions);
    }

    //adding default config value
    let mut default_config_map = Map::new();
    default_config_map.insert("key_val".to_string(), default_config_val.to_owned());
    dimensions.push(default_config_map);

    /*
    We now have dimensions array, which is a vector of elements representing each context present where each element is a type of Map<String,Value> which contains the following
        1. all the dimensions and value of those dimensions in the context
        2. key_val, which is the value of the override key for which we are trying to reduce
        3. A req_payload which contains the details of the context like, context_id, override_id, the context_condition, new overrides (without containing the key that has to be reduced)
        {
            dimension1_in_context : value_of_dimension1_in_context,
            dimension2_in_context : value_of_dimension2_in_context,
            .
            .
            key_val: value of the override key that we are trying to reduce
            req_payload : {
                override : new_overrides(without the key that is to be reduced)
                context : context_condition
                id : context_id
                to_be_deleted : if new_overrides is empty then delete this context
            }
        }

    We have also sorted this dimensions vector in descending order based on the weight of the dimensions in that context
    and in this vector the default config will be at the end of the list as it has no dimensions and it's weight is the least

    Now we iterate from start and then pick an element and generate all subsets of that element keys excluding the req_payload and key_val
    i.e we only generate different subsets of dimensions of that context along with the value of those dimensions in that context

    Next we check if in the vector we find any other element c2 whose dimensions is part of the subsets of the parent element c1
    if dimensions_subsets_of_c1 contains dimensions_of_c2

        if the value of the override key is same in both c1 and c2 then we can reduce or remove that key in the override of c1
        so we mark the can_be_reduce to be true, and then update the dimensions vector.

        but if we find any other element c3 whose dimensions is a subset of c1_dimensions but the value is not the same
        then that means we can't reduce this key from c1, because in resolve if we remove it from c1 it will pick the value form c3 which is different.
        So if we find this element c3 before any other element which is a subset of c1 with the same value, then we can't reduce this key for c1 so we break
        and continue with the next element.
        Here "before" means the element with higher weight comes first with a subset of c1 but differnt override value for the key
     */
    for (c1_index, dimensions_of_c1_with_payload) in dimensions.clone().iter().enumerate()
    {
        let mut dimensions_of_c1 = dimensions_of_c1_with_payload.clone();
        dimensions_of_c1.remove("req_payload");
        let override_val_of_key_in_c1 = dimensions_of_c1.remove("key_val");
        let dimensions_subsets_of_c1 = generate_subsets(&dimensions_of_c1);
        for (c2_index, dimensions_in_c2_with_payload) in dimensions.iter().enumerate() {
            let mut dimensions_of_c2 = dimensions_in_c2_with_payload.clone();
            dimensions_of_c2.remove("req_payload");
            let override_val_of_key_in_c2 = dimensions_of_c2.remove("key_val");
            if c2_index != c1_index
                && dimensions_subsets_of_c1.contains(&dimensions_of_c2)
            {
                if override_val_of_key_in_c1 == override_val_of_key_in_c2 {
                    let mut temp_c1 = dimensions_of_c1_with_payload.to_owned();
                    temp_c1.insert("can_be_reduced".to_string(), Value::Bool(true));
                    dimensions[c1_index] = temp_c1;
                    break;
                } else if override_val_of_key_in_c2.is_some() {
                    break;
                }
            }
        }
    }
    Ok(dimensions)
}

#[allow(clippy::type_complexity)]
fn get_contextids_from_overrideid(
    contexts: Vec<Context>,
    overrides: Map<String, Value>,
    key_val: Value,
    override_id: &str,
) -> superposition::Result<Vec<(Context, Map<String, Value>, Value, String)>> {
    let mut res: Vec<(Context, Map<String, Value>, Value, String)> = Vec::new();
    for ct in contexts {
        if ct.condition.contains_key("variantIds") {
            continue;
        }
        let override_keys = &ct.override_with_keys;
        if override_keys.contains(&override_id.to_owned()) {
            res.push((
                ct,
                overrides.clone(),
                key_val.clone(),
                override_id.to_string(),
            ));
        }
    }
    Ok(res)
}

fn construct_new_payload(
    req_payload: &Map<String, Value>,
) -> superposition::Result<PutRequest> {
    let mut res = req_payload.clone();
    res.remove("to_be_deleted");
    res.remove("override_id");
    res.remove("id");

    let context = res
        .get("context")
        .and_then(|val| val.as_object())
        .map_or_else(
            || {
                log::error!("construct new payload: Context not present");
                Err(bad_argument!("Context not present"))
            },
            |val| {
                Cac::<Condition>::try_from(val.to_owned()).map_err(|err| {
                    log::error!("failed to decode condition with error : {}", err);
                    bad_argument!(err)
                })
            },
        )?;

    let override_ = res
        .get("override")
        .and_then(|val| val.as_object())
        .map_or_else(
            || {
                log::error!("construct new payload Override not present");
                Err(bad_argument!("Override not present"))
            },
            |val| {
                Cac::<Overrides>::try_from(val.to_owned()).map_err(|err| {
                    log::error!("failed to decode override with error : {}", err);
                    bad_argument!(err)
                })
            },
        )?;

    let change_reason =
        ChangeReason::try_from("Context updated during reduce operation".to_string())
            .map_err(|e| unexpected_error!(e))?;

    Ok(PutRequest {
        context,
        r#override: override_,
        description: None,
        change_reason,
    })
}

#[allow(clippy::too_many_arguments)]
async fn reduce_config_key(
    user: &User,
    conn: &mut DBConnection,
    mut og_contexts: Vec<Context>,
    mut og_overrides: HashMap<String, Overrides>,
    check_key: &str,
    dimension_schema_map: &HashMap<String, DimensionInfo>,
    default_config: Map<String, Value>,
    is_approve: bool,
    workspace_context: &WorkspaceContext,
    state: &AppState,
) -> superposition::Result<Config> {
    let default_config_val =
        default_config
            .get(check_key)
            .ok_or(superposition::AppError::BadArgument(format!(
                "{} not found in default config",
                check_key
            )))?;
    let mut contexts_overrides_values = Vec::new();

    for (override_id, mut override_value) in og_overrides.clone() {
        if let Some(value_of_check_key) = override_value.remove(check_key) {
            let context_arr = get_contextids_from_overrideid(
                og_contexts.clone(),
                override_value.into(),
                value_of_check_key.clone(),
                &override_id,
            )?;
            contexts_overrides_values.extend(context_arr);
        }
    }

    let mut weights = Vec::new();

    for (index, ctx) in contexts_overrides_values.iter().enumerate() {
        let weight =
            calculate_context_weight(&json!((ctx.0).condition), dimension_schema_map)
                .map_err(|err| bad_argument!(err))?;
        weights.push((index, weight))
    }

    // Sort the collected results based on weight
    weights.sort_by(|a, b| b.1.cmp(&a.1));

    // Use the sorted indices to reorder the original vector
    let sorted_weight_contexts = weights
        .into_iter()
        .map(|(index, _)| contexts_overrides_values[index].clone())
        .collect();

    let resolved_dimensions = reduce(sorted_weight_contexts, default_config_val)?;
    for rd in resolved_dimensions {
        match (
            rd.get("can_be_reduced"),
            rd.get("req_payload"),
            rd.get("req_payload").and_then(|v| v.get("id")),
            rd.get("req_payload").and_then(|v| v.get("override_id")),
            rd.get("req_payload").and_then(|v| v.get("to_be_deleted")),
            rd.get("req_payload").and_then(|v| v.get("override")),
        ) {
            (
                Some(Value::Bool(true)),
                Some(Value::Object(request_payload)),
                Some(Value::String(cid)),
                Some(Value::String(oid)),
                Some(Value::Bool(to_be_deleted)),
                Some(Value::Object(override_val)),
            ) => {
                if *to_be_deleted {
                    if is_approve {
                        let _ = context::delete(
                            cid.clone(),
                            user,
                            conn,
                            &workspace_context.schema_name,
                        );
                    }
                    og_contexts.retain(|x| x.id != *cid);
                } else {
                    if is_approve {
                        let _ = context::delete(
                            cid.clone(),
                            user,
                            conn,
                            &workspace_context.schema_name,
                        );
                        if let Ok(put_req) = construct_new_payload(request_payload) {
                            let description = match put_req.description.clone() {
                                Some(val) => val,
                                None => query_description(
                                    Value::Object(
                                        put_req.context.clone().into_inner().into(),
                                    ),
                                    conn,
                                    &workspace_context.schema_name,
                                )?,
                            };

                            let _ = context::upsert(
                                put_req,
                                description,
                                conn,
                                false,
                                user,
                                workspace_context,
                                false,
                                &state.master_encryption_key,
                            );
                        }
                    }

                    let override_val = Cac::<Overrides>::validate_db_data(
                        override_val.clone(),
                    )
                    .map_err(|err| {
                        log::error!(
                            "reduce_config_key: failed to decode overrides from db {err}"
                        );
                        unexpected_error!(err)
                    })?
                    .into_inner();

                    let new_id =
                        context::hash(&Value::Object(override_val.clone().into()));
                    og_overrides.insert(new_id.clone(), override_val);

                    let mut ctx_index = 0;
                    let mut delete_old_oid = true;

                    for (ind, ctx) in og_contexts.iter().enumerate() {
                        if ctx.id == *cid {
                            ctx_index = ind;
                        } else if ctx.override_with_keys.contains(oid) {
                            delete_old_oid = false;
                        }
                    }

                    let mut elem = og_contexts[ctx_index].clone();
                    elem.override_with_keys = OverrideWithKeys::new(new_id);
                    og_contexts[ctx_index] = elem;

                    if delete_old_oid {
                        og_overrides.remove(oid);
                    }
                }
            }
            _ => continue,
        }
    }

    Ok(Config {
        contexts: og_contexts,
        overrides: og_overrides,
        default_configs: default_config,
        dimensions: dimension_schema_map.clone(),
    })
}

#[authorized]
#[put("/reduce")]
async fn reduce_handler(
    workspace_context: WorkspaceContext,
    req: HttpRequest,
    user: User,
    db_conn: DbConnection,
    state: Data<AppState>,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let is_approve = req
        .headers()
        .get("x-approve")
        .and_then(|value| value.to_str().ok().and_then(|s| s.parse::<bool>().ok()))
        .unwrap_or(false);

    let dimensions_info_map =
        fetch_dimensions_info_map(&mut conn, &workspace_context.schema_name)?;
    let mut config = generate_cac(&mut conn, &workspace_context.schema_name)?;
    let default_config = (config.default_configs).clone();
    for (key, _) in default_config {
        let contexts = config.contexts;
        let overrides = config.overrides;
        let default_config = config.default_configs;
        config = reduce_config_key(
            &user,
            &mut conn,
            contexts.clone(),
            overrides.clone(),
            key.as_str(),
            &dimensions_info_map,
            default_config.clone(),
            is_approve,
            &workspace_context,
            &state,
        )
        .await?;
        if is_approve {
            config = generate_cac(&mut conn, &workspace_context.schema_name)?;
        }
    }

    Ok(HttpResponse::Ok().json(config))
}

#[cfg(feature = "high-performance-mode")]
#[authorized]
#[get("/fast")]
async fn get_fast_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
) -> superposition::Result<HttpResponse> {
    use fred::interfaces::MetricsInterface;

    log::debug!("Started redis fetch");
    let config_key = format!("{}::cac_config", *workspace_context.schema_name);
    let last_modified_at_key = format!(
        "{}::cac_config::last_modified_at",
        *workspace_context.schema_name
    );
    let audit_id_key =
        format!("{}::cac_config::audit_id", *workspace_context.schema_name);
    let config_version_key = format!(
        "{}::cac_config::config_version",
        *workspace_context.schema_name
    );
    let client = state.redis.next_connected();
    let config = client.get::<String, String>(config_key).await;
    let metrics = client.take_latency_metrics();
    let network_metrics = client.take_network_latency_metrics();
    log::trace!(
        "Network metrics for config fetch in milliseconds :: max: {}, min: {}, avg: {}; Latency metrics :: max: {}, min: {}, avg: {}",
        network_metrics.max,
        network_metrics.min,
        network_metrics.avg,
        metrics.max,
        metrics.min,
        metrics.avg
    );
    match config {
        Ok(config) => {
            let mut response = HttpResponse::Ok();
            if let Ok(max_created_at) =
                client.get::<String, String>(last_modified_at_key).await
            {
                let metrics = client.take_latency_metrics();
                let network_metrics = client.take_network_latency_metrics();
                log::trace!(
                    "Network metrics max-created-by fetch in milliseconds :: max: {}, min: {}, avg: {}; Latency metrics :: max: {}, min: {}, avg: {}",
                    network_metrics.max,
                    network_metrics.min,
                    network_metrics.avg,
                    metrics.max,
                    metrics.min,
                    metrics.avg
                );
                response
                    .insert_header((AppHeader::LastModified.to_string(), max_created_at));
            }
            if let Ok(audit_id) = client.get::<String, String>(audit_id_key).await {
                let metrics = client.take_latency_metrics();
                let network_metrics = client.take_network_latency_metrics();
                log::trace!(
                    "Network metrics for audit ID in milliseconds :: max: {}, min: {}, avg: {}; Latency metrics :: max: {}, min: {}, avg: {}",
                    network_metrics.max,
                    network_metrics.min,
                    network_metrics.avg,
                    metrics.max,
                    metrics.min,
                    metrics.avg
                );
                response.insert_header((AppHeader::XAuditId.to_string(), audit_id));
            }
            if let Ok(config_version) =
                client.get::<Option<i64>, String>(config_version_key).await
            {
                let metrics = client.take_latency_metrics();
                let network_metrics = client.take_network_latency_metrics();
                log::trace!(
                    "Network metrics for version ID in milliseconds :: max: {}, min: {}, avg: {}; Latency metrics :: max: {}, min: {}, avg: {}",
                    network_metrics.max,
                    network_metrics.min,
                    network_metrics.avg,
                    metrics.max,
                    metrics.min,
                    metrics.avg
                );
                add_config_version_to_header(&config_version, &mut response);
            }
            response.insert_header(ContentType::json());
            Ok(response.body(config))
        }
        Err(err) => {
            log::error!("Could not get config in redis due to {}", err);
            Err(response_error!(
                StatusCode::INTERNAL_SERVER_ERROR,
                "could not fetch config, please try /config API"
            ))
        }
    }
}

#[authorized]
#[routes]
#[get("")]
#[post("")]
async fn get_handler(
    req: HttpRequest,
    body: Option<Json<ContextPayload>>,
    db_conn: DbConnection,
    dimension_params: DimensionQuery<QueryMap>,
    query_filters: superposition_query::Query<ConfigQuery>,
    workspace_context: WorkspaceContext,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let max_created_at = get_max_created_at(&mut conn, &workspace_context.schema_name)
        .map_err(|e| log::error!("failed to fetch max timestamp from event_log: {e}"))
        .ok();

    log::info!("Max created at: {max_created_at:?}");

    let is_not_modified = is_not_modified(max_created_at, &req);

    if is_not_modified {
        return Ok(HttpResponse::NotModified().finish());
    }

    let query_filters = query_filters.into_inner();
    let mut version = get_config_version(&query_filters.version, &workspace_context)?;

    let mut config = generate_config_from_version(
        &mut version,
        &mut conn,
        &workspace_context.schema_name,
    )?;

    config = apply_prefix_filter_to_config(&query_filters.prefix, config)?;
    let is_smithy: bool;
    let context = if req.method() == actix_web::http::Method::GET {
        is_smithy = false;
        dimension_params.into_inner()
    } else {
        // Assuming smithy.
        is_smithy = true;
        body.map_or_else(QueryMap::default, |body| body.into_inner().context.into())
    };
    if !context.is_empty() {
        config = config.filter_by_dimensions(&context);
    }

    let mut response = HttpResponse::Ok();
    add_last_modified_to_header(max_created_at, is_smithy, &mut response);
    add_audit_id_to_header(&mut conn, &mut response, &workspace_context.schema_name);
    add_config_version_to_header(&version, &mut response);
    Ok(response.json(config))
}

#[allow(clippy::too_many_arguments)]
#[authorized]
#[routes]
#[get("/resolve")]
#[post("/resolve")]
async fn resolve_handler(
    req: HttpRequest,
    body: Option<Json<ContextPayload>>,
    merge_strategy: Header<MergeStrategy>,
    db_conn: DbConnection,
    dimension_params: DimensionQuery<QueryMap>,
    query_filters: superposition_query::Query<ResolveConfigQuery>,
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let query_filters = query_filters.into_inner();

    let max_created_at = get_max_created_at(&mut conn, &workspace_context.schema_name)
        .map_err(|e| log::error!("failed to fetch max timestamp from event_log : {e}"))
        .ok();

    if is_not_modified(max_created_at, &req) {
        return Ok(HttpResponse::NotModified().finish());
    }

    let mut config_version =
        get_config_version(&query_filters.version, &workspace_context)?;
    let mut config = generate_config_from_version(
        &mut config_version,
        &mut conn,
        &workspace_context.schema_name,
    )?;
    let (is_smithy, query_data) = setup_query_data(&req, &body, &dimension_params)?;

    let resolved_config = resolve(
        &mut config,
        query_data,
        merge_strategy,
        &mut conn,
        &query_filters,
        &workspace_context,
        &state.master_encryption_key,
    )?;

    let mut resp = HttpResponse::Ok();
    add_last_modified_to_header(max_created_at, is_smithy, &mut resp);
    add_audit_id_to_header(&mut conn, &mut resp, &workspace_context.schema_name);
    add_config_version_to_header(&config_version, &mut resp);
    Ok(resp.json(resolved_config))
}

#[authorized]
#[get("/versions")]
async fn list_version_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
) -> superposition::Result<Json<PaginatedResponse<ConfigVersionListItem>>> {
    let DbConnection(mut conn) = db_conn;

    if let Some(true) = filters.all {
        let config_versions = config_versions::config_versions
            .schema_name(&workspace_context.schema_name)
            .select(ConfigVersionListItem::as_select())
            .get_results(&mut conn)?;
        return Ok(Json(PaginatedResponse::all(config_versions)));
    }

    let n_version: i64 = config_versions::config_versions
        .count()
        .schema_name(&workspace_context.schema_name)
        .get_result(&mut conn)?;

    let limit = filters.count.unwrap_or(10);
    let mut builder = config_versions::config_versions
        .schema_name(&workspace_context.schema_name)
        .into_boxed()
        .order(config_versions::created_at.desc())
        .limit(limit);
    if let Some(page) = filters.page {
        let offset = (page - 1) * limit;
        builder = builder.offset(offset);
    }
    let config_versions = builder
        .select(ConfigVersionListItem::as_select())
        .load(&mut conn)?;
    let total_pages = (n_version as f64 / limit as f64).ceil() as i64;
    Ok(Json(PaginatedResponse {
        total_pages,
        total_items: n_version,
        data: config_versions,
    }))
}

#[authorized]
#[get("/version/{version}")]
async fn get_version_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    version: Path<i64>,
) -> superposition::Result<Json<ConfigVersion>> {
    let DbConnection(mut conn) = db_conn;

    let config_version = config_versions::config_versions
        .schema_name(&workspace_context.schema_name)
        .find(version.into_inner())
        .get_result::<ConfigVersion>(&mut conn)?;

    Ok(Json(config_version))
}
