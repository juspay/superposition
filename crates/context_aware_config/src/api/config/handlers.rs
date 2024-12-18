use std::{collections::HashMap, str::FromStr};

use actix_http::header::HeaderValue;
#[cfg(feature = "high-performance-mode")]
use actix_http::StatusCode;
#[cfg(feature = "high-performance-mode")]
use actix_web::http::header::ContentType;
#[cfg(feature = "high-performance-mode")]
use actix_web::web::Data;
use actix_web::{
    get, put,
    web::{self, Json, Query},
    HttpRequest, HttpResponse, HttpResponseBuilder, Scope,
};
use cac_client::{eval_cac, eval_cac_with_reasoning, MergeStrategy};
use chrono::{DateTime, NaiveDateTime, TimeZone, Timelike, Utc};
use diesel::{
    dsl::max,
    r2d2::{ConnectionManager, PooledConnection},
    ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
};
#[cfg(feature = "high-performance-mode")]
use fred::interfaces::KeysInterface;
use itertools::Itertools;
use serde_json::{json, Map, Value};
#[cfg(feature = "high-performance-mode")]
use service_utils::service::types::{AppState, Tenant};
use service_utils::{
    helpers::extract_dimensions,
    service::types::{AppHeader, DbConnection},
};
#[cfg(feature = "high-performance-mode")]
use superposition_macros::response_error;
use superposition_macros::{bad_argument, db_error, unexpected_error};
use superposition_types::{
    custom_query::{
        self as superposition_query, CustomQuery, PaginationParams, QueryMap,
    },
    database::{
        models::cac::ConfigVersion,
        schema::{config_versions::dsl as config_versions, event_log::dsl as event_log},
    },
    result as superposition, Cac, Condition, Config, Context, Overrides,
    PaginatedResponse, TenantConfig, User,
};
use uuid::Uuid;

use crate::helpers::generate_cac;
use crate::{
    api::context::{delete_context_api, hash, put, PutReq},
    helpers::DimensionData,
};
use crate::{
    api::dimension::{get_dimension_data, get_dimension_data_map},
    helpers::calculate_context_weight,
};

use super::helpers::apply_prefix_filter_to_config;

pub fn endpoints() -> Scope {
    let scope = Scope::new("")
        .service(get_config)
        .service(get_resolved_config)
        .service(reduce_config)
        .service(get_config_versions);
    #[cfg(feature = "high-performance-mode")]
    let scope = scope.service(get_config_fast);
    scope
}

fn validate_version_in_params(
    query_params_map: &mut Map<String, Value>,
) -> superposition::Result<Option<i64>> {
    query_params_map
        .remove("version")
        .map_or(Ok(None), |version| {
            version.as_i64().map_or_else(
                || {
                    log::error!("failed to decode version as integer: {}", version);
                    Err(bad_argument!("version is not of type integer"))
                },
                |v| Ok(Some(v)),
            )
        })
}

pub fn add_audit_id_to_header(
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    resp_builder: &mut HttpResponseBuilder,
) {
    if let Ok(uuid) = event_log::event_log
        .select(event_log::id)
        .filter(event_log::table_name.eq("contexts"))
        .order_by(event_log::timestamp.desc())
        .first::<Uuid>(conn)
    {
        resp_builder.insert_header((AppHeader::XAuditId.to_string(), uuid.to_string()));
    } else {
        log::error!("Failed to fetch contexts from event_log");
    }
}

fn add_last_modified_to_header(
    max_created_at: Option<NaiveDateTime>,
    resp_builder: &mut HttpResponseBuilder,
) {
    if let Some(ele) = max_created_at {
        let datetime_utc: DateTime<Utc> = TimeZone::from_utc_datetime(&Utc, &ele);
        let value = HeaderValue::from_str(&DateTime::to_rfc2822(&datetime_utc));
        if let Ok(header_value) = value {
            resp_builder
                .insert_header((AppHeader::LastModified.to_string(), header_value));
        } else {
            log::error!("failed parsing datetime_utc {:?}", value);
        }
    }
}

fn add_config_version_to_header(
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

fn get_max_created_at(
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> Result<NaiveDateTime, diesel::result::Error> {
    event_log::event_log
        .select(max(event_log::timestamp))
        .filter(event_log::table_name.eq_any(vec!["contexts", "default_configs"]))
        .first::<Option<NaiveDateTime>>(conn)
        .and_then(|res| res.ok_or(diesel::result::Error::NotFound))
}

fn is_not_modified(max_created_at: Option<NaiveDateTime>, req: &HttpRequest) -> bool {
    let nanosecond_erasure = |t: NaiveDateTime| t.with_nanosecond(0);
    let last_modified = req
        .headers()
        .get("If-Modified-Since")
        .and_then(|header_val| {
            let header_str = header_val.to_str().ok()?;
            DateTime::parse_from_rfc2822(header_str)
                .map(|datetime| datetime.with_timezone(&Utc).naive_utc())
                .ok()
        })
        .and_then(nanosecond_erasure);
    log::info!("last modified {last_modified:?}");
    let parsed_max: Option<NaiveDateTime> = max_created_at.and_then(nanosecond_erasure);
    max_created_at.is_some() && parsed_max <= last_modified
}

pub fn generate_config_from_version(
    version: &mut Option<i64>,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<Config> {
    if let Some(val) = version {
        let val = val.clone();
        let config = config_versions::config_versions
            .select(config_versions::config)
            .filter(config_versions::id.eq(val))
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
            .first::<(i64, Value)>(conn)
        {
            Ok((latest_version, config)) => {
                *version = Some(latest_version);
                serde_json::from_value::<Config>(config).or_else(|err| {
                    log::error!("failed to decode config: {}", err);
                    generate_cac(conn)
                })
            }
            Err(err) => {
                log::error!("failed to find latest config: {err}");
                generate_cac(conn)
            }
        }
    }
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
        let mut ct_dimensions = extract_dimensions(&context.condition)?;
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

fn get_contextids_from_overrideid(
    contexts: Vec<Context>,
    overrides: Map<String, Value>,
    key_val: Value,
    override_id: &str,
) -> superposition::Result<Vec<(Context, Map<String, Value>, Value, String)>> {
    let mut res: Vec<(Context, Map<String, Value>, Value, String)> = Vec::new();
    for ct in contexts {
        let ct_dimensions = extract_dimensions(&ct.condition)?;
        if ct_dimensions.contains_key("variantIds") {
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
) -> superposition::Result<web::Json<PutReq>> {
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

    return Ok(web::Json(PutReq {
        context: context,
        r#override: override_,
    }));
}

#[allow(clippy::too_many_arguments)]
async fn reduce_config_key(
    user: User,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    tenant_config: &TenantConfig,
    mut og_contexts: Vec<Context>,
    mut og_overrides: HashMap<String, Overrides>,
    check_key: &str,
    dimension_schema_map: &HashMap<String, DimensionData>,
    default_config: Map<String, Value>,
    is_approve: bool,
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
                let override_val =
                    Cac::<Overrides>::validate_db_data(override_val.clone())
                        .map_err(|err| {
                            log::error!(
                        "reduce_config_key: failed to decode overrides from db {err}"
                    );
                            unexpected_error!(err)
                        })?
                        .into_inner();

                if *to_be_deleted {
                    if is_approve {
                        let _ = delete_context_api(cid.clone(), user.clone(), conn);
                    }
                    og_contexts.retain(|x| x.id != *cid);
                } else {
                    if is_approve {
                        let _ = delete_context_api(cid.clone(), user.clone(), conn);
                        if let Ok(put_req) = construct_new_payload(request_payload) {
                            let _ =
                                put(put_req, conn, false, &user, &tenant_config, false);
                        }
                    }

                    let new_id = hash(&Value::Object(override_val.clone().into()));
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
                    elem.override_with_keys = [new_id];
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
    })
}

#[put("/reduce")]
async fn reduce_config(
    req: HttpRequest,
    user: User,
    db_conn: DbConnection,
    tenant_config: TenantConfig,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let is_approve = req
        .headers()
        .get("x-approve")
        .and_then(|value| value.to_str().ok().and_then(|s| s.parse::<bool>().ok()))
        .unwrap_or(false);

    let dimensions_vec = get_dimension_data(&mut conn)?;
    let dimensions_data_map = get_dimension_data_map(&dimensions_vec)?;
    let mut config = generate_cac(&mut conn)?;
    let default_config = (config.default_configs).clone();
    for (key, _) in default_config {
        let contexts = config.contexts;
        let overrides = config.overrides;
        let default_config = config.default_configs;
        config = reduce_config_key(
            user.clone(),
            &mut conn,
            &tenant_config,
            contexts.clone(),
            overrides.clone(),
            key.as_str(),
            &dimensions_data_map,
            default_config.clone(),
            is_approve,
        )
        .await?;
        if is_approve {
            config = generate_cac(&mut conn)?;
        }
    }

    Ok(HttpResponse::Ok().json(config))
}

#[cfg(feature = "high-performance-mode")]
#[get("/fast")]
async fn get_config_fast(
    tenant: Tenant,
    state: Data<AppState>,
) -> superposition::Result<HttpResponse> {
    use fred::interfaces::MetricsInterface;

    log::debug!("Started redis fetch");
    let config_key = format!("{}::cac_config", *tenant);
    let last_modified_at_key = format!("{}::cac_config::last_modified_at", *tenant);
    let audit_id_key = format!("{}::cac_config::audit_id", *tenant);
    let config_version_key = format!("{}::cac_config::config_version", *tenant);
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

#[get("")]
async fn get_config(
    req: HttpRequest,
    db_conn: DbConnection,
    query_map: superposition_query::Query<QueryMap>,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let max_created_at = get_max_created_at(&mut conn)
        .map_err(|e| log::error!("failed to fetch max timestamp from event_log: {e}"))
        .ok();

    log::info!("Max created at: {max_created_at:?}");

    let is_not_modified = is_not_modified(max_created_at, &req);

    if is_not_modified {
        return Ok(HttpResponse::NotModified().finish());
    }

    let mut query_params_map = query_map.into_inner();
    let mut config_version = validate_version_in_params(&mut query_params_map)?;
    let mut config = generate_config_from_version(&mut config_version, &mut conn)?;

    config = apply_prefix_filter_to_config(&mut query_params_map, config)?;

    if !query_params_map.is_empty() {
        config = config.filter_by_dimensions(&query_params_map)
    }

    let mut response = HttpResponse::Ok();
    add_last_modified_to_header(max_created_at, &mut response);
    add_audit_id_to_header(&mut conn, &mut response);
    add_config_version_to_header(&config_version, &mut response);
    Ok(response.json(config))
}

#[get("/resolve")]
async fn get_resolved_config(
    req: HttpRequest,
    db_conn: DbConnection,
    query_map: superposition_query::Query<QueryMap>,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let mut query_params_map = query_map.into_inner();

    let max_created_at = get_max_created_at(&mut conn)
        .map_err(|e| log::error!("failed to fetch max timestamp from event_log : {e}"))
        .ok();

    let is_not_modified = is_not_modified(max_created_at, &req);

    if is_not_modified {
        return Ok(HttpResponse::NotModified().finish());
    }

    let mut config_version = validate_version_in_params(&mut query_params_map)?;
    let mut config = generate_config_from_version(&mut config_version, &mut conn)?;

    config = apply_prefix_filter_to_config(&mut query_params_map, config)?;

    let merge_strategy = req
        .headers()
        .get("x-merge-strategy")
        .and_then(|header_value: &HeaderValue| header_value.to_str().ok())
        .and_then(|val| MergeStrategy::from_str(val).ok())
        .unwrap_or_default();

    let mut override_map = HashMap::new();
    for (key, val) in config.overrides {
        override_map.insert(key, val);
    }

    let response = if let Some(Value::String(_)) = query_params_map.get("show_reasoning")
    {
        eval_cac_with_reasoning(
            config.default_configs,
            &config.contexts,
            &override_map,
            &query_params_map,
            merge_strategy,
        )
        .map_err(|err| {
            log::error!("failed to eval cac with err: {}", err);
            unexpected_error!("cac eval failed")
        })?
    } else {
        eval_cac(
            config.default_configs,
            &config.contexts,
            &override_map,
            &query_params_map,
            merge_strategy,
        )
        .map_err(|err| {
            log::error!("failed to eval cac with err: {}", err);
            unexpected_error!("cac eval failed")
        })?
    };
    let mut resp = HttpResponse::Ok();
    add_last_modified_to_header(max_created_at, &mut resp);
    add_audit_id_to_header(&mut conn, &mut resp);
    add_config_version_to_header(&config_version, &mut resp);

    Ok(resp.json(response))
}

#[get("/versions")]
async fn get_config_versions(
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
) -> superposition::Result<Json<PaginatedResponse<ConfigVersion>>> {
    let DbConnection(mut conn) = db_conn;

    if let Some(true) = filters.all {
        let config_versions: Vec<ConfigVersion> =
            config_versions::config_versions.get_results(&mut conn)?;
        return Ok(Json(PaginatedResponse {
            total_pages: 1,
            total_items: config_versions.len() as i64,
            data: config_versions,
        }));
    }

    let n_version: i64 = config_versions::config_versions
        .count()
        .get_result(&mut conn)?;

    let limit = filters.count.unwrap_or(10);
    let mut builder = config_versions::config_versions
        .into_boxed()
        .order(config_versions::created_at.desc())
        .limit(limit);
    if let Some(page) = filters.page {
        let offset = (page - 1) * limit;
        builder = builder.offset(offset);
    }
    let config_versions: Vec<ConfigVersion> = builder.load(&mut conn)?;
    let total_pages = (n_version as f64 / limit as f64).ceil() as i64;
    Ok(Json(PaginatedResponse {
        total_pages,
        total_items: n_version,
        data: config_versions,
    }))
}
