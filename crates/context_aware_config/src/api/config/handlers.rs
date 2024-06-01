use std::collections::HashSet;
use std::{collections::HashMap, str::FromStr};

use super::helpers::{
    filter_config_by_dimensions, filter_config_by_prefix, filter_context,
};
use super::types::{Config, Context};
use crate::api::context::{
    delete_context_api, hash, put, validate_dimensions_and_calculate_priority, PutReq,
};
use crate::api::dimension::get_all_dimension_schema_map;
use crate::db::schema::{
    contexts::dsl as ctxt, default_configs::dsl as def_conf, event_log::dsl as event_log,
};
use actix_http::header::{HeaderName, HeaderValue};
use actix_web::web;
use actix_web::{get, put, web::Query, HttpRequest, HttpResponse, Scope};
use cac_client::{eval_cac, eval_cac_with_reasoning, MergeStrategy};
use chrono::{DateTime, NaiveDateTime, TimeZone, Timelike, Utc};
use diesel::{
    dsl::max,
    r2d2::{ConnectionManager, PooledConnection},
    ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
};
use serde_json::{json, Map, Value};
use service_utils::service::types::DbConnection;
use service_utils::{bad_argument, db_error, unexpected_error};

use itertools::Itertools;
use jsonschema::JSONSchema;
use service_utils::helpers::extract_dimensions;
use service_utils::result::{self as superposition, AppError};
use superposition_types::User;
use uuid::Uuid;
pub fn endpoints() -> Scope {
    Scope::new("")
        .service(get)
        .service(get_resolved_config)
        .service(reduce_config)
        .service(get_filtered_config)
}

pub fn add_audit_header(
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    mut res: HttpResponse,
) -> superposition::Result<HttpResponse> {
    let header_name = HeaderName::from_static("x-audit-id");
    if let Ok(uuid) = event_log::event_log
        .select(event_log::id)
        .filter(event_log::table_name.eq("contexts"))
        .order_by(event_log::timestamp.desc())
        .first::<Uuid>(conn)
    {
        let uuid_string = uuid.to_string();
        if let Ok(header_value) = HeaderValue::from_str(&uuid_string) {
            res.headers_mut().insert(header_name, header_value);
        } else {
            log::error!("Failed to convert UUID to string");
        }
    } else {
        log::error!("Failed to fetch contexts from event_log");
    }
    Ok(res)
}

fn add_last_modified_header(
    max_created_at: Option<NaiveDateTime>,
    mut res: HttpResponse,
) -> superposition::Result<HttpResponse> {
    let header_name = HeaderName::from_static("last-modified");

    if let Some(ele) = max_created_at {
        let datetime_utc: DateTime<Utc> = TimeZone::from_utc_datetime(&Utc, &ele);
        let value = HeaderValue::from_str(&DateTime::to_rfc2822(&datetime_utc));
        if let Ok(header_value) = value {
            res.headers_mut().insert(header_name, header_value);
        }
    }
    Ok(res)
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

async fn generate_cac(
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> superposition::Result<Config> {
    let contexts_vec = ctxt::contexts
        .select((ctxt::id, ctxt::value, ctxt::override_id, ctxt::override_))
        .order_by((ctxt::priority.asc(), ctxt::created_at.asc()))
        .load::<(String, Value, String, Value)>(conn)
        .map_err(|err| {
            log::error!("failed to fetch contexts with error: {}", err);
            db_error!(err)
        })?;

    let (contexts, overrides) = contexts_vec.into_iter().fold(
        (Vec::new(), Map::new()),
        |(mut ctxts, mut overrides), (id, condition, override_id, override_)| {
            let ctxt = super::types::Context {
                id,
                condition,
                override_with_keys: [override_id.to_owned()],
            };
            ctxts.push(ctxt);
            overrides.insert(override_id, override_);
            (ctxts, overrides)
        },
    );

    let default_config_vec = def_conf::default_configs
        .select((def_conf::key, def_conf::value))
        .load::<(String, Value)>(conn)
        .map_err(|err| {
            log::error!("failed to fetch default_configs with error: {}", err);
            db_error!(err)
        })?;

    let default_configs =
        default_config_vec
            .into_iter()
            .fold(Map::new(), |mut acc, item| {
                acc.insert(item.0, item.1);
                acc
            });

    Ok(Config {
        contexts,
        overrides,
        default_configs,
    })
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

    We have also sorted this dimensions vector in descending order based on the priority of the dimensions in that context
    and in this vector the default config will be at the end of the list as it has no dimensions and it's priority is the least

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
        Here "before" means the element with higher priority comes first with a subset of c1 but differnt override value for the key
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

fn construct_new_payload(req_payload: &Map<String, Value>) -> web::Json<PutReq> {
    let mut res = req_payload.clone();
    res.remove("to_be_deleted");
    res.remove("override_id");
    res.remove("id");
    if let Some(Value::Object(res_context)) = res.get("context") {
        if let Some(Value::Object(res_override)) = res.get("override") {
            return web::Json(PutReq {
                context: res_context.to_owned(),
                r#override: res_override.to_owned(),
            });
        }
    }
    web::Json(PutReq {
        context: Map::new(),
        r#override: Map::new(),
    })
}

#[allow(clippy::too_many_arguments)]
async fn reduce_config_key(
    user: User,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    mut og_contexts: Vec<Context>,
    mut og_overrides: Map<String, Value>,
    check_key: &str,
    dimension_schema_map: &HashMap<String, (JSONSchema, i32)>,
    default_config: Map<String, Value>,
    is_approve: bool,
) -> superposition::Result<Config> {
    let default_config_val =
        default_config
            .get(check_key)
            .ok_or(AppError::BadArgument(format!(
                "{} not found in default config",
                check_key
            )))?;
    let mut contexts_overrides_values = Vec::new();

    for (override_id, override_value) in og_overrides.clone() {
        if let Value::Object(mut override_obj) = override_value {
            if let Some(value_of_check_key) = override_obj.remove(check_key) {
                let context_arr = get_contextids_from_overrideid(
                    og_contexts.clone(),
                    override_obj,
                    value_of_check_key.clone(),
                    &override_id,
                )?;
                contexts_overrides_values.extend(context_arr);
            }
        }
    }

    let mut priorities = Vec::new();

    for (index, ctx) in contexts_overrides_values.iter().enumerate() {
        let priority = validate_dimensions_and_calculate_priority(
            "context",
            &(ctx.0).condition,
            dimension_schema_map,
        )?;
        priorities.push((index, priority))
    }

    // Sort the collected results based on priority
    priorities.sort_by(|a, b| b.1.cmp(&a.1));

    // Use the sorted indices to reorder the original vector
    let sorted_priority_contexts = priorities
        .into_iter()
        .map(|(index, _)| contexts_overrides_values[index].clone())
        .collect();

    let resolved_dimensions = reduce(sorted_priority_contexts, default_config_val)?;
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
                Some(override_val),
            ) => {
                if *to_be_deleted {
                    if is_approve {
                        let _ = delete_context_api(cid.clone(), user.clone(), conn).await;
                    }
                    og_contexts.retain(|x| x.id != *cid);
                } else {
                    if is_approve {
                        let _ = delete_context_api(cid.clone(), user.clone(), conn).await;
                        let put_req = construct_new_payload(request_payload);
                        let _ = put(put_req, conn, false, &user);
                    }

                    let new_id = hash(override_val);
                    og_overrides.insert(new_id.clone(), override_val.clone());

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
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let is_approve = req
        .headers()
        .get("x-approve")
        .and_then(|value| value.to_str().ok().and_then(|s| s.parse::<bool>().ok()))
        .unwrap_or(false);

    let dimensions_schema_map = get_all_dimension_schema_map(&mut conn)?;
    let mut config = generate_cac(&mut conn).await?;
    let default_config = (config.default_configs).clone();
    for (key, _) in default_config {
        let contexts = config.contexts;
        let overrides = config.overrides;
        let default_config = config.default_configs;
        config = reduce_config_key(
            user.clone(),
            &mut conn,
            contexts.clone(),
            overrides.clone(),
            key.as_str(),
            &dimensions_schema_map,
            default_config.clone(),
            is_approve,
        )
        .await?;
        if is_approve {
            config = generate_cac(&mut conn).await?;
        }
    }

    Ok(HttpResponse::Ok().json(config))
}

#[get("")]
async fn get(
    req: HttpRequest,
    db_conn: DbConnection,
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

    let params = Query::<HashMap<String, String>>::from_query(req.query_string())
        .map_err(|err| {
            log::error!("Failed to parse query params with err: {}", err);
            bad_argument!("Unable to retrieve query parameters.")
        })?;
    let mut query_params_map: serde_json::Map<String, Value> = Map::new();

    for (key, value) in params.0.into_iter() {
        query_params_map.insert(
            key,
            value
                .parse::<i32>()
                .map_or_else(|_| json!(value), |int_val| json!(int_val)),
        );
    }

    let mut config = generate_cac(&mut conn).await?;
    if let Some(prefix) = query_params_map.get("prefix") {
        let prefix_list: HashSet<&str> = prefix
            .as_str()
            .ok_or_else(|| {
                log::error!("Prefix is not a valid string.");
                bad_argument!("Prefix is not a valid string")
            })?
            .split(',')
            .collect();
        config = filter_config_by_prefix(&config, &prefix_list)?
    }

    query_params_map.remove("prefix");

    if !query_params_map.is_empty() {
        config = filter_config_by_dimensions(&config, &query_params_map)?
    }

    let resp = HttpResponse::Ok().json(config);
    let audit_resp = add_audit_header(&mut conn, resp)?;

    add_last_modified_header(max_created_at, audit_resp)
}

#[get("/resolve")]
async fn get_resolved_config(
    req: HttpRequest,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let params = Query::<HashMap<String, String>>::from_query(req.query_string())
        .map_err(|err| {
            log::error!("failed to parse query params with err: {}", err);
            bad_argument!("error getting query params")
        })?;

    let mut query_params_map: serde_json::Map<String, Value> = Map::new();

    for item in params.0.into_iter() {
        query_params_map.insert(
            item.0,
            item.1
                .parse::<i32>()
                .map_or_else(|_| json!(item.1), |int_val| json!(int_val)),
        );
    }

    let max_created_at = get_max_created_at(&mut conn)
        .map_err(|e| log::error!("failed to fetch max timestamp from event_log : {e}"))
        .ok();

    let is_not_modified = is_not_modified(max_created_at, &req);

    if is_not_modified {
        return Ok(HttpResponse::NotModified().finish());
    }

    let res = generate_cac(&mut conn).await?;

    let cac_client_contexts = res
        .contexts
        .into_iter()
        .map(|val| cac_client::Context {
            condition: val.condition,
            override_with_keys: val.override_with_keys,
        })
        .collect::<Vec<_>>();

    let merge_strategy = req
        .headers()
        .get("x-merge-strategy")
        .and_then(|header_value: &HeaderValue| header_value.to_str().ok())
        .and_then(|val| MergeStrategy::from_str(val).ok())
        .unwrap_or_default();

    let response = if let Some(Value::String(_)) = query_params_map.get("show_reasoning")
    {
        HttpResponse::Ok().json(
            eval_cac_with_reasoning(
                res.default_configs,
                &cac_client_contexts,
                &res.overrides,
                &query_params_map,
                merge_strategy,
            )
            .map_err(|err| {
                log::error!("failed to eval cac with err: {}", err);
                unexpected_error!("cac eval failed")
            })?,
        )
    } else {
        HttpResponse::Ok().json(
            eval_cac(
                res.default_configs,
                &cac_client_contexts,
                &res.overrides,
                &query_params_map,
                merge_strategy,
            )
            .map_err(|err| {
                log::error!("failed to eval cac with err: {}", err);
                unexpected_error!("cac eval failed")
            })?,
        )
    };
    let audit_resp = add_audit_header(&mut conn, response)?;
    add_last_modified_header(max_created_at, audit_resp)
}

#[get("/filter")]
async fn get_filtered_config(
    req: HttpRequest,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let params = Query::<HashMap<String, String>>::from_query(req.query_string())
        .map_err(|err| {
            log::error!("failed to parse query params with err: {}", err);
            bad_argument!("Error getting query params.")
        })?;
    let mut query_params_map: serde_json::Map<String, Value> = Map::new();

    for (key, value) in params.0.into_iter() {
        query_params_map.insert(
            key,
            value
                .parse::<i32>()
                .map_or_else(|_| json!(value), |int_val| json!(int_val)),
        );
    }
    let config = generate_cac(&mut conn).await?;
    let contexts = config.contexts;

    let filtered_context = filter_context(&contexts, &query_params_map)?;
    let mut filtered_overrides: Map<String, Value> = Map::new();
    for ele in filtered_context.iter() {
        let override_with_key = &ele.override_with_keys[0];
        filtered_overrides.insert(
            override_with_key.to_string(),
            config
                .overrides
                .get(override_with_key)
                .ok_or_else(|| {
                    log::error!("Could not fetch override_with_key");
                    unexpected_error!("Something went wrong")
                })?
                .to_owned(),
        );
    }

    let filtered_config = Config {
        contexts: filtered_context,
        overrides: filtered_overrides,
        default_configs: config.default_configs,
    };

    add_audit_header(&mut conn, HttpResponse::Ok().json(filtered_config))
}
