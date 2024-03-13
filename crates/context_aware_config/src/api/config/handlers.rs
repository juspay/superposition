use std::collections::HashSet;
use std::{collections::HashMap, str::FromStr};

use super::helpers::{
    filter_config_by_dimensions, filter_config_by_prefix, filter_context,
};
use super::types::{Config, Context};
use crate::api::context::validate_dimensions_and_calculate_priority;
use crate::api::dimension::get_all_dimension_schema_map;
use crate::{
    db::schema::{
        contexts::dsl as ctxt, default_configs::dsl as def_conf,
        event_log::dsl as event_log,
    },
    helpers::json_to_sorted_string,
};
use actix_http::header::{HeaderName, HeaderValue};
use actix_web::{get, web::Query, HttpRequest, HttpResponse, Scope};
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

use service_utils::result as superposition;
use uuid::Uuid;
use itertools::Itertools;
use jsonschema::JSONSchema;
use service_utils::helpers::extract_dimensions;

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(get)
        .service(get_resolved_config)
        .service(reduce_context)
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

// and and ==

fn generate_subsets(map: &Map<String, Value>) -> Vec<Map<String, Value>> {
    let mut subsets = Vec::new();
    let keys: Vec<String> = map.keys().cloned().collect_vec();
    let all_subsets_keys = generate_subsets_keys(keys);
    // println!("All Subset Keys {:#?}",all_subsets_keys);

    for subset_keys in &all_subsets_keys {
        if subset_keys.len() >= 0 {
            let mut subset_map = Map::new();

            for key in subset_keys {
                if let Some(value) = map.get(key) {
                    subset_map.insert(key.to_string(), value.clone());
                }
            }

            subsets.push(subset_map);
        }
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

fn resolve(
    contexts_overrides_values: Vec<(Context, Map<String, Value>, Value, String)>,
    default_config_val: Option<&Value>,
) -> Vec<Map<String, Value>> {
    let mut dimensions: Vec<Map<String, Value>> = Vec::new();
    println!("Saurav key_val is {:?}", default_config_val);
    for (context, overrides, key_val, override_id) in contexts_overrides_values {
        let mut ct_dimensions = extract_dimensions(&context.condition).unwrap();
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
    if let Some(some_default_config_val) = default_config_val {
        default_config_map
            .insert("key_val".to_string(), some_default_config_val.to_owned());
    };
    dimensions.push(default_config_map);

    for (index1, c1) in dimensions.clone().iter().enumerate() {
        let mut temp_c1 = c1.clone();
        let mut nc1 = c1.clone();
        nc1.remove("req_payload");
        let c1_val = nc1.remove("key_val");
        let all_subsets_c1 = generate_subsets(&nc1);
        let mut can_be_removed = false;
        for (index2, c2) in dimensions.iter().enumerate() {
            let mut temp_c2 = c2.clone();
            temp_c2.remove("req_payload");
            let c2_val = temp_c2.remove("key_val");
            if index2 != index1 {
                if all_subsets_c1.contains(&temp_c2) {
                    if c1_val == c2_val {
                        println!("Matched one is : {:#?}", c2);
                        can_be_removed = true;
                        break;
                    } else if c2_val.is_some() {
                        println!("There is a c2 which has different value : {:#?} {:#?} {:#?} {:#?}",c2, temp_c2,c1_val,c2_val);
                        break;
                    }
                }
            }
        }
        if can_be_removed {
            if let Some(req_payload) = temp_c1.get("req_payload") {
                let mut t = Map::new();
                t.insert("req_payload".to_string(), req_payload.clone());
                temp_c1 = t;
            };
        }
        dimensions[index1] = temp_c1;
    }
    dimensions
}

fn get_contextids_from_overrideid(
    contexts: Vec<Context>,
    overrides: Map<String, Value>,
    key_val: Value,
    override_id: &str,
) -> Vec<(Context, Map<String, Value>, Value, String)> {
    let mut res: Vec<(Context, Map<String, Value>, Value, String)> = Vec::new();
    for ct in contexts {
        let ct_dimensions = extract_dimensions(&ct.condition).unwrap();
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
    res
}

async fn reduce_context_key(
    mut og_contexts: Vec<Context>,
    mut og_overrides: Map<String, Value>,
    check_key: &str,
    dimension_schema_map: &HashMap<String, (JSONSchema, i32)>,
    default_config: Map<String, Value>,
) -> Config {
    let default_config_val = default_config.get(check_key);
    let mut contexts_overrides_values: Vec<(Context, Map<String, Value>, Value, String)> =
        Vec::new();

    for (key, val) in og_overrides.clone() {
        match val {
            Value::Object(obj_val) => {
                if let Some(ans_val) = obj_val.get(check_key) {
                    let mut temp_obj_val = obj_val.clone();
                    temp_obj_val.remove(check_key);
                    let context_arr = get_contextids_from_overrideid(
                        og_contexts.clone(),
                        temp_obj_val,
                        ans_val.clone(),
                        &key,
                    );
                    contexts_overrides_values.extend(context_arr);
                }
            }
            _ => (),
        }
    }

    contexts_overrides_values.sort_by(|a, b| {
        (validate_dimensions_and_calculate_priority(
            "context",
            &(b.0).condition,
            dimension_schema_map,
        )
        .unwrap())
        .cmp(
            &(validate_dimensions_and_calculate_priority(
                "context",
                &(a.0).condition,
                dimension_schema_map,
            )
            .unwrap()),
        )
    });

    let resolved_dimensions = resolve(contexts_overrides_values, default_config_val);
    for rd in resolved_dimensions {
        if rd.len() == 1 {
            if let Some(Value::Object(request_payload)) = rd.get("req_payload") {
                if let Some(Value::String(cid)) = request_payload.get("id") {
                    if let Some(Value::String(oid)) = request_payload.get("override_id") {
                        if let Some(Value::Bool(to_be_deleted)) =
                            request_payload.get("to_be_deleted")
                        {
                            // After extracting values
                            if *to_be_deleted {
                                // remove cid from contexts
                                println!(
                                    "Saurav is removing this context {:#?}",
                                    rd.get("context")
                                );
                                og_contexts.retain(|x| x.id != *cid);
                            } else {
                                //update the override by removing the key
                                if let Some(override_val) =
                                    request_payload.get("override")
                                {
                                    println!("Saurav is updating this context {:#?} by removing {:?} from it's override",rd.get("context"), check_key);
                                    let new_id = hash(override_val);
                                    og_overrides
                                        .insert(new_id.clone(), override_val.to_owned());

                                    // the below thing is not necessary if we just do delete and update context api
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
                        }
                    }
                }
            }
        }
    }

    Config {
        contexts: og_contexts,
        overrides: og_overrides,
        default_configs: default_config,
    }
}

fn hash(val: &Value) -> String {
    let sorted_str: String = json_to_sorted_string(val);
    blake3::hash(sorted_str.as_bytes()).to_string()
}

#[get("/reduce")]
async fn reduce_context(
    req: HttpRequest,
    db_conn: DbConnection,
) -> actix_web::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let dimensions_schema_map = get_all_dimension_schema_map(&mut conn).unwrap();
    let mut config = generate_cac(&mut conn).await?;
    let default_config = (config.default_configs).clone();
    for (key, val) in default_config {
        let contexts = config.contexts;
        let overrides = config.overrides;
        let default_config = config.default_configs;
        config = reduce_context_key(
            contexts.clone(),
            overrides.clone(),
            key.as_str(),
            &dimensions_schema_map,
            default_config.clone(),
        )
        .await;
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
            .split(",")
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
        .collect();

    let merge_strategy = req
        .headers()
        .get("x-merge-strategy")
        .and_then(|header_value: &HeaderValue| header_value.to_str().ok())
        .and_then(|val| MergeStrategy::from_str(val).ok())
        .unwrap_or(MergeStrategy::default());

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
