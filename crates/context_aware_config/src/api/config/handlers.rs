use std::collections::HashSet;
use std::{collections::HashMap, str::FromStr};

use super::helpers::{
    filter_config_by_dimensions, filter_config_by_prefix, filter_context,
};

use super::types::Config;
use crate::db::schema::{
    contexts::dsl as ctxt, default_configs::dsl as def_conf, event_log::dsl as event_log,
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

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(get)
        .service(get_resolved_config)
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
