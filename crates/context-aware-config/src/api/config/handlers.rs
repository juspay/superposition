use std::collections::HashMap;

use super::types::{Config, Context};
use crate::db::schema::{
    contexts::dsl as ctxt, default_configs::dsl as def_conf, event_log::dsl as event_log,
};
use actix_http::header::{HeaderName, HeaderValue};
use actix_web::{
    error::ErrorBadRequest, get, web::Query, HttpRequest, HttpResponse, Scope,
};
use cac_client::{eval_cac, eval_cac_with_reasoning};
use chrono::{DateTime, NaiveDateTime, Timelike, Utc};
use diesel::{
    dsl::max,
    r2d2::{ConnectionManager, PooledConnection},
    ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl,
};
use experimentation_platform::api::experiments::helpers::extract_dimensions;
use serde_json::{json, Map, Value, Value::Null};
use service_utils::{
    errors::types::Error as err, helpers::ToActixErr, service::types::DbConnection,
    types as app,
};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(get)
        .service(get_resolved_config)
        .service(get_filtered_config)
}

fn add_last_modified_header(
    max_created_at: Option<NaiveDateTime>,
    mut res: HttpResponse,
) -> actix_web::Result<HttpResponse> {
    let header_name = HeaderName::from_static("last-modified");

    if let Some(ele) = max_created_at {
        let datetime_utc: DateTime<Utc> = DateTime::from_utc(ele, Utc);
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

fn is_not_modified(
    max_created_at: Option<NaiveDateTime>,
    req: &HttpRequest,
) -> anyhow::Result<bool> {
    let last_modified = req
        .headers()
        .get("If-Modified-Since")
        .and_then(|header_val| {
            let header_str = header_val.to_str().ok()?;
            DateTime::parse_from_rfc2822(header_str)
                .map(|datetime| datetime.with_timezone(&Utc).naive_utc())
                .ok()
        })
        .and_then(|t| t.with_nanosecond(0));
    Ok(max_created_at.is_some() && max_created_at <= last_modified)
}

async fn generate_cac(
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
) -> actix_web::Result<Config> {
    let contexts_vec = ctxt::contexts
        .select((ctxt::id, ctxt::value, ctxt::override_id, ctxt::override_))
        .order_by((ctxt::priority.asc(), ctxt::created_at.asc()))
        .load::<(String, Value, String, Value)>(conn)
        .map_err_to_internal_server("error getting contexts", Null)?;

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
        .map_err_to_internal_server("error getting default configs", Null)?;

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
async fn get(req: HttpRequest, db_conn: DbConnection) -> actix_web::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let max_created_at = get_max_created_at(&mut conn)
        .map_err(|e| log::error!("failed to fetch max timestamp from event_log: {e}"))
        .ok();

    let is_not_modified = is_not_modified(max_created_at, &req)
        .map_err(|e| log::error!("config not modified: {e}"));

    if let Ok(true) = is_not_modified {
        return Ok(HttpResponse::NotModified().finish());
    }

    let res = HttpResponse::Ok().json(generate_cac(&mut conn).await?);

    add_last_modified_header(max_created_at, res)
}

#[get("/resolve")]
async fn get_resolved_config(
    req: HttpRequest,
    db_conn: DbConnection,
) -> actix_web::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let params = Query::<HashMap<String, String>>::from_query(req.query_string())
        .map_err(|_| ErrorBadRequest("error getting query params"))?;

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

    let is_not_modified = is_not_modified(max_created_at, &req)
        .map_err(|e| log::error!("config not modified: {e}"));

    if let Ok(true) = is_not_modified {
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

    let response = if let Some(Value::String(_)) = query_params_map.get("show_reasoning")
    {
        HttpResponse::Ok().json(
            eval_cac_with_reasoning(
                res.default_configs,
                &cac_client_contexts,
                &res.overrides,
                &query_params_map,
            )
            .map_err_to_internal_server("cac eval failed", Null)?,
        )
    } else {
        HttpResponse::Ok().json(
            eval_cac(
                res.default_configs,
                &cac_client_contexts,
                &res.overrides,
                &query_params_map,
            )
            .map_err_to_internal_server("cac eval failed", Null)?,
        )
    };
    add_last_modified_header(max_created_at, response)
}

#[get("/filter")]
async fn get_filtered_config(
    req: HttpRequest,
    db_conn: DbConnection,
) -> actix_web::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let params = Query::<HashMap<String, String>>::from_query(req.query_string())
        .map_err(|_| ErrorBadRequest("error getting query params"))?;
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

    let filtered_context = filter_context(contexts, query_params_map)?;
    let mut filtered_overrides: Map<String, Value> = Map::new();
    for ele in filtered_context.iter() {
        let override_with_key = &ele.override_with_keys[0];
        filtered_overrides.insert(
            override_with_key.to_string(),
            config
                .overrides
                .get(override_with_key)
                .ok_or(err::InternalServerErr(
                    "Could not fetch override_key".to_string(),
                ))?
                .to_owned(),
        );
    }

    let filtered_config = Config {
        contexts: filtered_context,
        overrides: filtered_overrides,
        default_configs: config.default_configs,
    };

    Ok(HttpResponse::Ok().json(filtered_config))
}

fn filter_context(
    contexts: Vec<Context>,
    query_params_map: Map<String, Value>,
) -> app::Result<Vec<Context>> {
    let mut filtered_context: Vec<Context> = Vec::new();
    for context in contexts.into_iter() {
        if should_add_ctx(&context, &query_params_map)? {
            filtered_context.push(context);
        }
    }
    return Ok(filtered_context);
}

fn should_add_ctx(
    context: &Context,
    query_params_map: &Map<String, Value>,
) -> app::Result<bool> {
    let dimension = extract_dimensions(&context.condition)?;
    Ok(dimension
        .iter()
        .all(|(key, value)| query_params_map.get(key).map_or(true, |val| val == value)))
}
