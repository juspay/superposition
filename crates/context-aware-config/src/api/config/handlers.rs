use std::collections::HashMap;

use super::types::Config;
use crate::db::schema::cac_v1::{
    contexts::dsl as ctxt, default_configs::dsl as def_conf, event_log::dsl as event_log,
};
use actix_web::{error::ErrorBadRequest, get, web::Query, HttpRequest, HttpResponse, Scope};
use chrono::{DateTime, NaiveDateTime, Utc};
use diesel::{dsl::max, ExpressionMethods, QueryDsl, RunQueryDsl, r2d2::{PooledConnection, ConnectionManager}, PgConnection};
use serde_json::{json, Map, Value, Value::Null};
use service_utils::{helpers::ToActixErr, service::types::DbConnection};
use cac_client::eval_cac;

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(get)
        .service(get_resolved_config)
}


fn is_not_modified (req: &HttpRequest, conn: &mut PooledConnection<ConnectionManager<PgConnection>>) -> actix_web::Result<bool> {
    let max_created_at: Option<NaiveDateTime> = event_log::event_log
        .select(max(event_log::timestamp))
        .filter(event_log::table_name.eq("contexts"))
        .first(conn)
        .map_err_to_internal_server("error getting created at", Null)?;

    let last_modified = req
        .headers()
        .get("If-Modified-Since")
        .and_then(|header_val| {
            let header_str = header_val.to_str().ok()?;
            DateTime::parse_from_rfc2822(header_str)
                .map(|datetime| datetime.with_timezone(&Utc).naive_utc())
                .ok()
        });

    Ok(max_created_at.is_some() && max_created_at < last_modified)
}

async fn generate_cac(conn: &mut PooledConnection<ConnectionManager<PgConnection>>) -> actix_web::Result<Config> {

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

    if is_not_modified(&req, &mut conn)? {
        return Ok(HttpResponse::NotModified().finish());
    }

    Ok(
        HttpResponse::Ok()
        .json(
            generate_cac(&mut conn)
            .await?
        )
    )
}

#[get("/resolve")]
async fn get_resolved_config(req: HttpRequest, db_conn: DbConnection) -> actix_web::Result<HttpResponse> {

    let DbConnection(mut conn) = db_conn;
    let params =
        Query::<HashMap<String, String>>::from_query(req.query_string())
        .map_err(|_| ErrorBadRequest("error getting query params"))?;

    let mut query_params_map = Map::new();

    for item in params.0.into_iter() {
        query_params_map.insert(
            item.0,
            item.1
            .parse::<i32>()
            .map_or_else(
                |_| json!(item.1),
                |int_val| json!(int_val)
            )
        );
    }

    if is_not_modified(&req, &mut conn)? {
        return Ok(HttpResponse::NotModified().finish());
    }

    let res = generate_cac(&mut conn).await?;

    let cac_client_contexts =
        res.contexts.into_iter().map(|val| cac_client::Context {
            condition: val.condition,
            override_with_keys: val.override_with_keys
        }).collect();


    Ok(HttpResponse::Ok()
        .json(
            eval_cac(
                json!(res.default_configs),
                &cac_client_contexts,
                &res.overrides,
                &json!(query_params_map),
            ).map_err_to_internal_server("cac eval failed", Null)?
        )
    )
}
