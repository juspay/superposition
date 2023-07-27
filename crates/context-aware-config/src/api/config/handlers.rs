use super::types::Config;
use crate::db::schema::cac_v1::{
    contexts::dsl as ctxt, default_configs::dsl as def_conf,
};
use actix_web::{get, web::Data, HttpRequest, HttpResponse, Scope};
use chrono::{DateTime, Utc};
use diesel::{dsl::max, ExpressionMethods, QueryDsl, RunQueryDsl};
use serde_json::{Map, Value, Value::Null};
use service_utils::{helpers::ToActixErr, service::types::AppState};

pub fn endpoints() -> Scope {
    Scope::new("").service(get)
}

#[get("")]
async fn get(req: HttpRequest, state: Data<AppState>) -> actix_web::Result<HttpResponse> {
    let mut conn = state
        .db_pool
        .get()
        .map_err_to_internal_server("error getting a connection from db pool", Null)?;
    let max_created_at: Option<DateTime<Utc>> = ctxt::contexts
        .select(max(ctxt::created_at))
        .first(&mut conn)
        .map_err_to_internal_server("error getting created at", Null)?;

    let last_modified = req
        .headers()
        .get("If-Modified-Since")
        .and_then(|header_val| {
            let header_str = header_val.to_str().ok()?;
            DateTime::parse_from_rfc2822(header_str)
                .map(|datetime| datetime.with_timezone(&Utc))
                .ok()
        });

    if max_created_at.is_some() && max_created_at < last_modified {
        return Ok(HttpResponse::NotModified().finish());
    };

    let contexts_vec = ctxt::contexts
        .select((ctxt::id, ctxt::value, ctxt::override_id, ctxt::override_))
        .order_by(ctxt::priority.asc())
        .load::<(String, Value, String, Value)>(&mut conn)
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
        .load::<(String, Value)>(&mut conn)
        .map_err_to_internal_server("error getting default configs", Null)?;

    let default_configs =
        default_config_vec
            .into_iter()
            .fold(Map::new(), |mut acc, item| {
                acc.insert(item.0, item.1);
                acc
            });

    let config = Config {
        contexts,
        overrides,
        default_configs,
    };

    return Ok(HttpResponse::Ok().json(config));
}
