use super::types::Config;
use crate::{
    db::utils::AppState,
    v1::{
        db::schema::cac_v1::{contexts::dsl as ctxt, default_configs::dsl as def_conf},
        helpers::ToActixErr,
    },
};
use actix_web::{
    get,
    web::{Data, Json},
    Scope,
};
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use serde_json::{Map, Value, Value::Null};

pub fn endpoints() -> Scope {
    Scope::new("").service(get)
}

#[get("")]
async fn get(state: Data<AppState>) -> actix_web::Result<Json<Config>> {
    let mut conn = state
        .db_pool
        .get()
        .map_err_to_internal_server("error getting a connection from db pool", Null)?;

    let contexts_vec = ctxt::contexts
        .select((ctxt::id, ctxt::value, ctxt::override_id, ctxt::override_))
        .order_by(ctxt::priority.asc())
        .load::<(String, Value, String, Value)>(&mut conn)
        .map_err_to_internal_server("error getting contexts", Null)?;

    let (contexts, overrides) = contexts_vec.into_iter().fold(
        (Vec::new(), Map::new()),
        |(mut ctxts, mut overrides), (context_id, condition, override_id, override_)| {
            let ctxt = super::types::Context {
                context_id: context_id,
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

    Ok(Json(config))
}
