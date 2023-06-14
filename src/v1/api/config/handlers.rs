use crate::{
    db::utils::AppState,
    v1::{db::{
        schema::{default_configs::dsl as def_conf, contexts::dsl as ctxt, overrides::dsl as over_dsl},//TODO rename over_dsl
    }, helpers::ToActixErr}
};
use super::types::Config;
use actix_web::{
    get,
    web::{Data, Json},
    Scope
};
use diesel::{QueryDsl, RunQueryDsl, ExpressionMethods};
use serde_json::{Map, Value, Value::Null};

pub fn endpoints() -> Scope {
    Scope::new("").service(get)
}

#[get("")]
async fn get(state: Data<AppState>) -> actix_web::Result<Json<Config>> {
    let mut conn = state
        .db_pool.get()
        .map_err_to_internal_server("error getting a connection from db pool", Null)?;

    let overrides_vec = over_dsl::overrides
        .select((over_dsl::id, over_dsl::value))
        .load::<(String, Value)>(&mut conn)
        .map_err_to_internal_server("error getting overrides", Null)?;

    let overrides = overrides_vec.into_iter().fold(Map::new(), |mut acc, item| {
        acc.insert(item.0, item.1);
        acc
    });

    let contexts_vec = ctxt::contexts
        .select((ctxt::value, ctxt::override_id))
        .order_by(ctxt::priority.asc())
        .load::<(Value, String)>(&mut conn)
        .map_err_to_internal_server("error getting contexts", Null)?;

    let contexts = contexts_vec.into_iter().fold(Vec::new(), |mut acc, item| {
        let ctxt = super::types::Context {
            condition: item.0,
            override_with_keys: [item.1]
        };
        acc.push(ctxt);
        acc
    });

    let default_config_vec = def_conf::default_configs
        .select((def_conf::key, def_conf::value))
        .load::<(String, Value)>(&mut conn)
        .map_err_to_internal_server("error getting default configs", Null)?;

    let default_configs = default_config_vec.into_iter().fold(Map::new(), |mut acc, item| {
        acc.insert(item.0, item.1);
        acc
    });
    
    let config =  Config {
        contexts,
        overrides,
        default_configs
    };

    Ok(Json(config))
}
