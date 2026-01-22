use actix_web::{HttpRequest, web::Data};
use diesel::{
    Connection, ExpressionMethods, RunQueryDsl,
    query_dsl::methods::{OrderDsl, SelectDsl},
};
use superposition_types::database::superposition_schema::superposition::organisations;

use crate::service::types::AppState;

pub(super) fn fetch_org_ids_from_db(
    req: &HttpRequest,
) -> Result<Vec<String>, &'static str> {
    let app_state = match req.app_data::<Data<AppState>>() {
        Some(state) => state,
        None => {
            log::info!("DbConnection-FromRequest: Unable to get app_data from request");
            return Err("Unable to get app_data from request");
        }
    };

    match app_state.db_pool.get() {
        Ok(mut conn) => {
            conn.set_prepared_statement_cache_size(
                diesel::connection::CacheSize::Disabled,
            );
            let orgs = organisations::table
                .order(organisations::created_at.desc())
                .select(organisations::id)
                .get_results::<String>(&mut conn);

            match orgs {
                Ok(orgs) => Ok(orgs),
                Err(e) => {
                    log::error!("Failed to fetch organisations: {:?}", e);
                    Err("Failed to fetch organisations")
                }
            }
        }
        Err(e) => {
            log::info!("Unable to get db connection from pool, error: {e}");
            Err("Unable to get db connection from pool")
        }
    }
}
