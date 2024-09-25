use crate::{
    db::{models::ConfigVersion, schema::config_versions},
    types::{PaginatedResponse, QueryFilters},
};

extern crate base64;
use service_utils::service::types::DbConnection;

use superposition_types::result as superposition;

use actix_web::{
    get,
    web::{Json, Query},
    Scope,
};
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};

pub fn endpoints() -> Scope {
    Scope::new("").service(get)
}

#[get("")]
async fn get(
    db_conn: DbConnection,
    filters: Query<QueryFilters>,
) -> superposition::Result<Json<PaginatedResponse<ConfigVersion>>> {
    let DbConnection(mut conn) = db_conn;

    let n_version: i64 = config_versions::dsl::config_versions
        .count()
        .get_result(&mut conn)?;
    let mut builder = config_versions::dsl::config_versions
        .into_boxed()
        .order(config_versions::dsl::created_at.desc());
    if let Some(limit) = filters.count {
        builder = builder.limit(limit);
    }
    if let Some(page) = filters.page {
        let offset = (page - 1) * filters.count.unwrap_or(10);
        builder = builder.offset(offset);
    }
    let limit = filters.count.unwrap_or(10);
    let config_versions: Vec<ConfigVersion> = builder.load(&mut conn)?;
    let total_pages = (n_version as f64 / limit as f64).ceil() as i64;
    Ok(Json(PaginatedResponse {
        total_pages,
        total_items: n_version,
        data: config_versions,
    }))
}
