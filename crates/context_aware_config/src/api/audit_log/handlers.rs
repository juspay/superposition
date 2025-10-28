use actix_web::{
    get,
    web::{Json, Query},
    Scope,
};
use chrono::{Duration, Utc};
use diesel::{BoolExpressionMethods, ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::service::types::{DbConnection, SchemaName};
use superposition_types::{
    api::audit_log::AuditQueryFilters,
    custom_query::PaginationParams,
    database::{models::cac::EventLog, schema::event_log::dsl as event_log},
    result as superposition, PaginatedResponse, SortBy,
};

use actix_web_lab::extract::Query as MultiValueQuery;

pub fn endpoints() -> Scope {
    Scope::new("").service(get_audit_logs)
}

#[get("")]
async fn get_audit_logs(
    filters: MultiValueQuery<AuditQueryFilters>,
    pagination_params: Query<PaginationParams>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<PaginatedResponse<EventLog>>> {
    let filters = filters.into_inner();
    let now = Utc::now();
    let from_date = filters.from_date.unwrap_or(now - Duration::days(7));
    let to_date = filters.to_date.unwrap_or(now);

    if from_date > to_date {
        return Ok(Json(PaginatedResponse::default()));
    }

    let DbConnection(mut conn) = db_conn;

    let query_builder = |filters: &AuditQueryFilters| {
        let mut builder = event_log::event_log.schema_name(&schema_name).into_boxed();
        if let Some(tables) = filters.table.clone() {
            builder = builder.filter(event_log::table_name.eq_any(tables));
        }
        if let Some(actions) = filters.action.clone() {
            builder = builder.filter(event_log::action.eq_any(actions));
        }
        if let Some(username) = filters.username.clone() {
            builder = builder.filter(event_log::user_name.eq(username));
        }
        builder.filter(
            event_log::timestamp
                .ge(from_date)
                .and(event_log::timestamp.le(to_date)),
        )
    };

    let pagination_params = pagination_params.into_inner();
    let sort_by = filters.sort_by.clone().unwrap_or(SortBy::Desc);
    let base_query = query_builder(&filters);
    let count_query = query_builder(&filters);

    let base_query = match sort_by {
        SortBy::Desc => base_query.order(event_log::timestamp.desc()),
        SortBy::Asc => base_query.order(event_log::timestamp.asc()),
    };

    let limit = pagination_params.count.unwrap_or(10);
    let offset = (pagination_params.page.unwrap_or(1) - 1) * limit;
    let logs = base_query.limit(limit).offset(offset).load(&mut conn)?;

    let log_count: i64 = count_query.count().get_result(&mut conn)?;

    let total_pages = (log_count as f64 / limit as f64).ceil() as i64;

    Ok(Json(PaginatedResponse {
        total_items: log_count,
        total_pages,
        data: logs,
    }))
}
