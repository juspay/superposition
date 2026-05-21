use actix_web::{Scope, get, web::Json};
use chrono::{Duration, Utc};
use diesel::dsl::sql;
use diesel::{
    BoolExpressionMethods, ExpressionMethods, QueryDsl, RunQueryDsl,
    sql_types::{Bool, Text},
};
use service_utils::{
    helpers::fetch_dimensions_info_map,
    service::types::{DbConnection, WorkspaceContext},
};
use superposition_derives::{authorized, declare_resource};
use superposition_macros::unexpected_error;
use superposition_types::{
    PaginatedResponse, SortBy,
    api::audit_log::AuditQueryFilters,
    custom_query::{
        self as superposition_query, CustomQuery, DimensionQuery, PaginationParams,
        QueryMap,
    },
    database::{models::cac::EventLog, schema::event_log::dsl as event_log},
    logic::evaluate_local_cohorts_skip_unresolved,
    result as superposition,
};

declare_resource!(AuditLog);

pub fn endpoints() -> Scope {
    Scope::new("").service(list_handler)
}

#[authorized]
#[get("")]
async fn list_handler(
    workspace_context: WorkspaceContext,
    filters: superposition_query::Query<AuditQueryFilters>,
    dimension_params: DimensionQuery<QueryMap>,
    pagination_params: superposition_query::Query<PaginationParams>,
    db_conn: DbConnection,
) -> superposition::Result<Json<PaginatedResponse<EventLog>>> {
    let now = Utc::now();
    let from_date = filters.from_date.unwrap_or(now - Duration::days(7));
    let to_date = filters.to_date.unwrap_or(now);

    if from_date > to_date {
        return Ok(Json(PaginatedResponse::default()));
    }

    let DbConnection(mut conn) = db_conn;
    let dimension_params = dimension_params.into_inner();
    let audit_context = if dimension_params.is_empty() {
        None
    } else {
        let dimensions_info =
            fetch_dimensions_info_map(&mut conn, &workspace_context.schema_name)?;
        let evaluated_context =
            evaluate_local_cohorts_skip_unresolved(&dimensions_info, &dimension_params);
        Some(serde_json::to_string(&evaluated_context).map_err(|err| {
            unexpected_error!("failed to serialize audit context filter: {}", err)
        })?)
    };

    let query_builder = |filters: &AuditQueryFilters| {
        let mut builder = event_log::event_log
            .schema_name(&workspace_context.schema_name)
            .into_boxed();
        if let Some(tables) = filters.table.clone() {
            builder = builder.filter(event_log::table_name.eq_any(tables.0));
        }
        if let Some(actions) = filters.action.clone() {
            builder = builder.filter(event_log::action.eq_any(actions.0));
        }
        if let Some(audit_context) = audit_context.as_deref() {
            builder = builder.filter(
            sql::<Bool>(
                "(
                    table_name NOT IN ('contexts', 'experiments', 'experiment_groups')
                    OR EXISTS (
                        SELECT 1
                        FROM jsonb_each(CAST(",
            )
            .bind::<Text, _>(audit_context)
            .sql(
                " AS jsonb)) AS filter(key, value)
                        WHERE
                            (
                                table_name = 'contexts'
                                AND (
                                    CAST(original_data AS jsonb)->'value'->filter.key = filter.value
                                    OR CAST(new_data AS jsonb)->'value'->filter.key = filter.value
                                )
                            )
                            OR (
                                table_name IN ('experiments', 'experiment_groups')
                                AND (
                                    CAST(original_data AS jsonb)->'context'->filter.key = filter.value
                                    OR CAST(new_data AS jsonb)->'context'->filter.key = filter.value
                                )
                            )
                    )
                )",
            ),
        );
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

    let sort_by = filters.sort_by.unwrap_or(SortBy::Desc);
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
