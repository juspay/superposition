pub mod helpers;

use actix_web::{
    delete, get, post, put,
    web::{self, Data, Json, Query},
    HttpResponse, Scope,
};
use diesel::{
    ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper, TextExpressionMethods,
};
use service_utils::service::types::{AppState, DbConnection, WorkspaceContext};
use superposition_macros::bad_argument;
use superposition_types::{
    api::experiment_groups::{
        ExpGroupCreateRequest, ExpGroupFilters, ExpGroupUpdateRequest, SortOn,
    },
    custom_query::PaginationParams,
    database::{
        models::experimentation::{ExperimentGroup, ExperimentGroups},
        schema::experiment_groups::dsl as experiment_groups,
    },
    result as superposition, PaginatedResponse, SortBy, User,
};

pub fn endpoints(scope: Scope) -> Scope {
    scope
        .service(create_experiment_group)
        .service(update_experiment_group)
        .service(list_experiment_groups)
        .service(get_experiment_group)
        .service(delete_experiment_group)
}

#[post("")]
async fn create_experiment_group(
    state: Data<AppState>,
    req: Json<ExpGroupCreateRequest>,
    db_conn: DbConnection,
    workspace_request: WorkspaceContext,
    user: User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let DbConnection(mut conn) = db_conn;
    let req = req.into_inner();
    log::trace!("Creating experiment group with request: {:?}", req);
    helpers::create_experiment_group(state, req, &mut conn, workspace_request, user).await
}

#[put("/{exp_group_id}")]
async fn update_experiment_group(
    exp_group_id: web::Path<i64>,
    req: Json<ExpGroupUpdateRequest>,
    db_conn: DbConnection,
    workspace_request: WorkspaceContext,
    user: User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let req = req.into_inner();
    let DbConnection(mut conn) = db_conn;
    let id = exp_group_id.into_inner();
    helpers::update_experiment_group(id, req, &mut conn, workspace_request, user)
}

#[get("")]
async fn list_experiment_groups(
    pagination_params: Query<PaginationParams>,
    filters: Query<ExpGroupFilters>,
    db_conn: DbConnection,
    workspace_request: WorkspaceContext,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let pagination_params = pagination_params.into_inner();
    let schema_name = workspace_request.schema_name;
    let query_builder = |filters: &ExpGroupFilters| {
        let mut builder = experiment_groups::experiment_groups
            .schema_name(&schema_name)
            .into_boxed();
        if let Some(name) = &filters.name {
            builder = builder.filter(experiment_groups::name.like(format!("%{}%", name)));
        }
        if let Some(created_by) = &filters.created_by {
            builder =
                builder.filter(experiment_groups::created_by.eq(created_by.clone()));
        }
        if let Some(last_modified_by) = &filters.last_modified_by {
            builder = builder
                .filter(experiment_groups::last_modified_by.eq(last_modified_by.clone()));
        }
        builder
    };
    let filters = filters.into_inner();
    let base_query = query_builder(&filters);
    if let Some(true) = pagination_params.all {
        let result: ExperimentGroups =
            base_query.get_results::<ExperimentGroup>(&mut conn)?;
        return Ok(HttpResponse::Ok().json(PaginatedResponse::all(result)));
    }
    let count_query = query_builder(&filters);
    let total_items = count_query.count().get_result(&mut conn)?;
    let limit = pagination_params.count.unwrap_or(10);
    let offset = (pagination_params.page.unwrap_or(1) - 1) * limit;

    let sort_by = filters.sort_by.unwrap_or(SortBy::Desc);
    let sort_on = filters.sort_on.unwrap_or_default();

    #[rustfmt::skip]
    let base_query = match (sort_on, sort_by) {
        (SortOn::LastModifiedAt, SortBy::Desc) => base_query.order(experiment_groups::last_modified_at.desc()),
        (SortOn::LastModifiedAt, SortBy::Asc)  => base_query.order(experiment_groups::last_modified_at.asc()),
        (SortOn::CreatedAt, SortBy::Desc)      => base_query.order(experiment_groups::created_at.desc()),
        (SortOn::CreatedAt, SortBy::Asc)       => base_query.order(experiment_groups::created_at.asc()),
        (SortOn::Name, SortBy::Desc)           => base_query.order(experiment_groups::name.desc()),
        (SortOn::Name, SortBy::Asc)            => base_query.order(experiment_groups::name.asc()),
    };

    let query = base_query.limit(limit).offset(offset);
    let data = query.load::<ExperimentGroup>(&mut conn)?;
    let total_pages = (total_items as f64 / limit as f64).ceil() as i64;
    Ok(HttpResponse::Ok().json(PaginatedResponse {
        total_pages,
        total_items,
        data,
    }))
}

#[get("/{exp_group_id}")]
async fn get_experiment_group(
    exp_group_id: web::Path<i64>,
    db_conn: DbConnection,
    workspace_request: WorkspaceContext,
) -> superposition::Result<Json<ExperimentGroup>> {
    let id = exp_group_id.into_inner();
    let DbConnection(mut conn) = db_conn;
    let schema_name = workspace_request.schema_name;
    let result = experiment_groups::experiment_groups
        .schema_name(&schema_name)
        .filter(experiment_groups::experiment_group_id.eq(id))
        .first::<ExperimentGroup>(&mut conn)?;
    Ok(Json(result))
}

#[delete("/{exp_group_id}")]
async fn delete_experiment_group(
    exp_group_id: web::Path<i64>,
    db_conn: DbConnection,
    workspace_request: WorkspaceContext,
    user: User,
) -> superposition::Result<Json<ExperimentGroup>> {
    let id = exp_group_id.into_inner();
    let DbConnection(mut conn) = db_conn;
    let schema_name = workspace_request.schema_name;
    let marked_group = diesel::update(experiment_groups::experiment_groups)
        .filter(experiment_groups::experiment_group_id.eq(&id))
        .set((
            experiment_groups::last_modified_by.eq(user.email),
            experiment_groups::last_modified_at.eq(chrono::Utc::now()),
        ))
        .returning(ExperimentGroup::as_returning())
        .schema_name(&schema_name)
        .get_result(&mut conn)?;
    if !marked_group.member_experiment_ids.is_empty() {
        return Err(bad_argument!(
            "Cannot delete experiment group {} since it has members",
            marked_group.name
        ));
    }
    diesel::delete(experiment_groups::experiment_groups)
        .filter(experiment_groups::experiment_group_id.eq(&id))
        .schema_name(&schema_name)
        .execute(&mut conn)?;
    Ok(Json(marked_group))
}
