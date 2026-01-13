use actix_web::{
    delete, get, patch, post,
    web::{self, Json, Query},
    Scope,
};
use diesel::prelude::*;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::service::types::{DbConnection, WorkspaceContext};
use superposition_derives::authorized;
use superposition_types::{
    api::variables::*,
    custom_query::PaginationParams,
    database::{models::others::Variable, schema::variables::dsl::*},
    result as superposition, PaginatedResponse, SortBy, User,
};

use crate::helpers::validate_change_reason;

pub fn endpoints() -> Scope {
    web::scope("")
        .service(list_handler)
        .service(create_handler)
        .service(get_handler)
        .service(update_handler)
        .service(delete_handler)
}

#[authorized]
#[get("")]
async fn list_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    pagination: Query<PaginationParams>,
    filters: Query<VariableFilters>,
) -> superposition::Result<Json<PaginatedResponse<Variable>>> {
    let DbConnection(mut conn) = db_conn;

    let filters = filters.into_inner();

    let query_builder = |filters: &VariableFilters| {
        let mut builder = variables
            .schema_name(&workspace_context.schema_name)
            .into_boxed();

        if let Some(ref var_names) = filters.name {
            builder = builder.filter(name.eq_any(var_names.0.clone()));
        }

        if let Some(ref creators) = filters.created_by {
            builder = builder.filter(created_by.eq_any(creators.0.clone()));
        }

        if let Some(ref last_modifiers) = filters.last_modified_by {
            builder = builder.filter(last_modified_by.eq_any(last_modifiers.0.clone()));
        }

        builder
    };

    if let Some(true) = pagination.all {
        let result: Vec<Variable> = query_builder(&filters).get_results(&mut conn)?;
        return Ok(Json(PaginatedResponse::all(result)));
    }

    let base_query = query_builder(&filters);
    let count_query = query_builder(&filters);

    let n_variables: i64 = count_query.count().get_result(&mut conn)?;
    let limit = pagination.count.unwrap_or(10);

    let sort_on = filters.sort_on.unwrap_or_default();
    let sort_by = filters.sort_by.unwrap_or_default();

    #[rustfmt::skip]
    let base_query = match (sort_on, sort_by) {
        (SortOn::Name,           SortBy::Asc)  => base_query.order(name.asc()),
        (SortOn::Name,           SortBy::Desc) => base_query.order(name.desc()),
        (SortOn::CreatedAt,      SortBy::Asc)  => base_query.order(created_at.asc()),
        (SortOn::CreatedAt,      SortBy::Desc) => base_query.order(created_at.desc()),
        (SortOn::LastModifiedAt, SortBy::Asc)  => base_query.order(last_modified_at.asc()),
        (SortOn::LastModifiedAt, SortBy::Desc) => base_query.order(last_modified_at.desc()),
    };

    let mut builder = base_query.limit(limit);
    if let Some(page) = pagination.page {
        let offset = (page - 1) * limit;
        builder = builder.offset(offset);
    }
    let result: Vec<Variable> = builder.load(&mut conn)?;
    let total_pages = (n_variables as f64 / limit as f64).ceil() as i64;
    Ok(Json(PaginatedResponse {
        total_pages,
        total_items: n_variables,
        data: result,
    }))
}

#[authorized]
#[post("")]
async fn create_handler(
    workspace_context: WorkspaceContext,
    req: web::Json<CreateVariableRequest>,
    user: User,
    db_conn: DbConnection,
) -> superposition::Result<Json<Variable>> {
    let DbConnection(mut conn) = db_conn;
    let req = req.into_inner();

    validate_change_reason(&workspace_context, &req.change_reason, &mut conn)?;

    let now = chrono::Utc::now();

    let new_var = Variable {
        name: req.name,
        value: req.value,
        description: req.description,
        change_reason: req.change_reason,
        created_at: now,
        last_modified_at: now,
        created_by: user.get_email(),
        last_modified_by: user.get_email(),
    };

    let created_var = diesel::insert_into(variables)
        .values(&new_var)
        .returning(Variable::as_returning())
        .schema_name(&workspace_context.schema_name)
        .get_result(&mut conn)?;

    Ok(Json(created_var))
}

#[authorized]
#[get("/{variable_name}")]
async fn get_handler(
    workspace_context: WorkspaceContext,
    path: web::Path<String>,
    db_conn: DbConnection,
) -> superposition::Result<Json<Variable>> {
    let DbConnection(mut conn) = db_conn;

    let var_name = path.into_inner();

    let var = variables
        .filter(name.eq(var_name))
        .schema_name(&workspace_context.schema_name)
        .get_result::<Variable>(&mut conn)?;

    Ok(Json(var))
}

#[authorized]
#[patch("/{variable_name}")]
async fn update_handler(
    workspace_context: WorkspaceContext,
    path: web::Path<String>,
    req: web::Json<UpdateVariableRequest>,
    user: User,
    db_conn: DbConnection,
) -> superposition::Result<Json<Variable>> {
    let DbConnection(mut conn) = db_conn;
    let var_name = path.into_inner();

    validate_change_reason(&workspace_context, &req.change_reason, &mut conn)?;

    let updated_var = diesel::update(variables)
        .filter(name.eq(var_name))
        .set((
            req.into_inner(),
            last_modified_at.eq(chrono::Utc::now()),
            last_modified_by.eq(user.get_email()),
        ))
        .schema_name(&workspace_context.schema_name)
        .get_result::<Variable>(&mut conn)?;
    Ok(Json(updated_var))
}

#[authorized]
#[delete("/{variable_name}")]
async fn delete_handler(
    workspace_context: WorkspaceContext,
    path: web::Path<String>,
    user: User,
    db_conn: DbConnection,
) -> superposition::Result<Json<Variable>> {
    let DbConnection(mut conn) = db_conn;
    let var_name = path.into_inner();

    diesel::update(variables)
        .filter(name.eq(&var_name))
        .set((
            last_modified_at.eq(chrono::Utc::now()),
            last_modified_by.eq(user.get_email()),
        ))
        .schema_name(&workspace_context.schema_name)
        .execute(&mut conn)?;

    let deleted_variable = diesel::delete(variables)
        .filter(name.eq(&var_name))
        .schema_name(&workspace_context.schema_name)
        .get_result::<Variable>(&mut conn)?;

    Ok(Json(deleted_variable))
}
