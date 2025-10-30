use actix_web::{
    delete, get, patch, post,
    web::{self, Json, Query},
    HttpResponse, Scope,
};
use diesel::prelude::*;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::service::types::{DbConnection, SchemaName};
use superposition_types::{
    api::variables::*,
    custom_query::PaginationParams,
    database::{
        models::others::Variable,
        schema::variables::{self, dsl::*},
    },
    result as superposition, PaginatedResponse, User,
};

pub fn endpoints() -> Scope {
    web::scope("")
        .service(list_variables)
        .service(create_variable)
        .service(get_variable)
        .service(update_variable)
        .service(delete_variable)
}

#[get("")]
async fn list_variables(
    db_conn: DbConnection,
    pagination: Query<PaginationParams>,
    filters: Query<VariableFilters>,
    schema_name: SchemaName,
) -> superposition::Result<Json<PaginatedResponse<Variable>>> {
    let DbConnection(mut conn) = db_conn;

    let filters = filters.into_inner();

    let query_builder = |filters: &VariableFilters| {
        let mut builder = variables.schema_name(&schema_name).into_boxed();
        if let Some(ref var_name) = filters.name {
            builder = builder.filter(name.like(format!["%{}%", var_name]));
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
    let mut builder = base_query.order(variables::created_at.desc()).limit(limit);
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

#[post("")]
async fn create_variable(
    req: web::Json<CreateVariableRequest>,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<Variable>> {
    let DbConnection(mut conn) = db_conn;

    let new_var = Variable {
        name: req.name.clone(),
        value: req.value.clone(),
        description: req.description.clone(),
        change_reason: req.change_reason.clone(),
        created_at: chrono::Utc::now(),
        last_modified_at: chrono::Utc::now(),
        created_by: user.get_email(),
        last_modified_by: user.get_email(),
    };

    let created_var = diesel::insert_into(variables)
        .values(&new_var)
        .returning(Variable::as_returning())
        .schema_name(&schema_name)
        .get_result(&mut conn)?;

    Ok(Json(created_var))
}

#[get("/{variable_name}")]
async fn get_variable(
    path: web::Path<String>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<Variable>> {
    let DbConnection(mut conn) = db_conn;

    let var_name = path.into_inner();

    let var = variables
        .filter(name.eq(var_name))
        .schema_name(&schema_name)
        .get_result::<Variable>(&mut conn)?;

    Ok(Json(var))
}

#[patch("/{variable_name}")]
async fn update_variable(
    path: web::Path<String>,
    req: web::Json<UpdateVariableRequest>,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<Variable>> {
    let DbConnection(mut conn) = db_conn;
    let var_name = path.into_inner();

    let updated_var = diesel::update(variables)
        .filter(name.eq(var_name))
        .set((
            req.into_inner(),
            last_modified_at.eq(chrono::Utc::now()),
            last_modified_by.eq(user.get_email()),
        ))
        .schema_name(&schema_name)
        .get_result::<Variable>(&mut conn)?;
    Ok(Json(updated_var))
}

#[delete("/{variable_name}")]
async fn delete_variable(
    path: web::Path<String>,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let var_name = path.into_inner();

    diesel::update(variables)
        .filter(name.eq(&var_name))
        .set((
            last_modified_at.eq(chrono::Utc::now()),
            last_modified_by.eq(user.get_email()),
        ))
        .schema_name(&schema_name)
        .execute(&mut conn)?;

    diesel::delete(variables)
        .filter(name.eq(&var_name))
        .schema_name(&schema_name)
        .execute(&mut conn)?;

    Ok(HttpResponse::NoContent().finish())
}
