use actix_web::{delete, get, patch, post, web, HttpResponse, Scope};
use diesel::prelude::*;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::service::types::{DbConnection, SchemaName};
use superposition_macros::{db_error, not_found};
use superposition_types::{
    api::variables::*,
    custom_query::PaginationParams,
    database::{models::cac::Variable as DbVariable, schema::variables::dsl::*},
    result as superposition, User,
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
    pagination_params: web::Query<PaginationParams>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let pagination_params = pagination_params.into_inner();

    let page = pagination_params.page.unwrap_or(1);
    let count = pagination_params.count.unwrap_or(10);
    let show_all = pagination_params.all.unwrap_or(false);
    let total = variables
        .select(diesel::dsl::count(name))
        .schema_name(&schema_name)
        .first::<i64>(&mut conn)
        .map_err(|err| {
            log::error!("Failed to count variables: {}", err);
            db_error!(err)
        })?;

    let offset = count * (page - 1);

    let db_vars = if show_all {
        variables
            .order(name.asc())
            .schema_name(&schema_name)
            .load::<DbVariable>(&mut conn)
            .map_err(|err| {
                log::error!("Failed to fetch variables: {}", err);
                db_error!(err)
            })?
    } else {
        variables
            .order(name.asc())
            .limit(count as i64)
            .offset(offset as i64)
            .schema_name(&schema_name)
            .load::<DbVariable>(&mut conn)
            .map_err(|err| {
                log::error!("Failed to fetch variables: {}", err);
                db_error!(err)
            })?
    };

    let vars: Vec<Variable> = db_vars.into_iter().map(|v| v.into()).collect();

    let response = ListVariablesResponse {
        variables: vars,
        total: total as usize,
    };

    Ok(HttpResponse::Ok().json(response))
}

#[post("")]
async fn create_variable(
    req: web::Json<CreateVariableRequest>,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    // let exists = variables
    //     .filter(name.eq(&req.name.0))
    //     .schema_name(&schema_name)
    //     .first::<DbVariable>(&mut conn)
    //     .optional()
    //     .map_err(|err| {
    //         log::error!("Failed to check variable existence: {}", err);
    //         db_error!(err)
    //     })?;

    // if exists.is_some() {
    //     return Err(bad_argument!("Variable '{}' already exists", req.name.0));
    // }

    let new_var = DbVariable {
        name: req.name.0.clone(),
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
        .returning(DbVariable::as_returning())
        .schema_name(&schema_name)
        .execute(&mut conn)
        .map_err(|err| {
            log::error!("Failed to create variable: {}", err);
            db_error!(err)
        })?;

    Ok(HttpResponse::Created().json(created_var))
}

#[get("/{variable_name}")]
async fn get_variable(
    path: web::Path<String>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let var_name = path.into_inner();

    let var = variables
        .filter(name.eq(var_name.clone()))
        .schema_name(&schema_name)
        .first::<DbVariable>(&mut conn)
        .optional()
        .map_err(|err| {
            log::error!("Failed to fetch variable: {}", err);
            db_error!(err)
        })?;

    match var {
        Some(v) => {
            Ok(HttpResponse::Ok().json(GetVariableResponse { variable: v.into() }))
        }
        None => Err(not_found!("Variable '{}' not found", var_name)),
    }
}

#[patch("/{variable_name}")]
async fn update_variable(
    path: web::Path<String>,
    req: web::Json<UpdateVariableRequest>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let var_name = path.into_inner();

    let exists = variables
        .filter(name.eq(&var_name))
        .schema_name(&schema_name)
        .first::<DbVariable>(&mut conn)
        .optional()
        .map_err(|err| {
            log::error!("Failed to check variable existence: {}", err);
            db_error!(err)
        })?;

    if exists.is_none() {
        return Err(not_found!("Variable {} not found", var_name));
    }

    let updated_var = diesel::update(variables)
        .filter(name.eq(var_name))
        .set(value.eq(req.value.clone()))
        .schema_name(&schema_name)
        .get_result::<DbVariable>(&mut conn)
        .map_err(|err| {
            log::error!("Failed to update variable: {}", err);
            db_error!(err)
        })?;
    Ok(HttpResponse::Ok().json(updated_var))
}

#[delete("/{variable_name}")]
async fn delete_variable(
    path: web::Path<String>,
    req: web::Json<DeleteVariableRequest>,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let var_name = path.into_inner();

    // Check if variable exists
    let exists = variables
        .filter(name.eq(&var_name))
        .schema_name(&schema_name)
        .first::<DbVariable>(&mut conn)
        .optional()
        .map_err(|err| {
            log::error!("Failed to check variable existence: {}", err);
            db_error!(err)
        })?;

    if exists.is_none() {
        return Err(not_found!("Variable '{}' not found", var_name));
    }

    diesel::delete(variables)
        .filter(name.eq(&var_name))
        .schema_name(&schema_name)
        .execute(&mut conn)
        .map_err(|err| {
            log::error!("Failed to delete variable: {}", err);
            db_error!(err)
        })?;

    Ok(HttpResponse::Ok().json(serde_json::json!({
        "message": format!("Variable '{}' deleted successfully", var_name),
        "deleted_by": user.get_email(),
        "change_reason": req.change_reason
    })))
}
