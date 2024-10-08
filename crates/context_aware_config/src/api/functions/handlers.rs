extern crate base64;
use base64::prelude::*;

use super::helpers::{decode_function, fetch_function};

use crate::{
    api::functions::types::{Stage, TestFunctionRequest, TestParam},
    db::{
        self,
        models::Function,
        schema::functions::{dsl, dsl::functions, function_name, last_modified_at},
    },
    validation_functions,
};
use actix_web::{
    delete, get, patch, post, put,
    web::{self, Json, Path, Query},
    HttpResponse, Result, Scope,
};
use chrono::Utc;
use diesel::{delete, ExpressionMethods, QueryDsl, RunQueryDsl};
use serde_json::json;
use service_utils::service::types::DbConnection;

use superposition_macros::{bad_argument, not_found, unexpected_error};
use superposition_types::{result as superposition, QueryFilters, User};

use validation_functions::{compile_fn, execute_fn};

use super::types::{CreateFunctionRequest, FunctionName, UpdateFunctionRequest};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create)
        .service(update)
        .service(get)
        .service(list_functions)
        .service(delete_function)
        .service(test)
        .service(publish)
}

#[post("")]
async fn create(
    request: web::Json<CreateFunctionRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<Function>> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();

    compile_fn(&req.function)?;

    let function = Function {
        function_name: req.function_name.into(),
        draft_code: BASE64_STANDARD.encode(req.function),
        draft_runtime_version: req.runtime_version,
        draft_edited_by: user.get_email(),
        draft_edited_at: Utc::now().naive_utc(),
        published_code: None,
        published_at: None,
        published_by: None,
        published_runtime_version: None,
        function_description: req.description,
        last_modified_at: Utc::now().naive_utc(),
        last_modified_by: user.get_email(),
    };

    let insert: Result<Function, diesel::result::Error> = diesel::insert_into(functions)
        .values(&function)
        .get_result(&mut conn);

    match insert {
        Ok(mut res) => {
            decode_function(&mut res)?;
            Ok(Json(res))
        }
        Err(e) => match e {
            diesel::result::Error::DatabaseError(kind, e) => {
                log::error!("Function error: {:?}", e);
                match kind {
                    diesel::result::DatabaseErrorKind::UniqueViolation => {
                        Err(bad_argument!("Function already exists."))
                    }
                    _ => Err(unexpected_error!(
                        "Something went wrong, failed to create function"
                    )),
                }
            }
            _ => {
                log::error!("Function creation failed with error: {e}");
                Err(unexpected_error!(
                    "An error occured please contact the admin."
                ))
            }
        },
    }
}

#[patch("/{function_name}")]
async fn update(
    params: web::Path<FunctionName>,
    request: web::Json<UpdateFunctionRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<Function>> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();
    let f_name: String = params.into_inner().into();

    let result = match fetch_function(&f_name, &mut conn) {
        Ok(val) => val,
        Err(superposition::AppError::DbError(diesel::result::Error::NotFound)) => {
            log::error!("Function not found.");
            return Err(bad_argument!("Function {} doesn't exists", f_name));
        }
        Err(e) => {
            log::error!("Failed to update Function with error: {e}");
            return Err(unexpected_error!("Failed to update Function"));
        }
    };

    // Function Linter Check
    if let Some(function) = &req.function {
        compile_fn(function)?;
    }

    let new_function = Function {
        function_name: f_name.to_owned(),
        draft_code: req.function.map_or_else(
            || result.draft_code.clone(),
            |func| BASE64_STANDARD.encode(func),
        ),
        draft_runtime_version: req
            .runtime_version
            .unwrap_or(result.draft_runtime_version),
        function_description: req.description.unwrap_or(result.function_description),
        draft_edited_by: user.get_email(),
        draft_edited_at: Utc::now().naive_utc(),
        published_code: result.published_code,
        published_at: result.published_at,
        published_by: result.published_by,
        published_runtime_version: result.published_runtime_version,
        last_modified_at: Utc::now().naive_utc(),
        last_modified_by: user.get_email(),
    };

    let mut updated_function = diesel::update(functions)
        .filter(db::schema::functions::function_name.eq(f_name))
        .set(new_function)
        .get_result::<Function>(&mut conn)?;

    decode_function(&mut updated_function)?;
    Ok(Json(updated_function))
}

#[get("/{function_name}")]
async fn get(
    params: web::Path<FunctionName>,
    db_conn: DbConnection,
) -> superposition::Result<Json<Function>> {
    let DbConnection(mut conn) = db_conn;
    let f_name: String = params.into_inner().into();
    let mut function = fetch_function(&f_name, &mut conn)?;

    decode_function(&mut function)?;
    Ok(Json(function))
}

#[get("")]
async fn list_functions(
    db_conn: DbConnection,
    filters: Query<QueryFilters>,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let n_default_configs: i64 = functions.count().get_result(&mut conn)?;
    let mut builder = functions.into_boxed().order(last_modified_at.desc());
    if let Some(limit) = filters.count {
        builder = builder.limit(limit);
    }
    if let Some(page) = filters.page {
        let offset = (page - 1) * filters.count.unwrap_or(10);
        builder = builder.offset(offset);
    }
    let limit = filters.count.unwrap_or(10);
    let mut result: Vec<Function> = builder.load(&mut conn)?;
    let total_pages = (n_default_configs as f64 / limit as f64).ceil() as u64;

    for function in result.iter_mut() {
        decode_function(function)?;
    }
    Ok(HttpResponse::Ok().json(json!({
    "total_pages": total_pages,
    "total_items": n_default_configs,
    "data": result
    })))
}

#[delete("/{function_name}")]
async fn delete_function(
    params: web::Path<FunctionName>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let f_name: String = params.into_inner().into();

    diesel::update(functions)
        .filter(function_name.eq(&f_name))
        .set((
            dsl::last_modified_at.eq(Utc::now().naive_utc()),
            dsl::last_modified_by.eq(user.get_email()),
        ))
        .execute(&mut conn)?;
    let deleted_row =
        delete(functions.filter(function_name.eq(&f_name))).execute(&mut conn);
    match deleted_row {
        Ok(0) => Err(not_found!("Function {} doesn't exists", f_name)),
        Ok(_) => {
            log::info!("{f_name} function deleted by {}", user.get_email());
            Ok(HttpResponse::NoContent().finish())
        }
        Err(e) => {
            log::error!("function delete query failed with error: {e}");
            Err(unexpected_error!(
                "Something went wrong, failed to delete the function"
            ))
        }
    }
}

#[put("/{function_name}/{stage}/test")]
async fn test(
    params: Path<TestParam>,
    request: web::Json<TestFunctionRequest>,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let path_params = params.into_inner();
    let fun_name: &String = &path_params.function_name.into();
    let req = request.into_inner();
    let mut function = match fetch_function(fun_name, &mut conn) {
        Ok(val) => val,
        Err(superposition::AppError::DbError(diesel::result::Error::NotFound)) => {
            log::error!("Function not found.");
            return Err(bad_argument!("Function {} doesn't exists", fun_name));
        }
        Err(e) => {
            log::error!("Failed to fetch Function {fun_name} with error: {e}");
            return Err(unexpected_error!(
                "Something went wrong, failed to update function"
            ));
        }
    };

    decode_function(&mut function)?;
    let result = match path_params.stage {
        Stage::Draft => execute_fn(&function.draft_code, &req.key, req.value),
        Stage::Published => match function.published_code {
            Some(code) => execute_fn(&code, &req.key, req.value),
            None => {
                log::error!("Function test failed: function not published yet");
                Err((
                    "Function test failed as function not published yet".to_owned(),
                    None,
                ))
            }
        },
    };

    match result {
        Ok(stdout) => Ok(HttpResponse::Ok()
            .json(json!({"message": "Function validated the given value successfully", "stdout": stdout}))),
        Err((e, stdout)) => Err(bad_argument!("Function validation failed with error: {}, stdout: {:?}", e, stdout.unwrap_or(String::new()))),
    }
}

#[put("/{function_name}/publish")]
async fn publish(
    params: web::Path<FunctionName>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<Json<Function>> {
    let DbConnection(mut conn) = db_conn;
    let fun_name: String = params.into_inner().into();

    let function = match fetch_function(&fun_name, &mut conn) {
        Ok(val) => val,
        Err(superposition::AppError::DbError(diesel::result::Error::NotFound)) => {
            log::error!("Function {} not found.", fun_name);
            return Err(bad_argument!("Function {} doesn't exists", fun_name));
        }
        Err(e) => {
            log::error!("Failed to update Function with error: {e}");
            return Err(unexpected_error!(
                "Something went wrong, failed to update function"
            ));
        }
    };

    let updated_function = diesel::update(functions)
        .filter(dsl::function_name.eq(fun_name.clone()))
        .set((
            dsl::published_code.eq(Some(function.draft_code.clone())),
            dsl::published_runtime_version
                .eq(Some(function.draft_runtime_version.clone())),
            dsl::published_by.eq(Some(user.get_email())),
            dsl::published_at.eq(Some(Utc::now().naive_utc())),
        ))
        .get_result::<Function>(&mut conn)?;

    Ok(Json(updated_function))
}
