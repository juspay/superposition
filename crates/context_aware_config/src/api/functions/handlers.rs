use actix_web::{
    delete, get, patch, post, put,
    web::{self, Json, Path, Query},
    HttpResponse, Result, Scope,
};
use chrono::Utc;
use diesel::{delete, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
use service_utils::service::types::{DbConnection, SchemaName};
use superposition_macros::{bad_argument, not_found, unexpected_error};
use superposition_types::{
    api::functions::{
        CreateFunctionRequest, FunctionExecutionRequest, FunctionName,
        ListFunctionFilters, Stage, TestParam, UpdateFunctionRequest,
    },
    custom_query::PaginationParams,
    database::{
        models::cac::Function,
        schema::{self, functions::dsl as functions},
    },
    result as superposition, PaginatedResponse, User,
};
use validation_functions::{compile_fn, execute_fn};

use crate::validation_functions;

use super::helpers::fetch_function;

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
    schema_name: SchemaName,
) -> superposition::Result<Json<Function>> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();

    compile_fn(&req.function_type.get_js_fn_name(), &req.function)?;

    let function = Function {
        function_name: req.function_name.into(),
        draft_code: (req.function),
        draft_runtime_version: req.runtime_version,
        draft_edited_by: user.get_email(),
        draft_edited_at: Utc::now(),
        published_code: None,
        published_at: None,
        published_by: None,
        published_runtime_version: None,
        description: req.description,
        last_modified_at: Utc::now(),
        last_modified_by: user.get_email(),
        change_reason: req.change_reason,
        function_type: req.function_type,
    };

    let insert: Result<Function, diesel::result::Error> =
        diesel::insert_into(functions::functions)
            .values(&function)
            .returning(Function::as_returning())
            .schema_name(&schema_name)
            .get_result(&mut conn);

    match insert {
        Ok(res) => Ok(Json(res)),
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
    schema_name: SchemaName,
) -> superposition::Result<Json<Function>> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();
    let f_name: String = params.into_inner().into();

    // Function Linter Check
    if let Some(function) = &req.draft_code {
        compile_fn(&req.function_type.get_js_fn_name(), function)?;
    }

    let updated_function = diesel::update(functions::functions)
        .filter(schema::functions::function_name.eq(f_name))
        .set((
            req,
            functions::draft_edited_by.eq(user.get_email()),
            functions::draft_edited_at.eq(Utc::now()),
            functions::last_modified_by.eq(user.get_email()),
            functions::last_modified_at.eq(Utc::now()),
        ))
        .returning(Function::as_returning())
        .schema_name(&schema_name)
        .get_result::<Function>(&mut conn)?;

    Ok(Json(updated_function))
}

#[get("/{function_name}")]
async fn get(
    params: web::Path<FunctionName>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<Function>> {
    let DbConnection(mut conn) = db_conn;
    let f_name: String = params.into_inner().into();
    let function = fetch_function(&f_name, &mut conn, &schema_name)?;

    Ok(Json(function))
}

#[get("")]
async fn list_functions(
    db_conn: DbConnection,
    pagination: Query<PaginationParams>,
    filters: Query<ListFunctionFilters>,
    schema_name: SchemaName,
) -> superposition::Result<Json<PaginatedResponse<Function>>> {
    let DbConnection(mut conn) = db_conn;
    let filters = filters.into_inner();
    let query_builder = |f: &ListFunctionFilters| {
        let mut builder = functions::functions.schema_name(&schema_name).into_boxed();
        if let Some(ref fntype) = f.function_type {
            builder = builder.filter(functions::function_type.eq_any(fntype.0.clone()));
        }
        builder
    };
    if let Some(true) = pagination.all {
        let result: Vec<Function> = query_builder(&filters).get_results(&mut conn)?;
        return Ok(Json(PaginatedResponse::all(result)));
    }
    let n_functions: i64 = query_builder(&filters).count().get_result(&mut conn)?;
    let limit = pagination.count.unwrap_or(10);
    let offset = (pagination.page.unwrap_or(1) - 1) * limit;
    let data: Vec<Function> = query_builder(&filters)
        .order(functions::last_modified_at.desc())
        .limit(limit)
        .offset(offset)
        .load(&mut conn)?;
    let total_pages = (n_functions as f64 / limit as f64).ceil() as i64;
    Ok(Json(PaginatedResponse {
        total_pages,
        total_items: n_functions,
        data,
    }))
}

#[delete("/{function_name}")]
async fn delete_function(
    params: web::Path<FunctionName>,
    db_conn: DbConnection,
    user: User,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let f_name: String = params.into_inner().into();

    diesel::update(functions::functions)
        .filter(functions::function_name.eq(&f_name))
        .set((
            functions::last_modified_at.eq(Utc::now()),
            functions::last_modified_by.eq(user.get_email()),
        ))
        .returning(Function::as_returning())
        .schema_name(&schema_name)
        .execute(&mut conn)?;
    let deleted_row =
        delete(functions::functions.filter(functions::function_name.eq(&f_name)))
            .schema_name(&schema_name)
            .execute(&mut conn);
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
    request: web::Json<FunctionExecutionRequest>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let path_params = params.into_inner();
    let fun_name: &String = &path_params.function_name.into();
    let req = request.into_inner();
    let function = match fetch_function(fun_name, &mut conn, &schema_name) {
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

    let result = match path_params.stage {
        Stage::Draft => execute_fn(&function.draft_code, &req),
        Stage::Published => match function.published_code {
            Some(code) => execute_fn(&code, &req),
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
        Ok(res) => Ok(HttpResponse::Ok().json(res)),
        Err((e, stdout)) => Err(bad_argument!(
            "Function failed with error: {}, stdout: {:?}",
            e,
            stdout.unwrap_or(String::new())
        )),
    }
}

#[put("/{function_name}/publish")]
async fn publish(
    params: web::Path<FunctionName>,
    db_conn: DbConnection,
    user: User,
    schema_name: SchemaName,
) -> superposition::Result<Json<Function>> {
    let DbConnection(mut conn) = db_conn;
    let fun_name: String = params.into_inner().into();

    let function = match fetch_function(&fun_name, &mut conn, &schema_name) {
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

    let updated_function = diesel::update(functions::functions)
        .filter(functions::function_name.eq(fun_name.clone()))
        .set((
            functions::published_code.eq(Some(function.draft_code.clone())),
            functions::published_runtime_version
                .eq(Some(function.draft_runtime_version.clone())),
            functions::published_by.eq(Some(user.get_email())),
            functions::published_at.eq(Some(Utc::now())),
        ))
        .returning(Function::as_returning())
        .schema_name(&schema_name)
        .get_result::<Function>(&mut conn)?;

    Ok(Json(updated_function))
}
