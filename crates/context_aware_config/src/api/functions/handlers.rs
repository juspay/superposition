use actix_web::{
    delete, get, patch, post,
    web::{Json, Path},
    HttpResponse, Result, Scope,
};
use chrono::Utc;
use diesel::{delete, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
use service_utils::service::types::{DbConnection, SchemaName};
use superposition_macros::{bad_argument, not_found, unexpected_error};
use superposition_types::{
    api::functions::{
        CreateFunctionRequest, FunctionExecutionRequest, FunctionExecutionResponse,
        FunctionName, FunctionStateChangeRequest, ListFunctionFilters, Stage, TestParam,
        UpdateFunctionRequest,
    },
    custom_query::{self as superposition_query, PaginationParams},
    database::{
        models::cac::{Function, FunctionType},
        schema::{self, functions::dsl as functions},
    },
    result as superposition, PaginatedResponse, User,
};

use crate::validation_functions::{compile_fn, execute_fn};

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
    request: Json<CreateFunctionRequest>,
    db_conn: DbConnection,
    user: User,
    schema_name: SchemaName,
) -> superposition::Result<Json<Function>> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();

    compile_fn(&req.function_type.get_js_fn_name(), &req.function)?;

    let now = Utc::now();
    let function = Function {
        function_name: req.function_name.into(),
        draft_code: (req.function),
        draft_runtime_version: req.runtime_version,
        draft_edited_by: user.get_email(),
        draft_edited_at: now,
        published_code: None,
        published_at: None,
        published_by: None,
        published_runtime_version: None,
        description: req.description,
        last_modified_at: now,
        last_modified_by: user.get_email(),
        change_reason: req.change_reason,
        function_type: req.function_type,
        created_at: now,
        created_by: user.get_email(),
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
    params: Path<FunctionName>,
    request: Json<UpdateFunctionRequest>,
    db_conn: DbConnection,
    user: User,
    schema_name: SchemaName,
) -> superposition::Result<Json<Function>> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();
    let f_name: String = params.into_inner().into();

    // Function Linter Check
    if let Some(function) = &req.draft_code {
        let function_type = functions::functions
            .select(schema::functions::function_type)
            .filter(schema::functions::function_name.eq(&f_name))
            .schema_name(&schema_name)
            .get_result::<FunctionType>(&mut conn)?;

        compile_fn(&function_type.get_js_fn_name(), function)?;
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
    params: Path<FunctionName>,
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
    pagination: superposition_query::Query<PaginationParams>,
    filters: superposition_query::Query<ListFunctionFilters>,
    schema_name: SchemaName,
) -> superposition::Result<Json<PaginatedResponse<Function>>> {
    let DbConnection(mut conn) = db_conn;
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
    params: Path<FunctionName>,
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
            .execute(&mut conn)?;
    match deleted_row {
        0 => Err(not_found!("Function {} doesn't exists", f_name)),
        _ => {
            log::info!("{f_name} function deleted by {}", user.get_email());
            Ok(HttpResponse::NoContent().finish())
        }
    }
}

#[post("/{function_name}/{stage}/test")]
async fn test(
    params: Path<TestParam>,
    request: Json<FunctionExecutionRequest>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<FunctionExecutionResponse>> {
    let DbConnection(mut conn) = db_conn;
    let path_params = params.into_inner();
    let fun_name: &String = &path_params.function_name.into();
    let req = request.into_inner();
    let function = fetch_function(fun_name, &mut conn, &schema_name)?;

    let code = match path_params.stage {
        Stage::Draft => function.draft_code,
        Stage::Published => match function.published_code {
            Some(code) => code,
            None => {
                log::error!("Function test failed: function not published yet");
                return Err(bad_argument!(
                    "Function test failed as function not published yet"
                ));
            }
        },
    };

    let result =
        execute_fn(&code, &req, &mut conn, &schema_name).map_err(|(e, stdout)| {
            bad_argument!(
                "Function failed with error: {}, stdout: {:?}",
                e,
                stdout.unwrap_or_default()
            )
        })?;

    Ok(Json(result))
}

#[patch("/{function_name}/publish")]
async fn publish(
    params: Path<FunctionName>,
    request: Json<FunctionStateChangeRequest>,
    db_conn: DbConnection,
    user: User,
    schema_name: SchemaName,
) -> superposition::Result<Json<Function>> {
    let DbConnection(mut conn) = db_conn;
    let fun_name: String = params.into_inner().into();
    let function = fetch_function(&fun_name, &mut conn, &schema_name)?;

    let updated_function = diesel::update(functions::functions)
        .filter(functions::function_name.eq(fun_name.clone()))
        .set((
            request.into_inner(),
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
