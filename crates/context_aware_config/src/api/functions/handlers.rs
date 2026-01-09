use actix_web::{
    delete, get, patch, post,
    web::{Json, Path},
    HttpResponse, Result, Scope,
};
use chrono::Utc;
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
use service_utils::service::types::{DbConnection, SchemaName};
use superposition_derives::authorized;
use superposition_macros::{bad_argument, not_found, unexpected_error};
use superposition_types::{
    api::functions::{
        CreateFunctionRequest, FunctionExecutionRequest, FunctionExecutionResponse,
        FunctionName, FunctionStateChangeRequest, ListFunctionFilters, Stage, TestParam,
        UpdateFunctionRequest,
    },
    custom_query::{self as superposition_query, PaginationParams},
    database::{
        models::{
            cac::{Function, FunctionType},
            Workspace,
        },
        schema::{self, functions::dsl as functions},
    },
    result as superposition, PaginatedResponse, User,
};

use crate::{
    helpers::validate_change_reason,
    validation_functions::{compile_fn, execute_fn},
};

use super::helpers::fetch_function;

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create_handler)
        .service(update_handler)
        .service(get_handler)
        .service(list_handler)
        .service(delete_handler)
        .service(test_handler)
        .service(publish_handler)
}

#[authorized]
#[post("")]
async fn create_handler(
    workspace_settings: Workspace,
    request: Json<CreateFunctionRequest>,
    db_conn: DbConnection,
    user: User,
    schema_name: SchemaName,
) -> superposition::Result<Json<Function>> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();

    if req.function_type == FunctionType::ContextValidation
        || req.function_type == FunctionType::ChangeReasonValidation
    {
        log::error!(
            "Attempted to create reserved function type: {:?}",
            req.function_type
        );
        return Err(bad_argument!(
            "Cannot create function of type {:?}: This function type is reserved and cannot be created manually.",
            req.function_type
        ));
    }

    validate_change_reason(
        &workspace_settings,
        &req.change_reason,
        &mut conn,
        &schema_name,
    )?;

    compile_fn(&req.function)?;

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

#[authorized]
#[patch("/{function_name}")]
async fn update_handler(
    workspace_settings: Workspace,
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
        compile_fn(function)?;
    }

    validate_change_reason(
        &workspace_settings,
        &req.change_reason,
        &mut conn,
        &schema_name,
    )?;

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

#[authorized]
#[get("/{function_name}")]
async fn get_handler(
    params: Path<FunctionName>,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<Json<Function>> {
    let DbConnection(mut conn) = db_conn;
    let f_name: String = params.into_inner().into();
    let function = fetch_function(&f_name, &mut conn, &schema_name)?;

    Ok(Json(function))
}

#[authorized]
#[get("")]
async fn list_handler(
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

#[authorized]
#[delete("/{function_name}")]
async fn delete_handler(
    params: Path<FunctionName>,
    db_conn: DbConnection,
    user: User,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let f_name: String = params.into_inner().into();

    let function = fetch_function(&f_name, &mut conn, &schema_name)?;
    match function.function_type {
        FunctionType::ContextValidation | FunctionType::ChangeReasonValidation => {
            return Err(bad_argument!(
                "Cannot delete function of type {:?}: This function type is reserved and cannot be deleted.",
                function.function_type
            ));
        }
        _ => {}
    }

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
        diesel::delete(functions::functions.filter(functions::function_name.eq(&f_name)))
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

#[authorized]
#[post("/{function_name}/{stage}/test")]
async fn test_handler(
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

    let (code, version) = match path_params.stage {
        Stage::Draft => (function.draft_code, function.draft_runtime_version),
        Stage::Published => {
            match (function.published_code, function.published_runtime_version) {
                (Some(code), Some(version)) => (code, version),
                _ => {
                    return Err(bad_argument!(
                        "Function test failed as function not published yet"
                    ));
                }
            }
        }
    };

    let result = execute_fn(&code, &req, version, &mut conn, &schema_name).map_err(
        |(e, stdout)| {
            bad_argument!(
                "Function failed with error: {}, stdout: {:?}",
                e,
                stdout.unwrap_or_default()
            )
        },
    )?;

    Ok(Json(result))
}

#[authorized]
#[patch("/{function_name}/publish")]
async fn publish_handler(
    workspace_settings: Workspace,
    params: Path<FunctionName>,
    request: Json<FunctionStateChangeRequest>,
    db_conn: DbConnection,
    user: User,
    schema_name: SchemaName,
) -> superposition::Result<Json<Function>> {
    let DbConnection(mut conn) = db_conn;
    let fun_name: String = params.into_inner().into();
    let function = fetch_function(&fun_name, &mut conn, &schema_name)?;
    let req = request.into_inner();

    validate_change_reason(
        &workspace_settings,
        &req.change_reason,
        &mut conn,
        &schema_name,
    )?;

    let updated_function = diesel::update(functions::functions)
        .filter(functions::function_name.eq(fun_name.clone()))
        .set((
            req,
            functions::published_code.eq(Some(function.draft_code.clone())),
            functions::published_runtime_version.eq(Some(function.draft_runtime_version)),
            functions::published_by.eq(Some(user.get_email())),
            functions::published_at.eq(Some(Utc::now())),
            functions::last_modified_by.eq(user.get_email()),
            functions::last_modified_at.eq(Utc::now()),
        ))
        .returning(Function::as_returning())
        .schema_name(&schema_name)
        .get_result::<Function>(&mut conn)?;

    Ok(Json(updated_function))
}
