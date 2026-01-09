use actix_web::{
    delete, get, post, routes,
    web::{Data, Json, Path, Query},
    HttpResponse, Scope,
};
use chrono::Utc;
use diesel::{
    Connection, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper,
    TextExpressionMethods,
};
use jsonschema::{Draft, JSONSchema, ValidationError};
use serde_json::Value;
use service_utils::{
    helpers::{parse_config_tags, validation_err_to_str},
    service::types::{
        AppHeader, AppState, CustomHeaders, DbConnection, SchemaName, WorkspaceContext,
    },
};
use superposition_derives::authorized;
use superposition_macros::{
    bad_argument, db_error, not_found, unexpected_error, validation_error,
};
use superposition_types::{
    api::{
        default_config::{
            DefaultConfigCreateRequest, DefaultConfigFilters, DefaultConfigKey,
            DefaultConfigUpdateRequest,
        },
        functions::{FunctionEnvironment, FunctionExecutionRequest, KeyType},
    },
    custom_query::PaginationParams,
    database::{
        models::{
            cac::{self as models, Context, DefaultConfig, FunctionType},
            Description,
        },
        schema::{self, contexts::dsl::contexts, default_configs::dsl},
    },
    result as superposition, DBConnection, PaginatedResponse, User,
};

#[cfg(feature = "high-performance-mode")]
use crate::helpers::put_config_in_redis;
use crate::{
    api::{
        context::helpers::validation_function_executor,
        functions::{
            helpers::{check_fn_published, get_published_function_code},
            types::FunctionInfo,
        },
    },
    helpers::{add_config_version, validate_change_reason},
};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create_handler)
        .service(update_handler)
        .service(get_handler)
        .service(list_handler)
        .service(delete_handler)
}

#[authorized]
#[post("")]
async fn create_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    request: Json<DefaultConfigCreateRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();
    let key = req.key;
    let tags = parse_config_tags(custom_headers.config_tags)?;

    if req.schema.is_empty() {
        return Err(bad_argument!("Schema cannot be empty."));
    }

    validate_change_reason(&workspace_context, &req.change_reason, &mut conn)?;

    let value = req.value;

    let default_config = DefaultConfig {
        key: key.to_owned(),
        value,
        schema: req.schema,
        value_validation_function_name: req.value_validation_function_name,
        created_by: user.get_email(),
        created_at: Utc::now(),
        last_modified_at: Utc::now(),
        last_modified_by: user.get_email(),
        description: req.description,
        change_reason: req.change_reason.clone(),
        value_compute_function_name: req.value_compute_function_name,
    };

    let schema = Value::from(&default_config.schema);
    let schema_compile_result = JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&schema);
    let jschema = match schema_compile_result {
        Ok(jschema) => jschema,
        Err(e) => {
            log::info!("Failed to compile as a Draft-7 JSON schema: {e}");
            return Err(bad_argument!("Invalid JSON schema (failed to compile)"));
        }
    };

    if let Err(e) = jschema.validate(&default_config.value) {
        let verrors = e.collect::<Vec<ValidationError>>();
        log::info!(
            "Validation for value with given JSON schema failed: {:?}",
            verrors
        );
        return Err(validation_error!(
            "Schema validation failed: {}",
            &validation_err_to_str(verrors)
                .first()
                .unwrap_or(&String::new())
        ));
    }

    validate_default_config_with_function(
        &mut conn,
        &default_config.value_validation_function_name,
        &default_config.key,
        &default_config.value,
        &workspace_context.schema_name,
    )?;

    validate_fn_published(
        &default_config.value_compute_function_name,
        FunctionType::ValueCompute,
        &mut conn,
        &workspace_context.schema_name,
    )?;

    let version_id =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            diesel::insert_into(dsl::default_configs)
                .values(&default_config)
                .returning(DefaultConfig::as_returning())
                .schema_name(&workspace_context.schema_name)
                .execute(transaction_conn)?;

            let version_id = add_config_version(
                &state,
                tags,
                req.change_reason.into(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;
            Ok(version_id)
        })?;

    #[cfg(feature = "high-performance-mode")]
    put_config_in_redis(version_id, state, &workspace_context.schema_name, &mut conn)
        .await?;
    let mut http_resp = HttpResponse::Ok();

    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        version_id.to_string(),
    ));

    Ok(http_resp.json(default_config))
}

#[authorized]
#[get("/{key}")]
async fn get_handler(
    workspace_context: WorkspaceContext,
    key: Path<DefaultConfigKey>,
    db_conn: DbConnection,
) -> superposition::Result<Json<DefaultConfig>> {
    let DbConnection(mut conn) = db_conn;
    let res = fetch_default_key(&key, &mut conn, &workspace_context.schema_name)?;
    Ok(Json(res))
}

#[allow(clippy::too_many_arguments)]
#[authorized]
#[routes]
#[put("/{key}")]
#[patch("/{key}")]
async fn update_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    key: Path<DefaultConfigKey>,
    custom_headers: CustomHeaders,
    request: Json<DefaultConfigUpdateRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();
    let key_str = key.into_inner().into();
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let existing = fetch_default_key(&key_str, &mut conn, &workspace_context.schema_name)
        .map_err(|e| match e {
            superposition::AppError::DbError(diesel::NotFound) => {
                bad_argument!(
                    "No record found for {}. Use create endpoint instead.",
                    key_str
                )
            }
            _ => {
                log::error!("Failed to fetch {key_str}: {e}");
                unexpected_error!("Something went wrong.")
            }
        })?;

    validate_change_reason(&workspace_context, &req.change_reason, &mut conn)?;

    let value = req.value.clone().unwrap_or_else(|| existing.value.clone());

    if let Some(ref schema) = req.schema {
        let schema = Value::from(schema);

        let jschema = JSONSchema::options()
            .with_draft(Draft::Draft7)
            .compile(&schema)
            .map_err(|e| {
                log::info!("Failed to compile JSON schema: {e}");
                bad_argument!("Invalid JSON schema.")
            })?;

        jschema.validate(&value).map_err(|e| {
            let verrors = e.collect::<Vec<ValidationError>>();
            validation_error!(
                "Schema validation failed: {}",
                validation_err_to_str(verrors)
                    .first()
                    .unwrap_or(&String::new())
            )
        })?;
    }

    if let Some(ref validation_function_name) = req.value_validation_function_name {
        let value = req.value.clone().unwrap_or_else(|| existing.value.clone());

        validate_default_config_with_function(
            &mut conn,
            validation_function_name,
            &key_str,
            &value,
            &workspace_context.schema_name,
        )?
    }

    if let Some(ref value_compute_function_name) = req.value_compute_function_name {
        validate_fn_published(
            value_compute_function_name,
            FunctionType::ValueCompute,
            &mut conn,
            &workspace_context.schema_name,
        )?;
    }

    let (db_row, version_id) =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let change_reason = req.change_reason.clone();
            let val = diesel::update(dsl::default_configs)
                .filter(dsl::key.eq(key_str.clone()))
                .set((
                    req,
                    dsl::last_modified_at.eq(Utc::now()),
                    dsl::last_modified_by.eq(user.get_email()),
                ))
                .schema_name(&workspace_context.schema_name)
                .get_result::<DefaultConfig>(transaction_conn)?;

            let version_id = add_config_version(
                &state,
                tags.clone(),
                change_reason.into(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;

            Ok((val, version_id))
        })?;

    #[cfg(feature = "high-performance-mode")]
    put_config_in_redis(version_id, state, &schema_name, &mut conn).await?;

    let mut http_resp = HttpResponse::Ok();
    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        version_id.to_string(),
    ));
    Ok(http_resp.json(db_row))
}

fn validate_fn_published(
    function: &Option<String>,
    f_type: FunctionType,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    let Some(func_name) = function else {
        return Ok(());
    };
    check_fn_published(func_name, f_type, conn, schema_name)
}

fn validate_default_config_with_function(
    conn: &mut DBConnection,
    function_name: &Option<String>,
    key: &str,
    value: &Value,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    if let Some(f_name) = function_name {
        let FunctionInfo {
            published_code: function_code,
            published_runtime_version: function_version,
            ..
        } = get_published_function_code(
            conn,
            f_name,
            FunctionType::ValueValidation,
            schema_name,
        )
        .map_err(|_| {
            bad_argument!("Function {}'s published code does not exist.", f_name)
        })?;
        if let (Some(f_code), Some(f_version)) = (function_code, function_version) {
            validation_function_executor(
                f_name.as_str(),
                &f_code,
                &FunctionExecutionRequest::ValueValidationFunctionRequest {
                    key: key.to_string(),
                    value: value.clone(),
                    r#type: KeyType::ConfigKey,
                    environment: FunctionEnvironment::default(),
                },
                f_version,
                conn,
                schema_name,
            )?;
        }
    }
    Ok(())
}

fn fetch_default_key(
    key: &String,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<models::DefaultConfig> {
    let res = dsl::default_configs
        .filter(schema::default_configs::key.eq(key))
        .select(models::DefaultConfig::as_select())
        .schema_name(schema_name)
        .get_result(conn)?;
    Ok(res)
}

#[authorized]
#[get("")]
async fn list_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    pagination: Query<PaginationParams>,
    filters: Query<DefaultConfigFilters>,
) -> superposition::Result<Json<PaginatedResponse<DefaultConfig>>> {
    let DbConnection(mut conn) = db_conn;

    let filters = filters.into_inner();

    let query_builder = |filters: &DefaultConfigFilters| {
        let mut builder = dsl::default_configs
            .schema_name(&workspace_context.schema_name)
            .into_boxed();
        if let Some(ref config_name) = filters.name {
            builder = builder
                .filter(schema::default_configs::key.like(format!["%{}%", config_name]));
        }
        builder
    };

    if let Some(true) = pagination.all {
        let result: Vec<DefaultConfig> =
            query_builder(&filters).get_results(&mut conn)?;
        return Ok(Json(PaginatedResponse::all(result)));
    }

    let base_query = query_builder(&filters);
    let count_query = query_builder(&filters);

    let n_default_configs: i64 = count_query.count().get_result(&mut conn)?;
    let limit = pagination.count.unwrap_or(10);
    let mut builder = base_query.order(dsl::created_at.desc()).limit(limit);
    if let Some(page) = pagination.page {
        let offset = (page - 1) * limit;
        builder = builder.offset(offset);
    }
    let result: Vec<DefaultConfig> = builder.load(&mut conn)?;
    let total_pages = (n_default_configs as f64 / limit as f64).ceil() as i64;
    Ok(Json(PaginatedResponse {
        total_pages,
        total_items: n_default_configs,
        data: result,
    }))
}

pub fn get_key_usage_context_ids(
    key: &str,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Vec<String>> {
    let result: Vec<Context> =
        contexts
            .schema_name(schema_name)
            .load(conn)
            .map_err(|err| {
                log::error!("failed to fetch contexts with error: {}", err);
                db_error!(err)
            })?;

    let mut context_ids = vec![];
    for context in result.iter() {
        context
            .override_
            .get(key)
            .map_or((), |_| context_ids.push(context.id.to_owned()))
    }
    Ok(context_ids)
}

#[authorized]
#[delete("/{key}")]
async fn delete_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    path: Path<DefaultConfigKey>,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let key: String = path.into_inner().into();
    let mut version_id = 0;

    let context_ids =
        get_key_usage_context_ids(&key, &mut conn, &workspace_context.schema_name)
            .map_err(|_| unexpected_error!("Something went wrong"))?;
    if context_ids.is_empty() {
        let resp: Result<HttpResponse, superposition::AppError> =
            conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
                diesel::update(dsl::default_configs)
                    .filter(dsl::key.eq(&key))
                    .set((
                        dsl::last_modified_at.eq(Utc::now()),
                        dsl::last_modified_by.eq(user.get_email()),
                    ))
                    .schema_name(&workspace_context.schema_name)
                    .execute(transaction_conn)?;

                let deleted_row =
                    diesel::delete(dsl::default_configs.filter(dsl::key.eq(&key)))
                        .schema_name(&workspace_context.schema_name)
                        .execute(transaction_conn);
                match deleted_row {
                    Ok(0) => {
                        Err(not_found!("default config key `{}` doesn't exists", key))
                    }
                    Ok(_) => {
                        let config_version_desc = Description::try_from(format!(
                            "Context Deleted by {}",
                            user.get_email()
                        ))
                        .map_err(|e| unexpected_error!(e))?;
                        version_id = add_config_version(
                            &state,
                            tags,
                            config_version_desc,
                            transaction_conn,
                            &workspace_context.schema_name,
                        )?;
                        log::info!(
                            "default config key: {key} deleted by {}",
                            user.get_email()
                        );
                        Ok(HttpResponse::NoContent()
                            .insert_header((
                                AppHeader::XConfigVersion.to_string(),
                                version_id.to_string(),
                            ))
                            .finish())
                    }
                    Err(e) => {
                        log::error!("default config delete query failed with error: {e}");
                        Err(unexpected_error!("Something went wrong."))
                    }
                }
            });

        if resp.is_ok() {
            #[cfg(feature = "high-performance-mode")]
            put_config_in_redis(version_id, state, &schema_name, &mut conn).await?;
        }
        resp
    } else {
        Err(bad_argument!(
            "Given key already in use in contexts: {}",
            context_ids.join(",")
        ))
    }
}
