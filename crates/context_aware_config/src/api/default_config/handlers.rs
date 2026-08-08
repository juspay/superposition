use std::ops::Deref;

use actix_web::{
    HttpResponse, Scope, delete, get, post, put, routes,
    web::{Data, Json, Path, Query},
};
use chrono::Utc;
use diesel::{
    Connection, ExpressionMethods, OptionalExtension, QueryDsl, RunQueryDsl,
    SelectableHelper, TextExpressionMethods,
};
use service_utils::{
    helpers::{WebhookData, execute_webhook_call, parse_config_tags},
    service::types::{
        AppHeader, AppState, CustomHeaders, DbConnection, SchemaName, WorkspaceContext,
        WorkspaceLockTtlPolicy, WorkspaceWritePermit,
    },
};
use superposition_derives::{authorized, declare_resource};
use superposition_macros::{bad_argument, db_error, not_found, unexpected_error};
use superposition_types::{
    DBConnection, PaginatedResponse, Resource, User,
    api::{
        default_config::{
            BulkOperation, BulkOperationResponse, DefaultConfigAction,
            DefaultConfigBulkResponse, DefaultConfigCreateRequest, DefaultConfigFilters,
            DefaultConfigKey, DefaultConfigUpdateRequest,
        },
        webhook::Action,
    },
    custom_query::PaginationParams,
    database::{
        models::{
            Description,
            cac::{self as models, Context, DefaultConfig},
            others::WebhookEvent,
        },
        schema::{self, contexts::dsl::contexts, default_configs::dsl},
    },
    result as superposition,
};

use crate::helpers::{add_config_version, put_config_in_redis, validate_bulk_size};

use super::validations::{
    validate_bulk_operation, validate_create_request, validate_delete_request,
    validate_update_request,
};

declare_resource!(DefaultConfig);

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create_handler)
        .service(
            Scope::new("/bulk-operations")
                .app_data(WorkspaceLockTtlPolicy::Batch)
                .service(bulk_operations_handler),
        )
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
    mut write_permit: WorkspaceWritePermit,
    user: User,
) -> superposition::Result<HttpResponse> {
    let req = request.into_inner();
    _auth_z.authorized(&[req.key.deref()]).await?;
    let conn = write_permit.connection();

    let tags = parse_config_tags(custom_headers.config_tags)?;
    validate_create_request(&req, &workspace_context, conn, &state.master_encryption_key)
        .await?;

    let default_config = DefaultConfig {
        key: req.key.into(),
        value: req.value,
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

    let config_version =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            diesel::insert_into(dsl::default_configs)
                .values(&default_config)
                .returning(DefaultConfig::as_returning())
                .schema_name(&workspace_context.schema_name)
                .execute(transaction_conn)?;

            let config_version = add_config_version(
                &state,
                tags,
                req.change_reason.into(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;
            Ok(config_version)
        })?;

    let _ = put_config_in_redis(
        &config_version,
        &state,
        &workspace_context.schema_name,
        conn,
    )
    .await;

    let data = WebhookData {
        payload: &default_config,
        resource: Resource::DefaultConfig,
        event: WebhookEvent::ConfigChanged,
        config_version_opt: Some(config_version.id.to_string()),
        action: Action::Create,
    };

    let webhook_status =
        execute_webhook_call(data, &workspace_context, &state, conn).await;

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            actix_web::http::StatusCode::from_u16(512)
                .unwrap_or(actix_web::http::StatusCode::INTERNAL_SERVER_ERROR),
        )
    };

    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        config_version.id.to_string(),
    ));

    Ok(http_resp.json(default_config))
}

#[authorized]
#[put("")]
async fn bulk_operations_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    request: Json<BulkOperation>,
    mut write_permit: WorkspaceWritePermit,
    user: User,
) -> superposition::Result<HttpResponse> {
    let conn = write_permit.connection();
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let operations = request.into_inner().operations;
    validate_bulk_size(operations.len())?;
    let mut change_reasons = Vec::with_capacity(operations.len());

    for operation in &operations {
        change_reasons.push(
            validate_bulk_operation(
                operation,
                &workspace_context,
                conn,
                &state.master_encryption_key,
                &user,
            )
            .await?,
        );
    }

    let (output, webhook_rows, actions, config_version) = conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let mut output = Vec::with_capacity(operations.len());
            let mut webhook_rows = Vec::with_capacity(operations.len());
            let mut actions = Vec::with_capacity(operations.len());

            for operation in operations {
                match operation {
                    DefaultConfigAction::Create(request) => {
                        let now = Utc::now();
                        let email = user.get_email();
                        let row = DefaultConfig {
                            key: request.key.into(),
                            value: request.value,
                            schema: request.schema,
                            value_validation_function_name: request
                                .value_validation_function_name,
                            value_compute_function_name: request
                                .value_compute_function_name,
                            created_at: now,
                            created_by: email.clone(),
                            last_modified_at: now,
                            last_modified_by: email,
                            description: request.description,
                            change_reason: request.change_reason,
                        };
                        let row = diesel::insert_into(dsl::default_configs)
                            .values(&row)
                            .returning(DefaultConfig::as_returning())
                            .schema_name(&workspace_context.schema_name)
                            .get_result::<DefaultConfig>(transaction_conn)?;
                        output.push(DefaultConfigBulkResponse::Create(row.clone()));
                        webhook_rows.push(row);
                        actions.push(Action::Create);
                    }
                    DefaultConfigAction::Update { key, request } => {
                        let key: String = key.into();
                        let row = diesel::update(
                            dsl::default_configs.filter(dsl::key.eq(&key)),
                        )
                        .set((
                            request,
                            dsl::last_modified_at.eq(Utc::now()),
                            dsl::last_modified_by.eq(user.get_email()),
                        ))
                        .returning(DefaultConfig::as_returning())
                        .schema_name(&workspace_context.schema_name)
                        .get_result::<DefaultConfig>(transaction_conn)?;
                        output.push(DefaultConfigBulkResponse::Update(row.clone()));
                        webhook_rows.push(row);
                        actions.push(Action::Update);
                    }
                    DefaultConfigAction::Delete(key) => {
                        let key: String = key.into();
                        let row = diesel::delete(
                            dsl::default_configs.filter(dsl::key.eq(&key)),
                        )
                        .schema_name(&workspace_context.schema_name)
                        .returning(DefaultConfig::as_returning())
                        .get_result::<DefaultConfig>(transaction_conn)?;
                        output.push(DefaultConfigBulkResponse::Delete(format!(
                            "{} deleted successfully",
                            row.key
                        )));
                        webhook_rows.push(row);
                        actions.push(Action::Delete);
                    }
                }
            }

            let config_version = add_config_version(
                &state,
                tags,
                Description::try_from_change_reasons(change_reasons).unwrap_or_default(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;
            Ok((output, webhook_rows, actions, config_version))
        })?;

    let _ = put_config_in_redis(
        &config_version,
        &state,
        &workspace_context.schema_name,
        conn,
    )
    .await;
    let webhook_status = execute_webhook_call(
        WebhookData {
            payload: &webhook_rows,
            resource: Resource::DefaultConfig,
            event: WebhookEvent::ConfigChanged,
            config_version_opt: Some(config_version.id.to_string()),
            action: Action::Batch(actions),
        },
        &workspace_context,
        &state,
        conn,
    )
    .await;
    let mut response = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            actix_web::http::StatusCode::from_u16(512)
                .unwrap_or(actix_web::http::StatusCode::INTERNAL_SERVER_ERROR),
        )
    };
    response.insert_header((
        AppHeader::XConfigVersion.to_string(),
        config_version.id.to_string(),
    ));
    Ok(response.json(BulkOperationResponse { output }))
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
    mut write_permit: WorkspaceWritePermit,
    user: User,
) -> superposition::Result<HttpResponse> {
    let key = key.into_inner();
    _auth_z.authorized(&[key.deref()]).await?;

    let req = request.into_inner();
    let key_str = key.into();
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let conn = write_permit.connection();

    let existing = fetch_default_key(&key_str, conn, &workspace_context.schema_name)
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

    validate_update_request(
        &key_str,
        &req,
        &existing,
        &workspace_context,
        conn,
        &state.master_encryption_key,
    )
    .await?;

    let (db_row, config_version) =
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

            let config_version = add_config_version(
                &state,
                tags.clone(),
                change_reason.into(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;

            Ok((val, config_version))
        })?;

    let _ = put_config_in_redis(
        &config_version,
        &state,
        &workspace_context.schema_name,
        conn,
    )
    .await;

    let data = WebhookData {
        payload: &db_row,
        resource: Resource::DefaultConfig,
        event: WebhookEvent::ConfigChanged,
        config_version_opt: Some(config_version.id.to_string()),
        action: Action::Update,
    };

    let webhook_status =
        execute_webhook_call(data, &workspace_context, &state, conn).await;

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            actix_web::http::StatusCode::from_u16(512)
                .unwrap_or(actix_web::http::StatusCode::INTERNAL_SERVER_ERROR),
        )
    };
    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        config_version.id.to_string(),
    ));
    Ok(http_resp.json(db_row))
}

pub(super) fn fetch_default_key(
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
    mut write_permit: WorkspaceWritePermit,
    user: User,
) -> superposition::Result<HttpResponse> {
    let key = path.into_inner();
    _auth_z.authorized(&[key.deref()]).await?;

    let tags = parse_config_tags(custom_headers.config_tags)?;

    let key: String = key.into();

    let conn = write_permit.connection();

    validate_delete_request(&key, conn, &workspace_context.schema_name)?;

    let (config_version, default_config) = conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
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
                    .get_result::<DefaultConfig>(transaction_conn)
                    .optional()?;
            match deleted_row {
                None => Err(not_found!("default config key `{}` doesn't exists", key))?,
                Some(default_config) => {
                    let config_version_desc = Description::try_from(format!(
                        "Context Deleted by {}",
                        user.get_email()
                    ))
                    .map_err(|e| unexpected_error!(e))?;
                    let config_version = add_config_version(
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
                    Ok((config_version, default_config))
                }
            }
        })?;

    let _ = put_config_in_redis(
        &config_version,
        &state,
        &workspace_context.schema_name,
        conn,
    )
    .await;

    let data = WebhookData {
        payload: &default_config,
        resource: Resource::DefaultConfig,
        event: WebhookEvent::ConfigChanged,
        config_version_opt: Some(config_version.id.to_string()),
        action: Action::Delete,
    };

    let webhook_status =
        execute_webhook_call(data, &workspace_context, &state, conn).await;

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            actix_web::http::StatusCode::from_u16(512)
                .unwrap_or(actix_web::http::StatusCode::INTERNAL_SERVER_ERROR),
        )
    };
    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        config_version.id.to_string(),
    ));

    Ok(http_resp.finish())
}
