use actix_web::{
    HttpResponse, Scope, delete, get, post, put, routes,
    web::{self, Data, Json, Path, Query},
};
use chrono::Utc;
use diesel::{
    Connection, ExpressionMethods, OptionalExtension, QueryDsl, RunQueryDsl,
    SelectableHelper,
};
use serde_json::Value;
use service_utils::{
    helpers::{WebhookData, execute_webhook_call, parse_config_tags},
    service::types::{
        AppHeader, AppState, CustomHeaders, DbConnection, WorkspaceContext,
        WorkspaceLockTtlPolicy, WorkspaceWritePermit,
    },
};
use superposition_derives::{authorized, declare_resource};
use superposition_macros::{bad_argument, db_error, not_found, unexpected_error};
use superposition_types::{
    PaginatedResponse, Resource, User,
    api::{
        dimension::{
            BulkOperation, BulkOperationResponse, CreateRequest, DeleteRequest,
            DimensionAction, DimensionBulkResponse, DimensionName, DimensionResponse,
            UpdateRequest,
        },
        webhook::Action,
    },
    custom_query::PaginationParams,
    database::{
        models::{
            Description,
            cac::{DependencyGraph, Dimension, DimensionType},
            others::WebhookEvent,
        },
        schema::dimensions::{self, dsl::*},
    },
    result as superposition,
};

use crate::api::dimension::operations::{refresh_dependency_graphs, upsert_dimension};
use crate::helpers::{put_config_in_redis, validate_bulk_size};
use crate::{
    api::dimension::{
        utils::{
            create_connections_with_dependents, get_dimension_usage_context_ids,
            remove_connections_with_dependents,
        },
        validations::{
            does_dimension_exist_for_cohorting, validate_bulk_operation,
            validate_cohort_position, validate_cohort_schema,
            validate_dimension_functions, validate_dimension_position,
            validate_dimension_schema, validate_position_wrt_dependency,
        },
    },
    helpers::{add_config_version, validate_change_reason},
};

declare_resource!(Dimension);

fn dimension_from_create_request(req: CreateRequest, email: String) -> Dimension {
    let now = Utc::now();
    Dimension {
        dimension: req.dimension.into(),
        position: req.position,
        schema: req.schema,
        value_validation_function_name: req.value_validation_function_name,
        created_at: now,
        created_by: email.clone(),
        last_modified_at: now,
        last_modified_by: email,
        description: req.description,
        change_reason: req.change_reason,
        dependency_graph: DependencyGraph::default(),
        value_compute_function_name: req.value_compute_function_name,
        dimension_type: req.dimension_type,
    }
}

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
    req: web::Json<CreateRequest>,
    user: User,
    custom_headers: CustomHeaders,
    mut write_permit: WorkspaceWritePermit,
) -> superposition::Result<HttpResponse> {
    let conn = write_permit.connection();
    let create_req = req.into_inner();
    let schema_value = Value::from(&create_req.schema);
    let tags = parse_config_tags(custom_headers.config_tags)?;

    validate_change_reason(
        &workspace_context,
        &create_req.change_reason,
        conn,
        &state.master_encryption_key,
    )
    .await?;

    let num_rows = dimensions
        .count()
        .schema_name(&workspace_context.schema_name)
        .get_result::<i64>(conn)
        .map_err(|err| {
            log::error!("failed to fetch number of dimension with error: {}", err);
            db_error!(err)
        })?;

    validate_dimension_position(
        create_req.dimension.clone(),
        create_req.position,
        num_rows,
    )?;

    match create_req.dimension_type {
        DimensionType::Regular {} => {
            validate_dimension_schema(&create_req.dimension_type, &create_req.schema)?;
        }
        DimensionType::RemoteCohort(ref cohort_based_on) => {
            validate_dimension_schema(&create_req.dimension_type, &create_req.schema)?;
            let based_on_dimension = does_dimension_exist_for_cohorting(
                cohort_based_on,
                &workspace_context.schema_name,
                conn,
            )?;
            validate_cohort_position(&create_req.position, &based_on_dimension, true)?;
        }
        DimensionType::LocalCohort(ref cohort_based_on) => {
            let based_on_dimension = validate_cohort_schema(
                &schema_value,
                cohort_based_on,
                &workspace_context.schema_name,
                conn,
            )?;
            validate_cohort_position(&create_req.position, &based_on_dimension, true)?;
        }
    }

    validate_dimension_functions(
        &create_req.dimension_type,
        &create_req.value_validation_function_name,
        &create_req.value_compute_function_name,
        conn,
        &workspace_context.schema_name,
    )?;

    let validation_function_name = create_req.value_validation_function_name.clone();
    let dimension_data = dimension_from_create_request(create_req, user.get_email());

    let (inserted_dimension, is_mandatory, config_version) =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            diesel::update(dimensions::table)
                .filter(dimensions::position.ge(dimension_data.position))
                .set((
                    last_modified_at.eq(Utc::now()),
                    last_modified_by.eq(user.get_email()),
                    dimensions::position.eq(dimensions::position + 1),
                ))
                .returning(Dimension::as_returning())
                .schema_name(&workspace_context.schema_name)
                .execute(transaction_conn)?;

            match dimension_data.dimension_type {
                DimensionType::LocalCohort(ref cohort_based_on)
                | DimensionType::RemoteCohort(ref cohort_based_on) => {
                    // Update dependency graphs of all dimensions that
                    // depend on the cohort_based_on dimension as well as
                    // the cohorted dimension itself
                    create_connections_with_dependents(
                        cohort_based_on,
                        &dimension_data.dimension,
                        &user.get_email(),
                        &workspace_context.schema_name,
                        transaction_conn,
                    )?
                }
                DimensionType::Regular {} => (),
            }

            let insert_resp = diesel::insert_into(dimensions::table)
                .values(&dimension_data)
                .returning(Dimension::as_returning())
                .schema_name(&workspace_context.schema_name)
                .get_result(transaction_conn);

            match insert_resp {
                Ok(inserted_dimension) => {
                    let is_mandatory = workspace_context
                        .settings
                        .mandatory_dimensions
                        .clone()
                        .unwrap_or_default()
                        .contains(&inserted_dimension.dimension);

                    let config_version = add_config_version(
                        &state,
                        tags,
                        dimension_data.change_reason.into(),
                        transaction_conn,
                        &workspace_context.schema_name,
                    )?;
                    Ok((inserted_dimension, is_mandatory, config_version))
                }
                Err(diesel::result::Error::DatabaseError(
                    diesel::result::DatabaseErrorKind::ForeignKeyViolation,
                    e,
                )) => {
                    let fun_name = validation_function_name.clone();
                    log::error!("{fun_name:?} function not found with error: {e:?}");
                    Err(bad_argument!(
                        "Function {} doesn't exists",
                        Into::<Option<String>>::into(validation_function_name.clone())
                            .unwrap_or_default()
                    ))
                }
                Err(e) => {
                    log::error!("Dimension create failed with error: {e}");
                    Err(db_error!(e))
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
        payload: &inserted_dimension,
        resource: Resource::Dimension,
        event: WebhookEvent::ConfigChanged,
        config_version_opt: Some(config_version.id.to_string()),
        action: Action::Create,
    };

    let webhook_status =
        execute_webhook_call(data, &workspace_context, &state, conn).await;

    let mut http_resp = if webhook_status {
        HttpResponse::Created()
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
    Ok(http_resp.json(DimensionResponse::new(inserted_dimension, is_mandatory)))
}

#[allow(clippy::too_many_arguments)]
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
    validate_bulk_size(request.operations.len())?;
    let operations = request.into_inner().operations;
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

    let mut actions = Vec::with_capacity(operations.len());
    let mut affected_dimensions = Vec::with_capacity(operations.len());
    let (output, config_version) =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let mut output = Vec::with_capacity(operations.len());
            let mut written = Vec::new();
            for operation in operations {
                match operation {
                    DimensionAction::Create(request) => {
                        let row =
                            dimension_from_create_request(request, user.get_email());
                        let (created, row) = upsert_dimension(
                            transaction_conn,
                            &workspace_context.schema_name,
                            row,
                        )?;
                        if !created {
                            return Err(bad_argument!(
                                "Dimension '{}' already exists",
                                row.dimension
                            ));
                        }
                        let mandatory = workspace_context
                            .settings
                            .mandatory_dimensions
                            .as_ref()
                            .is_some_and(|mandatory_dimensions| {
                                mandatory_dimensions.contains(&row.dimension)
                            });
                        output.push(DimensionBulkResponse::Create(
                            DimensionResponse::new(row.clone(), mandatory),
                        ));
                        actions.push(Action::Create);
                        affected_dimensions.push(row.clone());
                        written.push((true, row));
                    }
                    DimensionAction::Update {
                        dimension: dimension_name,
                        request,
                    } => {
                        let name: String = dimension_name.into();
                        let existing = dimensions
                            .filter(dimension.eq(&name))
                            .schema_name(&workspace_context.schema_name)
                            .get_result::<Dimension>(transaction_conn)?;
                        let row = Dimension {
                            dimension: existing.dimension,
                            position: request.position.unwrap_or(existing.position),
                            schema: request.schema.unwrap_or(existing.schema),
                            value_validation_function_name: request
                                .value_validation_function_name
                                .unwrap_or(existing.value_validation_function_name),
                            created_at: existing.created_at,
                            created_by: existing.created_by,
                            last_modified_at: Utc::now(),
                            last_modified_by: user.get_email(),
                            description: request
                                .description
                                .unwrap_or(existing.description),
                            change_reason: request.change_reason,
                            dependency_graph: existing.dependency_graph,
                            value_compute_function_name: request
                                .value_compute_function_name
                                .unwrap_or(existing.value_compute_function_name),
                            dimension_type: existing.dimension_type,
                        };
                        let (created, row) = upsert_dimension(
                            transaction_conn,
                            &workspace_context.schema_name,
                            row,
                        )?;
                        if created {
                            return Err(bad_argument!(
                                "Dimension '{}' does not exist",
                                row.dimension
                            ));
                        }
                        let mandatory = workspace_context
                            .settings
                            .mandatory_dimensions
                            .as_ref()
                            .is_some_and(|mandatory_dimensions| {
                                mandatory_dimensions.contains(&row.dimension)
                            });
                        output.push(DimensionBulkResponse::Update(
                            DimensionResponse::new(row.clone(), mandatory),
                        ));
                        actions.push(Action::Update);
                        affected_dimensions.push(row.clone());
                        written.push((false, row));
                    }
                    DimensionAction::Delete(request) => {
                        let name: String = request.into();
                        let row = dimensions
                            .filter(dimension.eq(&name))
                            .schema_name(&workspace_context.schema_name)
                            .get_result::<Dimension>(transaction_conn)?;
                        if !row.dependency_graph.is_empty() {
                            return Err(bad_argument!(
                                "Dimension {} has dependent cohort dimensions",
                                row.dimension
                            ));
                        }
                        if let DimensionType::LocalCohort(parent)
                        | DimensionType::RemoteCohort(parent) = &row.dimension_type
                        {
                            remove_connections_with_dependents(
                                &row.dimension,
                                parent,
                                &user.get_email(),
                                &workspace_context.schema_name,
                                transaction_conn,
                            )?;
                        }
                        diesel::delete(dimensions.filter(dimension.eq(&row.dimension)))
                            .schema_name(&workspace_context.schema_name)
                            .execute(transaction_conn)?;
                        diesel::update(dimensions.filter(position.gt(row.position)))
                            .set(position.eq(position - 1))
                            .schema_name(&workspace_context.schema_name)
                            .execute(transaction_conn)?;
                        output.push(DimensionBulkResponse::Delete(format!(
                            "{} deleted successfully",
                            row.dimension
                        )));
                        actions.push(Action::Delete);
                        affected_dimensions.push(row);
                    }
                }
            }

            let dimension_count = dimensions
                .count()
                .schema_name(&workspace_context.schema_name)
                .get_result::<i64>(transaction_conn)?;
            for (created, row) in &written {
                validate_dimension_position(
                    DimensionName::try_from(row.dimension.clone()).map_err(|error| {
                        bad_argument!(
                            "Invalid dimension name '{}': {}",
                            row.dimension,
                            error
                        )
                    })?,
                    row.position,
                    dimension_count - 1,
                )?;

                let based_on = match &row.dimension_type {
                    DimensionType::Regular {} => continue,
                    DimensionType::RemoteCohort(parent) => {
                        does_dimension_exist_for_cohorting(
                            parent,
                            &workspace_context.schema_name,
                            transaction_conn,
                        )?
                    }
                    DimensionType::LocalCohort(parent) => validate_cohort_schema(
                        &Value::from(&row.schema),
                        parent,
                        &workspace_context.schema_name,
                        transaction_conn,
                    )?,
                };
                validate_cohort_position(&row.position, &based_on, *created)?;
            }

            refresh_dependency_graphs(transaction_conn, &workspace_context.schema_name)?;
            let config_version = add_config_version(
                &state,
                tags,
                Description::try_from_change_reasons(change_reasons).unwrap_or_default(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;
            Ok((output, config_version))
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
            payload: &affected_dimensions,
            resource: Resource::Dimension,
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
#[get("/{name}")]
async fn get_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    req: Path<String>,
) -> superposition::Result<Json<DimensionResponse>> {
    let DbConnection(mut conn) = db_conn;

    let result: Dimension = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(req.into_inner()))
        .schema_name(&workspace_context.schema_name)
        .get_result::<Dimension>(&mut conn)?;

    let is_mandatory = workspace_context
        .settings
        .mandatory_dimensions
        .unwrap_or_default()
        .contains(&result.dimension);

    Ok(Json(DimensionResponse::new(result, is_mandatory)))
}

#[allow(clippy::too_many_arguments)]
#[authorized]
#[routes]
#[put("/{name}")]
#[patch("/{name}")]
async fn update_handler(
    workspace_context: WorkspaceContext,
    path: Path<DimensionName>,
    state: Data<AppState>,
    req: web::Json<UpdateRequest>,
    user: User,
    custom_headers: CustomHeaders,
    mut write_permit: WorkspaceWritePermit,
) -> superposition::Result<HttpResponse> {
    let name: String = path.clone().into();
    use dimensions::dsl;
    let conn = write_permit.connection();
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let update_req = req.into_inner();

    validate_change_reason(
        &workspace_context,
        &update_req.change_reason,
        conn,
        &state.master_encryption_key,
    )
    .await?;

    let dimension_data: Dimension = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(name.clone()))
        .schema_name(&workspace_context.schema_name)
        .get_result::<Dimension>(conn)?;

    let num_rows = dimensions
        .count()
        .schema_name(&workspace_context.schema_name)
        .get_result::<i64>(conn)
        .map_err(|err| {
            log::error!("failed to fetch number of dimension with error: {}", err);
            db_error!(err)
        })?;

    if let Some(ref new_schema) = update_req.schema {
        let schema_value = Value::from(new_schema);
        match dimension_data.dimension_type {
            DimensionType::Regular {} | DimensionType::RemoteCohort(_) => {
                validate_dimension_schema(&dimension_data.dimension_type, new_schema)?;
            }
            DimensionType::LocalCohort(ref cohort_based_on) => {
                validate_cohort_schema(
                    &schema_value,
                    cohort_based_on,
                    &workspace_context.schema_name,
                    conn,
                )?;
            }
        }
    }

    if let Some(ref new_position) = update_req.position {
        match dimension_data.dimension_type {
            DimensionType::Regular {} => (),
            DimensionType::RemoteCohort(ref cohort_based_on)
            | DimensionType::LocalCohort(ref cohort_based_on) => {
                let based_on_dimension = does_dimension_exist_for_cohorting(
                    cohort_based_on,
                    &workspace_context.schema_name,
                    conn,
                )?;
                validate_cohort_position(new_position, &based_on_dimension, false)?;
            }
        }
    }

    if update_req.value_validation_function_name.is_some()
        || update_req.value_compute_function_name.is_some()
    {
        let validation_function = update_req
            .value_validation_function_name
            .as_ref()
            .unwrap_or(&dimension_data.value_validation_function_name);
        let compute_function = update_req
            .value_compute_function_name
            .as_ref()
            .unwrap_or(&dimension_data.value_compute_function_name);
        validate_dimension_functions(
            &dimension_data.dimension_type,
            validation_function,
            compute_function,
            conn,
            &workspace_context.schema_name,
        )?;
    }

    let update_change_reason = update_req.change_reason.clone();

    let (result, is_mandatory, config_version) = conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            if let Some(position_val) = update_req.position {
                let new_position = position_val;
                validate_dimension_position(
                    path.into_inner(),
                    position_val,
                    num_rows - 1,
                )?;
                validate_position_wrt_dependency(
                    &name,
                    &position_val,
                    transaction_conn,
                    &workspace_context.schema_name,
                )?;
                let previous_position = dimension_data.position;

                diesel::update(dimensions)
                    .filter(dsl::dimension.eq(&name))
                    .set((
                        dsl::last_modified_at.eq(Utc::now()),
                        dsl::last_modified_by.eq(user.get_email()),
                        dimensions::position.eq((num_rows + 100) as i32),
                    ))
                    .returning(Dimension::as_returning())
                    .schema_name(&workspace_context.schema_name)
                    .get_result::<Dimension>(transaction_conn)?;

                if previous_position < new_position {
                    diesel::update(dsl::dimensions)
                        .filter(dimensions::position.gt(previous_position))
                        .filter(dimensions::position.le(&new_position))
                        .set((
                            dsl::last_modified_at.eq(Utc::now()),
                            dsl::last_modified_by.eq(user.get_email()),
                            dimensions::position.eq(dimensions::position - 1),
                        ))
                        .returning(Dimension::as_returning())
                        .schema_name(&workspace_context.schema_name)
                        .execute(transaction_conn)?
                } else {
                    diesel::update(dsl::dimensions)
                        .filter(dimensions::position.lt(previous_position))
                        .filter(dimensions::position.ge(&new_position))
                        .set((
                            dsl::last_modified_at.eq(Utc::now()),
                            dsl::last_modified_by.eq(user.get_email()),
                            dimensions::position.eq(dimensions::position + 1),
                        ))
                        .returning(Dimension::as_returning())
                        .schema_name(&workspace_context.schema_name)
                        .execute(transaction_conn)?
                };
            }

            let result = diesel::update(dimensions)
                .filter(dsl::dimension.eq(name))
                .set((
                    update_req,
                    dimensions::last_modified_at.eq(Utc::now()),
                    dimensions::last_modified_by.eq(user.get_email()),
                ))
                .returning(Dimension::as_returning())
                .schema_name(&workspace_context.schema_name)
                .get_result::<Dimension>(transaction_conn)
                .map_err(|err| db_error!(err))?;

            let is_mandatory = workspace_context
                .settings
                .mandatory_dimensions
                .clone()
                .unwrap_or_default()
                .contains(&result.dimension);

            let config_version = add_config_version(
                &state,
                tags,
                update_change_reason.into(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;

            Ok((result, is_mandatory, config_version))
        })?;

    let _ = put_config_in_redis(
        &config_version,
        &state,
        &workspace_context.schema_name,
        conn,
    )
    .await;

    let data = WebhookData {
        payload: &result,
        resource: Resource::Dimension,
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
    Ok(http_resp.json(DimensionResponse::new(result, is_mandatory)))
}

#[authorized]
#[get("")]
async fn list_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
) -> superposition::Result<Json<PaginatedResponse<DimensionResponse>>> {
    let DbConnection(mut conn) = db_conn;

    let (total_pages, total_items, result) = match filters.all {
        Some(true) => {
            let result: Vec<Dimension> = dimensions
                .schema_name(&workspace_context.schema_name)
                .get_results(&mut conn)?;
            (1, result.len() as i64, result)
        }
        _ => {
            let n_dimensions: i64 = dimensions
                .count()
                .schema_name(&workspace_context.schema_name)
                .get_result(&mut conn)?;
            let limit = filters.count.unwrap_or(10);
            let mut builder = dimensions
                .schema_name(&workspace_context.schema_name)
                .order(created_at.desc())
                .limit(limit)
                .into_boxed();
            if let Some(page) = filters.page {
                let offset = (page - 1) * limit;
                builder = builder.offset(offset);
            }
            let result: Vec<Dimension> = builder.load(&mut conn)?;
            let total_pages = (n_dimensions as f64 / limit as f64).ceil() as i64;
            (total_pages, n_dimensions, result)
        }
    };

    let mandatory_dimensions = workspace_context
        .settings
        .mandatory_dimensions
        .unwrap_or_default();

    let dimensions_with_mandatory: Vec<DimensionResponse> = result
        .into_iter()
        .map(|ele| {
            let is_mandatory = mandatory_dimensions.contains(&ele.dimension);
            DimensionResponse::new(ele, is_mandatory)
        })
        .collect();

    Ok(Json(PaginatedResponse {
        total_pages,
        total_items,
        data: dimensions_with_mandatory,
    }))
}

#[authorized]
#[delete("/{name}")]
async fn delete_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    path: Path<DeleteRequest>,
    user: User,
    custom_headers: CustomHeaders,
    mut write_permit: WorkspaceWritePermit,
) -> superposition::Result<HttpResponse> {
    let name: String = path.into_inner().into();
    let conn = write_permit.connection();
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let dimension_data: Dimension = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(&name))
        .select(Dimension::as_select())
        .schema_name(&workspace_context.schema_name)
        .get_result(conn)?;

    let is_mandatory = workspace_context
        .settings
        .mandatory_dimensions
        .as_ref()
        .is_some_and(|dims| dims.contains(&dimension_data.dimension));

    if is_mandatory {
        return Err(bad_argument!(
            "Dimension `{}` is mandatory and cannot be deleted",
            name
        ));
    }

    let context_ids =
        get_dimension_usage_context_ids(&name, conn, &workspace_context.schema_name)?;

    if context_ids.is_empty() {
        let (config_version, dimension_data) = conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            use dimensions::dsl;

            if !dimension_data.dependency_graph.is_empty() {
                return Err(bad_argument!("The dimension {} currently has other dimensions that are using it in their cohort definitions. To delete this dimension, you need to delete these cohorts", &dimension_data.dimension))
            }

            match dimension_data.dimension_type {
                DimensionType::LocalCohort(ref cohort_based_on)
                | DimensionType::RemoteCohort(ref cohort_based_on) => {
                    // Remove dependency graphs of all dimensions that
                    // depend on the cohort_based_on dimension as well as
                    // the cohorted dimension itself
                    remove_connections_with_dependents(
                        &dimension_data.dimension,
                        cohort_based_on,
                        &user.get_email(),
                        &workspace_context.schema_name,
                        transaction_conn,
                    )?
                }
                DimensionType::Regular{} => (),
            }
            diesel::update(dsl::dimensions)
                .filter(dsl::dimension.eq(&name))
                .set((
                    dsl::last_modified_at.eq(Utc::now()),
                    dsl::last_modified_by.eq(user.get_email()),
                ))
                .returning(Dimension::as_returning())
                .schema_name(&workspace_context.schema_name)
                .execute(transaction_conn)?;

            let deleted_row = diesel::delete(dsl::dimensions.filter(dsl::dimension.eq(&name)))
                .schema_name(&workspace_context.schema_name)
                .get_result::<Dimension>(transaction_conn)
                .optional()?;

            diesel::update(dimensions::dsl::dimensions)
                .filter(dimensions::position.gt(dimension_data.position))
                .set(dimensions::position.eq(dimensions::position - 1))
                .returning(Dimension::as_returning())
                .schema_name(&workspace_context.schema_name)
                .execute(transaction_conn)?;

            match deleted_row {
                None => Err(not_found!("Dimension `{}` doesn't exists", name))?,
                Some(dimension_data) => {
                    let config_version_desc = Description::try_from(format!(
                        "Dimension Deleted by {}",
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
                        "Dimension: {name} deleted by {}",
                        user.get_email()
                    );
                    Ok((config_version, dimension_data))
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
            payload: &dimension_data,
            resource: Resource::Dimension,
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
    } else {
        Err(bad_argument!(
            "Given key already in use in contexts: {}",
            context_ids.join(",")
        ))
    }
}
