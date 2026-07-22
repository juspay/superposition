use actix_web::{
    HttpResponse, Scope, delete, get, post, routes,
    web::{self, Data, Json, Path, Query},
};
use chrono::Utc;
use diesel::{
    Connection, ExpressionMethods, OptionalExtension, QueryDsl, RunQueryDsl,
    SelectableHelper,
};
use serde_json::{Map, Value};
use service_utils::{
    helpers::{WebhookData, execute_webhook_call, parse_config_tags},
    service::types::{
        AppHeader, AppState, CustomHeaders, DbConnection, WorkspaceContext,
    },
};
use superposition_core::validations::validate_schema;
use superposition_derives::{authorized, declare_resource};
use superposition_macros::{
    bad_argument, db_error, not_found, unexpected_error, validation_error,
};
use superposition_types::{
    ExtendedMap, PaginatedResponse, Resource, User,
    api::{
        dimension::{
            CreateRequest, DeleteRequest, DimensionName, DimensionResponse, UpdateRequest,
        },
        webhook::Action,
    },
    custom_query::PaginationParams,
    database::{
        models::{
            Description,
            cac::{DefaultConfig, DependencyGraph, Dimension, DimensionType},
            others::WebhookEvent,
        },
        schema::{
            default_configs,
            dimensions::{self, dsl::*},
        },
    },
    logic::{
        build_user_cohort_definition_schema, extract_user_cohort_definitions,
        validate_user_cohort_definitions,
    },
    result as superposition,
};

use crate::api::dimension::validations::allow_primitive_types;
use crate::helpers::put_config_in_redis;
use crate::{
    api::default_config::get_key_usage_context_ids,
    api::dimension::{
        utils::{
            create_connections_with_dependents, get_dimension_usage_context_ids,
            remove_connections_with_dependents,
        },
        validations::{
            does_dimension_exist_for_cohorting, validate_cohort_position,
            validate_cohort_schema, validate_dimension_position,
            validate_position_wrt_dependency, validate_validation_function,
            validate_value_compute_function,
        },
    },
    helpers::{add_config_version, validate_change_reason},
};

declare_resource!(Dimension);

fn generated_user_cohort_config_parts(
    dimension_schema: &Map<String, Value>,
    based_on: &str,
) -> superposition::Result<(Value, ExtendedMap)> {
    let definitions = extract_user_cohort_definitions(dimension_schema)
        .map_err(|error| validation_error!(error))?;
    let value = Value::Object(definitions);
    validate_user_cohort_definitions(&value, dimension_schema, based_on)
        .map_err(|error| validation_error!(error))?;
    let generated_schema = build_user_cohort_definition_schema(dimension_schema)
        .map_err(|error| validation_error!(error))?;

    Ok((value, generated_schema))
}

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
    req: web::Json<CreateRequest>,
    user: User,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let create_req = req.into_inner();
    let schema_value = Value::from(&create_req.schema);
    let tags = parse_config_tags(custom_headers.config_tags)?;

    validate_change_reason(
        &workspace_context,
        &create_req.change_reason,
        &mut conn,
        &state.master_encryption_key,
    )
    .await?;

    let num_rows = dimensions
        .count()
        .schema_name(&workspace_context.schema_name)
        .get_result::<i64>(&mut conn)
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
            allow_primitive_types(&create_req.schema)?;
            validate_schema(&schema_value).map_err(|e| {
                superposition::AppError::ValidationError(format!(
                    "JSON Schema's schema is broken - this is unexpected {}",
                    e.join("")
                ))
            })?;
        }
        DimensionType::RemoteCohort(ref cohort_based_on) => {
            allow_primitive_types(&create_req.schema)?;
            validate_schema(&schema_value).map_err(|e| {
                superposition::AppError::ValidationError(format!(
                    "JSON Schema's schema is broken - this is unexpected {}",
                    e.join("")
                ))
            })?;
            let based_on_dimension = does_dimension_exist_for_cohorting(
                cohort_based_on,
                &workspace_context.schema_name,
                &mut conn,
            )?;
            validate_cohort_position(&create_req.position, &based_on_dimension, true)?;
        }
        DimensionType::LocalCohort(ref cohort_based_on)
        | DimensionType::UserCohort(ref cohort_based_on) => {
            let based_on_dimension = validate_cohort_schema(
                &schema_value,
                cohort_based_on,
                &workspace_context.schema_name,
                &mut conn,
            )?;
            validate_cohort_position(&create_req.position, &based_on_dimension, true)?;
        }
    }

    validate_validation_function(
        &create_req.value_validation_function_name,
        &mut conn,
        &workspace_context.schema_name,
    )?;

    validate_value_compute_function(
        &create_req.dimension_type,
        &create_req.value_compute_function_name,
        &mut conn,
        &workspace_context.schema_name,
    )?;

    let generated_default_config = match &create_req.dimension_type {
        DimensionType::UserCohort(cohort_based_on) => {
            let (value, generated_schema) =
                generated_user_cohort_config_parts(&create_req.schema, cohort_based_on)?;
            let key: String = create_req.dimension.clone().into();
            let now = Utc::now();

            Some(DefaultConfig {
                key,
                value,
                created_at: now,
                created_by: user.get_email(),
                schema: generated_schema,
                value_validation_function_name: None,
                last_modified_at: now,
                last_modified_by: user.get_email(),
                description: create_req.description.clone(),
                change_reason: create_req.change_reason.clone(),
                value_compute_function_name: None,
            })
        }
        _ => None,
    };

    let dimension_data = Dimension {
        dimension: create_req.dimension.into(),
        position: create_req.position,
        schema: create_req.schema,
        created_by: user.get_email(),
        created_at: Utc::now(),
        value_validation_function_name: create_req.value_validation_function_name.clone(),
        last_modified_at: Utc::now(),
        last_modified_by: user.get_email(),
        description: create_req.description,
        change_reason: create_req.change_reason,
        dependency_graph: DependencyGraph::default(),
        value_compute_function_name: create_req.value_compute_function_name,
        dimension_type: create_req.dimension_type,
    };

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
                | DimensionType::RemoteCohort(ref cohort_based_on)
                | DimensionType::UserCohort(ref cohort_based_on) => {
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
                    if let Some(ref generated_default_config) = generated_default_config {
                        diesel::insert_into(default_configs::table)
                            .values(generated_default_config)
                            .returning(DefaultConfig::as_returning())
                            .schema_name(&workspace_context.schema_name)
                            .execute(transaction_conn)
                            .map_err(|error| {
                                if matches!(
                                    error,
                                    diesel::result::Error::DatabaseError(
                                        diesel::result::DatabaseErrorKind::UniqueViolation,
                                        _
                                    )
                                ) {
                                    bad_argument!(
                                        "A default config key named `{}` already exists",
                                        generated_default_config.key
                                    )
                                } else {
                                    db_error!(error)
                                }
                            })?;
                    }

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
                    let fun_name = create_req.value_validation_function_name.clone();
                    log::error!("{fun_name:?} function not found with error: {e:?}");
                    Err(bad_argument!(
                        "Function {} doesn't exists",
                        Into::<Option<String>>::into(
                            create_req.value_validation_function_name.clone()
                        )
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
        &mut conn,
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
        execute_webhook_call(data, &workspace_context, &state, &mut conn).await;

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
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let name: String = path.clone().into();
    use dimensions::dsl;
    let DbConnection(mut conn) = db_conn;
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let update_req = req.into_inner();

    validate_change_reason(
        &workspace_context,
        &update_req.change_reason,
        &mut conn,
        &state.master_encryption_key,
    )
    .await?;

    let dimension_data: Dimension = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(name.clone()))
        .schema_name(&workspace_context.schema_name)
        .get_result::<Dimension>(&mut conn)?;

    let num_rows = dimensions
        .count()
        .schema_name(&workspace_context.schema_name)
        .get_result::<i64>(&mut conn)
        .map_err(|err| {
            log::error!("failed to fetch number of dimension with error: {}", err);
            db_error!(err)
        })?;

    if let Some(ref new_schema) = update_req.schema {
        let schema_value = Value::from(new_schema);
        match dimension_data.dimension_type {
            DimensionType::Regular {} | DimensionType::RemoteCohort(_) => {
                allow_primitive_types(new_schema)?;
                validate_schema(&schema_value).map_err(|e| {
                    superposition::AppError::ValidationError(format!(
                        "JSON Schema's schema is broken - this is unexpected {}",
                        e.join("")
                    ))
                })?;
            }
            DimensionType::LocalCohort(ref cohort_based_on)
            | DimensionType::UserCohort(ref cohort_based_on) => {
                validate_cohort_schema(
                    &schema_value,
                    cohort_based_on,
                    &workspace_context.schema_name,
                    &mut conn,
                )?;
            }
        }
    }

    if let Some(ref new_position) = update_req.position {
        match dimension_data.dimension_type {
            DimensionType::Regular {} => (),
            DimensionType::RemoteCohort(ref cohort_based_on)
            | DimensionType::LocalCohort(ref cohort_based_on)
            | DimensionType::UserCohort(ref cohort_based_on) => {
                let based_on_dimension = does_dimension_exist_for_cohorting(
                    cohort_based_on,
                    &workspace_context.schema_name,
                    &mut conn,
                )?;
                validate_cohort_position(new_position, &based_on_dimension, false)?;
            }
        }
    }

    if let Some(ref fn_name) = update_req.value_validation_function_name {
        validate_validation_function(fn_name, &mut conn, &workspace_context.schema_name)?;
    }

    if let Some(ref value_compute_function_name_) = update_req.value_compute_function_name
    {
        validate_value_compute_function(
            &dimension_data.dimension_type,
            value_compute_function_name_,
            &mut conn,
            &workspace_context.schema_name,
        )?;
    }

    let update_change_reason = update_req.change_reason.clone();
    let generated_default_update = match &dimension_data.dimension_type {
        DimensionType::UserCohort(cohort_based_on) => {
            let dimension_schema =
                update_req.schema.as_ref().unwrap_or(&dimension_data.schema);
            let (value, generated_schema) =
                generated_user_cohort_config_parts(dimension_schema, cohort_based_on)?;

            if update_req.schema.is_some() {
                crate::api::dimension::validations::validate_user_cohort_overrides(
                    &name,
                    dimension_schema,
                    cohort_based_on,
                    &mut conn,
                    &workspace_context.schema_name,
                )?;
            }

            let generated_description = update_req
                .description
                .clone()
                .unwrap_or_else(|| dimension_data.description.clone());
            Some((value, generated_schema, generated_description))
        }
        _ => None,
    };

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

            if let Some((value, generated_schema, generated_description)) =
                &generated_default_update
            {
                diesel::update(
                    default_configs::table
                        .filter(default_configs::key.eq(&result.dimension)),
                )
                .set((
                    default_configs::value.eq(value),
                    default_configs::schema.eq(Value::from(generated_schema)),
                    default_configs::last_modified_at.eq(Utc::now()),
                    default_configs::last_modified_by.eq(user.get_email()),
                    default_configs::description.eq(generated_description),
                    default_configs::change_reason.eq(update_change_reason.clone()),
                ))
                .returning(DefaultConfig::as_returning())
                .schema_name(&workspace_context.schema_name)
                .get_result::<DefaultConfig>(transaction_conn)?;
            }

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
        &mut conn,
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
        execute_webhook_call(data, &workspace_context, &state, &mut conn).await;

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
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let name: String = path.into_inner().into();
    let DbConnection(mut conn) = db_conn;
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let dimension_data: Dimension = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(&name))
        .select(Dimension::as_select())
        .schema_name(&workspace_context.schema_name)
        .get_result(&mut conn)?;

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

    let mut context_ids = get_dimension_usage_context_ids(
        &name,
        &mut conn,
        &workspace_context.schema_name,
    )?;
    let is_user_cohort =
        matches!(dimension_data.dimension_type, DimensionType::UserCohort(_));
    if is_user_cohort {
        context_ids.extend(get_key_usage_context_ids(
            &name,
            &mut conn,
            &workspace_context.schema_name,
        )?);
        context_ids.sort();
        context_ids.dedup();
    }

    if context_ids.is_empty() {
        let (config_version, dimension_data) = conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            use dimensions::dsl;

            if !dimension_data.dependency_graph.is_empty() {
                return Err(bad_argument!("The dimension {} currently has other dimensions that are using it in their cohort definitions. To delete this dimension, you need to delete these cohorts", &dimension_data.dimension))
            }

            match dimension_data.dimension_type {
                DimensionType::LocalCohort(ref cohort_based_on)
                | DimensionType::RemoteCohort(ref cohort_based_on)
                | DimensionType::UserCohort(ref cohort_based_on) => {
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

            if is_user_cohort {
                diesel::delete(
                    default_configs::table.filter(default_configs::key.eq(&name)),
                )
                .schema_name(&workspace_context.schema_name)
                .get_result::<DefaultConfig>(transaction_conn)?;
            }

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
            &mut conn,
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
            execute_webhook_call(data, &workspace_context, &state, &mut conn).await;

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
