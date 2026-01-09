use actix_web::{
    delete, get, post, routes,
    web::{self, Data, Json, Path, Query},
    HttpResponse, Scope,
};
use chrono::Utc;
use diesel::{Connection, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper};
use serde_json::Value;
use service_utils::{
    helpers::parse_config_tags,
    service::types::{AppHeader, AppState, CustomHeaders, DbConnection, SchemaName},
};
use superposition_derives::authorized;
use superposition_macros::{bad_argument, db_error, not_found, unexpected_error};
use superposition_types::{
    api::dimension::{
        CreateRequest, DeleteRequest, DimensionName, DimensionResponse, UpdateRequest,
    },
    custom_query::PaginationParams,
    database::{
        models::{
            cac::{DependencyGraph, Dimension, DimensionType},
            Description, Workspace,
        },
        schema::dimensions::{self, dsl::*},
    },
    result as superposition, PaginatedResponse, User,
};

use crate::api::dimension::validations::allow_primitive_types;
#[cfg(feature = "high-performance-mode")]
use crate::helpers::put_config_in_redis;
use crate::{
    api::dimension::{
        utils::{
            create_connections_with_dependents, get_dimension_usage_context_ids,
            remove_connections_with_dependents,
        },
        validations::{
            does_dimension_exist_for_cohorting, validate_cohort_position,
            validate_cohort_schema, validate_dimension_position, validate_jsonschema,
            validate_position_wrt_dependency, validate_validation_function,
            validate_value_compute_function,
        },
    },
    helpers::{add_config_version, get_workspace, validate_change_reason},
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
    state: Data<AppState>,
    req: web::Json<CreateRequest>,
    user: User,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let create_req = req.into_inner();
    let schema_value = Value::from(&create_req.schema);
    let tags = parse_config_tags(custom_headers.config_tags)?;

    validate_change_reason(&create_req.change_reason, &mut conn, &schema_name)?;

    let num_rows = dimensions
        .count()
        .schema_name(&schema_name)
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
            validate_jsonschema(&state.meta_schema, &schema_value)?;
        }
        DimensionType::RemoteCohort(ref cohort_based_on) => {
            allow_primitive_types(&create_req.schema)?;
            validate_jsonschema(&state.meta_schema, &schema_value)?;
            let based_on_dimension = does_dimension_exist_for_cohorting(
                cohort_based_on,
                &schema_name,
                &mut conn,
            )?;
            validate_cohort_position(&create_req.position, &based_on_dimension, true)?;
        }
        DimensionType::LocalCohort(ref cohort_based_on) => {
            let based_on_dimension = validate_cohort_schema(
                &schema_value,
                cohort_based_on,
                &schema_name,
                &mut conn,
            )?;
            validate_cohort_position(&create_req.position, &based_on_dimension, true)?;
        }
    }

    validate_validation_function(
        &create_req.value_validation_function_name,
        &mut conn,
        &schema_name,
    )?;

    validate_value_compute_function(
        &create_req.dimension_type,
        &create_req.value_compute_function_name,
        &mut conn,
        &schema_name,
    )?;

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

    let (inserted_dimension, is_mandatory, version_id) = conn
        .transaction::<_, superposition::AppError, _>(|transaction_conn| {
            diesel::update(dimensions::table)
                .filter(dimensions::position.ge(dimension_data.position))
                .set((
                    last_modified_at.eq(Utc::now()),
                    last_modified_by.eq(user.get_email()),
                    dimensions::position.eq(dimensions::position + 1),
                ))
                .returning(Dimension::as_returning())
                .schema_name(&schema_name)
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
                        &schema_name,
                        transaction_conn,
                    )?
                }
                DimensionType::Regular {} => (),
            }

            let insert_resp = diesel::insert_into(dimensions::table)
                .values(&dimension_data)
                .returning(Dimension::as_returning())
                .schema_name(&schema_name)
                .get_result(transaction_conn);

            match insert_resp {
                Ok(inserted_dimension) => {
                    let workspace_settings: Workspace =
                        get_workspace(&schema_name, transaction_conn)?;
                    let is_mandatory = workspace_settings
                        .mandatory_dimensions
                        .unwrap_or_default()
                        .contains(&inserted_dimension.dimension);

                    let version_id = add_config_version(
                        &state,
                        tags,
                        dimension_data.change_reason.into(),
                        transaction_conn,
                        &schema_name,
                    )?;
                    Ok((inserted_dimension, is_mandatory, version_id))
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

    #[cfg(feature = "high-performance-mode")]
    put_config_in_redis(version_id, state, &schema_name, &mut conn).await?;

    let mut http_resp = HttpResponse::Created();
    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        version_id.to_string(),
    ));
    Ok(http_resp.json(DimensionResponse::new(inserted_dimension, is_mandatory)))
}

#[authorized]
#[get("/{name}")]
async fn get_handler(
    db_conn: DbConnection,
    req: Path<String>,
    schema_name: SchemaName,
) -> superposition::Result<Json<DimensionResponse>> {
    let DbConnection(mut conn) = db_conn;

    let result: Dimension = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(req.into_inner()))
        .schema_name(&schema_name)
        .get_result::<Dimension>(&mut conn)?;

    let workspace_settings = get_workspace(&schema_name, &mut conn)?;
    let is_mandatory = workspace_settings
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
    path: Path<DimensionName>,
    state: Data<AppState>,
    req: web::Json<UpdateRequest>,
    user: User,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let name: String = path.clone().into();
    use dimensions::dsl;
    let DbConnection(mut conn) = db_conn;
    let tags = parse_config_tags(custom_headers.config_tags)?;
    let update_req = req.into_inner();

    validate_change_reason(&update_req.change_reason, &mut conn, &schema_name)?;

    let dimension_data: Dimension = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(name.clone()))
        .schema_name(&schema_name)
        .get_result::<Dimension>(&mut conn)?;

    let num_rows = dimensions
        .count()
        .schema_name(&schema_name)
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
                validate_jsonschema(&state.meta_schema, &schema_value)?;
            }
            DimensionType::LocalCohort(ref cohort_based_on) => {
                validate_cohort_schema(
                    &schema_value,
                    cohort_based_on,
                    &schema_name,
                    &mut conn,
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
                    &schema_name,
                    &mut conn,
                )?;
                validate_cohort_position(new_position, &based_on_dimension, false)?;
            }
        }
    }

    if let Some(ref fn_name) = update_req.value_validation_function_name {
        validate_validation_function(fn_name, &mut conn, &schema_name)?;
    }

    if let Some(ref value_compute_function_name_) = update_req.value_compute_function_name
    {
        validate_value_compute_function(
            &dimension_data.dimension_type,
            value_compute_function_name_,
            &mut conn,
            &schema_name,
        )?;
    }

    let (result, is_mandatory, version_id) = conn
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
                    &schema_name,
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
                    .schema_name(&schema_name)
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
                        .schema_name(&schema_name)
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
                        .schema_name(&schema_name)
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
                .schema_name(&schema_name)
                .get_result::<Dimension>(transaction_conn)
                .map_err(|err| db_error!(err))?;

            let workspace_settings = get_workspace(&schema_name, transaction_conn)?;

            let is_mandatory = workspace_settings
                .mandatory_dimensions
                .unwrap_or_default()
                .contains(&result.dimension);

            let version_id = add_config_version(
                &state,
                tags,
                dimension_data.change_reason.into(),
                transaction_conn,
                &schema_name,
            )?;

            Ok((result, is_mandatory, version_id))
        })?;

    #[cfg(feature = "high-performance-mode")]
    put_config_in_redis(version_id, state, &schema_name, &mut conn).await?;

    let mut http_resp = HttpResponse::Ok();
    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        version_id.to_string(),
    ));
    Ok(http_resp.json(DimensionResponse::new(result, is_mandatory)))
}

#[authorized]
#[get("")]
async fn list_handler(
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
    schema_name: SchemaName,
) -> superposition::Result<Json<PaginatedResponse<DimensionResponse>>> {
    let DbConnection(mut conn) = db_conn;

    let (total_pages, total_items, result) = match filters.all {
        Some(true) => {
            let result: Vec<Dimension> = dimensions
                .schema_name(&schema_name)
                .get_results(&mut conn)?;
            (1, result.len() as i64, result)
        }
        _ => {
            let n_dimensions: i64 = dimensions
                .count()
                .schema_name(&schema_name)
                .get_result(&mut conn)?;
            let limit = filters.count.unwrap_or(10);
            let mut builder = dimensions
                .schema_name(&schema_name)
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

    let workspace_settings = get_workspace(&schema_name, &mut conn)?;

    let mandatory_dimensions =
        workspace_settings.mandatory_dimensions.unwrap_or_default();

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
    state: Data<AppState>,
    path: Path<DeleteRequest>,
    user: User,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let name: String = path.into_inner().into();
    let DbConnection(mut conn) = db_conn;
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let dimension_data: Dimension = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(&name))
        .select(Dimension::as_select())
        .schema_name(&schema_name)
        .get_result(&mut conn)?;

    let context_ids = get_dimension_usage_context_ids(&name, &mut conn, &schema_name)?;

    if context_ids.is_empty() {
        let (resp, _version_id) = conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
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
                        &schema_name,
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
                .schema_name(&schema_name)
                .execute(transaction_conn)?;

            let deleted_row = diesel::delete(dsl::dimensions.filter(dsl::dimension.eq(&name)))
                .schema_name(&schema_name)
                .execute(transaction_conn);

            diesel::update(dimensions::dsl::dimensions)
                .filter(dimensions::position.gt(dimension_data.position))
                .set(dimensions::position.eq(dimensions::position - 1))
                .returning(Dimension::as_returning())
                .schema_name(&schema_name)
                .execute(transaction_conn)?;

            match deleted_row {
                Ok(0) => Err(not_found!("Dimension `{}` doesn't exists", name)),
                Ok(_) => {
                    let config_version_desc = Description::try_from(format!(
                        "Dimension Deleted by {}",
                        user.get_email()
                    ))
                    .map_err(|e| unexpected_error!(e))?;
                    let version_id = add_config_version(
                        &state,
                        tags,
                        config_version_desc,
                        transaction_conn,
                        &schema_name,
                    )?;
                    log::info!(
                        "Dimension: {name} deleted by {}",
                        user.get_email()
                    );
                    Ok((HttpResponse::NoContent()
                        .insert_header((
                            AppHeader::XConfigVersion.to_string(),
                            version_id.to_string(),
                        ))
                        .finish(), version_id))
                    },
                Err(e) => {
                    log::error!("dimension delete query failed with error: {e}");
                    Err(unexpected_error!("Something went wrong."))
                }
            }
        })?;

        #[cfg(feature = "high-performance-mode")]
        put_config_in_redis(_version_id, state, &schema_name, &mut conn).await?;
        Ok(resp)
    } else {
        Err(bad_argument!(
            "Given key already in use in contexts: {}",
            context_ids.join(",")
        ))
    }
}
