use actix_web::{
    delete, get, post, put,
    web::{self, Data, Json, Path, Query},
    HttpResponse, Scope,
};
use chrono::Utc;
use diesel::{
    delete, Connection, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper,
};
use service_utils::service::types::{AppState, DbConnection, SchemaName};
use superposition_macros::{bad_argument, db_error, not_found, unexpected_error};
use superposition_types::{
    api::dimension::{
        CreateRequest, DeleteRequest, DimensionName, DimensionResponse, UpdateRequest,
    },
    custom_query::PaginationParams,
    database::{
        models::{
            cac::{DependencyGraph, Dimension},
            Workspace,
        },
        schema::dimensions::{self, dsl::*},
    },
    result as superposition, PaginatedResponse, User,
};

#[cfg(not(feature = "jsonlogic"))]
use crate::helpers::allow_primitive_types;
use crate::{
    api::dimension::utils::{
        get_dimension_usage_context_ids, validate_and_update_dimension_hierarchy,
        validate_dimension_deletability, validate_dimension_position,
    },
    helpers::{get_workspace, validate_jsonschema},
};

use super::utils::validate_and_initialize_dimension_hierarchy;

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create)
        .service(update)
        .service(get)
        .service(list)
        .service(delete_dimension)
}

#[post("")]
async fn create(
    state: Data<AppState>,
    req: web::Json<CreateRequest>,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let create_req = req.into_inner();
    let schema_value = create_req.schema;

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
    #[cfg(not(feature = "jsonlogic"))]
    allow_primitive_types(&schema_value)?;
    validate_jsonschema(&state.meta_schema, &schema_value)?;

    let mut dimension_data = Dimension {
        dimension: create_req.dimension.into(),
        position: create_req.position,
        schema: schema_value,
        created_by: user.get_email(),
        created_at: Utc::now(),
        function_name: create_req.function_name.clone(),
        last_modified_at: Utc::now(),
        last_modified_by: user.get_email(),
        description: create_req.description,
        change_reason: create_req.change_reason,
        dependency_graph: DependencyGraph::default(),
        dependents: Vec::new(),
        dependencies: create_req.dependencies.unwrap_or_default(),
        autocomplete_function_name: create_req.autocomplete_function_name,
    };

    conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
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

        dimension_data.dependency_graph = validate_and_initialize_dimension_hierarchy(
            &dimension_data.dimension,
            &dimension_data.dependencies,
            &user.get_email(),
            &schema_name,
            transaction_conn,
        )?;

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
                Ok(HttpResponse::Created()
                    .json(DimensionResponse::new(inserted_dimension, is_mandatory)))
            }
            Err(diesel::result::Error::DatabaseError(
                diesel::result::DatabaseErrorKind::ForeignKeyViolation,
                e,
            )) => {
                let fun_name = create_req.function_name.clone();
                log::error!("{fun_name:?} function not found with error: {e:?}");
                Err(bad_argument!(
                    "Function {} doesn't exists",
                    Into::<Option<String>>::into(create_req.function_name.clone())
                        .unwrap_or_default()
                ))
            }
            Err(e) => {
                log::error!("Dimension create failed with error: {e}");
                Err(db_error!(e))
            }
        }
    })
}

#[get("/{name}")]
async fn get(
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

#[put("/{name}")]
async fn update(
    path: Path<DimensionName>,
    state: Data<AppState>,
    req: web::Json<UpdateRequest>,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let name: String = path.clone().into();
    use dimensions::dsl;
    let DbConnection(mut conn) = db_conn;

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

    let update_req = req.into_inner();

    if let Some(schema_value) = update_req.schema.clone() {
        #[cfg(not(feature = "jsonlogic"))]
        allow_primitive_types(&schema_value)?;
        validate_jsonschema(&state.meta_schema, &schema_value)?;
    }

    let result =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            if let Some(position_val) = update_req.position {
                let new_position = position_val;
                validate_dimension_position(
                    path.into_inner(),
                    position_val,
                    num_rows - 1,
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

            if let Some(dependent_dimension) = &update_req.dependencies {
                validate_and_update_dimension_hierarchy(
                    &dimension_data,
                    dependent_dimension,
                    &user.get_email(),
                    &schema_name,
                    transaction_conn,
                )?;
            }

            diesel::update(dimensions)
                .filter(dsl::dimension.eq(name))
                .set((
                    update_req,
                    dimensions::last_modified_at.eq(Utc::now()),
                    dimensions::last_modified_by.eq(user.get_email()),
                ))
                .returning(Dimension::as_returning())
                .schema_name(&schema_name)
                .get_result::<Dimension>(transaction_conn)
                .map_err(|err| db_error!(err))
        })?;

    let workspace_settings = get_workspace(&schema_name, &mut conn)?;
    let is_mandatory = workspace_settings
        .mandatory_dimensions
        .unwrap_or_default()
        .contains(&result.dimension);

    Ok(HttpResponse::Ok().json(DimensionResponse::new(result, is_mandatory)))
}

#[get("")]
async fn list(
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

#[delete("/{name}")]
async fn delete_dimension(
    path: Path<DeleteRequest>,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let name: String = path.into_inner().into();
    let DbConnection(mut conn) = db_conn;
    let dimension_data: Dimension = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(&name))
        .select(Dimension::as_select())
        .schema_name(&schema_name)
        .get_result(&mut conn)?;

    let context_ids = get_dimension_usage_context_ids(&name, &mut conn, &schema_name)
        .map_err(|_| unexpected_error!("Something went wrong"))?;
    if context_ids.is_empty() {
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            use dimensions::dsl;

            validate_dimension_deletability(
                &name,
                &dimension_data,
                &user.get_email(),
                transaction_conn,
                &schema_name,
            )?;

            diesel::update(dsl::dimensions)
                .filter(dsl::dimension.eq(&name))
                .set((
                    dsl::last_modified_at.eq(Utc::now()),
                    dsl::last_modified_by.eq(user.get_email()),
                ))
                .returning(Dimension::as_returning())
                .schema_name(&schema_name)
                .execute(transaction_conn)?;

            let deleted_row = delete(dsl::dimensions.filter(dsl::dimension.eq(&name)))
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
                Ok(_) => Ok(HttpResponse::NoContent().finish()),
                Err(e) => {
                    log::error!("dimension delete query failed with error: {e}");
                    Err(unexpected_error!("Something went wrong."))
                }
            }
        })
    } else {
        Err(bad_argument!(
            "Given key already in use in contexts: {}",
            context_ids.join(",")
        ))
    }
}
