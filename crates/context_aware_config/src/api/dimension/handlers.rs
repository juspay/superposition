extern crate base64;

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
    custom_query::PaginationParams,
    database::{
        models::{cac::Dimension, Workspace},
        schema::dimensions::{self, dsl::*},
        types::DimensionWithMandatory,
    },
    result as superposition, PaginatedResponse, User,
};

use crate::{
    api::dimension::{
        types::{CreateReq, FunctionNameEnum},
        utils::{get_dimension_usage_context_ids, validate_dimension_position},
    },
    helpers::{get_workspace, validate_jsonschema},
};

use super::types::{DeleteReq, DimensionName, UpdateReq};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create)
        .service(update)
        .service(get)
        .service(delete_dimension)
}

#[post("")]
async fn create(
    state: Data<AppState>,
    req: web::Json<CreateReq>,
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
        create_req.position.clone(),
        num_rows,
    )?;
    validate_jsonschema(&state.meta_schema, &schema_value)?;

    let dimension_data = Dimension {
        dimension: create_req.dimension.into(),
        position: create_req.position.into(),
        schema: schema_value,
        created_by: user.get_email(),
        created_at: Utc::now(),
        function_name: create_req.function_name.clone(),
        last_modified_at: Utc::now().naive_utc(),
        last_modified_by: user.get_email(),
        description: create_req.description,
        change_reason: create_req.change_reason,
    };

    conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
        diesel::update(dimensions::table)
            .filter(dimensions::position.ge(dimension_data.position))
            .set((
                last_modified_at.eq(Utc::now().naive_utc()),
                last_modified_by.eq(user.get_email()),
                dimensions::position.eq(dimensions::position + 1),
            ))
            .returning(Dimension::as_returning())
            .schema_name(&schema_name)
            .execute(transaction_conn)?;
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
                Ok(HttpResponse::Created().json(DimensionWithMandatory::new(
                    inserted_dimension,
                    is_mandatory,
                )))
            }
            Err(diesel::result::Error::DatabaseError(
                diesel::result::DatabaseErrorKind::ForeignKeyViolation,
                e,
            )) => {
                let fun_name = create_req.function_name;
                log::error!("{fun_name:?} function not found with error: {e:?}");
                Err(bad_argument!(
                    "Function {} doesn't exists",
                    fun_name.unwrap_or_default()
                ))
            }
            Err(e) => {
                log::error!("Dimension create failed with error: {e}");
                Err(db_error!(e))
            }
        }
    })
}

#[put("/{name}")]
async fn update(
    path: Path<DimensionName>,
    state: Data<AppState>,
    req: web::Json<UpdateReq>,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let name: String = path.clone().into();
    use dimensions::dsl;
    let DbConnection(mut conn) = db_conn;

    let mut dimension_row: Dimension = dsl::dimensions
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

    if let Some(schema_value) = update_req.schema {
        validate_jsonschema(&state.meta_schema, &schema_value)?;
        dimension_row.schema = schema_value;
    }

    dimension_row.change_reason = update_req.change_reason;
    dimension_row.description =
        update_req.description.unwrap_or(dimension_row.description);

    dimension_row.function_name = match update_req.function_name {
        Some(FunctionNameEnum::Name(func_name)) => Some(func_name),
        Some(FunctionNameEnum::Remove) => None,
        None => dimension_row.function_name,
    };

    let result =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let mut new_position = dimension_row.position;

            if let Some(position_val) = update_req.position {
                new_position = position_val.clone().into();
                validate_dimension_position(
                    path.into_inner(),
                    position_val,
                    num_rows - 1,
                )?;
                let previous_position = dimension_row.position;

                diesel::update(dimensions)
                    .filter(dsl::dimension.eq(&dimension_row.dimension))
                    .set((
                        dsl::last_modified_at.eq(Utc::now().naive_utc()),
                        dsl::last_modified_by.eq(user.get_email()),
                        dimensions::position.eq((num_rows + 100) as i32),
                    ))
                    .returning(Dimension::as_returning())
                    .schema_name(&schema_name)
                    .get_result::<Dimension>(transaction_conn)?;

                if previous_position < new_position {
                    diesel::update(dsl::dimensions)
                        .filter(dimensions::position.gt(previous_position))
                        .filter(dimensions::position.le(new_position))
                        .set((
                            dsl::last_modified_at.eq(Utc::now().naive_utc()),
                            dsl::last_modified_by.eq(user.get_email()),
                            dimensions::position.eq(dimensions::position - 1),
                        ))
                        .returning(Dimension::as_returning())
                        .schema_name(&schema_name)
                        .execute(transaction_conn)?
                } else {
                    diesel::update(dsl::dimensions)
                        .filter(dimensions::position.lt(previous_position))
                        .filter(dimensions::position.ge(new_position))
                        .set((
                            dsl::last_modified_at.eq(Utc::now().naive_utc()),
                            dsl::last_modified_by.eq(user.get_email()),
                            dimensions::position.eq(dimensions::position + 1),
                        ))
                        .returning(Dimension::as_returning())
                        .schema_name(&schema_name)
                        .execute(transaction_conn)?
                };
            }

            diesel::update(dimensions)
                .filter(dsl::dimension.eq(&dimension_row.dimension))
                .set((
                    dimensions::last_modified_at.eq(Utc::now().naive_utc()),
                    dimensions::last_modified_by.eq(user.get_email()),
                    dimensions::function_name.eq(dimension_row.function_name),
                    dimensions::schema.eq(dimension_row.schema),
                    dimensions::position.eq(new_position),
                    dimensions::description.eq(dimension_row.description),
                    dimensions::change_reason.eq(dimension_row.change_reason),
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

    Ok(HttpResponse::Ok().json(DimensionWithMandatory::new(result, is_mandatory)))
}

#[get("")]
async fn get(
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
    schema_name: SchemaName,
) -> superposition::Result<Json<PaginatedResponse<DimensionWithMandatory>>> {
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

    let dimensions_with_mandatory: Vec<DimensionWithMandatory> = result
        .into_iter()
        .map(|ele| {
            let is_mandatory = mandatory_dimensions.contains(&ele.dimension);
            DimensionWithMandatory::new(ele, is_mandatory)
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
    path: Path<DeleteReq>,
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
            diesel::update(dsl::dimensions)
                .filter(dsl::dimension.eq(&name))
                .set((
                    dsl::last_modified_at.eq(Utc::now().naive_utc()),
                    dsl::last_modified_by.eq(user.get_email()),
                ))
                .returning(Dimension::as_returning())
                .schema_name(&schema_name)
                .execute(transaction_conn)?;
            diesel::update(dimensions::dsl::dimensions)
                .filter(dimensions::position.gt(dimension_data.position))
                .set(dimensions::position.eq(dimensions::position - 1))
                .returning(Dimension::as_returning())
                .schema_name(&schema_name)
                .execute(transaction_conn)?;
            let deleted_row = delete(dsl::dimensions.filter(dsl::dimension.eq(&name)))
                .schema_name(&schema_name)
                .execute(transaction_conn);
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
