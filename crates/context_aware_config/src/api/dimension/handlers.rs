extern crate base64;

use actix_web::{
    delete, get, put,
    web::{self, Data, Json, Path, Query},
    HttpResponse, Scope,
};
use chrono::Utc;
use diesel::{
    delete, Connection, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper,
};
use jsonschema::{Draft, JSONSchema};
use serde_json::Value;
use service_utils::service::types::{AppState, DbConnection};
use superposition_macros::{bad_argument, not_found, unexpected_error};
use superposition_types::{
    cac::{
        models::Dimension,
        schema::{dimensions, dimensions::dsl::*},
    },
    custom_query::PaginationParams,
    result as superposition, PaginatedResponse, TenantConfig, User,
};

use crate::{
    api::dimension::{types::CreateReq, utils::get_dimension_usage_context_ids},
    helpers::validate_jsonschema,
};

use super::types::{DeleteReq, DimensionName, DimensionWithMandatory, UpdateReq};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create)
        .service(update)
        .service(get)
        .service(delete_dimension)
        .service(temp_position_update)
}

#[put("")]
async fn create(
    state: Data<AppState>,
    req: web::Json<CreateReq>,
    user: User,
    db_conn: DbConnection,
    tenant_config: TenantConfig,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let create_req = req.into_inner();
    let schema_value = create_req.schema;

    validate_jsonschema(&state.meta_schema, &schema_value)?;

    let schema_compile_result = JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&schema_value);

    if let Err(e) = schema_compile_result {
        return Err(bad_argument!(
            "Invalid JSON schema (failed to compile): {:?}",
            e
        ));
    };

    let fun_name = match create_req.function_name {
        Some(Value::String(func_name)) => Some(func_name),
        Some(Value::Null) | None => None,
        _ => {
            log::error!("Expected a string or null as the function name.");
            return Err(bad_argument!(
                "Expected a string or null as the function name."
            ));
        }
    };

    let dimension_data = Dimension {
        dimension: create_req.dimension.into(),
        position: create_req.position.into(),
        schema: schema_value,
        created_by: user.get_email(),
        created_at: Utc::now(),
        function_name: fun_name.clone(),
        last_modified_at: Utc::now().naive_utc(),
        last_modified_by: user.get_email(),
    };

    conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
        diesel::update(dimensions)
            .filter(dimensions::position.ge(dimension_data.position.clone()))
            .set((
                last_modified_at.eq(Utc::now().naive_utc()),
                last_modified_by.eq(user.get_email()),
                dimensions::position.eq(dimensions::position + 1),
            ))
            .execute(transaction_conn)?;
        let insert_resp = diesel::insert_into(dimensions)
            .values(&dimension_data)
            .get_result::<Dimension>(transaction_conn);

        match insert_resp {
            Ok(inserted_dimension) => {
                let is_mandatory = tenant_config
                    .mandatory_dimensions
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
                log::error!("{fun_name:?} function not found with error: {e:?}");
                Err(bad_argument!(
                    "Function {} doesn't exists",
                    fun_name.unwrap_or(String::new())
                ))
            }
            Err(e) => {
                log::error!("Dimension create failed with error: {e}");
                Err(unexpected_error!(
                    "Something went wrong, failed to create dimension"
                ))
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
    tenant_config: TenantConfig,
) -> superposition::Result<HttpResponse> {
    let name: String = path.into_inner().into();
    let DbConnection(mut conn) = db_conn;

    let mut dimension_row: Dimension = dimensions.find(&name).first(&mut conn)?;
    let update_req = req.into_inner();

    if let Some(schema_value) = update_req.schema {
        validate_jsonschema(&state.meta_schema, &schema_value)?;

        let schema_compile_result = JSONSchema::options()
            .with_draft(Draft::Draft7)
            .compile(&schema_value);

        if let Err(e) = schema_compile_result {
            return Err(bad_argument!(
                "Invalid JSON schema (failed to compile): {:?}",
                e
            ));
        };
        dimension_row.schema = schema_value;
    }

    let fun_name = match update_req.function_name {
        Some(Value::String(func_name)) => Some(func_name),
        Some(Value::Null) | None => None,
        _ => {
            log::error!("Expected a string or null as the function name.");
            return Err(bad_argument!(
                "Expected a string or null as the function name."
            ));
        }
    };
    dimension_row.function_name = fun_name.clone();

    use dimensions::dsl;
    if let Some(position_val) = update_req.position.clone() {
        let new_position: i32 = position_val.into();

        let previous_position = dimension_row.position.clone();
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            if previous_position < new_position {
                diesel::update(dsl::dimensions)
                    .filter(dimensions::position.gt(dimension_row.position))
                    .filter(dimensions::position.le(new_position.clone()))
                    .set((
                        dsl::last_modified_at.eq(Utc::now().naive_utc()),
                        dsl::last_modified_by.eq(user.get_email()),
                        dimensions::position.eq(dimensions::position - 1),
                    ))
                    .get_result::<Dimension>(transaction_conn)?
            } else {
                diesel::update(dsl::dimensions)
                    .filter(dimensions::position.lt(dimension_row.position))
                    .filter(dimensions::position.ge(new_position.clone()))
                    .set((
                        dsl::last_modified_at.eq(Utc::now().naive_utc()),
                        dsl::last_modified_by.eq(user.get_email()),
                        dimensions::position.eq(dimensions::position + 1),
                    ))
                    .get_result::<Dimension>(transaction_conn)?
            };

            let result = diesel::update(dimensions)
                .filter(dsl::dimension.eq(&dimension_row.dimension))
                .set((
                    dsl::last_modified_at.eq(Utc::now().naive_utc()),
                    dsl::last_modified_by.eq(user.get_email()),
                    dimensions::function_name.eq(dimension_row.function_name),
                    dimensions::schema.eq(dimension_row.schema),
                    dimensions::position.eq(new_position),
                ))
                .get_result::<Dimension>(transaction_conn)?;
            let is_mandatory = tenant_config
                .mandatory_dimensions
                .contains(&result.dimension);
            Ok(HttpResponse::Created()
                .json(DimensionWithMandatory::new(result, is_mandatory)))
        })
    } else {
        let result = diesel::update(dimensions)
            .filter(dsl::dimension.eq(&dimension_row.dimension))
            .set((
                dsl::last_modified_at.eq(Utc::now().naive_utc()),
                dsl::last_modified_by.eq(user.get_email()),
                dimensions::function_name.eq(dimension_row.function_name),
                dimensions::schema.eq(dimension_row.schema),
            ))
            .get_result::<Dimension>(&mut conn)?;
        let is_mandatory = tenant_config
            .mandatory_dimensions
            .contains(&result.dimension);
        Ok(HttpResponse::Created()
            .json(DimensionWithMandatory::new(result, is_mandatory)))
    }
}

#[get("")]
async fn get(
    db_conn: DbConnection,
    tenant_config: TenantConfig,
    filters: Query<PaginationParams>,
) -> superposition::Result<Json<PaginatedResponse<DimensionWithMandatory>>> {
    let DbConnection(mut conn) = db_conn;

    let (total_pages, total_items, result) = match filters.all {
        Some(true) => {
            let result: Vec<Dimension> = dimensions.get_results(&mut conn)?;
            (1, result.len() as i64, result)
        }
        _ => {
            let n_dimensions: i64 = dimensions.count().get_result(&mut conn)?;
            let limit = filters.count.unwrap_or(10);
            let mut builder = dimensions
                .into_boxed()
                .order(created_at.desc())
                .limit(limit);
            if let Some(page) = filters.page {
                let offset = (page - 1) * limit;
                builder = builder.offset(offset);
            }
            let result: Vec<Dimension> = builder.load(&mut conn)?;
            let total_pages = (n_dimensions as f64 / limit as f64).ceil() as i64;
            (total_pages, n_dimensions, result)
        }
    };

    let dimensions_with_mandatory: Vec<DimensionWithMandatory> = result
        .into_iter()
        .map(|ele| {
            let is_mandatory =
                tenant_config.mandatory_dimensions.contains(&ele.dimension);
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
) -> superposition::Result<HttpResponse> {
    let name: String = path.into_inner().into();
    let DbConnection(mut conn) = db_conn;
    let dimension_data: Dimension = dimensions::dsl::dimensions
        .filter(dimensions::dimension.eq(&name))
        .select(Dimension::as_select())
        .get_result(&mut conn)?;
    let context_ids = get_dimension_usage_context_ids(&name, &mut conn)
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
                .execute(transaction_conn)?;
            diesel::update(dimensions::dsl::dimensions)
                .filter(dimensions::position.gt(dimension_data.position))
                .set(dimensions::position.eq(dimensions::position - 1))
                .execute(transaction_conn)?;
            let deleted_row = delete(dsl::dimensions.filter(dsl::dimension.eq(&name)))
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

#[put("/position/update")]
async fn temp_position_update(
    user: User,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let results: Vec<String> = dimensions
        .select(dimension)
        .load::<String>(&mut conn)
        .map_err(|err| {
            log::error!("failed to fetch dimensions with error: {}", err);
            unexpected_error!("Something went wrong")
        })?;

    let _ = for (index, dimension_name) in results.iter().enumerate() {
        diesel::update(dimensions)
            .filter(dimension.eq(dimension_name))
            .set((
                position.eq(index as i32),
                last_modified_at.eq(Utc::now().naive_utc()),
                last_modified_by.eq(user.get_email()),
            ))
            .execute(&mut conn)?;
    };
    Ok(HttpResponse::Ok()
        .json(serde_json::json!({"message": "Position updated sucessfully"})))
}
