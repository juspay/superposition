use crate::{
    api::dimension::{types::CreateReq, utils::get_dimension_usage_context_ids},
    helpers::validate_jsonschema,
};
use actix_web::{
    delete, get, put,
    web::{self, Data, Json, Path, Query},
    HttpResponse, Scope,
};
use chrono::Utc;
use diesel::{
    delete, Connection, ExpressionMethods, OptionalExtension, QueryDsl, RunQueryDsl,
    SelectableHelper,
};
use jsonschema::{Draft, JSONSchema};
use serde_json::Value;
use service_utils::service::types::{AppState, DbConnection};
use superposition_macros::{bad_argument, not_found, unexpected_error};
use superposition_types::{
    cac::{
        models::Dimension,
        schema::{
            dimensions,
            dimensions::dsl::{
                created_at, dimension, dimensions as dimension_table, last_modified_at,
                last_modified_by, position, priority,
            },
        },
    },
    custom_query::PaginationParams,
    result as superposition, PaginatedResponse, TenantConfig, User,
};

use super::types::{DeleteReq, DimensionWithMandatory};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create)
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

    let dimension_name = create_req.dimension.to_string();

    let existing_dimension = match dimension_table
        .filter(dimension.eq(&dimension_name))
        .first::<Dimension>(&mut conn)
        .optional()
    {
        Ok(dim) => dim,
        Err(e) => {
            log::warn!(
                "Failed to fetch dimension {}: {:?}. Proceeding as if it does not exist.",
                dimension_name,
                e
            );
            None
        }
    };

    let description = match &create_req.description {
        Some(desc) if !desc.trim().is_empty() => desc.clone(),
        _ => {
            if let Some(existing_dim) = &existing_dimension {
                existing_dim.description.clone()
            } else {
                // No description provided and no existing dimension
                // Return an error
                log::error!(
                    "No description provided and no existing dimension for {}.",
                    dimension_name
                );
                return Err(bad_argument!(
                    "No description provided for the new dimension {}.",
                    dimension_name
                ));
            }
        }
    };

    let new_dimension = Dimension {
        dimension: create_req.dimension.into(),
        priority: create_req.priority.into(),
        position: 0, // hard coded for now till we migrate
        schema: schema_value,
        created_by: user.get_email(),
        created_at: Utc::now(),
        function_name: fun_name.clone(),
        last_modified_at: Utc::now().naive_utc(),
        last_modified_by: user.get_email(),
        description,
        change_reason: create_req.change_reason,
    };

    let upsert = diesel::insert_into(dimension_table)
        .values(&new_dimension)
        .on_conflict(dimension)
        .do_update()
        .set(&new_dimension)
        .get_result::<Dimension>(&mut conn);

    match upsert {
        Ok(upserted_dimension) => {
            let is_mandatory = tenant_config
                .mandatory_dimensions
                .contains(&upserted_dimension.dimension);
            Ok(HttpResponse::Created().json(DimensionWithMandatory::new(
                upserted_dimension,
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
            log::error!("Dimension upsert failed with error: {e}");
            Err(unexpected_error!(
                "Something went wrong, failed to create/update dimension"
            ))
        }
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
            let result: Vec<Dimension> = dimension_table.get_results(&mut conn)?;
            (1, result.len() as i64, result)
        }
        _ => {
            let n_dimensions: i64 = dimension_table.count().get_result(&mut conn)?;
            let limit = filters.count.unwrap_or(10);
            let mut builder = dimension_table
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
    let dimension_data = dimension_table
        .filter(dimension.eq(&name))
        .select(Dimension::as_select())
        .get_result(&mut conn)?;
    let context_ids = get_dimension_usage_context_ids(&name, &mut conn)
        .map_err(|_| unexpected_error!("Something went wrong"))?;
    if context_ids.is_empty() {
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            diesel::update(dimension_table)
                .filter(dimension.eq(&name))
                .set((
                    last_modified_at.eq(Utc::now().naive_utc()),
                    last_modified_by.eq(user.get_email()),
                ))
                .execute(transaction_conn)?;
            diesel::update(dimensions::dsl::dimensions)
                .filter(dimensions::position.gt(dimension_data.position))
                .set(dimensions::position.eq(dimensions::position - 1))
                .execute(transaction_conn)?;
            let deleted_row = delete(dimension_table.filter(dimension.eq(&name)))
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
    let results: Vec<String> = dimension_table
        .order(priority.asc())
        .select(dimension)
        .load::<String>(&mut conn)
        .map_err(|err| {
            log::error!("failed to fetch dimensions with error: {}", err);
            unexpected_error!("Something went wrong")
        })?;

    let _ = for (index, dimension_name) in results.iter().enumerate() {
        diesel::update(dimension_table)
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
