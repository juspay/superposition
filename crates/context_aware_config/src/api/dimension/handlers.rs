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

use super::types::{DeleteReq, DimensionWithMandatory};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create)
        .service(get)
        .service(delete_dimension)
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
    let n_dimensions: i64 = dimensions.count().get_result(&mut conn)?;
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

    let req_priority_val: i32 = create_req.priority.clone().into();
    let dimension_name: String = create_req.dimension.clone().into();
    let req_position_val = Into::<Option<i32>>::into(create_req.position.clone());

    let mut results: Vec<(String, i32, i32)> = dimensions
        .order(priority.asc())
        .select((dimension, priority, position))
        .load::<(String, i32, i32)>(&mut conn)
        .expect("Error loading dimensions");
    let prev_index = results
        .iter()
        .position(|(name, _, _)| name.to_owned() == dimension_name.clone());
    results.push((dimension_name.clone(), req_priority_val, 0));
    results.sort_by_key(|&(_, val, _)| val);
    let new_index = results
        .iter()
        .position(|(name, _, _)| name.to_owned() == dimension_name);
    let mut position_val = new_index.unwrap_or(1) as i32;
    if let Some(val) = req_position_val.clone() {
        position_val = val;
    }
    let mut new_dimension = Dimension {
        dimension: create_req.dimension.into(),
        priority: create_req.priority.clone().into(),
        position: position_val.clone(),
        schema: schema_value,
        created_by: user.get_email(),
        created_at: Utc::now(),
        function_name: fun_name.clone(),
        last_modified_at: Utc::now().naive_utc(),
        last_modified_by: user.get_email(),
    };

    conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
        match (prev_index, new_index.clone(), req_position_val.clone()) {
            (Some(prev_val), Some(new_val), None) => {
                println!("1");
                if prev_val < new_val {
                    new_dimension.position = new_val.clone() as i32 - 1;
                } else {
                    new_dimension.position = new_val.clone() as i32
                };
                println!("2");
                diesel::update(dimensions)
                    .filter(position.gt(prev_val as i32))
                    .filter(position.lt(new_val.clone() as i32))
                    .filter(position.gt(0))
                    .set(position.eq(position - 1))
                    .execute(transaction_conn)?;

                diesel::update(dimensions)
                    .filter(position.ge(new_val as i32))
                    .set(position.eq(position + 1))
                    .execute(transaction_conn)?;
                println!("3");
            }
            (None, Some(new_index), None) => {
                println!("4");
                new_dimension.position = new_index.clone() as i32;
                diesel::update(dimensions)
                    .filter(position.ge(new_index as i32))
                    .set(position.eq(position + 1))
                    .execute(transaction_conn)?;
            }
            (_, _, Some(val)) => {
                if val > n_dimensions.clone() as i32 {
                    return Err(bad_argument!(
                        "Expected psoition value less than {}",
                        n_dimensions
                    ));
                }
                diesel::update(dimensions::dsl::dimensions)
                    .filter(dimensions::position.ge(val))
                    .set(dimensions::position.eq(dimensions::position + 1))
                    .execute(transaction_conn)?;
            }
            (_, _, _) => {}
        };

        let upsert = diesel::insert_into(dimensions)
            .values(&new_dimension)
            .on_conflict(dimensions::dimension)
            .do_update()
            .set(&new_dimension)
            .get_result::<Dimension>(transaction_conn);

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
    })
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
