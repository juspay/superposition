use crate::{
    api::dimension::{types::CreateReq, utils::get_dimension_usage_context_ids},
    db::{
        models::Dimension,
        schema::{dimensions, dimensions::dsl::*},
    },
    helpers::validate_jsonschema,
};
use actix_web::{
    delete, get, put,
    web::{self, Data, Path, Query},
    HttpResponse, Scope,
};
use chrono::Utc;
use diesel::{
    delete, Connection, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper,
};
use jsonschema::{Draft, JSONSchema};
use serde_json::{json, Value};
use service_utils::service::types::{AppState, DbConnection};
use superposition_macros::{bad_argument, not_found, unexpected_error};
use superposition_types::{result as superposition, QueryFilters, TenantConfig, User};

extern crate base64;

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

    let new_dimension = Dimension {
        dimension: create_req.dimension.into(),
        priority: create_req.priority.into(),
        schema: schema_value,
        created_by: user.get_email(),
        created_at: Utc::now(),
        function_name: fun_name.clone(),
        last_modified_at: Utc::now().naive_utc(),
        last_modified_by: user.get_email(),
    };

    let upsert = diesel::insert_into(dimensions)
        .values(&new_dimension)
        .on_conflict(dimensions::dimension)
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
    filters: Query<QueryFilters>,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let n_dimensions: i64 = dimensions.count().get_result(&mut conn)?;
    let mut builder = dimensions.into_boxed().order(created_at.desc());
    if let Some(limit) = filters.count {
        builder = builder.limit(limit);
    }
    if let Some(page) = filters.page {
        let offset = (page - 1) * filters.count.unwrap_or(10);
        builder = builder.offset(offset);
    }
    let limit = filters.count.unwrap_or(10);
    let result: Vec<Dimension> = builder.load(&mut conn)?;
    let total_pages = (n_dimensions as f64 / limit as f64).ceil() as u64;

    let dimensions_with_mandatory: Vec<DimensionWithMandatory> = result
        .into_iter()
        .map(|ele| {
            let is_mandatory =
                tenant_config.mandatory_dimensions.contains(&ele.dimension);
            DimensionWithMandatory::new(ele, is_mandatory)
        })
        .collect();

    Ok(HttpResponse::Ok().json(json!({
        "total_pages": total_pages,
        "total_items": n_dimensions,
        "data": dimensions_with_mandatory
    })))
}

#[delete("/{name}")]
async fn delete_dimension(
    path: Path<DeleteReq>,
    user: User,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let name: String = path.into_inner().into();
    let DbConnection(mut conn) = db_conn;
    dimensions::dsl::dimensions
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
