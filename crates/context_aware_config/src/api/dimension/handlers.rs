use crate::{
    api::dimension::types::CreateReq,
    db::{models::Dimension, schema::dimensions::dsl::*},
    helpers::validate_jsonschema,
};
use actix_web::{
    get, put,
    web::{self, Data, Json},
    HttpResponse, Scope,
};
use chrono::Utc;
use diesel::RunQueryDsl;
use jsonschema::{Draft, JSONSchema};
use serde_json::Value;
use superposition_macros::{bad_argument, unexpected_error};
use superposition_types::{result as superposition, SuperpositionUser, User};

use service_utils::service::types::{AppState, DbConnection};

pub fn endpoints() -> Scope {
    Scope::new("").service(create).service(get)
}

#[put("")]
async fn create(
    state: Data<AppState>,
    req: web::Json<CreateReq>,
    user: User,
    db_conn: DbConnection,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    if req.priority <= 0 {
        return Err(bad_argument!("Priority should be greater than 0"));
    }

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
        dimension: create_req.dimension,
        priority: create_req.priority,
        schema: schema_value,
        created_by: user.get_email(),
        created_at: Utc::now(),
        function_name: fun_name.clone(),
        last_modified_at: Utc::now().naive_utc(),
        last_modified_by: user.get_email(),
    };

    let upsert = diesel::insert_into(dimensions)
        .values(&new_dimension)
        .on_conflict(dimension)
        .do_update()
        .set(&new_dimension)
        .get_result::<Dimension>(&mut conn);

    match upsert {
        Ok(upserted_dimension) => Ok(HttpResponse::Created().json(upserted_dimension)),
        Err(diesel::result::Error::DatabaseError(
            diesel::result::DatabaseErrorKind::ForeignKeyViolation,
            e,
        )) => {
            log::error!("{fun_name:?} function not found with error: {e:?}");
            Err(bad_argument!(
                "Funtion {} doesn't exists",
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
async fn get(db_conn: DbConnection) -> superposition::Result<Json<Vec<Dimension>>> {
    let DbConnection(mut conn) = db_conn;

    let result: Vec<Dimension> = dimensions.get_results(&mut conn)?;
    Ok(Json(result))
}
