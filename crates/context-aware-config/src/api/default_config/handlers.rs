use super::types::CreateReq;
use crate::{
    db::{models::DefaultConfig, schema::default_configs::dsl::default_configs},
    helpers::validate_jsonschema
};
use actix_web::{
    put,
    web::{self, Data},
    HttpResponse, Scope,
};
use chrono::Utc;
use dashboard_auth::{middleware::acl, types::User};
use diesel::RunQueryDsl;
use jsonschema::{Draft, JSONSchema};
use serde_json::Value;
use service_utils::service::types::AppState;

pub fn endpoints() -> Scope {
    Scope::new("")
        .guard(acl([("mjos_manager".into(), "RW".into())]))
        .service(create)
}

#[put("/{key}")]
async fn create(
    state: Data<AppState>,
    key: web::Path<String>,
    request: web::Json<CreateReq>,
    user: User,
) -> HttpResponse {
    let req = request.into_inner();
    let schema = Value::Object(req.schema);
    if let Err(e) = validate_jsonschema(&state.default_config_validation_schema, &schema)
    {
        return HttpResponse::BadRequest().body(e);
    };
    let schema_compile_result = JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&schema);
    let jschema = match schema_compile_result {
        Ok(jschema) => jschema,
        Err(e) => {
            log::info!("Failed to compile as a Draft-7 JSON schema: {e}");
            return HttpResponse::BadRequest().body("Bad json schema.");
        }
    };

    match jschema.validate(&req.value) {
        Ok(_) => (),
        Err(_) => {
            log::info!("Validation for value with given JSON schema failed.");
            return HttpResponse::BadRequest()
                .body("Validation with given schema failed.");
        }
    };

    let new_default_config = DefaultConfig {
        key: key.into_inner(),
        value: req.value,
        schema: schema,
        created_by: user.email,
        created_at: Utc::now(),
    };

    let mut conn = match state.db_pool.get() {
        Ok(conn) => conn,
        Err(e) => {
            log::info!("unable to get db connection from pool, error: {e}");
            return HttpResponse::InternalServerError().finish();
        }
    };

    let upsert = diesel::insert_into(default_configs)
        .values(&new_default_config)
        .execute(&mut conn);

    match upsert {
        Ok(_) => {
            return HttpResponse::Created().body("DefaultConfig created successfully.")
        }
        Err(e) => {
            log::info!("DefaultConfig creation failed with error: {e}");
            return HttpResponse::InternalServerError()
                .body("Failed to create DefaultConfig");
        }
    }
}