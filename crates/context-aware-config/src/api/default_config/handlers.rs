use super::helpers::validate_schema;
use super::types::CreateReq;
use crate::db::{
    models::DefaultConfig, schema::cac_v1::default_configs::dsl::default_configs,
};
use actix_web::{
    put,
    web::{self, Data},
    HttpResponse, Scope,
};
use chrono::Utc;
use diesel::RunQueryDsl;
use jsonschema::{Draft, JSONSchema};
use serde_json::Value;
use service_utils::service::types::{AppState, AuthenticationInfo};

pub fn endpoints() -> Scope {
    Scope::new("").service(create)
}

#[put("/{key}")]
async fn create(
    state: Data<AppState>,
    key: web::Path<String>,
    request: web::Json<CreateReq>,
    auth_info: AuthenticationInfo,
) -> HttpResponse {
    let req = request.into_inner();
    let schema = Value::Object(req.schema);
    if let Err(e) =
        validate_schema(&state.default_config_validation_schema, schema.to_owned())
    {
        return HttpResponse::BadRequest().body(e);
    };
    let schema_compile_result = JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&schema);
    let jschema = match schema_compile_result {
        Ok(jschema) => jschema,
        Err(e) => {
            println!("Failed to compile as a Draft-7 JSON schema: {e}");
            return HttpResponse::BadRequest().body("Bad json schema.");
        }
    };

    match jschema.validate(&req.value) {
        Ok(_) => (),
        Err(_) => {
            println!("Validation for value with given JSON schema failed.");
            return HttpResponse::BadRequest()
                .body("Validation with given schema failed.");
        }
    };

    let AuthenticationInfo(email) = auth_info;

    let new_default_config = DefaultConfig {
        key: key.into_inner(),
        value: req.value,
        schema: schema,
        created_by: email,
        created_at: Utc::now(),
    };

    let mut conn = match state.db_pool.get() {
        Ok(conn) => conn,
        Err(e) => {
            println!("unable to get db connection from pool, error: {e}");
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
            println!("DefaultConfig creation failed with error: {e}");
            return HttpResponse::InternalServerError()
                .body("Failed to create DefaultConfig");
        }
    }
}
