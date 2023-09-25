use crate::{
    api::dimension::types::CreateReq,
    db::{models::Dimension, schema::cac_v1::dimensions::dsl::*},
    helpers::validate_jsonschema,
};
use actix_web::{
    put,
    web::{self, Data},
    HttpResponse, Scope,
};
use chrono::Utc;
use dashboard_auth::{
    middleware::acl,
    types::User,
};
use diesel::RunQueryDsl;
use jsonschema::{Draft, JSONSchema};
use service_utils::service::types::AppState;

pub fn endpoints() -> Scope {
    Scope::new("")
        .guard(acl([("mjos_manager".into(), "RW".into())]))
        .service(create)
}

#[put("")]
async fn create(
    state: Data<AppState>,
    req: web::Json<CreateReq>,
    user: User,
) -> HttpResponse {
    //TODO move this to the type itself rather than special if check
    if req.priority <= 0 {
        return HttpResponse::BadRequest().body("Priority should be greater than 0");
    }

    let create_req = req.into_inner();
    let schema_value = create_req.schema;

    if let Err(e) = validate_jsonschema(&state.meta_schema, &schema_value) {
        return HttpResponse::BadRequest().body(e);
    };

    let schema_compile_result = JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&schema_value);

    if let Err(e) = schema_compile_result {
        return HttpResponse::BadRequest()
            .body(String::from(format!("Bad schema: {:?}", e)));
    };

    let new_dimension = Dimension {
        dimension: create_req.dimension,
        priority: i32::from(create_req.priority),
        schema: schema_value,
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

    let upsert = diesel::insert_into(dimensions)
        .values(&new_dimension)
        .on_conflict(dimension)
        .do_update()
        .set(&new_dimension)
        .execute(&mut conn);

    match upsert {
        Ok(_) => {
            return HttpResponse::Created()
                .body("Dimension created/updated successfully.")
        }
        Err(e) => {
            log::info!("Dimension upsert failed with error: {e}");
            return HttpResponse::InternalServerError()
                .body("Failed to create/udpate dimension\n");
        }
    }
}
