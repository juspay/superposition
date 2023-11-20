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
use dashboard_auth::types::User;
use diesel::RunQueryDsl;
use jsonschema::{Draft, JSONSchema};
use service_utils::{
    service::types::{AppState, DbConnection},
    types as app,
};

pub fn endpoints() -> Scope {
    Scope::new("").service(create).service(get)
}

#[put("")]
async fn create(
    state: Data<AppState>,
    req: web::Json<CreateReq>,
    user: User,
    db_conn: DbConnection,
) -> HttpResponse {
    //TODO move this to the type itself rather than special if check
    let DbConnection(mut conn) = db_conn;

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
                .body("Failed to create/update dimension\n");
        }
    }
}

#[get("")]
async fn get(db_conn: DbConnection) -> app::Result<Json<Vec<Dimension>>> {
    let DbConnection(mut conn) = db_conn;

    let result: Vec<Dimension> = dimensions.get_results(&mut conn)?;
    Ok(Json(result))
}
