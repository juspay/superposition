use crate::{
    db::utils::AppState,
    v1::{
        api::{dimension::types::CreateReq, types::AuthenticationInfo},
        db::{models::Dimension, schema::cac_v1::dimensions::dsl::*},
    },
};
use actix_web::{
    put,
    web::{self, Data},
    HttpResponse, Scope,
};
use chrono::Utc;
use diesel::RunQueryDsl;

pub fn endpoints() -> Scope {
    Scope::new("").service(create)
}

#[put("")]
async fn create(
    state: Data<AppState>,
    req: web::Json<CreateReq>,
    auth_info: AuthenticationInfo,
) -> HttpResponse {
    //TODO move this to the type itself rather than special if check
    if req.priority <= 0 {
        return HttpResponse::BadRequest().body("Priority should be greater than 0");
    }

    let AuthenticationInfo(email) = auth_info;

    let new_dimension = Dimension {
        dimension: req.dimension.clone(),
        priority: i32::from(req.priority),
        type_: req.r#type,
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
            println!("Dimension upsert failed with error: {e}");
            return HttpResponse::InternalServerError()
                .body("Failed to create/udpate dimension\n");
        }
    }
}
