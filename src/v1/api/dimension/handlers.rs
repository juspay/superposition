use crate::{
    db::utils::AppState,
    v1::{
        api::dimension::types::CreateReq,
        db::{models::Dimension, schema::dimensions::dsl::*},
    },
};
use actix_web::{
    delete, put,
    web::{self, Data},
    HttpResponse, Scope,
};
use chrono::Utc;
use diesel::{prelude::*, QueryDsl, RunQueryDsl};

pub fn endpoints() -> Scope {
    Scope::new("").service(create).service(delete)
}

#[put("")]
async fn create(state: Data<AppState>, req: web::Json<CreateReq>) -> HttpResponse {
    //TODO move this to the type itself rather than special if check
    if req.priority <= 0 {
        return HttpResponse::BadRequest().body("Priority should be greater than 0");
    }
    let new_dimension = Dimension {
        dimension: req.dimension.clone(),
        priority: i32::from(req.priority),
        type_: req.r#type,
        created_by: String::from("some_user"), //TODO update after authentication is added
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
        Ok(_) => return HttpResponse::Created().body("Dimension created/updated successfully."),
        Err(e) => {
            println!("Dimension upsert failed with error: {e}");
            return HttpResponse::InternalServerError().body("Failed to create/udpate dimension\n");
        }
    }
}

#[delete("/{dimension_name}")]
async fn delete(state: Data<AppState>, path: web::Path<String>) -> HttpResponse {
    let dimension_name = path.into_inner();

    let mut conn = match state.db_pool.get() {
        Ok(conn) => conn,
        Err(e) => {
            println!("unable to get db connection from pool, error: {e}");
            return HttpResponse::InternalServerError().finish();
        }
    };

    let delete = diesel::delete(QueryDsl::filter(dimensions, dimension.eq(&dimension_name)))
        .execute(&mut conn);

    match delete {
        Ok(_) => HttpResponse::Ok().body("Dimension: {dimension_name} deleted successfully"),
        Err(e) => {
            println!("unable to delete dimension {dimension_name}, error: {e}");
            return HttpResponse::InternalServerError().finish();
        }
    }
}
