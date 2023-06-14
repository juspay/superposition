use actix_web::{Scope, web::{self, Data}, HttpResponse, put};
use chrono::Utc;
use crate::{db::utils::AppState, v1::db::{models::DefaultConfig, schema::default_configs::dsl::default_configs}};
use diesel::{RunQueryDsl};
use super::types::CreateReq;

pub fn endpoints() -> Scope {
    Scope::new("").service(create)
}

#[put("/{key}")]
async fn create(state: Data<AppState>, key: web::Path<String>, request: web::Json<CreateReq> ) -> HttpResponse {
    let req = request.into_inner();
    let new_default_config = DefaultConfig {
        key: key.into_inner(),
        value: req.value,
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

    let upsert = diesel::insert_into(default_configs)
        .values(&new_default_config)
        .execute(&mut conn);

    match upsert {
        Ok(_) => return HttpResponse::Created().body("DefaultConfig created successfully."),
        Err(e) => {
            println!("DefaultConfig creation failed with error: {e}");
            return HttpResponse::InternalServerError().body("Failed to create DefaultConfig");
        }
    }
}
