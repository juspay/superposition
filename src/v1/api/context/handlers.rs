use crate::{
    db::utils::AppState,
    v1::{
        api::context::types::{AddContextReq, AddContextResp},
        db::{
            models::{Context, Override},
            schema::{contexts::dsl::contexts, overrides::dsl::overrides},
        },
    },
};
use actix_web::{
    put, get,
    web::{self, Data},
    HttpResponse, Scope, Result, Responder, error
};
use chrono::Utc;
use diesel::{
    result::{DatabaseErrorKind::*, Error::DatabaseError},
    RunQueryDsl,
    QueryDsl, ExpressionMethods, QueryResult
};
use serde_json::json;

pub fn endpoints() -> Scope {
    Scope::new("")
	.service(add_contexts_overrides)
	.service(get_context)
}

#[put("add")]
async fn add_contexts_overrides(
    req: web::Json<AddContextReq>,
    state: Data<AppState>,
) -> HttpResponse {
    let ctxt_cond = json!({
        "and": req.context
    });
    let context_id = blake3::hash((ctxt_cond).to_string().as_bytes()).to_string();
    let override_id = blake3::hash((req.r#override).to_string().as_bytes()).to_string();

    let new_override = Override {
        id: override_id.clone(),
        value: req.r#override.clone(),
        created_at: Utc::now(),
        created_by: "some_user".to_string(), //TODO update once authentication is added
    };

    let new_ctxt = Context {
        id: context_id.clone(),
        value: ctxt_cond,
        override_id: override_id.clone(),
        created_at: Utc::now(),
        created_by: "some_user".to_string(),
    };

    let mut conn = match state.db_pool.get() {
        Ok(conn) => conn,
        Err(e) => {
            println!("unable to get db connection from pool, error: {e}");
            return HttpResponse::InternalServerError().finish();
        }
    };

    let txn = conn.build_transaction().run(|conn| {
        diesel::insert_into(overrides)
            .values(&new_override)
            .on_conflict_do_nothing()
            .execute(conn)?;
        diesel::insert_into(contexts)
            .values(&new_ctxt)
            .execute(conn)
    });

    let resp = AddContextResp {
        context_id,
        override_id,
    };

    match txn {
        Ok(_) => HttpResponse::Created()
            .insert_header(("x-info", "new context created"))
            .json(resp),
        Err(DatabaseError(UniqueViolation, _)) => HttpResponse::Ok()
            .insert_header(("x-info", "context already exists"))
            .json(resp),
        e => {
            println!("DB transaction failed with error: {e:?}");
            return HttpResponse::InternalServerError().finish();
        }
    }
}

#[get("/{ctx_id}")]
async fn get_context(
    path: web::Path<String>,
    state: Data<AppState>
) -> Result<impl Responder> {
    use crate::v1::db::schema::contexts::dsl::*;

    let ctx_id = path.into_inner();
    let mut conn = match state.db_pool.get() {
        Ok(conn) => conn,
        Err(e) => {
            println!("Unable to get db connection from pool, error: {e}");
            return Err(error::ErrorInternalServerError(""));
        }
    };

    let result: QueryResult<Vec<Context>> = contexts
        .filter(id.eq(ctx_id))
        .load(&mut conn);

    let ctx_vec = match result {
	Ok(ctx_vec) => ctx_vec,
        Err(e)      => {
            println!("Failed to execute query, error: {e}");
            return Err(error::ErrorInternalServerError(""));
        }
    };

    match ctx_vec.first() {
	Some(ctx) => Ok(web::Json(ctx.clone())),
	_         => Err(error::ErrorNotFound(""))
    }
}
