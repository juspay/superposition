use actix::Addr;
use actix_web::{
    Either::{Left},
    delete,
    get,
    post,
    web::{Data, Json, Path},
};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::{
    messages::context_overrides::{CreateCtxOverrides, DeleteCtxOverrides, FetchCtxOverrides},
    AppState, DbActor,
};

use crate::utils::{
    errors::{
        AppError,
        AppErrorType::{
            DataExists,
            DBError,
            NotFound
        }
    },
};

#[derive(Deserialize)]
pub struct BodyType {
    context_id: String,
    override_id: String
}

#[derive(Serialize)]
pub struct IDResponse {
    id: String,
}

// TODO :: Have to re-think and re-implement all these apis
#[post("")]
pub async fn add_ctx_override(state: Data<AppState>, body: Json<BodyType>) -> Result<Json<IDResponse>, AppError> {

    let db: Addr<DbActor> = state.as_ref().db.clone();
    let ctx_id: String = body.context_id.clone();
    let ovr_id : String = body.override_id.clone();
    let ctx_over_concat = "".to_string() + &ctx_id + &ovr_id;

    match db
        .send(CreateCtxOverrides {
            key: ctx_over_concat,
            context_id: ctx_id,
            override_id: ovr_id
        })
        .await
    {
        Ok(Ok(result)) => Ok(Json(IDResponse {id: result.key})),
        Ok(Err(err)) => Err(AppError {
            message: Some(Left("Data already exists".to_string())),
            cause: Some(err.to_string()),
            status: DataExists
        }),
        Err(err) => Err(AppError {message: None, cause: Some(err.to_string()), status: DBError})
    }
}

#[get("/{id}")]
pub async fn get_ctx_override(state: Data<AppState>, id: Path<String>) -> Result<Json<Value>, AppError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db
        .send(FetchCtxOverrides {
            key: id.to_string(),
        })
        .await
    {
        Ok(Ok(result)) => Ok(Json(serde_json::Value::String(result.key))),
        Ok(Err(err)) => Err(AppError {
            message: Some(Left("failed to fetch key value".to_string())),
            cause: Some(err.to_string()),
            status: NotFound
        }),
        Err(err) => Err(AppError {message: None, cause: Some(err.to_string()), status: DBError})
    }
}

#[delete("/{id}")]
pub async fn delete_ctx_override(state: Data<AppState>, id: Path<String>) -> Result<Json<Value>, AppError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db
        .send(DeleteCtxOverrides {
            key: id.to_string(),
        })
        .await
    {
        Ok(Ok(result)) => Ok(Json(serde_json::Value::String(result.key))),
        Ok(Err(err)) => Err(AppError {
            message: Some(Left("Data not found".to_string())),
            cause: Some(err.to_string()),
            status: NotFound
        }),
        Err(err) => Err(AppError {message: None, cause: Some(err.to_string()), status: DBError})
    }
}
