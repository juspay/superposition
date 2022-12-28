use actix::Addr;
use actix_web::{
    Either::{Left},
    delete,
    get,
    post,
    web::{Data, Json, Path},
};
use serde::Serialize;
use serde_json::Value;


use crate::{
    messages::contexts::{CreateContext, DeleteContext, FetchContext},
    AppState, DbActor,
};

use crate::utils::{
    hash::string_based_b64_hash,
    helpers::sort_multi_level_keys_in_stringified_json,
    errors::{
        AppError,
        AppErrorType::{
            DataExists,
            NotFound,
            DBError,
            BadRequest
        }
    }
};


#[derive(Serialize, Clone)]
pub struct IDResponse {
    id: String,
}


#[post("")]
pub async fn post_context(state: Data<AppState>, body: Json<Value>) -> Result<Json<IDResponse>, AppError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    // TODO :: Post as an array of value
    let context_value = body.clone();

    // TODO :: Sort query based on key and add to DB
    let formatted_value =
        sort_multi_level_keys_in_stringified_json(context_value)
        .ok_or(AppError {
            message: Some("error in parsing context".to_string()),
            cause: Some(Left("ill formed context".to_string())),
            status: BadRequest
        })?;

    let hashed_value = string_based_b64_hash((&formatted_value).to_string()).to_string();


    match db
        .send(CreateContext {
            key: hashed_value,
            value: formatted_value,
        })
        .await
    {
        Ok(Ok(result)) => Ok(Json(IDResponse {id: result.key})),
        Ok(Err(err)) => Err(AppError {
                message: Some("failed to add context".to_string()),
                cause: Some(Left(err.to_string())),
                status: DataExists
            }),
        Err(err) => Err(AppError {
                message: None,
                cause: Some(Left(err.to_string())),
                status: DBError
            }),
    }
}

#[get("/{key}")]
pub async fn get_context(state: Data<AppState>, id: Path<String>) -> Result<Json<Value>, AppError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db
        .send(FetchContext {
            key: id.to_string(),
        })
        .await
    {
        Ok(Ok(result)) => Ok(Json(result.value)),
        Ok(Err(err)) => Err(AppError {
                message: Some("failed to get context".to_string()),
                cause: Some(Left(err.to_string())),
                status: NotFound
            }),
        Err(err) => Err(AppError {
                message: None,
                cause: Some(Left(err.to_string())),
                status: DBError
            }),

    }
}

#[delete("/{key}")]
pub async fn delete_context(state: Data<AppState>, key: Path<String>) -> Result<Json<Value>, AppError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db
        .send(DeleteContext {
            key: key.to_string(),
        })
        .await
    {
        Ok(Ok(result)) => Ok(Json(result.value)),
        Ok(Err(err)) => Err(AppError {
                message: Some("failed to remove context".to_string()),
                cause: Some(Left(err.to_string())),
                status: NotFound
            }),
        Err(err) => Err(AppError {
                message: None,
                cause: Some(Left(err.to_string())),
                status: DBError
            }),

    }
}
