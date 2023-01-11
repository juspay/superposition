use std::collections::HashMap;

use actix::Addr;
use actix_web::{
    Either::{Left},
    delete,
    get,
    post,
    web::{Data, Json, Path},
};
use serde::{Deserialize, Serialize};
use serde_json::{to_value, Value};

use crate::{
    messages::context_overrides::{CreateCtxOverrides, DeleteCtxOverrides, FetchAllCtxOverrides, FetchCtxOverrides},
    AppState, DbActor, models::db_models::CtxOverrides,
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
pub struct ContextOverrideResponse {
    context_id: String,
}

// TODO :: Have to re-think and re-implement all these apis
pub async fn add_ctx_override(state: &Data<AppState>, context_id: String, override_id: String, return_if_present: bool) -> Result<Json<ContextOverrideResponse>, AppError> {

    let db: Addr<DbActor> = state.db.clone();

    match db
        .send(CreateCtxOverrides {context_id: context_id.to_owned(), override_id})
        .await
    {
        Ok(Ok(result)) => Ok(Json(ContextOverrideResponse {context_id: result.context_id})),
        Ok(Err(err)) =>
            if return_if_present {
                Ok(Json(ContextOverrideResponse {context_id}))
            } else {
                Err(AppError {
                    message: Some("Data already exists".to_string()),
                    cause: Some(Left(err.to_string())),
                    status: DataExists
                })
            },
        Err(err) => Err(AppError {message: None, cause: Some(Left(err.to_string())), status: DBError})
    }
}

pub async fn fetch_override_from_ctx_id(state: &Data<AppState>, context_id: &str) -> Result<String, AppError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db
        .send(FetchCtxOverrides {
            context_id: context_id.to_string()
        })
        .await
    {
        Ok(Ok(result)) => Ok(result.override_id),
        Ok(Err(err)) => Err(AppError {
            message: Some("failed to fetch key value".to_string()),
            cause: Some(Left(err.to_string())),
            status: NotFound
        }),
        Err(err) => Err(AppError {message: None, cause: Some(Left(err.to_string())), status: DBError})
    }
}

pub async fn fetch_all_ctx_overrides(state: &Data<AppState>) -> Result<Vec<CtxOverrides>, AppError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db
        .send(FetchAllCtxOverrides)
        .await
    {
        Ok(Ok(result)) => Ok(result),
        Ok(Err(err)) => Err(AppError {
            message: Some("failed to fetch key value".to_string()),
            cause: Some(Left(err.to_string())),
            status: NotFound
        }),
        Err(err) => Err(AppError {message: None, cause: Some(Left(err.to_string())), status: DBError})
    }
}

#[post("")]
pub async fn post_ctx_override(state: Data<AppState>, body: Json<BodyType>) -> Result<Json<ContextOverrideResponse>, AppError> {
    let ctx_id: String = body.context_id.clone();
    let ovr_id : String = body.override_id.clone();
    add_ctx_override(&state, ctx_id, ovr_id, false).await
}

#[get("/{id}")]
pub async fn get_ctx_override(state: Data<AppState>, id: Path<String>) -> Result<Json<Value>, AppError> {
    let context_id = id.to_string();
    let override_id = fetch_override_from_ctx_id(&state, &context_id).await?;

    Ok(Json(
        to_value(
            HashMap::from([
                ("context_id", context_id),
                ("override_id", override_id)
            ])
        ).map_err(|err| AppError {
            message: None,
            cause: Some(Left(err.to_string())),
            status: DBError
        })?
    ))
}

#[delete("/{id}")]
pub async fn delete_ctx_override(state: Data<AppState>, id: Path<String>) -> Result<Json<Value>, AppError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db
        .send(DeleteCtxOverrides {
            context_id: id.to_string(),
        })
        .await
    {
        Ok(Ok(result)) => Ok(Json(serde_json::Value::String(result.context_id))),
        Ok(Err(err)) => Err(AppError {
            message: Some("Data not found for context override deletion".to_string()),
            cause: Some(Left(err.to_string())),
            status: NotFound
        }),
        Err(err) => Err(AppError {message: None, cause: Some(Left(err.to_string())), status: DBError})
    }
}
