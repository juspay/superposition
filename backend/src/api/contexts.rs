use actix::Addr;
use actix_web::{
    delete,
    error::ResponseError,
    get,
    http::{header::ContentType, StatusCode},
    post,
    web::{Data, Json, Path},
    HttpResponse,
};
use serde::Serialize;
use serde_json::Value;
use strum_macros::{Display, EnumString};


use crate::{
    messages::contexts::{CreateContext, DeleteContext, FetchContext},
    AppState, DbActor,
};

use crate::utils::{
    hash::string_based_b64_hash,
    helpers::sort_multi_level_keys_in_stringified_json,
};


#[derive(Debug, Display, EnumString)]
pub enum ContextError {
    BadRequest,
    FailedToAddContext,
    SomethingWentWrong,
    FailedToGetContext,
    ContextNotFound,
    DataAlreadyExists,
    DeletionFailed,
}

impl ResponseError for ContextError {
    fn error_response(&self) -> HttpResponse<actix_web::body::BoxBody> {
        HttpResponse::build(self.status_code())
            .insert_header(ContentType::json())
            .body(self.to_string())
    }

    fn status_code(&self) -> StatusCode {
        match self {
            ContextError::ContextNotFound => StatusCode::NOT_FOUND,
            ContextError::FailedToGetContext => StatusCode::INTERNAL_SERVER_ERROR,
            ContextError::SomethingWentWrong => StatusCode::FAILED_DEPENDENCY,
            ContextError::BadRequest => StatusCode::BAD_REQUEST,
            ContextError::FailedToAddContext => StatusCode::FAILED_DEPENDENCY,
            ContextError::DataAlreadyExists => StatusCode::FAILED_DEPENDENCY,
            ContextError::DeletionFailed => StatusCode::FAILED_DEPENDENCY,
        }
    }
}

#[derive(Serialize, Clone)]
pub struct IDResponse {
    id: String,
}


#[post("")]
pub async fn post_context(state: Data<AppState>, body: Json<Value>) -> Result<Json<IDResponse>, ContextError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    // TODO :: Handle unwraps properly
    // TODO :: Post as an array of value
    let override_value = body.clone();
    let formatted_value = sort_multi_level_keys_in_stringified_json(override_value).unwrap();
    let hashed_value = string_based_b64_hash((&formatted_value).to_string()).to_string();


    match db
        .send(CreateContext {
            key: hashed_value,
            value: formatted_value,
        })
        .await
    {
        Ok(Ok(info)) => Ok(Json(IDResponse {id: info.key})),
        Ok(Err(_)) => Err(ContextError::DataAlreadyExists),
        _ => Err(ContextError::FailedToAddContext),
    }
}

#[get("/{key}")]
pub async fn get_context(state: Data<AppState>, id: Path<String>) -> Result<Json<Value>, ContextError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db
        .send(FetchContext {
            key: id.to_string(),
        })
        .await
    {
        Ok(Ok(info)) => Ok(Json(info.value)),
        Ok(Err(_)) => Err(ContextError::ContextNotFound),
        _ => Err(ContextError::FailedToGetContext),
    }
}

#[delete("/{key}")]
pub async fn delete_context(state: Data<AppState>, key: Path<String>) -> Result<Json<Value>, ContextError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db
        .send(DeleteContext {
            key: key.to_string(),
        })
        .await
    {
        Ok(Ok(info)) => Ok(Json(info.value)),
        Ok(_) => Err(ContextError::ContextNotFound),
        _ => Err(ContextError::DeletionFailed),
    }
}
