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
use serde::{Deserialize, Serialize};
use serde_json::Value;
use strum_macros::{Display, EnumString};

use crate::{
    messages::context_overrides::{CreateCtxOverrides, DeleteCtxOverrides, FetchCtxOverrides},
    AppState, DbActor,
};


#[derive(Debug, Display, EnumString)]
pub enum CtxOverrideError {
    BadRequest,
    FailedToAddCtxOverride,
    SomethingWentWrong,
    FailedToGetCtxOverride,
    CtxOverrideNotFound,
    DataAlreadyExists,
    DeletionFailed,
}

impl ResponseError for CtxOverrideError {
    fn error_response(&self) -> HttpResponse<actix_web::body::BoxBody> {
        HttpResponse::build(self.status_code())
            .insert_header(ContentType::json())
            .body(self.to_string())
    }

    fn status_code(&self) -> StatusCode {
        match self {
            CtxOverrideError::CtxOverrideNotFound => StatusCode::NOT_FOUND,
            CtxOverrideError::FailedToGetCtxOverride => StatusCode::INTERNAL_SERVER_ERROR,
            CtxOverrideError::SomethingWentWrong => StatusCode::FAILED_DEPENDENCY,
            CtxOverrideError::BadRequest => StatusCode::BAD_REQUEST,
            CtxOverrideError::FailedToAddCtxOverride => StatusCode::FAILED_DEPENDENCY,
            CtxOverrideError::DataAlreadyExists => StatusCode::FAILED_DEPENDENCY,
            CtxOverrideError::DeletionFailed => StatusCode::FAILED_DEPENDENCY,
        }
    }
}


#[derive(Deserialize)]
pub struct BodyType {
    context_id: String,
    override_id: String
}

#[derive(Serialize, Clone)]
pub struct IDResponse {
    id: String,
}

#[post("")]
pub async fn add_ctx_override(state: Data<AppState>, body: Json<BodyType>) -> Result<Json<IDResponse>, CtxOverrideError> {

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
        Ok(Err(_)) => Err(CtxOverrideError::CtxOverrideNotFound),
        _ => Err(CtxOverrideError::FailedToGetCtxOverride),
    }
}

#[get("/{id}")]
pub async fn get_ctx_override(state: Data<AppState>, id: Path<String>) -> Result<Json<Value>, CtxOverrideError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db
        .send(FetchCtxOverrides {
            key: id.to_string(),
        })
        .await
    {
        Ok(Ok(result)) => Ok(Json(serde_json::Value::String(result.key))),
        Ok(Err(_)) => Err(CtxOverrideError::CtxOverrideNotFound),
        _ => Err(CtxOverrideError::FailedToGetCtxOverride),
    }
}

#[delete("/{id}")]
pub async fn delete_ctx_override(state: Data<AppState>, id: Path<String>) -> Result<Json<Value>, CtxOverrideError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db
        .send(DeleteCtxOverrides {
            key: id.to_string(),
        })
        .await
    {
        Ok(Ok(result)) => Ok(Json(serde_json::Value::String(result.key))),
        Ok(_) => Err(CtxOverrideError::CtxOverrideNotFound),
        _ => Err(CtxOverrideError::DeletionFailed),
    }
}
