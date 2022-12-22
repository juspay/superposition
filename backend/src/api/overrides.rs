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
use serde_json::{Value, to_value};
use strum_macros::{Display, EnumString};

use crate::{
    messages::overrides::{CreateOverride, DeleteOverride, FetchOverride},
    AppState, DbActor,
};

use crate::utils::{
    hash::string_based_b64_hash,
    helpers::sort_multi_level_keys_in_stringified_json,
};

use crate::api::global_config::{get_complete_config};
use crate::utils::validations::validate_sub_tree;

#[derive(Debug, Display, EnumString)]
pub enum OverrideError {
    BadRequest,
    FailedToAddOverride,
    SomethingWentWrong,
    FailedToGetOverride,
    OverrideNotFound,
    DataAlreadyExists,
    DeletionFailed,
    ErrorOnParsingBody,
}

impl ResponseError for OverrideError {
    fn error_response(&self) -> HttpResponse<actix_web::body::BoxBody> {
        HttpResponse::build(self.status_code())
            .insert_header(ContentType::json())
            .body(self.to_string())
    }

    fn status_code(&self) -> StatusCode {
        match self {
            OverrideError::OverrideNotFound => StatusCode::NOT_FOUND,
            OverrideError::FailedToGetOverride => StatusCode::INTERNAL_SERVER_ERROR,
            OverrideError::SomethingWentWrong => StatusCode::FAILED_DEPENDENCY,
            OverrideError::BadRequest => StatusCode::BAD_REQUEST,
            OverrideError::FailedToAddOverride => StatusCode::FAILED_DEPENDENCY,
            OverrideError::DataAlreadyExists => StatusCode::FAILED_DEPENDENCY,
            OverrideError::DeletionFailed => StatusCode::FAILED_DEPENDENCY,
            OverrideError::ErrorOnParsingBody => StatusCode::BAD_REQUEST,
        }
    }
}

#[derive(Serialize, Clone)]
pub struct IDResponse {
    id: String,
}

#[post("")]
pub async fn post_override(state: Data<AppState>, body: Json<Value>) -> Result<Json<IDResponse>, OverrideError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    let global_config =
        get_complete_config(db.clone()).await
        .map_err(|_| OverrideError::FailedToAddOverride)?;

    let global_config_as_value = to_value(global_config).map_err(|_| OverrideError::FailedToAddOverride)?;

    if let Err(error_message) = validate_sub_tree(&global_config_as_value, &body) {
        // TODO :: Add code to parse error value into response body
        return Err(OverrideError::ErrorOnParsingBody);
    }


    // TODO :: Post as an array of value
    let override_value = body.clone();
    let formatted_value =
        sort_multi_level_keys_in_stringified_json(override_value)
        .ok_or(OverrideError::ErrorOnParsingBody)?;

    let hashed_value = string_based_b64_hash((&formatted_value).to_string()).to_string();

    match db
        .send(CreateOverride {
            key: hashed_value,
            value: formatted_value,
        })
        .await
    {
        Ok(Ok(info)) => Ok(Json(IDResponse {id: info.key})),
        Ok(Err(_)) => Err(OverrideError::DataAlreadyExists),
        _ => Err(OverrideError::FailedToAddOverride),
    }
}

#[get("/{key}")]
pub async fn get_override(state: Data<AppState>, key: Path<String>) -> Result<Json<Value>, OverrideError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db
        .send(FetchOverride {
            key: key.to_string(),
        })
        .await
    {
        Ok(Ok(info)) => Ok(Json(info.value)),
        Ok(Err(_)) => Err(OverrideError::OverrideNotFound),
        _ => Err(OverrideError::FailedToGetOverride),
    }
}

#[delete("/{key}")]
pub async fn delete_override(state: Data<AppState>, id: Path<String>) -> Result<Json<Value>, OverrideError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db
        .send(DeleteOverride {
            key: id.to_string(),
        })
        .await
    {
        Ok(Ok(info)) => Ok(Json(info.value)),
        Ok(_) => Err(OverrideError::OverrideNotFound),
        _ => Err(OverrideError::DeletionFailed),
    }
}
