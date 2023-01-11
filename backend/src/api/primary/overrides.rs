use actix::Addr;
use actix_web::{
    delete,
    get,
    post,
    web::{Data, Json, Path},
    Either::{Left, Right}
};
use serde::Serialize;
use serde_json::{Value};

use crate::{
    messages::overrides::{CreateOverride, DeleteOverride, FetchOverride},
    AppState, DbActor,
};

use crate::utils::{
    errors::{
        AppError,
        AppErrorType::{
            BadRequest,
            DataExists,
            DBError,
            SomethingWentWrong,
            NotFound
        }
    },
    hash::string_based_b64_hash,
    helpers::sort_multi_level_keys_in_stringified_json,
    validations::validate_sub_tree,
};

use crate::api::primary::global_config::get_complete_config;

#[derive(Serialize, Clone)]
pub struct OverrideIdResponse {
    pub id: String,
}


pub async fn add_new_override(state: &Data<AppState>, override_value: Value, return_if_present: bool) -> Result<OverrideIdResponse, AppError> {
    let db: Addr<DbActor> = state.db.clone();

    let global_config_as_value = get_complete_config(&state).await?;

    if let Err(error_message) = validate_sub_tree(&global_config_as_value, &override_value) {
        return Err(AppError {
            message: Some("Validation failed".to_string()),
            cause: Some(Right(error_message)),
            status: BadRequest
        })
    }

    // TODO :: Post as an array of value
    let formatted_value =
        sort_multi_level_keys_in_stringified_json(override_value)
        // TODO :: Fix this properly
        // .ok_or(OverrideError::ErrorOnParsingBody {error_message : to_value("Error on sorting keys".to_string())})?;
        .ok_or(AppError {
            message: Some("Unable to parse override value".to_string()),
            cause: None,
            status: SomethingWentWrong
        })?;

    let hashed_value = string_based_b64_hash((&formatted_value).to_string()).to_string();

    match db
        .send(CreateOverride {
            key: hashed_value.to_owned(),
            value: formatted_value,
        })
        .await
    {
        Ok(Ok(result)) => Ok(OverrideIdResponse {id: result.key}),
        Ok(Err(err)) =>
            if return_if_present {
                Ok(OverrideIdResponse {id: hashed_value})
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

#[post("")]
pub async fn post_override(state: Data<AppState>, body: Json<Value>) -> Result<Json<OverrideIdResponse>, AppError> {
    let override_value = body.clone();
    Ok(Json(add_new_override(&state, override_value, false).await?))
}

pub async fn get_override_helper(state: &Data<AppState>, key: String) -> Result<Json<Value>, AppError> {
    let db: Addr<DbActor> = state.db.clone();

    match db
        .send(FetchOverride {key})
        .await
    {
        Ok(Ok(result)) => Ok(Json(result.value)),
        Ok(Err(err)) => Err(AppError {
            message: Some("Failed to fetch value for given override key".to_string()),
            cause: Some(Left(err.to_string())),
            status: NotFound
        }),
        Err(err) => Err(AppError {message: None, cause: Some(Left(err.to_string())), status: DBError})
    }
}

#[get("/{key}")]
pub async fn get_override(state: Data<AppState>, key: Path<String>) -> Result<Json<Value>, AppError> {
    get_override_helper(&state, key.to_owned()).await
}

#[delete("/{key}")]
pub async fn delete_override(state: Data<AppState>, id: Path<String>) -> Result<Json<Value>, AppError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db
        .send(DeleteOverride {
            key: id.to_string(),
        })
        .await
    {
        Ok(Ok(result)) => Ok(Json(result.value)),
        Ok(Err(err)) => Err(AppError {
            message: Some("Data not found".to_string()),
            cause: Some(Left(err.to_string())),
            status: NotFound
        }),
        Err(err) => Err(AppError {message: None, cause: Some(Left(err.to_string())), status: DBError})
    }
}
