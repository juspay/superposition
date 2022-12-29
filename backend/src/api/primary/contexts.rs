use std::collections::{
    BTreeMap,
    HashMap
};

use actix::Addr;
use actix_web::{
    Either::{Left},
    delete,
    get,
    post,
    web::{Data, Json, Path},
};
use serde::Serialize;
use serde_json::{from_value, Error, Value};

use crate::{
    messages::contexts::{CreateContext, DeleteContext, FetchContext},
    AppState, DbActor,
};

use crate::utils::{
    hash::string_based_b64_hash,
    errors::{
        AppError,
        AppErrorType::{
            DataExists,
            NotFound,
            DBError,
            SomethingWentWrong
        }
    }
};


#[derive(Serialize, Clone)]
pub struct ContextIdResponse {
    pub id: String,
}

fn default_error(err: Error) -> AppError{
    AppError {
        message: None,
        cause: Some(Left(err.to_string())),
        status: SomethingWentWrong
    }
}

// TODO :: Implement Range based transforms
fn transform_context(raw_context_value: Value) -> Result<String, AppError> {

    // BTreeMap is used to make keys in sorted order
    let b_tree: BTreeMap<String, Value> = from_value(raw_context_value).map_err(default_error)?;

    let mut result: Vec<String> = Vec::new();

    for (key, value) in b_tree {
        let value_object: HashMap<String, String> = from_value(value).map_err(default_error)?;

        let operator = value_object.get("operator").map(|val| val.to_string());
        let value = value_object.get("value").map(|val| val.to_string());

        let max_range = value_object.get("max_range").map(|val| val.to_string());
        let min_range = value_object.get("min_range").map(|val| val.to_string());

        match (operator.as_deref(), value, max_range, min_range) {

            // ? `==` or `equals` ?
            (Some("=="), Some(val), _, _) => result.push(key + "=" + &val),

            // TODO :: Implement Range based transforms properly
            // ? Do we need to add inclusive check
            (Some("range"), _, Some(max_range_val), Some(min_range_val))  => result.push(min_range_val + "<" + &key + "<" + &max_range_val),
            (_, _, _, _) => ()
        };

    }

    Ok(result.join("&"))
}


pub async fn add_new_context(state: &Data<AppState>, context_value: Value) -> Result<ContextIdResponse, AppError> {
    let db: Addr<DbActor> = state.db.clone();

    let transformed_context_value = transform_context(context_value)?;

    // ? TODO :: Post as an array of value
    // ? TODO :: Sort query based on key and add to DB
    let hashed_value = string_based_b64_hash(&transformed_context_value).to_string();

    match db
        .send(CreateContext {
            key: hashed_value,
            value: transformed_context_value,
        })
        .await
    {
        Ok(Ok(result)) => Ok(ContextIdResponse {id: result.key}),
        Ok(Err(err)) => Err(AppError {
                message: Some("Failed to add context".to_string()),
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


#[post("")]
pub async fn post_context(state: Data<AppState>, body: Json<Value>) -> Result<Json<ContextIdResponse>, AppError> {
    let context_value = body.clone();
    Ok(Json(add_new_context(&state, context_value).await?))
}

#[get("/{key}")]
pub async fn get_context(state: Data<AppState>, id: Path<String>) -> Result<Json<String>, AppError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db
        .send(FetchContext {
            key: id.to_string(),
        })
        .await
    {
        Ok(Ok(result)) => Ok(Json(result.value)),
        Ok(Err(err)) => Err(AppError {
                message: Some("Failed to get context".to_string()),
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
pub async fn delete_context(state: Data<AppState>, key: Path<String>) -> Result<Json<String>, AppError> {
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
