use std::collections::{BTreeMap, HashMap};

use crate::db::models::db_models::GlobalConfig;
use crate::utils::errors::{
    AppError,
    AppErrorType::{DBError, DataExists, NotFound, SomethingWentWrong},
};
use crate::{
    db::messages::global_config::{CreateGlobalKey, FetchConfigKey, FetchGlobalConfig},
    AppState, DbActor,
};
use actix::Addr;
use actix_web::{
    get, post,
    web::{Data, Json, Path},
    Either::Left,
};
use serde::{Deserialize, Serialize};
use serde_json::{from_value, to_value, Value};

async fn get_all_rows_from_global_config(
    state: &Data<AppState>,
) -> Result<Vec<GlobalConfig>, AppError> {
    let db: Addr<DbActor> = state.db.clone();
    match db.send(FetchGlobalConfig).await {
        Ok(Ok(result)) => Ok(result),
        Ok(Err(err)) => Err(AppError {
            message: Some("config not found".to_string()),
            cause: Some(Left(err.to_string())),
            status: NotFound,
        }),
        Err(err) => Err(AppError {
            message: None,
            cause: Some(Left(err.to_string())),
            status: DBError,
        }),
    }
}

pub async fn get_complete_config(state: &Data<AppState>) -> Result<Value, AppError> {
    let db_rows = get_all_rows_from_global_config(&state).await?;
    let mut hash_map: HashMap<String, Value> = HashMap::new();

    for row in db_rows {
        hash_map.insert(row.key, row.value);
    }

    to_value(&hash_map).map_err(|err| AppError {
        message: Some("Unable to fetch global config".to_string()),
        cause: Some(Left(err.to_string())),
        status: SomethingWentWrong,
    })
}

// Get whole global config
#[get("")]
pub async fn get_global_config(state: Data<AppState>) -> Result<Json<Value>, AppError> {
    Ok(Json(get_complete_config(&state).await?))
}

// Get request to fetch value for given key
#[derive(Deserialize, Serialize)]
pub struct Key {
    key: String,
}

#[get("/{key}")]
pub async fn get_global_config_key(
    state: Data<AppState>,
    params: Path<Key>,
) -> Result<Json<GlobalConfig>, AppError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();
    let key = params.into_inner().key;

    match db.send(FetchConfigKey { key }).await {
        Ok(Ok(result)) => Ok(Json(result)),
        Ok(Err(err)) => Err(AppError {
            message: Some("Failed to fetch global config".to_string()),
            cause: Some(Left(err.to_string())),
            status: NotFound,
        }),
        Err(err) => Err(AppError {
            message: None,
            cause: Some(Left(err.to_string())),
            status: DBError,
        }),
    }
}

#[post("")]
pub async fn post_config_key_value(
    state: Data<AppState>,
    body: Json<Value>,
) -> Result<Json<Value>, AppError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    let b_tree: BTreeMap<String, Value> = from_value(body.clone()).map_err(|err| AppError {
        message: None,
        cause: Some(Left(err.to_string())),
        status: DBError,
    })?;

    for (key, value) in b_tree {
        match db.send(CreateGlobalKey { key, value }).await {
            Ok(Ok(result)) => Ok(Json(result)),
            Ok(Err(err)) => Err(AppError {
                message: Some("Failed to add new key to global config".to_string()),
                cause: Some(Left(err.to_string())),
                status: DataExists,
            }),
            Err(err) => Err(AppError {
                message: None,
                cause: Some(Left(err.to_string())),
                status: DBError,
            }),
        }?;
    }

    Ok(body)
}
