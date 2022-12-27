use std::collections::HashMap;

use actix_web:: {
    get,
    post,
    web::{Path, Json, Data},
};
use serde_json::{Value, to_value};
use crate::models::db_models::GlobalConfig;
use serde::{Serialize, Deserialize};
use crate::{
    messages::global_config::{
        FetchGlobalConfig,
        FetchConfigKey,
        CreateGlobalKey,
    }, AppState, DbActor};
use actix::Addr;
use crate::utils::errors::{
    AppError,
    AppErrorType::{
        SomethingWentWrong,
        DBError,
        NotFound
    }
};

async fn get_all_rows_from_global_config(state: Data<AppState>) -> Result<Vec<GlobalConfig>, AppError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();
    match db.send(FetchGlobalConfig).await {
        Ok(Ok(result)) => Ok(result),
        Ok(Err(err)) => Err(AppError {
            message: Some("config not found".to_string()),
            cause: Some(err.to_string()),
            status: NotFound
        }),
        Err(err) => Err(AppError {
            message: None,
            cause: Some(err.to_string()),
            status: DBError
        }),
    }
}

pub async fn get_complete_config(state: Data<AppState>) -> Result<Json<Value>, AppError> {
    let db_rows = get_all_rows_from_global_config(state).await?;
    let mut hash_map: HashMap<String, Value> = HashMap::new();

    for row in db_rows {
        hash_map.insert(row.key, row.value);
    }

    match to_value(hash_map.clone()) {
        Ok(res) => if hash_map.keys().len() == 0 {
                Err(AppError {
                    message: Some("failed to get global config".to_string()),
                    cause: Some("global config doesn't exist".to_string()),
                    status: NotFound
                })
            } else {
                Ok(Json(res))
            },
        Err(err) => Err(AppError {
            message: None,
            cause: Some(err.to_string()),
            status: SomethingWentWrong
        })

    }
}


// Get whole global config
#[get("")]
pub async fn get_global_config(state: Data<AppState>) -> Result<Json<Value>, AppError> {
    get_complete_config(state).await
}

// Get request to fetch value for given key
#[derive(Deserialize, Serialize)]
pub struct Key {
    key: String,
}

#[get("/{key}")]
pub async fn get_global_config_key(state: Data<AppState>, params: Path<Key>) -> Result<Json<GlobalConfig>, AppError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();
    let key = params.into_inner().key;

    match db.send(FetchConfigKey {key}).await {
        Ok(Ok(result)) => Ok(Json(result)),
        Ok(Err(err)) => Err(AppError {
            message: Some("failed to fetch key value".to_string()),
            cause: Some(err.to_string()),
            status: NotFound
        }),
        Err(err) => Err(AppError {message: None, cause: Some(err.to_string()), status: DBError})
    }
}


// Post request to add key, value
#[derive(Deserialize, Serialize)]
pub struct KeyValue {
    key: String,
    value: Value,
}

#[post("")]
pub async fn post_config_key_value(state: Data<AppState>, body: Json<KeyValue>) -> Result<Json<GlobalConfig>, AppError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db.send(CreateGlobalKey {
        key: body.key.clone(),
        value: body.value.clone()
    }).await {
        Ok(Ok(result)) => Ok(Json(result)),
        Ok(Err(err)) => Err(AppError {
                message: Some("failed to add new key to global config".to_string()),
                cause: Some(err.to_string()),
                status: NotFound
            }),
        Err(err) => Err(AppError {
                message: None,
                cause: Some(err.to_string()),
                status: DBError
            })
    }

}
