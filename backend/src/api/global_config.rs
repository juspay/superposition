use actix_web:: {
    get,
    post,
    error::ResponseError,
    web::Path,
    web::Json,
    web::Data,
    HttpResponse, 
    http::{header::ContentType, StatusCode}
};
use serde_json::Value;
use crate::models::db_models::GlobalConfig;
use strum_macros::{EnumString, Display};
use serde::{Serialize, Deserialize};
use crate::{
    messages::global_config::{
        FetchGlobalConfig, 
        FetchConfigKey,
        CreateGlobalKey,
    }, AppState, DbActor};
use actix::Addr;

// Error codes and their implementation 
#[derive(Debug, Display, EnumString)]
pub enum GlobalConfigError {
    KeyNotFound,
    BadRequest,
    FailedToAddToConfig,
    SomethingWentWrong,
    FailedToGetConfig,
    ConfigNotFound,
}


impl ResponseError for GlobalConfigError {
    fn error_response(&self) -> HttpResponse<actix_web::body::BoxBody> {
        HttpResponse::build(self.status_code())
        .insert_header(ContentType::json())
        .body(self.to_string())
    }

    fn status_code(&self) -> StatusCode {
        match self {
            GlobalConfigError::ConfigNotFound => StatusCode::NOT_FOUND,
            GlobalConfigError::KeyNotFound => StatusCode::NOT_FOUND,
            GlobalConfigError::FailedToGetConfig => StatusCode::INTERNAL_SERVER_ERROR,
            GlobalConfigError::SomethingWentWrong => StatusCode::FAILED_DEPENDENCY,
            GlobalConfigError::BadRequest => StatusCode::BAD_REQUEST,
            GlobalConfigError::FailedToAddToConfig => StatusCode::FAILED_DEPENDENCY
        }
    }
}

// Get whole global config 
#[get("")]
pub async fn get_config(state: Data<AppState>) -> Result<Json<Vec<GlobalConfig>>, GlobalConfigError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();

    match db.send(FetchGlobalConfig).await {
        Ok(Ok(info)) => Ok(Json(info)),
        Ok(Err(_)) => Err(GlobalConfigError::ConfigNotFound), 
        _ => Err(GlobalConfigError::SomethingWentWrong) 
    }

}

// Get request to fetch value for given key
#[derive(Deserialize, Serialize)]
pub struct Key {
    key: String,
}

#[get("/{key}")]
pub async fn get_global_config_key(state: Data<AppState>, params: Path<Key>) -> Result<Json<GlobalConfig>, GlobalConfigError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();
    let key = params.into_inner().key;

    match db.send(FetchConfigKey {key}).await {
        Ok(Ok(info)) => Ok(Json(info)),
        Ok(Err(_)) => Err(GlobalConfigError::KeyNotFound),
        _ => Err(GlobalConfigError::SomethingWentWrong)
    }
}


// Post request to add key, value
#[derive(Deserialize, Serialize)]
pub struct KeyValue {
    key: String,
    value: Value,
}

#[post("")]
pub async fn post_config_key_value(state: Data<AppState>, body: Json<KeyValue>) -> Result<Json<GlobalConfig>, GlobalConfigError> {
    let db: Addr<DbActor> = state.as_ref().db.clone();
    
    match db.send(CreateGlobalKey {
        key: body.key.clone(),
        value: body.value.clone()
    }).await {
        
        Ok(Ok(info)) => Ok(Json(info)),
        _ => Err(GlobalConfigError::FailedToAddToConfig)
    } 

}


