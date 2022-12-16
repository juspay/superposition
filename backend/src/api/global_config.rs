use actix_web:: {
    get,
    post,
    error::ResponseError,
    web::Path,
    web::Json, 
    HttpResponse, 
    http::{header::ContentType, StatusCode}
};
use serde_json::{Value, json};
use crate::model::global_config::GlobalConfig;
use strum_macros::{EnumString, Display};
use serde::{Serialize, Deserialize};

// Error codes and their implementation 
#[derive(Debug, Display, EnumString)]
pub enum GlobalConfigError {
    KeyNotFound,
    BadRequest,
    FailedToAddToConfig,
    SomethingWentWrong,
    FailedToGetConfig,
}


impl ResponseError for GlobalConfigError {
    fn error_response(&self) -> HttpResponse<actix_web::body::BoxBody> {
        HttpResponse::build(self.status_code())
        .insert_header(ContentType::json())
        .body(self.to_string())
    }

    fn status_code(&self) -> StatusCode {
        match self {
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
pub async fn get_config() -> Result<Json<Vec<GlobalConfig>>, GlobalConfigError> {
    return Err(GlobalConfigError::FailedToGetConfig)

}

// Get request to fetch value for given key
#[derive(Deserialize, Serialize)]
pub struct Key {
    key: String,
}

#[get("/{key}")]
pub async fn get_key(params: Path<Key>) -> Result<Json<GlobalConfig>, GlobalConfigError> {
    return Ok(Json(GlobalConfig::new("somthing".to_string(), json!(params.key))));    
}


// Post request to add key, value
#[derive(Deserialize, Serialize)]
pub struct KeyValue {
    key: String,
    value: Value,
}

#[post("")]
pub async fn post_config_key_value(_body: Json<KeyValue>) -> Result<Json<GlobalConfig>, GlobalConfigError> {
    return Err(GlobalConfigError::FailedToAddToConfig);

}


