// TODO :: Handle errors with appropriate error message

use std::collections::HashMap;

use actix_web::{
    error::ResponseError,
    get,
    http::{header::ContentType, StatusCode},
    web::{Data, Json},
    HttpRequest,
    HttpResponse,
};
use serde_json::{Value, to_value};
use strum_macros::{Display, EnumString};

use crate::api::primary::{
    global_config::get_complete_config,
    overrides::get_override_helper
};

use crate::utils::helpers::create_all_unique_subsets;

use log::{info};

use crate::{AppState};

#[derive(Debug, Display, EnumString)]
pub enum ConfigError {
    BadRequest,
    SomethingWentWrong,
    FailedToGetContextOverride {key: String},
    FailedToGetGlobalConfig,
}

impl ResponseError for ConfigError {
    fn error_response(&self) -> HttpResponse<actix_web::body::BoxBody> {
        HttpResponse::build(self.status_code())
            .insert_header(ContentType::json())
            .body(self.to_string())
    }

    fn status_code(&self) -> StatusCode {
        match self {
            ConfigError::BadRequest => StatusCode::BAD_REQUEST,

            ConfigError::SomethingWentWrong => StatusCode::FAILED_DEPENDENCY,

            ConfigError::FailedToGetContextOverride {..} => StatusCode::FAILED_DEPENDENCY,
            ConfigError::FailedToGetGlobalConfig => StatusCode::FAILED_DEPENDENCY,
        }
    }
}

async fn get_context_overrides_object(state: Data<AppState>, query_string: &str) -> Result<Value, ConfigError> {
    let conditions_vector: Vec<&str> = query_string.split("&").collect();

    info!("Input contexts =======> {:?}", conditions_vector);

    let keys = create_all_unique_subsets(&conditions_vector);

    let mut context_override_map = HashMap::new();

    for item in keys {
        // TODO :: Sort query based on key and fetch from DB
        // Add the same logic while posting new context

        let key = item.join("&");

        let fetched_value =
            get_override_helper(state.to_owned(), key.to_owned())
            .await
            .map_err(|_| ConfigError::FailedToGetContextOverride {key: key.to_owned()})?;

        context_override_map.insert(key, fetched_value);
    }

    to_value(context_override_map).map_err(|_| ConfigError::SomethingWentWrong)
}

#[get("/{key}")]
pub async fn get_config(state: Data<AppState>, req: HttpRequest) -> Result<Json<Value>, ConfigError> {

    let query_string = req.query_string();

    let global_config =
        to_value(
            get_complete_config(state.to_owned())
            .await
            .map_err(|_| ConfigError::FailedToGetGlobalConfig)?
        ).map_err(|_| ConfigError::SomethingWentWrong)?;

    let context_overrides = get_context_overrides_object(state.to_owned(), query_string).await?;

    let mut result = HashMap::new();
    result.insert("global_config", global_config);
    result.insert("context_overrides", context_overrides);

    Ok(Json(to_value(result).map_err(|_| ConfigError::SomethingWentWrong)?))
}