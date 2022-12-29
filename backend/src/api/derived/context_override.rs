// TODO :: Handle errors with appropriate error message
use std::collections::HashMap;

use actix_web::{
    Either::Left,
    post,
    web::{Data, Json},
};

use log::info;
use serde::{Serialize, Deserialize};
use serde_json::{to_value, Value};

use crate::utils::errors::{
    AppError,
    AppErrorType::{
        SomethingWentWrong,
    }
};

use crate::api::primary::{
    overrides::add_new_override,
    contexts::add_new_context,
    context_overrides::add_ctx_override,
};


use crate::{AppState};

#[derive(Deserialize, Serialize)]
pub struct KeyValue {
    context_value: Value,
    override_value: Value,
}

#[post("")]
pub async fn add_new_context_override(state: Data<AppState>, body: Json<KeyValue>) -> Result<Json<Value>, AppError> {
    let override_value_from_body = body.override_value.clone();
    let context_value_from_body = body.context_value.clone();


    info!("Prithiv {:?}", context_value_from_body);

    // ! Create transaction
    // ! TODO:: Fix this asap

    // Ignore even there is an existing context
    let context_id = add_new_context(&state, context_value_from_body).await?;

    // Ignore even there is an existing override
    let override_id = add_new_override(&state, override_value_from_body.to_owned()).await?;

    add_ctx_override(&state, context_id.clone().id, override_id.clone().id).await?;

    Ok(Json(
        to_value(
            HashMap::from([
                ("context_id", context_id.id),
                ("override_id", override_id.id)
            ])
        ).map_err(|err| AppError {
            message: None,
            cause: Some(Left(err.to_string())),
            status: SomethingWentWrong
        })?
    ))
}