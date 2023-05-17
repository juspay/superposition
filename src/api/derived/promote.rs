// TODO :: Handle errors with appropriate error message
use actix_web::{
    Either::Left,
    post,
    web::{Data, Json},
};
use serde_json::{to_value, Error, Value};
use serde::{Serialize, Deserialize};

use crate::api::primary::{
    context_overrides::{ContextOverrideResponse, add_ctx_override, delete_ctx_override_helper, fetch_override_from_ctx_id},
    new_contexts::{add_new_context_v2, delete_new_context_helper, process_input_context_json}
};
use crate::AppState;
use crate::utils::{
    errors::{
        AppError,
        AppErrorType::{
            SomethingWentWrong
        }
    },
    hash::string_based_b64_hash
};

fn default_parsing_error(err: Error) -> AppError {
    AppError {
        message: None,
        cause: Some(Left(err.to_string())),
        status: SomethingWentWrong
    }
}

#[derive(Deserialize, Serialize, Clone)]
pub struct KeyValue {
    context_value: Value,
    dimension_to_be_dropped: String,
}

#[post("")]
pub async fn promote_contexts_overrides(input_state: Data<AppState>, body: Json<KeyValue>) -> Result<Json<ContextOverrideResponse>, AppError> {
    let state = &input_state;

    let input_context = body.context_value.clone();
    let dimension_to_be_dropped = body.dimension_to_be_dropped.clone();

    let get_processed_context_and_hash_values = |keys_to_be_excluded: Option<&Vec<String>>| -> Result<(String, Value), AppError>{
        let (processed_context_value, _) =
            process_input_context_json(&input_context, keys_to_be_excluded)?;
        let context_value =
            to_value(processed_context_value).map_err(default_parsing_error)?;
        let hashed_value =
            string_based_b64_hash(context_value.to_string()).to_string();

        Ok((hashed_value, context_value))
    };

    let (existing_hashed_value, _) = get_processed_context_and_hash_values(None)?;
    let (_, new_context_value) = get_processed_context_and_hash_values(Some(&Vec::from([dimension_to_be_dropped])))?;

    // Add transaction wrapper here
    let override_id = fetch_override_from_ctx_id(&state, &existing_hashed_value).await?;
    let context_response = add_new_context_v2(&state, new_context_value, true).await?;

    delete_new_context_helper(&state, existing_hashed_value.clone()).await?;
    delete_ctx_override_helper(&state, existing_hashed_value).await?;
    add_ctx_override(&state, context_response.id, override_id, true).await

}