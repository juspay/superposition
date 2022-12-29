// TODO :: Handle errors with appropriate error message

use std::collections::HashMap;

use actix_web::{
    Either::Left,
    get,
    web::{Data, Json},
    HttpRequest,
};
use serde_json::{to_value, Error, Value};

use crate::api::primary::{
    context_overrides::fetch_override_from_ctx_id, global_config::get_complete_config,
    overrides::get_override_helper,
};

use crate::utils::{
    errors::{
        AppError,
        AppErrorType::{
            DBError,
            SomethingWentWrong,
        }
    },
    hash::string_based_b64_hash,
    helpers::create_all_unique_subsets,
};

use crate::AppState;

fn default_error(err: Error) -> AppError{
    AppError {
        message: None,
        cause: Some(Left(err.to_string())),
        status: SomethingWentWrong
    }
}

fn split_stringified_key_value_pair(input: &str) -> Vec<Vec<&str>> {
    let conditions_vector_splits: Vec<&str> = input.split("&").collect();
    let mut conditions_vector: Vec<Vec<&str>> = conditions_vector_splits
        .iter()
        .map(|&x| x.split("=").collect())
        .collect();

    conditions_vector.sort_by(|a, b| a[0].cmp(&b[0]));

    return conditions_vector;
}

fn format_context_json(input: &str, override_with_keys: &str) -> Result<Value, AppError> {
    let conditions_vector = split_stringified_key_value_pair(input);

    let mut formatted_conditions_vector = Vec::new();

    for condition in conditions_vector {
        let var_map = to_value(HashMap::from([("var", condition[0])])).map_err(default_error)?;
        let value_as_value = to_value(condition[1]).map_err(default_error)?;

        let value_arr = vec![[var_map, value_as_value]];

        // Add range based queries
        let condition_map = to_value(HashMap::from([("==", value_arr)])).map_err(default_error)?;

        formatted_conditions_vector.push(condition_map);
    }

    let condition_value = if formatted_conditions_vector.len() == 1 {
        formatted_conditions_vector[0].to_owned()
    } else {
        to_value(HashMap::from([("and", formatted_conditions_vector)])).map_err(default_error)?
    };

    Ok(to_value(HashMap::from([
        ("overrideWithKeys", &to_value(override_with_keys).map_err(default_error)?),
        ("condition", &condition_value),
    ]))
    .map_err(default_error)?
    )
}

async fn get_context_overrides_object(state: &Data<AppState>, query_string: &str) -> Result<Value, AppError> {
    if query_string == "" {
        return Ok(Value::default());
    }

/************************************************************************************************************/
    // ! Optimize this section
    let conditions_vector_temp: Vec<String> =
        split_stringified_key_value_pair(&query_string)
        .iter()
        .map(|x| x.join("="))
        .collect();

    let conditions_vector: Vec<&str> =
        conditions_vector_temp
        .iter()
        .map(|s| &**s)
        .collect();

/************************************************************************************************************/

    let keys = create_all_unique_subsets(&conditions_vector);

    let mut override_map = HashMap::new();
    let mut contexts = Vec::new();

    for item in keys {
        // TODO :: Sort query based on key and fetch from DB
        // Add the same logic while posting new context

        let key_string = item.to_owned().join("&");
        let hashed_key = string_based_b64_hash(&key_string).to_string();

        let override_id = fetch_override_from_ctx_id(&state, &hashed_key).await?;
        let fetched_override_value = get_override_helper(&state, override_id.to_owned()).await?;

        override_map.insert(override_id.to_owned(), fetched_override_value);
        contexts.push(format_context_json(&key_string, &override_id)?);
    }

    to_value(HashMap::from([
        ("context", to_value(contexts).map_err(default_error)?),
        ("overrides", to_value(override_map).map_err(default_error)?),
    ]))
    .map_err(|err| AppError {
        message: None,
        cause: Some(Left(err.to_string())),
        status: DBError,
    })
}




#[get("")]
pub async fn get_config(state: Data<AppState>, req: HttpRequest) -> Result<Json<Value>, AppError> {
    let query_string = req.query_string();

    let global_config = to_value(get_complete_config(&state).await?).map_err(|err| AppError {
        message: None,
        cause: Some(Left(err.to_string())),
        status: DBError,
    })?;

    let context_overrides = get_context_overrides_object(&state, query_string).await?;

    Ok(Json(to_value(HashMap::from([
        ("global_config", global_config),
        ("context_overrides", context_overrides)
    ])).map_err(|err| AppError {
        message: None,
        cause: Some(Left(err.to_string())),
        status: DBError,
    })?))
}
