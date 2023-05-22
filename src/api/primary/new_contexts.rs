use std::{cmp::Ordering, collections::HashMap};

use actix::Addr;
use actix_web::{
    delete, get, post,
    web::{Data, Json, Path},
    Either::Left,
    HttpRequest,
};
use serde::Serialize;
use serde_json::{from_value, to_value, Error, Value};

use crate::{
    api::primary::dimensions::fetch_dimensions,
    db::messages::new_contexts::{
        CreateNewContext, DeleteNewContext, FetchAllNewContexts, FetchNewContext,
    },
    AppState, DbActor,
};

use crate::db::models::db_models::NewContexts;

use crate::utils::{
    errors::{
        AppError,
        AppErrorType::{DBError, DataExists, NotFound, SomethingWentWrong},
    },
    hash::string_based_b64_hash,
    helpers::{split_stringified_key_value_pair, strip_double_quotes},
};

#[derive(Serialize, Clone)]
pub struct ContextIdResponse {
    pub id: String,
}

fn default_parsing_error(err: Error) -> AppError {
    AppError {
        message: None,
        cause: Some(Left(err.to_string())),
        status: SomethingWentWrong,
    }
}

fn get_dimension_name(idx: usize) -> String {
    let dimesions = Vec::from(["tier", "merchantId", "os", "country"]);
    dimesions[idx].to_string()
}

type ContextValueAndDimensionMap = (HashMap<String, Value>, HashMap<String, String>);
type MaybeContextValueAndDimensionMap = (Option<HashMap<String, Value>>, HashMap<String, String>);

fn contexts_comparator(
    priority_map: HashMap<String, i32>,
) -> impl Fn(&NewContexts, &NewContexts) -> Ordering {
    let get_value_from_map = move |key: Option<String>| {
        if let Some(val) = key {
            if let Some(val1) = priority_map.get(&val) {
                return val1.to_owned();
            }
        }

        return 0;
    };

    move |ctx1: &NewContexts, ctx2: &NewContexts| {
        let val1 = get_value_from_map(ctx1.column1.to_owned().map(|_| get_dimension_name(0)))
            + get_value_from_map(ctx1.column2.to_owned().map(|_| get_dimension_name(1)))
            + get_value_from_map(ctx1.column3.to_owned().map(|_| get_dimension_name(2)))
            + get_value_from_map(ctx1.column4.to_owned().map(|_| get_dimension_name(3)));

        let val2 = get_value_from_map(ctx2.column1.to_owned().map(|_| get_dimension_name(0)))
            + get_value_from_map(ctx2.column2.to_owned().map(|_| get_dimension_name(1)))
            + get_value_from_map(ctx2.column3.to_owned().map(|_| get_dimension_name(2)))
            + get_value_from_map(ctx2.column4.to_owned().map(|_| get_dimension_name(3)));

        val1.cmp(&val2)
    }
}

pub async fn fetch_raw_context_v2(
    state: &Data<AppState>,
    column1: Option<&String>,
    column2: Option<&String>,
    column3: Option<&String>,
    column4: Option<&String>,
    dimesion_map: HashMap<String, i32>,
) -> Result<Vec<NewContexts>, AppError> {
    let db: Addr<DbActor> = state.db.clone();

    let result: Vec<NewContexts> = match db
        .send(FetchNewContext {
            column1: column1.map(|x| x.to_string()),
            column2: column2.map(|x| x.to_string()),
            column3: column3.map(|x| x.to_string()),
            column4: column4.map(|x| x.to_string()),
        })
        .await
    {
        Ok(Ok(res)) => Ok(res),
        Ok(Err(err)) => Err(AppError {
            message: Some("Failed to get context".to_string()),
            cause: Some(Left(err.to_string())),
            status: NotFound,
        }),
        Err(err) => Err(AppError {
            message: None,
            cause: Some(Left(err.to_string())),
            status: DBError,
        }),
    }?;

    let mut final_result: Vec<NewContexts> = result
        .into_iter()
        .filter(|y| {
            let x = y.to_owned();
            let column1_from_row: Option<String> = x.column1.to_owned();
            let column2_from_row: Option<String> = x.column2.to_owned();
            let column3_from_row: Option<String> = x.column3.to_owned();
            let column4_from_row: Option<String> = x.column4.to_owned();

            (column1_from_row.is_none()
                || column1.is_none()
                || column1_from_row == column1.map(|x| x.to_string()))
                && (column2_from_row.is_none()
                    || column2.is_none()
                    || column2_from_row == column2.map(|x| x.to_string()))
                && (column3_from_row.is_none()
                    || column3.is_none()
                    || column3_from_row == column3.map(|x| x.to_string()))
                && (column4_from_row.is_none()
                    || column4.is_none()
                    || column4_from_row == column4.map(|x| x.to_string()))
        })
        .collect();

    final_result.sort_by(contexts_comparator(dimesion_map));

    Ok(final_result)
}

pub fn process_single_condition(
    value_object: &HashMap<String, Value>,
    keys_to_be_excluded: Option<&Vec<String>>,
) -> MaybeContextValueAndDimensionMap {
    let mut result_map: HashMap<String, String> = HashMap::new();
    let mut result_context: Option<HashMap<String, Value>> = None;

    // Range check have to be implemented
    if value_object.contains_key("==") {
        let variable_array_value = value_object.get("==").unwrap();
        let variable_array: Vec<Value> = from_value(variable_array_value.to_owned()).unwrap();

        if variable_array.len() >= 2 {
            let variable_map: HashMap<String, Value> =
                from_value(variable_array[0].to_owned()).unwrap();
            let mapped_value = variable_array[1].to_string();

            if let Some(key) = variable_map.get("var") {
                let key_string = strip_double_quotes(&key.to_string()).to_string();
                let to_be_included =
                    keys_to_be_excluded.map_or(true, |vect| !vect.contains(&key_string));

                if to_be_included {
                    result_map.insert(
                        // Removing double quotes at the start and end for both key and value
                        strip_double_quotes(&key_string).to_owned(),
                        strip_double_quotes(&mapped_value).to_owned(),
                    );

                    result_context = Some(value_object.to_owned());
                }
            }
        }
    }

    (result_context, result_map)
}

pub fn process_input_context_json(
    input_json: &Value,
    keys_to_be_excluded: Option<&Vec<String>>,
) -> Result<ContextValueAndDimensionMap, AppError> {
    let input_as_map: HashMap<String, Value> =
        from_value(input_json.to_owned()).map_err(default_parsing_error)?;
    let mut result_context_array = Vec::new();

    // Single dimension or condition
    if !input_as_map.contains_key("and") {
        // ! TODO :: Fix this asap
        return Err(AppError {
            message: None,
            cause: Some(Left("Case yet to handled properly".to_string())),
            status: SomethingWentWrong,
        });
        // return Ok(process_single_condition(&input_as_map, keys_to_be_excluded));
    }

    let mut result_map = HashMap::new();
    let val = input_as_map.get("and").unwrap();
    let multiple_condition_array: Vec<Value> = from_value(val.to_owned()).unwrap();

    for item in multiple_condition_array {
        let value_object: HashMap<String, Value> =
            from_value(item).map_err(default_parsing_error)?;
        let (maybe_ctx, map) = process_single_condition(&value_object, keys_to_be_excluded);

        if let Some(ctx) = maybe_ctx {
            result_context_array.push(ctx);
            result_map.extend(map);
        }
    }

    Ok((
        HashMap::from([(
            "and".to_string(),
            to_value(result_context_array).map_err(default_parsing_error)?,
        )]),
        result_map,
    ))
}

pub async fn add_new_context_v2(
    state: &Data<AppState>,
    raw_context_value: Value,
    return_if_present: bool,
) -> Result<ContextIdResponse, AppError> {
    let db: Addr<DbActor> = state.db.clone();

    let (processed_context_value, key_value_map) =
        process_input_context_json(&raw_context_value, None)?;
    let context_value = to_value(processed_context_value).map_err(default_parsing_error)?;

    // ? TODO :: Post as an array of value
    // ? TODO :: Sort query based on key and add to DB
    // let ss = Json(&context_value);
    // let hashed_value = "test12".to_string();

    let hashed_value = string_based_b64_hash(context_value.to_string()).to_string();

    match db
        .send(CreateNewContext {
            key: hashed_value.to_owned(),
            value: context_value,
            column1: key_value_map
                .get(&get_dimension_name(0))
                .map(|x| x.to_string()),
            column2: key_value_map
                .get(&get_dimension_name(1))
                .map(|x| x.to_string()),
            column3: key_value_map
                .get(&get_dimension_name(2))
                .map(|x| x.to_string()),
            column4: key_value_map
                .get(&get_dimension_name(3))
                .map(|x| x.to_string()),
        })
        .await
    {
        Ok(Ok(result)) => Ok(ContextIdResponse { id: result.key }),
        Ok(Err(err)) => {
            if return_if_present {
                Ok(ContextIdResponse { id: hashed_value })
            } else {
                Err(AppError {
                    message: Some("Failed to add context".to_string()),
                    cause: Some(Left(err.to_string())),
                    status: DataExists,
                })
            }
        }
        Err(err) => Err(AppError {
            message: None,
            cause: Some(Left(err.to_string())),
            status: DBError,
        }),
    }
}

pub async fn fetch_all_new_contexts(state: &Data<AppState>) -> Result<Vec<NewContexts>, AppError> {
    let db: Addr<DbActor> = state.db.clone();

    match db.send(FetchAllNewContexts).await {
        Ok(Ok(res)) => Ok(res),
        Ok(Err(err)) => Err(AppError {
            message: Some("Failed to get context".to_string()),
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

pub async fn delete_new_context_helper(
    state: &Data<AppState>,
    key: String,
) -> Result<NewContexts, AppError> {
    let db: Addr<DbActor> = state.db.clone();

    match db.send(DeleteNewContext { key }).await {
        Ok(Ok(res)) => Ok(res),
        Ok(Err(err)) => Err(AppError {
            message: Some("Failed to delete context".to_string()),
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

pub async fn fetch_new_contexts(
    state: &Data<AppState>,
    query_string: String,
) -> Result<Vec<HashMap<&str, Value>>, AppError> {
    let key_value_pairs = split_stringified_key_value_pair(&query_string);

    let all_dimesions = fetch_dimensions(&state).await?;

    let mut dimesion_map = HashMap::new();

    for item in all_dimesions {
        dimesion_map.insert(item.dimension, item.priority);
    }

    let mut filter_input_keys_map = HashMap::new();

    for item in key_value_pairs {
        if dimesion_map.contains_key(item[0]) {
            filter_input_keys_map.insert(item[0].to_string(), item[1].to_string());
        }
    }

    let raw_contexts = fetch_raw_context_v2(
        &state,
        filter_input_keys_map.get(&get_dimension_name(0)),
        filter_input_keys_map.get(&get_dimension_name(1)),
        filter_input_keys_map.get(&get_dimension_name(2)),
        filter_input_keys_map.get(&get_dimension_name(3)),
        dimesion_map,
    )
    .await?;

    let mut formatted_contexts = Vec::new();
    for item in raw_contexts {
        formatted_contexts.push(HashMap::from([
            ("condition", item.value),
            ("id", to_value(item.key).map_err(default_parsing_error)?),
        ]));
    }

    Ok(formatted_contexts)
}

#[get("")]
pub async fn get_new_context(
    state: Data<AppState>,
    req: HttpRequest,
) -> Result<Json<Value>, AppError> {
    let query_string = req.query_string();
    let formatted_contexts = fetch_new_contexts(&state, query_string.to_string()).await?;
    let mut result = Vec::new();

    for item in &formatted_contexts {
        result.push(HashMap::from([(
            "condition",
            item.get("condition").to_owned(),
        )]));
    }

    Ok(Json(to_value(result).map_err(default_parsing_error)?))
}

#[post("")]
pub async fn post_new_context(
    state: Data<AppState>,
    body: Json<Value>,
) -> Result<Json<ContextIdResponse>, AppError> {
    let context_value = body.clone();
    Ok(Json(
        add_new_context_v2(&state, context_value, false).await?,
    ))
}

#[delete("/{id}")]
pub async fn delete_new_context(
    state: Data<AppState>,
    id: Path<String>,
) -> Result<Json<NewContexts>, AppError> {
    Ok(Json(
        delete_new_context_helper(&state, id.to_string()).await?,
    ))
}
