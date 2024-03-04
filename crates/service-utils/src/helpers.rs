use actix_web::{error::ErrorInternalServerError, http::StatusCode, Error};
use log::info;
use serde::de::{self, IntoDeserializer};
use std::{
    env::VarError,
    fmt::{self, Display},
    str::FromStr,
};

use super::errors::types::{Error as err, ErrorResponse};
use crate::types as app;
use serde_json::{Map, Value};

//WARN Do NOT use this fxn inside api requests, instead add the required
//env to AppState and get value from there. As this panics, it should
//only be used for envs needed during app start.
pub fn get_from_env_unsafe<F>(name: &str) -> Result<F, VarError>
where
    F: FromStr,
    <F as FromStr>::Err: std::fmt::Debug,
{
    std::env::var(name)
        .map(|val| val.parse().unwrap())
        .map_err(|e| {
            log::info!("{name} env not found with error: {e}");
            return e;
        })
}

pub fn get_from_env_or_default<F>(name: &str, default: F) -> F
where
    F: FromStr + Display,
    <F as FromStr>::Err: std::fmt::Debug,
{
    match std::env::var(name) {
        Ok(env) => env.parse().unwrap(),
        Err(err) => {
            info!(
                "{name} ENV failed to load due to {err}, using default value {default}"
            );
            default
        }
    }
}

pub trait ToActixErr<T> {
    fn map_err_to_internal_server<B>(
        self,
        log_prefix: &str,
        err_body: B,
    ) -> Result<T, Error>
    where
        B: fmt::Debug + fmt::Display + 'static;
}

impl<T, E> ToActixErr<T> for Result<T, E>
where
    E: fmt::Debug,
{
    fn map_err_to_internal_server<B>(
        self,
        log_prefix: &str,
        err_body: B,
    ) -> Result<T, Error>
    where
        B: fmt::Debug + fmt::Display + 'static,
    {
        self.map_err(|e| {
            log::info!("{log_prefix}, err: {e:?}");
            ErrorInternalServerError(err_body)
        })
    }
}

pub fn deserialize_stringified_list<'de, D, I>(
    deserializer: D,
) -> std::result::Result<Vec<I>, D::Error>
where
    D: de::Deserializer<'de>,
    I: de::DeserializeOwned,
{
    struct StringVecVisitor<I>(std::marker::PhantomData<I>);

    impl<'de, I> de::Visitor<'de> for StringVecVisitor<I>
    where
        I: de::DeserializeOwned,
    {
        type Value = Vec<I>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str(
                "a string containing comma separated values eg: CREATED,INPROGRESS",
            )
        }

        fn visit_str<E>(self, v: &str) -> std::result::Result<Self::Value, E>
        where
            E: de::Error,
        {
            let mut query_vector = Vec::new();
            for param in v.split(",") {
                let p: I = I::deserialize(param.into_deserializer())?;
                query_vector.push(p);
            }
            Ok(query_vector)
        }
    }

    deserializer.deserialize_any(StringVecVisitor(std::marker::PhantomData::<I>))
}

pub fn get_pod_info() -> (String, String) {
    let hostname: String = get_from_env_unsafe("HOSTNAME").expect("HOSTNAME is not set");
    let tokens = hostname
        .split("-")
        .map(str::to_string)
        .collect::<Vec<String>>();
    let mut tokens = tokens.iter().rev();
    let (pod_id, _replica_set, deployment_id) = (
        tokens.next().unwrap().to_owned(),
        tokens.next().unwrap().to_owned(),
        tokens.next().unwrap().to_owned(),
    );
    (pod_id, deployment_id)
}

pub fn remove_error_abstraction(e: reqwest::Error) -> err {
    match e.status() {
        Some(StatusCode::BAD_REQUEST) => err::BadRequest(ErrorResponse {
            message: e.to_string(),
            possible_fix:
                "Please try again with correct value. Schema type / Validation is failing"
                    .to_string(),
        }),
        _ => err::InternalServerErr(e.to_string()),
    }
}

pub fn extract_dimensions(context_json: &Value) -> app::Result<Map<String, Value>> {
    // Assuming max 2-level nesting in context json logic
    let context = context_json
        .as_object()
        .ok_or(err::BadArgument(ErrorResponse { message: "An error occurred while extracting dimensions: context not a valid JSON object".to_string(), possible_fix: "send a valid JSON context".to_string() }))?;

    let conditions = match context.get("and") {
        Some(conditions_json) => conditions_json
            .as_array()
            .ok_or(err::BadArgument(ErrorResponse { message: "An error occurred while extracting dimensions: failed parsing conditions as an array".to_string(), possible_fix: "ensure the context provided obeys the rules of JSON logic".to_string() }))?
            .clone(),
        None => vec![context_json.clone()],
    };

    let mut dimension_tuples = Vec::new();
    for condition in &conditions {
        let condition_obj =
            condition
                .as_object()
                .ok_or(err::BadArgument(ErrorResponse {
                    message: " failed to parse condition as an object".to_string(),
                    possible_fix:
                        "ensure the context provided obeys the rules of JSON logic"
                            .to_string(),
                }))?;
        let operators = condition_obj.keys();

        for operator in operators {
            let operands = condition_obj[operator].as_array().ok_or(err::BadArgument(
                ErrorResponse {
                    message: " failed to parse operands as an arrays".to_string(),
                    possible_fix:
                        "ensure the context provided obeys the rules of JSON logic"
                            .to_string(),
                },
            ))?;

            let (variable_name, variable_value) = get_variable_name_and_value(operands)?;

            dimension_tuples.push((String::from(variable_name), variable_value.clone()));
        }
    }

    Ok(Map::from_iter(dimension_tuples))
}

pub fn get_variable_name_and_value(operands: &Vec<Value>) -> app::Result<(&str, &Value)> {
    let (obj_pos, variable_obj) = operands
        .iter()
        .enumerate()
        .find(|(_, operand)| {
            operand.is_object() && operand.as_object().unwrap().get("var").is_some()
        })
        .ok_or(err::BadArgument(ErrorResponse {
            message: " failed to get variable name from operands list".to_string(),
            possible_fix: "ensure the context provided obeys the rules of JSON logic"
                .to_string(),
        }))?;

    let variable_name = variable_obj
        .as_object()
        .map_or(None, |obj| obj.get("var"))
        .map_or(None, |value| value.as_str())
        .ok_or(err::BadArgument(ErrorResponse {
            message: " failed to get variable name from operands list".to_string(),
            possible_fix: "ensure the context provided obeys the rules of JSON logic"
                .to_string(),
        }))?;

    let value_pos = (obj_pos + 1) % 2;
    let variable_value =
        operands
            .get(value_pos)
            .ok_or(err::BadArgument(ErrorResponse {
                message: " failed to get variable value from operands list".to_string(),
                possible_fix: "ensure the context provided obeys the rules of JSON logic"
                    .to_string(),
            }))?;

    Ok((variable_name, variable_value))
}
