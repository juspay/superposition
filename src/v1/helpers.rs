use std::{env::VarError, fmt, str::FromStr};

use actix_web::{error::ErrorInternalServerError, Error};
use jsonschema::{Draft, JSONSchema};
use serde_json::json;

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
            println!("{name} env not found with error: {e}");
            return e;
        })
}

pub trait ToActixErr<T> {
    fn map_err_to_internal_server<B>(self, log_prefix: &str, err_body: B) -> Result<T, Error>
    where
        B: fmt::Debug + fmt::Display + 'static;
}

impl<T, E> ToActixErr<T> for Result<T, E>
where
    E: fmt::Debug,
{
    fn map_err_to_internal_server<B>(self, log_prefix: &str, err_body: B) -> Result<T, Error>
    where
        B: fmt::Debug + fmt::Display + 'static,
    {
        self.map_err(|e| {
            println!("{log_prefix}, err: {e:?}");
            ErrorInternalServerError(err_body)
        })
    }
}

pub fn get_default_config_validation_schema() -> JSONSchema {
    let my_schema = json!({
    "type": "object",
    "properties": {
        "type": {
        "enum": ["string", "object", "enum", "number", "boolean", "array"]
        }
    },
    "required": ["type"],
    "allOf": [
//        {
//        "if": {
//            "properties": { "type": { "const": "object" } }
//        },
//        "then": {
//            "properties": { "properties": { "type": "object" } },
//            "required": ["properties"]
//        }
//        },
        {
        "if": {
            "properties": { "type": { "const": "string" } }
        },
        "then": {
            "properties": { "pattern": { "type": "string" } },
            "required": ["pattern"]
        }
        },
        // TODO: Add validations for Array types.
    ]
    });

    JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&my_schema)
        .expect("THE IMPOSSIBLE HAPPENED, failed to compile the schema for the schema!")
}
