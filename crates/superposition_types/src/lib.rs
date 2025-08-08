#![deny(unused_crate_dependencies)]

uniffi::setup_scaffolding!();

#[cfg(feature = "api")]
pub mod api;
mod config;
mod contextual;
pub mod custom_query;
pub mod database;
pub mod logic;
mod overridden;
#[cfg(feature = "result")]
pub mod result;

use std::fmt::Display;
#[cfg(feature = "server")]
use std::future::{ready, Ready};

#[cfg(feature = "server")]
use actix_web::{dev::Payload, FromRequest, HttpMessage, HttpRequest};
#[cfg(feature = "diesel_derives")]
use diesel::r2d2::{ConnectionManager, PooledConnection};
#[cfg(feature = "diesel_derives")]
use diesel::PgConnection;
#[cfg(feature = "diesel_derives")]
use diesel_derive_enum as _;
use regex::Regex;
use serde::{Deserialize, Serialize};

pub use config::{Condition, Config, Context, OverrideWithKeys, Overrides};
pub use contextual::Contextual;
pub use logic::{apply, partial_apply};
pub use overridden::Overridden;

pub trait IsEmpty {
    fn is_empty(&self) -> bool;
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub email: String,
    pub username: String,
}

impl User {
    pub fn get_email(&self) -> String {
        self.email.clone()
    }

    pub fn get_username(&self) -> String {
        self.username.clone()
    }
}

impl Default for User {
    fn default() -> Self {
        Self {
            email: "user@superposition.io".into(),
            username: "superposition".into(),
        }
    }
}

#[cfg(feature = "server")]
impl FromRequest for User {
    type Error = actix_web::error::Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(req: &HttpRequest, _: &mut Payload) -> Self::Future {
        if let Some(user) = req.extensions().get::<Self>() {
            ready(Ok(user.to_owned()))
        } else {
            log::error!("No user was found while validating token");
            ready(Err(actix_web::error::ErrorUnauthorized(
                serde_json::json!({"message":"invalid token provided"}),
            )))
        }
    }
}

#[derive(Clone, Debug, PartialEq, Copy, Serialize)]
pub struct Cac<T>(T);
impl<T> Cac<T> {
    pub fn into_inner(self) -> T {
        self.0
    }
}

#[derive(Clone, Debug, PartialEq, Copy, Serialize)]
pub struct Exp<T>(T);
impl<T> Exp<T> {
    pub fn into_inner(self) -> T {
        self.0
    }
}

const ALPHANUMERIC_WITH_DOT: &str =
    "^[a-zA-Z0-9-_]([a-zA-Z0-9-_.]{0,254}[a-zA-Z0-9-_])?$";
const ALPHANUMERIC_WITH_DOT_WORDS: &str =
    "It can contain the following characters only [a-zA-Z0-9-_.] \
                                    and it should not start or end with a '.' character.";

const ALPHANUMERIC_WITHOUT_DOT: &str = "^[a-zA-Z0-9-_]{1,64}$";
const ALPHANUMERIC_WITHOUT_DOT_WORDS: &str =
    "It can contain the following characters only [a-zA-Z0-9-_]";

pub enum RegexEnum {
    DefaultConfigKey,
    DimensionName,
    FunctionName,
    TypeTemplateName,
}

impl RegexEnum {
    pub fn match_regex(&self, val: &str) -> Result<(), String> {
        let regex_str = self.to_string();
        let regex = Regex::new(regex_str.as_str()).map_err(|err| {
            log::error!("error while validating with regex : {err}");
            "Something went wrong".to_string()
        })?;

        if !regex.is_match(val) {
            Err(format!(
                "{val} is invalid, it should obey the regex {regex_str}. \
                {}",
                self.get_error_message()
            ))
        } else {
            Ok(())
        }
    }

    fn get_error_message(&self) -> String {
        match self {
            Self::DefaultConfigKey => ALPHANUMERIC_WITH_DOT_WORDS,
            Self::DimensionName => ALPHANUMERIC_WITH_DOT_WORDS,
            Self::FunctionName => ALPHANUMERIC_WITHOUT_DOT_WORDS,
            Self::TypeTemplateName => ALPHANUMERIC_WITHOUT_DOT_WORDS,
        }
        .to_string()
    }
}

impl Display for RegexEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let regex = match self {
            Self::DefaultConfigKey => ALPHANUMERIC_WITH_DOT,
            Self::DimensionName => ALPHANUMERIC_WITH_DOT,
            Self::FunctionName => ALPHANUMERIC_WITHOUT_DOT,
            Self::TypeTemplateName => ALPHANUMERIC_WITHOUT_DOT,
        }
        .to_string();
        write!(f, "{regex}")
    }
}

#[derive(Serialize, Debug, Clone, Deserialize)]
pub struct PaginatedResponse<T> {
    pub total_pages: i64,
    pub total_items: i64,
    pub data: Vec<T>,
}

impl<T> Default for PaginatedResponse<T> {
    fn default() -> Self {
        Self {
            total_pages: 0,
            total_items: 0,
            data: Vec::new(),
        }
    }
}

impl<T> PaginatedResponse<T> {
    pub fn all(data: Vec<T>) -> Self {
        Self {
            total_pages: 1,
            total_items: data.len() as i64,
            data,
        }
    }
}

#[derive(Serialize, Clone, Deserialize)]
pub struct ListResponse<T> {
    pub data: Vec<T>,
}

impl<T> ListResponse<T> {
    pub fn new(response: Vec<T>) -> Self {
        Self { data: response }
    }
}

#[derive(
    Debug,
    Serialize,
    Deserialize,
    Clone,
    PartialEq,
    PartialOrd,
    strum_macros::Display,
    strum_macros::EnumIter,
)]
#[serde(rename_all = "lowercase")]
#[strum(serialize_all = "lowercase")]
pub enum SortBy {
    Desc,
    Asc,
}

impl SortBy {
    pub fn flip(&self) -> Self {
        match self {
            Self::Desc => Self::Asc,
            Self::Asc => Self::Desc,
        }
    }

    pub fn label(&self) -> String {
        match self {
            Self::Desc => "Descending".to_string(),
            Self::Asc => "Ascending".to_string(),
        }
    }
}

impl Default for SortBy {
    fn default() -> Self {
        Self::Asc
    }
}

#[cfg(feature = "diesel_derives")]
pub type DBConnection = PooledConnection<ConnectionManager<PgConnection>>;

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{json, Map, Value};

    #[test]
    fn ok_test_deserialize_condition() {
        let db_request_condition_map: Map<String, Value> = Map::from_iter(vec![(
            "and".to_string(),
            json!([
                {
                "==": [
                    {
                        "var": "clientId"
                    },
                    "meesho"
                ]
                }
            ]),
        )]);

        let default_request_condition_map: Map<String, Value> = Map::from_iter(vec![(
            "and".to_string(),
            json!([
                {
                "==": [
                    {
                        "var": "os"
                    },
                    "ios"
                ]
                }
            ]),
        )]);

        let exp_request_condition_map: Map<String, Value> = Map::from_iter(vec![(
            "and".to_string(),
            json!([
                {
                "==": [
                    {
                        "var": "clientId"
                    },
                    "meesho"
                ]
                }
            ]),
        )]);

        let db_condition = serde_json::from_value::<Condition>(Value::Object(
            db_request_condition_map.clone(),
        ))
        .unwrap();
        let db_expected_condition =
            Cac::<Condition>::validate_db_data(db_request_condition_map)
                .map(|a| a.into_inner());
        assert_eq!(Ok(db_condition), db_expected_condition);

        let default_condition = serde_json::from_str::<Condition>(
            &json!(default_request_condition_map).to_string(),
        )
        .unwrap();
        let default_expected_condition =
            Cac::<Condition>::try_from(default_request_condition_map)
                .map(|a| a.into_inner());
        assert_eq!(Ok(default_condition), default_expected_condition);

        let exp_condition = serde_json::from_str::<Condition>(
            &json!(exp_request_condition_map).to_string(),
        )
        .unwrap();
        let exp_expected_condition =
            Exp::<Condition>::try_from(exp_request_condition_map).map(|a| a.into_inner());
        assert_eq!(Ok(exp_condition), exp_expected_condition);
    }

    #[test]
    fn fail_test_deserialize_condition() {
        #[cfg(feature = "jsonlogic")]
        let request_condition_map: Map<String, Value> = Map::from_iter(vec![(
            "and".to_string(),
            json!([
                {
                ".": [
                    {
                        "var": "clientId"
                    },
                    "meesho"
                ]
                }
            ]),
        )]);

        #[cfg(feature = "jsonlogic")]
        let exp_condition_map: Map<String, Value> = Map::from_iter(vec![(
            "and".to_string(),
            json!([
                {
                "in": [
                    "variant-id",
                    {
                        "var": "variantIds"
                    }
                ]
                }
            ]),
        )]);

        #[cfg(not(feature = "jsonlogic"))]
        let exp_condition_map: Map<String, Value> =
            Map::from_iter(vec![("variantIds".to_string(), json!("variant-id"))]);

        #[cfg(feature = "jsonlogic")]
        let fail_condition = serde_json::from_str::<Cac<Condition>>(
            &json!(request_condition_map).to_string(),
        )
        .map_err(|_| "Invalid operation".to_owned());

        let fail_exp_condition = Exp::<Condition>::try_from(exp_condition_map.clone())
            .map(|a| a.into_inner())
            .map_err(|_| "variantIds should not be present".to_owned());

        #[cfg(feature = "jsonlogic")]
        assert!(json!(fail_condition)
            .to_string()
            .contains("Invalid operation"));

        assert!(json!(fail_exp_condition)
            .to_string()
            .contains("variantIds should not be present"));

        let db_expected_condition = Exp::<Condition>::validate_db_data(exp_condition_map)
            .map(|_| true)
            .map_err(|_| "variantIds should not be present".to_string());

        assert!(json!(db_expected_condition)
            .to_string()
            .contains("variantIds should not be present"));
    }

    #[test]
    fn test_deserialize_override() {
        let override_map = Map::from_iter(vec![
            ("key1".to_string(), json!("val1")),
            ("key2".to_string(), json!(5)),
        ]);

        let empty_override_map = Map::new();

        let deserialize_overrides =
            serde_json::from_value::<Overrides>(Value::Object(override_map.clone()))
                .unwrap();
        let db_expected_overrides =
            Cac::<Overrides>::validate_db_data(override_map.clone())
                .map(|a| a.into_inner());
        assert_eq!(Ok(deserialize_overrides.clone()), db_expected_overrides);

        let exp_expected_overrides =
            Exp::<Overrides>::try_from(override_map.clone()).map(|a| a.into_inner());
        assert_eq!(Ok(deserialize_overrides.clone()), exp_expected_overrides);

        let default_expected_overrides =
            Cac::<Overrides>::try_from(override_map.clone()).map(|a| a.into_inner());
        assert_eq!(Ok(deserialize_overrides), default_expected_overrides);

        let empty_overrides = serde_json::from_str::<Cac<Overrides>>(
            &json!(empty_override_map.clone()).to_string(),
        )
        .map_err(|_| "override should not be empty".to_string());

        assert!(json!(empty_overrides)
            .to_string()
            .contains("override should not be empty"));
    }
}
