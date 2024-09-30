#![deny(unused_crate_dependencies)]
#[cfg(feature = "result")]
pub mod result;

use std::fmt::Display;
use std::future::{ready, Ready};

use actix_web::{dev::Payload, error, FromRequest, HttpMessage, HttpRequest};
use derive_more::{AsRef, Deref, DerefMut, Into};
use log::error;
use regex::Regex;
use serde::{de, Deserialize, Deserializer, Serialize};
use serde_json::{json, Map, Value};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub email: String,
    pub username: String,
    pub auth_token: String,
    pub auth_type: String,
}

impl User {
    pub fn get_email(&self) -> String {
        self.email.clone()
    }

    pub fn get_username(&self) -> String {
        self.username.clone()
    }

    pub fn get_auth_token(&self) -> String {
        self.auth_token.clone()
    }

    pub fn get_auth_type(&self) -> String {
        self.auth_type.clone()
    }
}

impl Default for User {
    fn default() -> Self {
        Self {
            email: "user@superposition.io".into(),
            username: "superposition".into(),
            auth_token: "1234abcd".into(),
            auth_type: "Bearer".into(),
        }
    }
}

impl FromRequest for User {
    type Error = actix_web::error::Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(req: &HttpRequest, _: &mut Payload) -> Self::Future {
        if let Some(user) = req.extensions().get::<Self>() {
            ready(Ok(user.to_owned()))
        } else {
            error!("No user was found while validating token");
            ready(Err(error::ErrorUnauthorized(
                json!({"message":"invalid token provided"}),
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

macro_rules! impl_try_from_map {
    ($wrapper:ident, $type:ident, $validate:expr) => {
        impl TryFrom<Map<String, Value>> for $wrapper<$type> {
            type Error = String;

            fn try_from(map: Map<String, Value>) -> Result<Self, Self::Error> {
                Ok(Self($validate(map)?))
            }
        }

        impl<'de> Deserialize<'de> for $wrapper<$type> {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: Deserializer<'de>,
            {
                let map = Map::<String, Value>::deserialize(deserializer)?;
                Self::try_from(map).map_err(serde::de::Error::custom)
            }
        }

        impl $wrapper<$type> {
            pub fn try_from_db(map: Map<String, Value>) -> Result<Self, String> {
                #[cfg(feature = "disable_db_data_validation")]
                return Ok(Self($type(map)));
                #[cfg(not(feature = "disable_db_data_validation"))]
                return Self::try_from(map);
            }
        }
    };
}

#[derive(
    Deserialize, Serialize, Clone, AsRef, Deref, DerefMut, Debug, Eq, PartialEq, Into,
)]
pub struct Overrides(Map<String, Value>);

impl Overrides {
    fn validate_data(override_map: Map<String, Value>) -> Result<Self, String> {
        if override_map.is_empty() {
            log::error!("Override validation error: Override is empty");
            return Err("Override should not be empty".to_owned());
        }
        Ok(Self(override_map))
    }
}

impl IntoIterator for Overrides {
    type Item = (String, Value);
    type IntoIter = <Map<String, Value> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl_try_from_map!(Cac, Overrides, Overrides::validate_data);
impl_try_from_map!(Exp, Overrides, Overrides::validate_data);

#[derive(Deserialize, Serialize, Clone, AsRef, Deref, Debug, Eq, PartialEq, Into)]
pub struct Condition(Map<String, Value>);
impl Condition {
    fn validate_data_for_cac(condition_map: Map<String, Value>) -> Result<Self, String> {
        if condition_map.is_empty() {
            log::error!("Condition validation error: Context is empty");
            return Err("Context should not be empty".to_owned());
        }
        jsonlogic::expression::Expression::from_json(&json!(condition_map)).map_err(
            |msg| {
                log::error!("Condition validation error: {}", msg);
                msg
            },
        )?;
        Ok(Self(condition_map))
    }

    fn validate_data_for_exp(condition_map: Map<String, Value>) -> Result<Self, String> {
        let condition_val = json!(condition_map);
        let ast = jsonlogic::expression::Expression::from_json(&condition_val).map_err(
            |msg| {
                log::error!("Condition validation error: {}", msg);
                msg
            },
        )?;
        let dimensions = ast.get_variable_names().map_err(|msg| {
            log::error!("Error while parsing variable names : {}", msg);
            msg
        })?;
        if dimensions.contains("variantIds") {
            log::error!("experiment's context should not contain variantIds dimension");
            return Err(
                "experiment's context should not contain variantIds dimension"
                    .to_string(),
            );
        }
        Ok(Self(condition_map))
    }
}

impl_try_from_map!(Cac, Condition, Condition::validate_data_for_cac);
impl_try_from_map!(Exp, Condition, Condition::validate_data_for_exp);

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
            return Err(format!(
                "{val} is invalid, it should obey the regex {regex_str}. \
                {}",
                self.get_error_message()
            ));
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

#[derive(Clone, Deserialize)]
pub struct TenantConfig {
    pub mandatory_dimensions: Vec<String>,
}

impl FromRequest for TenantConfig {
    type Error = actix_web::error::Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let result = req.extensions().get::<Self>().cloned().ok_or_else(|| {
            log::error!("Tenant config not found");
            error::ErrorInternalServerError("Tenant config not found")
        });

        ready(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

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

        let db_condition = serde_json::from_str::<Condition>(
            &json!(db_request_condition_map).to_string(),
        )
        .unwrap();
        let db_expected_condition =
            Cac::<Condition>::try_from_db(db_request_condition_map)
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

        let fail_condition = serde_json::from_str::<Cac<Condition>>(
            &json!(request_condition_map).to_string(),
        )
        .map_err(|_| "Invalid operation".to_owned());

        let fail_exp_condition = Exp::<Condition>::try_from(exp_condition_map.clone())
            .map(|a| a.into_inner())
            .map_err(|_| "variantIds should not be present".to_owned());

        assert_eq!(
            json!(fail_condition)
                .to_string()
                .contains("Invalid operation"),
            true
        );

        assert_eq!(
            json!(fail_exp_condition)
                .to_string()
                .contains("variantIds should not be present"),
            true
        );

        let db_expected_condition = Exp::<Condition>::try_from_db(exp_condition_map)
            .map(|_| true)
            .map_err(|_| "variantIds should not be present".to_string());

        assert_eq!(
            json!(db_expected_condition)
                .to_string()
                .contains("variantIds should not be present"),
            true
        );
    }

    #[test]
    fn test_deserialize_override() {
        let override_map = Map::from_iter(vec![
            ("key1".to_string(), json!("val1")),
            ("key2".to_string(), json!(5)),
        ]);

        let empty_override_map = Map::new();

        let deserialize_overrides =
            serde_json::from_str::<Overrides>(&json!(override_map).to_string()).unwrap();
        let db_expected_overrides =
            Cac::<Overrides>::try_from_db(override_map.clone()).map(|a| a.into_inner());
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

        assert_eq!(
            json!(empty_overrides)
                .to_string()
                .contains("override should not be empty"),
            true
        );
    }
}

#[derive(Debug, Clone)]
pub struct QueryFilters {
    pub count: Option<i64>,
    pub page: Option<i64>,
}

impl<'de> Deserialize<'de> for QueryFilters {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct Helper {
            count: Option<i64>,
            page: Option<i64>,
        }

        let helper = Helper::deserialize(deserializer)?;

        if let Some(count) = helper.count {
            if count <= 0 {
                return Err(de::Error::custom("Count should be greater than 0."));
            }
        }

        if let Some(page) = helper.page {
            if page <= 0 {
                return Err(de::Error::custom("Page should be greater than 0."));
            }
        }

        Ok(QueryFilters {
            count: helper.count,
            page: helper.page,
        })
    }
}

#[derive(Serialize, Debug, Clone, Deserialize)]
pub struct PaginatedResponse<T> {
    pub total_pages: i64,
    pub total_items: i64,
    pub data: Vec<T>,
}
