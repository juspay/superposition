pub mod result;
use crate::result as superposition;
use actix::fut::{ready, Ready};
use actix_web::{dev::Payload, error, FromRequest, HttpMessage, HttpRequest};
use derive_more::{AsRef, Deref, DerefMut, Into};
use log::error;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};

pub trait SuperpositionUser {
    fn get_email(&self) -> String;
    fn get_username(&self) -> String;
    fn get_auth_token(&self) -> String;
    fn get_auth_type(&self) -> String;
}

#[derive(Debug, Clone)]
pub struct User {
    pub email: String,
    pub username: String,
    pub auth_token: String,
    pub auth_type: String,
}

impl SuperpositionUser for User {
    fn get_email(&self) -> String {
        self.email.clone()
    }

    fn get_username(&self) -> String {
        self.username.clone()
    }

    fn get_auth_token(&self) -> String {
        self.auth_token.clone()
    }

    fn get_auth_type(&self) -> String {
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

impl From<Box<dyn SuperpositionUser>> for User {
    fn from(value: Box<dyn SuperpositionUser>) -> Self {
        User {
            email: value.get_email(),
            username: value.get_username(),
            auth_token: value.get_auth_token(),
            auth_type: value.get_auth_type(),
        }
    }
}

impl FromRequest for User {
    type Error = actix_web::error::Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(req: &HttpRequest, _: &mut Payload) -> Self::Future {
        if let Some(user) = req.extensions().get::<User>() {
            ready(Ok(user.to_owned()))
        } else {
            error!("No user was found while validating token");
            ready(Err(error::ErrorUnauthorized(
                json!({"message":"invalid token provided"}),
            )))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValidationType {
    DEFAULT,
    EXPERIMENTAL,
    DB,
}

#[derive(Deserialize, Clone, AsRef, Deref, Debug, Eq, PartialEq, Serialize, Into)]
#[serde(try_from = "Map<String,Value>")]
pub struct Condition(Map<String, Value>);

impl Condition {
    pub fn new(
        condition_map: Map<String, Value>,
        validation_type: ValidationType,
    ) -> superposition::Result<Self> {
        match validation_type {
            ValidationType::DEFAULT => {
                if condition_map.is_empty() {
                    log::error!("Condition validation error: Context is empty");
                    return Err(superposition::AppError::BadArgument(
                        "Context should not be empty".to_owned(),
                    ));
                }
                jsonlogic::expression::Expression::from_json(&json!(condition_map))
                    .map_err(|msg| {
                        log::error!("Condition validation error: {}", msg);
                        superposition::AppError::BadArgument(msg)
                    })?;
            }
            ValidationType::EXPERIMENTAL => {
                let condition_val = json!(condition_map);
                let ast = jsonlogic::expression::Expression::from_json(&condition_val)
                    .map_err(|msg| {
                        log::error!("Condition validation error: {}", msg);
                        superposition::AppError::BadArgument(msg)
                    })?;
                let dimensions = ast.get_variable_names().map_err(|msg| {
                    log::error!("Error while parsing variable names : {}", msg);
                    superposition::AppError::BadArgument(msg)
                })?;
                if dimensions.contains("variantIds") {
                    log::error!(
                        "experiment's context should not contain variantIds dimension"
                    );
                    return Err(superposition::AppError::BadArgument(
                        "experiment's context should not contain variantIds dimension"
                            .to_string(),
                    ));
                }
            }
            ValidationType::DB => (),
        };
        Ok(Self(condition_map))
    }
}

impl TryFrom<Map<String, Value>> for Condition {
    type Error = String;
    fn try_from(s: Map<String, Value>) -> Result<Self, Self::Error> {
        Condition::new(s, ValidationType::DEFAULT).map_err(|err| err.message())
    }
}

#[derive(
    Deserialize, Serialize, Clone, AsRef, Deref, DerefMut, Debug, Eq, PartialEq, Into,
)]
#[serde(try_from = "Map<String,Value>")]
pub struct Overrides(Map<String, Value>);

impl Overrides {
    pub fn new(
        override_map: Map<String, Value>,
        validation_type: ValidationType,
    ) -> superposition::Result<Self> {
        match validation_type {
            ValidationType::DEFAULT | ValidationType::EXPERIMENTAL => {
                if override_map.is_empty() {
                    log::error!("Override validation error: Override is empty");
                    return Err(superposition::AppError::BadArgument(
                        "Override should not be empty".to_owned(),
                    ));
                }
            }
            ValidationType::DB => (),
        };

        Ok(Self(override_map))
    }
}

impl TryFrom<Map<String, Value>> for Overrides {
    type Error = String;
    fn try_from(s: Map<String, Value>) -> Result<Self, Self::Error> {
        Overrides::new(s, ValidationType::DEFAULT).map_err(|err| err.message())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn ok_test_deserialize_condition() -> superposition::Result<()> {
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
            Condition::new(db_request_condition_map, ValidationType::DB)?;
        assert_eq!(db_condition, db_expected_condition);

        let default_condition = serde_json::from_str::<Condition>(
            &json!(default_request_condition_map).to_string(),
        )
        .unwrap();
        let default_expected_condition =
            Condition::new(default_request_condition_map, ValidationType::DEFAULT)?;
        assert_eq!(default_condition, default_expected_condition);

        let exp_condition = serde_json::from_str::<Condition>(
            &json!(exp_request_condition_map).to_string(),
        )
        .unwrap();
        let exp_expected_condition =
            Condition::new(exp_request_condition_map, ValidationType::EXPERIMENTAL)?;
        assert_eq!(exp_condition, exp_expected_condition);

        Ok(())
    }

    #[test]
    fn fail_test_deserialize_condition() -> superposition::Result<()> {
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

        let fail_condition =
            serde_json::from_str::<Condition>(&json!(request_condition_map).to_string())
                .map_err(|_| "Invalid operation".to_owned());

        let fail_exp_condition =
            Condition::new(exp_condition_map, ValidationType::EXPERIMENTAL)
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

        let db_expected_condition =
            Condition::new(request_condition_map.clone(), ValidationType::DB)
                .map(|_| true)?;
        assert_eq!(db_expected_condition, true);

        Ok(())
    }

    #[test]
    fn test_deserialize_override() -> superposition::Result<()> {
        let override_map = Map::from_iter(vec![
            ("key1".to_string(), json!("val1")),
            ("key2".to_string(), json!(5)),
        ]);

        let empty_override_map = Map::new();

        let deserialize_overrides =
            serde_json::from_str::<Overrides>(&json!(override_map).to_string()).unwrap();
        let db_expected_overrides =
            Overrides::new(override_map.clone(), ValidationType::DB)?;
        assert_eq!(deserialize_overrides, db_expected_overrides);

        let exp_expected_overrides =
            Overrides::new(override_map.clone(), ValidationType::EXPERIMENTAL)?;
        assert_eq!(deserialize_overrides, exp_expected_overrides);

        let default_expected_overrides =
            Overrides::new(override_map.clone(), ValidationType::DEFAULT)?;
        assert_eq!(deserialize_overrides, default_expected_overrides);

        let empty_overrides = serde_json::from_str::<Overrides>(
            &json!(empty_override_map.clone()).to_string(),
        )
        .map_err(|_| "override should not be empty".to_string());

        assert_eq!(
            json!(empty_overrides)
                .to_string()
                .contains("override should not be empty"),
            true
        );

        Ok(())
    }
}
