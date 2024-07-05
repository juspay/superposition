pub mod result;
use crate::result as superposition;
use actix::fut::{ready, Ready};
use actix_web::{dev::Payload, error, FromRequest, HttpMessage, HttpRequest};
use derive_more::{AsRef, Deref, Into};
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
                    log::error!("experiment's context should not contain variantIds dimension");
                    return Err(superposition::AppError::BadArgument("experiment's context should not contain variantIds dimension".to_string()))
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

#[derive(Deserialize, Serialize, Clone, AsRef, Deref, Debug, Eq, PartialEq, Into)]
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
