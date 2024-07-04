pub mod result;
use actix::fut::{ready, Ready};
use actix_web::{dev::Payload, error, FromRequest, HttpMessage, HttpRequest};
use log::error;
use serde_json::json;

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
