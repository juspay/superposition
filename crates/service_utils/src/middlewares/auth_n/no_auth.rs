use actix_web::{error, HttpRequest, HttpResponse, Scope};
use futures_util::future::LocalBoxFuture;
use superposition_types::User;

use crate::middlewares::auth_n::helpers::fetch_org_ids_from_db;

use super::authentication::{Authenticator, Login};

/// An Authenticator implementation that performs no authentication
/// This is primarily for development and testing purposes
/// In production, a proper Authenticator (like OIDCAuthenticator) should be used
pub struct DisabledAuthenticator {
    path_prefix: String,
}

impl DisabledAuthenticator {
    pub fn new(path_prefix: String) -> Self {
        Self { path_prefix }
    }
}

impl Authenticator for DisabledAuthenticator {
    fn get_path_prefix(&self) -> String {
        self.path_prefix.clone()
    }

    fn authenticate(
        &self,
        _: &HttpRequest,
        _: &Login,
    ) -> LocalBoxFuture<'static, Result<User, HttpResponse>> {
        Box::pin(async { Ok(User::default()) })
    }

    fn routes(&self) -> actix_web::Scope {
        Scope::new("no_auth")
    }

    fn get_organisations(&self, req: &actix_web::HttpRequest) -> HttpResponse {
        match fetch_org_ids_from_db(req) {
            Ok(resp) => HttpResponse::Ok().json(resp),
            Err(resp) => error::ErrorInternalServerError(resp).into(),
        }
    }

    fn generate_org_user(
        &self,
        _: &HttpRequest,
        _: &str,
        _: &Login,
    ) -> LocalBoxFuture<'_, Result<String, HttpResponse>> {
        Box::pin(async { Ok("org_token".to_string()) })
    }
}
