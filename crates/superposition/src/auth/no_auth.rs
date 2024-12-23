use actix_web::{
    cookie::{time::Duration, Cookie},
    dev::ServiceRequest,
    web::Path,
    HttpRequest, HttpResponse, Scope,
};
use futures_util::future::LocalBoxFuture;
use superposition_types::User;

use super::authenticator::{Authenticator, SwitchOrgParams};

pub struct DisabledAuthenticator(Vec<String>);

impl DisabledAuthenticator {
    pub fn new(organisations: Vec<String>) -> Self {
        Self(organisations)
    }
}

impl Authenticator for DisabledAuthenticator {
    fn authenticate(&self, _: &ServiceRequest) -> Result<User, actix_web::HttpResponse> {
        Ok(User::default())
    }

    fn routes(&self) -> actix_web::Scope {
        Scope::new("no_auth")
    }

    fn get_organisations(&self, _: &actix_web::HttpRequest) -> HttpResponse {
        HttpResponse::Ok().json(serde_json::json!(self.0))
    }

    fn switch_organisation(
        &self,
        _: &HttpRequest,
        path: &Path<SwitchOrgParams>,
    ) -> LocalBoxFuture<'static, actix_web::Result<HttpResponse>> {
        let cookie = Cookie::build("org_user", "org_token")
            .path("/")
            .http_only(true)
            .max_age(Duration::days(1))
            .finish();

        let org_id = path.organisation_id.clone();

        Box::pin(async move {
            Ok(HttpResponse::Found()
                .cookie(cookie)
                .insert_header(("Location", format!("/admin/{org_id}/workspaces")))
                .finish())
        })
    }
}