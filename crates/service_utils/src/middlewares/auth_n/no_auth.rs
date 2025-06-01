use actix_web::{
    cookie::{time::Duration, Cookie},
    dev::ServiceRequest,
    web::Path,
    HttpRequest, HttpResponse, Scope,
};
use futures_util::future::LocalBoxFuture;
use superposition_types::User;

use super::{
    authentication::{Authenticator, Login},
    SwitchOrgParams,
};

pub struct DisabledAuthenticator {
    organisations: Vec<String>,
    path_prefix: String,
}

impl DisabledAuthenticator {
    pub fn new(organisations: Vec<String>, path_prefix: String) -> Self {
        Self {
            organisations,
            path_prefix,
        }
    }
}

impl Authenticator for DisabledAuthenticator {
    fn get_path_prefix(&self) -> String {
        self.path_prefix.clone()
    }

    fn authenticate(
        &self,
        _: &ServiceRequest,
        _: &Login,
    ) -> Result<User, actix_web::HttpResponse> {
        Ok(User::default())
    }

    fn routes(&self) -> actix_web::Scope {
        Scope::new("no_auth")
    }

    fn get_organisations(&self, _: &actix_web::HttpRequest) -> HttpResponse {
        HttpResponse::Ok().json(serde_json::json!(self.organisations))
    }

    fn switch_organisation(
        &self,
        _: &HttpRequest,
        path_params: &Path<SwitchOrgParams>,
    ) -> LocalBoxFuture<'static, actix_web::Result<HttpResponse>> {
        let path = if self.path_prefix.as_str() == "" {
            String::from("/")
        } else {
            self.path_prefix.clone()
        };
        let cookie = Cookie::build(Login::Org.to_string(), "org_token")
            .path(path)
            .http_only(true)
            .max_age(Duration::days(1))
            .finish();

        let org_id = path_params.organisation_id.clone();
        let path_prefix = self.path_prefix.clone();

        Box::pin(async move {
            Ok(HttpResponse::Found()
                .cookie(cookie)
                .insert_header((
                    "Location",
                    format!("{path_prefix}/admin/{org_id}/workspaces"),
                ))
                .finish())
        })
    }
}
