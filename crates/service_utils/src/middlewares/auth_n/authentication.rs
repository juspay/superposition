use std::fmt::Display;

use actix_web::{
    cookie::{time::Duration, Cookie},
    http::header,
    web::Path,
    HttpRequest, HttpResponse, Scope,
};
use futures_util::future::LocalBoxFuture;
use serde::Deserialize;
use superposition_types::User;

#[derive(Deserialize)]
pub(super) struct SwitchOrgParams {
    pub(super) organisation_id: String,
}

#[derive(Debug, Clone)]
pub enum Login {
    None,
    Global,
    Org(String),
}

impl Display for Login {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Login::None => write!(f, "none"),
            Login::Global => write!(f, "user"),
            Login::Org(org_id) => write!(f, "org_{org_id}"),
        }
    }
}

pub trait Authenticator: Sync + Send {
    fn routes(&self) -> Scope;

    fn get_path_prefix(&self) -> String;

    fn get_cookie_path(&self) -> String {
        let prefix = self.get_path_prefix();
        if prefix.as_str() == "" {
            String::from('/')
        } else {
            prefix
        }
    }

    fn authenticate(
        &self,
        request: &HttpRequest,
        login_type: &Login,
    ) -> LocalBoxFuture<'static, Result<User, HttpResponse>>;

    fn get_organisations(&self, req: &HttpRequest) -> HttpResponse;

    fn generate_org_user<'a>(
        &'a self,
        req: &HttpRequest,
        org_id: &str,
        login_type: &Login,
    ) -> LocalBoxFuture<'a, Result<String, HttpResponse>>;

    fn switch_organisation<'a>(
        &'a self,
        req: &HttpRequest,
        path: &Path<SwitchOrgParams>,
    ) -> LocalBoxFuture<'a, HttpResponse> {
        let login_type = Login::Org(path.organisation_id.clone());
        let user_token_future =
            self.generate_org_user(req, &path.organisation_id, &login_type);

        let prefix = self.get_path_prefix();
        let cookie_path = self.get_cookie_path();
        let org_id = path.organisation_id.clone();

        Box::pin(async move {
            match user_token_future.await {
                Ok(token) => {
                    let cookie = Cookie::build(login_type.to_string(), token)
                        .path(cookie_path)
                        .http_only(true)
                        .secure(true)
                        .max_age(Duration::days(1))
                        .finish();
                    HttpResponse::Found()
                        .cookie(cookie)
                        .insert_header((
                            header::LOCATION,
                            format!("{prefix}/admin/{org_id}/workspaces"),
                        ))
                        .finish()
                }
                Err(resp) => resp,
            }
        })
    }
}
