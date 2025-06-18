use std::fmt::Display;

use actix_web::{dev::ServiceRequest, web::Path, HttpRequest, HttpResponse, Scope};
use futures_util::future::LocalBoxFuture;
use serde::Deserialize;
use superposition_types::User;

#[derive(Deserialize)]
pub(super) struct SwitchOrgParams {
    pub(super) organisation_id: String,
}

#[derive(Debug)]
pub enum Login {
    None,
    Global,
    Org,
}

impl Display for Login {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "none"),
            Self::Global => write!(f, "user"),
            Self::Org => write!(f, "org_user"),
        }
    }
}

pub trait Authenticator: Sync + Send {
    fn get_path_prefix(&self) -> String;

    fn authenticate(
        &self,
        request: &ServiceRequest,
        login_type: &Login,
    ) -> Result<User, HttpResponse>;
    fn routes(&self) -> Scope;

    fn get_organisations(&self, req: &HttpRequest) -> HttpResponse;

    fn switch_organisation(
        &self,
        req: &HttpRequest,
        path: &Path<SwitchOrgParams>,
    ) -> LocalBoxFuture<'static, actix_web::Result<HttpResponse>>;
}
