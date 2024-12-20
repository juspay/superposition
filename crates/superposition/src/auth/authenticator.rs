use actix_web::{dev::ServiceRequest, web::Path, HttpRequest, HttpResponse, Scope};
use futures_util::future::LocalBoxFuture;
use serde::Deserialize;
use superposition_types::User;

#[derive(Deserialize)]
pub(super) struct SwitchOrgParams {
    pub(super) organisation_id: String,
}

pub trait Authenticator: Sync + Send {
    fn authenticate(&self, request: &ServiceRequest) -> Result<User, HttpResponse>;
    fn routes(&self) -> Scope;

    fn get_organisations(&self, req: &HttpRequest) -> HttpResponse;

    fn switch_organisation(
        &self,
        req: &HttpRequest,
        path: &Path<SwitchOrgParams>,
    ) -> LocalBoxFuture<'static, actix_web::Result<HttpResponse>>;
}
