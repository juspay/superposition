use actix_web::{dev::ServiceRequest, HttpResponse, Scope};

pub trait Authenticator: Sync + Send {
    fn authenticate(
        &self,
        request: &ServiceRequest,
    ) -> Result<(), HttpResponse>;
    fn routes(&self) -> Scope;
}
