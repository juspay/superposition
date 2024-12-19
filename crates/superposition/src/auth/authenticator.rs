use actix_web::{dev::ServiceRequest, HttpResponse, Scope};
use superposition_types::User;

pub trait Authenticator: Sync + Send {
    fn authenticate(&self, request: &ServiceRequest) -> Result<User, HttpResponse>;
    fn routes(&self) -> Scope;
}
