use actix_web::Scope;
use futures_util::future::LocalBoxFuture;
use superposition_types::User;

use crate::service::types::{Resource, WorkspaceContext};

use super::authorization::Authorizer;

pub struct NoAuth;

impl Authorizer for NoAuth {
    fn management_routes(&self) -> actix_web::Scope {
        Scope::new("")
    }

    fn is_allowed(
        &self,
        _: &WorkspaceContext,
        _: &User,
        _: &Resource,
        _: &str,
        _: Option<&String>,
    ) -> LocalBoxFuture<'_, Result<bool, String>> {
        Box::pin(async { Ok(true) })
    }
}
