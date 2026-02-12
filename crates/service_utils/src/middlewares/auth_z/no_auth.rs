use futures_util::future::LocalBoxFuture;
use superposition_types::{Resource, User};

use crate::service::types::{OrganisationId, SchemaName};

use super::authorization::Authorizer;

pub struct NoAuth;

impl Authorizer for NoAuth {
    fn is_allowed(
        &self,
        _: &(OrganisationId, SchemaName),
        _: &User,
        _: &Resource,
        _: &str,
        _: Option<&[&str]>,
    ) -> LocalBoxFuture<'_, Result<bool, String>> {
        Box::pin(async { Ok(true) })
    }
}
