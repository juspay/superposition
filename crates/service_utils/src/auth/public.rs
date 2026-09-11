//! The excluded-route authenticator.

use authn_kit::{
    AuthContext, AuthError, Authenticator, Verdict, authenticator::AuthProfile,
};
use superposition_types::User;

use crate::auth::{
    principal::{SuperpositionPrincipal, SuperpositionProfile},
    scope::SuperpositionScope,
};

/// Authenticates every request on an excluded route as a fixed identity.
///
/// Reproduces the original's `Login::None` handling, where every authenticator
/// branch returned `User::default()` — `user@superposition.io` — *without
/// examining the credential at all*. That is why this is registered **first**:
/// an excluded route must short-circuit before any token is validated, exactly
/// as `authenticate_with_bearer_token` did when it matched `Login::None`.
///
/// The identity is supplied rather than assumed, so a deployment that would
/// rather excluded routes carry no identity can simply not register this.
pub struct PublicScopeAuthenticator {
    identity: User,
}

impl PublicScopeAuthenticator {
    pub fn new(identity: User) -> Self {
        Self { identity }
    }

    /// `User::default()`, matching the original exactly.
    pub fn with_default_identity() -> Self {
        Self::new(User::default())
    }
}

impl std::fmt::Debug for PublicScopeAuthenticator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("PublicScopeAuthenticator")
    }
}

#[async_trait::async_trait]
impl Authenticator<SuperpositionProfile> for PublicScopeAuthenticator {
    fn name(&self) -> &'static str {
        "public-scope"
    }

    async fn authenticate(
        &self,
        ctx: &AuthContext<'_, SuperpositionProfile>,
    ) -> Result<Verdict<<SuperpositionProfile as AuthProfile>::User>, AuthError> {
        if !matches!(ctx.scope, SuperpositionScope::None) {
            return Ok(Verdict::NotApplicable);
        }
        Ok(Verdict::Authenticated(SuperpositionPrincipal::standard(
            self.identity.clone(),
        )))
    }
}
