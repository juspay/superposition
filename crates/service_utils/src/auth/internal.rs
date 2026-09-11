//! The trusted `Internal` service-to-service scheme.

use authn_kit::{
    AuthContext, AuthError, Authenticator, Credential, Verdict,
    authenticator::AuthProfile,
};
use secrecy::{ExposeSecret, SecretString};
use superposition_types::User;

use crate::auth::principal::{
    PrincipalKind, SuperpositionPrincipal, SuperpositionProfile,
};

/// Authenticates `Authorization: Internal <token>` plus an `x-user` header.
///
/// The token proves the caller is inside the trust boundary; the `x-user` header
/// then *asserts* an identity, which is taken at face value. `auth_z` skips
/// authorization entirely for these requests, so the service token is
/// effectively a root credential.
///
/// Ported unchanged from `process_internal_token`, including the fall-through:
/// a correct token with a missing or unparseable `x-user` header **declines**
/// rather than failing, so the request is still eligible for cookie
/// authentication. That was the original's behaviour via `Option`, and changing
/// it during a migration would be a silent behavioural change.
pub struct InternalAuthenticator {
    token: SecretString,
}

impl InternalAuthenticator {
    pub fn new(token: impl Into<String>) -> Self {
        Self {
            token: SecretString::from(token.into()),
        }
    }
}

impl std::fmt::Debug for InternalAuthenticator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("InternalAuthenticator")
    }
}

#[async_trait::async_trait]
impl Authenticator<SuperpositionProfile> for InternalAuthenticator {
    fn name(&self) -> &'static str {
        "internal"
    }

    async fn authenticate(
        &self,
        ctx: &AuthContext<'_, SuperpositionProfile>,
    ) -> Result<Verdict<<SuperpositionProfile as AuthProfile>::User>, AuthError> {
        let Credential::Other { scheme, value } = ctx.credential else {
            return Ok(Verdict::NotApplicable);
        };
        if scheme != "internal" || value.expose_secret() != self.token.expose_secret() {
            return Ok(Verdict::NotApplicable);
        }

        // A valid service token but no usable `x-user`: decline, matching the
        // original's `None`, so cookie authentication still gets a turn.
        let Some(user) = ctx
            .request
            .header("x-user")
            .and_then(|raw| serde_json::from_str::<User>(raw).ok())
        else {
            return Ok(Verdict::NotApplicable);
        };

        Ok(Verdict::Authenticated(SuperpositionPrincipal::new(
            user,
            PrincipalKind::Internal,
        )))
    }
}
