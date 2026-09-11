//! The Kronos webhook callback credential.

use authn_kit::{
    AuthContext, AuthError, Authenticator, Credential, Verdict,
    authenticator::AuthProfile,
};
use secrecy::{ExposeSecret, SecretString};
use superposition_types::User;

use crate::{
    auth::principal::{PrincipalKind, SuperpositionPrincipal, SuperpositionProfile},
    kronos_dispatch::DISPATCHER_USERNAME,
};

/// Authenticates the Kronos dispatcher's `Basic` callback credential.
///
/// Deliberately narrow, as in the original: it applies **only** on the dispatch
/// webhook path, so the credential cannot be replayed against any other route.
/// `auth_z` skips authorization for the resulting principal, which is why the
/// path restriction matters.
pub struct DispatcherAuthenticator {
    token: SecretString,
    path_suffix: &'static str,
}

impl DispatcherAuthenticator {
    /// The path suffix the original matched with
    /// `request.path().ends_with("/dispatch/webhook")`.
    pub const WEBHOOK_PATH: &'static str = "/dispatch/webhook";

    pub fn new(token: impl Into<String>) -> Self {
        Self {
            token: SecretString::from(token.into()),
            path_suffix: Self::WEBHOOK_PATH,
        }
    }
}

impl std::fmt::Debug for DispatcherAuthenticator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DispatcherAuthenticator")
            .field("path_suffix", &self.path_suffix)
            .finish()
    }
}

#[async_trait::async_trait]
impl Authenticator<SuperpositionProfile> for DispatcherAuthenticator {
    fn name(&self) -> &'static str {
        "kronos-dispatcher"
    }

    async fn authenticate(
        &self,
        ctx: &AuthContext<'_, SuperpositionProfile>,
    ) -> Result<Verdict<<SuperpositionProfile as AuthProfile>::User>, AuthError> {
        if !ctx.request.path().ends_with(self.path_suffix) {
            return Ok(Verdict::NotApplicable);
        }

        let Credential::Basic { id, secret } = ctx.credential else {
            return Ok(Verdict::NotApplicable);
        };
        if id != DISPATCHER_USERNAME
            || secret.expose_secret() != self.token.expose_secret()
        {
            // Not the dispatcher: leave it to the ordinary Basic authenticator,
            // which is what the original did by falling through.
            return Ok(Verdict::NotApplicable);
        }

        Ok(Verdict::Authenticated(SuperpositionPrincipal::new(
            User::new(
                format!("{DISPATCHER_USERNAME}@superposition.io"),
                DISPATCHER_USERNAME.to_string(),
            ),
            PrincipalKind::Dispatch,
        )))
    }
}
