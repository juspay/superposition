//! What a principal is, and how claims become one.

use authn_kit::{
    AuthProfile,
    claims::{ClaimSource, IdentityClaims},
};
use superposition_types::User;

use crate::auth::scope::SuperpositionScope;

/// How a request authenticated.
///
/// Carried on the principal because two downstream consumers need it and cannot
/// recover it otherwise: `auth_z` bypasses authorization entirely for internal
/// and dispatch callers, and `InternalUserContext` is an extractor in several
/// handlers. The original recorded this by inserting marker types into request
/// extensions from inside the authenticator; an `authn_kit` authenticator has no
/// access to extensions, so the marker travels on the principal and the actix
/// adapter's extension hook puts it where existing code expects it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrincipalKind {
    /// Authenticated by a normal credential — OIDC, an API token, Basic auth.
    Standard,
    /// A trusted internal caller presenting the service token and an `x-user`
    /// header.
    Internal,
    /// The Kronos dispatcher calling back on the webhook route.
    Dispatch,
}

/// Superposition's authenticated principal.
#[derive(Clone, Debug)]
pub struct SuperpositionPrincipal {
    pub user: User,
    pub kind: PrincipalKind,
}

impl SuperpositionPrincipal {
    pub fn new(user: User, kind: PrincipalKind) -> Self {
        Self { user, kind }
    }

    pub fn standard(user: User) -> Self {
        Self::new(user, PrincipalKind::Standard)
    }
}

/// Maps claims from any mechanism onto a principal.
///
/// This is the whole of what the original spread across four places:
/// `try_user_from` for ID tokens, `IntrospectionResponse::into_user`,
/// `StaticToken::to_user`, and the inline `service-account-{id}` construction in
/// the client-credentials branch. Each is reproduced below, per source.
impl TryFrom<IdentityClaims> for SuperpositionPrincipal {
    type Error = String;

    fn try_from(claims: IdentityClaims) -> Result<Self, Self::Error> {
        let user = match claims.source {
            // No human identity exists in a machine grant, so the principal is
            // derived from the validated `client_id`. The `service-account-`
            // prefix is what existing Casbin policies are written against, so it
            // is reproduced exactly.
            ClaimSource::ClientCredentials => {
                let client_id = claims
                    .client_id
                    .clone()
                    .ok_or_else(|| String::from("client_id claim not found"))?;
                let principal = format!("service-account-{client_id}");
                User::new(principal.clone(), principal)
            }

            // Introspection was permissive in the original: identity fell back
            // across `username` -> `sub` -> `email`, and the email defaulted to
            // whichever was found.
            ClaimSource::Introspection => {
                let username = claims
                    .preferred_username
                    .clone()
                    .or_else(|| claims.subject.clone())
                    .or_else(|| claims.email.clone())
                    .ok_or_else(|| String::from("no usable identity claim"))?;
                let email = claims.email.clone().unwrap_or_else(|| username.clone());
                User::new(email, username)
            }

            // A static token names its principal directly, and its email
            // defaults to that principal.
            ClaimSource::StaticToken => {
                let principal = claims
                    .preferred_username
                    .clone()
                    .or_else(|| claims.subject.clone())
                    .ok_or_else(|| String::from("static token has no principal"))?;
                let email = claims.email.clone().unwrap_or_else(|| principal.clone());
                User::new(email, principal)
            }

            // ID and access tokens: an email is required, and the username falls
            // back to it. Exactly `try_user_from`.
            ClaimSource::IdToken | ClaimSource::AccessToken => {
                let email = claims
                    .email
                    .clone()
                    .ok_or_else(|| String::from("Email not found"))?;
                let username = claims
                    .preferred_username
                    .clone()
                    .or_else(|| claims.email.clone())
                    .ok_or_else(|| String::from("Username not found"))?;
                User::new(email, username)
            }
        };

        Ok(Self::standard(user))
    }
}

/// Binds the principal and scope types together for `authn_kit`.
pub struct SuperpositionProfile;

impl AuthProfile for SuperpositionProfile {
    type User = SuperpositionPrincipal;
    type Scope = SuperpositionScope;
}
