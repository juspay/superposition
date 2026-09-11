//! Superposition's request scoping — the replacement for `Login`.

use std::{collections::HashSet, fmt::Display};

use authn_kit::{AuthRequest, AuthScope, ScopeResolver};

/// Which realm a request authenticates against.
///
/// The `Display` values are load-bearing: they are the session cookie names, so
/// they must match the original's `Login::to_string()` exactly or every existing
/// browser session is invalidated on deploy.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum SuperpositionScope {
    /// A route excluded from authentication.
    None,
    /// Organisation-independent: the org list, admin authz, admin settings.
    Global,
    /// Scoped to one organisation.
    Org(String),
}

impl Display for SuperpositionScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "none"),
            Self::Global => write!(f, "user"),
            Self::Org(org_id) => write!(f, "org_{org_id}"),
        }
    }
}

impl AuthScope for SuperpositionScope {
    /// Always `false`, including for [`Self::None`].
    ///
    /// `authn_kit`'s notion of "public" yields
    /// [`Outcome::Anonymous`](authn_kit::Outcome) — no principal at all. The
    /// original instead handed excluded routes a fixed `User::default()`, and
    /// reproducing that exactly means the scope must be authenticated by
    /// something. That something is
    /// [`PublicScopeAuthenticator`](crate::auth::public::PublicScopeAuthenticator),
    /// registered first in the chain so an excluded route short-circuits before
    /// any credential is examined — which is what the original did by returning
    /// `User::default()` from every `Login::None` branch.
    fn is_public(&self) -> bool {
        false
    }

    /// Mixed into credential-cache keys, so a credential validated for one
    /// organisation can never be served from cache for another. The original
    /// left this to each call site to remember.
    fn cache_discriminator(&self) -> Option<&str> {
        match self {
            Self::Org(org_id) => Some(org_id),
            Self::None | Self::Global => None,
        }
    }
}

/// Derives the scope of a request.
///
/// A direct port of `AuthNMiddleware::get_login_type`, including the ordering
/// quirk: a path matching one of the global families is `Global` **even when it
/// also appears in the exclusion list**, because the original matched
/// `(_, true)` before `(true, false)`. `/organisations` is in the default
/// exclusion list and is nonetheless authenticated, and reproducing that is the
/// difference between a working cutover and an unauthenticated org endpoint.
pub struct SuperpositionScopes {
    exclusions: HashSet<String>,
    path_prefix: String,
}

impl SuperpositionScopes {
    pub fn new(exclusions: HashSet<String>, path_prefix: String) -> Self {
        Self {
            exclusions,
            path_prefix,
        }
    }

    /// The organisation id, from the same three places the original looked:
    /// the `x-org-id` header, an `{org_id}` path segment, then an `org` query
    /// parameter.
    fn organisation_id(request: &AuthRequest) -> String {
        request
            .header("x-org-id")
            .or_else(|| request.path_param("{org_id}"))
            .or_else(|| request.query_param("org"))
            .unwrap_or_default()
            .to_string()
    }
}

impl ScopeResolver for SuperpositionScopes {
    type Scope = SuperpositionScope;

    fn resolve(&self, request: &AuthRequest) -> SuperpositionScope {
        let pattern = request
            .route_pattern()
            .map(|pattern| pattern.replace(&self.path_prefix, ""))
            .unwrap_or_else(|| request.path().replace(&self.path_prefix, ""));

        let excluded = self.exclusions.contains(&pattern);
        let path = request.path();
        let global = path.contains("/organisations")
            || path.contains("/authz/admin")
            || path.contains("/admin/settings");

        match (excluded, global) {
            (true, false) => SuperpositionScope::None,
            (_, true) => SuperpositionScope::Global,
            (false, false) => SuperpositionScope::Org(Self::organisation_id(request)),
        }
    }
}
