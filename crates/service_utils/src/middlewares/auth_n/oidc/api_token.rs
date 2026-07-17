//! Optional **API-token authentication** for machine-to-machine access under an
//! OIDC provider. A bearer token of the form `Bearer <prefix><delimiter><key>`
//! is routed here (the prefix marks it as an API key) and validated by one of
//! two mechanisms, tried in order:
//!
//! 1. **Static tokens** — a KMS-encrypted, operator-configured list of tokens,
//!    each mapping to a fixed principal (and, in SaaS, a specific organisation).
//!    Validated locally with a constant-time compare; no network call. Handy for
//!    a small, fixed set of service accounts or for setups without an
//!    introspection endpoint.
//! 2. **RFC 7662 token introspection** — the `<key>` is forwarded, per RFC 7662,
//!    to a configured/discovered introspection endpoint which answers whether the
//!    token is active and returns its claims. Modelled on RFC 7662 so any
//!    provider (Keycloak, Okta, Hydra, ... or a small custom service) can serve
//!    it with little or no bespoke code.
//!
//! The whole flow is optional and only available under an OIDC provider: it
//! activates only when a prefix plus at least one of the two mechanisms is
//! configured (see [`ApiTokenConfig::from_env`]).

use std::time::{Duration, SystemTime, UNIX_EPOCH};

use actix_web::error::{ErrorServiceUnavailable, ErrorUnauthorized};
use once_cell::sync::Lazy;
use serde::Deserialize;
use superposition_types::User;

use crate::helpers::{get_from_env_or_default, get_from_env_unsafe};

/// Reused connection-pooling client for introspection calls.
static HTTP_CLIENT: Lazy<reqwest::Client> = Lazy::new(reqwest::Client::new);

/// Default upper bound (seconds) on how long an introspection result is cached,
/// regardless of the token's `exp`. Introspection exists for near-real-time
/// revocation, so we cap caching to keep revocation lag small even for
/// long-lived tokens. Overridable via `OIDC_MAX_INTROSPECTION_CACHE_TTL_SECS`.
const DEFAULT_MAX_INTROSPECTION_CACHE_TTL_SECS: u64 = 300;

/// Length-independent* byte comparison, to avoid leaking how many leading bytes
/// of a static token matched via response timing. (*Length is not hidden; token
/// values are high-entropy secrets so that leak is immaterial.)
fn constant_time_eq(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    let mut diff = 0u8;
    for (x, y) in a.iter().zip(b.iter()) {
        diff |= x ^ y;
    }
    diff == 0
}

/// One entry in the static-token list: a shared-secret token bound to a fixed
/// identity (and, in SaaS, to an organisation). Deserialized from the JSON in
/// `OIDC_API_STATIC_TOKENS`.
#[derive(Deserialize)]
struct StaticToken {
    /// The secret value presented as the API key (after the prefix is stripped).
    token: String,
    /// Identity to authenticate as; used as the username and (absent `email`)
    /// the email. Authorization (Casbin) keys off this.
    principal: String,
    /// Optional email for the principal; defaults to `principal`.
    #[serde(default)]
    email: Option<String>,
    /// SaaS only: the organisation this token is valid for. `None` means the
    /// token is valid for global-scoped requests. Ignored in Simple OIDC.
    #[serde(default)]
    org: Option<String>,
}

impl StaticToken {
    fn to_user(&self) -> User {
        let email = self.email.clone().unwrap_or_else(|| self.principal.clone());
        User::new(email, self.principal.clone())
    }

    /// Whether this entry authenticates `api_key` for the request `scope`.
    /// When `enforce_org` (SaaS), the org binding must match: an org-scoped
    /// request needs `org == Some(scope)`, a global request needs `org == None`.
    /// In Simple OIDC (`!enforce_org`) the org binding is ignored.
    fn matches(&self, enforce_org: bool, scope: Option<&str>, api_key: &str) -> bool {
        if enforce_org {
            let scope_ok = match (scope, self.org.as_deref()) {
                (Some(req), Some(tok)) => req == tok,
                (None, None) => true,
                _ => false,
            };
            if !scope_ok {
                return false;
            }
        }
        constant_time_eq(self.token.as_bytes(), api_key.as_bytes())
    }
}

/// Introspection-endpoint settings, present only when RFC 7662 introspection is
/// configured (`OIDC_INTROSPECTION_AUTH_HEADER` set). Absent for a static-only
/// setup.
struct IntrospectionSettings {
    /// Verbatim `Authorization` header value Superposition presents to the
    /// introspection endpoint (e.g. `Bearer <token>` or `Basic <base64>`) — the
    /// endpoint is protected per RFC 7662 §2.1. A secret; KMS-decrypted upstream
    /// and passed in.
    auth_header: String,
    /// SaaS per-org endpoint format with `<organisation>` (`None` for Simple).
    /// This is *not* discoverable — per-org discovery would be a network call
    /// per org — so org-scoped API tokens require it.
    org_endpoint_format: Option<String>,
    /// Explicit single/global introspection endpoint (both modes). Optional:
    /// when absent, the endpoint discovered from provider metadata is used.
    global_endpoint: Option<String>,
    /// Upper bound on how long an introspection result is cached, regardless of
    /// the token's `exp`. Read from env at startup.
    max_cache_ttl: Duration,
}

/// Static configuration for the API-token flow, loaded from env. Owns the prefix
/// plus whichever validation mechanism(s) are configured.
pub(super) struct ApiTokenConfig {
    /// `custom_prefix` + `delimiter`, matched against (and stripped from) the
    /// incoming bearer token to select this flow.
    pub(super) prefix: String,
    /// SaaS (per-org) mode: enforces static-token org binding and requires the
    /// per-org introspection endpoint format.
    is_saas: bool,
    /// Static tokens, checked before introspection. May be empty.
    static_tokens: Vec<StaticToken>,
    /// Introspection settings; `None` for a static-only setup.
    introspection: Option<IntrospectionSettings>,
}

impl ApiTokenConfig {
    /// Loads config from env and the two upstream-decrypted secrets. Returns
    /// `Ok(None)` (feature disabled) when the prefix is unset, or when the prefix
    /// is set but neither validation mechanism is configured. Returns `Err` on a
    /// malformed static-token list so the misconfiguration fails startup.
    ///
    /// - `auth_header`: the KMS-decrypted `OIDC_INTROSPECTION_AUTH_HEADER`;
    ///   `Some` enables the introspection mechanism.
    /// - `static_tokens_raw`: the KMS-decrypted `OIDC_API_STATIC_TOKENS` JSON;
    ///   a non-empty list enables the static-token mechanism.
    pub(super) fn from_env(
        auth_header: Option<String>,
        static_tokens_raw: Option<String>,
        is_saas: bool,
    ) -> Result<Option<Self>, String> {
        // Without a prefix the flow can never be triggered from a bearer token.
        let Some(prefix) = get_from_env_unsafe::<String>("OIDC_API_TOKEN_PREFIX")
            .ok()
            .filter(|p| !p.is_empty())
        else {
            return Ok(None);
        };
        let delimiter: String =
            get_from_env_or_default("OIDC_API_TOKEN_DELIMITER", "_".into());

        let static_tokens = match static_tokens_raw {
            Some(raw) => parse_static_tokens(&raw)?,
            None => Vec::new(),
        };

        let introspection = auth_header.map(|auth_header| {
            let max_cache_ttl = Duration::from_secs(get_from_env_or_default(
                "OIDC_MAX_INTROSPECTION_CACHE_TTL_SECS",
                DEFAULT_MAX_INTROSPECTION_CACHE_TTL_SECS,
            ));
            let org_endpoint_format = is_saas
                .then(|| {
                    get_from_env_unsafe::<String>(
                        "OIDC_ORG_TOKEN_INTROSPECTION_URL_FORMAT",
                    )
                    .ok()
                })
                .flatten();
            let global_endpoint =
                get_from_env_unsafe::<String>("OIDC_TOKEN_INTROSPECTION_URL").ok();
            IntrospectionSettings {
                auth_header,
                org_endpoint_format,
                global_endpoint,
                max_cache_ttl,
            }
        });

        // A prefix alone validates nothing; require at least one mechanism.
        if introspection.is_none() && static_tokens.is_empty() {
            log::warn!(
                "OIDC_API_TOKEN_PREFIX is set but neither static tokens \
                 (OIDC_API_STATIC_TOKENS) nor introspection \
                 (OIDC_INTROSPECTION_AUTH_HEADER) are configured; API-token \
                 authentication is disabled"
            );
            return Ok(None);
        }

        Ok(Some(Self {
            prefix: format!("{prefix}{delimiter}"),
            is_saas,
            static_tokens,
            introspection,
        }))
    }

    /// Whether RFC 7662 introspection is configured (vs. static-only).
    pub(super) fn introspection_enabled(&self) -> bool {
        self.introspection.is_some()
    }

    /// Cache TTL for an introspection result: the time until the token's `exp`,
    /// capped at the configured `max_cache_ttl`. Returns `None` (no or
    /// already-past `exp`, or introspection not configured) so the cache
    /// applies its own short fallback.
    pub(super) fn cache_ttl(&self, exp: Option<u64>) -> Option<Duration> {
        let cap = self.introspection.as_ref()?.max_cache_ttl;
        let exp = exp?;
        let now = SystemTime::now().duration_since(UNIX_EPOCH).ok()?.as_secs();
        let remaining = exp.checked_sub(now)?;
        Some(Duration::from_secs(remaining).min(cap))
    }

    /// Startup validation for the introspection mechanism (a static-only config
    /// needs no endpoint, so this is a no-op then). Fails fast so a
    /// misconfiguration surfaces at boot rather than on the first request.
    ///
    /// - SaaS: the per-org format is the base requirement — a global-only config
    ///   contradicts the per-org isolation model and is rejected.
    /// - Simple: a single endpoint must be resolvable, from an explicit URL or
    ///   the `discovered` endpoint in provider metadata.
    pub(super) fn ensure_serviceable(
        &self,
        discovered: Option<&str>,
    ) -> Result<(), String> {
        let Some(introspection) = &self.introspection else {
            return Ok(());
        };
        if self.is_saas {
            if introspection.org_endpoint_format.is_none() {
                return Err("SaaS API-token introspection requires \
                     OIDC_ORG_TOKEN_INTROSPECTION_URL_FORMAT (the per-organisation \
                     introspection endpoint); a global-only configuration is not \
                     valid for a per-org (SaaS) setup"
                    .to_string());
            }
        } else if introspection.global_endpoint.is_none() && discovered.is_none() {
            return Err(
                "API-token introspection is enabled (OIDC_API_TOKEN_PREFIX + \
                 OIDC_INTROSPECTION_AUTH_HEADER are set) but no introspection \
                 endpoint is configured or discoverable — set \
                 OIDC_TOKEN_INTROSPECTION_URL, or use an IdP that advertises \
                 introspection_endpoint in its metadata"
                    .to_string(),
            );
        }
        Ok(())
    }

    /// Returns the principal for a static token matching `api_key` within
    /// `scope`, or `None` if no static token matches. Checked before
    /// introspection.
    pub(super) fn match_static_token(
        &self,
        scope: Option<&str>,
        api_key: &str,
    ) -> Option<User> {
        self.static_tokens
            .iter()
            .find(|t| t.matches(self.is_saas, scope, api_key))
            .map(StaticToken::to_user)
    }

    /// Resolves the introspection endpoint for a request scope. Returns `None`
    /// when introspection is disabled, or when nothing can serve the scope.
    /// - `Some(org)` substitutes `<organisation>` in the per-org format (SaaS).
    /// - `None` (global / Simple single realm): the explicit `global_endpoint`
    ///   if set, otherwise the `discovered` endpoint from provider metadata.
    pub(super) fn resolve_endpoint(
        &self,
        org_id: Option<&str>,
        discovered: Option<String>,
    ) -> Option<String> {
        let introspection = self.introspection.as_ref()?;
        match org_id {
            Some(org_id) => introspection
                .org_endpoint_format
                .as_ref()
                .map(|fmt| fmt.replace("<organisation>", org_id)),
            None => introspection.global_endpoint.clone().or(discovered),
        }
    }

    /// Introspects `api_key` against `endpoint`, returning the resolved
    /// principal and the token's `exp` (seconds since the Unix epoch, if any).
    pub(super) async fn introspect(
        &self,
        endpoint: &str,
        api_key: &str,
    ) -> Result<(User, Option<u64>), IntrospectError> {
        let introspection = self.introspection.as_ref().ok_or_else(|| {
            IntrospectError::Unreachable("introspection is not configured".to_string())
        })?;

        let response = HTTP_CLIENT
            .post(endpoint)
            .header(reqwest::header::AUTHORIZATION, &introspection.auth_header)
            .form(&[("token", api_key)])
            .send()
            .await
            .map_err(|e| IntrospectError::Unreachable(format!("request failed: {e}")))?;

        if !response.status().is_success() {
            return Err(IntrospectError::Unreachable(format!(
                "introspection endpoint returned status {}",
                response.status()
            )));
        }

        let body: IntrospectionResponse = response.json().await.map_err(|e| {
            IntrospectError::Unreachable(format!(
                "could not parse introspection response: {e}"
            ))
        })?;

        if !body.active {
            return Err(IntrospectError::Inactive);
        }

        let exp = body.exp;
        let user = body.into_user().ok_or(IntrospectError::Inactive)?;
        Ok((user, exp))
    }
}

/// Parses the `OIDC_API_STATIC_TOKENS` JSON array. An empty/blank value yields
/// an empty list (mechanism off); invalid JSON is an error (fails startup).
fn parse_static_tokens(raw: &str) -> Result<Vec<StaticToken>, String> {
    let raw = raw.trim();
    if raw.is_empty() {
        return Ok(Vec::new());
    }
    serde_json::from_str::<Vec<StaticToken>>(raw).map_err(|e| {
        format!(
            "OIDC_API_STATIC_TOKENS must be a JSON array of \
             {{\"token\", \"principal\"[, \"email\", \"org\"]}}: {e}"
        )
    })
}

/// The subset of the RFC 7662 introspection response we consume. All fields are
/// standard; unknown fields are ignored.
#[derive(Deserialize)]
struct IntrospectionResponse {
    /// The only REQUIRED field per RFC 7662 §2.2.
    active: bool,
    username: Option<String>,
    sub: Option<String>,
    email: Option<String>,
    /// Expiry, seconds since the Unix epoch.
    exp: Option<u64>,
}

impl IntrospectionResponse {
    /// Maps standard introspection claims onto a Superposition [`User`].
    /// Identity falls back across `username` -> `sub` -> `email`; returns `None`
    /// when the active token carries no usable identity claim.
    fn into_user(self) -> Option<User> {
        let username = self.username.or(self.sub).or_else(|| self.email.clone())?;
        let email = self.email.unwrap_or_else(|| username.clone());
        Some(User::new(email, username))
    }
}

/// Failure of an introspection call, mapped to distinct HTTP statuses so a
/// caller can tell "your token is bad" (401) from "we couldn't check" (503).
pub(super) enum IntrospectError {
    /// `active:false`, or an active token with no usable identity claim.
    Inactive,
    /// The endpoint was unreachable, errored, or returned an unparseable body.
    Unreachable(String),
}

impl From<IntrospectError> for actix_web::Error {
    fn from(err: IntrospectError) -> Self {
        match err {
            IntrospectError::Inactive => {
                ErrorUnauthorized("Invalid or inactive API token")
            }
            IntrospectError::Unreachable(detail) => {
                log::error!("Token introspection failed: {detail}");
                ErrorServiceUnavailable("Token introspection unavailable")
            }
        }
    }
}
