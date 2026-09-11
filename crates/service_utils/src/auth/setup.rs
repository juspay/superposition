//! Assembling superposition's authentication stack.
//!
//! Replaces `AuthNHandler::init`. The differences are structural: the provider
//! is parsed into a typed value rather than `split('+')` plus `unwrap`, every
//! failure is returned rather than panicked, and the mechanisms are chain
//! elements instead of methods on one authenticator.

use std::{collections::HashSet, sync::Arc};

use authn_kit::{
    AuthGateway, AuthnBuilder,
    adapters::actix::ActixAuthn,
    mechanisms::{
        ApiTokenAuthenticator, BasicAuthenticator, BearerAuthenticator,
        DisabledAuthenticator, IntrospectionValidator, SessionAuthenticator,
    },
    oidc::{CookieSettings, LoginFlow, OidcConfig, OidcProvider},
};
use superposition_types::{DispatchUser, InternalUser, User};

use crate::{
    auth::{
        dispatcher::DispatcherAuthenticator,
        internal::InternalAuthenticator,
        principal::{PrincipalKind, SuperpositionProfile},
        public::PublicScopeAuthenticator,
        routes::AuthRoutesState,
        scope::{SuperpositionScope, SuperpositionScopes},
    },
    db::utils::{
        get_introspection_auth_header, get_oidc_client_secret, get_static_api_tokens,
    },
    helpers::get_from_env_unsafe,
    kms::SecretProviderClient,
    service::types::AppEnv,
};

/// The names the original used, which existing browsers already hold. Changing
/// either would sign every user out on deploy.
const PROTECTION_COOKIE: &str = "protection";

/// Which authenticator `AUTH_PROVIDER` selects.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AuthProvider {
    Disabled,
    Oidc {
        issuer_url: String,
    },
    /// Still served by the legacy middleware; `authn_kit` has no per-organisation
    /// issuer support.
    LegacySaas,
}

impl AuthProvider {
    /// Parses `AUTH_PROVIDER`, whose form is `<KIND>` or `<KIND>+<issuer-url>`.
    ///
    /// The original did `split('+')` then `auth.next().unwrap()`, so a malformed
    /// value panicked with a backtrace rather than a message.
    pub fn parse(raw: &str) -> Result<Self, String> {
        let (kind, issuer) = match raw.split_once('+') {
            Some((kind, issuer)) => (kind, Some(issuer)),
            None => (raw, None),
        };

        match kind.trim() {
            "DISABLED" => Ok(Self::Disabled),
            "OIDC_SAAS" => Ok(Self::LegacySaas),
            "OIDC" => issuer
                .filter(|issuer| !issuer.trim().is_empty())
                .map(|issuer| Self::Oidc {
                    issuer_url: issuer.trim().to_string(),
                })
                .ok_or_else(|| {
                    String::from(
                        "AUTH_PROVIDER=OIDC requires an issuer url, as \
                         `OIDC+https://issuer.example.com/realms/users`",
                    )
                }),
            other => Err(format!(
                "unknown AUTH_PROVIDER {other:?}; expected DISABLED, \
                 OIDC+<issuer-url> or OIDC_SAAS+<issuer-url>"
            )),
        }
    }

    pub fn from_env() -> Result<Self, String> {
        let raw: String = get_from_env_unsafe("AUTH_PROVIDER")
            .map_err(|e| format!("AUTH_PROVIDER is not set: {e}"))?;
        Self::parse(&raw)
    }

    pub fn is_legacy_saas(&self) -> bool {
        matches!(self, Self::LegacySaas)
    }
}

/// The assembled stack: a middleware plus the state its routes need.
pub struct SuperpositionAuthn {
    pub middleware: ActixAuthn<SuperpositionProfile, SuperpositionScopes>,
    pub routes: AuthRoutesState,
}

/// Everything the stack needs that is not read from the environment.
pub struct AuthnSettings {
    pub path_prefix: String,
    pub exclusions: HashSet<String>,
    /// Validates the `Internal` service-to-service scheme.
    pub superposition_token: String,
    /// Validates the Kronos webhook callback credential.
    pub dispatch_token: String,
    /// `false` only for local development over plain HTTP, where a `Secure`
    /// cookie is never sent back. The original hard-coded `true`.
    pub secure_cookies: bool,
}

/// Builds the authentication stack for a non-SaaS provider.
///
/// Returns `Err` rather than panicking on any misconfiguration, so a bad value
/// surfaces as a startup message naming the problem.
pub async fn build(
    provider: &AuthProvider,
    settings: AuthnSettings,
    kms_client: &Option<SecretProviderClient>,
    app_env: &AppEnv,
) -> Result<SuperpositionAuthn, String> {
    let scopes =
        SuperpositionScopes::new(settings.exclusions, settings.path_prefix.clone());

    let session_cookies = |name: String| {
        CookieSettings::session(name)
            .with_path(cookie_path(&settings.path_prefix))
            .with_secure(settings.secure_cookies)
    };

    let (gateway, routes) = match provider {
        AuthProvider::LegacySaas => {
            return Err(String::from(
                "OIDC_SAAS is served by the legacy middleware and must not be \
                 built here",
            ));
        }

        AuthProvider::Disabled => {
            // The original's `DisabledAuthenticator` returned `User::default()`
            // for every request, on every scope.
            let gateway = AuthnBuilder::<SuperpositionProfile, _>::new(scopes)
                .with(PublicScopeAuthenticator::with_default_identity())
                .with(DisabledAuthenticator::new(default_identity_claims()))
                .build()
                .map_err(|e| e.to_string())?;

            let routes = AuthRoutesState {
                // Nothing to log in to.
                login: None,
                provider: None,
                path_prefix: settings.path_prefix.clone(),
            };
            (gateway, routes)
        }

        AuthProvider::Oidc { issuer_url } => {
            let redirect_url =
                format!("{}{}/oidc/login", redirect_host()?, settings.path_prefix);
            let client_id: String = get_from_env_unsafe("OIDC_CLIENT_ID")
                .map_err(|e| format!("OIDC_CLIENT_ID is not set: {e}"))?;
            let client_secret = get_oidc_client_secret(kms_client, app_env).await;

            let config = OidcConfig::new(issuer_url.clone(), client_id, redirect_url)
                .map_err(|e| e.to_string())?
                .with_client_secret(client_secret);

            let provider = Arc::new(
                OidcProvider::discover(config)
                    .await
                    .map_err(|e| format!("OIDC discovery failed: {e}"))?,
            );

            let login = Arc::new(
                LoginFlow::new(provider.clone())
                    .with_session_cookie(session_cookies(
                        SuperpositionScope::Global.to_string(),
                    ))
                    .with_protection_cookie(
                        CookieSettings::protection(PROTECTION_COOKIE)
                            .with_path(cookie_path(&settings.path_prefix))
                            .with_secure(settings.secure_cookies),
                    ),
            );

            let api_tokens =
                api_token_authenticator(kms_client, app_env, provider.clone()).await?;

            let mut builder = AuthnBuilder::<SuperpositionProfile, _>::new(scopes)
                // First: an excluded route short-circuits before any credential
                // is examined, as the original's `Login::None` branches did.
                .with(PublicScopeAuthenticator::with_default_identity())
                .with(InternalAuthenticator::new(settings.superposition_token))
                .with(DispatcherAuthenticator::new(settings.dispatch_token));

            if let Some(api_tokens) = api_tokens {
                builder = builder.with(api_tokens);
            }

            let gateway = builder
                .with(BearerAuthenticator::new(provider.clone()))
                // Parity: the original accepted the password grant.
                .with(BasicAuthenticator::new(provider.clone()).with_password_grant(true))
                // Last: the only mechanism that turns an *absent* credential
                // into a response.
                .with(
                    SessionAuthenticator::new(provider.clone())
                        .with_login_redirect(login.clone()),
                )
                .build()
                .map_err(|e| e.to_string())?;

            let routes = AuthRoutesState {
                login: Some(login),
                provider: Some(provider),
                path_prefix: settings.path_prefix.clone(),
            };
            (gateway, routes)
        }
    };

    Ok(SuperpositionAuthn {
        middleware: with_extensions(gateway),
        routes,
    })
}

/// Wires the principal into the request extensions the rest of the service
/// already reads.
///
/// `auth_z` bypasses authorization for internal and dispatch callers, and
/// `InternalUserContext` is an extractor in several handlers. The original set
/// these markers from inside the authenticator; here they travel on the
/// principal and are unpacked once, here.
fn with_extensions(
    gateway: AuthGateway<SuperpositionProfile, SuperpositionScopes>,
) -> ActixAuthn<SuperpositionProfile, SuperpositionScopes> {
    ActixAuthn::new(gateway).with_extensions(|principal, extensions| {
        extensions.insert(principal.user.clone());
        match principal.kind {
            PrincipalKind::Internal => {
                extensions.insert(InternalUser);
            }
            PrincipalKind::Dispatch => {
                extensions.insert(DispatchUser);
            }
            PrincipalKind::Standard => {}
        }
    })
}

/// The API-token flow, when configured. `None` disables it entirely, matching
/// the original's `ApiTokenConfig::from_env` returning `Ok(None)`.
async fn api_token_authenticator(
    kms_client: &Option<SecretProviderClient>,
    app_env: &AppEnv,
    provider: Arc<OidcProvider>,
) -> Result<Option<ApiTokenAuthenticator<SuperpositionProfile>>, String> {
    let Ok(prefix) = get_from_env_unsafe::<String>("OIDC_API_TOKEN_PREFIX") else {
        return Ok(None);
    };
    if prefix.is_empty() {
        return Ok(None);
    }
    let delimiter: String =
        crate::helpers::get_from_env_or_default("OIDC_API_TOKEN_DELIMITER", "_".into());

    let static_tokens = get_static_api_tokens(kms_client, app_env)
        .await
        .unwrap_or_default();
    let introspection_header = get_introspection_auth_header(kms_client, app_env).await;

    let mut authenticator =
        ApiTokenAuthenticator::from_json(format!("{prefix}{delimiter}"), &static_tokens)
            .map_err(|e| e.to_string())?;
    let static_count = authenticator.token_count();
    let mut has_fallback = false;

    if let Some(header) = introspection_header {
        let endpoint = get_from_env_unsafe::<String>("OIDC_TOKEN_INTROSPECTION_URL")
            .ok()
            .or_else(|| provider.snapshot().introspection_endpoint())
            .ok_or_else(|| {
                String::from(
                    "API-token introspection is enabled but no endpoint is \
                     configured or advertised: set OIDC_TOKEN_INTROSPECTION_URL, \
                     or use a provider advertising introspection_endpoint",
                )
            })?;
        authenticator =
            authenticator.with_fallback(Arc::new(IntrospectionValidator::with_client(
                endpoint,
                header,
                provider.http_client().clone(),
            )));
        has_fallback = true;
    }

    // A prefix alone validates nothing.
    if static_count == 0 && !has_fallback {
        log::warn!(
            "OIDC_API_TOKEN_PREFIX is set but neither static tokens nor \
             introspection are configured; API-token authentication is disabled"
        );
        return Ok(None);
    }

    Ok(Some(authenticator))
}

fn default_identity_claims() -> authn_kit::IdentityClaims {
    let default = User::default();
    let value = serde_json::to_value(&default).unwrap_or_default();
    let email = value["email"].as_str().unwrap_or_default().to_string();
    let username = value["username"].as_str().unwrap_or_default().to_string();

    authn_kit::IdentityClaims::new(authn_kit::ClaimSource::IdToken)
        .with_subject(username.clone())
        .with_preferred_username(username)
        .with_email(email)
}

fn cookie_path(path_prefix: &str) -> String {
    if path_prefix.is_empty() {
        String::from("/")
    } else {
        path_prefix.to_string()
    }
}

fn redirect_host() -> Result<String, String> {
    get_from_env_unsafe::<String>("OIDC_REDIRECT_HOST")
        .map_err(|e| format!("OIDC_REDIRECT_HOST is not set: {e}"))
}
