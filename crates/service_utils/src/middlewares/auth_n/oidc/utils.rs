use std::time::Duration;

use actix_web::error::{
    ErrorInternalServerError, ErrorNotImplemented, ErrorUnauthorized,
};
use openidconnect::{
    self as oidcrs, AdditionalClaims, AdditionalProviderMetadata, ClientId, ClientSecret,
    GenderClaim, IdTokenClaims, IntrospectionUrl, IssuerUrl, Nonce, OAuth2TokenResponse,
    ProviderMetadata, RedirectUrl, RequestTokenError, ResourceOwnerPassword,
    ResourceOwnerUsername, Scope, TokenResponse,
    core::{
        CoreAuthDisplay, CoreClaimName, CoreClaimType, CoreClient, CoreClientAuthMethod,
        CoreErrorResponseType, CoreGrantType, CoreJsonWebKey, CoreJsonWebKeyType,
        CoreJsonWebKeyUse, CoreJweContentEncryptionAlgorithm,
        CoreJweKeyManagementAlgorithm, CoreJwsSigningAlgorithm, CoreResponseMode,
        CoreResponseType, CoreSubjectIdentifierType,
    },
};
use serde::{Deserialize, Serialize};
use superposition_types::User;

/// Additional provider-metadata beyond OIDC Core discovery: the RFC 7662
/// introspection endpoint (defined by RFC 8414). It is OPTIONAL — a provider may
/// support introspection without advertising it — hence `Option`.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub(super) struct IntrospectionMetadata {
    #[serde(default)]
    pub(super) introspection_endpoint: Option<IntrospectionUrl>,
}

impl AdditionalProviderMetadata for IntrospectionMetadata {}

/// OIDC provider metadata carrying the introspection endpoint alongside the
/// standard Core fields. Mirrors `CoreProviderMetadata` with our additional
/// metadata swapped in for the empty default.
pub(super) type OidcProviderMetadata = ProviderMetadata<
    IntrospectionMetadata,
    CoreAuthDisplay,
    CoreClientAuthMethod,
    CoreClaimName,
    CoreClaimType,
    CoreGrantType,
    CoreJweContentEncryptionAlgorithm,
    CoreJweKeyManagementAlgorithm,
    CoreJwsSigningAlgorithm,
    CoreJsonWebKeyType,
    CoreJsonWebKeyUse,
    CoreJsonWebKey,
    CoreResponseMode,
    CoreResponseType,
    CoreSubjectIdentifierType,
>;

/// The introspection endpoint advertised in the provider's discovery metadata
/// (RFC 8414 `introspection_endpoint`), as a plain string, if present. Keeps the
/// `openidconnect` metadata-digging in one place so callers (and the
/// introspection config) deal only in URLs.
pub(super) fn discovered_introspection_endpoint(
    metadata: &OidcProviderMetadata,
) -> Option<String> {
    metadata
        .additional_metadata()
        .introspection_endpoint
        .as_ref()
        .map(|url| url.url().as_str().to_string())
}

/// Classifies a failure of the Basic-auth (ROPC / password grant) exchange so
/// callers can surface an accurate HTTP status instead of a blanket 500.
#[derive(Debug)]
pub(super) enum BasicAuthError {
    /// The identity provider does not support the password grant (e.g. Google
    /// rejects it with `unsupported_grant_type`). Basic auth cannot work here.
    Unsupported,
    /// The supplied username/password was rejected by the identity provider.
    InvalidCredentials,
    /// The exchanged ID token could not be verified. The most common cause is
    /// stale JWKS: the IdP rotated its signing keys since the provider metadata
    /// was last fetched. Callers can retry after refreshing the metadata. Detail
    /// is for logs, not the client.
    TokenVerification(String),
    /// Any other failure: network error, misconfiguration, or missing id-token.
    /// Detail is for logs, not the client.
    Internal(String),
}

impl std::fmt::Display for BasicAuthError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unsupported => {
                write!(f, "password grant not supported by identity provider")
            }
            Self::InvalidCredentials => write!(f, "invalid username or password"),
            Self::TokenVerification(detail) => write!(f, "{detail}"),
            Self::Internal(detail) => write!(f, "{detail}"),
        }
    }
}

impl From<BasicAuthError> for actix_web::Error {
    fn from(err: BasicAuthError) -> Self {
        match err {
            BasicAuthError::Unsupported => ErrorNotImplemented(
                "Basic auth is not supported by the configured identity provider",
            ),
            BasicAuthError::InvalidCredentials => {
                ErrorUnauthorized("Invalid username or password")
            }
            BasicAuthError::TokenVerification(_) | BasicAuthError::Internal(_) => {
                ErrorInternalServerError("Failed to authenticate user")
            }
        }
    }
}

pub(super) fn verify_presence(n: Option<&Nonce>) -> Result<(), String> {
    if n.is_some() {
        Ok(())
    } else {
        Err("missing nonce claim".to_string())
    }
}

pub(super) fn presence_no_check(_: Option<&Nonce>) -> Result<(), String> {
    Ok(())
}

pub(super) fn try_user_from<A: AdditionalClaims, B: GenderClaim>(
    claims: &IdTokenClaims<A, B>,
) -> Result<User, String> {
    let email = claims
        .email()
        .ok_or(String::from("Email not found"))?
        .to_string();
    let username = claims
        .preferred_username()
        .map(|u| u.to_string())
        .or_else(|| claims.email().map(|e| e.to_string()))
        .ok_or(String::from("Username not found"))?;

    Ok(User::new(email, username))
}

pub(super) async fn fetch_provider_metadata(
    issuer_url: IssuerUrl,
) -> Result<OidcProviderMetadata, Box<dyn std::error::Error>> {
    let provider_metadata = OidcProviderMetadata::discover_async(
        issuer_url,
        oidcrs::reqwest::async_http_client,
    )
    .await?;

    Ok(provider_metadata)
}

pub(super) fn build_client(
    provider_metadata: OidcProviderMetadata,
    client_id: ClientId,
    client_secret: ClientSecret,
    redirect_url: RedirectUrl,
) -> CoreClient {
    CoreClient::from_provider_metadata(provider_metadata, client_id, Some(client_secret))
        .set_redirect_uri(redirect_url)
}

/// Maps a standard OAuth token-endpoint error code onto a classified
/// [`BasicAuthError`]. Shared by the password (ROPC) and client-credentials
/// exchanges: `invalid_grant` (bad user creds) and `invalid_client` /
/// `unauthorized_client` (bad/disallowed client creds) both read as
/// "invalid credentials", while `unsupported_grant_type` means the IdP does
/// not offer the grant at all.
fn status_for_token_error_code(code: &CoreErrorResponseType) -> BasicAuthError {
    match code {
        CoreErrorResponseType::UnsupportedGrantType => BasicAuthError::Unsupported,
        CoreErrorResponseType::InvalidGrant
        | CoreErrorResponseType::InvalidClient
        | CoreErrorResponseType::UnauthorizedClient => BasicAuthError::InvalidCredentials,
        other => BasicAuthError::Internal(format!("token endpoint error: {other:?}")),
    }
}

pub(super) async fn exchange_password_for_id_token(
    client: &CoreClient,
    username: &ResourceOwnerUsername,
    password: &ResourceOwnerPassword,
) -> Result<(String, Option<Duration>), BasicAuthError> {
    let token_response = client
        .exchange_password(username, password)
        .add_scope(Scope::new("openid".to_string()))
        .add_scope(Scope::new("email".to_string()))
        .add_scope(Scope::new("profile".to_string()))
        .request_async(oidcrs::reqwest::async_http_client)
        .await
        .map_err(|e| match &e {
            RequestTokenError::ServerResponse(resp) => {
                status_for_token_error_code(resp.error())
            }
            other => BasicAuthError::Internal(format!(
                "Failed to exchange password for token: {other:?}"
            )),
        })?;

    let expires_in = token_response.expires_in();
    let id_token = token_response
        .id_token()
        .ok_or_else(|| BasicAuthError::Internal("No identity-token!".to_string()))?;
    id_token
        .claims(&client.id_token_verifier(), presence_no_check)
        .map_err(|e| {
            BasicAuthError::TokenVerification(format!("Couldn't verify claims: {e:?}"))
        })?;

    Ok((id_token.to_string(), expires_in))
}

/// Validates a machine's client_id/client_secret against the IdP using the
/// `client_credentials` grant. The `client` must already be configured with the
/// machine's credentials (see each authenticator's `machine_client`). A
/// successful exchange proves the credentials are valid; the returned token is
/// not needed since the principal is derived from the client_id. Returns the
/// token's `expires_in` (if provided) so the caller can bound its cache entry.
pub(super) async fn validate_client_credentials(
    client: &CoreClient,
) -> Result<Option<Duration>, BasicAuthError> {
    client
        .exchange_client_credentials()
        .request_async(oidcrs::reqwest::async_http_client)
        .await
        .map_err(|e| match &e {
            RequestTokenError::ServerResponse(resp) => {
                status_for_token_error_code(resp.error())
            }
            other => BasicAuthError::Internal(format!(
                "Failed to exchange client credentials: {other:?}"
            )),
        })
        .map(|t| t.expires_in())
}
