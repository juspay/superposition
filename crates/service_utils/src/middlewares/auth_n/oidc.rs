mod saas_authenticator;
mod simple_authenticator;
mod types;
mod utils;

use actix_web::{
    HttpRequest, HttpResponse,
    cookie::{Cookie, time::Duration},
    error::ErrorInternalServerError,
    http::header,
    web::{Data, Query},
};
use base64::{Engine, engine::general_purpose};
use openidconnect::{
    self as oidcrs, AuthenticationFlow, ClaimsVerificationError, CsrfToken, Nonce,
    TokenResponse,
    core::{CoreClient, CoreIdToken, CoreResponseType, CoreTokenResponse},
};
pub use saas_authenticator::SaasOIDCAuthenticator;
pub use simple_authenticator::SimpleOIDCAuthenticator;
use superposition_types::User;

use crate::middlewares::auth_n::{
    authentication::{Authenticator, Login},
    oidc::types::{LoginParams, ProtectionCookie, RedirectionState},
};

/// Trait defining OIDC specific authenticator methods
/// This is to be implemented by any OIDC based authenticator - SimpleOIDCAuthenticator, SaasOIDCAuthenticator etc.
trait OIDCAuthenticator: Authenticator {
    fn get_client(&self) -> CoreClient;

    /// Re-fetch the OpenID Provider metadata (including the JWKS signing keys)
    /// from the issuer and swap in a freshly-built client. Called when ID token
    /// verification fails, so a long-lived process recovers automatically once
    /// the IdP rotates its signing keys (e.g. Google rotates JWKS every few
    /// hours/days) instead of serving stale keys until the pod is restarted.
    async fn refresh_client(&self) -> Result<(), String>;

    fn get_global_user(
        &self,
        request: &HttpRequest,
        path: String,
    ) -> Result<User, HttpResponse>;

    fn new_redirect(&self, cookie_type: &Login, path: String) -> HttpResponse {
        let state = RedirectionState {
            csrf: CsrfToken::new_random(),
            redirect_uri: path,
        };

        let encoded_state = general_purpose::STANDARD
            .encode(serde_json::to_string(&state).unwrap_or_default());

        let (auth_url, csrf_token, nonce) = self
            .get_client()
            .authorize_url(
                AuthenticationFlow::<CoreResponseType>::AuthorizationCode,
                || CsrfToken::new(encoded_state),
                Nonce::new_random,
            )
            .add_scope(oidcrs::Scope::new("email".to_string()))
            .add_scope(oidcrs::Scope::new("profile".to_string()))
            .url();

        let protection = ProtectionCookie {
            csrf: csrf_token,
            nonce,
        };

        let cookie_result = serde_json::to_string(&protection)
            .map_err(|e| {
                log::error!("Unable to stringify data: {e:?}");
                ErrorInternalServerError("Unable to stringify data".to_string())
            })
            .map(|cookie| {
                Cookie::build("protection", cookie)
                    .max_age(Duration::days(7))
                    .secure(true)
                    // .http_only(true)
                    // .same_site(SameSite::Strict) -- TODO: figure out why this does not work for our case
                    .path(self.get_cookie_path())
                    .finish()
            });

        match cookie_result {
            Ok(p_cookie) => HttpResponse::Found()
                .insert_header((header::LOCATION, auth_url.to_string()))
                .cookie(p_cookie)
                // Deletes the cookie.
                .cookie(
                    Cookie::build(cookie_type.to_string(), "")
                        .max_age(Duration::seconds(0))
                        .secure(true)
                        .finish(),
                )
                .finish(),
            Err(_) => HttpResponse::InternalServerError().finish(),
        }
    }

    async fn login(
        data: Data<Self>,
        req: HttpRequest,
        params: Query<LoginParams>,
    ) -> actix_web::Result<HttpResponse> {
        let login_type = Login::Global;

        let p_cookie = match ProtectionCookie::from_req(&req) {
            Ok(p_cookie) => p_cookie,
            Err(e) => {
                log::error!("OIDC: Missing/Bad protection-cookie, redirecting... {e}");
                return Ok(data.new_redirect(
                    &login_type,
                    format!("{}/admin/organisations", data.get_path_prefix()),
                ));
            }
        };

        if *params.state.csrf.secret() != *p_cookie.csrf.secret() {
            log::error!("OIDC: Bad csrf");
            return Ok(data.new_redirect(
                &login_type,
                format!("{}/admin/organisations", data.get_path_prefix()),
            ));
        }

        // Exchange the code with a token.
        let token_response = data
            .get_client()
            .exchange_code(params.code.clone())
            .request_async(oidcrs::reqwest::async_http_client)
            .await
            .map_err(|e| {
                log::error!("Failed to exchange auth-code for token: {e:?}");
                ErrorInternalServerError(
                    "Failed to exchange auth-code for token".to_string(),
                )
            })?;

        let client = data.get_client();

        // Verify the freshly-minted ID token. If verification fails, the most
        // common cause is that the IdP has rotated its signing keys since we
        // last fetched the JWKS, so the cached keys can't validate the new
        // token's signature. Refresh the provider metadata once and retry
        // before giving up — this self-heals key rotation without a restart.
        let mut response = verify_id_token(&client, &token_response, &p_cookie.nonce);
        if let Err(e) = &response {
            log::error!(
                "OIDC: ID token verification failed; refreshing provider keys and retrying, error: {e:?}"
            );
            match data.refresh_client().await {
                Ok(()) => {
                    response = verify_id_token(&client, &token_response, &p_cookie.nonce);
                }
                Err(e) => {
                    log::error!("OIDC: failed to refresh provider metadata: {e}")
                }
            }
        }

        match response {
            Ok(r) => {
                let cookie = Cookie::build(login_type.to_string(), r.to_string())
                    .path(data.get_cookie_path())
                    .http_only(true)
                    .secure(true)
                    .max_age(Duration::days(1))
                    .finish();
                Ok(HttpResponse::Found()
                    .cookie(cookie)
                    .insert_header((header::LOCATION, params.state.redirect_uri.clone()))
                    .finish())
            }
            // Verification failed even after refreshing keys. Avoid retrying.
            Err(e) => {
                log::error!(
                    "OIDC: ID token verification failed even after refreshing keys: {e:?}"
                );
                Ok(sign_in_failed_response(&format!(
                    "{}/admin/organisations",
                    data.get_path_prefix()
                )))
            }
        }
    }
}

fn verify_id_token<'a>(
    client: &'a CoreClient,
    token_response: &'a CoreTokenResponse,
    nonce: &'a Nonce,
) -> Result<&'a CoreIdToken, ClaimsVerificationError> {
    let id_token = token_response.id_token().ok_or_else(|| {
        ClaimsVerificationError::Other("Id Token not found".to_string())
    })?;
    id_token.claims(&client.id_token_verifier(), nonce)?;
    Ok(id_token)
}

fn sign_in_failed_response(retry_url: &str) -> HttpResponse {
    let body = format!(
        "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>Sign-in failed</title></head>\
         <body style=\"font-family:sans-serif;max-width:32rem;margin:4rem auto;text-align:center\">\
         <h1>Sign-in failed</h1>\
         <p>We couldn't complete your sign-in. This is usually temporary.</p>\
         <p><a href=\"{retry_url}\">Try signing in again</a></p>\
         </body></html>"
    );
    HttpResponse::Unauthorized()
        .content_type("text/html; charset=utf-8")
        .body(body)
}
