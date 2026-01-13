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
    self as oidcrs, AuthenticationFlow, CsrfToken, Nonce, TokenResponse,
    core::{CoreClient, CoreResponseType},
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
    fn get_client(&self) -> &CoreClient;

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
                log::error!("Unable to stringify data: {e}");
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
                log::error!("Failed to exchange auth-code for token: {e}");
                ErrorInternalServerError(
                    "Failed to exchange auth-code for token".to_string(),
                )
            })?;

        let response = token_response
            .id_token()
            .ok_or_else(|| log::error!("No identity-token!"))
            .and_then(|t| {
                t.claims(&data.get_client().id_token_verifier(), &p_cookie.nonce)
                    .map_err(|e| log::error!("Couldn't verify claims: {e}"))
            })
            .map(|_| token_response.clone());

        match response {
            Ok(r) => {
                let token = serde_json::to_string(&r).map_err(|e| {
                    log::error!("Unable to stringify data: {e}");
                    ErrorInternalServerError("Unable to stringify data".to_string())
                })?;
                let cookie = Cookie::build(login_type.to_string(), token)
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
            Err(()) => Ok(data.new_redirect(
                &login_type,
                format!("{}/admin/organisations", data.get_path_prefix()),
            )),
        }
    }
}
