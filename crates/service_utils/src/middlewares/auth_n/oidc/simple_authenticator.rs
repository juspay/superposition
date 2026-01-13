use std::sync::Arc;

use actix_web::{
    HttpRequest, HttpResponse,
    error::{ErrorBadRequest, ErrorInternalServerError},
    web::{self, Data, get, resource},
};
use derive_more::{Deref, DerefMut};
use futures_util::future::LocalBoxFuture;
use openidconnect::{
    self as oidcrs, ClientId, ClientSecret, IssuerUrl, RedirectUrl, TokenResponse,
    core::{CoreClient, CoreIdTokenClaims, CoreProviderMetadata, CoreTokenResponse},
};
use superposition_types::User;

use crate::middlewares::auth_n::{
    authentication::{Authenticator, Login},
    helpers::fetch_org_ids_from_db,
    oidc::{
        OIDCAuthenticator,
        utils::{try_user_from, verify_presence},
    },
};

#[derive(Clone)]
pub struct AuthenticatorInner {
    client: CoreClient,
    path_prefix: String,
}

/// A simple OIDC Authenticator implementation that uses a single
/// OpenID Provider for authentication, no org specific issuers
///
/// Env(s) needed for Simple OIDC Authenticator:
/// OIDC_CLIENT_ID, OIDC_CLIENT_SECRET, OIDC_REDIRECT_HOST
#[derive(Deref, DerefMut, Clone)]
pub struct SimpleOIDCAuthenticator(Arc<AuthenticatorInner>);

impl SimpleOIDCAuthenticator {
    pub async fn new(
        idp_url: String,
        base_url: String,
        path_prefix: String,
        client_id: String,
        client_secret: String,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let issuer_url = IssuerUrl::new(idp_url)
            .map_err(|e| format!("Unable to create issuer url: {}", e))
            .unwrap();

        // Discover OpenID Provider metadata
        let provider_metadata = CoreProviderMetadata::discover_async(
            issuer_url,
            oidcrs::reqwest::async_http_client,
        )
        .await?;

        // Create client
        let client = CoreClient::from_provider_metadata(
            provider_metadata.clone(),
            ClientId::new(client_id.clone()),
            Some(ClientSecret::new(client_secret.clone())),
        )
        .set_redirect_uri(RedirectUrl::new(format!(
            "{base_url}{path_prefix}/oidc/login"
        ))?);

        Ok(Self(Arc::new(AuthenticatorInner {
            client,
            path_prefix,
        })))
    }

    fn decode_global_token(&self, cookie: &str) -> Result<CoreIdTokenClaims, String> {
        let ctr = serde_json::from_str::<CoreTokenResponse>(cookie)
            .map_err(|e| format!("Error while decoding token: {e}"))?;
        ctr.id_token()
            .ok_or(String::from("Id Token not found"))?
            .claims(&self.client.id_token_verifier(), verify_presence)
            .map_err(|e| format!("Error in claims verification: {e}"))
            .cloned()
    }
}

impl OIDCAuthenticator for SimpleOIDCAuthenticator {
    fn get_client(&self) -> &CoreClient {
        &self.client
    }

    fn get_global_user(
        &self,
        request: &HttpRequest,
        path: String,
    ) -> Result<User, HttpResponse> {
        let token = request.cookie(&Login::Global.to_string()).and_then(|c| {
            self.decode_global_token(c.value())
                .map_err(|e| log::error!("Error in decoding user : {e}"))
                .ok()
        });
        if let Some(token_response) = token {
            Ok(try_user_from(&token_response).map_err(|e| {
                log::error!("Unable to get user: {e}");
                ErrorBadRequest(String::from("Unable to get user"))
            })?)
        } else {
            log::error!("Error user not found in cookies");
            Err(self.new_redirect(&Login::Global, path))
        }
    }
}

impl Authenticator for SimpleOIDCAuthenticator {
    fn get_path_prefix(&self) -> String {
        self.path_prefix.clone()
    }

    fn authenticate(
        &self,
        request: &HttpRequest,
        login_type: &Login,
    ) -> LocalBoxFuture<'static, Result<User, HttpResponse>> {
        let auth_n = self.clone();
        match login_type {
            Login::None => Box::pin(async { Ok(User::default()) }),
            Login::Global => {
                let resp = auth_n.get_global_user(
                    request,
                    format!("{}/admin/organisations", self.path_prefix),
                );
                Box::pin(async { resp })
            }
            Login::Org(_) => {
                let resp = auth_n.get_global_user(request, request.path().to_string());
                Box::pin(async { resp })
            }
        }
    }

    fn routes(&self) -> actix_web::Scope {
        web::scope("oidc")
            .app_data(Data::new(self.to_owned()))
            .service(resource("login").route(get().to(Self::login)))
    }

    fn get_organisations(&self, req: &actix_web::HttpRequest) -> HttpResponse {
        match fetch_org_ids_from_db(req) {
            Ok(resp) => HttpResponse::Ok().json(resp),
            Err(resp) => ErrorInternalServerError(resp).into(),
        }
    }

    fn generate_org_user(
        &self,
        req: &HttpRequest,
        _: &str,
        login_type: &Login,
    ) -> LocalBoxFuture<'_, Result<String, HttpResponse>> {
        let user = req
            .cookie(&Login::Global.to_string())
            .and_then(|user_cookie| {
                self.decode_global_token(user_cookie.value())
                    .map_err(|e| log::error!("Error in decoding user : {e}"))
                    .map(|_| user_cookie.value().to_string())
                    .ok()
            });

        match user {
            Some(u) => Box::pin(async { Ok(u) }),
            None => {
                let redirect = self.new_redirect(
                    login_type,
                    format!("{}/admin/organisations", self.path_prefix),
                );
                Box::pin(async { Err(redirect) })
            }
        }
    }
}
