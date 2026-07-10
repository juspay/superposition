use std::sync::{Arc, RwLock};

use actix_web::{
    HttpRequest, HttpResponse,
    error::{ErrorBadRequest, ErrorInternalServerError, ErrorUnauthorized},
    web::{self, Data, get, resource},
};
use derive_more::{Deref, DerefMut};
use futures_util::future::LocalBoxFuture;
use openidconnect::{
    ClientId, ClientSecret, IssuerUrl, RedirectUrl,
    core::{CoreClient, CoreIdToken, CoreIdTokenClaims},
};
use superposition_types::User;

use crate::middlewares::auth_n::{
    authentication::{Authenticator, Login},
    helpers::fetch_org_ids_from_db,
    oidc::{
        OIDCAuthenticator,
        utils::{build_client, fetch_provider_metadata, try_user_from, verify_presence},
    },
};

pub struct AuthenticatorInner {
    client: RwLock<CoreClient>,
    issuer_url: IssuerUrl,
    client_id: ClientId,
    client_secret: ClientSecret,
    redirect_url: RedirectUrl,
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
        let redirect_url =
            RedirectUrl::new(format!("{base_url}{path_prefix}/oidc/login"))?;
        let client_id = ClientId::new(client_id);
        let client_secret = ClientSecret::new(client_secret);

        let provider_metadata = fetch_provider_metadata(issuer_url.clone()).await?;

        let client = build_client(
            provider_metadata,
            client_id.clone(),
            client_secret.clone(),
            redirect_url.clone(),
        );

        Ok(Self(Arc::new(AuthenticatorInner {
            client: RwLock::new(client),
            issuer_url,
            client_id,
            client_secret,
            redirect_url,
            path_prefix,
        })))
    }

    fn decode_global_token(&self, cookie: &str) -> Result<CoreIdTokenClaims, String> {
        let client = self.get_client();
        let ctr = cookie
            .parse::<CoreIdToken>()
            .map_err(|e| format!("Error while decoding token: {e}"))?;
        ctr.claims(&client.id_token_verifier(), verify_presence)
            .map_err(|e| format!("Error in claims verification: {e}"))
            .cloned()
    }
}

impl OIDCAuthenticator for SimpleOIDCAuthenticator {
    fn get_client(&self) -> CoreClient {
        self.client
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .clone()
    }

    async fn refresh_client(&self) -> Result<(), String> {
        let provider_metadata = fetch_provider_metadata(self.issuer_url.clone())
            .await
            .map_err(|e| format!("Failed to refresh provider metadata: {e:?}"))?;
        let client = build_client(
            provider_metadata,
            self.client_id.clone(),
            self.client_secret.clone(),
            self.redirect_url.clone(),
        );

        *self.client.write().unwrap_or_else(|e| e.into_inner()) = client;
        Ok(())
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

    fn authenticate_with_token(
        &self,
        login_type: &Login,
        token: &str,
    ) -> Result<User, HttpResponse> {
        match login_type {
            Login::None => Ok(User::default()),
            _ => self
                .decode_global_token(token)
                .map_err(|e| {
                    log::error!("Error in decoding user : {e}");
                    ErrorUnauthorized(String::from("Unable to get user"))
                })
                .and_then(|claims| {
                    try_user_from(&claims).map_err(|e| {
                        log::error!("Unable to get user: {e}");
                        ErrorUnauthorized(String::from("Unable to get user"))
                    })
                })
                .map_err(Into::into),
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
