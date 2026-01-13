use std::sync::Arc;

use actix_web::{
    HttpRequest, HttpResponse,
    cookie::{Cookie, time::Duration},
    error::{ErrorBadRequest, ErrorInternalServerError},
    http::header,
    web::{self, Data, Json, get, resource},
};
use derive_more::{Deref, DerefMut};
use futures_util::future::LocalBoxFuture;
use openidconnect::{
    self as oidcrs, ClientId, ClientSecret, IssuerUrl, RedirectUrl,
    ResourceOwnerPassword, ResourceOwnerUsername, Scope, TokenResponse, TokenUrl,
    core::{CoreClient, CoreProviderMetadata},
};
use superposition_types::User;

use crate::{
    extensions::HttpRequestExt,
    helpers::get_from_env_unsafe,
    middlewares::auth_n::{
        authentication::{Authenticator, Login},
        oidc::{
            OIDCAuthenticator,
            types::{
                GlobalUserClaims, GlobalUserTokenResponse, OrgUserClaims,
                OrgUserTokenResponse,
            },
            utils::{presence_no_check, try_user_from, verify_presence},
        },
    },
};

#[derive(Clone)]
pub struct AuthenticatorInner {
    client: CoreClient,
    provider_metadata: CoreProviderMetadata,
    client_id: String,
    client_secret: String,
    base_url: String,
    path_prefix: String,
    issuer_endpoint_format: String,
    token_endpoint_format: String,
}

/// An OIDC Authenticator implementation for SaaS setups
/// where each organisation has its own OIDC provider endpoints
/// First issuer also acts as a global identity provider which
/// provides authorization to the individual orgs
///
/// Env(s) needed for OIDC SaaS Authenticator:
/// OIDC_CLIENT_ID, OIDC_CLIENT_SECRET, OIDC_REDIRECT_HOST - reused from Simple OIDC Authenticator
/// OIDC_ORG_TOKEN_ENDPOINT_FORMAT, OIDC_ORG_ISSUER_ENDPOINT_FORMAT - new envs for SaaS setup
#[derive(Deref, DerefMut, Clone)]
pub struct SaasOIDCAuthenticator(Arc<AuthenticatorInner>);

impl SaasOIDCAuthenticator {
    pub async fn new(
        idp_url: String,
        base_url: String,
        path_prefix: String,
        client_id: String,
        client_secret: String,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let issuer_endpoint_format =
            get_from_env_unsafe::<String>("OIDC_ORG_ISSUER_ENDPOINT_FORMAT").unwrap();
        let token_endpoint_format =
            get_from_env_unsafe::<String>("OIDC_ORG_TOKEN_ENDPOINT_FORMAT").unwrap();

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
            provider_metadata,
            client_id,
            client_secret,
            base_url,
            path_prefix,
            issuer_endpoint_format,
            token_endpoint_format,
        })))
    }

    fn get_org_client(&self, org_id: &str) -> Result<CoreClient, String> {
        let issuer_url = match self.get_issuer_url(org_id) {
            Ok(issuer_url) => issuer_url,
            Err(e) => return Err(format!("Unable to create issuer url: {e}")),
        };

        let token_url = match self.get_token_url(org_id) {
            Ok(token_url) => token_url,
            Err(e) => return Err(format!("Unable to create token url: {e}")),
        };

        let redirect_url = match RedirectUrl::new(format!(
            "{}{}/",
            self.base_url.clone(),
            self.path_prefix
        )) {
            Ok(redirect_url) => redirect_url,
            Err(e) => return Err(format!("Unable to create redirect url: {e}")),
        };

        let provider = self
            .provider_metadata
            .clone()
            .set_issuer(issuer_url)
            .set_token_endpoint(Some(token_url));

        Ok(CoreClient::from_provider_metadata(
            provider,
            ClientId::new(self.client_id.clone()),
            Some(ClientSecret::new(self.client_secret.clone())),
        )
        .set_redirect_uri(redirect_url))
    }

    fn get_issuer_url(
        &self,
        organisation_id: &str,
    ) -> Result<IssuerUrl, url::ParseError> {
        let issuer_endpoint = self
            .issuer_endpoint_format
            .replace("<organisation>", organisation_id);
        IssuerUrl::new(issuer_endpoint)
    }

    fn get_token_url(&self, organisation_id: &str) -> Result<TokenUrl, url::ParseError> {
        let token_endpoint = self
            .token_endpoint_format
            .replace("<organisation>", organisation_id);
        TokenUrl::new(token_endpoint)
    }

    fn decode_global_token(&self, cookie: &str) -> Result<GlobalUserClaims, String> {
        let ctr = serde_json::from_str::<GlobalUserTokenResponse>(cookie)
            .map_err(|e| format!("Error while decoding token: {e}"))?;
        ctr.id_token()
            .ok_or(String::from("Id Token not found"))?
            .claims(&self.client.id_token_verifier(), verify_presence)
            .map_err(|e| format!("Error in claims verification: {e}"))
            .cloned()
    }

    fn decode_org_token(
        &self,
        org_id: &str,
        cookie: &str,
    ) -> Result<OrgUserClaims, String> {
        let client = self
            .get_org_client(org_id)
            .map_err(|e| format!("Error in getting Org specific client: {e}"))?;
        let id_token_verifier = client.id_token_verifier();

        let ctr = serde_json::from_str::<OrgUserTokenResponse>(cookie)
            .map_err(|e| format!("Error while decoding token: {e}"))?;
        ctr.id_token()
            .ok_or(String::from("Id Token not found"))?
            .claims(&id_token_verifier, presence_no_check)
            .map_err(|e| format!("Error in claims verification: {e}"))
            .cloned()
    }

    async fn get_org_user(
        self,
        request: HttpRequest,
        login_type: Login,
    ) -> Result<User, HttpResponse> {
        let org_id = request.get_organisation_id().unwrap_or_default();
        let token = request.cookie(&login_type.to_string()).and_then(|c| {
            self.decode_org_token(&org_id, c.value())
                .map_err(|e| log::error!("Error in decoding org_user : {e}"))
                .ok()
        });
        if let Some(token_response) = token {
            Ok(try_user_from(&token_response).map_err(|e| {
                log::error!("Unable to get org_user: {e}");
                ErrorBadRequest(String::from("Unable to get user"))
            })?)
        } else {
            self.generate_org_user(&request, &org_id, &login_type)
                .await
                .and_then(|token| {
                    let cookie = Cookie::build(login_type.to_string(), token)
                        .path(self.get_cookie_path())
                        .http_only(true)
                        .secure(true)
                        .max_age(Duration::days(1))
                        .finish();
                    Err(HttpResponse::Found()
                        .cookie(cookie)
                        .insert_header((header::LOCATION, request.path().to_string()))
                        .finish())
                })
        }
    }
}

impl OIDCAuthenticator for SaasOIDCAuthenticator {
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

impl Authenticator for SaasOIDCAuthenticator {
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
                match auth_n.get_global_user(request, request.path().to_string()) {
                    Err(e) => Box::pin(async { Err(e) }),
                    Ok(_) => {
                        let fut =
                            auth_n.get_org_user(request.clone(), login_type.clone());
                        Box::pin(fut)
                    }
                }
            }
        }
    }

    fn routes(&self) -> actix_web::Scope {
        web::scope("oidc")
            .app_data(Data::new(self.to_owned()))
            .service(resource("login").route(get().to(Self::login)))
    }

    fn get_organisations(&self, req: &HttpRequest) -> HttpResponse {
        let organisations = req
            .cookie(&Login::Global.to_string())
            .and_then(|user_cookie| {
                self.decode_global_token(user_cookie.value())
                    .map_err(|e| log::error!("Error in decoding user : {e}"))
                    .ok()
            })
            .map(|claims| claims.additional_claims().organisations.clone());

        match organisations {
            Some(organisations) => HttpResponse::Ok().json(Json(organisations)),
            None => self.new_redirect(
                &Login::Global,
                format!("{}/admin/organisations", self.path_prefix),
            ),
        }
    }

    fn generate_org_user(
        &self,
        req: &HttpRequest,
        org_id: &str,
        login_type: &Login,
    ) -> LocalBoxFuture<'_, Result<String, HttpResponse>> {
        let client = match self.get_org_client(org_id) {
            Ok(client) => client,
            Err(e) => {
                log::error!("Error in getting Org specific client: {e}");
                return Box::pin(async {
                    Err(ErrorInternalServerError(String::from(
                        "Error in getting Org specific client",
                    ))
                    .into())
                });
            }
        };

        let user = req
            .cookie(&Login::Global.to_string())
            .and_then(|user_cookie| {
                self.decode_global_token(user_cookie.value())
                    .map_err(|e| log::error!("Error in decoding user : {e}"))
                    .ok()
            })
            .map(|claims| {
                (
                    claims.preferred_username().cloned(),
                    claims.additional_claims().switch_pass.clone(),
                )
            });
        let (username, switch_pass) = if let Some(user) = user {
            user
        } else {
            return Box::pin(async { Err(ErrorBadRequest("Cookie incorrect").into()) });
        };

        let username = if let Some(u) = username {
            u
        } else {
            return Box::pin(async { Err(ErrorBadRequest("Username not found").into()) });
        };

        let user = ResourceOwnerUsername::new(username.to_string());
        let pass = ResourceOwnerPassword::new(switch_pass);
        let redirect = self.new_redirect(
            login_type,
            format!("{}/admin/organisations", self.path_prefix),
        );

        Box::pin(async move {
            client
                .exchange_password(&user, &pass)
                .add_scope(Scope::new(String::from("openid")))
                .request_async(oidcrs::reqwest::async_http_client)
                .await
                .map_err(|e| {
                    log::error!("Failed to switch organisation for token: {e}");
                    Some(ErrorInternalServerError(
                        "Failed to switch organisation for token".to_string(),
                    ))
                })
                .and_then(|tr| {
                    tr.id_token()
                        .ok_or_else(|| {
                            log::error!("No identity-token!");
                            None
                        })?
                        .claims(&client.id_token_verifier(), presence_no_check)
                        .map_err(|e| {
                            log::error!("Couldn't verify claims: {e}");
                            None
                        })?;

                    serde_json::to_string(&tr).map_err(|e| {
                        log::error!("Unable to stringify data: {e}");
                        Some(ErrorInternalServerError(
                            "Unable to stringify data".to_string(),
                        ))
                    })
                })
                .map_err(|e| e.map(Into::into).unwrap_or(redirect))
        })
    }
}
