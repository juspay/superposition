use std::sync::{Arc, RwLock};

use actix_web::{
    HttpRequest, HttpResponse,
    cookie::{Cookie, time::Duration},
    error::{
        ErrorBadRequest, ErrorInternalServerError, ErrorNotImplemented, ErrorUnauthorized,
    },
    http::header,
    web::{self, Data, Json, get, resource},
};
use derive_more::{Deref, DerefMut};
use futures_util::future::LocalBoxFuture;
use openidconnect::{
    self as oidcrs, ClientId, ClientSecret, IssuerUrl, RedirectUrl,
    ResourceOwnerPassword, ResourceOwnerUsername, Scope, TokenResponse, TokenUrl,
    core::{CoreClient, CoreIdToken},
};
use superposition_types::User;

use crate::{
    extensions::HttpRequestExt,
    helpers::get_from_env_unsafe,
    middlewares::auth_n::{
        authentication::{Authenticator, BasicAuthGrant, Login},
        oidc::{
            OIDCAuthenticator,
            api_token::ApiTokenConfig,
            token_cache::{CachedGrant, TokenCacheConfig, TokenExchangeCache},
            types::{GlobalUserClaims, GlobalUserIdToken, OrgUserClaims},
            utils::{
                BasicAuthError, OidcProviderMetadata, build_client,
                discovered_introspection_endpoint, exchange_password_for_id_token,
                fetch_provider_metadata, presence_no_check, try_user_from,
                validate_client_credentials, verify_presence,
            },
        },
    },
};

pub struct AuthenticatorInner {
    client: RwLock<CoreClient>,
    provider_metadata: RwLock<OidcProviderMetadata>,
    issuer_url: IssuerUrl,
    client_id: ClientId,
    client_secret: ClientSecret,
    base_url: String,
    redirect_url: RedirectUrl,
    path_prefix: String,
    issuer_endpoint_format: String,
    token_endpoint_format: String,
    token_cache: TokenExchangeCache,
    api_token: Option<ApiTokenConfig>,
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
        introspection_auth_header: Option<String>,
        static_tokens_raw: Option<String>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let issuer_endpoint_format =
            get_from_env_unsafe::<String>("OIDC_ORG_ISSUER_ENDPOINT_FORMAT").unwrap();
        let token_endpoint_format =
            get_from_env_unsafe::<String>("OIDC_ORG_TOKEN_ENDPOINT_FORMAT").unwrap();

        let issuer_url = IssuerUrl::new(idp_url)
            .map_err(|e| format!("Unable to create issuer url: {}", e))
            .unwrap();
        let redirect_url =
            RedirectUrl::new(format!("{base_url}{path_prefix}/oidc/login"))?;
        let client_id = ClientId::new(client_id);
        let client_secret = ClientSecret::new(client_secret);

        let provider_metadata = fetch_provider_metadata(issuer_url.clone()).await?;

        let client = build_client(
            provider_metadata.clone(),
            client_id.clone(),
            client_secret.clone(),
            redirect_url.clone(),
        );

        let api_token =
            ApiTokenConfig::from_env(introspection_auth_header, static_tokens_raw, true)?;
        if let Some(cfg) = &api_token {
            let discovered = discovered_introspection_endpoint(&provider_metadata);
            cfg.ensure_serviceable(discovered.as_deref())?;
        }

        Ok(Self(Arc::new(AuthenticatorInner {
            client: RwLock::new(client),
            provider_metadata: RwLock::new(provider_metadata),
            issuer_url,
            client_id,
            client_secret,
            base_url,
            redirect_url,
            path_prefix,
            issuer_endpoint_format,
            token_endpoint_format,
            token_cache: TokenExchangeCache::new(TokenCacheConfig::from_env()),
            api_token,
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
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .clone()
            .set_issuer(issuer_url)
            .set_token_endpoint(Some(token_url));

        Ok(build_client(
            provider,
            self.client_id.clone(),
            self.client_secret.clone(),
            redirect_url,
        ))
    }

    /// Builds an OIDC client configured with the *machine's* credentials (from
    /// a `Basic` header) against the **org-specific** endpoints, so a
    /// `client_credentials` exchange authenticates the machine. In a SaaS setup
    /// service accounts are provisioned per organisation, so a
    /// client_credentials request that isn't org-scoped is rejected.
    fn machine_client(
        &self,
        login_type: &Login,
        client_id: ClientId,
        client_secret: ClientSecret,
    ) -> Result<CoreClient, String> {
        let Login::Org(org_id) = login_type else {
            return Err(
                "client_credentials requires an organisation-scoped request".to_string()
            );
        };
        let issuer_url = self
            .get_issuer_url(org_id)
            .map_err(|e| format!("Unable to create issuer url: {e}"))?;
        let token_url = self
            .get_token_url(org_id)
            .map_err(|e| format!("Unable to create token url: {e}"))?;
        let redirect_url =
            RedirectUrl::new(format!("{}{}/", self.base_url, self.path_prefix))
                .map_err(|e| format!("Unable to create redirect url: {e}"))?;
        let provider = self
            .provider_metadata
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .clone()
            .set_issuer(issuer_url)
            .set_token_endpoint(Some(token_url));
        Ok(build_client(
            provider,
            client_id,
            client_secret,
            redirect_url,
        ))
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
        let client = self.get_client();
        let ctr = cookie
            .parse::<GlobalUserIdToken>()
            .map_err(|e| format!("Error while decoding token: {e}"))?;
        ctr.claims(&client.id_token_verifier(), verify_presence)
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

        let ctr = cookie
            .parse::<CoreIdToken>()
            .map_err(|e| format!("Error while decoding token: {e}"))?;
        ctr.claims(&id_token_verifier, presence_no_check)
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

    fn get_specific_client(&self, login_type: &Login) -> actix_web::Result<CoreClient> {
        match login_type {
            Login::Org(org_id) => self.get_org_client(org_id).map_err(|e| {
                log::error!("Error in getting Org specific client: {e}");
                ErrorInternalServerError(String::from(
                    "Error in getting Org specific client",
                ))
            }),
            _ => Ok(self.get_client()),
        }
    }
}

impl OIDCAuthenticator for SaasOIDCAuthenticator {
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
            provider_metadata.clone(),
            self.client_id.clone(),
            self.client_secret.clone(),
            self.redirect_url.clone(),
        );

        // Update the metadata first so org-specific clients derived from it also
        // pick up the fresh keys, then swap in the rebuilt global client.
        // Recover through poisoning: these locks only guard whole-value swaps.
        *self
            .provider_metadata
            .write()
            .unwrap_or_else(|e| e.into_inner()) = provider_metadata;
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

    fn authenticate_with_bearer_token(
        &self,
        login_type: &Login,
        token: &str,
    ) -> Result<User, HttpResponse> {
        match login_type {
            Login::None => Ok(User::default()),
            Login::Global => self
                .decode_global_token(token)
                .map_err(|e| format!("Error in decoding user : {e}"))
                .and_then(|claims| {
                    try_user_from(&claims).map_err(|e| format!("Unable to get user: {e}"))
                })
                .map_err(|e| {
                    log::error!("{e}");
                    ErrorUnauthorized(String::from("Unable to get org_user")).into()
                }),
            Login::Org(org_id) => self
                .decode_org_token(org_id, token)
                .map_err(|e| format!("Error in decoding org_user : {e}"))
                .and_then(|claims| {
                    try_user_from(&claims)
                        .map_err(|e| format!("Unable to get org_user: {e}"))
                })
                .map_err(|e| {
                    log::error!("{e}");
                    ErrorUnauthorized(String::from("Unable to get org_user")).into()
                }),
        }
    }

    fn api_token_prefix(&self) -> Option<String> {
        self.api_token.as_ref().map(|c| c.prefix.clone())
    }

    fn authenticate_with_api_token(
        &self,
        login_type: &Login,
        api_key: &str,
    ) -> LocalBoxFuture<'static, Result<User, HttpResponse>> {
        if matches!(login_type, Login::None) {
            return Box::pin(async { Ok(User::default()) });
        }

        let scope = match login_type {
            Login::Org(org_id) => Some(org_id.clone()),
            _ => None,
        };
        let auth_n = self.clone();
        let api_key = api_key.to_string();
        let key =
            TokenExchangeCache::key(scope.clone(), CachedGrant::ApiToken, "", &api_key);
        Box::pin(async move {
            let Some(config) = auth_n.api_token.as_ref() else {
                return Err(ErrorNotImplemented(
                    "API token authentication is not configured",
                )
                .into());
            };

            if let Some(user) = config.match_static_token(scope.as_deref(), &api_key) {
                return Ok(user);
            }

            if !config.introspection_enabled() {
                return Err(ErrorUnauthorized("Invalid or inactive API token").into());
            }

            if let Some(user) = auth_n.token_cache.get(&key) {
                return Ok(user);
            }

            // For a global request, fall back to the introspection endpoint
            // advertised in the (cached) provider metadata.
            let discovered = discovered_introspection_endpoint(
                &auth_n
                    .provider_metadata
                    .read()
                    .unwrap_or_else(|e| e.into_inner()),
            );
            let Some(endpoint) = config.resolve_endpoint(scope.as_deref(), discovered)
            else {
                return Err(ErrorBadRequest(
                    "API token authentication is not available for global requests \
                     in this configuration",
                )
                .into());
            };

            let (user, exp) = config
                .introspect(&endpoint, &api_key)
                .await
                .map_err(actix_web::Error::from)?;
            auth_n
                .token_cache
                .insert(key, user.clone(), config.cache_ttl(exp));
            Ok(user)
        })
    }

    fn authenticate_with_basic_auth(
        &self,
        login_type: &Login,
        grant: BasicAuthGrant,
    ) -> LocalBoxFuture<'static, Result<User, HttpResponse>> {
        if matches!(login_type, Login::None) {
            return Box::pin(async { Ok(User::default()) });
        }

        // Per-org realms in SaaS: scope the cache key by org so the same
        // credentials can't be reused across org realms.
        let scope = match login_type {
            Login::Org(org_id) => Some(org_id.clone()),
            _ => None,
        };

        match grant {
            BasicAuthGrant::Password { username, password } => {
                let client = match self.get_specific_client(login_type) {
                    Ok(client) => client,
                    Err(e) => return Box::pin(async { Err(e.into()) }),
                };
                let auth_n = self.clone();
                let login_type = login_type.clone();
                let key = TokenExchangeCache::key(
                    scope,
                    CachedGrant::Password,
                    username.as_str(),
                    password.secret(),
                );
                Box::pin(async move {
                    if let Some(user) = auth_n.token_cache.get(&key) {
                        return Ok(user);
                    }

                    let (id_token, expires_in) = match exchange_password_for_id_token(
                        &client, &username, &password,
                    )
                    .await
                    {
                        Err(BasicAuthError::TokenVerification(detail)) => {
                            log::error!(
                                "Basic auth (password): ID token verification failed; \
                                 refreshing provider keys and retrying: {detail}"
                            );
                            auth_n.refresh_client().await.map_err(|e| {
                                log::error!("Failed to refresh provider metadata: {e}");
                                ErrorInternalServerError(String::from(
                                    "Unable to authenticate user",
                                ))
                            })?;
                            let client = auth_n.get_specific_client(&login_type)?;
                            exchange_password_for_id_token(&client, &username, &password)
                                .await
                                .map_err(|e| {
                                    log::error!(
                                        "Basic auth (password) failed after key refresh: {e}"
                                    );
                                    actix_web::Error::from(e)
                                })?
                        }
                        result => result.map_err(|e| {
                            log::error!("Basic auth (password) failed: {e}");
                            actix_web::Error::from(e)
                        })?,
                    };

                    let user =
                        auth_n.authenticate_with_bearer_token(&login_type, &id_token)?;
                    auth_n.token_cache.insert(key, user.clone(), expires_in);
                    Ok(user)
                })
            }
            BasicAuthGrant::ClientCredentials {
                client_id,
                client_secret,
            } => {
                let auth_n = self.clone();
                let key = TokenExchangeCache::key(
                    scope,
                    CachedGrant::ClientCredentials,
                    client_id.as_str(),
                    client_secret.secret(),
                );
                let client = match self.machine_client(
                    login_type,
                    client_id.clone(),
                    client_secret,
                ) {
                    Ok(client) => client,
                    Err(e) => {
                        log::error!("Error building machine client: {e}");
                        return Box::pin(async {
                            Err(ErrorInternalServerError(String::from(
                                "Error building machine client",
                            ))
                            .into())
                        });
                    }
                };
                Box::pin(async move {
                    if let Some(user) = auth_n.token_cache.get(&key) {
                        return Ok(user);
                    }

                    let expires_in =
                        validate_client_credentials(&client).await.map_err(|e| {
                            log::error!("Basic auth (client_credentials) failed: {e}");
                            actix_web::Error::from(e)
                        })?;

                    // No user identity in a client_credentials grant; derive a
                    // stable, namespaced service principal from the client_id.
                    let principal = format!("service-account-{}", client_id.as_str());
                    let user = User::new(principal.clone(), principal);
                    auth_n.token_cache.insert(key, user.clone(), expires_in);
                    Ok(user)
                })
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
                    .map_err(|e| log::error!("Error in decoding user : {e:?}"))
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
                .add_scope(Scope::new("openid".to_string()))
                .request_async(oidcrs::reqwest::async_http_client)
                .await
                .map_err(|e| {
                    log::error!("Failed to switch organisation for token: {e:?}");
                    Some(ErrorInternalServerError(
                        "Failed to switch organisation for token".to_string(),
                    ))
                })
                .and_then(|tr| {
                    let id_token = tr.id_token().ok_or_else(|| {
                        log::error!("No identity-token!");
                        None
                    })?;
                    id_token
                        .claims(&client.id_token_verifier(), presence_no_check)
                        .map_err(|e| {
                            log::error!("Couldn't verify claims: {e:?}");
                            None
                        })?;

                    Ok(id_token.to_string())
                })
                .map_err(|e| e.map(Into::into).unwrap_or(redirect))
        })
    }
}
