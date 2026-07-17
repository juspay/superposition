use std::sync::{Arc, RwLock};

use actix_web::{
    HttpRequest, HttpResponse,
    error::{
        ErrorBadRequest, ErrorInternalServerError, ErrorNotImplemented, ErrorUnauthorized,
    },
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
    authentication::{Authenticator, BasicAuthGrant, Login},
    helpers::fetch_org_ids_from_db,
    oidc::{
        OIDCAuthenticator,
        api_token::ApiTokenConfig,
        token_cache::{CachedGrant, TokenCacheConfig, TokenExchangeCache},
        utils::{
            BasicAuthError, OidcProviderMetadata, build_client,
            discovered_introspection_endpoint, exchange_password_for_id_token,
            fetch_provider_metadata, try_user_from, validate_client_credentials,
            verify_presence,
        },
    },
};

pub struct AuthenticatorInner {
    client: RwLock<CoreClient>,
    provider_metadata: RwLock<OidcProviderMetadata>,
    issuer_url: IssuerUrl,
    client_id: ClientId,
    client_secret: ClientSecret,
    redirect_url: RedirectUrl,
    path_prefix: String,
    token_cache: TokenExchangeCache,
    api_token: Option<ApiTokenConfig>,
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
        introspection_auth_header: Option<String>,
        static_tokens_raw: Option<String>,
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
            provider_metadata.clone(),
            client_id.clone(),
            client_secret.clone(),
            redirect_url.clone(),
        );

        let api_token = ApiTokenConfig::from_env(
            introspection_auth_header,
            static_tokens_raw,
            false,
        )?;
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
            redirect_url,
            path_prefix,
            token_cache: TokenExchangeCache::new(TokenCacheConfig::from_env()),
            api_token,
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

    fn get_user_from_id_token(&self, token: &str) -> Result<User, String> {
        self.decode_global_token(token)
            .map_err(|e| format!("Error in decoding user: {e}"))
            .and_then(|claims| {
                try_user_from(&claims).map_err(|e| format!("Unable to get user: {e}"))
            })
    }

    /// Builds an OIDC client configured with the *machine's* credentials (from
    /// a `Basic` header) so a `client_credentials` exchange authenticates the
    /// machine rather than this service's own OIDC client.
    fn machine_client(
        &self,
        client_id: ClientId,
        client_secret: ClientSecret,
    ) -> CoreClient {
        let provider_metadata = self
            .provider_metadata
            .read()
            .unwrap_or_else(|e| e.into_inner())
            .clone();
        build_client(
            provider_metadata,
            client_id,
            client_secret,
            self.redirect_url.clone(),
        )
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
            provider_metadata.clone(),
            self.client_id.clone(),
            self.client_secret.clone(),
            self.redirect_url.clone(),
        );

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

    fn authenticate_with_bearer_token(
        &self,
        login_type: &Login,
        token: &str,
    ) -> Result<User, HttpResponse> {
        match login_type {
            Login::None => Ok(User::default()),
            _ => self.get_user_from_id_token(token).map_err(|e| {
                log::error!("Error in decoding user : {e}");
                ErrorUnauthorized(String::from("Unable to get user")).into()
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

        let auth_n = self.clone();
        let api_key = api_key.to_string();
        // Simple is single-realm, so the introspected principal is
        // org-independent: scope the cache key by `None`.
        let key = TokenExchangeCache::key(None, CachedGrant::ApiToken, "", &api_key);
        Box::pin(async move {
            let Some(config) = auth_n.api_token.as_ref() else {
                return Err(ErrorNotImplemented(
                    "API token authentication is not configured",
                )
                .into());
            };

            if let Some(user) = config.match_static_token(None, &api_key) {
                return Ok(user);
            }

            if !config.introspection_enabled() {
                return Err(ErrorUnauthorized("Invalid or inactive API token").into());
            }

            if let Some(user) = auth_n.token_cache.get(&key) {
                return Ok(user);
            }

            // Single-realm: resolve the one endpoint from explicit config, else
            // the endpoint discovered in the (cached) provider metadata.
            let discovered = discovered_introspection_endpoint(
                &auth_n
                    .provider_metadata
                    .read()
                    .unwrap_or_else(|e| e.into_inner()),
            );
            let Some(endpoint) = config.resolve_endpoint(None, discovered) else {
                return Err(ErrorInternalServerError(
                    "API token introspection endpoint is not configured and none \
                     was advertised in provider metadata",
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

        let auth_n = self.clone();
        match grant {
            BasicAuthGrant::Password { username, password } => {
                let key = TokenExchangeCache::key(
                    None,
                    CachedGrant::Password,
                    username.as_str(),
                    password.secret(),
                );
                let client = self.get_client();

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
                            exchange_password_for_id_token(
                                &auth_n.get_client(),
                                &username,
                                &password,
                            )
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

                    let user = auth_n.get_user_from_id_token(&id_token).map_err(|e| {
                        log::error!("Error in decoding user : {e}");
                        ErrorInternalServerError(String::from("Unable to get user"))
                    })?;
                    auth_n.token_cache.insert(key, user.clone(), expires_in);
                    Ok(user)
                })
            }
            BasicAuthGrant::ClientCredentials {
                client_id,
                client_secret,
            } => {
                let key = TokenExchangeCache::key(
                    None,
                    CachedGrant::ClientCredentials,
                    client_id.as_str(),
                    client_secret.secret(),
                );
                let client = self.machine_client(client_id.clone(), client_secret);

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
