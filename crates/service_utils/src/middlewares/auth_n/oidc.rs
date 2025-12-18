mod types;
mod utils;

use std::sync::Arc;

use actix_web::{
    cookie::{time::Duration, Cookie},
    error::{ErrorBadRequest, ErrorInternalServerError},
    get,
    http::header,
    web::{self, Data, Json, Query},
    HttpRequest, HttpResponse,
};
use base64::{engine::general_purpose, Engine};
use derive_more::{Deref, DerefMut};
use diesel::{
    query_dsl::methods::{OrderDsl, SelectDsl},
    Connection, ExpressionMethods, RunQueryDsl,
};
use futures_util::future::LocalBoxFuture;
use openidconnect::{
    self as oidcrs,
    core::{CoreClient, CoreProviderMetadata, CoreResponseType},
    AuthenticationFlow, ClientId, ClientSecret, CsrfToken, IssuerUrl, Nonce, RedirectUrl,
    TokenResponse,
};
use superposition_types::{
    database::superposition_schema::superposition::organisations, User,
};
use types::{
    GlobalUserClaims, GlobalUserTokenResponse, LoginParams, ProtectionCookie,
    RedirectionState,
};
use utils::{try_user_from, verify_presence};

use crate::service::types::AppState;

use super::authentication::{Authenticator, Login};

#[derive(Clone)]
pub struct OIDCAuthenticatorInner {
    client: CoreClient,
    path_prefix: String,
}

#[derive(Deref, DerefMut, Clone)]
pub struct OIDCAuthenticator(Arc<OIDCAuthenticatorInner>);

impl OIDCAuthenticator {
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

        Ok(Self(Arc::new(OIDCAuthenticatorInner {
            client,
            path_prefix,
        })))
    }

    fn new_redirect(&self, cookie_type: &Login, path: String) -> HttpResponse {
        let state = RedirectionState {
            csrf: CsrfToken::new_random(),
            redirect_uri: path,
        };

        let encoded_state = general_purpose::STANDARD
            .encode(serde_json::to_string(&state).unwrap_or_default());

        let (auth_url, csrf_token, nonce) = self
            .client
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

    fn decode_global_token(&self, cookie: &str) -> Result<GlobalUserClaims, String> {
        let ctr = serde_json::from_str::<GlobalUserTokenResponse>(cookie)
            .map_err(|e| format!("Error while decoding token: {e}"))?;
        ctr.id_token()
            .ok_or(String::from("Id Token not found"))?
            .claims(&self.client.id_token_verifier(), verify_presence)
            .map_err(|e| format!("Error in claims verification: {e}"))
            .cloned()
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

impl Authenticator for OIDCAuthenticator {
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
                let resp = auth_n.get_global_user(
                    request,
                    format!("{}/admin/organisations", self.path_prefix),
                );
                Box::pin(async { resp })
            }
        }
    }

    fn routes(&self) -> actix_web::Scope {
        web::scope("oidc")
            .app_data(Data::new(self.to_owned()))
            .service(login)
    }

    fn get_organisations(&self, req: &HttpRequest) -> HttpResponse {
        let app_state = match req.app_data::<Data<AppState>>() {
            Some(state) => state,
            None => {
                log::info!(
                    "DbConnection-FromRequest: Unable to get app_data from request"
                );
                return ErrorInternalServerError("Unable to get app_data from request")
                    .into();
            }
        };

        let result = match app_state.db_pool.get() {
            Ok(mut conn) => {
                conn.set_prepared_statement_cache_size(
                    diesel::connection::CacheSize::Disabled,
                );
                let orgs = organisations::table
                    .order(organisations::created_at.desc())
                    .select(organisations::id)
                    .get_results::<String>(&mut conn);

                match orgs {
                    Ok(orgs) => Ok(orgs),
                    Err(e) => {
                        log::error!("Failed to fetch organisations: {:?}", e);
                        Err("Failed to fetch organisations")
                    }
                }
            }
            Err(e) => {
                log::info!("Unable to get db connection from pool, error: {e}");
                Err("Unable to get db connection from pool")
            }
        };

        match result {
            Ok(resp) => HttpResponse::Ok().json(Json(resp)),
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

#[get("/login")]
async fn login(
    data: Data<OIDCAuthenticator>,
    req: HttpRequest,
    params: Query<LoginParams>,
) -> actix_web::Result<HttpResponse> {
    let login_type = Login::Global;
    let p_cookie = match ProtectionCookie::from_req(&req) {
        Ok(p_cookie) => p_cookie,
        Err(e) => {
            log::error!("OIDC: Missing/Bad protection-cookie, redirecting... {}", e);
            return Ok(data.new_redirect(
                &login_type,
                format!("{}/admin/organisations", data.path_prefix),
            ));
        }
    };

    if *params.state.csrf.secret() != *p_cookie.csrf.secret() {
        log::error!("OIDC: Bad csrf",);
        return Ok(data.new_redirect(
            &login_type,
            format!("{}/admin/organisations", data.path_prefix),
        ));
    }

    // Exchange the code with a token.
    let token_response = data
        .client
        .exchange_code(params.code.clone())
        .request_async(oidcrs::reqwest::async_http_client)
        .await
        .map_err(|e| {
            log::error!("Failed to exchange auth-code for token: {e}");
            ErrorInternalServerError("Failed to exchange auth-code for token".to_string())
        })?;

    let response = token_response
        .id_token()
        .ok_or_else(|| log::error!("No identity-token!"))
        .and_then(|t| {
            t.claims(&data.client.id_token_verifier(), &p_cookie.nonce)
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
            format!("{}/admin/organisations", data.path_prefix),
        )),
    }
}
