mod types;
mod utils;

use actix_web::{
    cookie::{time::Duration, Cookie},
    dev::ServiceRequest,
    error::{ErrorBadRequest, ErrorInternalServerError},
    get,
    http::header,
    web::{self, Data, Path, Query},
    HttpRequest, HttpResponse,
};
use futures_util::future::LocalBoxFuture;
use openidconnect::{
    self as oidcrs,
    core::{CoreClient, CoreProviderMetadata, CoreResponseType},
    AuthenticationFlow, ClientId, ClientSecret, CsrfToken, IssuerUrl, Nonce, RedirectUrl,
    ResourceOwnerPassword, ResourceOwnerUsername, Scope, TokenResponse, TokenUrl,
};
use service_utils::{extensions::ServiceRequestExt, helpers::get_from_env_unsafe};
use superposition_types::User;
use types::{
    GlobalUserClaims, GlobalUserTokenResponse, LoginParams, OrgUserClaims,
    OrgUserTokenResponse, ProtectionCookie,
};
use utils::{presence_no_check, try_user_from, verify_presence};

use crate::auth::authenticator::SwitchOrgParams;

use super::authenticator::{Authenticator, Login};

#[derive(Clone)]
pub struct OIDCAuthenticator {
    client: CoreClient,
    provider_metadata: CoreProviderMetadata,
    client_id: String,
    client_secret: String,
    base_url: String,
    path_prefix: String,
    issuer_endpoint_format: String,
    token_endpoint_format: String,
}

impl OIDCAuthenticator {
    pub async fn new(
        idp_url: url::Url,
        base_url: String,
        path_prefix: String,
        client_id: String,
        client_secret: String,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let issuer_endpoint_format =
            get_from_env_unsafe::<String>("OIDC_ISSUER_ENDPOINT_FORMAT").unwrap();
        let token_endpoint_format =
            get_from_env_unsafe::<String>("OIDC_TOKEN_ENDPOINT_FORMAT").unwrap();

        let issuer_url = IssuerUrl::from_url(idp_url);

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

        Ok(Self {
            client,
            provider_metadata,
            client_id,
            client_secret,
            base_url,
            path_prefix,
            issuer_endpoint_format,
            token_endpoint_format,
        })
    }

    fn get_org_client(&self, org_id: &String) -> Result<CoreClient, String> {
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
        organisation_id: &String,
    ) -> Result<IssuerUrl, url::ParseError> {
        let issuer_endpoint = self
            .issuer_endpoint_format
            .replace("<organisation>", organisation_id);
        IssuerUrl::new(issuer_endpoint)
    }

    fn get_token_url(
        &self,
        organisation_id: &String,
    ) -> Result<TokenUrl, url::ParseError> {
        let token_endpoint = self
            .token_endpoint_format
            .replace("<organisation>", organisation_id);
        TokenUrl::new(token_endpoint)
    }

    fn get_cookie_path(&self) -> String {
        if self.path_prefix.as_str() == "" {
            String::from('/')
        } else {
            self.path_prefix.clone()
        }
    }

    fn new_redirect(&self, cookie_name: &str) -> HttpResponse {
        let (auth_url, csrf_token, nonce) = self
            .client
            .authorize_url(
                AuthenticationFlow::<CoreResponseType>::AuthorizationCode,
                CsrfToken::new_random,
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
                ErrorInternalServerError(format!("Unable to stringify data"))
            })
            .map(|cookie| {
                Cookie::build("protection", cookie)
                    .max_age(Duration::days(7))
                    // .http_only(true)
                    // .same_site(SameSite::Strict)
                    .path(self.get_cookie_path())
                    .finish()
            });

        match cookie_result {
            Ok(p_cookie) => HttpResponse::Found()
                .insert_header((header::LOCATION, auth_url.to_string()))
                .cookie(p_cookie)
                // Deletes the cookie.
                .cookie(
                    Cookie::build(cookie_name, "")
                        .max_age(Duration::seconds(0))
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

    fn get_global_user(&self, request: &ServiceRequest) -> Result<User, HttpResponse> {
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
            Err(self.new_redirect(&Login::Global.to_string()))
        }
    }

    fn decode_org_token(
        &self,
        org_id: &String,
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

    fn get_org_user(&self, request: &ServiceRequest) -> Result<User, HttpResponse> {
        let org_id = request.get_organisation_id().unwrap_or_default();
        let token = request.cookie(&Login::Org.to_string()).and_then(|c| {
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
            log::error!("Error org_user not found in cookies");
            Err(self.new_redirect(&Login::Org.to_string()))
        }
    }
}

impl Authenticator for OIDCAuthenticator {
    fn get_path_prefix(&self) -> String {
        self.path_prefix.clone()
    }

    fn authenticate(
        &self,
        request: &ServiceRequest,
        login_type: &Login,
    ) -> Result<User, HttpResponse> {
        match login_type {
            Login::None => Ok(User::default()),
            Login::Global => self.get_global_user(request),
            Login::Org => match self.get_global_user(request) {
                Err(e) => Err(e),
                Ok(_) => self.get_org_user(request),
            },
        }
    }

    fn routes(&self) -> actix_web::Scope {
        web::scope("oidc")
            .app_data(Data::new(self.to_owned()))
            .service(login)
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
            Some(organisations) => {
                HttpResponse::Ok().json(serde_json::json!(organisations))
            }
            None => self.new_redirect(&Login::Global.to_string()),
        }
    }

    fn switch_organisation(
        &self,
        req: &HttpRequest,
        path: &Path<SwitchOrgParams>,
    ) -> LocalBoxFuture<'static, actix_web::Result<HttpResponse>> {
        let client = match self.get_org_client(&path.organisation_id) {
            Ok(client) => client,
            Err(e) => {
                log::error!("Error in getting Org specific client: {e}");
                return Box::pin(async move {
                    Err(ErrorInternalServerError(String::from(
                        "Error in getting Org specific client",
                    )))
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
            return Box::pin(async move { Err(ErrorBadRequest("Cookie incorrect")) });
        };

        let username = if let Some(u) = username {
            u
        } else {
            return Box::pin(async move { Err(ErrorBadRequest("Username not found")) });
        };

        let org_id = path.organisation_id.clone();
        let user = ResourceOwnerUsername::new(username.to_string());
        let pass = ResourceOwnerPassword::new(switch_pass);
        let redirect = self.new_redirect(&Login::Org.to_string());
        let prefix = self.path_prefix.clone();
        let cookie_path = self.get_cookie_path();

        Box::pin(async move {
            let response = client
                .exchange_password(&user, &pass)
                .add_scope(Scope::new(String::from("openid")))
                .request_async(oidcrs::reqwest::async_http_client)
                .await
                .map_err(|e| log::error!("Failed to switch organisation for token: {e}"))
                .and_then(|tr| {
                    tr.id_token()
                        .ok_or_else(|| log::error!("No identity-token!"))?
                        .claims(&client.id_token_verifier(), presence_no_check)
                        .map_err(|e| log::error!("Couldn't verify claims: {e}"))?;
                    Ok(tr)
                });

            match response {
                Ok(r) => {
                    let token = serde_json::to_string(&r).map_err(|e| {
                        log::error!("Unable to stringify data: {e}");
                        ErrorInternalServerError(format!("Unable to stringify data"))
                    })?;
                    let cookie = Cookie::build(Login::Org.to_string(), token)
                        .path(cookie_path)
                        .http_only(true)
                        .max_age(Duration::days(1))
                        .finish();
                    Ok(HttpResponse::Found()
                        .cookie(cookie)
                        .insert_header((
                            "Location",
                            format!("{prefix}/admin/{org_id}/workspaces"),
                        ))
                        .finish())
                }
                Err(()) => Ok(redirect),
            }
        })
    }
}

#[get("/login")]
async fn login(
    data: Data<OIDCAuthenticator>,
    req: HttpRequest,
    params: Query<LoginParams>,
) -> actix_web::Result<HttpResponse> {
    let p_cookie = if let Some(p_cookie) = ProtectionCookie::from_req(&req) {
        p_cookie
    } else {
        log::error!("OIDC: Missing/Bad protection-cookie, redirecting...");
        return Ok(data.new_redirect(&Login::Global.to_string()));
    };

    if params.state.secret() != p_cookie.csrf.secret() {
        log::error!("OIDC: Bad csrf",);
        return Ok(data.new_redirect(&Login::Global.to_string()));
    }

    // Exchange the code with a token.
    let response = data
        .client
        .exchange_code(params.code.clone())
        .request_async(oidcrs::reqwest::async_http_client)
        .await
        .map_err(|e| log::error!("Failed to exchange auth-code for token: {e}"))
        .and_then(|tr| {
            tr.id_token()
                .ok_or_else(|| log::error!("No identity-token!"))
                .and_then(|t| {
                    t.claims(&data.client.id_token_verifier(), &p_cookie.nonce)
                        .map_err(|e| log::error!("Couldn't verify claims: {e}"))
                })?;
            Ok(tr)
        });

    match response {
        Ok(r) => {
            let token = serde_json::to_string(&r).map_err(|e| {
                log::error!("Unable to stringify data: {e}");
                ErrorInternalServerError(format!("Unable to stringify data"))
            })?;
            let cookie = Cookie::build(Login::Global.to_string(), token)
                .path(data.get_cookie_path())
                .http_only(true)
                .max_age(Duration::days(1))
                .finish();
            Ok(HttpResponse::Found()
                .cookie(cookie)
                .insert_header((
                    "Location",
                    format!("{}/admin/organisations", data.path_prefix.clone()),
                ))
                .finish())
        }
        Err(()) => Ok(data.new_redirect(&Login::Global.to_string())),
    }
}
