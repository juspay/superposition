use std::fmt::Display;

use actix_web::{
    HttpRequest, HttpResponse, Scope,
    cookie::{Cookie, time::Duration},
    error::ErrorNotImplemented,
    http::header,
    web::Path,
};
use futures_util::future::LocalBoxFuture;
use openidconnect::{
    ClientId, ClientSecret, ResourceOwnerPassword, ResourceOwnerUsername,
};
use serde::Deserialize;
use superposition_types::User;

#[derive(Deserialize)]
pub(super) struct SwitchOrgParams {
    pub(super) organisation_id: String,
}

/// The grant a `Basic` credential should be validated under. Selected by the
/// `X-Grant-Type` header on the request (see [`BasicAuthGrant::resolve`]).
pub enum BasicAuthGrant {
    /// Resource Owner Password Credentials — the `Basic` pair is a user's
    /// username/password, exchanged with the IdP for an id-token.
    Password {
        username: ResourceOwnerUsername,
        password: ResourceOwnerPassword,
    },
    /// Client Credentials (machine-to-machine) — the `Basic` pair is a
    /// client_id/client_secret validated against the IdP.
    ClientCredentials {
        client_id: ClientId,
        client_secret: ClientSecret,
    },
}

impl BasicAuthGrant {
    /// Maps the `X-Grant-Type` header to a grant. Absent defaults to
    /// `client_credentials`; `password` selects ROPC; anything else is
    /// rejected (`None`) so the caller can fail closed with a 400.
    pub fn resolve(grant_type: Option<&str>, id: String, secret: String) -> Option<Self> {
        match grant_type {
            None | Some("client_credentials") => Some(Self::ClientCredentials {
                client_id: ClientId::new(id),
                client_secret: ClientSecret::new(secret),
            }),
            Some("password") => Some(Self::Password {
                username: ResourceOwnerUsername::new(id),
                password: ResourceOwnerPassword::new(secret),
            }),
            Some(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Login {
    None,
    Global,
    Org(String),
}

impl Display for Login {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Login::None => write!(f, "none"),
            Login::Global => write!(f, "user"),
            Login::Org(org_id) => write!(f, "org_{org_id}"),
        }
    }
}

pub trait Authenticator: Sync + Send {
    fn routes(&self) -> Scope;

    fn get_path_prefix(&self) -> String;

    fn get_cookie_path(&self) -> String {
        let prefix = self.get_path_prefix();
        if prefix.as_str() == "" {
            String::from('/')
        } else {
            prefix
        }
    }

    fn authenticate(
        &self,
        request: &HttpRequest,
        login_type: &Login,
    ) -> LocalBoxFuture<'static, Result<User, HttpResponse>>;

    fn authenticate_with_bearer_token(
        &self,
        login_type: &Login,
        token: &str,
    ) -> Result<User, HttpResponse>;

    /// The configured API-token prefix (`custom_prefix` + `delimiter`) when the
    /// optional RFC 7662 introspection flow is enabled, else `None`. The
    /// middleware uses this to route `Bearer <prefix>...` tokens to
    /// [`Self::authenticate_with_api_token`] instead of OIDC id-token validation.
    fn api_token_prefix(&self) -> Option<String> {
        None
    }

    /// Validates an API token (the prefix already stripped) via RFC 7662 token
    /// introspection. Only invoked when [`Self::api_token_prefix`] is `Some`;
    /// the default is a safety net for providers that don't support the flow.
    fn authenticate_with_api_token(
        &self,
        _login_type: &Login,
        _api_key: &str,
    ) -> LocalBoxFuture<'static, Result<User, HttpResponse>> {
        Box::pin(async {
            Err(ErrorNotImplemented("API token authentication is not supported").into())
        })
    }

    fn authenticate_with_basic_auth(
        &self,
        login_type: &Login,
        grant: BasicAuthGrant,
    ) -> LocalBoxFuture<'static, Result<User, HttpResponse>>;

    fn get_organisations(&self, req: &HttpRequest) -> HttpResponse;

    fn generate_org_user<'a>(
        &'a self,
        req: &HttpRequest,
        org_id: &str,
        login_type: &Login,
    ) -> LocalBoxFuture<'a, Result<String, HttpResponse>>;

    fn switch_organisation<'a>(
        &'a self,
        req: &HttpRequest,
        path: &Path<SwitchOrgParams>,
    ) -> LocalBoxFuture<'a, HttpResponse> {
        let login_type = Login::Org(path.organisation_id.clone());
        let user_token_future =
            self.generate_org_user(req, &path.organisation_id, &login_type);

        let prefix = self.get_path_prefix();
        let cookie_path = self.get_cookie_path();
        let org_id = path.organisation_id.clone();

        Box::pin(async move {
            match user_token_future.await {
                Ok(token) => {
                    let cookie = Cookie::build(login_type.to_string(), token)
                        .path(cookie_path)
                        .http_only(true)
                        .secure(true)
                        .max_age(Duration::days(1))
                        .finish();
                    HttpResponse::Found()
                        .cookie(cookie)
                        .insert_header((
                            header::LOCATION,
                            format!("{prefix}/admin/{org_id}/workspaces"),
                        ))
                        .finish()
                }
                Err(resp) => resp,
            }
        })
    }
}
