//! HTTP routes the authentication stack owns.
//!
//! Replaces `AuthNHandler::routes()` and `AuthNHandler::org_routes()`. In the
//! original these were methods on the `Authenticator` trait, so every
//! implementation had to carry route-mounting concerns whether or not it had any
//! routes. Here they are ordinary handlers over a `LoginFlow`.

use std::sync::Arc;

use actix_web::{
    HttpRequest, HttpResponse, Scope,
    cookie::Cookie,
    get,
    http::header,
    web::{self, Data, Path},
};
use authn_kit::{
    AuthScope,
    adapters::actix::error_to_response,
    oidc::{CallbackParams, LoginFlow, NonceCheck, OidcProvider},
};
use serde::Deserialize;

use crate::{
    auth::scope::SuperpositionScope, middlewares::auth_n::helpers::fetch_org_ids_from_db,
};

/// Everything the routes need that is not request-scoped.
#[derive(Clone)]
pub struct AuthRoutesState {
    /// `None` when authentication is disabled: there is no issuer to log in to.
    pub login: Option<Arc<LoginFlow>>,
    /// `None` when authentication is disabled, in which case no token is
    /// validated and organisation switching issues a placeholder, exactly as the
    /// original's `DisabledAuthenticator` did.
    pub provider: Option<Arc<OidcProvider>>,
    /// The service's path prefix, used to build redirect targets.
    pub path_prefix: String,
}

impl AuthRoutesState {
    fn cookie_path(&self) -> String {
        if self.path_prefix.is_empty() {
            String::from("/")
        } else {
            self.path_prefix.clone()
        }
    }
}

/// The OIDC callback route: `<prefix>/oidc/login`.
pub fn oidc_routes(state: AuthRoutesState) -> Scope {
    web::scope("oidc")
        .app_data(Data::new(state))
        .service(web::resource("login").route(web::get().to(oidc_login)))
}

/// The organisation routes: `<prefix>/organisations`.
pub fn org_routes(state: AuthRoutesState) -> Scope {
    web::scope("organisations")
        .app_data(Data::new(state))
        .service(list_organisations)
        .service(switch_organisation)
}

/// Completes the authorization-code flow and installs the session.
async fn oidc_login(state: Data<AuthRoutesState>, request: HttpRequest) -> HttpResponse {
    let callback = match CallbackParams::from_query(request.query_string()) {
        Ok(callback) => callback,
        Err(error) => return error_to_response(&error),
    };

    let Some(login) = &state.login else {
        return HttpResponse::NotFound()
            .json(serde_json::json!({ "message": "Authentication is disabled" }));
    };

    let protection = request
        .cookie(&login.protection_cookie().name)
        .map(|cookie| cookie.value().to_string());

    match login.complete(callback, protection.as_deref()).await {
        Ok(complete) => {
            let mut response = HttpResponse::Found();
            response.insert_header((header::LOCATION, complete.redirect_to));
            for directive in &complete.cookies {
                response.cookie(Cookie::from(directive));
            }
            response.finish()
        }
        Err(error) => error_to_response(&error),
    }
}

#[get("")]
async fn list_organisations(request: HttpRequest) -> HttpResponse {
    match fetch_org_ids_from_db(&request) {
        Ok(organisations) => HttpResponse::Ok().json(organisations),
        Err(message) => actix_web::error::ErrorInternalServerError(message).into(),
    }
}

#[derive(Deserialize)]
struct SwitchOrgParams {
    organisation_id: String,
}

/// Issues an organisation-scoped session cookie.
///
/// A direct port of `Authenticator::switch_organisation` plus the simple
/// authenticator's `generate_org_user`: the org cookie carries the *same* token
/// as the global session, so switching organisation re-scopes an existing login
/// rather than obtaining a new one.
#[get("/switch/{organisation_id}")]
async fn switch_organisation(
    state: Data<AuthRoutesState>,
    request: HttpRequest,
    path: Path<SwitchOrgParams>,
) -> HttpResponse {
    let org_id = &path.organisation_id;
    let global_cookie = SuperpositionScope::Global.session_cookie_name();

    let token = match &state.provider {
        // Authentication disabled: the original returned a fixed placeholder.
        None => Some(String::from("org_token")),
        Some(provider) => match request.cookie(&global_cookie) {
            Some(cookie) => {
                let value = cookie.value().to_string();
                match provider.verify_id_token(&value, NonceCheck::Present).await {
                    Ok(_) => Some(value),
                    Err(error) => {
                        log::error!("switch-org: session token rejected: {error}");
                        None
                    }
                }
            }
            None => None,
        },
    };

    let Some(token) = token else {
        // No usable session: start a login and come back to the organisation
        // list, as the original's `generate_org_user` did.
        let Some(login) = &state.login else {
            return HttpResponse::NotFound()
                .json(serde_json::json!({ "message": "Authentication is disabled" }));
        };
        let destination = format!("{}/admin/organisations", state.path_prefix);
        return match login.authorize(&destination) {
            Ok(redirect) => {
                let mut response = HttpResponse::Found();
                response.insert_header((header::LOCATION, redirect.location));
                for directive in &redirect.cookies {
                    response.cookie(Cookie::from(directive));
                }
                response.finish()
            }
            Err(error) => error_to_response(&error),
        };
    };

    let scope = SuperpositionScope::Org(org_id.clone());
    let cookie = Cookie::build(scope.session_cookie_name(), token)
        .path(state.cookie_path())
        .http_only(true)
        .secure(
            state
                .login
                .as_ref()
                .is_none_or(|l| l.session_cookie().secure),
        )
        .max_age(actix_web::cookie::time::Duration::days(1))
        .finish();

    HttpResponse::Found()
        .cookie(cookie)
        .insert_header((
            header::LOCATION,
            format!("{}/admin/{org_id}/workspaces", state.path_prefix),
        ))
        .finish()
}
