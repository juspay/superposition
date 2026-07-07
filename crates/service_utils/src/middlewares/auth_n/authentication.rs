use std::fmt::Display;

use actix_web::{
    HttpRequest, HttpResponse, HttpResponseBuilder, Scope,
    cookie::{Cookie, time::Duration},
    http::header,
    web::Path,
};
use futures_util::future::LocalBoxFuture;
use serde::Deserialize;
use superposition_types::User;

#[derive(Deserialize)]
pub(super) struct SwitchOrgParams {
    pub(super) organisation_id: String,
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

const MAX_ORG_COOKIES: usize = 2;
const ORG_COOKIE_PREFIX: &str = "org_";
const ORGS_ORDER_COOKIE: &str = "orgs_order";

fn parse_org_ids_from_cookie_header(req: &HttpRequest) -> Vec<String> {
    req.headers()
        .get(header::COOKIE)
        .and_then(|v| v.to_str().ok())
        .map(|header_val| {
            header_val
                .split(';')
                .filter_map(|pair| {
                    let pair = pair.trim();
                    let eq = pair.find('=')?;
                    let name = &pair[..eq];
                    name.strip_prefix(ORG_COOKIE_PREFIX)
                        .map(|id| id.to_string())
                })
                .collect()
        })
        .unwrap_or_default()
}

pub(super) fn add_org_cookie_with_eviction(
    req: &HttpRequest,
    login_type: &Login,
    token: String,
    cookie_path: String,
    mut builder: HttpResponseBuilder,
) -> HttpResponseBuilder {
    let Login::Org(new_org_id) = login_type else {
        return builder;
    };

    let mut order: Vec<String> = req
        .cookie(ORGS_ORDER_COOKIE)
        .and_then(|c| {
            let v: Vec<String> = c
                .value()
                .split(',')
                .filter(|s| !s.is_empty())
                .map(|s| s.to_string())
                .collect();
            (!v.is_empty()).then_some(v)
        })
        .unwrap_or_else(|| {
            log::warn!(
                "orgs_order cookie missing, scanning request cookies for org cookies"
            );
            parse_org_ids_from_cookie_header(req)
        });

    order.retain(|id| *id != *new_org_id);
    order.push(new_org_id.clone());

    while order.len() > MAX_ORG_COOKIES {
        let evicted_id = order.remove(0);
        let evict_cookie = Cookie::build(format!("{ORG_COOKIE_PREFIX}{evicted_id}"), "")
            .path(cookie_path.clone())
            .http_only(true)
            .secure(true)
            .max_age(Duration::seconds(0))
            .finish();
        builder.cookie(evict_cookie);
    }

    let new_cookie = Cookie::build(login_type.to_string(), token)
        .path(cookie_path.clone())
        .http_only(true)
        .secure(true)
        .max_age(Duration::days(1))
        .finish();
    builder.cookie(new_cookie);

    let order_cookie = Cookie::build(ORGS_ORDER_COOKIE.to_string(), order.join(","))
        .path(cookie_path)
        .http_only(true)
        .secure(true)
        .max_age(Duration::days(1))
        .finish();
    builder.cookie(order_cookie);

    builder
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

    fn get_organisations(&self, req: &HttpRequest) -> HttpResponse;

    fn generate_org_user<'a>(
        &'a self,
        req: &HttpRequest,
        org_id: &str,
        login_type: &Login,
    ) -> LocalBoxFuture<'a, Result<String, HttpResponse>>;

    fn switch_organisation<'a>(
        &'a self,
        req: &'a HttpRequest,
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
                Ok(token) => add_org_cookie_with_eviction(
                    req,
                    &login_type,
                    token,
                    cookie_path,
                    HttpResponse::Found(),
                )
                .insert_header((
                    header::LOCATION,
                    format!("{prefix}/admin/{org_id}/workspaces"),
                ))
                .finish(),
                Err(resp) => resp,
            }
        })
    }
}
