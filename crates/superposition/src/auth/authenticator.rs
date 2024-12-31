use std::fmt::Display;

use actix_web::{
    dev::ServiceRequest,
    http::header::{HeaderMap, HeaderValue},
    web::Path,
    HttpRequest, HttpResponse, Scope,
};
use futures_util::future::LocalBoxFuture;
use serde::Deserialize;
use service_utils::service::types::OrganisationId;
use superposition_types::User;

#[derive(Deserialize)]
pub(super) struct SwitchOrgParams {
    pub(super) organisation_id: String,
}

fn extract_org_from_header(headers: &HeaderMap) -> Option<&str> {
    headers
        .get("x-org-id")
        .and_then(|header_value: &HeaderValue| header_value.to_str().ok())
}

fn extract_org_from_url(path: &str, match_pattern: Option<String>) -> Option<&str> {
    match_pattern.and_then(move |pattern| {
        let pattern_segments = pattern.split('/');
        let path_segments = path.split('/').collect::<Vec<&str>>();

        std::iter::zip(path_segments, pattern_segments)
            .find(|(_, pattern_seg)| *pattern_seg == "{org_id}")
            .map(|(path_seg, _)| path_seg)
    })
}

fn extract_org_from_query_params(query_str: &str) -> Option<&str> {
    query_str
        .split('&')
        .find(|segment| segment.contains("org="))
        .and_then(|tenant_query_param| tenant_query_param.split('=').nth(1))
}

#[derive(Debug)]
pub enum Login {
    None,
    Global,
    Org,
}

impl Display for Login {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "none"),
            Self::Global => write!(f, "user"),
            Self::Org => write!(f, "org_user"),
        }
    }
}

pub trait Authenticator: Sync + Send {
    fn get_path_prefix(&self) -> String;

    fn authenticate(
        &self,
        request: &ServiceRequest,
        login_type: &Login,
    ) -> Result<User, HttpResponse>;
    fn routes(&self) -> Scope;

    fn get_organisations(&self, req: &HttpRequest) -> HttpResponse;

    fn switch_organisation(
        &self,
        req: &HttpRequest,
        path: &Path<SwitchOrgParams>,
    ) -> LocalBoxFuture<'static, actix_web::Result<HttpResponse>>;

    fn get_org_id(&self, request: &ServiceRequest) -> OrganisationId {
        extract_org_from_header(request.headers())
            .or_else(|| extract_org_from_url(request.path(), request.match_pattern()))
            .or_else(|| extract_org_from_query_params(request.query_string()))
            .map(String::from)
            .map(OrganisationId)
            .unwrap_or_default()
    }
}
