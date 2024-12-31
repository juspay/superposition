use std::future::{ready, Ready};

use crate::service::types::{AppState, OrganisationId, Tenant};
use actix_web::{
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    error::{self},
    http::header::{HeaderMap, HeaderValue},
    web::Data,
    Error, HttpMessage,
};
use futures_util::future::LocalBoxFuture;
use log::debug;
use regex::Regex;
use std::rc::Rc;
use superposition_types::TenantConfig;

pub struct OrgWorkspaceMiddlewareFactory {
    enable_org_id: bool,
    enable_workspace_id: bool,
}

impl OrgWorkspaceMiddlewareFactory {
    pub fn new(enable_org_id: bool, enable_workspace_id: bool) -> Self {
        Self {
            enable_org_id,
            enable_workspace_id,
        }
    }
}

impl<S, B> Transform<S, ServiceRequest> for OrgWorkspaceMiddlewareFactory
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type InitError = ();
    type Transform = OrgWorkspaceMiddleware<S>;
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(OrgWorkspaceMiddleware {
            service: Rc::new(service),
            enable_org_id: self.enable_org_id,
            enable_workspace_id: self.enable_workspace_id,
        }))
    }
}

pub struct OrgWorkspaceMiddleware<S> {
    service: Rc<S>,
    enable_org_id: bool,
    enable_workspace_id: bool,
}

fn extract_org_workspace_from_header(
    headers: &HeaderMap,
    header_name: String,
) -> Option<&str> {
    headers
        .get(header_name)
        .and_then(|header_value: &HeaderValue| header_value.to_str().ok())
}

fn extract_org_workspace_from_url(
    path: &str,
    match_pattern: Option<String>,
    matching_url_param: String,
) -> Option<&str> {
    match_pattern.and_then(move |pattern| {
        let pattern_segments = pattern.split('/');
        let path_segments = path.split('/').collect::<Vec<&str>>();

        debug!("PATTERN_SEGMENTS ===> {:?}", pattern_segments);
        debug!("PATH_SEGMENTS ===> {:?}", path_segments);

        std::iter::zip(path_segments, pattern_segments)
            .find(|(_, pattern_seg)| *pattern_seg == matching_url_param.as_str())
            .map(|(path_seg, _)| path_seg)
    })
}

fn extract_org_workspace_from_query_params(
    query_str: &str,
    matching_pattern: String,
) -> Option<&str> {
    query_str
        .split('&')
        .find(|segment| segment.contains(matching_pattern.as_str()))
        .and_then(|tenant_query_param| tenant_query_param.split('=').nth(1))
}

impl<S, B> Service<ServiceRequest> for OrgWorkspaceMiddleware<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

    forward_ready!(service);

    fn call(&self, req: ServiceRequest) -> Self::Future {
        let srv = self.service.clone();
        let enable_org_id = self.enable_org_id;
        let enable_workspace_id = self.enable_workspace_id;

        Box::pin(async move {
            let app_state = match req.app_data::<Data<AppState>>() {
                Some(val) => val,
                None => {
                    log::error!("app state not set");
                    return Err(error::ErrorInternalServerError(""));
                }
            };

            let base = match app_state.service_prefix.as_str() {
                "" | "/" => "".to_owned(),
                prefix => "/".to_owned() + prefix,
            };

            let request_path = req.uri().path().replace(&base, "");
            let request_pattern = req
                .match_pattern()
                .map(|a| a.replace(&base, ""))
                .unwrap_or_else(|| request_path.clone());
            let pkg_regex = Regex::new(".*/pkg/.+")
                .map_err(|err| error::ErrorInternalServerError(err.to_string()))?;
            let assets_regex = Regex::new(".*/assets/.+")
                .map_err(|err| error::ErrorInternalServerError(err.to_string()))?;
            let is_excluded: bool = app_state
                .tenant_middleware_exclusion_list
                .contains(&request_pattern)
                || pkg_regex.is_match(&request_path)
                || assets_regex.is_match(&request_path);

            if !is_excluded && app_state.enable_tenant_and_scope {
                debug!(
                    "Workspace FROM HEADER ==> {:?}",
                    extract_org_workspace_from_header(
                        req.headers(),
                        String::from("x-tenant")
                    )
                );
                debug!(
                    "Workspace FROM URL ==> {:?}",
                    extract_org_workspace_from_url(
                        req.path(),
                        req.match_pattern(),
                        String::from("{tenant}")
                    )
                );
                debug!(
                    "Workspace FROM QUERY ==> {:?}",
                    extract_org_workspace_from_query_params(
                        req.query_string(),
                        String::from("tenant=")
                    )
                );

                let workspace = extract_org_workspace_from_header(
                    req.headers(),
                    String::from("x-tenant"),
                )
                .or_else(|| {
                    extract_org_workspace_from_url(
                        req.path(),
                        req.match_pattern(),
                        String::from("{tenant}"),
                    )
                })
                .or_else(|| {
                    extract_org_workspace_from_query_params(
                        req.query_string(),
                        String::from("tenant="),
                    )
                });

                let org = extract_org_workspace_from_header(
                    req.headers(),
                    String::from("x-org-id"),
                )
                .or_else(|| {
                    extract_org_workspace_from_url(
                        req.path(),
                        req.match_pattern(),
                        String::from("{org_id}"),
                    )
                })
                .or_else(|| {
                    extract_org_workspace_from_query_params(
                        req.query_string(),
                        String::from("org="),
                    )
                });

                let workspace_id = match (enable_workspace_id, workspace) {
                    (true, None) => return Err(error::ErrorBadRequest("The parameter workspace id is required, and must be passed through headers/url params/query params. Consult the documentation to know which to use for this endpoint")),
                    (true, Some(workspace_id)) => workspace_id,
                    (false, _) => "public",
                };

                // TODO: validate the tenant, get correct TenantConfig
                let (validated_tenant, tenant_config) =  match (enable_org_id, org) {
                    (true, None) => return Err(error::ErrorBadRequest("The parameter org id is required, and must be passed through headers/url params/query params. Consult the documentation to know which to use for this endpoint")),
                    (true, Some(org_id)) => {
                        let tenant = format!("{org_id}_{workspace_id}");
                        (Tenant(tenant), TenantConfig::default())
                    },
                    (false, _) => (Tenant("public".into()), TenantConfig::default()),
                };

                let organisation = org
                    .map(String::from)
                    .map(OrganisationId)
                    .unwrap_or_default();

                req.extensions_mut().insert(validated_tenant);
                req.extensions_mut().insert(organisation);
                req.extensions_mut().insert(tenant_config);
            }

            let res = srv.call(req).await?;

            Ok(res)
        })
    }
}
