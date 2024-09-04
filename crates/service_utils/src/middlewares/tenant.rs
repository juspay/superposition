use std::future::{ready, Ready};

use crate::service::types::{AppState, Tenant};
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

pub struct TenantMiddlewareFactory;
impl<S, B> Transform<S, ServiceRequest> for TenantMiddlewareFactory
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type InitError = ();
    type Transform = TenantMiddleware<S>;
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(TenantMiddleware {
            service: Rc::new(service),
        }))
    }
}

pub struct TenantMiddleware<S> {
    service: Rc<S>,
}

fn extract_tenant_from_header(headers: &HeaderMap) -> Option<&str> {
    headers
        .get("x-tenant")
        .and_then(|header_value: &HeaderValue| header_value.to_str().ok())
}

fn extract_tenant_from_url(path: &str, match_pattern: Option<String>) -> Option<&str> {
    match_pattern.and_then(move |pattern| {
        let pattern_segments = pattern.split('/');
        let path_segments = path.split('/').collect::<Vec<&str>>();

        debug!("PATTERN_SEGMENTS ===> {:?}", pattern_segments);
        debug!("PATH_SEGMENTS ===> {:?}", path_segments);

        std::iter::zip(path_segments, pattern_segments)
            .find(|(_, pattern_seg)| pattern_seg == &"{tenant}")
            .map(|(path_seg, _)| path_seg)
    })
}

fn extract_tenant_from_query_params(query_str: &str) -> Option<&str> {
    query_str
        .split('&')
        .find(|segment| segment.contains("tenant="))
        .and_then(|tenant_query_param| tenant_query_param.split('=').nth(1))
}

impl<S, B> Service<ServiceRequest> for TenantMiddleware<S>
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
            let pkg_regex = Regex::new(".*/pkg/.+")
                .map_err(|err| error::ErrorInternalServerError(err.to_string()))?;
            let assets_regex = Regex::new(".*/assets/.+")
                .map_err(|err| error::ErrorInternalServerError(err.to_string()))?;
            let is_excluded: bool = app_state
                .tenant_middleware_exclusion_list
                .contains(&request_path)
                || pkg_regex.is_match(&request_path)
                || assets_regex.is_match(&request_path);

            if !is_excluded && app_state.enable_tenant_and_scope {
                debug!(
                    "TENANT FROM HEADER ==> {:?}",
                    extract_tenant_from_header(req.headers())
                );
                debug!(
                    "TENANT FROM URL ==> {:?}",
                    extract_tenant_from_url(req.path(), req.match_pattern())
                );
                debug!(
                    "TENANT FROM QUERY ==> {:?}",
                    extract_tenant_from_query_params(req.query_string())
                );

                let tenant = extract_tenant_from_header(req.headers())
                    .or_else(|| extract_tenant_from_url(req.path(), req.match_pattern()))
                    .or_else(|| extract_tenant_from_query_params(req.query_string()));

                let validated_tenant: Tenant = match tenant {
                    Some(val) if app_state.tenants.contains(val) => {
                        Tenant(String::from(val))
                    }
                    Some(_) => {
                        return Err(error::ErrorBadRequest("invalid x-tenant value"));
                    }
                    None => {
                        return Err(error::ErrorBadRequest("x-tenant not set"));
                    }
                };

                req.extensions_mut().insert(validated_tenant);
            }

            let res = srv.call(req).await?;

            Ok(res)
        })
    }
}
