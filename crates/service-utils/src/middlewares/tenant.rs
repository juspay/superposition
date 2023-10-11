use std::future::{ready, Ready};

use crate::service::types::{AppState, Tenant};
use actix_web::{
    web::Data, error,
    http::header::HeaderValue,
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    Error, HttpMessage,
};
use futures_util::future::LocalBoxFuture;
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
        ready(Ok(TenantMiddleware { service: Rc::new(service) }))
    }
}

pub struct TenantMiddleware<S> {
    service: Rc<S>,
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
                    return Err(error::ErrorInternalServerError(""))
                }
            };

            if app_state.enable_tenant_and_scope {
                let tenant = req
                    .headers()
                    .get("x-tenant")
                    .map_or(None, |header_value: &HeaderValue| header_value.to_str().ok())
                    .map(|header_str| header_str.to_string());

                let validated_tenant: Tenant = match tenant {
                    Some(val) if app_state.tenants.contains(&val) => Tenant(val),
                    Some(_) => {
                        return Err(error::ErrorBadRequest("invalid x-tenant value"));
                    },
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