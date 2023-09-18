use std::future::{ready, Ready};

use crate::service::types::AppScope;
use actix_web::{
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    Error, HttpMessage,
};
use futures_util::future::LocalBoxFuture;

pub struct AppExecutionScope {
    scope: AppScope,
}

impl AppExecutionScope {
    pub fn new(scope: AppScope) -> Self {
        AppExecutionScope { scope }
    }
}

impl<S, B> Transform<S, ServiceRequest> for AppExecutionScope
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error>,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type InitError = ();
    type Transform = AppExecutionScopeMiddleware<S>;
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(AppExecutionScopeMiddleware {
            service,
            scope: self.scope,
        }))
    }
}

pub struct AppExecutionScopeMiddleware<S> {
    service: S,
    scope: AppScope,
}

impl<S, B> Service<ServiceRequest> for AppExecutionScopeMiddleware<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error>,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

    forward_ready!(service);

    fn call(&self, req: ServiceRequest) -> Self::Future {
        req.extensions_mut().insert(self.scope);
        let future = self.service.call(req);

        Box::pin(async move {
            let res = future.await?;
            Ok(res)
        })
    }
}
