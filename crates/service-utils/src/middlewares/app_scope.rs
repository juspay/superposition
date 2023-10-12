use std::future::{ready, Ready};

use crate::service::types::AppScope;
use actix_web::{
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    Error, HttpMessage,
};
use futures_util::future::LocalBoxFuture;

use std::rc::Rc;

pub struct AppExecutionScopeMiddlewareFactory {
    scope: AppScope,
}

impl AppExecutionScopeMiddlewareFactory {
    pub fn new(scope: AppScope) -> Self {
        AppExecutionScopeMiddlewareFactory { scope: scope }
    }
}

impl<S, B> Transform<S, ServiceRequest> for AppExecutionScopeMiddlewareFactory
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
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
            service: Rc::new(service),
            scope: self.scope.clone(),
        }))
    }
}

pub struct AppExecutionScopeMiddleware<S> {
    service: Rc<S>,
    scope: AppScope,
}

impl<S, B> Service<ServiceRequest> for AppExecutionScopeMiddleware<S>
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
        let scope = self.scope;

        Box::pin(async move {
            req.extensions_mut().insert(scope);
            let res = srv.call(req).await?;

            Ok(res)
        })
    }
}
