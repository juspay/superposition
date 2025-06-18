use std::{
    future::{ready, Ready},
    rc::Rc,
};

use actix_web::{
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    Error, HttpMessage,
};
use futures_util::future::LocalBoxFuture;

use crate::service::types::Resource;

pub struct ResourceMiddlewareFactory {
    resource: Resource,
}

impl ResourceMiddlewareFactory {
    pub fn new(resource: Resource) -> Self {
        ResourceMiddlewareFactory { resource }
    }
}

impl<S, B> Transform<S, ServiceRequest> for ResourceMiddlewareFactory
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type InitError = ();
    type Transform = ResourceMiddleware<S>;
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(ResourceMiddleware {
            service: Rc::new(service),
            resource: self.resource,
        }))
    }
}

pub struct ResourceMiddleware<S> {
    service: Rc<S>,
    resource: Resource,
}

impl<S, B> Service<ServiceRequest> for ResourceMiddleware<S>
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
        req.extensions_mut().insert(self.resource);
        Box::pin(self.service.call(req))
    }
}
