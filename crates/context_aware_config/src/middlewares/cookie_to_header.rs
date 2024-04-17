use actix_web::{
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    http::header::{HeaderName, HeaderValue},
    Error,
};
use futures_util::future::LocalBoxFuture;
use std::future::{ready, Ready};
use std::rc::Rc;

pub struct CookieToHeader;

impl<S, B> Transform<S, ServiceRequest> for CookieToHeader
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type InitError = ();
    type Transform = CookieToHeaderMiddleware<S>;
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(CookieToHeaderMiddleware {
            service: Rc::new(service),
            // other fields if required
        }))
    }
}

pub struct CookieToHeaderMiddleware<S> {
    service: Rc<S>,
}

impl<S, B> Service<ServiceRequest> for CookieToHeaderMiddleware<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

    forward_ready!(service);

    fn call(&self, mut req: ServiceRequest) -> Self::Future {
        let srv = self.service.clone();
        Box::pin(async move {
            if let Some(cookie) = req.cookie("token") {
                let token_value = cookie.value().to_string();
                req.headers_mut().insert(
                    HeaderName::from_static("authorization"),
                    HeaderValue::from_str(&format!("Bearer {}", token_value))
                        .unwrap_or_else(|_| HeaderValue::from_static("invalid")),
                );
            }
            srv.call(req).await
        })
    }
}
