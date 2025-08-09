use actix_web::dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform};
use actix_web::Error;
use futures_util::future::{ok, Ready};
use log::info;
use std::future::Future;
use std::pin::Pin;
use std::rc::Rc;

pub struct RequestResponseLogger;

impl Default for RequestResponseLogger {
    fn default() -> Self {
        RequestResponseLogger
    }
}

impl<S, B> Transform<S, ServiceRequest> for RequestResponseLogger
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type InitError = ();
    type Transform = RequestResponseLoggerMiddleware<S>;
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ok(RequestResponseLoggerMiddleware {
            service: Rc::new(service),
        })
    }
}

pub struct RequestResponseLoggerMiddleware<S> {
    service: Rc<S>,
}

impl<S, B> Service<ServiceRequest> for RequestResponseLoggerMiddleware<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>>>>;

    forward_ready!(service);

    fn call(&self, req: ServiceRequest) -> Self::Future {
        let service = self.service.clone();

        Box::pin(async move {
            // Log request details
            let method = req.method().to_string();
            let uri = req.uri().to_string();
            let query_string = req.query_string();
            
            // Log request headers
            let mut headers = Vec::new();
            for (name, value) in req.headers().iter() {
                if let Ok(value_str) = value.to_str() {
                    headers.push(format!("{}: {}", name, value_str));
                }
            }

            info!(
                "REQUEST: {} {} query={} headers=[{}]",
                method,
                uri,
                if query_string.is_empty() { "(none)" } else { query_string },
                headers.join(", ")
            );

            // Call the next service
            let res = service.call(req).await?;

            // Log response details
            let status = res.status();
            let mut response_headers = Vec::new();
            for (name, value) in res.headers().iter() {
                if let Ok(value_str) = value.to_str() {
                    response_headers.push(format!("{}: {}", name, value_str));
                }
            }

            info!(
                "RESPONSE: {} headers=[{}]",
                status,
                response_headers.join(", ")
            );

            Ok(res)
        })
    }
}