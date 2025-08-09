use actix_web::dev::{
    forward_ready, Payload, Service, ServiceRequest, ServiceResponse, Transform,
};
use actix_web::web::Bytes;
use actix_web::{error::PayloadError, Error};
use futures_util::future::{ok, Ready};
use futures_util::stream::{once, StreamExt};
use log::{info, warn};
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
            let query_string = req.query_string().to_string();

            // Log request headers
            let mut headers = Vec::new();
            for (name, value) in req.headers().iter() {
                if let Ok(value_str) = value.to_str() {
                    headers.push(format!("{}: {}", name, value_str));
                }
            }

            // For PUT/POST requests, extract and log the body
            let should_log_body = matches!(method.as_str(), "PUT" | "POST" | "PATCH");

            if should_log_body {
                // Extract the request and payload
                let (http_req, mut payload) = req.into_parts();
                let mut body_bytes = Vec::new();

                // Read the payload into a buffer
                while let Some(chunk) = payload.next().await {
                    match chunk {
                        Ok(bytes) => body_bytes.extend_from_slice(&bytes),
                        Err(e) => {
                            warn!("Error reading request body: {}", e);
                            break;
                        }
                    }
                }

                // Convert body to string for logging
                let request_body = if body_bytes.is_empty() {
                    String::from("(empty)")
                } else {
                    String::from_utf8(body_bytes.clone()).unwrap_or_else(|_| {
                        format!("(binary data, {} bytes)", body_bytes.len())
                    })
                };

                info!(
                    "REQUEST: {} {} query={} headers=[{}] body={}",
                    method,
                    uri,
                    if query_string.is_empty() {
                        "(none)"
                    } else {
                        &query_string
                    },
                    headers.join(", "),
                    request_body
                );

                // Reconstruct the request with the body
                let new_payload = if body_bytes.is_empty() {
                    Payload::None
                } else {
                    let bytes = Bytes::from(body_bytes);
                    let stream = once(async move { Ok::<Bytes, PayloadError>(bytes) });
                    Payload::from(Box::pin(stream)
                        as Pin<
                            Box<
                                dyn futures_util::Stream<
                                    Item = Result<Bytes, PayloadError>,
                                >,
                            >,
                        >)
                };

                let new_req = ServiceRequest::from_parts(http_req, new_payload);

                // Call the next service
                let res = service.call(new_req).await?;

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
            } else {
                // For GET/DELETE etc, don't extract body
                info!(
                    "REQUEST: {} {} query={} headers=[{}]",
                    method,
                    uri,
                    if query_string.is_empty() {
                        "(none)"
                    } else {
                        &query_string
                    },
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
            }
        })
    }
}
