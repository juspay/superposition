use actix_web::body::MessageBody;
use actix_web::dev::{
    forward_ready, Payload, Service, ServiceRequest, ServiceResponse, Transform,
};
use actix_web::web::Bytes;
use actix_web::{error::PayloadError, http::StatusCode, Error};
use futures_util::future::{ok, Ready};
use futures_util::stream::{once, StreamExt};
use log::{trace, warn};
use std::future::Future;
use std::pin::Pin;
use std::rc::Rc;
use std::task::{Context, Poll};

#[derive(Default)]
pub struct RequestResponseLogger;

// Custom body wrapper for logging response bodies
pub struct LoggingBody<B> {
    inner: B,
    status: StatusCode,
    headers: Vec<String>,
    body_bytes: Vec<u8>,
    consumed: bool,
}

impl<B> LoggingBody<B> {
    fn new(body: B, status: StatusCode, headers: Vec<String>) -> Self {
        Self {
            inner: body,
            status,
            headers,
            body_bytes: Vec::new(),
            consumed: false,
        }
    }
}

impl<B> MessageBody for LoggingBody<B>
where
    B: MessageBody + Unpin,
{
    type Error = B::Error;

    fn size(&self) -> actix_web::body::BodySize {
        self.inner.size()
    }

    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
    ) -> Poll<Option<Result<Bytes, Self::Error>>> {
        let this = self.get_mut();
        let inner = Pin::new(&mut this.inner);

        match inner.poll_next(cx) {
            Poll::Ready(Some(Ok(chunk))) => {
                this.body_bytes.extend_from_slice(&chunk);
                Poll::Ready(Some(Ok(chunk)))
            }
            Poll::Ready(Some(Err(e))) => Poll::Ready(Some(Err(e))),
            Poll::Ready(None) => {
                if !this.consumed {
                    this.consumed = true;
                    // Log the complete response body
                    let response_body = if this.body_bytes.is_empty() {
                        String::from("(empty)")
                    } else {
                        String::from_utf8(this.body_bytes.clone()).unwrap_or_else(|_| {
                            format!("(binary data, {} bytes)", this.body_bytes.len())
                        })
                    };

                    trace!(
                        "RESPONSE: {} headers=[{}] body={}",
                        this.status,
                        this.headers.join(", "),
                        response_body
                    );
                }
                Poll::Ready(None)
            }
            Poll::Pending => Poll::Pending,
        }
    }
}

impl<S, B> Transform<S, ServiceRequest> for RequestResponseLogger
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: MessageBody + Unpin + 'static,
{
    type Response = ServiceResponse<LoggingBody<B>>;
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
    B: MessageBody + Unpin + 'static,
{
    type Response = ServiceResponse<LoggingBody<B>>;
    type Error = Error;
    type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>>>>;

    forward_ready!(service);

    fn call(&self, req: ServiceRequest) -> Self::Future {
        let service = self.service.clone();

        Box::pin(async move {
            // to hold responses in all cases for logging
            let res: ServiceResponse<B>;

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
            let should_log_request_body =
                matches!(method.as_str(), "PUT" | "POST" | "PATCH");

            if should_log_request_body {
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

                trace!(
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
                #[rustfmt::skip]
                let new_payload = if body_bytes.is_empty() {
                    Payload::None
                } else {
                    let bytes = Bytes::from(body_bytes);
                    let stream = once(async move { Ok::<Bytes, PayloadError>(bytes) });
                    Payload::from(Box::pin(stream) as Pin< Box< dyn futures_util::Stream< Item = Result<Bytes, PayloadError> > > >)
                };
                let new_req = ServiceRequest::from_parts(http_req, new_payload);

                // Call the next service
                res = service.call(new_req).await?;
            } else {
                // For GET/DELETE etc, don't extract body
                trace!(
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
                res = service.call(req).await?;
            }

            // Log response details
            let status = res.status();
            let mut response_headers = Vec::new();
            for (name, value) in res.headers().iter() {
                if let Ok(value_str) = value.to_str() {
                    response_headers.push(format!("{}: {}", name, value_str));
                }
            }

            // Wrap the response body with our logging wrapper
            let logged_res =
                res.map_body(|_, body| LoggingBody::new(body, status, response_headers));

            Ok(logged_res)
        })
    }
}
