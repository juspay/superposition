use actix_web::HttpMessage;
use actix_web::body::MessageBody;
use actix_web::dev::{
    Payload, Service, ServiceRequest, ServiceResponse, Transform, forward_ready,
};
use actix_web::http::header::ContentType;
use actix_web::web::Bytes;
use actix_web::{Error, error::PayloadError, http::header};
use futures_util::future::{Ready, ok};
use futures_util::stream::{StreamExt, once};
use serde_json::Value;
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::rc::Rc;
use std::task::{Context, Poll};
use std::time::Instant;
use tracing::{info, trace, warn};

#[derive(Default)]
pub struct RequestResponseLogger;

// Custom body wrapper for logging request/response bodies
pub struct LoggingBody<B> {
    inner: B,
    headers: HashMap<String, String>,
    body_bytes: Vec<u8>,
    consumed: bool,
    start_time: Instant,
}

impl<B> LoggingBody<B> {
    fn new(body: B, headers: HashMap<String, String>, start_time: Instant) -> Self {
        Self {
            inner: body,
            headers,
            body_bytes: Vec::new(),
            consumed: false,
            start_time,
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
                    let latency_ms = this.start_time.elapsed().as_millis() as u64;
                    let is_json_response = this
                        .headers
                        .get(header::CONTENT_TYPE.as_str())
                        .map(|ct| ct.starts_with(&ContentType::json().to_string()))
                        .unwrap_or(false);
                    let response_body = if is_json_response {
                        serde_json::from_slice::<Value>(&this.body_bytes).unwrap_or_else(
                            |err| {
                                tracing::warn!(
                                    "Failed to parse response body, error: {}",
                                    err
                                );
                                let response_body = if this.body_bytes.is_empty() {
                                    String::from("(empty)")
                                } else {
                                    String::from_utf8_lossy(&this.body_bytes).into_owned()
                                };
                                Value::String(format!(
                                    "(invalid JSON: {})",
                                    response_body
                                ))
                            },
                        )
                    } else {
                        Value::String("(non-JSON response body omitted)".to_string())
                    };
                    trace!(
                        body = %response_body,
                        "ResponseSignal"
                    );
                    info!(latency = latency_ms, "GoldenSignal");
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
            let mut res: ServiceResponse<B>;

            let query_string = req.query_string().to_string();

            let request_id = req
                .extensions()
                .get::<tracing_actix_web::RequestId>()
                .map(|req_id| header::HeaderValue::from_str(&req_id.to_string()));

            let (http_req, mut payload) = req.into_parts();
            let mut body_bytes = Vec::new();

            while let Some(chunk) = payload.next().await {
                match chunk {
                    Ok(bytes) => body_bytes.extend_from_slice(&bytes),
                    Err(e) => {
                        warn!("Error reading request body: {}", e);
                        break;
                    }
                }
            }

            // Try to parse as JSON and log as structured data
            let request_body_value = if !body_bytes.is_empty() {
                serde_json::from_slice::<Value>(&body_bytes).ok()
            } else {
                None
            };

            if let Some(ref json) = request_body_value {
                // Successfully parsed as JSON - log as structured
                trace!(
                    query = %if query_string.is_empty() {
                        "(none)".to_string()
                    } else {
                        query_string.clone()
                    },
                    body = ?json,
                    "RequestSignal"
                );
            } else {
                // Not JSON - log as string
                let request_body = if body_bytes.is_empty() {
                    String::from("(empty)")
                } else {
                    String::from_utf8_lossy(&body_bytes).into_owned()
                };

                trace!(
                    query = %if query_string.is_empty() {
                        "(none)".to_string()
                    } else {
                        query_string.clone()
                    },
                    body = %request_body,
                    "RequestSignal"
                );
            }

            let new_payload = if body_bytes.is_empty() {
                Payload::None
            } else {
                let bytes = Bytes::from(body_bytes);
                let stream = once(async move { Ok::<Bytes, PayloadError>(bytes) });
                Payload::from(Box::pin(stream)
                    as Pin<
                        Box<dyn futures_util::Stream<Item = Result<Bytes, PayloadError>>>,
                    >)
            };
            let new_req = ServiceRequest::from_parts(http_req, new_payload);
            let start_time = Instant::now();
            res = service.call(new_req).await?;

            if let Some(Ok(request_id)) = request_id {
                res.headers_mut()
                    .insert(header::HeaderName::from_static("x-request-id"), request_id);
            }
            let response_headers: HashMap<String, String> = res
                .headers()
                .iter()
                .filter_map(|(name, value)| {
                    value
                        .to_str()
                        .ok()
                        .map(|v| (name.as_str().to_string(), v.to_string()))
                })
                .collect();

            let logged_res = res
                .map_body(|_, body| LoggingBody::new(body, response_headers, start_time));

            Ok(logged_res)
        })
    }
}
