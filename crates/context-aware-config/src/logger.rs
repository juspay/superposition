use crate::helpers::parse_headermap_safe;
use actix_web::{
    body::MessageBody,
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    web::BytesMut,
    Error, HttpMessage,
};
use futures::future::{ready, LocalBoxFuture, Ready};
use futures::{FutureExt, StreamExt};
use serde_json::{json, Value};
use service_utils::helpers::get_pod_info;
use std::{cell::RefCell, rc::Rc};
use tracing::{span, Level, Span};
use tracing_actix_web::{DefaultRootSpanBuilder, RootSpanBuilder};
use tracing_subscriber::filter::EnvFilter;

//---------------- GoldenSignal Middleware <start> -----------------

// There are two steps in middleware processing.
// 1. Middleware initialization, middleware factory gets called with
//    next service in chain as parameter.
// 2. Middleware's call method gets called with normal request.

pub struct GoldenSignalFactory;

// Middleware factory is `Transform` trait
// `S` - type of the next service
// `B` - type of response's body
impl<S, B> Transform<S, ServiceRequest> for GoldenSignalFactory
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static + std::fmt::Debug,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type InitError = ();
    type Transform = GoldenSignal<S>;
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(GoldenSignal {
            service: Rc::new(RefCell::new(service)),
        }))
    }
}

pub struct GoldenSignal<S> {
    service: Rc<RefCell<S>>,
}

impl<S, B> Service<ServiceRequest> for GoldenSignal<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static + std::fmt::Debug,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

    forward_ready!(service);

    fn call(&self, mut req: ServiceRequest) -> Self::Future {
        let svc = self.service.clone();

        async move {
            let mut body = BytesMut::new();
            let mut payload = req.take_payload();
            while let Some(chunk) = payload.next().await {
                let chunk = chunk?;
                body.extend_from_slice(&chunk);
            }

            let (_payload_sender, mut payload) = actix_http::h1::Payload::create(true);
            payload.unread_data(body.clone().into());
            req.set_payload(payload.into());

            let req_headers = parse_headermap_safe(req.headers());
            let res = svc.call(req).await?;
            let res_headers = parse_headermap_safe(res.headers());
            let res_body = res.response().body();
            let req_body = String::from_utf8(body.freeze().to_vec())
                .and_then(|s| {
                    Ok(serde_json::from_str::<serde_json::Value>(s.as_str())
                        .unwrap_or(Value::Null))
                })
                .unwrap_or(Value::Null);

            tracing::info!(
                request_body = format!("{}", req_body),
                request_headers = format!("{}", json!(req_headers)),
                reponse_body = format!("{:?}", res_body),
                response_headers = format!("{}", json!(res_headers)),
                http.status_code = res.status().as_u16(),
                "GoldenSignal",
            );
            Ok(res)
        }
        .boxed_local()
    }
}

//---------------- GoldenSignal Middleware <end> -----------------

pub struct CustomRootSpanBuilder;

impl RootSpanBuilder for CustomRootSpanBuilder {
    fn on_request_start(request: &ServiceRequest) -> Span {
        let (pod_identifier, deployment_id) = get_pod_info();
        tracing_actix_web::root_span!(
            request,
            service = "context-aware-config",
            pod_id = pod_identifier,
            deployment_id = deployment_id
        )
    }

    fn on_request_end<B: MessageBody>(
        span: Span,
        outcome: &Result<ServiceResponse<B>, Error>,
    ) {
        DefaultRootSpanBuilder::on_request_end(span, outcome);
        let (pod_identifier, deployment_id) = get_pod_info();
        let cac_span = span!(
            Level::INFO,
            "app",
            service = "context-aware-config",
            pod_id = pod_identifier,
            deployment_id = deployment_id
        );
        let _span_entered = cac_span.enter();
    }
}

//let custom_middleware = TracingLogger::<CustomRootSpanBuilder>::new();

pub fn init_log_subscriber() {
    let subscriber = tracing_subscriber::fmt::Subscriber::builder()
        .with_env_filter(EnvFilter::from_default_env());
    if Ok(String::from("DEV")) == std::env::var("APP_ENV") {
        subscriber.compact().init();
    } else {
        subscriber.json().init();
    }
}
