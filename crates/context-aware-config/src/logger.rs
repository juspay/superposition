use actix_http::header::{HeaderMap, HeaderValue};
use actix_web::{
    body::MessageBody,
    dev::{ServiceRequest, ServiceResponse},
    Error,
};
use service_utils::helpers::get_pod_info;
use tracing_utils::tracing_actix_web::{DefaultRootSpanBuilder, RootSpanBuilder};
use tracing_utils::tracing_subscriber::filter::EnvFilter;
use tracing_utils::{
    tracing::{span, Level, Span},
    tracing_actix_web,
};

pub struct CustomRootSpanBuilder;

impl RootSpanBuilder for CustomRootSpanBuilder {
    fn on_request_start(request: &ServiceRequest) -> Span {
        let (pod_identifier, deployment_id) = get_pod_info();
        let headers = request.headers();
        let extractor_header = |headers: &HeaderMap, key: &str, default: &str| {
            headers
                .get(key)
                .map(HeaderValue::to_str)
                .unwrap_or(Ok(default))
                .unwrap_or(default)
                .to_owned()
        };
        let tenant = extractor_header(headers, "x-tenant", "no-tenant-header");
        let user_agent = extractor_header(headers, "user-agent", "no-user-agent");
        let method = request.method().to_string();
        let path = request.path();
        tracing_actix_web::root_span!(
            request,
            service = "context-aware-config",
            tenant,
            user_agent,
            method,
            path,
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

pub fn init_log_subscriber() {
    let subscriber = tracing_utils::tracing_subscriber::fmt::Subscriber::builder()
        .with_env_filter(EnvFilter::from_default_env());
    if Ok(String::from("DEV")) == std::env::var("APP_ENV") {
        subscriber.compact().init();
    } else {
        subscriber.json().init();
    }
}
