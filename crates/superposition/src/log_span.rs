use actix_http::header::{HeaderMap, HeaderValue};
use actix_web::{
    Error,
    body::MessageBody,
    dev::{ServiceRequest, ServiceResponse},
};
use tracing::Span;
use tracing_actix_web::{DefaultRootSpanBuilder, RootSpanBuilder};

pub struct CustomRootSpanBuilder;

impl RootSpanBuilder for CustomRootSpanBuilder {
    fn on_request_start(request: &ServiceRequest) -> Span {
        let headers = request.headers();
        let santize_headers = |value: &str| {
            value
                .chars()
                .filter(|c| !c.is_control())
                .take(256)
                .collect::<String>()
        };
        let header_extractor = |headers: &HeaderMap, key: &str| {
            headers
                .get(key)
                .and_then(|v| HeaderValue::to_str(v).ok())
                .map(santize_headers)
        };
        let workspace = header_extractor(headers, "x-workspace")
            .unwrap_or_else(|| "no-workspace-header".to_string());
        let org = header_extractor(headers, "x-org-id")
            .unwrap_or_else(|| "no-org-header".to_string());
        let user_agent = header_extractor(headers, "user-agent")
            .unwrap_or_else(|| "no-user-agent".to_string());
        let method = request.method().to_string();
        let path = request.path();
        tracing_actix_web::root_span!(request, workspace, org, user_agent, method, path,)
    }

    fn on_request_end<B: MessageBody>(
        span: Span,
        outcome: &Result<ServiceResponse<B>, Error>,
    ) {
        DefaultRootSpanBuilder::on_request_end(span, outcome);
        // let cac_span =
        //     tracing::span!(Level::INFO, "app", service = "superposition",);
        // let _span_entered = cac_span.enter();
    }
}
