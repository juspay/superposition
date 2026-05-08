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
        let path_extractor = |path: &str, position: usize| {
            path.split('/')
                .filter(|p| !p.is_empty())
                .nth(position)
                .map(String::from)
        };
        let path = request.path();

        let workspace = header_extractor(headers, "x-workspace").unwrap_or_else(|| {
            path_extractor(path, 1).unwrap_or_else(|| "no-workspace-header".to_string())
        });
        let org = header_extractor(headers, "x-org-id").unwrap_or_else(|| {
            path_extractor(path, 0).unwrap_or_else(|| "no-org-header".to_string())
        });
        let request_id = header_extractor(headers, "x-request-id");
        let method = request.method().to_string();
        if let Some(request_id) = request_id {
            tracing_actix_web::root_span!(request, request_id = %request_id, workspace, org, method, path,)
        } else {
            tracing_actix_web::root_span!(request, workspace, org, method, path,)
        }
    }

    fn on_request_end<B: MessageBody>(
        span: Span,
        outcome: &Result<ServiceResponse<B>, Error>,
    ) {
        DefaultRootSpanBuilder::on_request_end(span, outcome);
    }
}
