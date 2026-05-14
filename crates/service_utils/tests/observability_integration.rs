//! End-to-end test: an Actix app wrapped with MetricsMiddleware serves several
//! routes; we then issue requests and parse the Prometheus scrape output to
//! assert on the metrics that should appear.

use actix_web::{App, HttpResponse, http::StatusCode, test, web};
use prometheus::Encoder;
use service_utils::observability::{
    LabelConfig, MetricsMiddleware, Observability, ObservabilityConfig,
};

fn cfg() -> ObservabilityConfig {
    ObservabilityConfig {
        enabled: true,
        bind: "127.0.0.1".parse().unwrap(),
        port: 0,
        label: LabelConfig::default(),
        collect_interval: std::time::Duration::from_secs(10),
        instance_id: "it".into(),
        service_name: "sp-it".into(),
        service_version: "0".into(),
        deployment_environment: None,
        otlp_endpoint: None,
    }
}

fn scrape(obs: &Observability) -> String {
    let metric_families = obs.registry().gather();
    let mut buf = Vec::new();
    prometheus::TextEncoder::new()
        .encode(&metric_families, &mut buf)
        .unwrap();
    String::from_utf8(buf).unwrap()
}

#[actix_web::test]
async fn metrics_appear_after_requests() {
    let obs = Observability::init(cfg()).unwrap();
    let mw = MetricsMiddleware::new(&obs.meter(), LabelConfig::default());
    let app = test::init_service(
        App::new()
            .wrap(mw)
            .route(
                "/ping",
                web::get().to(|| async { HttpResponse::Ok().finish() }),
            )
            .route(
                "/echo/{name}",
                web::post().to(|p: web::Path<String>| async move {
                    HttpResponse::Created().body(p.into_inner())
                }),
            )
            .route(
                "/boom",
                web::get().to(|| async { HttpResponse::InternalServerError().finish() }),
            ),
    )
    .await;

    for _ in 0..3 {
        let req = test::TestRequest::get().uri("/ping").to_request();
        let resp = test::call_service(&app, req).await;
        assert_eq!(resp.status(), StatusCode::OK);
    }
    let req = test::TestRequest::post().uri("/echo/world").to_request();
    let resp = test::call_service(&app, req).await;
    assert_eq!(resp.status(), StatusCode::CREATED);

    let req = test::TestRequest::get().uri("/boom").to_request();
    let resp = test::call_service(&app, req).await;
    assert_eq!(resp.status(), StatusCode::INTERNAL_SERVER_ERROR);

    let req = test::TestRequest::get().uri("/no-such-route").to_request();
    let resp = test::call_service(&app, req).await;
    assert_eq!(resp.status(), StatusCode::NOT_FOUND);

    let body = scrape(&obs);

    // Request duration histogram exists with expected labels for /ping (3 hits).
    let ping_count_line = body
        .lines()
        .find(|l| {
            l.starts_with("http_server_request_duration_seconds_count{")
                && l.contains("http_route=\"/ping\"")
                && l.contains("http_request_method=\"GET\"")
                && l.contains("http_response_status_code=\"200\"")
        })
        .unwrap_or_else(|| panic!("no /ping count line in:\n{body}"));
    let ping_count: f64 = ping_count_line
        .rsplit_once(' ')
        .unwrap()
        .1
        .trim()
        .parse()
        .unwrap();
    assert_eq!(ping_count as u64, 3);

    // 5xx series for /boom appears.
    assert!(
        body.lines().any(|l| {
            l.starts_with("http_server_request_duration_seconds_count{")
                && l.contains("http_route=\"/boom\"")
                && l.contains("http_response_status_code=\"500\"")
        }),
        "no /boom 500 series in:\n{body}"
    );

    // Unmatched path uses the sentinel.
    assert!(
        body.lines().any(|l| {
            l.starts_with("http_server_request_duration_seconds_count{")
                && l.contains("http_route=\"__not_found__\"")
        }),
        "no __not_found__ series in:\n{body}"
    );

    // busy_duration_total > 0
    let busy = body
        .lines()
        .find(|l| l.starts_with("http_server_busy_duration_seconds_total{"))
        .unwrap_or_else(|| panic!("no busy_duration line in:\n{body}"));
    let busy_value: f64 = busy.rsplit_once(' ').unwrap().1.trim().parse().unwrap();
    assert!(
        busy_value > 0.0,
        "expected busy_duration > 0, got {busy_value}"
    );

    // active_requests returns to 0 after all requests complete.
    let active_lines: Vec<_> = body
        .lines()
        .filter(|l| l.starts_with("http_server_active_requests{"))
        .collect();
    for line in &active_lines {
        let v: f64 = line.rsplit_once(' ').unwrap().1.trim().parse().unwrap();
        assert_eq!(v, 0.0, "active_requests not zero: {line}");
    }
}

#[actix_web::test]
async fn cardinality_stays_within_budget() {
    let obs = Observability::init(cfg()).unwrap();
    let mw = MetricsMiddleware::new(&obs.meter(), LabelConfig::default());
    let app = test::init_service(
        App::new()
            .wrap(mw)
            .route(
                "/a",
                web::get().to(|| async { HttpResponse::Ok().finish() }),
            )
            .route(
                "/b",
                web::get().to(|| async { HttpResponse::Ok().finish() }),
            )
            .route(
                "/c",
                web::post().to(|| async { HttpResponse::Created().finish() }),
            ),
    )
    .await;

    for _ in 0..10 {
        for path in &["/a", "/b"] {
            let req = test::TestRequest::get().uri(path).to_request();
            let _ = test::call_service(&app, req).await;
        }
        let req = test::TestRequest::post().uri("/c").to_request();
        let _ = test::call_service(&app, req).await;
    }

    let body = scrape(&obs);
    let series = body
        .lines()
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .count();

    // Budget for this scenario: 3 routes × 1 method each × 1 status × ~12
    // (10 buckets + sum + count) = ~36 series for the histogram, plus 3 for
    // busy_duration, plus 1 for active_requests, plus a few from `target_info`
    // that the prometheus exporter emits. Headroom: 200.
    assert!(
        series <= 200,
        "cardinality regression: {series} series\n{body}"
    );
}
