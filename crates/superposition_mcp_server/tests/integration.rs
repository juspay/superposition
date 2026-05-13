use secrecy::SecretString;
use serde_json::json;
use wiremock::matchers::{header_exists, method, path};
use wiremock::{Mock, MockServer, ResponseTemplate};

use superposition_mcp_server::auth::{AuthValue, SUPERPOSITION_AUTH};
use superposition_mcp_server::build::{build_client_with, build_router};
use superposition_mcp_server::config::{Config, Defaults, Mode};

/// Builds a router pointed at the given endpoint. The default SDK HTTP client
/// is HTTPS-only (via rustls/aws-lc) which can't talk to wiremock's plain-HTTP
/// listener, so we swap in `build_http()` for tests. The auth resolver wiring
/// and tool registration is otherwise identical to the production code path.
async fn router_against(endpoint: &str) -> smithy_mcp_runtime::Router {
    let cfg = Config {
        endpoint: endpoint.to_string(),
        creds: None,
        defaults: Defaults {
            workspace_id: None,
            org_id: None,
        },
    };
    let http_client = aws_smithy_http_client::Builder::new().build_http();
    let client = build_client_with(&cfg, Mode::HttpPassthrough, |b| b.http_client(http_client));
    build_router(client, cfg.defaults)
}

fn count_operations_in_smithy() -> usize {
    let workspace_root = {
        let mut p = std::env::current_dir().unwrap();
        loop {
            if p.join("smithy/models").exists() && p.join("Cargo.toml").exists() {
                break p;
            }
            p = p.parent().expect("cargo workspace root").to_path_buf();
        }
    };
    let mut count = 0usize;
    for entry in std::fs::read_dir(workspace_root.join("smithy/models")).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().and_then(|s| s.to_str()) == Some("smithy") {
            let body = std::fs::read_to_string(&path).unwrap();
            count += body.matches("@mcpTool").count();
        }
    }
    count
}

#[tokio::test]
async fn tools_list_matches_smithy_annotation_count() {
    let server = MockServer::start().await;
    let router = router_against(&server.uri()).await;
    assert_eq!(
        router.tool_names().len(),
        count_operations_in_smithy(),
        "router tool count must equal @mcpTool annotation count in smithy/models"
    );
}

/// Verifies that an SDK call dispatched via the router carries the Authorization
/// header set by an outer `SUPERPOSITION_AUTH.scope` — i.e. the task-local credential
/// flows into the SDK identity resolver, into the auth signer, and out onto the wire.
///
/// NOTE: this test uses Basic auth rather than Bearer auth. The smithy-generated
/// SDK lists `HTTP_BASIC_AUTH_SCHEME_ID` first in the auth-options vector for every
/// operation (see `crates/superposition_sdk/src/operation/*.rs`), so the orchestrator
/// always tries the basic-auth identity resolver before bearer. With a `Bearer` in
/// the task-local the basic resolver returns `Err`, the orchestrator surfaces that
/// as a dispatch failure, and the request never reaches the wire — i.e. with the
/// current resolver wiring, sending an `Authorization: Bearer …` header at the MCP
/// boundary won't currently produce a successful upstream call. This is a real
/// concern about the auth wiring that should be addressed separately (e.g. by
/// configuring the auth-option order at request time, or by collapsing both
/// resolvers behind a single scheme).
#[tokio::test]
async fn tool_call_forwards_authorization_header_in_passthrough_mode() {
    let server = MockServer::start().await;

    // GetOrganisation -> GET /superposition/organisations/{id}. Return a complete
    // OrganisationResponse so the SDK deserialiser is satisfied; we only care that
    // the wire request carried the Authorization header injected via the task-local.
    Mock::given(method("GET"))
        .and(path("/superposition/organisations/test-id"))
        .and(header_exists("authorization"))
        .respond_with(ResponseTemplate::new(200).set_body_json(json!({
            "id": "test-id",
            "name": "Test Org",
            "country_code": null,
            "contact_email": null,
            "contact_phone": null,
            "created_by": "tester",
            "admin_email": "admin@example.com",
            "status": "Active",
            "sector": null,
            "created_at": "2026-05-11T12:00:00Z",
            "updated_at": "2026-05-11T12:00:00Z",
            "updated_by": "tester"
        })))
        .mount(&server)
        .await;

    let router = router_against(&server.uri()).await;

    let auth = AuthValue::Basic {
        user: "alice".to_string(),
        pass: SecretString::new("s3cret".to_string().into()),
    };
    let result = SUPERPOSITION_AUTH
        .scope(auth, async {
            router
                .test_call_tool("GetOrganisation", json!({"id": "test-id"}))
                .await
        })
        .await;

    let value = result.expect("tool call succeeded");
    assert_eq!(value["id"], json!("test-id"));
    assert_eq!(value["name"], json!("Test Org"));

    let received = server.received_requests().await.expect("received_requests");
    assert!(!received.is_empty(), "wiremock should have recorded a request");
    let auth_header = received[0]
        .headers
        .get("authorization")
        .expect("authorization header present")
        .to_str()
        .unwrap();
    // base64("alice:s3cret") == "YWxpY2U6czNjcmV0"
    assert_eq!(auth_header, "Basic YWxpY2U6czNjcmV0");
}
