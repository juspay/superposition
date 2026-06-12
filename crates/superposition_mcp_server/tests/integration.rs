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

/// Stand up wiremock with a `GetOrganisation` stub that asserts an authorization
/// header is present and returns a minimal valid `OrganisationResponse`.
async fn stub_get_organisation(server: &MockServer) {
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
        .mount(server)
        .await;
}

async fn call_get_organisation(
    server_uri: &str,
    auth: AuthValue,
) -> serde_json::Value {
    let server_uri = server_uri.to_string();
    let router = router_against(&server_uri).await;
    let result = SUPERPOSITION_AUTH
        .scope(auth, async {
            router
                .test_call_tool("GetOrganisation", json!({"id": "test-id"}))
                .await
        })
        .await;
    result.expect("tool call succeeded")
}

/// Verifies Basic-auth credentials in the task-local flow through to the wire.
#[tokio::test]
async fn passthrough_basic_auth_reaches_the_wire() {
    let server = MockServer::start().await;
    stub_get_organisation(&server).await;

    let auth = AuthValue::Basic {
        user: "alice".to_string(),
        pass: SecretString::new("s3cret".to_string().into()),
    };
    let value = call_get_organisation(&server.uri(), auth).await;
    assert_eq!(value["id"], json!("test-id"));

    let received = server.received_requests().await.expect("received_requests");
    let auth_header = received[0]
        .headers
        .get("authorization")
        .expect("authorization header present")
        .to_str()
        .unwrap();
    // base64("alice:s3cret") == "YWxpY2U6czNjcmV0"
    assert_eq!(auth_header, "Basic YWxpY2U6czNjcmV0");
}

/// Verifies Bearer-token credentials in the task-local flow through to the wire.
///
/// Regression test: previously, the smithy-generated SDK listed
/// `HTTP_BASIC_AUTH_SCHEME_ID` first in every operation's auth-options vector,
/// and the orchestrator does not fall back to subsequent schemes when the first
/// resolver returns `Err`. So a `Bearer` value in the task-local hit
/// `BasicResolver` first, errored, and never reached the wire. Fixed by
/// installing a `TaskLocalAuthSchemeResolver` that picks the scheme per request
/// based on the credential variant in `SUPERPOSITION_AUTH`.
#[tokio::test]
async fn passthrough_bearer_auth_reaches_the_wire() {
    let server = MockServer::start().await;
    stub_get_organisation(&server).await;

    let auth = AuthValue::Bearer(SecretString::new("tok-abc".to_string().into()));
    let value = call_get_organisation(&server.uri(), auth).await;
    assert_eq!(value["id"], json!("test-id"));

    let received = server.received_requests().await.expect("received_requests");
    let auth_header = received[0]
        .headers
        .get("authorization")
        .expect("authorization header present")
        .to_str()
        .unwrap();
    assert_eq!(auth_header, "Bearer tok-abc");
}
