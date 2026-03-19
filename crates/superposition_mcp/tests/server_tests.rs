use superposition_mcp::config::McpServerConfig;
use superposition_mcp::SuperpositionMcpServer;

fn test_config() -> McpServerConfig {
    McpServerConfig {
        endpoint_url: "http://localhost:8080".to_string(),
        workspace_id: "test_workspace".to_string(),
        org_id: "test_org".to_string(),
        auth_token: None,
    }
}

fn test_config_with_token() -> McpServerConfig {
    McpServerConfig {
        auth_token: Some("test_token".to_string()),
        ..test_config()
    }
}

#[test]
fn test_config_fields() {
    let config = test_config();
    assert_eq!(config.endpoint_url, "http://localhost:8080");
    assert_eq!(config.workspace_id, "test_workspace");
    assert_eq!(config.org_id, "test_org");
    assert_eq!(config.auth_token, None);
}

#[test]
fn test_config_with_auth_token() {
    let config = test_config_with_token();
    assert_eq!(config.auth_token, Some("test_token".to_string()));
}

#[test]
fn test_server_construction() {
    let config = test_config();
    let server = SuperpositionMcpServer::new(config);
    assert_eq!(server.config.workspace_id, "test_workspace");
    assert_eq!(server.config.org_id, "test_org");
}

#[test]
fn test_server_construction_with_token() {
    let config = test_config_with_token();
    let server = SuperpositionMcpServer::new(config);
    assert_eq!(
        server.config.auth_token,
        Some("test_token".to_string())
    );
}

#[test]
fn test_server_info() {
    use rmcp::ServerHandler;

    let server = SuperpositionMcpServer::new(test_config());
    let info = server.get_info();

    assert!(info.instructions.is_some());
    let instructions = info.instructions.unwrap();
    assert!(instructions.contains("Superposition MCP Server"));
    assert!(instructions.contains("feature flags"));
}

#[test]
fn test_server_capabilities_include_tools() {
    use rmcp::ServerHandler;

    let server = SuperpositionMcpServer::new(test_config());
    let info = server.get_info();

    // The server should advertise tool capabilities
    assert!(info.capabilities.tools.is_some());
}

#[test]
fn test_server_clone() {
    let server = SuperpositionMcpServer::new(test_config());
    let cloned = server.clone();
    assert_eq!(cloned.config.workspace_id, server.config.workspace_id);
    assert_eq!(cloned.config.org_id, server.config.org_id);
}

#[test]
fn test_config_build_sdk_client() {
    // Should not panic
    let config = test_config();
    let _client = config.build_sdk_client();
}

#[test]
fn test_config_build_sdk_client_with_token() {
    // Should not panic even with auth token
    let config = test_config_with_token();
    let _client = config.build_sdk_client();
}
