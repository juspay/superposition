# Superposition MCP

A Model Context Protocol (MCP) server implementation for Superposition, providing AI assistants with access to Superposition's configuration management, experimentation, and feature flag capabilities.

## Overview

The Superposition MCP server exposes Superposition's functionality through the standardized Model Context Protocol, enabling AI assistants like Claude to interact with:

- **Configuration Management**: Create, read, update, and delete application configurations
- **Feature Flags & Experiments**: Manage A/B tests and feature rollouts
- **Context & Dimensions**: Set up targeting rules and user segmentation
- **Organizations & Workspaces**: Manage multi-tenant environments
- **Audit & Versioning**: Track configuration changes and history

## Architecture

This crate can be used in two ways:

### 1. **Library Integration** (Recommended)
Integrated into the main Superposition application under the `/mcp` path:

```rust
use superposition_mcp::{initialize_mcp_service_with_config, create_mcp_routes};

// Initialize MCP service
let mcp_service = initialize_mcp_service_with_config(
    workspace, org, host, token
).await?;

// Create routes for integration
let mcp_routes = create_mcp_routes(mcp_service);
```

### 2. **Standalone Server**
Run as a separate MCP server process:

```bash
cargo run --package superposition_mcp --features standalone
```

## Installation & Setup

### Prerequisites
- Rust 1.70+
- Access to a running Superposition instance
- Valid API credentials for Superposition

### Environment Variables

```bash
# Superposition Configuration
SUPERPOSITION_HOST=http://localhost:8080
SUPERPOSITION_DEFAULT_ORG=your-org
SUPERPOSITION_DEFAULT_WORKSPACE=dev
SUPERPOSITION_DEFAULT_TOKEN=your-api-token

# MCP Server Configuration (standalone mode)
MCP_TRANSPORT=http  # or "stdio" 
MCP_HTTP_PORT=8081  # HTTP mode only
```

### Library Usage

Add to your `Cargo.toml`:

```toml
[dependencies]
superposition_mcp = { path = "../superposition_mcp" }
```

### Standalone Usage

```bash
# HTTP transport
MCP_TRANSPORT=http cargo run --package superposition_mcp --features standalone

# stdio transport (for MCP clients)
MCP_TRANSPORT=stdio cargo run --package superposition_mcp --features standalone
```

## Available Tools

The MCP server provides comprehensive access to Superposition functionality:

### Configuration Management
- `get_resolved_config` - Get configuration with context resolution
- `create_default_config` - Create new configuration keys
- `update_default_config` - Update existing configurations
- `delete_default_config` - Remove configurations
- `list_default_configs` - List all configurations
- `get_config_fast` - Fast configuration retrieval

### Experimentation
- `create_experiment` - Set up A/B tests and experiments
- `list_experiment` - List all experiments
- `get_experiment` - Get experiment details
- `conclude_experiment` - End experiments
- `pause_experiment` / `resume_experiment` - Control experiment state
- `ramp_experiment` - Adjust experiment traffic

### Context & Targeting
- `create_context` - Set up targeting rules
- `list_contexts` - List all contexts
- `get_context` - Get context details
- `delete_context` - Remove contexts
- `create_dimension` - Create targeting dimensions
- `list_dimensions` - List available dimensions

### Organization Management
- `create_organisation` - Create new organizations
- `list_organisation` - List organizations
- `get_organisation` - Get organization details
- `update_organisation` - Update organization settings

### And many more...

## Usage Examples

### Creating a Feature Flag

```bash
curl -X POST http://localhost:8080/mcp/tools/call \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "create_default_config",
      "arguments": {
        "org_id": "my-org",
        "workspace_id": "production",
        "key": "new_checkout_flow",
        "value": false,
        "schema": {"type": "boolean"},
        "description": "Enable new checkout flow",
        "change_reason": "Rolling out improved checkout UX"
      }
    }
  }'
```

### Setting Up A/B Test

```bash
curl -X POST http://localhost:8080/mcp/tools/call \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 2,
    "method": "tools/call",
    "params": {
      "name": "create_experiment",
      "arguments": {
        "org_id": "my-org",
        "workspace_id": "production",
        "name": "checkout_button_color",
        "context": {"page": "checkout"},
        "variants": [
          {"id": "control", "variant_type": "CONTROL", "context_id": "control", "override_keys": {}},
          {"id": "test", "variant_type": "EXPERIMENTAL", "context_id": "test", "override_keys": {"button_color": "green"}}
        ],
        "change_reason": "Testing green vs blue checkout button"
      }
    }
  }'
```

### Querying Configuration with Context

```bash
curl -X POST http://localhost:8080/mcp/tools/call \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 3,
    "method": "tools/call",
    "params": {
      "name": "get_resolved_config",
      "arguments": {
        "org_id": "my-org",
        "workspace_id": "production",
        "context": {
          "user_id": "user123",
          "country": "US",
          "platform": "web"
        }
      }
    }
  }'
```

## Integration with AI Assistants

### Claude Desktop
Add to your Claude Desktop MCP configuration:

```json
{
  "mcpServers": {
    "superposition": {
      "command": "cargo",
      "args": ["run", "--package", "superposition_mcp", "--features", "standalone"],
      "cwd": "/path/to/superposition",
      "env": {
        "MCP_TRANSPORT": "stdio",
        "SUPERPOSITION_HOST": "http://localhost:8080",
        "SUPERPOSITION_DEFAULT_ORG": "your-org",
        "SUPERPOSITION_DEFAULT_WORKSPACE": "dev",
        "SUPERPOSITION_DEFAULT_TOKEN": "your-token"
      }
    }
  }
}
```

### Other MCP Clients
The server implements the standard MCP protocol and works with any MCP-compatible client.

## API Endpoints (HTTP Mode)

When running in HTTP mode, the following endpoints are available:

- `GET /mcp/health` - Health check
- `POST /mcp/initialize` - MCP initialization
- `POST /mcp/tools/list` - List available tools
- `POST /mcp/tools/call` - Execute tools
- `POST /mcp/resources/list` - List resources
- `POST /mcp/resources/read` - Read resources
- `POST /mcp/prompts/list` - List prompts

## Architecture Details

### Tool Organization
Tools are organized into logical groups using a trait-based architecture:

- **MCPTool trait**: Individual tool implementations
- **ToolsGroup trait**: Compositions of related tools
- **Macro-based composition**: Reduces boilerplate for tool groups

### Error Handling
- Graceful error handling with detailed error messages
- SDK errors are properly propagated and formatted
- Validation errors include helpful context

### Configuration Resolution
- Supports Superposition's context-aware configuration resolution
- Handles multi-dimensional targeting (country, user segment, etc.)
- Proper fallback to default values

## Development

### Building
```bash
# Library only
cargo build --package superposition_mcp

# With standalone binary
cargo build --package superposition_mcp --features standalone
```

### Testing
```bash
cargo test --package superposition_mcp
```

### Linting
```bash
cargo clippy --package superposition_mcp
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## License

This project is licensed under the same terms as the main Superposition project.

## Support

For issues and questions:
- Create an issue in the main Superposition repository
- Check the Superposition documentation
- Review MCP protocol specification at https://modelcontextprotocol.io/

---

**Note**: This MCP server provides programmatic access to Superposition's configuration management capabilities. Ensure proper authentication and authorization in production environments.