# Superposition MCP Server

A Model Context Protocol (MCP) server that exposes Superposition's configuration management and experimentation capabilities as tools for AI assistants. This server enables AI assistants to interact with the Superposition platform programmatically, allowing them to manage configurations, create experiments, and perform other operations.

## Table of Contents

- [Overview](#overview)
- [Installation](#installation)
- [Configuration](#configuration)
- [Available Tools](#available-tools)
- [Integration Examples](#integration-examples)
- [Deployment](#deployment)
- [Troubleshooting](#troubleshooting)
- [API Reference](#api-reference)

## Overview

### What is the Superposition MCP Server?

The Superposition MCP Server bridges AI assistants with the Superposition platform, providing:

- **Context-Aware Configuration (CAC) Management**: Create, read, update, and delete configuration entries with contextual overrides
- **Experimentation Platform**: Set up and manage A/B tests, experiment groups, and variants
- **Tenant & Workspace Management**: Handle multi-tenant configurations and workspace operations
- **Audit & Monitoring**: Access audit logs and configuration versioning

### Key Features

- **40+ Available Tools**: Comprehensive coverage of Superposition's API operations
- **Type-Safe Operations**: Full input validation using Pydantic schemas
- **Error Handling**: Robust error handling with detailed error messages
- **Authentication Support**: Bearer token authentication for secure API access
- **Contextual Operations**: Support for context-aware configuration management
- **Experiment Lifecycle**: Complete experiment management from creation to conclusion

## Installation

### Prerequisites

- Python 3.8+
- Access to a running Superposition instance
- Valid API credentials (Bearer token)

### Install from Source

```bash
# Clone the repository
git clone <repository-url>
cd superposition

# Navigate to MCP server directory
cd mcp-server

# Create virtual environment
python -m venv myenv
source myenv/bin/activate  # On Windows: myenv\Scripts\activate

# Install dependencies
pip install -e .
```

### Install via pip (when available)

```bash
pip install superposition-mcp
```

## Configuration

### Environment Variables

The MCP server uses environment variables for configuration:

```bash
# Required: Superposition API endpoint
SUPERPOSITION_API_URL=https://your-superposition-instance.com

# Required: Authentication token
SUPERPOSITION_API_TOKEN=your-bearer-token

# Optional: Default workspace (tenant)
SUPERPOSITION_DEFAULT_WORKSPACE=dev

# Optional: Default organization
SUPERPOSITION_DEFAULT_ORG=juspay

# Optional: Request timeout in seconds
SUPERPOSITION_TIMEOUT=30

# Optional: Enable debug logging
SUPERPOSITION_DEBUG=false
```

### Configuration File

Create a `.env` file in your working directory:

```env
SUPERPOSITION_API_URL=https://api.superposition.example.com
SUPERPOSITION_API_TOKEN=your-api-token-here
SUPERPOSITION_DEFAULT_WORKSPACE=dev
SUPERPOSITION_DEFAULT_ORG=juspay
```

## Available Tools

The MCP server provides tools organized into the following categories:

### Context-Aware Configuration (CAC) Tools

#### Configuration Management
- **`get_config`** - Retrieve configuration with context
- **`get_config_fast`** - Fast configuration retrieval
- **`get_resolved_config`** - Get resolved configuration with context merging
- **`list_versions`** - List configuration versions

#### Default Configuration Management
- **`create_default_config`** - Create default configuration keys
- **`list_default_configs`** - List all default configurations
- **`update_default_config`** - Update default configuration values
- **`delete_default_config`** - Remove default configuration keys

#### Context Management
- **`create_context`** - Create new contexts with conditions
- **`get_context`** - Retrieve context by ID
- **`get_context_from_condition`** - Find context by condition
- **`list_contexts`** - List all contexts with filtering
- **`move_context`** - Move context to different condition
- **`update_override`** - Update context overrides
- **`delete_context`** - Remove contexts
- **`weight_recompute`** - Recompute context weights
- **`bulk_operation`** - Perform bulk context operations

#### Dimension Management
- **`create_dimension`** - Create configuration dimensions
- **`get_dimension`** - Retrieve dimension details
- **`list_dimensions`** - List all dimensions
- **`update_dimension`** - Modify dimension properties
- **`delete_dimension`** - Remove dimensions

### Experimentation Platform Tools

#### Experiment Management
- **`create_experiment`** - Set up new experiments
- **`get_experiment`** - Retrieve experiment details
- **`list_experiment`** - List experiments with filtering
- **`update_overrides_experiment`** - Update experiment variants
- **`applicable_variants`** - Get applicable experiment variants
- **`conclude_experiment`** - Conclude running experiments
- **`discard_experiment`** - Discard experiments
- **`pause_experiment`** - Pause running experiments
- **`resume_experiment`** - Resume paused experiments
- **`ramp_experiment`** - Adjust experiment traffic

#### Experiment Group Management
- **`create_experiment_group`** - Create experiment groups
- **`get_experiment_group`** - Retrieve group details
- **`list_experiment_groups`** - List experiment groups
- **`update_experiment_group`** - Update group properties
- **`delete_experiment_group`** - Remove experiment groups
- **`add_members_to_group`** - Add experiments to groups
- **`remove_members_from_group`** - Remove experiments from groups

### Workspace & Organization Tools

#### Workspace Management
- **`create_workspace`** - Create new workspaces
- **`list_workspace`** - List available workspaces
- **`update_workspace`** - Update workspace properties

#### Organization Management
- **`create_organisation`** - Create organizations
- **`get_organisation`** - Retrieve organization details
- **`list_organisation`** - List organizations
- **`update_organisation`** - Update organization properties

### Function & Template Tools

#### Function Management
- **`create_function`** - Create validation functions
- **`get_function`** - Retrieve function details
- **`list_function`** - List available functions
- **`update_function`** - Update function definitions
- **`delete_function`** - Remove functions

#### Type Template Management
- **`create_type_templates`** - Create type templates
- **`get_type_templates_list`** - List type templates
- **`update_type_templates`** - Update templates
- **`delete_type_templates`** - Remove templates

### Webhook & Audit Tools

#### Webhook Management
- **`create_webhook`** - Set up webhooks
- **`get_webhook`** - Retrieve webhook details
- **`list_webhook`** - List configured webhooks
- **`update_webhook`** - Update webhook configuration

#### Audit & Monitoring
- **`list_audit_logs`** - Retrieve audit logs
- **`test`** - Test server connectivity
- **`publish`** - Publish configuration changes

## Integration Examples

### Claude Desktop Integration

Add to your Claude Desktop configuration:

```json
{
  "mcpServers": {
    "superposition": {
      "command": "python",
      "args": ["/path/to/superposition/mcp-server/src/superposition_mcp/main.py"],
      "env": {
        "SUPERPOSITION_API_URL": "https://your-superposition-instance.com",
        "SUPERPOSITION_API_TOKEN": "your-bearer-token"
      }
    }
  }
}
```

### Continue.dev Integration

```json
{
  "mcp": {
    "superposition": {
      "serverName": "superposition",
      "params": {
        "command": "python",
        "args": ["/path/to/superposition/mcp-server/src/superposition_mcp/main.py"]
      }
    }
  }
}
```

### Basic Usage Examples

#### Creating a Configuration

```python
# AI Assistant can use this tool to create a new default configuration
await create_default_config({
    "workspace_id": "dev",
    "org_id": "juspay",
    "key": "feature_flag_new_ui",
    "value": {"enabled": false},
    "schema": {
        "type": "object",
        "properties": {
            "enabled": {"type": "boolean"}
        }
    },
    "description": "Feature flag for new UI",
    "change_reason": "Initial setup"
})
```

#### Creating an Experiment

```python
# AI Assistant can create A/B tests
await create_experiment({
    "workspace_id": "dev",
    "org_id": "juspay",
    "name": "New Checkout Flow",
    "context": {
        "and": [
            {"var": "user_type", "in": ["premium", "standard"]}
        ]
    },
    "variants": [
        {
            "id": "control",
            "variant_type": "CONTROL",
            "overrides": {"checkout_flow": "classic"}
        },
        {
            "id": "experimental",
            "variant_type": "EXPERIMENTAL",
            "overrides": {"checkout_flow": "streamlined"}
        }
    ],
    "description": "Testing new streamlined checkout flow",
    "change_reason": "Improve conversion rates"
})
```

#### Getting Configuration with Context

```python
# AI Assistant can retrieve contextual configuration
await get_config({
    "workspace_id": "dev",
    "org_id": "juspay",
    "context": {
        "user_id": "user123",
        "user_type": "premium",
        "region": "us-west"
    }
})
```

## Deployment

### Production Deployment

#### Using Docker

Create a `Dockerfile`:

```dockerfile
FROM python:3.11-slim

WORKDIR /app

COPY requirements.txt .
RUN pip install -r requirements.txt

COPY src/ ./src/
COPY pyproject.toml .

RUN pip install -e .

EXPOSE 8000

CMD ["python", "src/superposition_mcp/main.py"]
```

Build and run:

```bash
docker build -t superposition-mcp .
docker run -d \
  -e SUPERPOSITION_API_URL=https://your-api.com \
  -e SUPERPOSITION_API_TOKEN=your-token \
  -p 8000:8000 \
  superposition-mcp
```

#### Using systemd

Create `/etc/systemd/system/superposition-mcp.service`:

```ini
[Unit]
Description=Superposition MCP Server
After=network.target

[Service]
Type=simple
User=www-data
WorkingDirectory=/opt/superposition-mcp
ExecStart=/opt/superposition-mcp/venv/bin/python src/superposition_mcp/main.py
EnvironmentFile=/opt/superposition-mcp/.env
Restart=always
RestartSec=5

[Install]
WantedBy=multi-user.target
```

Enable and start:

```bash
sudo systemctl enable superposition-mcp
sudo systemctl start superposition-mcp
```

### Health Monitoring

The server provides health check endpoints:

```bash
# Basic connectivity test
curl http://localhost:8000/health

# Test with actual API
curl -H "Authorization: Bearer $TOKEN" \
     http://localhost:8000/health/api
```

## Troubleshooting

### Common Issues

#### Authentication Errors

**Error**: `401 Unauthorized`

**Solution**:
- Verify `SUPERPOSITION_API_TOKEN` is correctly set
- Ensure the token has necessary permissions
- Check if the token has expired

#### Connection Timeouts

**Error**: `Connection timeout`

**Solution**:
- Verify `SUPERPOSITION_API_URL` is correct and accessible
- Check network connectivity
- Increase `SUPERPOSITION_TIMEOUT` value
- Verify firewall settings

#### Invalid Workspace/Organization

**Error**: `Workspace not found`

**Solution**:
- Verify workspace exists in your Superposition instance
- Check `SUPERPOSITION_DEFAULT_WORKSPACE` setting
- Ensure proper tenant configuration

#### Tool Validation Errors

**Error**: `Invalid input parameters`

**Solution**:
- Check the API reference for required fields
- Verify data types match expected schemas
- Ensure all required fields are provided

### Debug Mode

Enable debug logging:

```bash
export SUPERPOSITION_DEBUG=true
python src/superposition_mcp/main.py
```

This will provide detailed request/response logging.

### Log Analysis

Common log patterns to look for:

```bash
# Successful operations
INFO: Tool executed successfully: get_config

# Authentication issues
ERROR: Authentication failed: Invalid token

# Network issues
ERROR: Connection failed: timeout

# Validation errors
ERROR: Input validation failed: missing required field 'workspace_id'
```

## API Reference

### Tool Input Schemas

All tools use Pydantic models for input validation. Common patterns:

#### Workspace Identification

Most tools require workspace identification:

```json
{
  "workspace_id": "string (required)",
  "org_id": "string (default: 'juspay')"
}
```

#### Pagination

List operations support pagination:

```json
{
  "page": "integer (optional)",
  "count": "integer (optional)", 
  "all": "boolean (optional)"
}
```

#### Context Conditions

Context conditions use JSONLogic format:

```json
{
  "context": {
    "and": [
      {"var": "user_type", "in": ["premium", "standard"]},
      {"var": "region", "==": "us-west"}
    ]
  }
}
```

### Tool Output Formats

All tools return structured JSON responses:

#### Success Response
```json
{
  "success": true,
  "data": { /* tool-specific data */ },
  "metadata": {
    "timestamp": "2025-01-01T00:00:00Z",
    "version": "1.0.0"
  }
}
```

#### Error Response
```json
{
  "success": false,
  "error": {
    "code": "ERROR_CODE",
    "message": "Human readable error message",
    "details": { /* additional error context */ }
  }
}
```

### Rate Limiting

The server implements rate limiting to protect the Superposition API:

- **Default**: 100 requests per minute per client
- **Burst**: Up to 10 requests per second
- **Headers**: Rate limit status included in response headers

### Security Considerations

#### API Token Security

- Store tokens in environment variables, never in code
- Use least-privilege tokens when possible
- Rotate tokens regularly
- Monitor token usage in audit logs

#### Network Security

- Use HTTPS for all communications
- Implement proper firewall rules
- Consider VPN access for production deployments
- Monitor for unusual access patterns

#### Data Validation

- All inputs are validated against strict schemas
- SQL injection protection through parameterized queries
- XSS protection for configuration values
- Rate limiting to prevent abuse

### Performance Optimization

#### Caching

The server implements intelligent caching:

- Configuration responses cached for 30 seconds
- Dimension and function lists cached for 5 minutes
- Experiment data cached for 1 minute
- Cache invalidation on updates

#### Connection Pooling

- HTTP connection pooling for API requests
- Configurable pool size and timeout settings
- Automatic connection retry with exponential backoff

#### Async Operations

- All API calls are asynchronous
- Concurrent request handling
- Non-blocking I/O operations

## Support and Contributing

### Getting Help

- **Issues**: Report bugs and feature requests on GitHub
- **Documentation**: Comprehensive guides in the `/docs` directory
- **Community**: Join our Discord/Slack community
- **Enterprise**: Contact support for enterprise deployments

### Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

### Development Setup

```bash
# Clone and setup development environment
git clone <repository>
cd superposition/mcp-server

# Install development dependencies
pip install -e ".[dev]"

# Run tests
pytest

# Run linting
black src/
flake8 src/

# Run type checking
mypy src/
```

---

For more detailed information about specific tools and their parameters, see the individual tool documentation in the `/docs/tools/` directory.