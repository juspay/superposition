u Superposition MCP Server

[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Code style: black](https://img.shields.io/badge/code%20style-black-000000.svg)](https://github.com/psf/black)

A Model Context Protocol (MCP) server that exposes Superposition's configuration management and experimentation capabilities as tools for AI assistants.

## Quick Start

### Prerequisites

- Python 3.8 or higher
- Access to a Superposition instance
- Valid API credentials

### Installation

```bash
# Quick setup (recommended)
./scripts/setup.sh

# Or manual installation
python -m pip install superposition-mcp
```

### Configuration

Create a `.env` file:

```env
SUPERPOSITION_API_URL=https://your-superposition-instance.com
SUPERPOSITION_API_TOKEN=your-bearer-token
SUPERPOSITION_DEFAULT_WORKSPACE=dev
```

### Usage

```bash
# Start the MCP server
python -m superposition_mcp.main

# Or with Docker
docker-compose up -d
```

## Features

### 🔧 Configuration Management
- **40+ Tools**: Complete API coverage for configuration operations
- **Context-Aware**: Support for contextual configuration overrides
- **Type Safety**: Full input validation with Pydantic schemas
- **Caching**: Intelligent caching for performance optimization

### 🧪 Experimentation Platform
- **A/B Testing**: Create and manage experiments with multiple variants
- **Experiment Groups**: Organize related experiments
- **Lifecycle Management**: Full experiment lifecycle from creation to conclusion
- **Traffic Control**: Dynamic traffic ramping and control

### 🚀 AI Assistant Integration
- **Claude Desktop**: Ready-to-use configuration
- **Continue.dev**: Seamless integration
- **Custom Assistants**: Generic MCP protocol support
- **Rich Toolset**: 40+ tools for comprehensive platform interaction

## Available Tools

<details>
<summary><strong>Configuration Tools (13 tools)</strong></summary>

- `get_config` - Retrieve configuration with context
- `get_config_fast` - Fast configuration retrieval
- `get_resolved_config` - Get resolved configuration with merging
- `list_versions` - List configuration versions
- `create_default_config` - Create default configuration keys
- `list_default_configs` - List all default configurations
- `update_default_config` - Update default configuration values
- `delete_default_config` - Remove default configuration keys
- `create_context` - Create new contexts with conditions
- `get_context` - Retrieve context by ID
- `list_contexts` - List all contexts with filtering
- `update_override` - Update context overrides
- `delete_context` - Remove contexts

</details>

<details>
<summary><strong>Experimentation Tools (15 tools)</strong></summary>

- `create_experiment` - Set up new experiments
- `get_experiment` - Retrieve experiment details
- `list_experiment` - List experiments with filtering
- `update_overrides_experiment` - Update experiment variants
- `applicable_variants` - Get applicable experiment variants
- `conclude_experiment` - Conclude running experiments
- `discard_experiment` - Discard experiments
- `pause_experiment` - Pause running experiments
- `resume_experiment` - Resume paused experiments
- `ramp_experiment` - Adjust experiment traffic
- `create_experiment_group` - Create experiment groups
- `get_experiment_group` - Retrieve group details
- `list_experiment_groups` - List experiment groups
- `update_experiment_group` - Update group properties
- `delete_experiment_group` - Remove experiment groups

</details>

<details>
<summary><strong>Management Tools (12+ tools)</strong></summary>

- `create_dimension` - Create configuration dimensions
- `create_function` - Create validation functions
- `create_workspace` - Create new workspaces
- `create_organisation` - Create organizations
- `create_webhook` - Set up webhooks
- `list_audit_logs` - Retrieve audit logs
- `test` - Test server connectivity
- And more...

</details>

## Documentation

### Quick Links
- 📖 **[Complete Documentation](.ai/docs/mcp-server/README.md)** - Comprehensive guide
- ⚙️ **[Configuration Guide](.ai/docs/mcp-server/CONFIGURATION.md)** - All configuration options
- 🚀 **[Deployment Guide](.ai/docs/mcp-server/DEPLOYMENT.md)** - Production deployment
- 🐛 **[Troubleshooting](.ai/docs/mcp-server/TROUBLESHOOTING.md)** - Common issues and solutions

### Examples
- 💡 **[Usage Examples](.ai/docs/mcp-server/examples/usage-examples.py)** - Practical examples
- 🔌 **[Claude Integration](.ai/docs/mcp-server/examples/claude-integration.json)** - Claude Desktop setup
- 🐳 **[Docker Setup](docker-compose.yml)** - Container deployment

## Development

### Setup Development Environment

```bash
# Complete development setup
./scripts/setup.sh --type development

# Manual setup
git clone <repository>
cd superposition/mcp-server
python -m venv venv
source venv/bin/activate
pip install -e ".[dev]"
```

### Running Tests

```bash
# Run all tests
pytest

# Run with coverage
pytest --cov=superposition_mcp

# Run specific test categories
pytest -m unit           # Unit tests only
pytest -m integration    # Integration tests only
```

### Code Quality

```bash
# Format code
black src/ tests/
isort src/ tests/

# Lint code
flake8 src/ tests/
mypy src/

# Run all quality checks
pre-commit run --all-files
```

## Deployment Options

### 🐳 Docker (Recommended)

```bash
# Production deployment
docker-compose up -d

# Development with auto-reload
docker-compose --profile dev up -d

# With monitoring stack
docker-compose --profile monitoring up -d
```

### 🖥️ System Service

```bash
# Install as system service
sudo ./scripts/install.sh

# Manage service
sudo systemctl start superposition-mcp
sudo systemctl status superposition-mcp
sudo journalctl -u superposition-mcp -f
```

### ☁️ Cloud Deployment

- **AWS ECS**: See [deployment guide](.ai/docs/mcp-server/DEPLOYMENT.md#aws-ecs)
- **Kubernetes**: Complete K8s manifests included
- **Azure Container Instances**: Ready-to-use configurations
- **Google Cloud Run**: Optimized for serverless deployment

## Configuration

### Environment Variables

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `SUPERPOSITION_API_URL` | ✅ | - | Superposition API endpoint |
| `SUPERPOSITION_API_TOKEN` | ✅ | - | Bearer token for authentication |
| `SUPERPOSITION_DEFAULT_WORKSPACE` | ❌ | `dev` | Default workspace |
| `SUPERPOSITION_DEFAULT_ORG` | ❌ | `juspay` | Default organization |
| `SUPERPOSITION_TIMEOUT` | ❌ | `30` | Request timeout (seconds) |
| `SUPERPOSITION_DEBUG` | ❌ | `false` | Enable debug logging |

### Performance Tuning

```env
# High-performance configuration
SUPERPOSITION_CACHE_TTL=600
SUPERPOSITION_MAX_CONNECTIONS=50
SUPERPOSITION_MAX_CONCURRENT_REQUESTS=20
```

### Security

```env
# Security hardening
SUPERPOSITION_SSL_VERIFY=true
SUPERPOSITION_REQUEST_TIMEOUT=30
SUPERPOSITION_RATE_LIMIT=100
```

## Integration Examples

### Claude Desktop

Add to your `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "superposition": {
      "command": "python",
      "args": ["/path/to/superposition-mcp"],
      "env": {
        "SUPERPOSITION_API_URL": "https://your-api.com",
        "SUPERPOSITION_API_TOKEN": "your-token"
      }
    }
  }
}
```

### Continue.dev

```json
{
  "mcp": {
    "superposition": {
      "serverName": "superposition",
      "params": {
        "command": "superposition-mcp"
      }
    }
  }
}
```

## Monitoring and Observability

### Built-in Health Checks

```bash
# Basic health check
curl http://localhost:8000/health

# Detailed health with dependencies
curl http://localhost:8000/health/detailed
```

### Prometheus Metrics

```yaml
# Automatically exposed metrics
- mcp_requests_total
- mcp_request_duration_seconds  
- mcp_cache_hit_rate
- mcp_active_connections
```

### Logging

```bash
# Structured JSON logging
export SUPERPOSITION_LOG_FORMAT=json
export SUPERPOSITION_LOG_LEVEL=INFO

# Debug mode for troubleshooting
export SUPERPOSITION_DEBUG=true
```

## Performance

### Benchmarks

- **Average response time**: < 100ms for cached operations
- **Cache hit rate**: > 95% for repeated operations
- **Throughput**: 1000+ requests/minute per instance
- **Memory usage**: < 100MB typical, < 500MB peak

### Optimization

- ✅ **HTTP/2 connection pooling**
- ✅ **Intelligent caching with TTL**
- ✅ **Request compression**
- ✅ **Async I/O throughout**
- ✅ **Connection keep-alive**

## Security

### Authentication

- **Bearer token authentication**
- **Token validation and expiration checking**
- **Scope-based permissions**
- **Secure token storage**

### Network Security

- **TLS/SSL verification**
- **Request rate limiting**
- **Input validation and sanitization**
- **No sensitive data in logs**

## Troubleshooting

### Common Issues

❌ **Connection Timeout**
```bash
# Increase timeout
export SUPERPOSITION_TIMEOUT=90
```

❌ **Authentication Failed**
```bash
# Check token validity
python -c "from superposition_mcp.auth import validate_token; validate_token()"
```

❌ **Workspace Not Found**
```bash
# List available workspaces
curl -H "Authorization: Bearer $TOKEN" "$API_URL/workspaces"
```

See the [complete troubleshooting guide](.ai/docs/mcp-server/TROUBLESHOOTING.md) for more solutions.

## Contributing

We welcome contributions! Please see our contributing guidelines:

1. **Fork** the repository
2. **Create** a feature branch (`git checkout -b feature/amazing-feature`)
3. **Commit** your changes (`git commit -m 'Add amazing feature'`)
4. **Push** to the branch (`git push origin feature/amazing-feature`)
5. **Open** a Pull Request

### Development Guidelines

- Follow [PEP 8](https://www.python.org/dev/peps/pep-0008/) style guide
- Add tests for new features
- Update documentation
- Ensure all tests pass

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Support

- 📧 **Email**: support@superposition.io
- 💬 **Discussions**: [GitHub Discussions](https://github.com/juspay/superposition/discussions)
- 🐛 **Bug Reports**: [GitHub Issues](https://github.com/juspay/superposition/issues)
- 📚 **Documentation**: [docs.superposition.io](https://docs.superposition.io)

## Changelog

See [CHANGELOG.md](CHANGELOG.md) for a list of changes and version history.

---

**Built with ❤️ by the Superposition team**
