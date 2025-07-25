# Configuration Guide

This guide covers all configuration options for the Superposition MCP Server.

## Environment Variables

### Required Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `SUPERPOSITION_API_URL` | Base URL of your Superposition instance | `https://api.superposition.example.com` |
| `SUPERPOSITION_API_TOKEN` | Bearer token for API authentication | `eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...` |

### Optional Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `SUPERPOSITION_DEFAULT_WORKSPACE` | `dev` | Default workspace (tenant) to use |
| `SUPERPOSITION_DEFAULT_ORG` | `juspay` | Default organization to use |
| `SUPERPOSITION_TIMEOUT` | `30` | Request timeout in seconds |
| `SUPERPOSITION_DEBUG` | `false` | Enable debug logging |
| `SUPERPOSITION_RETRY_ATTEMPTS` | `3` | Number of retry attempts for failed requests |
| `SUPERPOSITION_RETRY_DELAY` | `1.0` | Base delay between retries (seconds) |
| `SUPERPOSITION_CACHE_TTL` | `300` | Cache TTL in seconds |
| `SUPERPOSITION_MAX_CONNECTIONS` | `10` | Maximum HTTP connections in pool |

## Configuration Files

### .env File

Create a `.env` file in your working directory:

```env
# Required Configuration
SUPERPOSITION_API_URL=https://your-superposition-instance.com
SUPERPOSITION_API_TOKEN=your-bearer-token-here

# Optional Configuration
SUPERPOSITION_DEFAULT_WORKSPACE=production
SUPERPOSITION_DEFAULT_ORG=your-org
SUPERPOSITION_TIMEOUT=60
SUPERPOSITION_DEBUG=true
SUPERPOSITION_RETRY_ATTEMPTS=5
SUPERPOSITION_RETRY_DELAY=2.0
SUPERPOSITION_CACHE_TTL=600
SUPERPOSITION_MAX_CONNECTIONS=20
```

### Configuration Priority

Configuration is loaded in the following order (highest to lowest priority):

1. Environment variables
2. `.env` file in current directory
3. `.env` file in user home directory
4. Default values

## Authentication Configuration

### Bearer Token Authentication

The MCP server uses Bearer token authentication. You can obtain a token from your Superposition instance.

#### Token Requirements

- Must be a valid JWT token
- Must have appropriate permissions for the operations you want to perform
- Should not be expired

#### Token Scopes

Ensure your token has the following scopes based on your usage:

| Scope | Required For |
|-------|--------------|
| `config:read` | Reading configurations and contexts |
| `config:write` | Creating/updating configurations |
| `config:delete` | Deleting configurations and contexts |
| `experiment:read` | Reading experiments and groups |
| `experiment:write` | Creating/updating experiments |
| `experiment:manage` | Managing experiment lifecycle |
| `audit:read` | Accessing audit logs |
| `workspace:read` | Reading workspace information |
| `workspace:write` | Creating/updating workspaces |

### Token Rotation

For production deployments, implement token rotation:

```bash
#!/bin/bash
# Token rotation script
NEW_TOKEN=$(curl -s -X POST \
  -H "Content-Type: application/json" \
  -d '{"refresh_token": "'$REFRESH_TOKEN'"}' \
  "$SUPERPOSITION_API_URL/auth/refresh" | jq -r '.access_token')

export SUPERPOSITION_API_TOKEN="$NEW_TOKEN"
```

## Network Configuration

### Timeouts

Configure appropriate timeouts for your environment:

```env
# For local development
SUPERPOSITION_TIMEOUT=10

# For production with network latency
SUPERPOSITION_TIMEOUT=60

# For high-latency networks
SUPERPOSITION_TIMEOUT=120
```

### Retry Configuration

Configure retry behavior for resilient operation:

```env
# Conservative retry settings
SUPERPOSITION_RETRY_ATTEMPTS=3
SUPERPOSITION_RETRY_DELAY=1.0

# Aggressive retry settings for unstable networks
SUPERPOSITION_RETRY_ATTEMPTS=10
SUPERPOSITION_RETRY_DELAY=0.5
```

### Connection Pooling

Optimize connection pooling for your load:

```env
# For low-traffic scenarios
SUPERPOSITION_MAX_CONNECTIONS=5

# For high-traffic scenarios
SUPERPOSITION_MAX_CONNECTIONS=50

# For very high-traffic scenarios
SUPERPOSITION_MAX_CONNECTIONS=100
```

## Caching Configuration

### Cache Settings

Configure caching to balance performance and data freshness:

```env
# Fast refresh for development
SUPERPOSITION_CACHE_TTL=60

# Standard refresh for production
SUPERPOSITION_CACHE_TTL=300

# Long cache for read-heavy workloads
SUPERPOSITION_CACHE_TTL=3600
```

### Cache Types

The server uses different cache TTLs for different data types:

| Data Type | Default TTL | Environment Variable |
|-----------|-------------|---------------------|
| Configurations | 30s | `SUPERPOSITION_CONFIG_CACHE_TTL` |
| Experiments | 60s | `SUPERPOSITION_EXPERIMENT_CACHE_TTL` |
| Dimensions | 300s | `SUPERPOSITION_DIMENSION_CACHE_TTL` |
| Functions | 300s | `SUPERPOSITION_FUNCTION_CACHE_TTL` |
| Workspaces | 600s | `SUPERPOSITION_WORKSPACE_CACHE_TTL` |

Example configuration:

```env
SUPERPOSITION_CONFIG_CACHE_TTL=10      # Fast refresh for configs
SUPERPOSITION_EXPERIMENT_CACHE_TTL=30  # Medium refresh for experiments
SUPERPOSITION_DIMENSION_CACHE_TTL=600  # Slow refresh for dimensions
```

## Logging Configuration

### Debug Logging

Enable debug logging for troubleshooting:

```env
SUPERPOSITION_DEBUG=true
```

This enables:
- Request/response logging
- Detailed error messages
- Performance timing information
- Cache hit/miss statistics

### Log Levels

Configure logging levels:

```env
SUPERPOSITION_LOG_LEVEL=INFO  # OPTIONS: DEBUG, INFO, WARNING, ERROR, CRITICAL
```

### Log Format

Choose log format:

```env
SUPERPOSITION_LOG_FORMAT=json    # OPTIONS: json, text
```

### Log Output

Configure log output destination:

```env
SUPERPOSITION_LOG_FILE=/var/log/superposition-mcp.log  # File output
# Leave empty for stdout
```

## Workspace Configuration

### Default Workspace

Set a default workspace to avoid specifying it in every request:

```env
SUPERPOSITION_DEFAULT_WORKSPACE=production
SUPERPOSITION_DEFAULT_ORG=mycompany
```

### Multi-Tenant Configuration

For multi-tenant deployments, you can configure tenant-specific settings:

```env
# Tenant mapping
SUPERPOSITION_TENANT_MAPPING='{
  "dev": {"workspace": "development", "org": "mycompany"},
  "staging": {"workspace": "staging", "org": "mycompany"},
  "prod": {"workspace": "production", "org": "mycompany"}
}'
```

## Security Configuration

### SSL/TLS Configuration

For secure communications:

```env
SUPERPOSITION_SSL_VERIFY=true           # Verify SSL certificates
SUPERPOSITION_SSL_CERT_PATH=/path/to/cert.pem  # Custom CA certificate
```

### Request Headers

Add custom headers to all requests:

```env
SUPERPOSITION_CUSTOM_HEADERS='{
  "X-Request-ID": "mcp-server",
  "X-Source": "ai-assistant"
}'
```

## Performance Configuration

### Concurrent Requests

Configure concurrent request limits:

```env
SUPERPOSITION_MAX_CONCURRENT_REQUESTS=10
```

### Request Queuing

Configure request queuing for high load:

```env
SUPERPOSITION_REQUEST_QUEUE_SIZE=100
SUPERPOSITION_REQUEST_QUEUE_TIMEOUT=30
```

### Memory Limits

Configure memory usage limits:

```env
SUPERPOSITION_MAX_CACHE_SIZE=100MB     # Maximum cache size
SUPERPOSITION_MAX_REQUEST_SIZE=10MB    # Maximum request body size
```

## Development Configuration

### Development-Specific Settings

For development environments:

```env
# Development configuration
SUPERPOSITION_DEBUG=true
SUPERPOSITION_TIMEOUT=5
SUPERPOSITION_CACHE_TTL=10
SUPERPOSITION_RETRY_ATTEMPTS=1
SUPERPOSITION_LOG_LEVEL=DEBUG
```

### Testing Configuration

For testing environments:

```env
# Testing configuration
SUPERPOSITION_API_URL=http://localhost:8080
SUPERPOSITION_TIMEOUT=1
SUPERPOSITION_CACHE_TTL=0              # Disable caching
SUPERPOSITION_RETRY_ATTEMPTS=0         # Disable retries
SUPERPOSITION_DEBUG=true
```

## Production Configuration

### Production-Optimized Settings

For production environments:

```env
# Production configuration
SUPERPOSITION_DEBUG=false
SUPERPOSITION_TIMEOUT=60
SUPERPOSITION_CACHE_TTL=300
SUPERPOSITION_RETRY_ATTEMPTS=5
SUPERPOSITION_RETRY_DELAY=2.0
SUPERPOSITION_MAX_CONNECTIONS=20
SUPERPOSITION_LOG_LEVEL=WARNING
SUPERPOSITION_LOG_FORMAT=json
SUPERPOSITION_SSL_VERIFY=true
```

### High-Availability Configuration

For high-availability deployments:

```env
# HA configuration
SUPERPOSITION_RETRY_ATTEMPTS=10
SUPERPOSITION_RETRY_DELAY=1.0
SUPERPOSITION_MAX_CONNECTIONS=50
SUPERPOSITION_REQUEST_QUEUE_SIZE=500
SUPERPOSITION_CACHE_TTL=600
```

## Docker Configuration

### Docker Environment

When running in Docker:

```dockerfile
# Dockerfile
FROM python:3.11-slim

ENV SUPERPOSITION_API_URL=""
ENV SUPERPOSITION_API_TOKEN=""
ENV SUPERPOSITION_DEFAULT_WORKSPACE="dev"
ENV SUPERPOSITION_DEFAULT_ORG="juspay"
ENV SUPERPOSITION_DEBUG="false"

COPY . /app
WORKDIR /app
RUN pip install -e .

CMD ["python", "src/superposition_mcp/main.py"]
```

### Docker Compose

```yaml
version: '3.8'
services:
  superposition-mcp:
    build: .
    environment:
      SUPERPOSITION_API_URL: ${SUPERPOSITION_API_URL}
      SUPERPOSITION_API_TOKEN: ${SUPERPOSITION_API_TOKEN}
      SUPERPOSITION_DEFAULT_WORKSPACE: production
      SUPERPOSITION_DEFAULT_ORG: mycompany
      SUPERPOSITION_DEBUG: "false"
      SUPERPOSITION_LOG_LEVEL: INFO
    volumes:
      - ./logs:/var/log
    restart: unless-stopped
```

## Configuration Validation

### Startup Validation

The server validates configuration on startup:

- Checks required environment variables
- Validates API connectivity
- Tests authentication
- Verifies workspace access

### Runtime Validation

Configuration is validated during runtime:

- Token expiration checks
- Network connectivity monitoring
- Cache health monitoring
- Performance metrics tracking

### Configuration Health Check

Test your configuration:

```bash
# Test basic connectivity
python -c "
import os
from superposition_mcp.config import Config
config = Config()
print('Configuration valid:', config.validate())
"

# Test API connectivity
python -c "
import asyncio
from superposition_mcp.client import SuperpositionClient
client = SuperpositionClient()
asyncio.run(client.test_connection())
"
```

## Troubleshooting Configuration

### Common Configuration Issues

1. **Invalid API URL**
   ```
   Error: Cannot connect to Superposition API
   Solution: Verify SUPERPOSITION_API_URL is correct and accessible
   ```

2. **Authentication Failure**
   ```
   Error: 401 Unauthorized
   Solution: Check SUPERPOSITION_API_TOKEN is valid and not expired
   ```

3. **Workspace Not Found**
   ```
   Error: Workspace 'xyz' not found
   Solution: Verify SUPERPOSITION_DEFAULT_WORKSPACE exists
   ```

4. **Timeout Issues**
   ```
   Error: Request timeout
   Solution: Increase SUPERPOSITION_TIMEOUT value
   ```

### Configuration Debugging

Enable configuration debugging:

```env
SUPERPOSITION_DEBUG_CONFIG=true
```

This will log:
- Configuration loading process
- Environment variable resolution
- Default value application
- Validation results

### Configuration Export

Export current configuration for review:

```bash
python -c "
from superposition_mcp.config import Config
config = Config()
config.export_config()
"
```

This will output all configuration values (with sensitive data masked).