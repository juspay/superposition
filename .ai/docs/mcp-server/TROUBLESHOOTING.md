# Troubleshooting Guide

This guide helps you diagnose and resolve common issues with the Superposition MCP Server.

## Table of Contents

- [Quick Diagnostics](#quick-diagnostics)
- [Common Issues](#common-issues)
- [Error Messages](#error-messages)
- [Performance Issues](#performance-issues)
- [Network and Connectivity](#network-and-connectivity)
- [Configuration Problems](#configuration-problems)
- [Debug Mode](#debug-mode)
- [Log Analysis](#log-analysis)
- [Health Checks](#health-checks)

## Quick Diagnostics

### 1. Basic Health Check

```bash
# Test basic connectivity
python -c "
import asyncio
from superposition_mcp.health import health_check
asyncio.run(health_check())
"
```

### 2. Configuration Validation

```bash
# Validate configuration
python -c "
from superposition_mcp.config import Config
config = Config()
print('Configuration valid:', config.validate())
print('API URL:', config.api_url)
print('Workspace:', config.default_workspace)
"
```

### 3. API Connectivity Test

```bash
# Test API connectivity
curl -H "Authorization: Bearer $SUPERPOSITION_API_TOKEN" \
     "$SUPERPOSITION_API_URL/health"
```

### 4. Check Environment Variables

```bash
# Check required environment variables
echo "API URL: $SUPERPOSITION_API_URL"
echo "Token: ${SUPERPOSITION_API_TOKEN:0:10}..." # Show first 10 chars
echo "Workspace: $SUPERPOSITION_DEFAULT_WORKSPACE"
echo "Debug: $SUPERPOSITION_DEBUG"
```

## Common Issues

### Issue 1: Cannot Connect to Superposition API

**Symptoms:**
- Connection timeout errors
- "Connection refused" messages
- Network unreachable errors

**Diagnosis:**
```bash
# Check API URL accessibility
curl -I "$SUPERPOSITION_API_URL"

# Check DNS resolution
nslookup $(echo $SUPERPOSITION_API_URL | cut -d'/' -f3)

# Check network connectivity
ping $(echo $SUPERPOSITION_API_URL | cut -d'/' -f3)
```

**Solutions:**
1. **Verify API URL**: Ensure `SUPERPOSITION_API_URL` is correct
2. **Check network**: Verify internet connectivity and firewall settings
3. **Update timeout**: Increase `SUPERPOSITION_TIMEOUT` for slow networks
4. **Check proxy**: Configure proxy settings if behind corporate firewall

```bash
# Fix network timeout
export SUPERPOSITION_TIMEOUT=120

# Configure proxy (if needed)
export HTTP_PROXY=http://proxy.company.com:8080
export HTTPS_PROXY=http://proxy.company.com:8080
```

### Issue 2: Authentication Failed (401 Unauthorized)

**Symptoms:**
- "401 Unauthorized" errors
- "Invalid token" messages
- Authentication failures

**Diagnosis:**
```bash
# Check token format
echo $SUPERPOSITION_API_TOKEN | base64 -d | jq .

# Verify token expiration
python -c "
import jwt
import os
token = os.getenv('SUPERPOSITION_API_TOKEN')
try:
    decoded = jwt.decode(token, options={'verify_signature': False})
    print('Token expires:', decoded.get('exp'))
except Exception as e:
    print('Token decode error:', e)
"
```

**Solutions:**
1. **Verify token**: Check if token is valid and not expired
2. **Update token**: Get a new token from Superposition instance
3. **Check permissions**: Ensure token has required scopes

```bash
# Get new token (example)
NEW_TOKEN=$(curl -s -X POST \
  "$SUPERPOSITION_API_URL/auth/token" \
  -H "Content-Type: application/json" \
  -d '{"username": "your-username", "password": "your-password"}' | \
  jq -r '.access_token')

export SUPERPOSITION_API_TOKEN="$NEW_TOKEN"
```

### Issue 3: Workspace Not Found

**Symptoms:**
- "Workspace not found" errors
- "Invalid tenant" messages
- Permission denied for workspace operations

**Diagnosis:**
```bash
# List available workspaces
curl -H "Authorization: Bearer $SUPERPOSITION_API_TOKEN" \
     "$SUPERPOSITION_API_URL/workspaces"

# Check current workspace setting
echo "Current workspace: $SUPERPOSITION_DEFAULT_WORKSPACE"
```

**Solutions:**
1. **Verify workspace exists**: Check if workspace is created in Superposition
2. **Update workspace**: Use correct workspace name
3. **Check permissions**: Ensure user has access to workspace

```bash
# Create workspace (if needed)
curl -X POST \
  -H "Authorization: Bearer $SUPERPOSITION_API_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"name": "dev", "description": "Development workspace"}' \
  "$SUPERPOSITION_API_URL/workspaces"

# Update environment
export SUPERPOSITION_DEFAULT_WORKSPACE="correct-workspace-name"
```

### Issue 4: Tool Validation Errors

**Symptoms:**
- "Invalid input parameters" errors
- Schema validation failures
- Missing required fields

**Diagnosis:**
```bash
# Enable debug mode for detailed validation errors
export SUPERPOSITION_DEBUG=true

# Check tool schema
python -c "
from superposition_mcp.tools import get_tool_schema
schema = get_tool_schema('create_default_config')
print(schema)
"
```

**Solutions:**
1. **Check required fields**: Ensure all required parameters are provided
2. **Validate data types**: Check parameter types match schema
3. **Review documentation**: Check API reference for correct format

Example correct tool usage:
```json
{
  "workspace_id": "dev",
  "org_id": "juspay",
  "key": "my.config.key",
  "value": {"enabled": true},
  "schema": {"type": "object"},
  "description": "My configuration",
  "change_reason": "Initial setup"
}
```

### Issue 5: Performance Issues

**Symptoms:**
- Slow response times
- Timeout errors
- High CPU/memory usage

**Diagnosis:**
```bash
# Check response times
time curl -H "Authorization: Bearer $SUPERPOSITION_API_TOKEN" \
          "$SUPERPOSITION_API_URL/config/fast"

# Monitor resource usage
top -p $(pgrep -f superposition_mcp)

# Check cache hit rates
python -c "
from superposition_mcp.metrics import get_cache_stats
print(get_cache_stats())
"
```

**Solutions:**
1. **Increase cache TTL**: Cache responses longer
2. **Optimize connections**: Increase connection pool size
3. **Enable compression**: Use request compression

```bash
# Performance optimization
export SUPERPOSITION_CACHE_TTL=600
export SUPERPOSITION_MAX_CONNECTIONS=20
export SUPERPOSITION_REQUEST_COMPRESSION=true
```

## Error Messages

### "Connection timeout after 30 seconds"

**Cause**: Network latency or server overload
**Solution**: Increase timeout value

```bash
export SUPERPOSITION_TIMEOUT=90
```

### "SSL certificate verification failed"

**Cause**: Invalid or self-signed SSL certificate
**Solution**: Disable SSL verification (development only) or add CA certificate

```bash
# Development only - DO NOT use in production
export SUPERPOSITION_SSL_VERIFY=false

# Production - add custom CA certificate
export SUPERPOSITION_SSL_CERT_PATH=/path/to/ca-bundle.crt
```

### "Rate limit exceeded"

**Cause**: Too many requests to Superposition API
**Solution**: Implement request throttling

```bash
export SUPERPOSITION_RATE_LIMIT=50  # requests per minute
export SUPERPOSITION_RATE_BURST=10  # burst capacity
```

### "Invalid JSON in response"

**Cause**: API returned malformed JSON
**Solution**: Check API health and enable response logging

```bash
export SUPERPOSITION_DEBUG=true
export SUPERPOSITION_LOG_LEVEL=DEBUG
```

### "Memory limit exceeded"

**Cause**: Large cache or memory leak
**Solution**: Limit cache size and restart service

```bash
export SUPERPOSITION_MAX_CACHE_SIZE=100MB
sudo systemctl restart superposition-mcp
```

## Performance Issues

### Slow Tool Execution

**Symptoms:**
- Tools take longer than expected to complete
- Timeout errors for complex operations

**Diagnosis:**
```bash
# Enable performance metrics
export SUPERPOSITION_METRICS_ENABLED=true

# Check execution times
grep "tool_duration" /var/log/superposition-mcp.log | tail -20

# Monitor API response times
curl -w "@curl-format.txt" -H "Authorization: Bearer $TOKEN" "$API_URL/health"
```

**Solutions:**
1. **Optimize caching strategy**
2. **Increase concurrent connections**
3. **Use faster configuration endpoints**

```bash
# Performance tuning
export SUPERPOSITION_CACHE_TTL=900
export SUPERPOSITION_MAX_CONNECTIONS=30
export SUPERPOSITION_USE_FAST_ENDPOINTS=true
```

### High Memory Usage

**Symptoms:**
- Increasing memory consumption over time
- Out-of-memory errors

**Diagnosis:**
```bash
# Monitor memory usage
ps aux | grep superposition_mcp
cat /proc/$(pgrep superposition_mcp)/status | grep VmRSS

# Check cache size
du -sh /path/to/cache/directory
```

**Solutions:**
```bash
# Limit cache size
export SUPERPOSITION_MAX_CACHE_SIZE=500MB

# Enable cache cleanup
export SUPERPOSITION_CACHE_CLEANUP_INTERVAL=300  # 5 minutes

# Restart service periodically (systemd timer)
sudo systemctl enable superposition-mcp-restart.timer
```

## Network and Connectivity

### Proxy Configuration

If behind a corporate proxy:

```bash
# Set proxy environment variables
export HTTP_PROXY=http://proxy.company.com:8080
export HTTPS_PROXY=http://proxy.company.com:8080
export NO_PROXY=localhost,127.0.0.1,.local

# Configure proxy in application
export SUPERPOSITION_PROXY_URL=http://proxy.company.com:8080
export SUPERPOSITION_PROXY_AUTH=username:password
```

### DNS Resolution Issues

```bash
# Check DNS resolution
nslookup api.superposition.example.com

# Use custom DNS
export SUPERPOSITION_DNS_SERVERS=8.8.8.8,8.8.4.4

# Add to /etc/hosts if needed
echo "192.168.1.100 api.superposition.local" | sudo tee -a /etc/hosts
```

### Firewall Configuration

```bash
# Check if port is blocked
telnet api.superposition.example.com 443

# Open firewall port (if needed)
sudo ufw allow out 443/tcp
sudo firewall-cmd --permanent --add-port=443/tcp
```

## Configuration Problems

### Environment Variable Issues

**Check environment loading:**
```bash
# Test environment loading
python -c "
import os
from dotenv import load_dotenv
load_dotenv()
print('API URL:', os.getenv('SUPERPOSITION_API_URL'))
print('Token set:', bool(os.getenv('SUPERPOSITION_API_TOKEN')))
"
```

**Fix environment issues:**
```bash
# Source environment file explicitly
set -a; source .env; set +a

# Check file permissions
ls -la .env
chmod 600 .env  # Secure permissions

# Validate format
grep -v '^#' .env | grep '='  # Show non-comment lines
```

### Configuration File Validation

**Create validation script:**
```bash
#!/bin/bash
# validate-config.sh

echo "Validating Superposition MCP configuration..."

# Check required variables
required_vars=("SUPERPOSITION_API_URL" "SUPERPOSITION_API_TOKEN")
for var in "${required_vars[@]}"; do
    if [[ -z "${!var}" ]]; then
        echo "ERROR: $var is not set"
        exit 1
    else
        echo "✓ $var is set"
    fi
done

# Validate API URL format
if [[ ! $SUPERPOSITION_API_URL =~ ^https?:// ]]; then
    echo "ERROR: SUPERPOSITION_API_URL must start with http:// or https://"
    exit 1
fi

# Test API connectivity
if curl -f -s "$SUPERPOSITION_API_URL/health" > /dev/null; then
    echo "✓ API is reachable"
else
    echo "WARNING: API health check failed"
fi

echo "Configuration validation completed"
```

## Debug Mode

### Enable Comprehensive Debug Logging

```bash
# Enable all debug options
export SUPERPOSITION_DEBUG=true
export SUPERPOSITION_DEBUG_CONFIG=true
export SUPERPOSITION_DEBUG_CACHE=true
export SUPERPOSITION_DEBUG_HTTP=true
export SUPERPOSITION_LOG_LEVEL=DEBUG
```

### Debug Output Examples

**Configuration loading:**
```
DEBUG: Loading configuration from environment
DEBUG: API URL: https://api.superposition.example.com
DEBUG: Default workspace: dev
DEBUG: Cache TTL: 300 seconds
```

**HTTP requests:**
```
DEBUG: Making request to /config/fast
DEBUG: Request headers: {'Authorization': 'Bearer eyJ...', 'User-Agent': '...'}
DEBUG: Response status: 200
DEBUG: Response time: 0.245s
```

**Cache operations:**
```
DEBUG: Cache miss for key: config:dev:fast
DEBUG: Storing in cache with TTL: 300s
DEBUG: Cache hit for key: dimensions:dev
```

## Log Analysis

### Useful Log Patterns

**Find errors:**
```bash
grep -E "(ERROR|CRITICAL)" /var/log/superposition-mcp.log | tail -20
```

**Find slow operations:**
```bash
grep "duration.*[5-9]\." /var/log/superposition-mcp.log | tail -10
```

**Find authentication issues:**
```bash
grep -i "auth\|401\|403" /var/log/superposition-mcp.log | tail -10
```

**Find network errors:**
```bash
grep -E "(timeout|connection|network)" /var/log/superposition-mcp.log | tail -10
```

### Log Analysis Script

```bash
#!/bin/bash
# analyze-logs.sh

LOG_FILE="/var/log/superposition-mcp.log"
HOURS=${1:-1}  # Default to last hour

echo "Analyzing logs from last $HOURS hour(s)..."

# Error summary
echo "=== Error Summary ==="
grep -E "(ERROR|CRITICAL)" "$LOG_FILE" | \
    awk -v hours=$HOURS '$0 > systime() - hours*3600' | \
    cut -d' ' -f4- | sort | uniq -c | sort -nr

# Performance summary
echo "=== Performance Summary ==="
grep "tool_duration" "$LOG_FILE" | \
    awk -v hours=$HOURS '$0 > systime() - hours*3600' | \
    awk '{sum+=$NF; count++} END {print "Average:", sum/count "s", "Count:", count}'

# Top slow operations
echo "=== Slowest Operations ==="
grep "tool_duration" "$LOG_FILE" | \
    awk '{print $NF, $0}' | sort -nr | head -5

# Cache hit rate
echo "=== Cache Statistics ==="
cache_hits=$(grep "cache_hit" "$LOG_FILE" | wc -l)
cache_misses=$(grep "cache_miss" "$LOG_FILE" | wc -l)
if [[ $((cache_hits + cache_misses)) -gt 0 ]]; then
    hit_rate=$(( cache_hits * 100 / (cache_hits + cache_misses) ))
    echo "Hit rate: ${hit_rate}%"
fi
```

## Health Checks

### Comprehensive Health Check Script

```bash
#!/bin/bash
# health-check.sh

echo "Superposition MCP Server Health Check"
echo "===================================="

# Check process
if pgrep -f superposition_mcp > /dev/null; then
    echo "✓ Process is running"
else
    echo "✗ Process is not running"
    exit 1
fi

# Check configuration
if python3 -c "from superposition_mcp.config import Config; Config().validate()" 2>/dev/null; then
    echo "✓ Configuration is valid"
else
    echo "✗ Configuration is invalid"
    exit 1
fi

# Check API connectivity
if python3 -c "
import asyncio
from superposition_mcp.client import SuperpositionClient
client = SuperpositionClient()
asyncio.run(client.test_connection())
" 2>/dev/null; then
    echo "✓ API connectivity is working"
else
    echo "✗ API connectivity failed"
    exit 1
fi

# Check memory usage
memory_percent=$(ps -o %mem -p $(pgrep superposition_mcp) | tail -n1 | tr -d ' ')
if (( $(echo "$memory_percent < 80" | bc -l) )); then
    echo "✓ Memory usage is normal (${memory_percent}%)"
else
    echo "⚠ Memory usage is high (${memory_percent}%)"
fi

# Check log file size
log_size=$(stat -f%z /var/log/superposition-mcp.log 2>/dev/null || echo 0)
if [[ $log_size -lt 100000000 ]]; then  # 100MB
    echo "✓ Log file size is manageable"
else
    echo "⚠ Log file is large ($(( log_size / 1024 / 1024 ))MB)"
fi

echo "Health check completed"
```

### Monitoring Setup

**SystemD health check service:**
```ini
[Unit]
Description=Superposition MCP Health Check
Requires=superposition-mcp.service

[Service]
Type=oneshot
ExecStart=/opt/superposition-mcp/bin/health-check.sh
User=superposition

[Install]
WantedBy=multi-user.target
```

**Cron job for regular checks:**
```bash
# Add to crontab
*/5 * * * * /opt/superposition-mcp/bin/health-check.sh >> /var/log/superposition-health.log 2>&1
```

This troubleshooting guide should help you diagnose and resolve most common issues with the Superposition MCP Server. If you encounter issues not covered here, check the logs for detailed error messages and consider enabling debug mode for more information.