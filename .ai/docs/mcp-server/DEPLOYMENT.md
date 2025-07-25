# Deployment Guide

This guide covers various deployment options for the Superposition MCP Server.

## Table of Contents

- [Local Development](#local-development)
- [Docker Deployment](#docker-deployment)
- [Production Deployment](#production-deployment)
- [Cloud Deployment](#cloud-deployment)
- [Monitoring and Health Checks](#monitoring-and-health-checks)

## Local Development

### Quick Start

```bash
# Clone repository
git clone <repository-url>
cd superposition/mcp-server

# Create virtual environment
python -m venv myenv
source myenv/bin/activate  # On Windows: myenv\Scripts\activate

# Install dependencies
pip install -e .

# Create configuration
cp examples/development-config.env .env
# Edit .env with your actual values

# Run the server
python src/superposition_mcp/main.py
```

### Development with Auto-Reload

```bash
# Install development dependencies
pip install -e ".[dev]"

# Run with auto-reload
watchfiles python src/superposition_mcp/main.py src/
```

## Docker Deployment

### Basic Docker Setup

Create a `Dockerfile`:

```dockerfile
FROM python:3.11-slim

# Set working directory
WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements and install Python dependencies
COPY pyproject.toml .
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copy source code
COPY src/ ./src/
COPY . .

# Install the package
RUN pip install -e .

# Create non-root user
RUN groupadd -r appuser && useradd -r -g appuser appuser
RUN chown -R appuser:appuser /app
USER appuser

# Expose port (if running as HTTP server)
EXPOSE 8000

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
  CMD python -c "import asyncio; from superposition_mcp.health import health_check; asyncio.run(health_check())" || exit 1

# Run the server
CMD ["python", "src/superposition_mcp/main.py"]
```

### Build and Run

```bash
# Build image
docker build -t superposition-mcp:latest .

# Run container
docker run -d \
  --name superposition-mcp \
  -e SUPERPOSITION_API_URL=https://your-api.com \
  -e SUPERPOSITION_API_TOKEN=your-token \
  -e SUPERPOSITION_DEFAULT_WORKSPACE=production \
  -p 8000:8000 \
  --restart unless-stopped \
  superposition-mcp:latest
```

### Docker Compose

Create `docker-compose.yml`:

```yaml
version: '3.8'

services:
  superposition-mcp:
    build: .
    container_name: superposition-mcp
    environment:
      - SUPERPOSITION_API_URL=${SUPERPOSITION_API_URL}
      - SUPERPOSITION_API_TOKEN=${SUPERPOSITION_API_TOKEN}
      - SUPERPOSITION_DEFAULT_WORKSPACE=${SUPERPOSITION_DEFAULT_WORKSPACE:-production}
      - SUPERPOSITION_DEFAULT_ORG=${SUPERPOSITION_DEFAULT_ORG:-juspay}
      - SUPERPOSITION_DEBUG=${SUPERPOSITION_DEBUG:-false}
      - SUPERPOSITION_LOG_LEVEL=${SUPERPOSITION_LOG_LEVEL:-INFO}
    volumes:
      - ./logs:/var/log
      - ./cache:/app/cache
    ports:
      - "8000:8000"
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "python", "-c", "import asyncio; from superposition_mcp.health import health_check; asyncio.run(health_check())"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 40s
    networks:
      - superposition-network

  # Optional: Redis for caching
  redis:
    image: redis:7-alpine
    container_name: superposition-redis
    volumes:
      - redis-data:/data
    networks:
      - superposition-network
    restart: unless-stopped

volumes:
  redis-data:

networks:
  superposition-network:
    driver: bridge
```

Run with Docker Compose:

```bash
# Create .env file with your configuration
cp examples/production-config.env .env

# Start services
docker-compose up -d

# View logs
docker-compose logs -f superposition-mcp

# Stop services
docker-compose down
```

## Production Deployment

### Systemd Service

Create `/etc/systemd/system/superposition-mcp.service`:

```ini
[Unit]
Description=Superposition MCP Server
Documentation=https://github.com/your-org/superposition
After=network.target network-online.target
Wants=network-online.target

[Service]
Type=simple
User=superposition
Group=superposition
WorkingDirectory=/opt/superposition-mcp
ExecStart=/opt/superposition-mcp/venv/bin/python src/superposition_mcp/main.py
ExecReload=/bin/kill -HUP $MAINPID
Restart=always
RestartSec=5
TimeoutStopSec=30

# Environment
EnvironmentFile=/opt/superposition-mcp/.env

# Security
NoNewPrivileges=true
PrivateTmp=true
ProtectSystem=strict
ProtectHome=true
ReadWritePaths=/opt/superposition-mcp/logs /opt/superposition-mcp/cache

# Limits
LimitNOFILE=65536
LimitNPROC=4096

[Install]
WantedBy=multi-user.target
```

### Installation Script

Create `install.sh`:

```bash
#!/bin/bash

set -e

# Configuration
APP_USER="superposition"
APP_DIR="/opt/superposition-mcp"
SERVICE_NAME="superposition-mcp"

# Create user
sudo useradd --system --home-dir "$APP_DIR" --shell /bin/false "$APP_USER" || true

# Create directories
sudo mkdir -p "$APP_DIR"/{logs,cache,config}
sudo chown -R "$APP_USER:$APP_USER" "$APP_DIR"

# Install application
sudo -u "$APP_USER" python -m venv "$APP_DIR/venv"
sudo -u "$APP_USER" "$APP_DIR/venv/bin/pip" install superposition-mcp

# Copy configuration
sudo cp examples/production-config.env "$APP_DIR/.env"
sudo chown "$APP_USER:$APP_USER" "$APP_DIR/.env"
sudo chmod 600 "$APP_DIR/.env"

# Install systemd service
sudo cp systemd/superposition-mcp.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable "$SERVICE_NAME"

echo "Installation complete!"
echo "1. Edit $APP_DIR/.env with your configuration"
echo "2. Start the service: sudo systemctl start $SERVICE_NAME"
echo "3. Check status: sudo systemctl status $SERVICE_NAME"
```

### Nginx Reverse Proxy

Create `/etc/nginx/sites-available/superposition-mcp`:

```nginx
upstream superposition_mcp {
    server 127.0.0.1:8000;
    # Add more instances for load balancing
    # server 127.0.0.1:8001;
    # server 127.0.0.1:8002;
}

server {
    listen 80;
    server_name mcp.superposition.example.com;
    
    # Redirect to HTTPS
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name mcp.superposition.example.com;

    # SSL Configuration
    ssl_certificate /etc/ssl/certs/superposition-mcp.crt;
    ssl_certificate_key /etc/ssl/private/superposition-mcp.key;
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers HIGH:!aNULL:!MD5;

    # Security Headers
    add_header X-Frame-Options DENY;
    add_header X-Content-Type-Options nosniff;
    add_header X-XSS-Protection "1; mode=block";
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;

    # Rate Limiting
    limit_req_zone $binary_remote_addr zone=mcp:10m rate=10r/s;
    limit_req zone=mcp burst=20 nodelay;

    location / {
        proxy_pass http://superposition_mcp;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        
        # Timeouts
        proxy_connect_timeout 60s;
        proxy_send_timeout 60s;
        proxy_read_timeout 60s;
        
        # Buffer settings
        proxy_buffering on;
        proxy_buffer_size 128k;
        proxy_buffers 4 256k;
        proxy_busy_buffers_size 256k;
    }

    # Health check endpoint
    location /health {
        proxy_pass http://superposition_mcp/health;
        access_log off;
    }

    # Logs
    access_log /var/log/nginx/superposition-mcp.access.log;
    error_log /var/log/nginx/superposition-mcp.error.log;
}
```

Enable the site:

```bash
sudo ln -s /etc/nginx/sites-available/superposition-mcp /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx
```

## Cloud Deployment

### AWS ECS

Create `task-definition.json`:

```json
{
  "family": "superposition-mcp",
  "networkMode": "awsvpc",
  "requiresCompatibilities": ["FARGATE"],
  "cpu": "256",
  "memory": "512",
  "executionRoleArn": "arn:aws:iam::ACCOUNT:role/ecsTaskExecutionRole",
  "taskRoleArn": "arn:aws:iam::ACCOUNT:role/superposition-mcp-task-role",
  "containerDefinitions": [
    {
      "name": "superposition-mcp",
      "image": "your-account.dkr.ecr.region.amazonaws.com/superposition-mcp:latest",
      "essential": true,
      "portMappings": [
        {
          "containerPort": 8000,
          "protocol": "tcp"
        }
      ],
      "environment": [
        {
          "name": "SUPERPOSITION_DEFAULT_WORKSPACE",
          "value": "production"
        }
      ],
      "secrets": [
        {
          "name": "SUPERPOSITION_API_URL",
          "valueFrom": "arn:aws:secretsmanager:region:account:secret:superposition/api-url"
        },
        {
          "name": "SUPERPOSITION_API_TOKEN",
          "valueFrom": "arn:aws:secretsmanager:region:account:secret:superposition/api-token"
        }
      ],
      "logConfiguration": {
        "logDriver": "awslogs",
        "options": {
          "awslogs-group": "/ecs/superposition-mcp",
          "awslogs-region": "us-west-2",
          "awslogs-stream-prefix": "ecs"
        }
      },
      "healthCheck": {
        "command": [
          "CMD-SHELL",
          "python -c 'import asyncio; from superposition_mcp.health import health_check; asyncio.run(health_check())' || exit 1"
        ],
        "interval": 30,
        "timeout": 5,
        "retries": 3,
        "startPeriod": 60
      }
    }
  ]
}
```

Deploy to ECS:

```bash
# Build and push image
docker build -t superposition-mcp .
docker tag superposition-mcp:latest $ECR_REPO:latest
docker push $ECR_REPO:latest

# Register task definition
aws ecs register-task-definition --cli-input-json file://task-definition.json

# Create or update service
aws ecs create-service \
  --cluster production \
  --service-name superposition-mcp \
  --task-definition superposition-mcp \
  --desired-count 2 \
  --launch-type FARGATE \
  --network-configuration "awsvpcConfiguration={subnets=[subnet-xxx,subnet-yyy],securityGroups=[sg-xxx],assignPublicIp=ENABLED}"
```

### Kubernetes

Create `k8s/deployment.yaml`:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: superposition-mcp
  namespace: superposition
  labels:
    app: superposition-mcp
spec:
  replicas: 3
  selector:
    matchLabels:
      app: superposition-mcp
  template:
    metadata:
      labels:
        app: superposition-mcp
    spec:
      containers:
      - name: superposition-mcp
        image: superposition-mcp:latest
        ports:
        - containerPort: 8000
        env:
        - name: SUPERPOSITION_API_URL
          valueFrom:
            secretKeyRef:
              name: superposition-secrets
              key: api-url
        - name: SUPERPOSITION_API_TOKEN
          valueFrom:
            secretKeyRef:
              name: superposition-secrets
              key: api-token
        - name: SUPERPOSITION_DEFAULT_WORKSPACE
          value: "production"
        - name: SUPERPOSITION_DEFAULT_ORG
          value: "your-org"
        resources:
          requests:
            memory: "256Mi"
            cpu: "100m"
          limits:
            memory: "512Mi"
            cpu: "500m"
        livenessProbe:
          exec:
            command:
            - python
            - -c
            - "import asyncio; from superposition_mcp.health import health_check; asyncio.run(health_check())"
          initialDelaySeconds: 30
          periodSeconds: 30
          timeoutSeconds: 10
          failureThreshold: 3
        readinessProbe:
          exec:
            command:
            - python
            - -c
            - "import asyncio; from superposition_mcp.health import health_check; asyncio.run(health_check())"
          initialDelaySeconds: 10
          periodSeconds: 10
          timeoutSeconds: 5
          failureThreshold: 3
        volumeMounts:
        - name: cache
          mountPath: /app/cache
        - name: logs
          mountPath: /var/log
      volumes:
      - name: cache
        emptyDir: {}
      - name: logs
        emptyDir: {}
---
apiVersion: v1
kind: Service
metadata:
  name: superposition-mcp-service
  namespace: superposition
spec:
  selector:
    app: superposition-mcp
  ports:
  - protocol: TCP
    port: 80
    targetPort: 8000
  type: ClusterIP
---
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: superposition-mcp-ingress
  namespace: superposition
  annotations:
    kubernetes.io/ingress.class: nginx
    cert-manager.io/cluster-issuer: letsencrypt-prod
    nginx.ingress.kubernetes.io/rate-limit: "100"
spec:
  tls:
  - hosts:
    - mcp.superposition.example.com
    secretName: superposition-mcp-tls
  rules:
  - host: mcp.superposition.example.com
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: superposition-mcp-service
            port:
              number: 80
```

Create secrets:

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: superposition-secrets
  namespace: superposition
type: Opaque
data:
  api-url: <base64-encoded-url>
  api-token: <base64-encoded-token>
```

Deploy to Kubernetes:

```bash
# Create namespace
kubectl create namespace superposition

# Apply secrets
kubectl apply -f k8s/secrets.yaml

# Apply deployment
kubectl apply -f k8s/deployment.yaml

# Check status
kubectl get pods -n superposition
kubectl logs -n superposition deployment/superposition-mcp
```

## Monitoring and Health Checks

### Health Check Endpoints

The server provides several health check endpoints:

```bash
# Basic health check
curl http://localhost:8000/health

# Detailed health check with dependencies
curl http://localhost:8000/health/detailed

# API connectivity check
curl -H "Authorization: Bearer $TOKEN" \
     http://localhost:8000/health/api
```

### Prometheus Metrics

Add Prometheus monitoring:

```python
# metrics.py
from prometheus_client import Counter, Histogram, Gauge, start_http_server

REQUEST_COUNT = Counter('mcp_requests_total', 'Total MCP requests', ['tool', 'status'])
REQUEST_DURATION = Histogram('mcp_request_duration_seconds', 'Request duration', ['tool'])
ACTIVE_CONNECTIONS = Gauge('mcp_active_connections', 'Active connections')
CACHE_HIT_RATE = Gauge('mcp_cache_hit_rate', 'Cache hit rate')

def start_metrics_server(port=9090):
    start_http_server(port)
```

### Log Monitoring

Configure structured logging for monitoring:

```python
import structlog

logger = structlog.get_logger()

# Log successful operations
logger.info("tool_executed", 
           tool="get_config", 
           duration=0.245, 
           workspace="production",
           status="success")

# Log errors
logger.error("tool_failed",
            tool="create_experiment",
            error="ValidationError",
            workspace="production",
            status="error")
```

### Alerting

Set up alerts for common issues:

```yaml
# Prometheus alerts
groups:
- name: superposition-mcp
  rules:
  - alert: SuperpositionMCPDown
    expr: up{job="superposition-mcp"} == 0
    for: 1m
    labels:
      severity: critical
    annotations:
      summary: Superposition MCP Server is down

  - alert: SuperpositionMCPHighErrorRate
    expr: rate(mcp_requests_total{status="error"}[5m]) > 0.1
    for: 2m
    labels:
      severity: warning
    annotations:
      summary: High error rate in Superposition MCP Server

  - alert: SuperpositionMCPSlowRequests
    expr: histogram_quantile(0.95, rate(mcp_request_duration_seconds_bucket[5m])) > 5
    for: 5m
    labels:
      severity: warning
    annotations:
      summary: Slow requests in Superposition MCP Server
```

### Backup and Recovery

For production deployments, implement backup strategies:

```bash
#!/bin/bash
# backup.sh

BACKUP_DIR="/var/backups/superposition-mcp"
DATE=$(date +%Y%m%d_%H%M%S)

# Backup configuration
cp /opt/superposition-mcp/.env "$BACKUP_DIR/config_$DATE.env"

# Backup logs
tar -czf "$BACKUP_DIR/logs_$DATE.tar.gz" /opt/superposition-mcp/logs/

# Backup cache state (if needed)
tar -czf "$BACKUP_DIR/cache_$DATE.tar.gz" /opt/superposition-mcp/cache/

# Cleanup old backups (keep 30 days)
find "$BACKUP_DIR" -name "*.tar.gz" -mtime +30 -delete
find "$BACKUP_DIR" -name "*.env" -mtime +30 -delete
```

### Performance Tuning

Monitor and tune performance:

```bash
# Monitor resource usage
htop
iotop
netstat -an | grep :8000

# Check application metrics
curl http://localhost:9090/metrics

# Analyze logs
tail -f /var/log/superposition-mcp.log | grep "duration"
```

This deployment guide covers most common scenarios. Choose the deployment method that best fits your infrastructure and requirements.