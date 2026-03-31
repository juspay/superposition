---
name: superposition-setup
description: Installation, deployment, and operations for Superposition. Use when setting up Superposition locally, deploying to production, configuring tenants, or troubleshooting operational issues.
license: Apache-2.0
compatibility: Requires Docker, Rust (for development), PostgreSQL
metadata:
  author: juspay
  version: "1.0"
---

# Superposition Setup & Operations

This skill helps you install, deploy, and operate Superposition - the context-aware configuration and experimentation platform.

## Quick Start

### Demo Mode (Fastest)

Run a pre-configured demo instance with sample data:

```bash
# Using Docker
docker run -p 8080:8080 ghcr.io/juspay/superposition-demo:latest

# Using Podman
podman run -p 8080:8080 ghcr.io/juspay/superposition-demo:latest
```

Access the admin UI at http://localhost:8080

**Note:** Demo image includes sample organizations, workspaces, and configurations. Not for production use.

### Production Image

```bash
docker run -p 8080:8080 ghcr.io/juspay/superposition:latest
```

## Development Setup

### Prerequisites

| Tool | Purpose |
|------|---------|
| Rust | Building from source |
| wasm-pack | WebAssembly frontend build |
| Docker | Running dependencies |
| libpq | PostgreSQL client library |
| OpenSSL | TLS support |

### macOS Setup

```bash
# Install dependencies
brew install libpq openssl libiconv awscli yq

# Set PostgreSQL library path
export PQ_LIB_DIR="$(brew --prefix libpq)/lib"

# Install Diesel CLI
cargo install diesel_cli --no-default-features --features postgres

# Configure AWS CLI (for local development)
aws configure
# Use: Access Key ID: Test, Secret: Test, Region: None

# Clone and setup
git clone https://github.com/juspay/superposition.git
cd superposition

# Start Docker
open --background -a Docker

# Setup database and environment
make setup

# Run Superposition
make run
```

### Linux Setup

```bash
# Install dependencies
sudo apt-get install build-essential libpq-dev openssl libssl-dev pkg-config awscli yq

# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Install wasm-pack
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh

# Install Diesel CLI
cargo install diesel_cli --no-default-features --features postgres

# Configure AWS CLI
aws configure

# Clone and setup
git clone https://github.com/juspay/superposition.git
cd superposition

# Start Docker
sudo systemctl start docker

# Setup and run
make setup
make run
```

### Nix Setup

```bash
# Install Docker
# Clone repository
git clone https://github.com/juspay/superposition.git
cd superposition

# Enter Nix development shell
nix develop

# Start Docker
open --background -a Docker  # macOS
# or sudo systemctl start docker  # Linux

# Setup and run
make setup
make run
```

### Verify Installation

```bash
# Health check
curl http://localhost:8080/health
# Expected: "Health is good :D"

# Check API version
curl http://localhost:8080/info
```

## Docker Compose Deployment

### Basic docker-compose.yml

```yaml
version: '3.8'

services:
  superposition:
    image: ghcr.io/juspay/superposition:latest
    ports:
      - "8080:8080"
    environment:
      - DATABASE_URL=postgresql://postgres:password@postgres:5432/superposition
      - TENANTS=prod,staging
      - HOST=0.0.0.0
      - PORT=8080
    depends_on:
      - postgres
    restart: unless-stopped

  postgres:
    image: postgres:15
    environment:
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=password
      - POSTGRES_DB=superposition
    volumes:
      - postgres_data:/var/lib/postgresql/data
    restart: unless-stopped

volumes:
  postgres_data:
```

```bash
docker-compose up -d
```

### With Redis Cache

```yaml
version: '3.8'

services:
  superposition:
    image: ghcr.io/juspay/superposition:latest
    ports:
      - "8080:8080"
    environment:
      - DATABASE_URL=postgresql://postgres:password@postgres:5432/superposition
      - REDIS_URL=redis://redis:6379
      - TENANTS=prod,staging
    depends_on:
      - postgres
      - redis

  postgres:
    image: postgres:15
    environment:
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=password
      - POSTGRES_DB=superposition
    volumes:
      - postgres_data:/var/lib/postgresql/data

  redis:
    image: redis:7-alpine
    volumes:
      - redis_data:/data

volumes:
  postgres_data:
  redis_data:
```

## Environment Variables

| Variable | Required | Description |
|----------|----------|-------------|
| `DATABASE_URL` | Yes | PostgreSQL connection string |
| `TENANTS` | Yes | Comma-separated tenant list |
| `HOST` | No | Bind host (default: `0.0.0.0`) |
| `PORT` | No | Bind port (default: `8080`) |
| `REDIS_URL` | No | Redis connection for caching |
| `REDIS_KEY_TTL` | No | Cache TTL in seconds |
| `KAFKA_BROKERS` | No | Kafka brokers for events |
| `KAFKA_TOPIC` | No | Kafka topic for events |
| `KAFKA_CONSUMER_GROUP` | No | Kafka consumer group ID |
| `AWS_ACCESS_KEY_ID` | No | AWS credentials for S3 |
| `AWS_SECRET_ACCESS_KEY` | No | AWS credentials for S3 |
| `AWS_REGION` | No | AWS region (default: `us-east-1`) |
| `S3_BUCKET` | No | S3 bucket for backups |
| `JWT_SECRET` | Yes | Secret for JWT tokens |
| `ENCRYPTION_KEY` | Yes | Master encryption key |
| `SERVICE_PREFIX` | No | Service name prefix |
| `WORKER_ID` | No | Worker identifier |
| `APP_ENV` | No | Application environment |
| `SUPERPOSITION_VERSION` | No | Application version |
| `LOG_LEVEL` | No | Log level (default: `info`) |
| `RUST_LOG` | No | Rust log filter |

## Multi-Tenant Setup

### Creating New Tenants

```bash
# Create a new tenant workspace
make tenant TENANT=mycompany

# Add to TENANTS env variable
export TENANTS=dev,test,mycompany

# Restart server
make run
```

### Tenant Isolation

Each tenant has completely isolated:
- Dimensions
- Default configs
- Contexts/Overrides
- Experiments
- Functions
- Type templates
- Webhooks

### Organization & Workspace Structure

```text
Organization (e.g., "acme-corp")
├── Workspace: "production"
│   ├── Dimensions: city, user_tier, platform
│   ├── Default configs: ...
│   └── Experiments: ...
├── Workspace: "staging"
│   └── ...
└── Workspace: "development"
    └── ...
```

## Production Deployment

### Kubernetes with Helm

```bash
# Add Helm repository
helm repo add superposition https://juspay.github.io/superposition/helm

# Install with default values
helm install superposition superposition/superposition

# Install with custom values
helm install superposition superposition/superposition \
  --set replicaCount=3 \
  --set image.tag=v0.100.0 \
  --set postgresql.enabled=true \
  --set redis.enabled=true \
  -f values.yaml
```

### Helm values.yaml

```yaml
replicaCount: 3

image:
  repository: ghcr.io/juspay/superposition
  tag: v0.100.0
  pullPolicy: IfNotPresent

service:
  type: ClusterIP
  port: 8080

ingress:
  enabled: true
  className: nginx
  annotations:
    cert-manager.io/cluster-issuer: letsencrypt-prod
  hosts:
    - host: superposition.example.com
      paths:
        - path: /
          pathType: Prefix
  tls:
    - secretName: superposition-tls
      hosts:
        - superposition.example.com

resources:
  limits:
    cpu: 1000m
    memory: 1Gi
  requests:
    cpu: 250m
    memory: 512Mi

postgresql:
  enabled: true
  auth:
    database: superposition
    username: superposition
    password: changeme

redis:
  enabled: true
  auth:
    password: changeme

config:
  tenants: prod,staging
  logLevel: info
```

### High Availability Setup

```yaml
# values-ha.yaml
replicaCount: 3

resources:
  limits:
    cpu: 2000m
    memory: 2Gi
  requests:
    cpu: 500m
    memory: 1Gi

postgresql:
  enabled: false  # Use managed PostgreSQL

externalPostgresql:
  host: postgres.managed.cloud
  port: 5432
  database: superposition
  username: superposition
  password: ${POSTGRES_PASSWORD}

redis:
  enabled: false  # Use managed Redis

externalRedis:
  host: redis.managed.cloud
  port: 6379
  password: ${REDIS_PASSWORD}

autoscaling:
  enabled: true
  minReplicas: 3
  maxReplicas: 10
  targetCPUUtilizationPercentage: 70
```

## Security Configuration

### Authentication

```yaml
# Enable JWT authentication
auth:
  enabled: true
  jwtSecret: ${JWT_SECRET}
  tokenExpiry: 24h
```

### RBAC Setup

Superposition supports role-based access control:

| Role | Permissions |
|------|-------------|
| `admin` | Full access to all operations |
| `config_manager` | Create/update configs, dimensions |
| `experiment_manager` | Create/ramp/conclude experiments |
| `viewer` | Read-only access |

### Encryption

```bash
# Generate encryption key
openssl rand -base64 32

# Set in environment
export ENCRYPTION_KEY=your-generated-key
```

### TLS Configuration

```yaml
# Ingress TLS with cert-manager
ingress:
  enabled: true
  annotations:
    cert-manager.io/cluster-issuer: letsencrypt-prod
  tls:
    - secretName: superposition-tls
      hosts:
        - superposition.example.com
```

## Backup & Recovery

### Database Backup

```bash
# Backup database
pg_dump $DATABASE_URL > superposition_backup_$(date +%Y%m%d).sql

# Or using Docker
docker exec postgres pg_dump -U postgres superposition > backup.sql
```

### Restore Database

```bash
# Restore from backup
psql $DATABASE_URL < backup.sql
```

### Configuration Export

```bash
# Export workspace configuration
curl -X GET http://localhost:8080/config.toml \
  -H "x-org-id: myorg" \
  -H "x-workspace: production" \
  > config_backup.toml
```

## Monitoring

### Health Endpoints

```bash
# Liveness probe
curl http://localhost:8080/health

# Readiness probe
curl http://localhost:8080/ready
```

### Prometheus Metrics

```yaml
# Enable Prometheus metrics
metrics:
  enabled: true
  path: /metrics
```

### Grafana Dashboard

Import the provided Grafana dashboard from `grafana/` directory:

```bash
# Dashboard includes:
# - Request latency (p50, p95, p99)
# - Request rate by endpoint
# - Error rate
# - Active experiments count
# - Configuration resolution time
```

### Logging

```bash
# Set log level
export LOG_LEVEL=debug
export RUST_LOG=superposition=debug,sqlx=info

# Structured JSON logging
export LOG_FORMAT=json
```

## Troubleshooting

### Common Issues

**Database connection fails:**
```bash
# Check PostgreSQL is running
docker ps | grep postgres

# Test connection
psql $DATABASE_URL -c "SELECT 1"
```

**Tenant not found:**
```bash
# Verify tenant exists in TENANTS
echo $TENANTS

# Create tenant if missing
make tenant TENANT=mytenant
```

**Config resolution slow:**
```bash
# Enable query logging
export RUST_LOG=sqlx=debug

# Check for missing indexes
# Add Redis cache
export REDIS_URL=redis://localhost:6379
```

### Debug Mode

```bash
# Run with debug logging
RUST_LOG=debug make run

# Run specific module debug
RUST_LOG=superposition_core=debug,superposition_sdk=trace make run
```

### Logs

```bash
# Docker logs
docker logs superposition

# Kubernetes logs
kubectl logs -l app=superposition -f

# With stern (multi-pod)
stern superposition
```

## Make Targets

| Target | Description |
|--------|-------------|
| `make setup` | Setup database and environment |
| `make run` | Run development server |
| `make tenant TENANT=name` | Create new tenant |
| `make db-init` | Initialize database |
| `make kill` | Stop all containers |
| `make ci-test` | Run unit tests |
| `make ci-build` | Build Docker image |
| `make ci-push` | Push Docker image |

## Related Skills

- [superposition-config](../superposition-config/) - Configure dimensions and contexts
- [superposition-experiments](../superposition-experiments/) - Run experiments
- [superposition-api](../superposition-api/) - API reference

See [references/REFERENCE.md](references/REFERENCE.md) for advanced deployment options.