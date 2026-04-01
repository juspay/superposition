# Setup & Operations Reference

Complete reference for installing, deploying, and operating Superposition.

## Table of Contents

- [Environment Variables](#environment-variables)
- [Docker Deployment](#docker-deployment)
- [Kubernetes Deployment](#kubernetes-deployment)
- [Database Management](#database-management)
- [Security Configuration](#security-configuration)
- [Monitoring & Observability](#monitoring--observability)
- [Backup & Recovery](#backup--recovery)
- [Troubleshooting](#troubleshooting)

---

## Environment Variables

### Core Configuration

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `DATABASE_URL` | Yes | - | PostgreSQL connection URL |
| `TENANTS` | Yes | `dev,test` | Comma-separated list of tenant names |
| `HOST` | No | `0.0.0.0` | Server bind host |
| `PORT` | No | `8080` | Server bind port |
| `LOG_LEVEL` | No | `info` | Logging level |
| `RUST_LOG` | No | - | Rust-specific log filter |

### Database Configuration

| Variable | Description |
|----------|-------------|
| `DATABASE_URL` | `postgresql://user:password@host:port/database` |
| `DATABASE_MAX_CONNECTIONS` | Maximum pool connections (default: 10) |
| `DATABASE_MIN_CONNECTIONS` | Minimum pool connections (default: 1) |
| `DATABASE_CONNECT_TIMEOUT` | Connection timeout in seconds (default: 30) |

### Cache Configuration

| Variable | Description |
|----------|-------------|
| `REDIS_URL` | Redis connection URL (`redis://host:port`) |
| `CACHE_TTL` | Default cache TTL in seconds (default: 300) |
| `CACHE_PREFIX` | Key prefix for multi-tenant isolation |

### Event Streaming

| Variable | Description |
|----------|-------------|
| `KAFKA_BROKERS` | Comma-separated Kafka broker addresses |
| `KAFKA_TOPIC` | Topic for configuration events |
| `KAFKA_CONSUMER_GROUP` | Consumer group ID |

### AWS/S3 Configuration

| Variable | Description |
|----------|-------------|
| `AWS_ACCESS_KEY_ID` | AWS access key |
| `AWS_SECRET_ACCESS_KEY` | AWS secret key |
| `AWS_REGION` | AWS region (default: `us-east-1`) |
| `S3_BUCKET` | S3 bucket for backups |

### Security Configuration

| Variable | Description |
|----------|-------------|
| `JWT_SECRET` | Secret for JWT token signing |
| `JWT_EXPIRY` | Token expiry duration (default: `24h`) |
| `ENCRYPTION_KEY` | Master key for data encryption |
| `CORS_ORIGINS` | Allowed CORS origins |

---

## Docker Deployment

### Single Container

```bash
# Basic run
docker run -p 8080:8080 \
  -e DATABASE_URL=postgresql://user:pass@host:5432/db \
  -e TENANTS=prod,staging \
  ghcr.io/juspay/superposition:latest

# With volume for persistence
docker run -p 8080:8080 \
  -v superposition_data:/data \
  -e DATABASE_URL=... \
  ghcr.io/juspay/superposition:latest
```

### Docker Compose - Full Stack

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
      - JWT_SECRET=REPLACE_WITH_SECURE_RANDOM_STRING  # Generate with: openssl rand -base64 32
      - ENCRYPTION_KEY=REPLACE_WITH_SECURE_RANDOM_STRING  # Generate with: openssl rand -base64 32
      - LOG_LEVEL=info
    depends_on:
      postgres:
        condition: service_healthy
      redis:
        condition: service_started
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8080/health"]
      interval: 30s
      timeout: 10s
      retries: 3
    restart: unless-stopped
    deploy:
      resources:
        limits:
          cpus: '2'
          memory: 2G
        reservations:
          cpus: '0.5'
          memory: 512M

  postgres:
    image: postgres:15-alpine
    environment:
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=password
      - POSTGRES_DB=superposition
    volumes:
      - postgres_data:/var/lib/postgresql/data
      - ./init.sql:/docker-entrypoint-initdb.d/init.sql
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U postgres"]
      interval: 10s
      timeout: 5s
      retries: 5
    restart: unless-stopped

  redis:
    image: redis:7-alpine
    command: redis-server --appendonly yes
    volumes:
      - redis_data:/data
    restart: unless-stopped

volumes:
  postgres_data:
  redis_data:
```

### Docker Compose - Development

```yaml
version: '3.8'

services:
  postgres:
    image: postgres:15-alpine
    ports:
      - "5432:5432"
    environment:
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=password
      - POSTGRES_DB=superposition
    volumes:
      - postgres_dev:/var/lib/postgresql/data

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"

volumes:
  postgres_dev:
```

---

## Kubernetes Deployment

### Basic Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: superposition
  labels:
    app: superposition
spec:
  replicas: 3
  selector:
    matchLabels:
      app: superposition
  template:
    metadata:
      labels:
        app: superposition
    spec:
      containers:
        - name: superposition
          image: ghcr.io/juspay/superposition:v0.100.0
          ports:
            - containerPort: 8080
          env:
            - name: DATABASE_URL
              valueFrom:
                secretKeyRef:
                  name: superposition-secrets
                  key: database-url
            - name: TENANTS
              value: "prod,staging"
            - name: REDIS_URL
              value: "redis://redis:6379"
          resources:
            requests:
              memory: "512Mi"
              cpu: "250m"
            limits:
              memory: "1Gi"
              cpu: "1000m"
          livenessProbe:
            httpGet:
              path: /health
              port: 8080
            initialDelaySeconds: 10
            periodSeconds: 30
          readinessProbe:
            httpGet:
              path: /ready
              port: 8080
            initialDelaySeconds: 5
            periodSeconds: 10
---
apiVersion: v1
kind: Service
metadata:
  name: superposition
spec:
  selector:
    app: superposition
  ports:
    - port: 80
      targetPort: 8080
  type: ClusterIP
---
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: superposition
  annotations:
    nginx.ingress.kubernetes.io/proxy-body-size: "10m"
spec:
  rules:
    - host: superposition.example.com
      http:
        paths:
          - path: /
            pathType: Prefix
            backend:
              service:
                name: superposition
                port:
                  number: 80
```

### With Horizontal Pod Autoscaler

```yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: superposition-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: superposition
  minReplicas: 3
  maxReplicas: 10
  metrics:
    - type: Resource
      resource:
        name: cpu
        target:
          type: Utilization
          averageUtilization: 70
    - type: Resource
      resource:
        name: memory
        target:
          type: Utilization
          averageUtilization: 80
```

### With Pod Disruption Budget

```yaml
apiVersion: policy/v1
kind: PodDisruptionBudget
metadata:
  name: superposition-pdb
spec:
  minAvailable: 2
  selector:
    matchLabels:
      app: superposition
```

### Helm Chart Values

```yaml
# values-production.yaml

replicaCount: 3

image:
  repository: ghcr.io/juspay/superposition
  tag: v0.100.0
  pullPolicy: IfNotPresent

serviceAccount:
  create: true
  annotations:
    eks.amazonaws.com/role-arn: arn:aws:iam::123456789:role/superposition

service:
  type: ClusterIP
  port: 8080

ingress:
  enabled: true
  className: nginx
  annotations:
    cert-manager.io/cluster-issuer: letsencrypt-prod
    nginx.ingress.kubernetes.io/proxy-body-size: "10m"
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
    cpu: 2000m
    memory: 2Gi
  requests:
    cpu: 500m
    memory: 1Gi

autoscaling:
  enabled: true
  minReplicas: 3
  maxReplicas: 10
  targetCPUUtilizationPercentage: 70
  targetMemoryUtilizationPercentage: 80

podDisruptionBudget:
  enabled: true
  minAvailable: 2

nodeSelector: {}

tolerations: []

affinity:
  podAntiAffinity:
    preferredDuringSchedulingIgnoredDuringExecution:
      - weight: 100
        podAffinityTerm:
          labelSelector:
            matchLabels:
              app: superposition
          topologyKey: kubernetes.io/hostname

# External database
postgresql:
  enabled: false

externalDatabase:
  host: postgres.production.svc.cluster.local
  port: 5432
  database: superposition
  existingSecret: postgres-credentials
  existingSecretPasswordKey: password

# External Redis
redis:
  enabled: false

externalRedis:
  host: redis.production.svc.cluster.local
  port: 6379
  existingSecret: redis-credentials
  existingSecretPasswordKey: password

# Application config
config:
  tenants: prod,staging
  logLevel: info
  jwtExpiry: 24h

secrets:
  jwtSecret:
    existingSecret: superposition-secrets
    existingSecretKey: jwt-secret
  encryptionKey:
    existingSecret: superposition-secrets
    existingSecretKey: encryption-key
```

---

## Database Management

### Schema Migration

```bash
# Run migrations (development)
diesel migration run

# Revert last migration
diesel migration redo

# Create new migration
diesel migration generate add_new_table
```

### Connection Pooling

```yaml
# PgBouncer configuration
[databases]
superposition = host=postgres port=5432 dbname=superposition

[pgbouncer]
pool_mode = transaction
max_client_conn = 1000
default_pool_size = 25
min_pool_size = 5
reserve_pool_size = 5
```

### Database Tuning

```sql
-- Recommended PostgreSQL settings
ALTER SYSTEM SET max_connections = 200;
ALTER SYSTEM SET shared_buffers = '256MB';
ALTER SYSTEM SET effective_cache_size = '1GB';
ALTER SYSTEM SET maintenance_work_mem = '64MB';
ALTER SYSTEM SET checkpoint_completion_target = 0.9;
ALTER SYSTEM SET wal_buffers = '16MB';
ALTER SYSTEM SET default_statistics_target = 100;
ALTER SYSTEM SET random_page_cost = 1.1;
ALTER SYSTEM SET effective_io_concurrency = 200;
ALTER SYSTEM SET work_mem = '2621kB';
ALTER SYSTEM SET min_wal_size = '1GB';
ALTER SYSTEM SET max_wal_size = '4GB';
```

---

## Security Configuration

### JWT Authentication

```yaml
# Application configuration
auth:
  enabled: true
  jwtSecret: ${JWT_SECRET}
  tokenExpiry: 24h
  refreshTokenExpiry: 168h
  issuer: superposition.example.com
  audience:
    - api://superposition
```

### RBAC Configuration

```yaml
# RBAC policy file
roles:
  admin:
    permissions:
      - "*"
  config_manager:
    permissions:
      - "dimensions:*"
      - "default-config:*"
      - "contexts:*"
      - "functions:*"
      - "type-templates:*"
  experiment_manager:
    permissions:
      - "experiments:*"
      - "webhooks:*"
  viewer:
    permissions:
      - "*:read"

bindings:
  - subjects:
      - type: group
        name: superposition-admins
    role: admin
  - subjects:
      - type: group
        name: config-team
    role: config_manager
```

### TLS Configuration

```yaml
# Mutual TLS
tls:
  enabled: true
  certFile: /etc/tls/tls.crt
  keyFile: /etc/tls/tls.key
  caFile: /etc/tls/ca.crt
  clientAuth: require-and-verify-client-cert
```

### Network Policies

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: superposition-network-policy
spec:
  podSelector:
    matchLabels:
      app: superposition
  policyTypes:
    - Ingress
    - Egress
  ingress:
    - from:
        - namespaceSelector:
            matchLabels:
              name: ingress-nginx
      ports:
        - protocol: TCP
          port: 8080
  egress:
    - to:
        - podSelector:
            matchLabels:
              app: postgres
      ports:
        - protocol: TCP
          port: 5432
    - to:
        - podSelector:
            matchLabels:
              app: redis
      ports:
        - protocol: TCP
          port: 6379
```

---

## Monitoring & Observability

### Prometheus ServiceMonitor

```yaml
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: superposition
  labels:
    app: superposition
spec:
  selector:
    matchLabels:
      app: superposition
  endpoints:
    - port: http
      path: /metrics
      interval: 30s
```

### Grafana Dashboard

```json
{
  "dashboard": {
    "title": "Superposition",
    "panels": [
      {
        "title": "Request Rate",
        "type": "graph",
        "targets": [
          {
            "expr": "rate(http_requests_total{app=\"superposition\"}[5m])"
          }
        ]
      },
      {
        "title": "Latency P99",
        "type": "graph",
        "targets": [
          {
            "expr": "histogram_quantile(0.99, rate(http_request_duration_seconds_bucket{app=\"superposition\"}[5m]))"
          }
        ]
      },
      {
        "title": "Error Rate",
        "type": "graph",
        "targets": [
          {
            "expr": "rate(http_requests_total{app=\"superposition\",status=~\"5..\"}[5m])"
          }
        ]
      },
      {
        "title": "Active Experiments",
        "type": "stat",
        "targets": [
          {
            "expr": "superposition_experiments_active"
          }
        ]
      }
    ]
  }
}
```

### Alerting Rules

```yaml
apiVersion: monitoring.coreos.com/v1
kind: PrometheusRule
metadata:
  name: superposition-alerts
spec:
  groups:
    - name: superposition
      rules:
        - alert: SuperpositionHighErrorRate
          expr: rate(http_requests_total{app="superposition",status=~"5.."}[5m]) > 0.1
          for: 5m
          labels:
            severity: critical
          annotations:
            summary: High error rate in Superposition
            description: Error rate is {{ $value }} requests/s

        - alert: SuperpositionHighLatency
          expr: histogram_quantile(0.99, rate(http_request_duration_seconds_bucket{app="superposition"}[5m])) > 1
          for: 5m
          labels:
            severity: warning
          annotations:
            summary: High latency in Superposition
            description: P99 latency is {{ $value }}s
```

---

## Backup & Recovery

### Automated Backup Script

```bash
#!/bin/bash
# backup.sh

DATE=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="/backups"
S3_BUCKET="s3://your-bucket/backups"

# Database backup
pg_dump $DATABASE_URL | gzip > "$BACKUP_DIR/superposition_$DATE.sql.gz"

# Upload to S3
aws s3 cp "$BACKUP_DIR/superposition_$DATE.sql.gz" "$S3_BUCKET/"

# Cleanup old backups (keep last 7 days)
find $BACKUP_DIR -name "*.sql.gz" -mtime +7 -delete

echo "Backup completed: superposition_$DATE.sql.gz"
```

### Kubernetes CronJob for Backups

```yaml
apiVersion: batch/v1
kind: CronJob
metadata:
  name: superposition-backup
spec:
  schedule: "0 2 * * *"  # Daily at 2 AM
  jobTemplate:
    spec:
      template:
        spec:
          containers:
            - name: backup
              image: postgres:15-alpine
              command:
                - /bin/sh
                - -c
                - |
                  pg_dump $DATABASE_URL | gzip > /backup/superposition_$(date +%Y%m%d).sql.gz
                  aws s3 cp /backup/superposition_$(date +%Y%m%d).sql.gz s3://your-bucket/backups/
              env:
                - name: DATABASE_URL
                  valueFrom:
                    secretKeyRef:
                      name: superposition-secrets
                      key: database-url
              volumeMounts:
                - name: backup
                  mountPath: /backup
          volumes:
            - name: backup
              emptyDir: {}
          restartPolicy: OnFailure
```

### Recovery Procedure

```bash
# 1. Stop the application
kubectl scale deployment superposition --replicas=0

# 2. Download backup
aws s3 cp s3://your-bucket/backups/superposition_20240115.sql.gz .

# 3. Restore database
gunzip -c superposition_20240115.sql.gz | psql $DATABASE_URL

# 4. Start the application
kubectl scale deployment superposition --replicas=3

# 5. Verify
curl http://superposition.example.com/health
```

---

## Troubleshooting

### Common Errors

**Error: "Tenant not found"**
```text
Solution: Add tenant to TENANTS environment variable and restart
```

**Error: "Database connection failed"**
```text
Solution: Check DATABASE_URL, verify PostgreSQL is running, check network connectivity
```

**Error: "Configuration resolution timeout"**
```text
Solution: Add Redis cache, check database query performance, increase connection pool
```

### Debug Commands

```bash
# Check application logs
kubectl logs -l app=superposition --tail=100

# Check database connections
psql $DATABASE_URL -c "SELECT count(*) FROM pg_stat_activity;"

# Check Redis connectivity
redis-cli -u $REDIS_URL ping

# Port forward for local debugging
kubectl port-forward svc/superposition 8080:8080

# Execute into running pod
kubectl exec -it deployment/superposition -- /bin/sh
```

### Performance Debugging

```bash
# Enable query logging
export RUST_LOG=sqlx=debug

# Profile with perf (Linux)
perf record -g -p $(pgrep superposition)
perf report

# Memory profiling
valgrind --tool=massif ./superposition
```
