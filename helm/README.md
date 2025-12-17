# superposition

![Version: 0.1.0](https://img.shields.io/badge/Version-0.1.0-informational?style=flat-square) ![Type: application](https://img.shields.io/badge/Type-application-informational?style=flat-square) ![AppVersion: 0.88.0](https://img.shields.io/badge/AppVersion-0.88.0-informational?style=flat-square)

A Helm chart for Superposition Deployment

## Source Code

* <https://github.com/juspay/superposition>

## Deploy Superposition on Kubernetes using Helm

This chart deploys Superposition, a context-aware configuration management platform, on Kubernetes.

### Prerequisites

- Kubernetes 1.16+
- Helm 3.0+
- PostgreSQL database (can be configured via values)

## Installation

### Step 1 - Add Helm Repository

```bash
git clone https://juspay.github.io/superposition
cd superposition/charts/superposition
```

### Step 2 - Create Namespace (Optional)

```bash
kubectl create namespace superposition
```

### Step 3 - Install Superposition

```bash
helm install superposition . -n superposition
```

## Configuration

### Database Configuration

Superposition requires a PostgreSQL database:

```yaml
configs:
  database_url: postgres://postgres:password@postgres-service:5432/config?sslmode=disable
  db_user: postgres
  db_host: postgres-service:5432
  db_name: config
  max_db_connection_pool_size: 3

secrets:
  db_password: "your-password"
```

### Application Settings

Configure core application settings:

```yaml
configs:
  port: 8080
  rust_log: debug
  app_env: DEV
  superposition_version: "v0.1.0"
  actix_keep_alive: 120
```

### Authentication Configuration

Superposition supports different authentication providers:

#### Disabled Authentication (Development)
```yaml
configs:
  auth_provider: DISABLED
```

#### OIDC Authentication (Production)
```yaml
configs:
  auth_provider: OIDC+http://localhost:8081/realms/users
  oidc_client_id: superposition
  oidc_redirect_host: "http://localhost:8080"

secrets:
  oidc_client_secret: "your-oidc-secret"
```

### AWS Configuration

For AWS services integration:

```yaml
configs:
  aws_access_key_id: test
  aws_region: ap-south-1
  aws_region_endpoint: http://localhost:4566

secrets:
  aws_secret_access_key: "your-aws-secret"
  aws_session_token: "your-session-token"
```

### Access Control

Configure ingress for external access:

```yaml
ingress:
  enabled: true
  className: "nginx"
  hosts:
    - host: superposition.yourdomain.com
      paths:
        - path: /
          pathType: ImplementationSpecific
  tls:
    - secretName: superposition-tls
      hosts:
        - superposition.yourdomain.com
```

### Istio Configuration

For Istio service mesh integration:

```yaml
istio:
  enabled: true
  virtualService:
    enabled: true
    hosts:
      - superposition.yourdomain.com
    gateways:
      - istio-system/gateway
    http:
      - name: "superposition-routes"
        match:
          - uri:
              prefix: /
        timeout: 30s
```

### Resource Configuration

Set resource limits and requests:

```yaml
resources:
  requests:
    cpu: 200m
    memory: 256Mi
  limits:
    cpu: 500m
    memory: 512Mi
```

## Post-Deployment Verification

After deployment, verify Superposition is working:

### Health Check
```bash
kubectl port-forward service/superposition 8080:80 -n superposition
curl http://localhost:8080/health
```

### Check Pods
```bash
kubectl get pods -n superposition
kubectl logs -f deployment/superposition -n superposition
```

## Values

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| affinity | object | `{}` |  |
| autoscaling.enabled | bool | `false` |  |
| autoscaling.maxReplicas | int | `3` |  |
| autoscaling.minReplicas | int | `1` |  |
| autoscaling.targetCPUUtilizationPercentage | int | `80` |  |
| configs.actix_keep_alive | int | `120` |  |
| configs.allow_diff_keys_overlapping_ctx | bool | `true` |  |
| configs.allow_same_keys_non_overlapping_ctx | bool | `true` |  |
| configs.allow_same_keys_overlapping_ctx | bool | `true` |  |
| configs.api_hostname | string | `""` |  |
| configs.app_env | string | `"DEV"` |  |
| configs.auth_provider | string | `"DISABLED"` |  |
| configs.aws_access_key_id | string | `"test"` |  |
| configs.aws_region | string | `"ap-south-1"` |  |
| configs.aws_region_endpoint | string | `"http://localhost:4566"` |  |
| configs.aws_secret_access_key | string | `"test"` |  |
| configs.aws_session_token | string | `"test"` |  |
| configs.cac_host | string | `"http://localhost:8080"` |  |
| configs.database_url | string | `"postgres://postgres:docker@localhost:5432/config?sslmode=disable"` |  |
| configs.db_host | string | `"localhost:5432"` |  |
| configs.db_name | string | `"config"` |  |
| configs.db_user | string | `"postgres"` |  |
| configs.encrypted_keys | string | `""` |  |
| configs.hostname | string | `"<application_name>-<deployment_id>-<replicaset>-<pod>"` |  |
| configs.max_db_connection_pool_size | int | `3` |  |
| configs.port | int | `8080` |  |
| configs.rust_log | string | `"debug"` |  |
| configs.service_prefix | string | `""` |  |
| configs.superposition_version | string | `"v0.1.0"` |  |
| configs.tenant_middleware_exclusion_list | string | `"/health,/assets/favicon.ico,/pkg/frontend.js,/pkg,/pkg/frontend_bg.wasm,/pkg/tailwind.css,/pkg/style.css,/assets,/admin,/oidc/login,/admin/organisations,/organisations,/organisations/switch/{organisation_id},/"` |  |
| configs.worker_id | int | `1` |  |
| fullnameOverride | string | `""` |  |
| global.affinity | object | `{}` |  |
| global.annotations | object | `{}` |  |
| global.imageRegistry | string | `nil` |  |
| image.pullPolicy | string | `"IfNotPresent"` |  |
| image.registry | string | `"ghcr.io/juspay"` |  |
| image.repository | string | `"superposition"` |  |
| image.tag | string | `"0.88.0"` |  |
| imagePullSecrets | list | `[]` |  |
| ingress.annotations | object | `{}` |  |
| ingress.className | string | `""` |  |
| ingress.enabled | bool | `false` |  |
| ingress.hosts[0].host | string | `"superposition.local"` |  |
| ingress.hosts[0].paths[0].path | string | `"/"` |  |
| ingress.hosts[0].paths[0].pathType | string | `"ImplementationSpecific"` |  |
| ingress.tls | list | `[]` |  |
| istio.destinationRule.enabled | bool | `false` |  |
| istio.destinationRule.trafficPolicy | object | `{}` |  |
| istio.enabled | bool | `false` |  |
| istio.virtualService.enabled | bool | `false` |  |
| istio.virtualService.gateways | list | `[]` |  |
| istio.virtualService.hosts | list | `[]` |  |
| istio.virtualService.http | list | `[]` |  |
| livenessProbe.httpGet.path | string | `"/"` |  |
| livenessProbe.httpGet.port | string | `"http"` |  |
| nameOverride | string | `""` |  |
| nodeSelector | object | `{}` |  |
| podAnnotations | object | `{}` |  |
| podLabels | object | `{}` |  |
| podSecurityContext | object | `{}` |  |
| readinessProbe.httpGet.path | string | `"/"` |  |
| readinessProbe.httpGet.port | string | `"http"` |  |
| replicaCount | int | `1` |  |
| resources | object | `{}` |  |
| secrets.db_password | string | `""` |  |
| secrets.oidc_client_secret | string | `""` |  |
| secrets.superposition_token | string | `""` |  |
| securityContext | object | `{}` |  |
| service.port | int | `80` |  |
| service.type | string | `"ClusterIP"` |  |
| serviceAccount.annotations | object | `{}` |  |
| serviceAccount.automount | bool | `true` |  |
| serviceAccount.create | bool | `true` |  |
| serviceAccount.name | string | `""` |  |
| tolerations | list | `[]` |  |
| volumeMounts | list | `[]` |  |
| volumes | list | `[]` |  |

## Examples

### Example 1: Basic Development Setup

```yaml
replicaCount: 1

configs:
  database_url: postgres://postgres:docker@postgres:5432/config?sslmode=disable
  port: 8080
  rust_log: debug
  app_env: DEV
  auth_provider: DISABLED

secrets:
  db_password: "docker"

service:
  type: ClusterIP
  port: 80
```

### Example 2: Production with Ingress

```yaml
replicaCount: 3

configs:
  database_url: postgres://postgres:password@postgres-prod:5432/config?sslmode=require
  port: 8080
  rust_log: info
  app_env: PRODUCTION
  auth_provider: OIDC+https://auth.yourdomain.com/realms/users
  oidc_client_id: superposition
  oidc_redirect_host: "https://superposition.yourdomain.com"
  max_db_connection_pool_size: 10

secrets:
  db_password: "production-password"
  oidc_client_secret: "oidc-secret"
  superposition_token: "api-token"

ingress:
  enabled: true
  className: "nginx"
  annotations:
    cert-manager.io/cluster-issuer: "letsencrypt-prod"
  hosts:
    - host: superposition.yourdomain.com
      paths:
        - path: /
          pathType: ImplementationSpecific
  tls:
    - secretName: superposition-tls
      hosts:
        - superposition.yourdomain.com

resources:
  requests:
    cpu: 500m
    memory: 512Mi
  limits:
    cpu: 1000m
    memory: 1Gi

autoscaling:
  enabled: true
  minReplicas: 3
  maxReplicas: 10
  targetCPUUtilizationPercentage: 70
```

### Example 3: With Istio Service Mesh

```yaml
replicaCount: 2

configs:
  database_url: postgres://postgres:password@postgres:5432/config?sslmode=disable
  port: 8080
  app_env: PRODUCTION
  auth_provider: OIDC+https://auth.yourdomain.com/realms/users

secrets:
  db_password: "password"
  oidc_client_secret: "secret"

istio:
  enabled: true
  virtualService:
    enabled: true
    hosts:
      - superposition.yourdomain.com
    gateways:
      - istio-system/gateway
    http:
      - name: "superposition-api"
        match:
          - uri:
              prefix: /
        timeout: 30s
        retries:
          attempts: 3
          perTryTimeout: 10s
  destinationRule:
    enabled: true
    trafficPolicy:
      loadBalancer:
        simple: ROUND_ROBIN
      connectionPool:
        tcp:
          maxConnections: 50
          connectTimeout: 30s
```

## Troubleshooting

### Database Connection Issues

Check database connectivity:
```bash
kubectl logs deployment/superposition -n superposition | grep -i database
```

### Authentication Problems

Verify OIDC configuration:
```bash
kubectl get configmap superposition-cm -n superposition -o yaml
```

### Pod Startup Issues

Check pod events:
```bash
kubectl describe pod <pod-name> -n superposition
```

## Support

For issues and questions:
- Documentation: https://github.com/juspay/superposition
- GitHub Issues: https://github.com/juspay/superposition/issues

----------------------------------------------
Autogenerated from chart metadata using [helm-docs v1.14.2](https://github.com/norwoodj/helm-docs/releases/v1.14.2)
