# Kubernetes Staggered Releaser (Mayday)

A Kubernetes deployment automation tool that leverages Superposition's experimentation features to perform safe, gradual rollouts with automatic traffic management and rollback capabilities.

## Overview

The `k8s-staggered-releaser` (internally named "Mayday") is a webhook-driven deployment orchestrator that integrates with Superposition's experimentation system to manage Kubernetes deployments. It automatically creates, updates, and manages Kubernetes resources (Deployments, Services, Ingresses) based on experiment lifecycle events from Superposition.

## Features

- **Webhook-Driven Automation**: Responds to Superposition experiment events via webhooks
- **Graduated Rollouts**: Implements canary deployments with configurable traffic splitting
- **Automatic Resource Management**: Creates and manages Kubernetes Deployments, Services, and Ingresses
- **Multi-Namespace Support**: Deploy to different Kubernetes namespaces based on context
- **Traffic Management**: Uses NGINX Ingress Controller for weighted traffic distribution
- **Rollback Capability**: Automatic cleanup and rollback when experiments conclude
- **Configuration Integration**: Fetches deployment configurations from Superposition

## Architecture

### Components

- **Main Server** (`main.rs`): HTTP server that receives webhook events from Superposition
- **Experiment Handler** (`experiment.rs`): Processes different experiment lifecycle events
- **Deployment Manager** (`deployment.rs`): Creates and configures Kubernetes Deployments
- **Service Manager** (`service.rs`): Manages Kubernetes Services for deployments
- **Ingress Manager** (`ingress.rs`): Handles NGINX Ingress resources for traffic routing
- **Utilities** (`utils.rs`): Common utilities and configuration constants

### Workflow

1. **Experiment Started**: Creates new Deployment, Service, and Ingress resources
2. **Experiment In Progress**: Updates traffic weights for gradual rollout
3. **Experiment Concluded**: Promotes winning variant and cleans up resources

## Installation

### Prerequisites

- Kubernetes cluster with NGINX Ingress Controller
- Superposition server running and accessible
- Rust 1.70 or later for building
- Valid Kubernetes API access token

### Setup

1. Clone the Superposition repository:
```bash
git clone https://github.com/juspay/superposition.git
cd superposition/examples/k8s-staggered-releaser
```

2. Configure the application by editing `src/utils.rs`:
```rust
pub const K8S_API_SERVER: &str = "https://your-k8s-api-server:6443";
pub const TOKEN: &str = "your-k8s-api-token";
```

3. Build the application:
```bash
cargo build --release
```

4. Run the webhook server:
```bash
cargo run
```

The server will start on `127.0.0.1:8090` and listen for webhook events at `/hi`.

## Configuration

### Environment Variables

You can configure the following constants in `src/utils.rs`:

- `K8S_API_SERVER`: Kubernetes API server URL
- `TOKEN`: Kubernetes API authentication token
- `NAMESPACE`: Default namespace for deployments

### Superposition Integration

Configure Superposition to send webhook events to this service:

1. Set up a webhook in Superposition pointing to `http://your-server:8090/hi`
2. Configure experiments with appropriate context conditions
3. Ensure your Superposition instance can resolve configurations for deployment variants

### Kubernetes RBAC

The service requires the following Kubernetes permissions:

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name: mayday-deployer
rules:
- apiGroups: ["apps"]
  resources: ["deployments"]
  verbs: ["create", "get", "list", "update", "patch", "delete"]
- apiGroups: [""]
  resources: ["services"]
  verbs: ["create", "get", "list", "update", "patch", "delete"]
- apiGroups: ["networking.k8s.io"]
  resources: ["ingresses"]
  verbs: ["create", "get", "list", "update", "patch", "delete"]
```

## Usage

### Webhook Event Types

The system responds to three types of Superposition webhook events:

#### 1. ExperimentStarted

Triggered when a new experiment begins. Creates initial deployment infrastructure:

```json
{
  "event_info": {
    "webhook_event": "ExperimentStarted",
    "workspace_id": "nginxservice"
  },
  "payload": {
    "context": {
      "and": [{
        "==": [{"var": "namespace"}, "mumbai"]
      }]
    },
    "variants": [
      {
        "id": "variant-123",
        "variant_type": "EXPERIMENTAL"
      }
    ],
    "traffic_percentage": 10
  }
}
```

**Actions Performed:**
- Creates Kubernetes Deployment with variant configuration
- Creates Service to expose the deployment
- Creates Ingress with canary annotations for traffic splitting

#### 2. ExperimentInprogress

Triggered when experiment traffic percentage is updated:

```json
{
  "event_info": {
    "webhook_event": "ExperimentInprogress",
    "workspace_id": "nginxservice"
  },
  "payload": {
    "traffic_percentage": 25,
    "variants": [...]
  }
}
```

**Actions Performed:**
- Updates Ingress resources with new traffic weights
- Adjusts canary routing configuration

#### 3. ExperimentConcluded

Triggered when an experiment ends with a chosen variant:

```json
{
  "event_info": {
    "webhook_event": "ExperimentConcluded",
    "workspace_id": "nginxservice"
  },
  "payload": {
    "chosen_variant": "variant-123",
    "variants": [...]
  }
}
```

**Actions Performed:**
- Updates main service to point to winning variant deployment
- Removes experimental Ingress resources
- Cleans up unused Services and Deployments

### Configuration Resolution

The system fetches deployment configurations from Superposition using the `/config/resolve` endpoint:

```
GET /config/resolve?namespace={namespace}&variantIds={variant_id}
Headers:
  x-org-id: localorg
  x-tenant: {service_name}
```

Expected configuration format:
```json
{
  "replicas": 3,
  "container.image": "nginx:latest",
  "env.DATABASE_URL": "postgres://...",
  "env.API_KEY": "secret-key"
}
```

### Deployment Resource Generation

The system generates Kubernetes resources with the following patterns:

#### Deployment Names
- Pattern: `{service}-dep-{variant_id}`
- Example: `nginxservice-dep-variant-123`

#### Service Names
- Pattern: `{service}-service-{variant_id}`
- Example: `nginxservice-service-variant-123`

#### Ingress Names
- Pattern: `{service}-ingress-{variant_id}`
- Example: `nginxservice-ingress-variant-123`

### Traffic Management

The system uses NGINX Ingress Controller canary features:

```yaml
metadata:
  annotations:
    nginx.ingress.kubernetes.io/canary: "true"
    nginx.ingress.kubernetes.io/canary-weight: "25"
spec:
  rules:
  - host: nginxservice.mumbai
    http:
      paths:
      - path: /
        backend:
          service:
            name: nginxservice-service-variant-123
            port:
              number: 80
```

## Examples

### Complete Experiment Workflow

1. **Start Experiment** (10% traffic):
```bash
# Webhook triggered automatically by Superposition
# Creates:
# - Deployment: nginxservice-dep-variant-123
# - Service: nginxservice-service-variant-123  
# - Ingress: nginxservice-ingress-variant-123 (10% weight)
```

2. **Increase Traffic** (25% traffic):
```bash
# Webhook updates traffic distribution
# Updates:
# - Ingress: nginxservice-ingress-variant-123 (25% weight)
```

3. **Conclude Experiment**:
```bash
# Webhook promotes winning variant
# Updates:
# - Service: nginxservice-service points to variant-123 deployment
# Deletes:
# - Ingress: nginxservice-ingress-variant-123
# - Service: nginxservice-service-variant-123
```

### Manual Testing

Test webhook endpoints directly:

```bash
# Test ExperimentStarted
curl -X GET http://127.0.0.1:8090/hi \
  -H "Content-Type: application/json" \
  -d '{
    "event_info": {
      "webhook_event": "ExperimentStarted",
      "workspace_id": "testservice"
    },
    "payload": {
      "context": {
        "and": [{
          "==": [{"var": "namespace"}, "mumbai"]
        }]
      },
      "variants": [{
        "id": "test-variant",
        "variant_type": "EXPERIMENTAL"
      }],
      "traffic_percentage": 10
    }
  }'
```

## Monitoring and Debugging

### Logging

The application provides detailed logging for each operation:

```
deployment created successfully!
service created successfully!
Ingress created successfully!
```

### Error Handling

Common error scenarios and troubleshooting:

#### Kubernetes API Connection Issues
```rust
// Check K8S_API_SERVER and TOKEN configuration
// Verify network connectivity to Kubernetes API
// Ensure proper RBAC permissions
```

#### Configuration Resolution Failures
```rust
// Verify Superposition server is accessible
// Check workspace_id and tenant configuration
// Ensure variant configurations exist
```

#### Resource Creation Failures
```rust
// Check Kubernetes cluster resources
// Verify namespace exists
// Review RBAC permissions
```

### Health Checks

Monitor the webhook endpoint:

```bash
# Basic health check
curl http://127.0.0.1:8090/hi
```

## Advanced Configuration

### Multi-Tenant Support

Configure different tenants in `main.rs`:

```rust
let app_state = Data::new(AppState {
    namespaces: ["mumbai".to_string(), "delhi".to_string()].to_vec(),
    tenants: ["service1".to_string(), "service2".to_string()].to_vec(),
});
```

### Custom Resource Templates

Modify resource generation in respective modules:

- `deployment.rs`: Customize Deployment specifications
- `service.rs`: Modify Service configurations  
- `ingress.rs`: Adjust Ingress rules and annotations

### Namespace Mapping

Customize namespace resolution in `utils.rs`:

```rust
pub fn get_namespace(context: Value) -> String {
    // Custom logic to extract namespace from context
    // Default implementation looks for comparison values
}
```

## Security Considerations

### TLS Configuration

The current implementation disables TLS verification:

```rust
let client = Client::builder()
    .danger_accept_invalid_certs(true) // ⚠️ Not for production
    .build()
```

For production deployments:
1. Use proper TLS certificates
2. Enable certificate verification
3. Use secure communication channels

### Authentication

- Store Kubernetes tokens securely (use Kubernetes secrets or vault systems)
- Implement webhook authentication if needed
- Use least-privilege RBAC policies

### Network Security

- Deploy in secure network environments
- Use network policies to restrict traffic
- Implement proper firewall rules

## Troubleshooting

### Common Issues

#### Webhook Not Receiving Events
- Verify Superposition webhook configuration
- Check network connectivity
- Ensure correct URL and port

#### Kubernetes Resource Creation Fails
- Verify RBAC permissions
- Check resource quotas
- Ensure namespaces exist

#### Traffic Routing Not Working
- Verify NGINX Ingress Controller is installed
- Check Ingress resource annotations
- Ensure DNS resolution works

#### Configuration Resolution Fails
- Verify Superposition server connectivity
- Check workspace and tenant configuration
- Ensure experiment contexts are properly configured

### Debug Mode

Enable verbose logging by modifying the application or adding environment variables for detailed operation tracking.

## Contributing

When contributing to this example:

1. Maintain backward compatibility with Superposition webhook formats
2. Follow Kubernetes resource naming conventions
3. Add proper error handling for all operations
4. Include comprehensive logging for debugging

## Source Code

The complete source code for this example is available in the [Superposition repository](https://github.com/juspay/superposition/tree/main/examples/k8s-staggered-releaser).