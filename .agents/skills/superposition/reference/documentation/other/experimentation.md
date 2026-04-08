---
sidebar_position: 4
title: Experimentation
description: Experimentation component of Superposition platform
---


# Experimentation

## Introduction

The **Experimentation** component enables one to run A/B/../Z testing for your
configurations in equi-sized groups/cohorts. It works on top of
**Context-Aware-Config**.  The Experimentation component can also be used as a
release system for your configuration changes which becomes an A/B test with
two variants - the current value and new value.

## Concepts

### Experiment
An experiment, as the name suggests, enables you to test and evaluate the
behaviour of the system for different values of the same configuration. An
experiment can have exactly one **CONTROL** variant and `n` **EXPERIMENTAL**
variants, with each variant overriding/changing same set of keys in
configuration. An experiment's scope can be controlled by declaring the
context, which chalks out the sample set for the experiment from the
population.

For example in our ride-hailing example, we may want to run an experiment for
the 8th hour of the day in the city of Bangalore.  We can do that by specifying
a context for the experiment.
```
[city IS "Bangalore", hour_of_day IS "8"]
```

### Variant
A variant in an experiment, represent one of the `n` tests. In simple terms,
its a collection of key-value pairs, where each key is one of your already
defined **default-config** key, and the value can be any valid new/old
configuration value for the key adhering to it type constraints. There are two kinds of variants:

1. **CONTROL**: It conceptually represents the current state of the configuration. 
2. **EXPERIMENTAL**: The experimental variant lets you define the newer value for the **default-config** keys.

#### Control Variant
```json
    {
        "base_rate": 50,
        "per_distance_unit_rate": 30
    }
```

The context for the above configuration is setup using a special additional dimension called `variantIds`
```
[city IS "Bangalore", hour_of_day IS 8, variantId IS "control-variant-for-experiment-1"]
```

#### Experimental Variant
```json
    {
        "base_rate": 45,
        "per_distance_unit_rate": 30
    }
```
The context for the above configuration is setup using a special additional dimension called `variantIds`
```
[city IS "Bangalore", hour_of_day IS 8, variantId IS "experimental-variant-for-experiment-1"]
```

> [!NOTE]
> Note how the control and experimental configuration add additional conditions to the base context of the experiment.


### Experiment's Traffic Percentage
This defines the traffic size for each variant of the experiment, for instance
if traffic percentage is `13%` and there are `4` variants in the experiment,
    this makes each variant of the experiment receive `13%` of the entire
    traffic and in entirety `13 * 4 = 52%` of the total traffic.

### Webhooks

Webhooks in Superposition are HTTP endpoints that receive real-time notifications about experiment lifecycle events. They enable seamless integration with external systems, allowing automated responses to experiment state changes such as deployments, monitoring, or notifications.

#### What are Webhooks?

Webhooks are HTTP POST/PUT/GET requests automatically triggered when specific experiment events occur. They allow external systems to:

- **Automate Deployments**: Trigger deployments when experiments start or conclude
- **Monitor Experiments**: Send notifications to monitoring systems
- **Update External Systems**: Sync experiment states with other platforms
- **Trigger Workflows**: Initiate downstream processes based on experiment events

#### Webhook Event Types

Superposition supports the following experiment lifecycle events:

| Event | Description | When Triggered |
|-------|-------------|----------------|
| `ExperimentCreated` | New experiment created | When an experiment is successfully created |
| `ExperimentStarted` | Experiment begins | When experiment starts receiving traffic (first ramp) |
| `ExperimentInprogress` | Traffic percentage updated | When experiment traffic is modified during ramping |
| `ExperimentUpdated` | Experiment configuration changed | When experiment overrides or settings are modified |
| `ExperimentConcluded` | Experiment ends with chosen variant | When experiment concludes with a winning variant |
| `ExperimentDiscarded` | Experiment discarded | When experiment is discarded without conclusion |
| `ExperimentPaused` | Experiment paused | When experiment is temporarily paused |

#### Webhook Configuration

Webhooks can be configured with the following properties:

**Basic Configuration:**
- **Name**: Unique identifier for the webhook
- **Description**: Human-readable description of the webhook's purpose
- **URL**: Target endpoint that will receive the webhook calls
- **Method**: HTTP method (GET, POST, PUT, DELETE, PATCH, HEAD)
- **Enabled**: Toggle to enable/disable the webhook

**Advanced Configuration:**
- **Events**: List of experiment events that trigger this webhook
- **Custom Headers**: Additional HTTP headers to include in requests
- **Max Retries**: Number of retry attempts for failed webhook calls
- **Payload Version**: API version for payload structure (currently V1)

#### Webhook Payload Structure

All webhook payloads follow a standardized structure:

```json
{
  "event_info": {
    "webhook_event": "ExperimentStarted",
    "time": "2024-12-24T10:30:00Z",
    "workspace_id": "your-workspace",
    "organisation_id": "your-org",
    "config_version": "12345"
  },
  "payload": {
    // Event-specific experiment data
  }
}
```

**Event Info Fields:**
- `webhook_event`: The type of event that triggered the webhook
- `time`: ISO 8601 timestamp when the event occurred
- `workspace_id`: Identifier of the workspace where the experiment exists
- `organisation_id`: Organization identifier
- `config_version`: Current configuration version (optional)

#### Example Webhook Payloads

**ExperimentStarted Event:**
```json
{
  "event_info": {
    "webhook_event": "ExperimentStarted",
    "time": "2024-12-24T10:30:00Z",
    "workspace_id": "ecommerce",
    "organisation_id": "mycompany"
  },
  "payload": {
    "id": 123456789,
    "name": "checkout-optimization",
    "context": {
      "and": [
        {"==": [{"var": "region"}, "north"]},
        {"==": [{"var": "user_type"}, "premium"]}
      ]
    },
    "variants": [
      {
        "id": "control-variant",
        "variant_type": "CONTROL"
      },
      {
        "id": "experimental-variant",
        "variant_type": "EXPERIMENTAL"
      }
    ],
    "traffic_percentage": 10,
    "status": "INPROGRESS"
  }
}
```

**ExperimentConcluded Event:**
```json
{
  "event_info": {
    "webhook_event": "ExperimentConcluded",
    "time": "2024-12-24T15:45:00Z",
    "workspace_id": "ecommerce",
    "organisation_id": "mycompany"
  },
  "payload": {
    "id": 123456789,
    "name": "checkout-optimization",
    "chosen_variant": "experimental-variant",
    "variants": [
      {
        "id": "control-variant",
        "variant_type": "CONTROL"
      },
      {
        "id": "experimental-variant",
        "variant_type": "EXPERIMENTAL"
      }
    ],
    "status": "CONCLUDED"
  }
}
```

#### Setting Up Webhooks

**1. Create a Webhook:**
```bash
curl -X POST https://your-superposition-instance/webhook \
  -H "Content-Type: application/json" \
  -H "x-tenant: your-workspace" \
  -d '{
    "name": "deployment-webhook",
    "description": "Triggers deployments based on experiment events",
    "enabled": true,
    "url": "https://your-deployment-service.com/webhook",
    "method": "POST",
    "events": ["ExperimentStarted", "ExperimentConcluded"],
    "custom_headers": {
      "Authorization": "Bearer your-api-token",
      "Content-Type": "application/json"
    },
    "change_reason": "Setting up automated deployments"
  }'
```

**2. Handle Webhook in Your Service:**
```javascript
app.post('/webhook', (req, res) => {
  const { event_info, payload } = req.body;
  
  switch (event_info.webhook_event) {
    case 'ExperimentStarted':
      // Create new deployment
      deployNewVariant(payload);
      break;
      
    case 'ExperimentConcluded':
      // Promote winning variant
      promoteVariant(payload.chosen_variant);
      break;
      
    case 'ExperimentInprogress':
      // Update traffic routing
      updateTrafficSplit(payload.traffic_percentage);
      break;
  }
  
  res.status(200).json({ message: 'Webhook processed successfully' });
});
```

#### Real-World Use Cases

**1. Automated Kubernetes Deployments:**
- **ExperimentStarted**: Create new Deployments, Services, and Ingresses
- **ExperimentInprogress**: Update traffic weights in Ingress controllers
- **ExperimentConcluded**: Promote winning variant and cleanup resources

**2. Monitoring and Alerting:**
- **ExperimentStarted**: Send notifications to Slack/Teams
- **ExperimentConcluded**: Update dashboards and send success reports
- **ExperimentDiscarded**: Alert teams about failed experiments

**3. CI/CD Integration:**
- **ExperimentStarted**: Trigger additional testing pipelines
- **ExperimentConcluded**: Update configuration repositories
- **ExperimentUpdated**: Validate configuration changes

#### Security and Best Practices

**Authentication:**
- Use custom headers for API keys or tokens
- Implement webhook signature validation in your endpoints
- Use HTTPS endpoints for secure communication

**Error Handling:**
- Implement proper HTTP status codes in your webhook endpoints
- Use the max_retries setting for resilient webhook delivery
- Log webhook failures for debugging

**Performance:**
- Keep webhook endpoints lightweight and fast-responding
- Use asynchronous processing for heavy operations
- Implement timeout handling in your webhook receivers

#### Webhook Management API

**List Webhooks:**
```bash
GET /webhook
```

**Get Specific Webhook:**
```bash
GET /webhook/{webhook_name}
```

**Update Webhook:**
```bash
PATCH /webhook/{webhook_name}
```

**Delete Webhook:**
```bash
DELETE /webhook/{webhook_name}
```

**Get Webhooks by Event:**
```bash
GET /webhook/event/{event_type}
```

Webhooks provide a powerful mechanism to build automated, event-driven workflows around your experimentation processes, enabling seamless integration with your existing infrastructure and tooling. 
